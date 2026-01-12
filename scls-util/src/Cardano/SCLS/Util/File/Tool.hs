{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | File manipulation utilities for SCLS files.
module Cardano.SCLS.Util.File.Tool (splitFile, mergeFiles, extract, ExtractOptions (..), unpack, UnpackOptions (..), SplitOptions (..)) where

import Cardano.SCLS.CDDL
import Cardano.SCLS.Internal.Entry.CBOREntry
import Cardano.SCLS.Internal.Entry.ChunkEntry
import Cardano.SCLS.Internal.Reader
import Cardano.SCLS.Internal.Record.Hdr (Hdr (..))
import Cardano.SCLS.Internal.Serializer.Dump
import Cardano.SCLS.Internal.Serializer.Dump.Plan (addChunks, defaultSerializationPlan, mkSortedSerializationPlan)
import Cardano.SCLS.Internal.Serializer.External.Impl (serialize)
import Cardano.SCLS.NamespaceKey (NamespaceKeySize)
import Cardano.SCLS.NamespaceSymbol (SomeNamespaceSymbol (SomeNamespaceSymbol))
import Cardano.SCLS.Util.Result
import Cardano.Types.Namespace (Namespace (..))
import Cardano.Types.Namespace qualified as Namespace
import Cardano.Types.Network (NetworkId (Mainnet))
import Cardano.Types.SlotNo (SlotNo (SlotNo))
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Monad (foldM)
import Control.Monad.Trans.Resource (allocate, runResourceT, MonadUnliftIO)
import Control.Monad.Catch (MonadCatch, SomeException, catch)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.MemPack.Extra
import Data.Text qualified as T
import Streaming qualified as S
import Streaming.Prelude qualified as S
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO

data FileFormat = SclsFormat | CBorFormat
  deriving (Eq, Show)

data SplitOptions = SplitOptions
  { splitIsQuiet :: Bool
  }

{- | Split a single SCLS file into multiple files by namespace.
Takes a source SCLS file and an output directory, and creates separate files
for each namespace found in the source file.
-}
splitFile :: (MonadCatch m, MonadIO m, MonadLogger m) => FilePath -> FilePath -> SplitOptions -> m Result
splitFile sourceFile outputDir SplitOptions{} = do
  logInfoN $ "Splitting file: " <> T.pack sourceFile
  logInfoN $ "Output directory: " <> T.pack outputDir
  catch
    do
      (hdr, fileNamespaces) <-
        liftIO do
          createDirectoryIfMissing True outputDir
          hdr <- withHeader sourceFile pure
          fileNamespaces <- extractNamespaceList sourceFile
          pure (hdr, fileNamespaces)

      mapM_
        ( \ns -> do
            let outputFile = outputDir </> Namespace.humanFileNameFor ns
            logDebugN $ "  Creating " <> T.pack outputFile <> " for namespace " <> Namespace.asText ns

            runResourceT $ do
              handle <- allocate (openBinaryFile outputFile WriteMode)
              sourceHandle <- allocate (openBinaryFile sourceFile ReadMode)
              withNamespacedDataHandle @RawBytes sourceHandle ns $ \stream -> do
                let dataStream = S.yield (ns S.:> stream)
                -- namespace-specific data should be sorted, so we can assume that and dump directly
                dumpToHandle handle hdr (mkSortedSerializationPlan (defaultSerializationPlan & addChunks dataStream) id)
        )
        fileNamespaces

      logInfoN $ "Split complete. Generated these files:"
      mapM_ (logInfoN . ("  - " <>) . (T.pack . (outputDir </>)) . Namespace.humanFileNameFor) fileNamespaces
      pure Ok
    \(e :: SomeException) -> do
      logErrorN $ "Error: " <> T.pack (show e)
      pure OtherError

{- | Merge multiple SCLS files into a single output file.

Takes a list of input files and combines their namespace data into a single
output file.
-}
mergeFiles :: (MonadCatch m, MonadLogger m, MonadIO m) => FilePath -> [FilePath] -> m Result
mergeFiles _ [] = do
  logErrorN "No source files provided for merging"
  pure OtherError
mergeFiles outputFile sourceFiles = do
  logInfoN $ "Merging " <> T.pack (show (length sourceFiles)) <> " file(s) into: " <> T.pack outputFile
  catch
    do
      nsToFiles <- Map.toList <$> liftIO $ collectNamespaceFiles sourceFiles

      logInfoN $ "Found " <> T.pack (show (length nsToFiles)) <> " unique namespace(s)"

      runResourceT $ do
        let stream =
              S.each nsToFiles
                & S.mapM_ \(ns, files) -> do
                  S.each files
                    & S.mapM
                      ( \file -> do
                          (_, handle) <- allocate (openFile file ReadMode) hClose
                          pure (ns S.:> namespacedData @RawBytes handle ns)
                      )

        liftIO $
          serialize
            outputFile
            Mainnet
            (SlotNo 1)
            (defaultSerializationPlan & addChunks stream)

      logInfoN "Merge complete"
      pure Ok
    \(e :: SomeException) -> do
      logErrorN $ "Error: " <> T.pack (show e)
      pure OtherError
 where
  collectNamespaceFiles :: [FilePath] -> IO (Map.Map Namespace [FilePath])
  collectNamespaceFiles files = do
    foldM
      ( \acc f -> do
          foldl'
            ( \m ns -> do
                let l = maybe [f] ((:) f) (Map.lookup ns m)
                Map.insert ns l m
            )
            acc
            <$> extractNamespaceList f
      )
      mempty
      files

data ExtractOptions = ExtractOptions
  { extractNamespaces :: Maybe [Namespace]
  , extractIsQuiet :: Bool
  }

{- | Extract specific data from an SCLS file into a new file.
Takes a source SCLS file, an output file, and extraction options specifying
which data to extract.
-}
extract :: (MonadCatch m, MonadLogger m, MonadIO m) => FilePath -> FilePath -> ExtractOptions -> m Result
extract sourceFile outputFile ExtractOptions{..} = do
  logDebugN $ "Extracting from file: " <> T.pack sourceFile
  logDebugN $ "Output file: " <> T.pack outputFile
  catch
    do
      Hdr{..} <- liftIO $ withHeader sourceFile pure

      withBinaryFile sourceFile ReadMode \handle -> do
        let chunks =
              case extractNamespaces of
                Nothing -> S.each []
                Just nsList ->
                  S.each nsList
                    & S.map
                      (\ns -> (ns S.:> namespacedData @RawBytes handle ns))

        runResourceT $
          serialize
            outputFile
            networkId
            slotNo
            (defaultSerializationPlan & addChunks chunks)

      pure Ok
    \(e :: SomeException) -> do
      logErrorN $ "Error: " <> T.pack (show e)
      pure OtherError

data UnpackOptions = UnpackOptions
  { unpackIsQuiet :: Bool
  }

unpack :: (MonadIO m, MonadCatch m, MonadUnliftIO m, MonadLogger m) => FilePath -> FilePath -> T.Text -> UnpackOptions -> m Result
unpack sourceFile unpackOutputFile unpackNamespace UnpackOptions{..} = do
  logDebugN $ "Converting file: " <> T.pack sourceFile
  logDebugN $ "Output file: " <> T.pack unpackOutputFile
  catch
    do
      let namespace = Namespace.fromText unpackNamespace

      runResourceT do
        (_, outputHandle) <- allocate (openBinaryFile unpackOutputFile WriteMode) (hClose)
        case namespaceSymbolFromText unpackNamespace of
          Nothing -> do
            logErrorN $ "Unknown namespace: " <> T.pack (Namespace.asString namespace)
            pure OtherError
          Just (SomeNamespaceSymbol (_ :: proxy ns)) -> do
            liftIO $ withNamespacedData @(GenericCBOREntry (NamespaceKeySize ns)) sourceFile namespace $ \stream -> do
              stream
                & S.mapM_ \(GenericCBOREntry (ChunkEntry (ByteStringSized k) b)) ->
                  BL.hPut outputHandle $
                    CBOR.toLazyByteString $
                      CBOR.encodeListLen 2 <> CBOR.encodeBytes k <> CBOR.encodePreEncoded (getEncodedBytes b)
            pure Ok
    \(e :: SomeException) -> do
      logErrorN $ "Error: " <> T.pack (show e)
      pure OtherError
