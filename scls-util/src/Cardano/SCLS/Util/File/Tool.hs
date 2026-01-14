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
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger
import Control.Monad.Trans.Resource (MonadUnliftIO, allocate, release, runResourceT)
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

{- | Split a single SCLS file into multiple files by namespace.
Takes a source SCLS file and an output directory, and creates separate files
for each namespace found in the source file.
-}
splitFile :: (MonadIO m, MonadLogger m, MonadUnliftIO m) => FilePath -> FilePath -> SplitOptions -> m Result
splitFile sourceFile outputDir SplitOptions{} = do
  logDebugN $ "Splitting file: " <> T.pack sourceFile
  logDebugN $ "Output directory: " <> T.pack outputDir
  (hdr, fileNamespaces) <-
    liftIO do
      createDirectoryIfMissing True outputDir
      hdr <- withHeader sourceFile pure
      fileNamespaces <- extractNamespaceList sourceFile
      pure (hdr, fileNamespaces)

  runResourceT do
    (_, sourceHandle) <- allocate (openBinaryFile sourceFile ReadMode) hClose
    mapM_
      ( \ns -> do
          let outputFile = outputDir </> Namespace.humanFileNameFor ns
          logDebugN $ "  Creating " <> T.pack outputFile <> " for namespace " <> Namespace.asText ns
          (key, handle) <- allocate (openBinaryFile outputFile WriteMode) hClose
          withNamespacedDataHandle @RawBytes sourceHandle ns $ \stream -> do
            let dataStream = S.yield (ns S.:> stream)
            -- namespace-specific data should be sorted, so we can assume that and dump directly
            dumpToHandle handle hdr (mkSortedSerializationPlan (defaultSerializationPlan & addChunks dataStream) id)
          release key
      )
      fileNamespaces

  logInfoN $ "Split complete. Generated these files:"
  mapM_ (logInfoN . ("  - " <>) . (T.pack . (outputDir </>)) . Namespace.humanFileNameFor) fileNamespaces
  pure Ok

{- | Merge multiple SCLS files into a single output file.

Takes a list of input files and combines their namespace data into a single
output file.
-}
mergeFiles :: (MonadLogger m, MonadIO m) => FilePath -> [FilePath] -> m Result
mergeFiles _ [] = do
  logErrorN "No source files provided for merging"
  pure OtherError
mergeFiles outputFile sourceFiles = do
  logDebugN $ "Merging " <> T.pack (show (length sourceFiles)) <> " file(s) into: " <> T.pack outputFile
  nsToFiles <- liftIO $ Map.toList <$> collectNamespaceFiles sourceFiles

  logDebugN $ "Found " <> T.pack (show (length nsToFiles)) <> " unique namespace(s)"

  liftIO $ runResourceT $ do
    let stream =
          S.each nsToFiles
            & S.mapM_ \(ns, files) -> do
              S.each files
                & S.mapM
                  ( \file -> do
                      (_, handle) <- allocate (openFile file ReadMode) hClose
                      pure (ns S.:> namespacedData @RawBytes handle ns)
                  )

    serialize
      outputFile
      Mainnet
      (SlotNo 1)
      (defaultSerializationPlan & addChunks stream)

  logDebugN "Merge complete"
  pure Ok
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
  }

{- | Extract specific data from an SCLS file into a new file.
Takes a source SCLS file, an output file, and extraction options specifying
which data to extract.
-}
extract :: (MonadLogger m, MonadIO m) => FilePath -> FilePath -> ExtractOptions -> m Result
extract sourceFile outputFile ExtractOptions{..} = do
  logDebugN $ "Extracting from file: " <> T.pack sourceFile
  logDebugN $ "Output file: " <> T.pack outputFile
  Hdr{..} <- liftIO $ withHeader sourceFile pure

  liftIO $ runResourceT do
    (_, handle) <- allocate (openBinaryFile sourceFile ReadMode) hClose
    let chunks =
          case extractNamespaces of
            Nothing -> S.each []
            Just nsList ->
              S.each nsList
                & S.map
                  (\ns -> (ns S.:> namespacedData @RawBytes handle ns))

    serialize
      outputFile
      networkId
      slotNo
      (defaultSerializationPlan & addChunks chunks)

  pure Ok

data UnpackOptions = UnpackOptions

unpack :: (MonadIO m, MonadUnliftIO m, MonadLogger m) => FilePath -> FilePath -> T.Text -> UnpackOptions -> m Result
unpack sourceFile unpackOutputFile unpackNamespace UnpackOptions{} = do
  logDebugN $ "Converting file: " <> T.pack sourceFile
  logDebugN $ "Output file: " <> T.pack unpackOutputFile
  let namespace = Namespace.fromText unpackNamespace

  runResourceT do
    case namespaceSymbolFromText unpackNamespace of
      Nothing -> do
        logErrorN $ "Unknown namespace: " <> T.pack (Namespace.asString namespace)
        pure OtherError
      Just (SomeNamespaceSymbol (_ :: proxy ns)) -> do
        (_, outputHandle) <- allocate (openBinaryFile unpackOutputFile WriteMode) hClose
        (_, sourceHandle) <- allocate (openBinaryFile sourceFile ReadMode) hClose
        withNamespacedDataHandle @(GenericCBOREntry (NamespaceKeySize ns)) sourceHandle namespace $ \stream -> do
          stream
            & S.mapM_ \(GenericCBOREntry (ChunkEntry (ByteStringSized k) b)) ->
              liftIO $
                BL.hPut outputHandle $
                  CBOR.toLazyByteString $
                    CBOR.encodeListLen 2 <> CBOR.encodeBytes k <> CBOR.encodePreEncoded (getEncodedBytes b)
        pure Ok
