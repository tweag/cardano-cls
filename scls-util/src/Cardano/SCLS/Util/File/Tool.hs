{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.SCLS.Util.File.Tool (splitFile, mergeFiles, extract, ExtractOptions (..)) where

import Cardano.SCLS.Internal.Reader
import Cardano.SCLS.Internal.Record.Hdr (Hdr (..))
import Cardano.SCLS.Internal.Serializer.Dump
import Cardano.SCLS.Internal.Serializer.Dump.Plan (addChunks, defaultSerializationPlan, mkSortedSerializationPlan)
import Cardano.SCLS.Internal.Serializer.External.Impl (serialize)
import Cardano.SCLS.Util.Result
import Cardano.Types.Namespace (Namespace (..))
import Cardano.Types.Namespace qualified as Namespace
import Cardano.Types.Network (NetworkId (Mainnet))
import Cardano.Types.SlotNo (SlotNo (SlotNo))
import Control.Exception (SomeException, catch)
import Control.Monad (foldM)
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.MemPack.Extra
import Streaming qualified as S
import Streaming.Prelude qualified as S
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO

splitFile :: FilePath -> FilePath -> IO Result
splitFile sourceFile outputDir = do
  putStrLn $ "Splitting file: " ++ sourceFile
  putStrLn $ "Output directory: " ++ outputDir
  catch
    do
      createDirectoryIfMissing True outputDir
      hdr <- withHeader sourceFile pure
      namespaces <- extractNamespaceList sourceFile

      mapM_
        ( \ns -> do
            let outputFile = outputDir </> Namespace.humanFileNameFor ns
            putStrLn $ "  Creating " ++ outputFile ++ " for namespace " ++ Namespace.asString ns

            withBinaryFile outputFile WriteMode $ \handle -> do
              withNamespacedData @RawBytes sourceFile ns $ \stream -> do
                let dataStream = S.yield (ns S.:> stream)
                -- namespace-specific data should be sorted, so we can assume that and dump directly
                dumpToHandle handle hdr (mkSortedSerializationPlan (defaultSerializationPlan & addChunks dataStream) id)
        )
        namespaces

      putStrLn $ "Split complete. Generated these files:"
      mapM_ (putStrLn . ("  - " ++) . (outputDir </>) . Namespace.humanFileNameFor) namespaces
      pure Ok
    \(e :: SomeException) -> do
      putStrLn $ "Error: " ++ show e
      pure OtherError

mergeFiles :: FilePath -> [FilePath] -> IO Result
mergeFiles _ [] = do
  putStrLn "No source files provided for merging"
  pure OtherError
mergeFiles outputFile sourceFiles = do
  putStrLn $ "Merging " ++ show (length sourceFiles) ++ " file(s) into: " ++ outputFile
  catch
    do
      nsToFiles <- collectNamespaceFiles sourceFiles

      putStrLn $ "Found " ++ show (Map.size nsToFiles) ++ " unique namespace(s)"

      let stream =
            S.each (Map.toList nsToFiles)
              & S.mapM_ \(ns, files) -> do
                S.each files
                  & S.mapM
                    ( \file -> do
                        s <- withNamespacedData @RawBytes file ns $ \s ->
                          -- eagerly load each stream to avoid issues with file handles
                          -- FIXME: use a different data structure like Vector
                          -- FIXME: concerns about loading entire namespace data into memory
                          S.toList_ s
                        pure (ns S.:> S.each s)
                    )

      serialize
        outputFile
        Mainnet
        (SlotNo 1)
        (defaultSerializationPlan & addChunks stream)

      putStrLn "Merge complete"
      pure Ok
    \(e :: SomeException) -> do
      putStrLn $ "Error: " ++ show e
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
  }

extract :: FilePath -> FilePath -> ExtractOptions -> IO Result
extract sourceFile outputFile ExtractOptions{..} = do
  putStrLn $ "Extracting from file: " ++ sourceFile
  putStrLn $ "Output file: " ++ outputFile
  catch
    do
      Hdr{..} <- withHeader sourceFile pure

      let chunks =
            case extractNamespaces of
              Nothing -> S.each []
              Just nsList ->
                S.each nsList
                  & S.mapM
                    ( \ns -> do
                        s <- withNamespacedData @RawBytes sourceFile ns $ \s ->
                          -- eagerly load each stream to avoid issues with file handles
                          -- FIXME: use a different data structure like Vector
                          -- FIXME: concerns about loading entire namespace data into memory
                          S.toList_ s
                        pure (ns S.:> S.each s)
                    )

      serialize
        outputFile
        networkId
        slotNo
        (defaultSerializationPlan & addChunks chunks)

      pure Ok
    \(e :: SomeException) -> do
      putStrLn $ "Error: " ++ show e
      pure OtherError
