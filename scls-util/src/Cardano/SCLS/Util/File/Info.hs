{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.SCLS.Util.File.Info (listNamespaces, displayInfo) where

import Cardano.SCLS.Internal.Reader
import Cardano.SCLS.Internal.Record.Manifest
import Cardano.SCLS.Util.Result
import Cardano.Types.Namespace qualified as Namespace
import Control.Exception (SomeException, catch)
import Data.Map.Strict qualified as Map

displayInfo :: FilePath -> IO Result
displayInfo filePath = do
  putStrLn $ "File: " ++ filePath
  catch
    do
      Manifest{..} <- withLatestManifestFrame pure filePath

      putStrLn "\n=== File Information ==="
      putStrLn $ "Root Hash: " ++ show rootHash
      putStrLn $ "Total Entries: " ++ show totalEntries
      putStrLn $ "Total Chunks: " ++ show totalChunks

      putStrLn "\n=== Namespaces ==="
      if null nsInfo
        then putStrLn "(No namespaces)"
        else do
          mapM_
            ( \(ns, NamespaceInfo{..}) -> do
                putStrLn $ "\n" ++ Namespace.asString ns ++ ":"
                putStrLn $ "  Hash: " ++ show namespaceHash
                putStrLn $ "  Entries: " ++ show namespaceEntries
                putStrLn $ "  Chunks: " ++ show namespaceChunks
            )
            $ Map.toList nsInfo

      pure Ok
    \(e :: SomeException) -> do
      putStrLn $ "Error: " ++ show e
      pure OtherError

listNamespaces :: FilePath -> IO Result
listNamespaces filePath = do
  catch
    do
      namespaces <- extractNamespaceList filePath
      if null namespaces
        then putStrLn "No namespaces found"
        else do
          putStrLn "Namespaces:"
          mapM_ (putStrLn . ("  - " <>) . Namespace.asString) namespaces
      pure Ok
    \(e :: SomeException) -> do
      putStrLn $ "Error: " ++ show e
      pure OtherError
