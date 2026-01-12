{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.SCLS.Util.File.Info (listNamespaces, displayInfo) where

import Cardano.SCLS.Internal.Reader
import Cardano.SCLS.Internal.Record.Manifest
import Cardano.SCLS.Util.Result
import Cardano.Types.Namespace qualified as Namespace
import Control.Monad.Catch (MonadCatch (..), SomeException (..))
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

-- TODO: return display info

displayInfo :: (MonadCatch m, MonadLogger m, MonadIO m) => FilePath -> m Result
displayInfo filePath = do
  logInfoN $ "File: " <> T.pack filePath
  catch
    do
      Manifest{..} <- liftIO do
        withLatestManifestFrame pure filePath

      liftIO do
        putStrLn "\n=== File Information ==="
        putStrLn $ "Root Hash: " ++ show rootHash
        putStrLn $ "Total Entries: " ++ show totalEntries
        putStrLn $ "Total Chunks: " ++ show totalChunks
        putStrLn "\n=== Namespaces ==="

      if null nsInfo
        then liftIO $ putStrLn "(No namespaces)"
        else do
          mapM_
            ( \(ns, NamespaceInfo{..}) -> liftIO do
                putStrLn $ "\n" ++ Namespace.asString ns ++ ":"
                putStrLn $ "  Hash: " ++ show namespaceHash
                putStrLn $ "  Entries: " ++ show namespaceEntries
                putStrLn $ "  Chunks: " ++ show namespaceChunks
            )
            $ Map.toList nsInfo

      pure Ok
    \(e :: SomeException) -> do
      logErrorN $ "Error: " <> T.pack (show e)
      pure OtherError

listNamespaces :: (MonadLogger m, MonadIO m, MonadCatch m) => FilePath -> m Result
listNamespaces filePath = do
  catch
    do
      namespaces <- liftIO do
        extractNamespaceList filePath
      if null namespaces
        then logInfoN "No namespaces found"
        else liftIO do
          putStrLn "Namespaces:"
          mapM_ (putStrLn . ("  - " <>) . Namespace.asString) namespaces
      pure Ok
    \(e :: SomeException) -> do
      logErrorN $ "Error: " <> T.pack (show e)
      pure OtherError
