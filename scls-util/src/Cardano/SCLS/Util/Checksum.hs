{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.SCLS.Util.Checksum (ChecksumCmd (..), runChecksumCmd) where

import Cardano.SCLS.Internal.Hash (Digest (..), digestToString)
import Cardano.SCLS.Internal.Reader
import Cardano.SCLS.Util.Result
import Cardano.Types.Namespace (Namespace)
import Cardano.Types.Namespace qualified as Namespace
import Control.Monad.Catch (MonadCatch, SomeException, catch)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Crypto.Hash (Blake2b_224)
import Crypto.Hash.MerkleTree.Incremental qualified as MT
import Data.Function ((&))
import Data.MemPack.Extra
import Data.Text qualified as T
import Streaming.Prelude qualified as S

data ChecksumCmd = ChecksumCmd
  { vcFilePath :: FilePath
  , vcNamespace :: Maybe Namespace
  , vcNoVerify :: Bool
  , vcQuiet :: Bool
  }
  deriving (Show, Eq)

runChecksumCmd :: (MonadIO m, MonadLogger m, MonadCatch m) => ChecksumCmd -> m Result
runChecksumCmd ChecksumCmd{..} =
  case vcNamespace of
    Nothing -> checksumRoot vcFilePath vcNoVerify
    Just namespace -> checksumNamespace vcFilePath namespace vcNoVerify

checksumRoot :: (MonadIO m, MonadLogger m, MonadCatch m) => FilePath -> Bool -> m Result
checksumRoot filePath noVerify = do
  logDebugN $ "Verifying root hash for: " <> T.pack filePath
  catch
    do
      storedHash <- liftIO $ extractRootHash filePath
      if noVerify
        then do
          liftIO $ putStrLn $ "Hash: " ++ digestToString storedHash
          pure Ok
        else do
          namespaces <- liftIO $ extractNamespaceList filePath
          computedHash <- liftIO $ computeRootHash filePath namespaces

          if storedHash == computedHash
            then do
              logInfoN "Root hash verification PASSED"
              liftIO $ putStrLn $ "Hash: " ++ digestToString storedHash
              pure Ok
            else do
              logErrorN "Root hash verification FAILED"
              logErrorN $ "Expected: " <> T.pack (digestToString storedHash)
              logErrorN $ "Computed: " <> T.pack (digestToString computedHash)
              pure VerifyFailure
    \(e :: SomeException) -> do
      logErrorN $ "Error: " <> T.pack (show e)
      pure OtherError
 where

computeRootHash :: FilePath -> [Namespace] -> IO Digest
computeRootHash filePath namespaces = do
  finalState <- foldl' (combineNamespaceHash filePath) (pure $ MT.empty (undefined :: Blake2b_224)) namespaces
  pure $ Digest $ MT.merkleRootHash $ MT.finalize finalState
 where
  combineNamespaceHash :: FilePath -> IO (MT.MerkleTreeState Blake2b_224) -> Namespace -> IO (MT.MerkleTreeState Blake2b_224)
  combineNamespaceHash fp stateIO ns = do
    state <- stateIO
    nsHash <- computeNamespaceHash fp ns
    pure $ MT.add state nsHash

checksumNamespace :: (MonadIO m, MonadLogger m, MonadCatch m) => FilePath -> Namespace -> Bool -> m Result
checksumNamespace filePath namespace noVerify = do
  logDebugN $ "Verifying hash for namespace: " <> Namespace.asText namespace
  catch
    do
      liftIO (extractNamespaceHash namespace filePath) >>= \case
        Nothing -> do
          logErrorN $ "Namespace not found"
          pure VerifyFailure
        Just storedHash -> do
          if noVerify
            then do
              liftIO $ putStrLn $ "Hash: " ++ digestToString storedHash
              pure Ok
            else do
              computedHash <- liftIO do
                computeNamespaceHash filePath namespace
              if storedHash == computedHash
                then do
                  logDebugN $ "Namespace hash verification PASSED"
                  liftIO $ putStrLn $ "Hash: " ++ digestToString storedHash
                  pure Ok
                else do
                  logErrorN $ "Namespace hash verification FAILED"
                  logErrorN $ "Expected: " <> T.pack (digestToString storedHash)
                  logErrorN $ "Computed: " <> T.pack (digestToString computedHash)
                  pure VerifyFailure
    \(e :: SomeException) -> do
      logErrorN $ "Error: " <> T.pack (show e)
      pure OtherError
 where

computeNamespaceHash :: FilePath -> Namespace -> IO Digest
computeNamespaceHash filePath namespace = do
  withNamespacedData filePath namespace \stream -> do
    stream
      & S.fold_
        (\acc (RawBytes bs) -> MT.add acc bs)
        (MT.empty undefined)
        (Digest . MT.merkleRootHash . MT.finalize)
