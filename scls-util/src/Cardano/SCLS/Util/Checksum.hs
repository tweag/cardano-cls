{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.SCLS.Util.Checksum (ChecksumCmd (..), runChecksumCmd) where

import Cardano.SCLS.Internal.Hash (Digest (..))
import Cardano.SCLS.Internal.Reader
import Cardano.SCLS.Util.Result
import Cardano.Types.Namespace (Namespace)
import Cardano.Types.Namespace qualified as Namespace
import Control.Exception (SomeException, catch)
import Crypto.Hash (Blake2b_224)
import Crypto.Hash.MerkleTree.Incremental qualified as MT
import Data.Function ((&))
import Data.MemPack.Extra
import Streaming.Prelude qualified as S
import System.IO (hPutStrLn, stderr)

data ChecksumCmd = ChecksumCmd
  { vcFilePath :: FilePath
  , vcNamespace :: Maybe Namespace
  , vcNoVerify :: Bool
  , vcQuiet :: Bool
  }
  deriving (Show, Eq)

runChecksumCmd :: ChecksumCmd -> IO Result
runChecksumCmd ChecksumCmd{..} =
  case vcNamespace of
    Nothing -> checksumRoot vcFilePath vcNoVerify vcQuiet
    Just namespace -> checksumNamespace vcFilePath namespace vcNoVerify vcQuiet

checksumRoot :: FilePath -> Bool -> Bool -> IO Result
checksumRoot filePath noVerify isQuiet = do
  output $ "Verifying root hash for: " ++ filePath
  catch
    do
      storedHash <- extractRootHash filePath
      if noVerify
        then do
          output $ "Hash: " ++ show storedHash
          pure Ok
        else do
          namespaces <- extractNamespaceList filePath
          computedHash <- computeRootHash filePath namespaces

          if storedHash == computedHash
            then do
              output "Root hash verification PASSED"
              output $ "Hash: " ++ show storedHash
              pure Ok
            else do
              output "Root hash verification FAILED"
              output $ "Expected: " ++ show storedHash
              output $ "Computed: " ++ show computedHash
              pure VerifyFailure
    \(e :: SomeException) -> do
      outputErr $ "Error: " ++ show e
      pure OtherError
 where
  output
    | isQuiet = \_ -> pure ()
    | otherwise = putStrLn
  outputErr
    | isQuiet = \_ -> pure ()
    | otherwise = hPutStrLn stderr

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

checksumNamespace :: FilePath -> Namespace -> Bool -> Bool -> IO Result
checksumNamespace filePath namespace noVerify isQuiet = do
  output $ "Verifying hash for namespace: " ++ Namespace.asString namespace
  catch
    do
      extractNamespaceHash namespace filePath >>= \case
        Nothing -> do
          output $ "Namespace not found"
          pure VerifyFailure
        Just storedHash -> do
          if noVerify
            then do
              output $ "Hash: " ++ show storedHash
              pure Ok
            else do
              computedHash <- computeNamespaceHash filePath namespace
              if storedHash == computedHash
                then do
                  output $ "Namespace hash verification PASSED"
                  output $ "Hash: " ++ show storedHash
                  pure Ok
                else do
                  output $ "Namespace hash verification FAILED"
                  output $ "Expected: " ++ show storedHash
                  output $ "Computed: " ++ show computedHash
                  pure VerifyFailure
    \(e :: SomeException) -> do
      outputErr $ "Error: " ++ show e
      pure OtherError
 where
  output
    | isQuiet = \_ -> pure ()
    | otherwise = putStrLn
  outputErr
    | isQuiet = \_ -> pure ()
    | otherwise = hPutStrLn stderr

computeNamespaceHash :: FilePath -> Namespace -> IO Digest
computeNamespaceHash filePath namespace = do
  withNamespacedData filePath namespace \stream -> do
    stream
      & S.fold_
        (\acc (RawBytes bs) -> MT.add acc bs)
        (MT.empty undefined)
        (Digest . MT.merkleRootHash . MT.finalize)
