{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | Module for checking SCLS file integrity and validity.

This module provides functionality to verify chunks in an SCLS file by:
- Checking if namespaces are known
- Validating chunk checksums
- Verifying entry counts
- Validating chunk data against CDDL schemas (for known namespaces)
-}
module Cardano.SCLS.Util.Verify (
  check,
  CheckResult (..),
  ChunkCheckResult (..),
  CheckError (..),
) where

import Cardano.SCLS.CBOR.Canonical (getRawEncoding)
import Cardano.SCLS.CBOR.Canonical.Encoder (toCanonicalCBOR)
import Cardano.SCLS.CDDL (namespaceSymbolFromText)
import Cardano.SCLS.CDDL.Validate (invalidSpecs, validSpecs)
import Cardano.SCLS.Internal.Entry.CBOREntry (GenericCBOREntry (..))
import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry (..))
import Cardano.SCLS.Internal.Hash (Digest (..), digest, digestToString)
import Cardano.SCLS.Internal.Reader (extractNamespaceList, streamChunkEntries, withRecordData)
import Cardano.SCLS.Internal.Record.Chunk (Chunk (..))
import Cardano.SCLS.NamespaceKey (NamespaceKeySize)
import Cardano.SCLS.NamespaceSymbol (SomeNamespaceSymbol (SomeNamespaceSymbol), toString)
import Cardano.SCLS.Testlib (prettyError)
import Cardano.SCLS.Util.Result
import Cardano.Types.Namespace (Namespace)
import Cardano.Types.Namespace qualified as Namespace
import Codec.CBOR.Cuddle.CBOR.Validator (validateCBOR)
import Codec.CBOR.Cuddle.CBOR.Validator.Trace (Evidenced (..), SValidity (..), ValidationTrace, Validity (..))
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot (..))
import Codec.CBOR.Cuddle.CDDL.Resolve (
  MonoReferenced,
 )
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (mapIndex))
import Codec.CBOR.Cuddle.Pretty ()
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term (Term, decodeTerm)
import Codec.CBOR.Write (toLazyByteString)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.MemPack.Extra (CBORTerm (..), Entry (..), RawBytes (..))
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32, Word64)
import Streaming.Prelude qualified as S

-- | Represents an error encountered during chunk validation.
data CheckError
  = -- | Checksum mismatch
    ChecksumMismatch
      { expectedHash :: Digest
      , actualHash :: Digest
      }
  | -- | Entry count doesn't match the actual number of entries parsed
    EntryCountMismatch
      { expectedCount :: Word32
      , actualCount :: Int
      }
  | -- | CDDL validation failed for an entry
    CDDLValidationError
      { entryIndex :: Int
      , errorResult :: ValidationTrace IsInvalid
      }
  | -- | Failed to parse CBOR data
    CBORParseError
      { entryIndex :: Int
      , errorMessage :: Text
      }
  | -- | Value encoding is not canonical
    CBORIsNotCanonicalError
      { entryIndex :: Int
      , errorExpectedCanonical :: Term
      , errorCurrentTerm :: Term
      }
  deriving (Show)

-- | Result of checking a single chunk.
data ChunkCheckResult = ChunkCheckResult
  { chunkSeqNum :: Word64
  , chunkNamespaceName :: Namespace
  , chunkIsKnown :: Bool
  , chunkErrors :: [CheckError]
  }
  deriving (Show)

-- | Overall check result for a file.
data CheckResult = CheckResult
  { totalChunks :: Int
  , passedChunks :: Int
  , failedChunks :: Int
  , unknownNamespaces :: [Namespace]
  , chunkResults :: [ChunkCheckResult]
  }
  deriving (Show)

{- | Check an SCLS file and return a lazy list of chunk check results.

This function:
1. Extracts the list of namespaces from the file
2. Identifies which namespaces are known
3. Builds CDDL validation trees for known namespaces (once)
4. Streams through all chunks and validates each one
-}
check :: (MonadIO m, MonadLogger m) => FilePath -> m Result
check filePath = do
  logDebugN $ "Checking file: " <> T.pack filePath
  fileNamespaces <- liftIO $ extractNamespaceList filePath

  liftIO $ do
    putStrLn "\n=== Namespace Analysis ==="

    unless (Map.null invalidSpecs) $ do
      putStrLn "Warning!\n Some namespaces failed to resolve CDDL schemas:"
      for_ (Map.toList invalidSpecs) $ \(ns, err) -> do
        putStrLn $ "  - Namespace: " ++ toString ns ++ ", Error: " ++ show err
      putStrLn "This should never happen, please contact upstream as the file verification may not work as intended"

    let unknownNs = filter (isNothing . namespaceSymbolFromText . Namespace.asText) fileNamespaces

    putStrLn $ "Total namespaces: " ++ show (length fileNamespaces)
    putStrLn $ "Known namespaces: " ++ show (length fileNamespaces - length unknownNs)
    putStrLn $ "Unknown namespaces: " ++ show (length unknownNs)
    when (not $ null unknownNs) $ do
      putStrLn "\nUnknown namespaces found:"
      mapM_ (putStrLn . ("  - " ++) . Namespace.asString) unknownNs

    putStrLn "\n=== Chunk Validation ==="

    results <- withRecordData filePath \stream -> do
      S.toList_ $ S.mapM validateChunk stream

    let totalCount = length results
        failedCount = length $ filter (not . null . chunkErrors) results
        passedCount = totalCount - failedCount

    putStrLn $ "\n=== Summary ==="
    putStrLn $ "Total chunks: " ++ show totalCount
    putStrLn $ "Passed: " ++ show passedCount
    putStrLn $ "Failed: " ++ show failedCount

    -- Print failed chunks
    when (failedCount > 0) $ do
      putStrLn "\n=== Failed Chunks ==="
      for_ (filter (not . null . chunkErrors) results) $ \ChunkCheckResult{..} -> do
        putStrLn $ "\nChunk #" ++ show chunkSeqNum ++ " (namespace: " ++ Namespace.asString chunkNamespaceName ++ ")"
        for_ chunkErrors $ \err -> do
          putStrLn $ "  - " ++ formatError err

    if failedCount > 0
      then pure VerifyFailure
      else pure Ok

-- | Validate a single chunk.
validateChunk :: Chunk -> IO ChunkCheckResult
validateChunk Chunk{..} = do
  let nsSymbol = namespaceSymbolFromText (Namespace.asText chunkNamespace)
      isKnown = isJust nsSymbol

  -- Check 1: Validate checksum
  let computedHash = digest chunkData
      checksumError =
        if computedHash /= chunkHash
          then [ChecksumMismatch chunkHash computedHash]
          else []

  dataErrors <- case nsSymbol >>= (\p -> fmap ((,) p) (Map.lookup p validSpecs)) of
    Nothing -> do
      -- We do not known how to decode values inside, so we just read the data
      -- this way we can calculate count and check digest
      actualCount <-
        streamChunkEntries @(Entry RawBytes) chunkData & S.length_
      pure $
        if actualCount /= fromIntegral chunkEntriesCount
          then [EntryCountMismatch chunkEntriesCount actualCount]
          else []
    Just ((SomeNamespaceSymbol (_ :: proxy ns)), spec) -> do
      (actualCount S.:> formatErrors S.:> ()) <-
        streamChunkEntries @(GenericCBOREntry (NamespaceKeySize ns)) chunkData
          & S.copy
          & S.zip (S.enumFrom 1)
          & S.mapMaybe (validateAgainst spec)
          & S.toList
          & S.length
      let countErrors =
            if actualCount /= fromIntegral chunkEntriesCount
              then [EntryCountMismatch chunkEntriesCount actualCount]
              else []
      pure (formatErrors <> countErrors)

  pure
    ChunkCheckResult
      { chunkSeqNum = chunkSeq
      , chunkNamespaceName = chunkNamespace
      , chunkIsKnown = isKnown
      , chunkErrors = checksumError <> dataErrors
      }

validateAgainst :: forall n. CTreeRoot MonoReferenced -> (Int, GenericCBOREntry n) -> Maybe CheckError
validateAgainst t v@(i, GenericCBOREntry (ChunkEntry _ cTerm)) =
  case validateCDDLAgainst t v of
    Just e -> Just e
    Nothing -> case checkCanonical of
      Nothing -> Nothing
      Just expected -> Just (CBORIsNotCanonicalError i expected (getRawTerm cTerm))
 where
  checkCanonical =
    let encodedData = toLazyByteString (getRawEncoding $ toCanonicalCBOR Proxy $ getRawTerm cTerm)
     in case deserialiseFromBytes (decodeTerm) encodedData of
          Right (_, decodedAsTerm) ->
            if getRawTerm cTerm == decodedAsTerm
              then Nothing
              else Just decodedAsTerm
          _ -> Nothing

validateCDDLAgainst :: CTreeRoot MonoReferenced -> (Int, GenericCBOREntry n) -> Maybe CheckError
validateCDDLAgainst cddl (seqNum, GenericCBOREntry (ChunkEntry _key cTerm)) =
  let name = Name (T.pack "record_entry")
   in case validateCBOR (getEncodedBytes cTerm) name (mapIndex cddl) of
        Evidenced SValid _ -> Nothing
        Evidenced SInvalid trc -> Just (CDDLValidationError seqNum trc)

-- | Format an error for display.
formatError :: CheckError -> String
formatError = \case
  ChecksumMismatch expected actual ->
    "Checksum mismatch: expected " ++ digestToString expected ++ ", got " ++ digestToString actual
  EntryCountMismatch expected actual ->
    "Entry count mismatch: expected " ++ show expected ++ ", got " ++ show actual
  CDDLValidationError idx err ->
    "CDDL validation error at entry #"
      ++ show idx
      ++ ":\n"
      ++ prettyError err
  CBORParseError idx msg ->
    "CBOR parse error at entry " ++ show idx ++ ": " ++ T.unpack msg
  CBORIsNotCanonicalError idx expected current ->
    "CBOR is not canonical at entry #"
      ++ show idx
      ++ ":\n"
      ++ "    Expected: "
      ++ show expected
      ++ "\n    Current: "
      ++ show current
