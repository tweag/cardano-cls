{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
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
import Cardano.SCLS.CDDL (NamespaceInfo (..), namespaces)
import Cardano.SCLS.Internal.Entry.CBOREntry (GenericCBOREntry (..))
import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry (..))
import Cardano.SCLS.Internal.Hash (Digest (..), digest, digestToString)
import Cardano.SCLS.Internal.Reader (extractNamespaceList, streamChunkEntries, withRecordData)
import Cardano.SCLS.Internal.Record.Chunk (Chunk (..))
import Cardano.SCLS.Util.Result
import Cardano.Types.Namespace (Namespace)
import Cardano.Types.Namespace qualified as Namespace
import Codec.CBOR.Cuddle.CBOR.Validator (CBORTermResult (..), CDDLResult (..), validateCBOR)
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot (..))
import Codec.CBOR.Cuddle.CDDL.Resolve (
  MonoReferenced,
  NameResolutionFailure (..),
  asMap,
  buildMonoCTree,
  buildRefCTree,
  buildResolvedCTree,
 )
import Codec.CBOR.Cuddle.Huddle (toCDDL)
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (mapIndex), mapCDDLDropExt)
import Codec.CBOR.Cuddle.Pretty ()
import Codec.CBOR.Pretty
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term (Term, decodeTerm, encodeTerm)
import Codec.CBOR.Write (toLazyByteString)
import Control.Exception (SomeException, catch)
import Control.Monad (unless, when)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.MemPack.Extra (CBORTerm (..), Entry (..), RawBytes (..))
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32, Word64)
import GHC.TypeLits hiding (withSomeSNat)
import GHC.TypeNats (withSomeSNat)
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
      , errorResult :: CDDLResult
      , errorTerm :: Term
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
check :: FilePath -> IO Result
check filePath = do
  putStrLn $ "Checking file: " ++ filePath
  catch
    do
      fileNamespaces <- extractNamespaceList filePath

      putStrLn "\n=== Namespace Analysis ==="
      let (failedNamespaces, knownNamespaces) = processNamespaceInfo namespaces

      unless (Map.null failedNamespaces) $ do
        putStrLn "Warning!\n Some namespaces failed to resolve CDDL schemas:"
        for_ (Map.toList failedNamespaces) $ \(ns, err) -> do
          putStrLn $ "  - Namespace: " ++ T.unpack ns ++ ", Error: " ++ show err
        putStrLn "It should never be happen, please contact upstream as the file verification may not work as inteded"

      let unknownNs = filter (\ns -> not $ Map.member (Namespace.asText ns) knownNamespaces) fileNamespaces

      putStrLn $ "Total namespaces: " ++ show (length fileNamespaces)
      putStrLn $ "Known namespaces: " ++ show (length fileNamespaces - length unknownNs)
      putStrLn $ "Unknown namespaces: " ++ show (length unknownNs)
      when (not $ null unknownNs) $ do
        putStrLn "\nUnknown namespaces found:"
        mapM_ (putStrLn . ("  - " ++) . Namespace.asString) unknownNs

      putStrLn "\n=== Chunk Validation ==="

      results <- withRecordData filePath \stream -> do
        S.toList_ $ S.mapM (validateChunk knownNamespaces) stream

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
    \(e :: SomeException) -> do
      putStrLn $ "Error: " ++ show e
      pure OtherError

-- | Build CDDL validation trees for all namespaces, skipping any that fail to compile.
processNamespaceInfo :: Map.Map Text NamespaceInfo -> (Map.Map Text NameResolutionFailure, Map.Map Text (CTreeRoot MonoReferenced, Natural))
processNamespaceInfo =
  Map.mapEitherWithKey \_namespace NamespaceInfo{..} -> do
    case buildMonoCTree =<< buildResolvedCTree (buildRefCTree $ asMap $ mapCDDLDropExt $ toCDDL namespaceSpec) of
      Left err -> Left err
      Right tree -> Right (tree, namespaceKeySize)

-- | Validate a single chunk.
validateChunk :: Map.Map Text (CTreeRoot MonoReferenced, Natural) -> Chunk -> IO ChunkCheckResult
validateChunk cddlTrees Chunk{..} = do
  let nsString = Namespace.asText chunkNamespace
      isKnown = Map.member nsString cddlTrees

  -- Check 1: Validate checksum
  let computedHash = digest chunkData
      checksumError =
        if computedHash /= chunkHash
          then [ChecksumMismatch chunkHash computedHash]
          else []

  dataErrors <- case Map.lookup nsString cddlTrees of
    Nothing -> do
      -- We do not known how to decode values inside, so we just read the data
      -- this way we can calculate count and check digest
      actualCount <-
        streamChunkEntries @(Entry RawBytes) chunkData & S.length_
      pure $
        if actualCount /= fromIntegral chunkEntriesCount
          then [EntryCountMismatch chunkEntriesCount actualCount]
          else []
    Just (spec, keySize) ->
      withSomeSNat keySize \(snat :: SNat n) -> do
        withKnownNat snat do
          (actualCount S.:> formatErrors S.:> ()) <-
            streamChunkEntries @(GenericCBOREntry n) chunkData
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
        CBORTermResult _term Valid{} -> Nothing
        CBORTermResult bad_term problem -> Just (CDDLValidationError seqNum problem bad_term)

-- | Format an error for display.
formatError :: CheckError -> String
formatError = \case
  ChecksumMismatch expected actual ->
    "Checksum mismatch: expected " ++ digestToString expected ++ ", got " ++ digestToString actual
  EntryCountMismatch expected actual ->
    "Entry count mismatch: expected " ++ show expected ++ ", got " ++ show actual
  CDDLValidationError idx err term ->
    "CDDL validation error at entry #"
      ++ show idx
      ++ ":\n"
      ++ prettyError err
      ++ "\n\nData:\n"
      ++ prettyHexEnc (encodeTerm term)
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

prettyError :: CDDLResult -> String
prettyError Valid{} = "sorry, everything is valid"
prettyError (ChoiceFail rule _alt alt_result) =
  "Choice failed for rule: "
    ++ (show rule)
    ++ "\n"
    ++ "Alternatives:\n"
    ++ intercalate
      "\n"
      (map (\(r, res) -> " - " <> (show r) <> ": " <> prettyErrorShort res) $ NE.toList alt_result)
prettyError (ListExpansionFail rule _expansions _expasions_with_result) =
  "List expansion failed for rule: " ++ (show rule) ++ "\n"
prettyError (MapExpansionFail rule _expansions _expasions_with) =
  "Map expansion failed for rule: " ++ (show rule) ++ "\n"
prettyError (InvalidControl rule _result) =
  "The rule was valid but the control failed: " ++ (show rule) ++ "\n"
prettyError (InvalidRule rule) =
  "The rule is invalid: " ++ (show rule) ++ "\n"
prettyError (InvalidTagged rule _result) =
  "The tagged rule is invalid: " ++ (show rule) ++ "\n"
prettyError (UnapplicableRule _info rule) =
  "The rule is unapplicable: " ++ (show rule) ++ "\n"

prettyErrorShort :: CDDLResult -> String
prettyErrorShort Valid{} = "Valid"
prettyErrorShort ChoiceFail{} = "choice failed"
prettyErrorShort ListExpansionFail{} = "list expansion failed"
prettyErrorShort MapExpansionFail{} = "map expansion failed"
prettyErrorShort InvalidControl{} = "invalid control"
prettyErrorShort InvalidRule{} = "invalid rule"
prettyErrorShort InvalidTagged{} = "invalid tagged"
prettyErrorShort UnapplicableRule{} = "unapplicable rule"
