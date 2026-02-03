{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.SCLS.Util.Diff (
  DiffCmd (..),
  DiffDepth (..),
  DiffVerbosity (..),
  runDiffCmd,
) where

import Cardano.SCLS.CDDL
import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry (..))
import Cardano.SCLS.Internal.Reader (extractNamespaceList, namespacedData)
import Cardano.SCLS.Util.Diff.Entry (DiffEntry (..))
import Cardano.SCLS.Util.Diff.Pretty (ppEditDiffEntry)
import Cardano.SCLS.Util.Diff.TermDiff (termToTree)
import Cardano.SCLS.Util.Result
import Cardano.Types.Namespace (Namespace)
import Cardano.Types.Namespace qualified as Namespace
import Codec.CBOR.FlatTerm (toFlatTerm)
import Codec.CBOR.Term (encodeTerm)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger
import Control.Monad.Trans.Resource (MonadUnliftIO, allocate, runResourceT)
import Data.ByteString (ByteString)
import Data.Either (partitionEithers)
import Data.Function ((&))
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.MemPack.Extra (ByteStringSized (..), CBORTerm (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.TreeDiff.Tree (treeDiff)
import GHC.TypeNats
import Prettyprinter.Render.Terminal (putDoc)
import Streaming qualified as S
import Streaming.Internal qualified as S
import Streaming.Prelude qualified as S
import System.IO (Handle, IOMode (ReadMode), hClose, openFile)

data DiffDepth
  = DepthSilent
  | DepthReference
  | DepthFull
  deriving (Show, Eq)

data DiffVerbosity
  = VerbosityQuiet
  | VerbosityNormal
  | VerbosityVerbose
  deriving (Show, Eq, Ord, Enum, Bounded)

data DiffCmd = DiffCmd
  { diffFirstFile :: FilePath
  , diffSecondFile :: FilePath
  , diffVerbosity :: DiffVerbosity
  , diffDepth :: DiffDepth
  , diffOnlyFirst :: Bool
  , diffNamespaces :: Maybe [Namespace]
  }
  deriving (Show, Eq)

runDiffCmd :: (MonadLogger m, MonadUnliftIO m) => [(Namespace, Int)] -> DiffCmd -> m Result
runDiffCmd namespaceKeySizes DiffCmd{..} = do
  namespacesFirst <- liftIO $ extractNamespaceList diffFirstFile
  namespacesSecond <- liftIO $ extractNamespaceList diffSecondFile
  let nsFirstSet = Set.fromList namespacesFirst
  let nsSecondSet = Set.fromList namespacesSecond
  let namespacesToCheck = selectNamespaces diffNamespaces nsFirstSet nsSecondSet
  let extraNs = Map.fromList [(Namespace.asString ns, fromIntegral size) | (ns, size) <- namespaceKeySizes]
  let allKeySizes = Map.union knownNamespaceKeySizes extraNs
  let (unknownNamespaces, namespaceKeySizesToCheck) =
        namespacesToCheck
          & partitionEithers
            . map \ns ->
              case Map.lookup (Namespace.asString ns) allKeySizes of
                Nothing -> Left ns
                Just keySize -> Right (ns, keySize)
  runResourceT $ do
    (_, handleFirst) <- allocate (openFile diffFirstFile ReadMode) hClose
    (_, handleSecond) <- allocate (openFile diffSecondFile ReadMode) hClose
    let diffs =
          S.for
            (S.each namespaceKeySizesToCheck)
            ( \(ns, keySize) ->
                case someNatVal (fromIntegral keySize) of
                  SomeNat p ->
                    diffNamespace p diffVerbosity handleFirst handleSecond ns nsFirstSet nsSecondSet
            )
    if not (null unknownNamespaces)
      then do
        when (diffVerbosity >= VerbosityNormal) do
          logErrorN $ "Unknown namespaces (no key size info): " <> T.intercalate ", " (map Namespace.asText unknownNamespaces)
        pure OtherError
      else do
        let filteredDiffs = if diffOnlyFirst then S.take 1 diffs else diffs
        nDiffs <- emitDiffOutput diffDepth filteredDiffs
        when (diffVerbosity >= VerbosityNormal) $
          logInfoN $
            "Total differences found: " <> T.pack (show nDiffs)
        pure $ if nDiffs == 0 then Ok else VerifyFailure

-- | Select only intersting namespaces.
selectNamespaces :: Maybe [Namespace] -> Set.Set Namespace -> Set.Set Namespace -> [Namespace]
selectNamespaces Nothing nsFirst nsSecond =
  sortOn Namespace.asText (Set.toList (Set.union nsFirst nsSecond))
selectNamespaces (Just nsList) nsFirst nsSecond =
  filter (\ns -> Set.member ns nsFirst || Set.member ns nsSecond) nsList

diffNamespace :: (MonadIO m, MonadLogger m, KnownNat n) => proxy n -> DiffVerbosity -> Handle -> Handle -> Namespace -> Set.Set Namespace -> Set.Set Namespace -> S.Stream (S.Of DiffEntry) m ()
diffNamespace (_ :: proxy n) verbosity handleFirst handleSecond ns nsFirst nsSecond =
  case (Set.member ns nsFirst, Set.member ns nsSecond) of
    (True, False) ->
      S.yield (DelNamespace ns)
    (False, True) ->
      S.yield (InsNamespace ns)
    (False, False) -> pure ()
    (True, True) -> do
      when (verbosity >= VerbosityVerbose) do
        S.lift $ logInfoN $ "Comparing namespace: " <> Namespace.asText ns
      let streamFirst =
            namespacedData @(ChunkEntry (ByteStringSized n) CBORTerm) handleFirst ns
      let streamSecond =
            namespacedData @(ChunkEntry (ByteStringSized n) CBORTerm) handleSecond ns
      diffStreams
        ns
        streamFirst
        streamSecond

diffStreams ::
  (Monad m) =>
  Namespace ->
  S.Stream (S.Of (ChunkEntry (ByteStringSized n) CBORTerm)) m () ->
  S.Stream (S.Of (ChunkEntry (ByteStringSized n) CBORTerm)) m () ->
  S.Stream (S.Of DiffEntry) m ()
diffStreams ns = go
 where
  go s1 s2 = do
    case (s1, s2) of
      -- Both streams have entries
      (S.Step (e1 S.:> r1), S.Step (e2 S.:> r2)) -> do
        let k1 = entryKey e1
            k2 = entryKey e2
        case compare k1 k2 of
          LT -> do
            S.yield (DelKey ns k1)
            go r1 (S.cons e2 r2)
          GT -> do
            S.yield (InsKey ns k2)
            go (S.cons e1 r1) r2
          EQ -> do
            let v1 = chunkEntryValue e1
                v2 = chunkEntryValue e2
            when (getEncodedBytes v1 /= getEncodedBytes v2) do
              let term1 = toFlatTerm $ encodeTerm $ getRawTerm v1
                  term2 = toFlatTerm $ encodeTerm $ getRawTerm v2
                  tree1 = termToTree term1
                  tree2 = termToTree term2
                  diff = treeDiff tree1 tree2
              S.yield (SwpValueTree ns k1 diff)
            go r1 r2
      -- Only first stream has entries
      (S.Step (e1 S.:> r1), S.Return _) -> do
        collectRemaining DelKey e1 r1
      -- Only second stream has entries
      (S.Return _, S.Step (e2 S.:> r2)) -> do
        collectRemaining InsKey e2 r2
      -- One stream has an effect, apply it and continue
      (S.Effect m, _) ->
        S.Effect $ fmap (\str -> go str s2) m
      (_, S.Effect m) ->
        S.Effect $ fmap (\str -> go s1 str) m
      -- Both streams are done
      (S.Return _, S.Return _) -> pure ()

  collectRemaining side entry =
    S.map (side ns . entryKey) . S.cons entry

entryKey :: ChunkEntry (ByteStringSized n) CBORTerm -> ByteString
entryKey ChunkEntry{chunkEntryKey = ByteStringSized k} = k

emitDiffOutput :: (MonadIO m) => DiffDepth -> S.Stream (S.Of DiffEntry) m () -> m Int
emitDiffOutput depth diffs =
  case depth of
    DepthSilent -> S.length_ diffs
    DepthReference -> do
      diffs
        & S.copy
        & S.mapM_
          ( \edit -> liftIO $ do
              putDoc $ ppEditDiffEntry edit
              putStrLn ""
          )
        & S.length_
    DepthFull ->
      diffs
        & S.copy
        & S.mapM_
          ( \edit -> liftIO $ do
              putDoc $ ppEditDiffEntry edit
              putStrLn ""
          )
        & S.length_
