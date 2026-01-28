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
import Cardano.SCLS.Util.Result
import Cardano.Types.Namespace (Namespace)
import Cardano.Types.Namespace qualified as Namespace
import Codec.CBOR.Pretty (prettyHexEnc)
import Codec.CBOR.Term (encodeTerm)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger
import Control.Monad.Trans.Resource (MonadUnliftIO, allocate, runResourceT)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.Either (partitionEithers)
import Data.Function ((&))
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.MemPack.Extra (ByteStringSized (..), CBORTerm (..))
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as TIO
import Data.TreeDiff.List (Edit (..), diffBy)
import GHC.TypeNats
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

data DiffSide = SideFirst | SideSecond
  deriving (Show, Eq)

data DiffEntry
  = NamespaceOnly DiffSide Namespace
  | KeyOnly DiffSide Namespace ByteString
  | ValueDiff Namespace ByteString CBORTerm CBORTerm
  deriving (Show)

data DiffLine
  = LineBoth Text
  | LineFirst Text
  | LineSecond Text
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
            (\(ns, keySize) -> diffNamespace keySize diffVerbosity handleFirst handleSecond ns nsFirstSet nsSecondSet)
    if not (null unknownNamespaces) && diffVerbosity >= VerbosityNormal
      then do
        logErrorN $ "Unknown namespaces (no key size info): " <> T.intercalate ", " (map Namespace.asText unknownNamespaces)
        pure OtherError
      else do
        let filteredDiffs = if diffOnlyFirst then S.take 1 diffs else diffs
        nDiffs <- emitDiffOutput diffDepth diffFirstFile diffSecondFile filteredDiffs
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

diffNamespace :: (MonadIO m, MonadLogger m) => Int -> DiffVerbosity -> Handle -> Handle -> Namespace -> Set.Set Namespace -> Set.Set Namespace -> S.Stream (S.Of DiffEntry) m ()
diffNamespace keySize verbosity handleFirst handleSecond ns nsFirst nsSecond =
  case (Set.member ns nsFirst, Set.member ns nsSecond) of
    (True, False) -> S.yield (NamespaceOnly SideFirst ns)
    (False, True) -> S.yield (NamespaceOnly SideSecond ns)
    (False, False) -> pure ()
    (True, True) -> do
      case someNatVal (fromIntegral keySize) of
        SomeNat (Proxy :: Proxy n) -> do
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
            S.yield (KeyOnly SideFirst ns k1)
            go r1 (S.cons e2 r2)
          GT -> do
            S.yield (KeyOnly SideSecond ns k2)
            go (S.cons e1 r1) r2
          EQ -> do
            let v1 = chunkEntryValue e1
                v2 = chunkEntryValue e2
            when (getEncodedBytes v1 /= getEncodedBytes v2) do
              S.yield (ValueDiff ns k1 v1 v2)
            go r1 r2
      -- Only first stream has entries
      (S.Step (e1 S.:> r1), S.Return _) -> do
        collectRemaining SideFirst e1 r1
      -- Only second stream has entries
      (S.Return _, S.Step (e2 S.:> r2)) -> do
        collectRemaining SideSecond e2 r2
      -- One stream has an effect, apply it and continue
      (S.Effect m, _) ->
        S.Effect $ fmap (\str -> go str s2) m
      (_, S.Effect m) ->
        S.Effect $ fmap (\str -> go s1 str) m
      -- Both streams are done
      (S.Return _, S.Return _) -> pure ()

  collectRemaining side entry rest = do
    S.yield (KeyOnly side ns (entryKey entry))
      <> S.map
        (\e -> KeyOnly side ns (entryKey e))
        rest

entryKey :: ChunkEntry (ByteStringSized n) CBORTerm -> ByteString
entryKey ChunkEntry{chunkEntryKey = ByteStringSized k} = k

emitDiffOutput :: (MonadIO m) => DiffDepth -> FilePath -> FilePath -> S.Stream (S.Of DiffEntry) m () -> m Int
emitDiffOutput depth fileFirst fileSecond diffs =
  case depth of
    DepthSilent -> S.length_ diffs
    DepthReference ->
      S.mapM_ (liftIO . TIO.putStrLn . renderReference) $ S.length_ $ S.copy $ diffs
    DepthFull ->
      S.mapM_ (liftIO . renderFull fileFirst fileSecond) $ S.length_ $ S.copy $ diffs

renderReference :: DiffEntry -> Text
renderReference = \case
  NamespaceOnly SideFirst ns -> "- " <> Namespace.asText ns
  NamespaceOnly SideSecond ns -> "+ " <> Namespace.asText ns
  KeyOnly SideFirst ns key -> "- " <> renderKeyRef ns key
  KeyOnly SideSecond ns key -> "+ " <> renderKeyRef ns key
  ValueDiff ns key _ _ -> "* " <> renderKeyRef ns key

renderFull :: FilePath -> FilePath -> DiffEntry -> IO ()
renderFull fileFirst fileSecond diffEntry =
  case diffEntry of
    ValueDiff ns key v1 v2 -> do
      TIO.putStrLn $ "* " <> renderKeyRef ns key
      TIO.putStrLn $ "--- " <> T.pack fileFirst
      TIO.putStrLn $ "+++ " <> T.pack fileSecond
      let linesFirst = prettyCBORLines v1
      let linesSecond = prettyCBORLines v2
      mapM_ (TIO.putStrLn . renderDiffLine) (diffLines linesFirst linesSecond)
    _ -> TIO.putStrLn (renderReference diffEntry)

renderKeyRef :: Namespace -> ByteString -> Text
renderKeyRef ns key =
  Namespace.asText ns <> "/" <> decodeUtf8 (Base16.encode key)

prettyCBORLines :: CBORTerm -> [Text]
prettyCBORLines term =
  T.lines $ T.pack $ prettyHexEnc $ encodeTerm $ getRawTerm term

renderDiffLine :: DiffLine -> Text
renderDiffLine = \case
  LineBoth t -> "  " <> t
  LineFirst t -> "- " <> t
  LineSecond t -> "+ " <> t

diffLines :: [Text] -> [Text] -> [DiffLine]
diffLines xs ys =
  foldMap renderEdit (diffBy (==) xs ys)
 where
  renderEdit = \case
    Cpy t -> [LineBoth t]
    Del t -> [LineFirst t]
    Ins t -> [LineSecond t]
    Swp t1 t2 -> [LineFirst t1, LineSecond t2]
