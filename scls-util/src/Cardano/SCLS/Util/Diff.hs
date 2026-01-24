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
import Control.Monad (foldM, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.List (sortOn)
import Data.Map.Strict (Map)
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
import Streaming.Prelude qualified as S
import System.IO (IOMode (ReadMode), withBinaryFile)

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

runDiffCmd :: (MonadIO m, MonadLogger m) => [(Namespace, Int)] -> DiffCmd -> m Result
runDiffCmd namespaceKeySizes DiffCmd{..} = do
  namespacesFirst <- liftIO $ extractNamespaceList diffFirstFile
  namespacesSecond <- liftIO $ extractNamespaceList diffSecondFile
  let nsFirstSet = Set.fromList namespacesFirst
  let nsSecondSet = Set.fromList namespacesSecond
  let namespacesToCheck = selectNamespaces diffNamespaces nsFirstSet nsSecondSet
  let extraNs = Map.fromList [(Namespace.asString ns, fromIntegral size) | (ns, size) <- namespaceKeySizes]
  (hadError, diffs) <-
    foldM
      ( \(anyError, acc) ns -> do
          (err, nsDiffs) <- diffNamespace (Map.union knownNamespaceKeySizes extraNs) diffVerbosity diffFirstFile diffSecondFile ns nsFirstSet nsSecondSet
          pure (anyError || err, acc <> nsDiffs)
      )
      (False, [])
      namespacesToCheck
  let filteredDiffs = applyOnlyFirst diffOnlyFirst diffs
  when (diffVerbosity >= VerbosityNormal && diffDepth /= DepthSilent) do
    logInfoN $ "Diff entries: " <> T.pack (show (length filteredDiffs))
  liftIO $ emitDiffOutput diffDepth diffFirstFile diffSecondFile filteredDiffs
  if hadError || not (null filteredDiffs)
    then pure OtherError
    else pure $ if null filteredDiffs then Ok else VerifyFailure

-- | Select only intersting namespaces.
selectNamespaces :: Maybe [Namespace] -> Set.Set Namespace -> Set.Set Namespace -> [Namespace]
selectNamespaces Nothing nsFirst nsSecond =
  sortOn Namespace.asText (Set.toList (Set.union nsFirst nsSecond))
selectNamespaces (Just nsList) nsFirst nsSecond =
  filter (\ns -> Set.member ns nsFirst || Set.member ns nsSecond) nsList

diffNamespace :: (MonadIO m, MonadLogger m) => Map String Int -> DiffVerbosity -> FilePath -> FilePath -> Namespace -> Set.Set Namespace -> Set.Set Namespace -> m (Bool, [DiffEntry])
diffNamespace namespaceKeySizes verbosity fileFirst fileSecond ns nsFirst nsSecond =
  case (Set.member ns nsFirst, Set.member ns nsSecond) of
    (True, False) -> pure (False, [NamespaceOnly SideFirst ns])
    (False, True) -> pure (False, [NamespaceOnly SideSecond ns])
    (False, False) -> pure (False, [])
    (True, True) -> do
      when (verbosity >= VerbosityVerbose) do
        logInfoN $ "Comparing namespace: " <> Namespace.asText ns
      case Map.lookup (Namespace.asString ns) namespaceKeySizes of
        Nothing -> do
          when (verbosity > VerbosityQuiet) do
            logErrorN $ "Unknown namespace, cannot decode entries: " <> Namespace.asText ns
          pure (True, [])
        Just p -> case someNatVal (fromIntegral p) of
          SomeNat (Proxy :: Proxy n) -> do
            diffs <-
              liftIO $
                withBinaryFile fileFirst ReadMode \handleFirst ->
                  withBinaryFile fileSecond ReadMode \handleSecond ->
                    diffStreams
                      ns
                      (namespacedData @(ChunkEntry (ByteStringSized n) CBORTerm) handleFirst ns)
                      (namespacedData @(ChunkEntry (ByteStringSized n) CBORTerm) handleSecond ns)
            pure (False, diffs)

diffStreams ::
  forall n.
  Namespace ->
  S.Stream (S.Of (ChunkEntry (ByteStringSized n) CBORTerm)) IO () ->
  S.Stream (S.Of (ChunkEntry (ByteStringSized n) CBORTerm)) IO () ->
  IO [DiffEntry]
diffStreams ns streamFirst streamSecond = go streamFirst streamSecond []
 where
  go s1 s2 acc = do
    next1 <- S.next s1
    next2 <- S.next s2
    case (next1, next2) of
      (Left _, Left _) -> pure (reverse acc)
      (Left _, Right (e2, r2)) -> do
        rest <- collectRemaining SideSecond e2 r2 acc
        pure (reverse rest)
      (Right (e1, r1), Left _) -> do
        rest <- collectRemaining SideFirst e1 r1 acc
        pure (reverse rest)
      (Right (e1, r1), Right (e2, r2)) ->
        case compare (entryKey e1) (entryKey e2) of
          LT -> go r1 (S.cons e2 r2) (KeyOnly SideFirst ns (entryKey e1) : acc)
          GT -> go (S.cons e1 r1) r2 (KeyOnly SideSecond ns (entryKey e2) : acc)
          EQ ->
            let acc' =
                  if getEncodedBytes (chunkEntryValue e1) == getEncodedBytes (chunkEntryValue e2)
                    then acc
                    else ValueDiff ns (entryKey e1) (chunkEntryValue e1) (chunkEntryValue e2) : acc
             in go r1 r2 acc'

  collectRemaining side entry rest acc = do
    acc' <-
      S.foldM_
        (\a e -> pure (KeyOnly side ns (entryKey e) : a))
        (pure (KeyOnly side ns (entryKey entry) : acc))
        pure
        rest
    pure acc'

entryKey :: ChunkEntry (ByteStringSized n) CBORTerm -> ByteString
entryKey ChunkEntry{chunkEntryKey = ByteStringSized k} = k

applyOnlyFirst :: Bool -> [DiffEntry] -> [DiffEntry]
applyOnlyFirst True [] = []
applyOnlyFirst True (x : _) = [x]
applyOnlyFirst False diffs = diffs

emitDiffOutput :: DiffDepth -> FilePath -> FilePath -> [DiffEntry] -> IO ()
emitDiffOutput depth fileFirst fileSecond diffs =
  case depth of
    DepthSilent ->
      putStrLn $ if null diffs then "OK" else "DIFF"
    DepthReference ->
      mapM_ (TIO.putStrLn . renderReference) diffs
    DepthFull ->
      mapM_ (renderFull fileFirst fileSecond) diffs

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
