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
import Cardano.SCLS.Internal.Reader (extractNamespaceList, withNamespacedData)
import Cardano.SCLS.Util.Result
import Cardano.Types.Namespace (Namespace)
import Cardano.Types.Namespace qualified as Namespace
import Codec.CBOR.Pretty (prettyHexEnc)
import Codec.CBOR.Term (encodeTerm)
import Control.Monad (foldM, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger
import Data.Array (Array, array, listArray, (!))
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.Function ((&))
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
import GHC.TypeNats
import Streaming.Prelude qualified as S

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
      loadNamespaceEntries namespaceKeySizes verbosity fileFirst ns >>= \case
        Left _ -> pure (True, [])
        Right firstEntries ->
          loadNamespaceEntries namespaceKeySizes verbosity fileSecond ns >>= \case
            Left _ -> pure (True, [])
            Right secondEntries ->
              pure (False, diffEntries ns firstEntries secondEntries)

loadNamespaceEntries :: (MonadIO m, MonadLogger m) => Map String Int -> DiffVerbosity -> FilePath -> Namespace -> m (Either () (Map ByteString CBORTerm))
loadNamespaceEntries namespaceKeySizes verbosity filePath ns =
  case Map.lookup (Namespace.asString ns) namespaceKeySizes of
    Nothing -> do
      when (verbosity > VerbosityQuiet) do
        logErrorN $ "Unknown namespace, cannot decode entries: " <> Namespace.asText ns
      pure (Left ())
    Just p -> do
      case someNatVal (fromIntegral p) of
        SomeNat (Proxy :: Proxy n) -> do
          entries <-
            liftIO $
              withNamespacedData @(ChunkEntry (ByteStringSized n) CBORTerm) filePath ns \stream ->
                stream
                  & S.fold_
                    ( \acc ChunkEntry{chunkEntryKey = ByteStringSized k, chunkEntryValue = v} ->
                        Map.insert k v acc
                    )
                    Map.empty
                    id
          pure (Right entries)

diffEntries :: Namespace -> Map ByteString CBORTerm -> Map ByteString CBORTerm -> [DiffEntry]
diffEntries ns firstEntries secondEntries =
  let keysFirst = Map.keysSet firstEntries
      keysSecond = Map.keysSet secondEntries
      onlyFirst = Set.toAscList (Set.difference keysFirst keysSecond)
      onlySecond = Set.toAscList (Set.difference keysSecond keysFirst)
      shared = Set.toAscList (Set.intersection keysFirst keysSecond)
      onlyFirstDiffs = map (KeyOnly SideFirst ns) onlyFirst
      onlySecondDiffs = map (KeyOnly SideSecond ns) onlySecond
      valueDiffs =
        foldMap
          ( \k ->
              case (Map.lookup k firstEntries, Map.lookup k secondEntries) of
                (Just v1, Just v2) ->
                  if getEncodedBytes v1 == getEncodedBytes v2
                    then []
                    else [ValueDiff ns k v1 v2]
                _ -> []
          )
          shared
   in onlyFirstDiffs <> onlySecondDiffs <> valueDiffs

applyOnlyFirst :: Bool -> [DiffEntry] -> [DiffEntry]
applyOnlyFirst onlyFirst =
  if onlyFirst
    then filter (not . isSecondOnly)
    else id

isSecondOnly :: DiffEntry -> Bool
isSecondOnly = \case
  NamespaceOnly SideSecond _ -> True
  KeyOnly SideSecond _ _ -> True
  ValueDiff{} -> False
  NamespaceOnly SideFirst _ -> False
  KeyOnly SideFirst _ _ -> False

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
  let xLen = length xs
      yLen = length ys
      xArr = listArray (1, xLen) xs
      yArr = listArray (1, yLen) ys
      lcsArr :: Array (Int, Int) Int
      lcsArr =
        array
          ((0, 0), (xLen, yLen))
          [((i, j), lcs i j) | i <- [0 .. xLen], j <- [0 .. yLen]]
      lcs 0 _ = 0
      lcs _ 0 = 0
      lcs i j =
        if xArr ! i == yArr ! j
          then lcsArr ! (i - 1, j - 1) + 1
          else max (lcsArr ! (i - 1, j)) (lcsArr ! (i, j - 1))
      backtrack i j acc
        | i > 0 && j > 0 && xArr ! i == yArr ! j =
            backtrack (i - 1) (j - 1) (LineBoth (xArr ! i) : acc)
        | j > 0 && (i == 0 || lcsArr ! (i, j - 1) >= lcsArr ! (i - 1, j)) =
            backtrack i (j - 1) (LineSecond (yArr ! j) : acc)
        | i > 0 =
            backtrack (i - 1) j (LineFirst (xArr ! i) : acc)
        | otherwise = acc
   in backtrack xLen yLen []
