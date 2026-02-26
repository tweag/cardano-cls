{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.SCLS.Internal.Serializer.Dump (
  DataStream (..),
  dumpToHandle,
  serialize,
) where

import Cardano.SCLS.Internal.Frame
import Cardano.SCLS.Internal.Hash (Digest (..))
import Cardano.SCLS.Internal.Record.Chunk
import Cardano.SCLS.Internal.Record.Hdr
import Cardano.SCLS.Internal.Record.Manifest
import Cardano.SCLS.Internal.Record.Metadata
import Cardano.SCLS.Internal.Serializer.ChunksBuilder.InMemory qualified as CB
import Cardano.SCLS.Internal.Serializer.Dump.Plan
import Cardano.SCLS.Internal.Serializer.HasKey (HasKey (..))
import Cardano.SCLS.Internal.Serializer.MetadataBuilder.InMemory qualified as MB
import Cardano.Types.Namespace (Namespace, asString)
import Cardano.Types.SlotNo
import Crypto.Hash.MerkleTree.Incremental qualified as MT

import Data.Foldable qualified as F
import Data.Function ((&))
import Data.Map (Map)
import Data.Map.Strict qualified as Map

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource, ResIO, allocate, release)
import Data.Maybe (fromMaybe)
import Data.MemPack
import Data.MemPack.Buffer (pinnedByteArrayToByteString)
import Data.MemPack.Extra
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), formatShow)
import Data.Typeable (Typeable)
import Data.Word (Word32, Word64)
import Streaming (Of (..))
import Streaming.Internal (Stream (..))
import Streaming.Prelude qualified as S
import System.Directory (removeFile)
import System.IO (Handle, IOMode (WriteMode), hClose, openBinaryFile)

{- | A stream of values grouped by namespace.

Each element of the outer stream is a pair of:
  * a 'Text' namespace identifier
  * a stream of values of type @a@ belonging to that namespace.

Constraints:
  * Each namespace appears at most once in the stream (no duplicate namespaces).
  * The values within each namespace stream are ordered as they appear.
  * The stream may be empty.

This type is used as input to chunked serialization routines, which expect the data to be grouped and ordered as described.
-}
newtype DataStream a m = DataStream {runDataStream :: ChunkStream a m}

{- | Dumps data to the handle, while splitting it into chunks.

This is reference implementation and it does not yet care about
proper working with the hardware, i.e. flushing and calling fsync
at the right moments.
-}
dumpToHandle ::
  (HasKey a, MemPack a, Typeable a, MemPackHeaderOffset a, MonadResource m) =>
  -- | Handle to write to
  Handle ->
  -- | Slot of the current transaction
  SlotNo ->
  -- | File header
  Hdr ->
  -- | Key sizes for namespaces
  Map String Int ->
  -- | Serialization plan to use
  SortedSerializationPlan a m ->
  -- | Returns either a list of unknown namespaces or a success unit
  m (Either [Namespace] ())
dumpToHandle handle slotNo hdr namespaceKeySizes sortedPlan = do
  let plan@SerializationPlan{..} = getSerializationPlan sortedPlan
  _ <- liftIO $ hWriteFrame handle hdr
  manifestDataOrUnknownNamespaces <-
    pChunkStream -- output our sorted stream
      & S.mapM
        ( \(namespace :> inner) -> do
            case Map.lookup (asString namespace) namespaceKeySizes of
              Nothing -> pure $ Left namespace
              Just keySize ->
                inner
                  & dedup
                  & constructChunks_ (namespace, pChunkFormat) pBufferSize -- compose entries into data for chunks records, returns digest of entries
                  & S.copy
                  & storeToHandle namespace (fromIntegral keySize) -- stores data to handle,passes digest of entries
                  & S.map CB.chunkItemEntriesCount -- keep only number of entries (other things are not needed)
                  & S.copy
                  & S.length -- returns number of chunks
                  & S.sum -- returns number of entries
                  & fmap (Right . (namespace,))
        )
      & S.fold_
        do
          \(unknownNamespaces, mInfo) ->
            \case
              Right (namespace, (entries :> (chunks :> rootHash))) ->
                let ni =
                      NamespaceInfo
                        { namespaceEntries = fromIntegral entries
                        , namespaceChunks = fromIntegral chunks
                        , namespaceHash = rootHash
                        }
                 in (unknownNamespaces, Map.insert namespace ni mInfo)
              Left ns -> (ns : unknownNamespaces, mInfo)
        mempty
        ( \case
            ([], mInfo) ->
              Right (ManifestInfo mInfo)
            (unknownNamespaces, _) ->
              Left unknownNamespaces
        )

  case manifestDataOrUnknownNamespaces of
    Left unknownNamespaces -> pure $ Left unknownNamespaces
    Right manifestData -> do
      case pMetadataStream of
        Nothing -> pure ()
        Just s -> do
          _rootHash <- -- TODO: parametrize builder machine to customize accumulator operation (replace hash computation with something else)
            s
              & constructMetadata_ pBufferSize -- compose entries into data for metadata records, returns digest of entries
              & S.map metadataToRecord
              & S.mapM_ (liftIO . hWriteFrame handle)
          pure ()

      manifest <- liftIO $ mkManifest slotNo manifestData plan
      _ <- liftIO $ hWriteFrame handle manifest
      pure $ Right ()
 where
  storeToHandle :: (MonadIO m) => Namespace -> Word32 -> Stream (Of CB.ChunkItem) m r -> m r
  storeToHandle namespace keySize s =
    s
      & S.zip (S.enumFrom 1)
      & S.map (chunkToRecord namespace keySize)
      & S.mapM_ (liftIO . hWriteFrame handle)

  chunkToRecord :: Namespace -> Word32 -> (Word64, CB.ChunkItem) -> Chunk
  chunkToRecord namespace keySize (seqno, CB.ChunkItem{..}) =
    Chunk
      seqno
      chunkItemFormat
      namespace
      keySize
      (pinnedByteArrayToByteString chunkItemData)
      (fromIntegral chunkItemEntriesCount)
      chunkItemHash

  metadataToRecord :: MB.MetadataItem -> Metadata
  metadataToRecord MB.MetadataItem{..} =
    mkMetadata
      (pinnedByteArrayToByteString metadataItemData)
      (fromIntegral metadataItemEntriesCount)
      metadataItemHash

-- | Serializes data to a file.
serialize ::
  (MemPack b, Typeable b, HasKey b, MemPackHeaderOffset b) =>
  -- | path to resulting file
  FilePath ->
  -- | Slot of the current transaction
  SlotNo ->
  -- | Key sizes for namespaces
  Map String Int ->
  -- | Serialization plan to use
  SerializationPlan a ResIO ->
  -- | Sorting logic to apply to the data stream
  (ChunkStream a ResIO -> ChunkStream b ResIO) ->
  ResIO (Either [Namespace] ())
serialize resultFilePath slotNo namespaceKeySizes plan sortStream = do
  (key, handle) <- allocate (openBinaryFile resultFilePath WriteMode) hClose
  result <-
    dumpToHandle handle slotNo mkHdr namespaceKeySizes $
      mkSortedSerializationPlan
        plan
        sortStream
  case result of
    Left unknownNamespaces -> do
      -- cleanup partial file
      release key
      liftIO $ removeFile resultFilePath
      pure $ Left unknownNamespaces
    Right () -> pure $ Right ()

dedup ::
  (HasKey a, Monad m) =>
  Stream (Of a) m r ->
  Stream (Of a) m r
dedup s0 = initialize s0
 where
  initialize (Return r) = Return r
  initialize (Effect e) = Effect (initialize <$> e)
  initialize (Step (x :> rest)) = S.yield x >> go (getKey x) rest
  go _ (Return r) = return r
  go p (Effect e) = Effect (go p <$> e)
  go p (Step (x :> rest)) =
    let currentKey = getKey x
     in if p == currentKey
          then go p rest
          else S.yield x >> go currentKey rest

constructChunks_ ::
  forall a r m.
  (MemPack a, Typeable a, MemPackHeaderOffset a, MonadIO m) =>
  (Namespace, ChunkFormat) ->
  Int ->
  Stream (Of a) m r ->
  Stream (Of CB.ChunkItem) m (Digest)
constructChunks_ params bufferSize s0 = liftIO initialize >>= consume s0
 where
  initialize = CB.mkMachine bufferSize params
  consume ::
    Stream (Of a) m r ->
    CB.BuilderMachine ->
    Stream (Of CB.ChunkItem) m (Digest)
  consume s1 !machine = do
    case s1 of
      Return{} ->
        liftIO (CB.interpretCommand machine CB.Finalize) >>= \case
          (digest, Nothing) -> return digest
          (digest, Just e) -> S.yield e >> return digest
      Effect e -> Effect (e >>= \s -> return (consume s machine))
      Step (u :> rest) -> do
        liftIO (CB.interpretCommand machine (CB.Append u)) >>= \case
          (machine', chunks) -> do
            S.each chunks
            consume rest machine'

constructMetadata_ ::
  forall r m.
  (MonadIO m) =>
  Int ->
  Stream (Of MetadataEntry) m r ->
  Stream (Of MB.MetadataItem) m (Digest)
constructMetadata_ bufferSize s0 = liftIO initialize >>= consume s0
 where
  initialize = MB.mkMachine bufferSize
  consume ::
    Stream (Of MetadataEntry) m r ->
    MB.BuilderMachine ->
    Stream (Of MB.MetadataItem) m (Digest)
  consume s1 !machine = do
    case s1 of
      Return{} ->
        liftIO (MB.interpretCommand machine MB.Finalize) >>= \case
          (digest, Nothing) -> return digest
          (digest, Just e) -> S.yield e >> return digest
      Effect e -> Effect (e >>= \s -> return (consume s machine))
      Step (u :> rest) -> do
        liftIO (MB.interpretCommand machine (MB.Append u)) >>= \case
          (machine', metadata) -> do
            S.each metadata
            consume rest machine'

data ManifestInfo = ManifestInfo
  { _namespaceInfo :: Map Namespace NamespaceInfo
  }

instance Semigroup ManifestInfo where
  (ManifestInfo a) <> (ManifestInfo b) = ManifestInfo (Map.union a b)

instance Monoid ManifestInfo where
  mempty = ManifestInfo Map.empty

mkManifest :: SlotNo -> ManifestInfo -> SerializationPlan a m -> IO Manifest
mkManifest slotNo (ManifestInfo namespaceInfo) (SerializationPlan{..}) = do
  let ns = Map.toList namespaceInfo
      totalEntries = F.foldl' (+) 0 (namespaceEntries . snd <$> ns)
      totalChunks = F.foldl' (+) 0 (namespaceChunks . snd <$> ns)
      rootHash =
        Digest $
          MT.merkleRootHash $
            MT.finalize $
              F.foldl' MT.add (MT.empty undefined) (namespaceHash . snd <$> ns)
  createdAt <- T.pack <$> formatShow iso8601Format <$> fromMaybe getCurrentTime (fmap pure pTimestamp)
  pure
    Manifest
      { slotNo
      , totalEntries
      , totalChunks
      , rootHash = rootHash
      , nsInfo = namespaceInfo
      , prevManifestOffset = 0 -- TODO: support chaining of manifests
      , summary =
          ManifestSummary
            { createdAt
            , tool = T.pack "scls-tool:reference" -- TODO: add version (?)
            , comment = pManifestComment
            }
      }
