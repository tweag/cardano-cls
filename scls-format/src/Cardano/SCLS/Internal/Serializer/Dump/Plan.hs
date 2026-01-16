{-# LANGUAGE RecordWildCards #-}

{- | Plan for running serialization.

SerializationPlan explains how to construct the values that should be serialized to
the dump file.
-}
module Cardano.SCLS.Internal.Serializer.Dump.Plan (
  -- * Plan
  SerializationPlan (pBufferSize, pChunkFormat, pChunkStream, pMetadataStream, pManifestComment, pTimestamp),
  Sorted,
  Unsorted,
  InputChunk,
  ChunkStream,

  -- ** Construction
  defaultSerializationPlan,
  addNamespacedChunks,
  addChunks,
  withChunkFormat,
  addMetadata,
  withBufferSize,
  withManifestComment,
  withTimestamp,

  -- * Sorted plan
  sortSerializationPlan,
) where

import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry, SomeChunkEntry (SomeChunkEntry), encodeChunkEntry)
import Cardano.SCLS.Internal.Record.Chunk
import Cardano.SCLS.Internal.Record.Metadata
import Cardano.SCLS.NamespaceCodec (KnownNamespace (..))
import Cardano.Types.Namespace (Namespace, fromSymbol)

import Data.MemPack (MemPack)
import Data.MemPack.Extra (RawBytes)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable (Proxy, Typeable)
import GHC.TypeLits (KnownSymbol)
import Streaming (Of (..))
import Streaming qualified as S
import Streaming.Internal (Stream (..))
import Streaming.Prelude qualified as S

{- | Helper to define an input data.
Each chunk is a stream of values that will be written under a given namespace.
-}
type InputChunk a m = S.Of Namespace (S.Stream (S.Of a) m ())

type ChunkStream a m = Stream (Of (InputChunk a m)) m ()

type MetadataStream m = Stream (Of MetadataEntry) m ()

data Sorted
data Unsorted

-- | Serialization plan with data sources and configuration options.
data SerializationPlan a m sorted = SerializationPlan
  -- Future fields for more dump configurations can be added here
  -- e.g. isToBuildIndex, deltaStream, etc.
  { pChunkFormat :: ChunkFormat
  -- ^ Compression format for chunks
  , pBufferSize :: Int
  -- ^ Buffer size for record building (in bytes)
  , pChunkStream :: ChunkStream a m
  -- ^ Input stream of entries to serialize, can be unsorted
  , pMetadataStream :: Maybe (MetadataStream m)
  -- ^ Optional stream of metadata records to include in the dump
  , pManifestComment :: Maybe Text
  -- ^ Optional comment to inlude in the file manifest
  , pTimestamp :: Maybe (UTCTime)
  -- ^ Optional timestamp value to include in the manifest. Defaults to the timestamp at the time of serialization.
  }

-- | Create a serialization plan with default options and no data.
defaultSerializationPlan :: (Monad m) => SerializationPlan a m Unsorted
defaultSerializationPlan =
  SerializationPlan
    { pChunkFormat = ChunkFormatRaw
    , pBufferSize = 16 * 1024 * 1024 -- 16 MB buffer size
    , pChunkStream = mempty
    , pMetadataStream = Nothing
    , pManifestComment = Nothing
    , pTimestamp = Nothing
    }

-- | Add a chunked data stream to the dump configuration.
addNamespacedChunks ::
  forall ns m sorted.
  (KnownSymbol ns, KnownNamespace ns, Monad m) =>
  Proxy ns ->
  Stream (Of (ChunkEntry (NamespaceKey ns) (NamespaceEntry ns))) m () ->
  SerializationPlan (SomeChunkEntry RawBytes) m sorted ->
  SerializationPlan (SomeChunkEntry RawBytes) m Unsorted
addNamespacedChunks p stream =
  addChunks $
    S.yield
      ((fromSymbol p) :> S.map (SomeChunkEntry . encodeChunkEntry p) stream)

addChunks ::
  (MemPack a, Typeable a, Monad m) =>
  ChunkStream a m ->
  SerializationPlan a m sorted ->
  SerializationPlan a m Unsorted
addChunks stream plan@SerializationPlan{..} =
  plan
    { pChunkStream = pChunkStream <> stream
    }

-- | Set the chunk format in the serialization plan.
withChunkFormat ::
  ChunkFormat ->
  SerializationPlan a m sorted ->
  SerializationPlan a m sorted
withChunkFormat format plan =
  plan
    { pChunkFormat = format
    }

-- | Add a metadata stream to the serialization plan.
addMetadata :: MetadataStream m -> SerializationPlan a m sorted -> SerializationPlan a m sorted
addMetadata stream plan =
  plan
    { pMetadataStream = Just stream
    }

-- | Set the buffer size in the serialization plan.
withBufferSize :: Int -> SerializationPlan a m sorted -> SerializationPlan a m sorted
withBufferSize size plan =
  plan
    { pBufferSize = size
    }

-- | Set the manifest comment value in the serialization plan.
withManifestComment :: Text -> SerializationPlan a m sorted -> SerializationPlan a m sorted
withManifestComment comment plan =
  plan
    { pManifestComment = Just comment
    }

-- | Set the timestamp value in the serialization plan.
withTimestamp :: UTCTime -> SerializationPlan a m sorted -> SerializationPlan a m sorted
withTimestamp timestamp plan =
  plan
    { pTimestamp = Just timestamp
    }

-- | Convert an unsorted serialization plan into a sorted one.
sortSerializationPlan ::
  SerializationPlan a m Unsorted ->
  (ChunkStream a m -> ChunkStream b m) ->
  SerializationPlan b m Sorted
sortSerializationPlan plan@SerializationPlan{..} sortF =
  plan
    { pChunkStream = sortF pChunkStream
    }
