{-# LANGUAGE RecordWildCards #-}

{- | Plan for running serialization.

SerializationPlan explains how to construct the values that should be serialized to
the dump file.
-}
module Cardano.SCLS.Internal.Serializer.Dump.Plan (
  -- * Plan
  SerializationPlan (..),
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
  SortedSerializationPlan (..),
  mkSortedSerializationPlan,

  -- * Re-exports
  RawBytes,
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

-- | Serialization plan with data sources and configuration options.
data SerializationPlan a m = SerializationPlan
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
defaultSerializationPlan :: (Monad m) => SerializationPlan a m
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
  forall ns m.
  (KnownSymbol ns, KnownNamespace ns, Monad m) =>
  Proxy ns ->
  Stream (Of (ChunkEntry (NamespaceKey ns) (NamespaceEntry ns))) m () ->
  SerializationPlan (SomeChunkEntry RawBytes) m ->
  SerializationPlan (SomeChunkEntry RawBytes) m
addNamespacedChunks p stream =
  addChunks $
    S.yield
      ((fromSymbol p) :> S.map (SomeChunkEntry . encodeChunkEntry p) stream)

addChunks :: (MemPack a, Typeable a, Monad m) => ChunkStream a m -> SerializationPlan a m -> SerializationPlan a m
addChunks stream plan@SerializationPlan{..} =
  plan
    { pChunkStream = pChunkStream <> stream
    }

-- | Set the chunk format in the serialization plan.
withChunkFormat :: ChunkFormat -> SerializationPlan a m -> SerializationPlan a m
withChunkFormat format plan =
  plan
    { pChunkFormat = format
    }

-- | Add a metadata stream to the serialization plan.
addMetadata :: MetadataStream m -> SerializationPlan a m -> SerializationPlan a m
addMetadata stream plan =
  plan
    { pMetadataStream = Just stream
    }

-- | Set the buffer size in the serialization plan.
withBufferSize :: Int -> SerializationPlan a m -> SerializationPlan a m
withBufferSize size plan =
  plan
    { pBufferSize = size
    }

-- | Set the manifest comment value in the serialization plan.
withManifestComment :: Text -> SerializationPlan a m -> SerializationPlan a m
withManifestComment comment plan =
  plan
    { pManifestComment = Just comment
    }

-- | Set the timestamp value in the serialization plan.
withTimestamp :: UTCTime -> SerializationPlan a m -> SerializationPlan a m
withTimestamp timestamp plan =
  plan
    { pTimestamp = Just timestamp
    }

-- | A serialization plan with sorted streams.
newtype SortedSerializationPlan a m = SortedSerializationPlan {getSerializationPlan :: SerializationPlan a m}

-- | Create a sorted serialization plan from an existing plan and sorter functions.
mkSortedSerializationPlan ::
  SerializationPlan a m ->
  (ChunkStream a m -> ChunkStream b m) ->
  SortedSerializationPlan b m
mkSortedSerializationPlan plan@SerializationPlan{..} sortF =
  SortedSerializationPlan $
    plan
      { pChunkStream = sortF pChunkStream
      }
