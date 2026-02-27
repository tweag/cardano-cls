{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Implementation of the state machine that fills current chunk in memory.

It manages proper filling of the buffers and emitting the values when
the next item can't be added.

Current implementation expects the incoming values in already sorted order.

Implementation is done in the way so it would be possible to use it with
any existing stream and effect system as long as they could carry a state.
-}
module Cardano.SCLS.Internal.Serializer.ChunksBuilder.InMemory (
  mkMachine,
  B.Command (..),
  BuilderMachine,
  ChunkItem (..),
  B.interpretCommand,
) where

import Cardano.SCLS.Internal.Hash (Digest)
import Cardano.SCLS.Internal.Record.Chunk
import Cardano.SCLS.Internal.Serializer.Builder.InMemory (BuilderItem (Parameters))
import Cardano.SCLS.Internal.Serializer.Builder.InMemory qualified as B
import Cardano.Types.Namespace (Namespace)

import Data.Primitive.ByteArray

data ChunkItem = ChunkItem
  { chunkItemFormat :: ChunkFormat
  , chunkItemData :: ByteArray
  , chunkItemEntriesCount :: Int
  , chunkItemHash :: Digest
  }

instance B.BuilderItem ChunkItem where
  type Parameters ChunkItem = (Namespace, ChunkFormat)
  bItemData = chunkItemData
  bItemEntriesCount = chunkItemEntriesCount
  bMkItem (_, chunkItemFormat) = ChunkItem chunkItemFormat
  bEncodeEntry (_, ChunkFormatRaw) entry = entry
  bEncodeEntry (_, ChunkFormatZstd) _entry = error "Chunk format zstd is not implemented yet"
  bEncodeEntry (_, ChunkFormatZstdE) _entry = error "Chunk format zstd-e is not implemented yet"
  bEntryDigest (namespace, _) = entryDigest namespace

type BuilderMachine = B.BuilderMachine ChunkItem

mkMachine :: Int -> B.Parameters ChunkItem -> IO BuilderMachine
mkMachine = B.mkMachine
