{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{- |

A chunk of the actual data that is stored in the file.
In order to efficiently store and retrieve data chunk is split into
header and footer parts, so we can access them without loading the entire contents of the chunk.
-}
module Cardano.SCLS.Internal.Record.Chunk (
  Chunk (..),
  ChunkFormat (..),
  DebugChunk (..),
  entryDigest,
) where

import Crypto.Hash (hashFinalize, hashUpdate)
import Crypto.Hash.MerkleTree.Incremental.Internal (leafHashInit)
import Data.ByteArray (ByteArrayAccess)
import Data.ByteString qualified as BS
import Data.MemPack (MemPack (..), packByteStringM, packTagM, unpackByteStringM, unpackTagM)
import Data.MemPack.ByteOrdered (packWord32beM, packWord64beM, unpackBigEndianM)
import Data.Word (Word32, Word64)

import Cardano.SCLS.Internal.Hash
import Cardano.SCLS.Internal.Record.Internal.Class
import Cardano.Types.Namespace (Namespace (..))
import Cardano.Types.Namespace qualified as Namespace

data ChunkFormat
  = -- | Entries are stored uncompressed
    ChunkFormatRaw
  | -- | Chunk is compressed with seekable zstd
    ChunkFormatZstd
  | -- | Entries are individually compressed with seekable zstd
    ChunkFormatZstdE
  deriving (Show, Eq)

instance MemPack ChunkFormat where
  packedByteCount _ = 1

  packM ChunkFormatRaw = packTagM 0
  packM ChunkFormatZstd = packTagM 1
  packM ChunkFormatZstdE = packTagM 2
  unpackM = do
    tag <- unpackTagM
    case tag of
      0 -> pure ChunkFormatRaw
      1 -> pure ChunkFormatZstd
      2 -> pure ChunkFormatZstdE
      _ -> fail "Unknown ChunkFormat"

-- | Data chunk, undecoded version. Loads entire data in memory.
data Chunk = Chunk
  { chunkSeq :: Word64
  , chunkFormat :: ChunkFormat
  , chunkNamespace :: Namespace
  , chunkKeySize :: Word32
  , chunkData :: BS.ByteString -- Use buffer instead (?) or even values generator
  , chunkEntriesCount :: Word32
  , chunkHash :: Digest
  }

newtype DebugChunk = DebugChunk Chunk

instance Show DebugChunk where
  show (DebugChunk Chunk{..}) =
    "Chunk{seq="
      ++ show chunkSeq
      ++ ", format="
      ++ show chunkFormat
      ++ ", namespace="
      ++ Namespace.asString chunkNamespace
      ++ ", keySize="
      ++ show chunkKeySize
      ++ ", data.len="
      ++ show (BS.length chunkData)
      ++ ", entries="
      ++ show chunkEntriesCount
      ++ ", hash="
      ++ show chunkHash
      ++ "}"

instance IsFrameRecord 0x10 Chunk where
  frameRecordSize Chunk{..} =
    packedByteCount chunkSeq
      + packedByteCount chunkFormat
      + 4
      + (BS.length $ Namespace.asBytes chunkNamespace)
      + 4
      + BS.length chunkData
      + packedByteCount chunkEntriesCount
      + hashDigestSize

  encodeRecordContents Chunk{..} = do
    packWord64beM chunkSeq
    packM chunkFormat
    packWord32beM (fromIntegral (BS.length namespace_bytes) :: Word32)
    packByteStringM namespace_bytes
    packWord32beM chunkKeySize
    packByteStringM chunkData
    packWord32beM chunkEntriesCount
    packM chunkHash
   where
    namespace_bytes = Namespace.asBytes chunkNamespace

  decodeRecordContents size = do
    chunkSeq <- unpackBigEndianM
    chunkFormat <- unpackM
    namespace_size :: Word32 <- unpackBigEndianM
    chunkNamespace <-
      (Namespace.parseBytes <$> unpackByteStringM (fromIntegral namespace_size)) >>= \case
        Left e -> fail (show e)
        Right x -> pure x
    let chunkDataSize = fromIntegral size - 1 - 8 - 1 - 4 - fromIntegral namespace_size - 4 - 4 - hashDigestSize
    chunkKeySize <- unpackBigEndianM
    chunkData <- unpackByteStringM chunkDataSize
    chunkEntriesCount <- unpackBigEndianM
    chunkHash <- unpackM
    pure Chunk{..}

{- | Compute the digest for a chunk entry given its namespace and raw data.
The digest is computed as H(0x01 || ns_str || key || value), where:
- 0x01 is a prefix byte to distinguish entry hashes from other types of hashes
- ns_str is the namespace string (as bytes)
- key and value are the raw bytes of the entry's key and value, respectively
-}
entryDigest :: (ByteArrayAccess ba) => Namespace -> ba -> Digest
entryDigest ns b = Digest $ hashFinalize $ leafHashInit `hashUpdate` (Namespace.asBytes ns) `hashUpdate` b
