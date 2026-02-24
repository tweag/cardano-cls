{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.SCLS.Internal.Entry.ChunkEntry (
  ChunkEntry (..),
  SomeChunkEntry (..),
  encodeChunkEntry,
  decodeChunkEntry,
) where

import Cardano.SCLS.CBOR.Canonical (CanonicalDecoder (getRawDecoder), CanonicalEncoding (getRawEncoding))
import Cardano.SCLS.Entry.IsKey (IsKey (..))
import Cardano.SCLS.Internal.Serializer.HasKey
import Cardano.SCLS.NamespaceCodec (CanonicalCBOREntryDecoder (decodeEntry), CanonicalCBOREntryEncoder (encodeEntry), KnownNamespace (..), NamespaceKeySize, decodeKeyFromBytes, encodeKeyToBytes)
import Cardano.SCLS.Versioned (Versioned (Versioned))
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toStrictByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.MemPack
import Data.MemPack.Extra (ByteStringSized (..), MemPackHeaderOffset (..), RawBytes (RawBytes))
import Data.Typeable (Proxy (Proxy), Typeable)
import GHC.TypeLits (KnownNat)

data ChunkEntry k v = ChunkEntry
  { chunkEntryKey :: k
  , chunkEntryValue :: v
  }
  deriving (Show)

instance (Eq k, Eq v) => Eq (ChunkEntry k v) where
  (ChunkEntry k1 v1) == (ChunkEntry k2 v2) = k1 == k2 && v1 == v2

instance (Ord k, Ord v) => Ord (ChunkEntry k v) where
  compare (ChunkEntry k1 v1) (ChunkEntry k2 v2) = compare k1 k2 <> compare v1 v2

instance (Ord k) => HasKey (ChunkEntry k v) where
  type Key (ChunkEntry k v) = k
  getKey (ChunkEntry k _) = k

instance (Typeable k, IsKey k, MemPack v, Typeable v) => MemPack (ChunkEntry k v) where
  packedByteCount (ChunkEntry _ v) = keySize @k + packedByteCount v
  packM (ChunkEntry k v) = do
    packKeyM k
    packM v
  unpackM = do
    k <- unpackKeyM
    v <- unpackM
    return (ChunkEntry k v)

instance (Typeable k, IsKey k, Typeable v, MemPack v) => MemPackHeaderOffset (ChunkEntry k v) where
  headerSizeOffset = 0

data SomeChunkEntry a = forall n. (KnownNat n) => SomeChunkEntry (ChunkEntry (ByteStringSized n) a)

instance (Show a) => Show (SomeChunkEntry a) where
  show (SomeChunkEntry e) = "SomeChunkEntry " ++ show e

instance (Eq a) => Eq (SomeChunkEntry a) where
  (==) (SomeChunkEntry (ChunkEntry (ByteStringSized k1) v1)) (SomeChunkEntry (ChunkEntry (ByteStringSized k2) v2)) =
    k1 == k2 && v1 == v2

instance (Ord a) => Ord (SomeChunkEntry a) where
  compare
    (SomeChunkEntry (ChunkEntry (ByteStringSized k1) v1))
    (SomeChunkEntry (ChunkEntry (ByteStringSized k2) v2)) =
      k1 `compare` k2 <> v1 `compare` v2

instance (MemPack a, Typeable a) => MemPack (SomeChunkEntry a) where
  packedByteCount (SomeChunkEntry e) = packedByteCount e

  packM :: (MemPack a, Typeable a) => SomeChunkEntry a -> Pack s ()
  packM (SomeChunkEntry e) = packM e

  unpackM = error "unpackM SomeChunkEntry: cannot determine size at runtime"

instance (MemPack a, Typeable a) => MemPackHeaderOffset (SomeChunkEntry a) where
  headerSizeOffset = 4

instance HasKey (SomeChunkEntry a) where
  type Key (SomeChunkEntry a) = BS.ByteString

  getKey (SomeChunkEntry (ChunkEntry (ByteStringSized bs) _)) = bs

-- | Encode a chunk entry from namespace-specific types to a bytes-only chunk entry.
encodeChunkEntry :: forall ns. (KnownNamespace ns) => Proxy ns -> ChunkEntry (NamespaceKey ns) (NamespaceEntry ns) -> ChunkEntry (ByteStringSized (NamespaceKeySize ns)) RawBytes
encodeChunkEntry _ (ChunkEntry k v) =
  let key = encodeKeyToBytes @ns k
      value = RawBytes $ toStrictByteString $ getRawEncoding $ encodeEntry @ns v
   in ChunkEntry key value

-- | Decode a chunk entry from raw bytes to namespace-specific types.
decodeChunkEntry :: forall ns. (KnownNamespace ns) => Proxy ns -> ChunkEntry (ByteStringSized (NamespaceKeySize ns)) RawBytes -> Maybe (ChunkEntry (NamespaceKey ns) (NamespaceEntry ns))
decodeChunkEntry _ (ChunkEntry (ByteStringSized k) (RawBytes v)) = do
  let keyMaybe = decodeKeyFromBytes (Proxy @ns) k
  let valueEither = deserialiseFromBytes (getRawDecoder $ decodeEntry @ns) $ BSL.fromStrict v
  case (keyMaybe, valueEither) of
    (Nothing, _) -> Nothing
    (_, Left _) -> Nothing
    (Just key, Right (_, Versioned value)) ->
      Just $ ChunkEntry key value
