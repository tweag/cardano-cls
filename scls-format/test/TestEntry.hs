{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TestEntry (
  TestEntry (..),
  TestEntryKey (..),
  TestUTxO,
  TestBlock,
  NamespacedTestEntry (..),
  chunkEntryFromBlock,
  chunkEntryFromUTxO,
) where

import Cardano.SCLS.CBOR.Canonical.Decoder
import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.CDDL ()
import Cardano.SCLS.Entry.IsKey (IsKey (keySize, packKeyM, unpackKeyM))
import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry (ChunkEntry))
import Cardano.SCLS.Internal.Serializer.HasKey (HasKey (Key, getKey))
import Cardano.SCLS.NamespaceCodec (CanonicalCBOREntryDecoder (decodeEntry), CanonicalCBOREntryEncoder (encodeEntry), KnownNamespace (NamespaceEntry, NamespaceKey), NamespaceKeySize, namespaceKeySize)
import Cardano.SCLS.Versioned (Versioned (Versioned))
import Data.ByteString qualified as BS
import Data.Data (Proxy (Proxy))
import Data.MemPack (MemPack (packM, packedByteCount, unpackM), packByteStringM, unpackByteStringM)
import Data.MemPack.Extra (ByteStringSized (ByteStringSized), MemPackHeaderOffset (headerSizeOffset))
import Test.QuickCheck

-- | Example data type for testing
newtype TestEntryKey = TestEntryKey BS.ByteString
  deriving (Eq, Ord, Show)

data TestEntry = TestEntry
  { key :: BS.ByteString
  , value :: Int
  }
  deriving (Eq, Show)

newtype TestUTxO = TestUTxO TestEntry
  deriving (Eq, Show)

instance Arbitrary TestUTxO where
  arbitrary =
    TestUTxO . unNamespacedTestEntry <$> genEntry (Proxy @"utxo/v0")

instance MemPack TestUTxO where
  packedByteCount (TestUTxO (TestEntry _ v)) = keySize @TestUTxOKey + packedByteCount v
  packM (TestUTxO (TestEntry k v)) = do
    packKeyM (TestUTxOKey k)
    packM v

  unpackM = do
    TestUTxOKey k <- unpackKeyM
    v <- unpackM
    pure $ TestUTxO (TestEntry k v)

instance MemPackHeaderOffset TestUTxO where
  headerSizeOffset = 0

newtype TestUTxOKey = TestUTxOKey BS.ByteString
  deriving (Eq, Ord)

instance IsKey TestUTxOKey where
  keySize = namespaceKeySize @"utxo/v0"

  packKeyM (TestUTxOKey bs) = packByteStringM bs

  unpackKeyM = do
    bs <- unpackByteStringM (keySize @TestUTxOKey)
    pure $ TestUTxOKey bs

newtype TestBlock = TestBlock TestEntry
  deriving (Eq, Show)

instance Arbitrary TestBlock where
  arbitrary =
    TestBlock . unNamespacedTestEntry <$> genEntry (Proxy @"blocks/v0")

instance MemPack TestBlock where
  packedByteCount (TestBlock (TestEntry _ v)) = keySize @TestBlockKey + packedByteCount v
  packM (TestBlock (TestEntry k v)) = do
    packKeyM (TestBlockKey k)
    packM v

  unpackM = do
    TestBlockKey k <- unpackKeyM
    v <- unpackM
    pure $ TestBlock (TestEntry k v)

instance MemPackHeaderOffset TestBlock where
  headerSizeOffset = 0

newtype TestBlockKey = TestBlockKey BS.ByteString
  deriving (Eq, Ord)

instance IsKey TestBlockKey where
  keySize = namespaceKeySize @"blocks/v0"

  packKeyM (TestBlockKey bs) = packByteStringM bs

  unpackKeyM = do
    bs <- unpackByteStringM (keySize @TestBlockKey)
    pure $ TestBlockKey bs

instance HasKey TestUTxO where
  type Key TestUTxO = TestUTxOKey

  getKey (TestUTxO (TestEntry k _)) = TestUTxOKey k

instance HasKey TestBlock where
  type Key TestBlock = TestBlockKey

  getKey (TestBlock (TestEntry k _)) = TestBlockKey k

instance CanonicalCBOREntryEncoder "utxo/v0" TestUTxO where
  encodeEntry (TestUTxO TestEntry{key, value}) = toCanonicalCBOR (Proxy @"utxo/v0") (key, value)

instance CanonicalCBOREntryDecoder "utxo/v0" TestUTxO where
  decodeEntry = do
    Versioned (key, value) <- fromCanonicalCBOR
    pure $ Versioned $ TestUTxO $ TestEntry key value

instance KnownNamespace "utxo/v0" where
  type NamespaceKey "utxo/v0" = TestUTxOKey
  type NamespaceEntry "utxo/v0" = TestUTxO

instance KnownNamespace "blocks/v0" where
  type NamespaceKey "blocks/v0" = TestBlockKey
  type NamespaceEntry "blocks/v0" = Int

instance CanonicalCBOREntryDecoder "blocks/v0" Int where
  decodeEntry = fmap (\x -> x - 1) <$> fromCanonicalCBOR

instance CanonicalCBOREntryEncoder "blocks/v0" Int where
  encodeEntry = toCanonicalCBOR Proxy . (+ 1)

genKey :: (KnownNamespace ns) => Proxy ns -> Gen (ByteStringSized (NamespaceKeySize ns))
genKey (_ :: proxy ns) =
  ByteStringSized . BS.pack <$> vectorOf (namespaceKeySize @ns) arbitrary

newtype NamespacedTestEntry ns = NamespacedTestEntry {unNamespacedTestEntry :: TestEntry}

genEntry :: forall ns. (KnownNamespace ns) => Proxy ns -> Gen (NamespacedTestEntry ns)
genEntry p = do
  (ByteStringSized key) <- genKey p
  value <- arbitrary
  pure $ NamespacedTestEntry $ TestEntry key value

chunkEntryFromUTxO :: TestUTxO -> ChunkEntry TestUTxOKey TestUTxO
chunkEntryFromUTxO (e@(TestUTxO (TestEntry k _))) =
  ChunkEntry (TestUTxOKey k) e

chunkEntryFromBlock :: TestBlock -> ChunkEntry TestBlockKey Int
chunkEntryFromBlock (TestBlock (TestEntry k v)) =
  ChunkEntry (TestBlockKey k) v
