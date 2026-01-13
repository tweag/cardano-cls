{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TestEntry (
  TestEntry (..),
  TestEntryKey (..),
  NamespacedTestEntry (..),
  genKey,
  genEntry,
  genUTxO,
  genBlock,
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
import Data.MemPack (packByteStringM, unpackByteStringM)
import Data.MemPack.Extra (ByteStringSized (ByteStringSized))
import System.Random.Stateful (Uniform (uniformM), globalStdGen, uniformByteStringM)
import Test.QuickCheck

-- | Example data type for testing
newtype TestEntryKey = TestEntryKey BS.ByteString
  deriving (Eq, Ord, Show)

instance IsKey TestEntryKey where
  keySize = 34

  packKeyM (TestEntryKey bs) = packByteStringM bs

  unpackKeyM =
    TestEntryKey <$> unpackByteStringM (keySize @TestEntryKey)

data TestEntry = TestEntry
  { key :: BS.ByteString
  , value :: Int
  }
  deriving (Eq, Show)

instance Arbitrary TestEntry where
  arbitrary = TestEntry <$> (BS.pack <$> arbitrary) <*> arbitrary

newtype TestUTxO = TestUTxO TestEntry
  deriving (Eq, Show)
  deriving newtype (Arbitrary)

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
  deriving newtype (Arbitrary)

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

genKey :: forall ns. (KnownNamespace ns) => Proxy ns -> IO (ByteStringSized (NamespaceKeySize ns))
genKey _ =
  ByteStringSized <$> uniformByteStringM (namespaceKeySize @ns) globalStdGen

newtype NamespacedTestEntry ns = NamespacedTestEntry {unNamespacedTestEntry :: TestEntry}

genEntry :: forall ns. (KnownNamespace ns) => Proxy ns -> IO (NamespacedTestEntry ns)
genEntry p = do
  (ByteStringSized key) <- genKey p
  value <- uniformM globalStdGen
  pure $ NamespacedTestEntry $ TestEntry key value

genUTxO :: IO TestUTxO
genUTxO =
  TestUTxO . unNamespacedTestEntry <$> genEntry (Proxy @"utxo/v0")

genBlock :: IO TestBlock
genBlock =
  TestBlock . unNamespacedTestEntry <$> genEntry (Proxy @"blocks/v0")

chunkEntryFromUTxO :: TestUTxO -> ChunkEntry TestUTxOKey TestUTxO
chunkEntryFromUTxO (e@(TestUTxO (TestEntry k _))) =
  ChunkEntry (TestUTxOKey k) e

chunkEntryFromBlock :: TestBlock -> ChunkEntry TestBlockKey Int
chunkEntryFromBlock (TestBlock (TestEntry k v)) =
  ChunkEntry (TestBlockKey k) v
