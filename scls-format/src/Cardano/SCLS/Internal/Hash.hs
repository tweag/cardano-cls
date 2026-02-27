module Cardano.SCLS.Internal.Hash (
  Digest (..),
  hashDigestSize,
  digestFromByteString,
  digestToString,
) where

import Crypto.Hash qualified as CH
import Data.ByteArray qualified as BA
import Data.MemPack

type HashAlgorithm = CH.Blake2b_224

-- | Size of the hash digest in bytes.
hashDigestSize :: Int
hashDigestSize = CH.hashDigestSize (undefined :: HashAlgorithm)

-- | Create a Digest from a byte array, if valid.
digestFromByteString :: (BA.ByteArrayAccess ba) => ba -> Maybe Digest
digestFromByteString = fmap Digest . CH.digestFromByteString

-- | Convert Digest to its string representation.
digestToString :: Digest -> String
digestToString (Digest h) = show h

newtype Digest = Digest (CH.Digest HashAlgorithm)
  deriving (Eq, Ord, Show, Read)

instance BA.ByteArrayAccess Digest where
  length (Digest h) = BA.length h
  withByteArray (Digest h) f = BA.withByteArray h f

instance MemPack Digest where
  packedByteCount _ = hashDigestSize

  packM (Digest h) = packByteStringM $ BA.convert h

  unpackM = do
    bs <- unpackByteStringM hashDigestSize
    case digestFromByteString bs of
      Just d -> pure d
      Nothing -> fail "Invalid digest"
