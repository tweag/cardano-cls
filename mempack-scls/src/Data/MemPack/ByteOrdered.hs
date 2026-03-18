-- | Helpers for dealing with endianness in binary formats.
module Data.MemPack.ByteOrdered (
  Storable (..),

  -- * Big endian
  BigEndian (..),
  packWord16beM,
  packWord32beM,
  packWord64beM,
  unpackBigEndianM,

  -- * Little endian
  LittleEndian (..),
  packWord16leM,
  packWord32leM,
  packWord64leM,
  unpackLittleEndianM,
) where

import Data.MemPack
import Data.MemPack.Buffer (Buffer)
import Data.Word
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))
import System.ByteOrder (Bytes (..), fromBigEndian, fromLittleEndian, toBigEndian, toLittleEndian)

{- | A wrapper type to indicate that a value is stored in big-endian format.

Intended to be used with 'DerivingVia' to derive instances.

Example:
@
newtype SlotNo = SlotNo { unSlotNo :: Word64 }
  deriving (Eq, Ord, Show, Read)
  deriving (Storable) via (BigEndian Word64)
@
-}
newtype BigEndian a = BigEndian {unBigEndian :: a}
  deriving (Eq, Ord, Show, Read)

{- | A wrapper type to indicate that a value is stored in little-endian format.

Intended to be used with 'DerivingVia' to derive instances.

Example:
@
newtype Foo = Foo { unFoo :: Word64 }
  deriving (Eq, Ord, Show, Read)
  deriving (Storable) via (LittleEndian Word64)
@
-}
newtype LittleEndian a = LittleEndian {unLittleEndian :: a}
  deriving (Eq, Ord, Show, Read)

instance (Bytes a, Storable a) => Storable (BigEndian a) where
  sizeOf _ = sizeOf (undefined :: a)
  alignment _ = alignment (undefined :: a)
  peek ptr = do
    w <- peek (castPtr ptr :: Ptr a)
    return (BigEndian (fromBigEndian w))
  poke ptr (BigEndian w) = poke (castPtr ptr :: Ptr a) (toBigEndian w)

instance (Bytes a, Storable a) => Storable (LittleEndian a) where
  sizeOf _ = sizeOf (undefined :: a)
  alignment _ = alignment (undefined :: a)
  peek ptr = do
    w <- peek (castPtr ptr :: Ptr a)
    return (LittleEndian (fromLittleEndian w))
  poke ptr (LittleEndian w) = poke (castPtr ptr :: Ptr a) (toLittleEndian w)

instance (Bytes a, MemPack a) => MemPack (BigEndian a) where
  typeName = "BigEndian " ++ typeName @a
  packedByteCount (BigEndian a) = packedByteCount a
  packM (BigEndian a) = packM (toBigEndian a)
  unpackM = BigEndian . fromBigEndian <$> unpackM

instance (Bytes a, MemPack a) => MemPack (LittleEndian a) where
  typeName = "LittleEndian " ++ typeName @a
  packedByteCount (LittleEndian a) = packedByteCount a
  packM (LittleEndian a) = packM (toLittleEndian a)
  unpackM = LittleEndian . fromLittleEndian <$> unpackM

packBigEndianM :: (Bytes a, MemPack a) => a -> Pack s ()
packBigEndianM = packM . BigEndian

packLittleEndianM :: (Bytes a, MemPack a) => a -> Pack s ()
packLittleEndianM = packM . LittleEndian

packWord16beM :: Word16 -> Pack s ()
packWord16beM = packBigEndianM

packWord32beM :: Word32 -> Pack s ()
packWord32beM = packBigEndianM

packWord64beM :: Word64 -> Pack s ()
packWord64beM = packBigEndianM

packWord16leM :: Word16 -> Pack s ()
packWord16leM = packLittleEndianM

packWord32leM :: Word32 -> Pack s ()
packWord32leM = packLittleEndianM

packWord64leM :: Word64 -> Pack s ()
packWord64leM = packLittleEndianM

unpackBigEndianM :: (Bytes a, MemPack a, Buffer b) => Unpack s b a
unpackBigEndianM =
  unBigEndian <$> unpackM

unpackLittleEndianM :: (Bytes a, MemPack a, Buffer b) => Unpack s b a
unpackLittleEndianM =
  unLittleEndian <$> unpackM
