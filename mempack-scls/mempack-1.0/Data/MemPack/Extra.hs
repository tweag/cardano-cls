{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Useful utilities for working with MemPack types.
module Data.MemPack.Extra (
  CStringLenBuffer (..),
  isolate,
  isolated,
  MemPackHeaderOffset (..),

  -- * Type helpers
  Entry (..),
  ByteStringSized (..),
  SomeByteStringSized (..),
  CBORTerm (getRawTerm, getEncodedBytes),
  mkCBORTerm,
  RawBytes (..),
  Unpack',
  hPutBuffer,
  runDecode,
) where

import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Trans.Fail
import Data.ByteArray (ByteArrayAccess, length, withByteArray)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.MemPack
import Data.MemPack.Buffer
import Data.MemPack.Error
import Data.Primitive.ByteArray
import Data.Primitive.Ptr
import Data.String (IsString)
import Data.Text qualified as T
import Data.Typeable
import Data.Word
import Foreign.C.String
import Foreign.Ptr
import GHC.ForeignPtr
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownNat, Nat, natVal)
import GHC.TypeNats (fromSNat, pattern SNat)
import System.ByteOrder
import System.IO

type Unpack' s b a = Unpack b a

-- | Typeclass for types that have a fixed header offset when serialized.
class (MemPack a) => MemPackHeaderOffset a where
  headerSizeOffset :: Int

{- | Wrapper that allows to store raw bytes without any prefix.

It's likely that this type will be removed in the future as
it does not provide a way to decode the data back.
-}
newtype RawBytes = RawBytes ByteString
  deriving (Eq, Ord, Show)

-- instance HasKey RawBytes where
--   type Key RawBytes = ByteString
--   getKey (RawBytes bs) = bs

-- Instance that reads all remaining bytes as a ByteString, relies
-- on running in 'isolated' context.
instance MemPack (RawBytes) where
  packedByteCount (RawBytes bs) = BS.length bs
  packM (RawBytes bs) = packByteStringM bs
  unpackM = RawBytes <$> consumeBytes

instance MemPackHeaderOffset RawBytes where
  headerSizeOffset = 4

{- | Entry wrapper for other mempack values, that explicitly
stores its length as a big-endian 'Word32' before the value
itself
-}
newtype Entry u = Entry u
  deriving (Eq, Ord)

instance (Typeable u, MemPack u) => MemPack (Entry u) where
  packedByteCount (Entry u) = 4 + packedByteCount u
  packM (Entry u) = do
    let l = packedByteCount u
    packM (toBigEndian (fromIntegral l :: Word32)) -- length prefix
    packM u
  unpackM = do
    l :: Word32 <- fromBigEndian <$> unpackM
    u <- isolated (fromIntegral l)
    return (Entry u)

instance (Typeable u, MemPack u) => MemPackHeaderOffset (Entry u) where
  headerSizeOffset = 4

{- | Isolate a decoder to operate with a fixed number of bytes, and fail if fewer bytes
were consumed, or more bytes were attempted to be consumed. If the given decoder fails,
isolate will also fail.
-}
isolated :: forall a b. (Buffer b, MemPack a) => (HasCallStack) => Int -> Unpack b a
isolated len = do
  start <- get
  b' :: Isolate b <- asks (isolate start len)
  a <- Unpack $ \_ -> StateT (\_ -> unpackLeftOverOff b' start unpackM)
  return a

{- | Consumes all remaining bytes in the current buffer.

Useful for reading isolated data.
-}
consumeBytes :: forall b. (Buffer b) => Unpack b ByteString
consumeBytes = do
  start <- get
  len <- asks bufferByteCount
  unpackByteStringM (len - start)

unpackLeftOverOff :: forall a b. (MemPack a, Buffer b) => (HasCallStack) => b -> Int -> Unpack b a -> Fail SomeError (a, Int)
unpackLeftOverOff buf off action = do
  let len = bufferByteCount buf
  res@(_, consumedBytes) <- runStateT (runUnpack action buf) off
  case len `compare` consumedBytes of
    EQ -> return res
    GT ->
      failT $
        toSomeError
          NotFullyConsumedError
            { notFullyConsumedRead = consumedBytes - off
            , notFullyConsumedAvailable = len - consumedBytes
            , notFullyConsumedTypeName = typeName @a
            }
    LT ->
      failT $
        toSomeError
          RanOutOfBytesError
            { ranOutOfBytesRead = consumedBytes - off
            , ranOutOfBytesRequested = len - consumedBytes - off
            , ranOutOfBytesAvailable = 0
            }

{- | Isolate a portion of a buffer to a given offset and length.

As the buffers are not sliceable it is done by keeping information
about the size of the new buffer.
-}
isolate :: (Buffer u) => Int -> Int -> u -> Isolate u
isolate off len buf = Isolate buf (min (bufferByteCount buf) (off + len))

data Isolate b = Isolate
  { isolatedBuffer :: b
  , isolatedLength :: Int
  }
  deriving (Show)

instance (Buffer b) => Buffer (Isolate b) where
  bufferByteCount Isolate{isolatedLength = len} = len
  buffer Isolate{isolatedBuffer = b} f g = buffer b f g

-- | Additional wrapper to use MemPack Buffer interface
newtype CStringLenBuffer = CStringLenBuffer CStringLen

instance Buffer CStringLenBuffer where
  bufferByteCount (CStringLenBuffer (_, l)) = l
  buffer (CStringLenBuffer (ptr, off)) _ withAddr =
    withAddr (case ptr `plusPtr` off of Ptr addr -> addr)

instance ByteArrayAccess CStringLenBuffer where
  length (CStringLenBuffer (_, l)) = l
  withByteArray (CStringLenBuffer (ptr, _)) f =
    f (ptr `plusPtr` 0)

{- | An existential wrapper for the case when we need to compare
fixed-size bytestrings to each other.

When we compare then we do it in length-first number, first we compare
the sizes, and then the bytestring content. This approach matches
the ordering required by the canonical CBOR.
-}
data SomeByteStringSized where
  SomeByteStringSized :: (KnownNat n) => ByteStringSized n -> SomeByteStringSized

instance Eq SomeByteStringSized where
  (SomeByteStringSized (ByteStringSized bs1)) == (SomeByteStringSized (ByteStringSized bs2)) = bs1 == bs2

instance Ord SomeByteStringSized where
  compare (SomeByteStringSized (ByteStringSized bs1 :: ByteStringSized n)) (SomeByteStringSized (ByteStringSized bs2 :: ByteStringSized m)) =
    compare
      (fromSNat @n SNat)
      (fromSNat @m SNat)
      <> compare bs1 bs2

-- | A bytestring with the size known at compile time.
newtype ByteStringSized (n :: Nat) = ByteStringSized ByteString
  deriving (Eq, Ord, Show)

instance (KnownNat n) => MemPack (ByteStringSized n) where
  packedByteCount _ = fromInteger (natVal (Proxy :: Proxy n))

  packM (ByteStringSized bs) = do
    let expected = fromIntegral (natVal (Proxy :: Proxy n)) :: Int
    let len = BS.length bs
    if len /= expected
      then error $! "ByteStringSized: expected " ++ show expected ++ " bytes, got " ++ show len
      else packByteStringM bs

  unpackM = do
    let expected = fromIntegral (natVal (Proxy :: Proxy n)) :: Int
    bs <- unpackByteStringM expected
    pure (ByteStringSized bs)

-- | Helper to store CBOR terms directly as entries.
data CBORTerm = CBORTerm {getRawTerm :: !CBOR.Term, getEncodedBytes :: ByteString}
  deriving (Eq, Ord, Show)

mkCBORTerm :: CBOR.Term -> CBORTerm
mkCBORTerm t = CBORTerm t (CBOR.toStrictByteString (CBOR.encodeTerm t))

instance MemPack CBORTerm where
  packedByteCount (CBORTerm _ bs) =
    BS.length bs

  packM (CBORTerm _ bs) =
    packByteStringM bs

  unpackM = do
    start <- gets fromIntegral
    bytes <- consumeBytes
    case CBOR.deserialiseFromBytesWithSize CBOR.decodeTerm (BSL.fromStrict bytes) of
      Left err -> failUnpack $ TextError $ "CBOR term deserialisation failed: " <> T.pack (show err)
      Right (_rest, bytesRead, term) -> do
        put (start + fromIntegral bytesRead)
        pure (CBORTerm term bytes)

hPutBuffer :: (Buffer u) => Handle -> u -> IO ()
hPutBuffer handle u =
  buffer
    u
    ( \bytes -> do
        pinnedBytes <- pinIfNeeded (ByteArray bytes)
        withForeignPtr (byteArrayAsForeignPtr pinnedBytes) $ \ptr -> do
          hPutBuf handle ptr len
    )
    (\addr -> hPutBuf handle (Ptr addr) len)
 where
  len = bufferByteCount u
  pinIfNeeded bytes
    | isByteArrayPinned bytes = return bytes
    | otherwise = do
        dest <- newPinnedByteArray len
        copyByteArray dest 0 bytes 0 len
        unsafeFreezeByteArray dest

runDecode :: (IsString e) => Fail e a -> Either e a
runDecode f = runFailLast f
