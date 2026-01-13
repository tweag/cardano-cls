{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.SCLS.CBOR.Canonical.Decoder (
  FromCanonicalCBOR (..),
  decodeListLenCanonicalOf,
  decodeListLenCanonical,
  decodeMapLenCanonical,
  decodeMapLenCanonicalOf,
  decodeTagCanonical,
  decodeTextCanonicalOf,
  decodeWordCanonicalOf,
  decodeWord8Canonical,
  peekTokenType,
  decodeSequenceLenNCanonical,
) where

import Cardano.SCLS.CBOR.Canonical (CanonicalDecoder (getRawDecoder), assumeCanonicalDecoder, getRawEncoding)
import Cardano.SCLS.CBOR.Canonical.Encoder (toCanonicalCBOR)
import Cardano.SCLS.Versioned
import Codec.CBOR.ByteArray qualified as BA
import Codec.CBOR.Decoding (TokenType)
import Codec.CBOR.Decoding qualified as D
import Codec.CBOR.Read qualified as CBOR.Read
import Codec.CBOR.Term
import Codec.CBOR.Write qualified as CBOR.Write
import Control.Exception (Exception)
import Control.Monad (unless)
import Data.Array.Byte qualified as Prim
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as SBS
import Data.Coerce
import Data.Int
import Data.Map qualified as Map
import Data.Proxy
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word
import GHC.TypeLits

class FromCanonicalCBOR (v :: Symbol) a where
  fromCanonicalCBOR :: CanonicalDecoder s (Versioned v a)

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

data DecodingError
  = DecodingErrorDeserialiseFailure T.Text CBOR.Read.DeserialiseFailure
  deriving (Eq, Show)

instance Exception DecodingError

--------------------------------------------------------------------------------
-- Primitive types
--------------------------------------------------------------------------------

instance FromCanonicalCBOR v () where
  fromCanonicalCBOR = assumeCanonicalDecoder $ Versioned @v <$> D.decodeNull

instance FromCanonicalCBOR v Bool where
  fromCanonicalCBOR = assumeCanonicalDecoder $ Versioned @v <$> D.decodeBool

--------------------------------------------------------------------------------
-- Numeric data
--------------------------------------------------------------------------------

instance FromCanonicalCBOR v Integer where
  fromCanonicalCBOR = assumeCanonicalDecoder $ Versioned @v <$> D.decodeIntegerCanonical

instance FromCanonicalCBOR v Word where
  fromCanonicalCBOR = assumeCanonicalDecoder $ Versioned @v <$> D.decodeWordCanonical

instance FromCanonicalCBOR v Word8 where
  fromCanonicalCBOR = assumeCanonicalDecoder $ Versioned @v <$> D.decodeWord8Canonical

instance FromCanonicalCBOR v Word16 where
  fromCanonicalCBOR = assumeCanonicalDecoder $ Versioned @v <$> D.decodeWord16Canonical

instance FromCanonicalCBOR v Word32 where
  fromCanonicalCBOR = assumeCanonicalDecoder $ Versioned @v <$> D.decodeWord32Canonical

instance FromCanonicalCBOR v Word64 where
  fromCanonicalCBOR = assumeCanonicalDecoder $ Versioned @v <$> D.decodeWord64Canonical

instance FromCanonicalCBOR v Int where
  fromCanonicalCBOR = assumeCanonicalDecoder $ Versioned @v <$> D.decodeIntCanonical

instance FromCanonicalCBOR v Int32 where
  fromCanonicalCBOR = assumeCanonicalDecoder $ Versioned @v <$> D.decodeInt32Canonical

instance FromCanonicalCBOR v Int64 where
  fromCanonicalCBOR = assumeCanonicalDecoder $ Versioned @v <$> D.decodeInt64Canonical

instance FromCanonicalCBOR v Float where
  fromCanonicalCBOR = assumeCanonicalDecoder $ Versioned @v <$> D.decodeFloatCanonical

instance FromCanonicalCBOR v Double where
  fromCanonicalCBOR = assumeCanonicalDecoder $ Versioned @v <$> D.decodeDoubleCanonical

--------------------------------------------------------------------------------
-- Bytes
--------------------------------------------------------------------------------

instance FromCanonicalCBOR v ByteString where
  fromCanonicalCBOR = assumeCanonicalDecoder $ Versioned @v <$> D.decodeBytesCanonical

instance FromCanonicalCBOR v SBS.ShortByteString where
  fromCanonicalCBOR = do
    BA.BA (Prim.ByteArray ba) <- assumeCanonicalDecoder D.decodeByteArrayCanonical
    pure $ Versioned @v $ SBS.SBS ba

--------------------------------------------------------------------------------
-- Text
--------------------------------------------------------------------------------

instance FromCanonicalCBOR v T.Text where
  fromCanonicalCBOR = Versioned @v <$> assumeCanonicalDecoder D.decodeStringCanonical

--------------------------------------------------------------------------------
-- Tuples
--------------------------------------------------------------------------------

instance
  (FromCanonicalCBOR v a, FromCanonicalCBOR v b) =>
  FromCanonicalCBOR v (a, b)
  where
  fromCanonicalCBOR = do
    assumeCanonicalDecoder $ D.decodeListLenCanonicalOf 2
    Versioned a <- fromCanonicalCBOR @v
    Versioned b <- fromCanonicalCBOR @v
    pure $ Versioned @v (a, b)

instance
  (FromCanonicalCBOR v a, FromCanonicalCBOR v b, FromCanonicalCBOR v c) =>
  FromCanonicalCBOR v (a, b, c)
  where
  fromCanonicalCBOR = do
    assumeCanonicalDecoder $ D.decodeListLenCanonicalOf 3
    Versioned a <- fromCanonicalCBOR @v
    Versioned b <- fromCanonicalCBOR @v
    Versioned c <- fromCanonicalCBOR @v
    pure $ Versioned @v (a, b, c)

instance
  ( FromCanonicalCBOR v a
  , FromCanonicalCBOR v b
  , FromCanonicalCBOR v c
  , FromCanonicalCBOR v d
  ) =>
  FromCanonicalCBOR v (a, b, c, d)
  where
  fromCanonicalCBOR = do
    assumeCanonicalDecoder $ D.decodeListLenCanonicalOf 4
    Versioned a <- fromCanonicalCBOR @v
    Versioned b <- fromCanonicalCBOR @v
    Versioned c <- fromCanonicalCBOR @v
    Versioned d <- fromCanonicalCBOR @v
    pure $ Versioned @v (a, b, c, d)

instance
  ( FromCanonicalCBOR v a
  , FromCanonicalCBOR v b
  , FromCanonicalCBOR v c
  , FromCanonicalCBOR v d
  , FromCanonicalCBOR v e
  ) =>
  FromCanonicalCBOR v (a, b, c, d, e)
  where
  fromCanonicalCBOR = do
    assumeCanonicalDecoder $ D.decodeListLenCanonicalOf 5
    Versioned a <- fromCanonicalCBOR @v
    Versioned b <- fromCanonicalCBOR @v
    Versioned c <- fromCanonicalCBOR @v
    Versioned d <- fromCanonicalCBOR @v
    Versioned e <- fromCanonicalCBOR @v
    pure $ Versioned @v (a, b, c, d, e)

instance
  ( FromCanonicalCBOR v a
  , FromCanonicalCBOR v b
  , FromCanonicalCBOR v c
  , FromCanonicalCBOR v d
  , FromCanonicalCBOR v e
  , FromCanonicalCBOR v f
  ) =>
  FromCanonicalCBOR v (a, b, c, d, e, f)
  where
  fromCanonicalCBOR = do
    assumeCanonicalDecoder $ D.decodeListLenCanonicalOf 6
    Versioned a <- fromCanonicalCBOR @v
    Versioned b <- fromCanonicalCBOR @v
    Versioned c <- fromCanonicalCBOR @v
    Versioned d <- fromCanonicalCBOR @v
    Versioned e <- fromCanonicalCBOR @v
    Versioned f <- fromCanonicalCBOR @v
    pure $ Versioned @v (a, b, c, d, e, f)

instance
  ( FromCanonicalCBOR v a
  , FromCanonicalCBOR v b
  , FromCanonicalCBOR v c
  , FromCanonicalCBOR v d
  , FromCanonicalCBOR v e
  , FromCanonicalCBOR v f
  , FromCanonicalCBOR v g
  ) =>
  FromCanonicalCBOR v (a, b, c, d, e, f, g)
  where
  fromCanonicalCBOR = do
    assumeCanonicalDecoder $ D.decodeListLenCanonicalOf 7
    Versioned a <- fromCanonicalCBOR @v
    Versioned b <- fromCanonicalCBOR @v
    Versioned c <- fromCanonicalCBOR @v
    Versioned d <- fromCanonicalCBOR @v
    Versioned e <- fromCanonicalCBOR @v
    Versioned f <- fromCanonicalCBOR @v
    Versioned g <- fromCanonicalCBOR @v
    pure $ Versioned @v (a, b, c, d, e, f, g)

instance
  ( FromCanonicalCBOR v a
  , FromCanonicalCBOR v b
  , FromCanonicalCBOR v c
  , FromCanonicalCBOR v d
  , FromCanonicalCBOR v e
  , FromCanonicalCBOR v f
  , FromCanonicalCBOR v g
  , FromCanonicalCBOR v h
  ) =>
  FromCanonicalCBOR v (a, b, c, d, e, f, g, h)
  where
  fromCanonicalCBOR = do
    assumeCanonicalDecoder $ D.decodeListLenCanonicalOf 8
    Versioned a <- fromCanonicalCBOR @v
    Versioned b <- fromCanonicalCBOR @v
    Versioned c <- fromCanonicalCBOR @v
    Versioned d <- fromCanonicalCBOR @v
    Versioned e <- fromCanonicalCBOR @v
    Versioned f <- fromCanonicalCBOR @v
    Versioned g <- fromCanonicalCBOR @v
    Versioned h <- fromCanonicalCBOR @v
    pure $ Versioned @v (a, b, c, d, e, f, g, h)

instance
  ( FromCanonicalCBOR v a
  , FromCanonicalCBOR v b
  , FromCanonicalCBOR v c
  , FromCanonicalCBOR v d
  , FromCanonicalCBOR v e
  , FromCanonicalCBOR v f
  , FromCanonicalCBOR v g
  , FromCanonicalCBOR v h
  , FromCanonicalCBOR v i
  ) =>
  FromCanonicalCBOR v (a, b, c, d, e, f, g, h, i)
  where
  fromCanonicalCBOR = do
    assumeCanonicalDecoder $ D.decodeListLenCanonicalOf 9
    Versioned a <- fromCanonicalCBOR @v
    Versioned b <- fromCanonicalCBOR @v
    Versioned c <- fromCanonicalCBOR @v
    Versioned d <- fromCanonicalCBOR @v
    Versioned e <- fromCanonicalCBOR @v
    Versioned f <- fromCanonicalCBOR @v
    Versioned g <- fromCanonicalCBOR @v
    Versioned h <- fromCanonicalCBOR @v
    Versioned i <- fromCanonicalCBOR @v
    pure $ Versioned @v (a, b, c, d, e, f, g, h, i)

instance
  ( FromCanonicalCBOR v a
  , FromCanonicalCBOR v b
  , FromCanonicalCBOR v c
  , FromCanonicalCBOR v d
  , FromCanonicalCBOR v e
  , FromCanonicalCBOR v f
  , FromCanonicalCBOR v g
  , FromCanonicalCBOR v h
  , FromCanonicalCBOR v i
  , FromCanonicalCBOR v j
  ) =>
  FromCanonicalCBOR v (a, b, c, d, e, f, g, h, i, j)
  where
  fromCanonicalCBOR = do
    assumeCanonicalDecoder $ D.decodeListLenCanonicalOf 10
    Versioned a <- fromCanonicalCBOR @v
    Versioned b <- fromCanonicalCBOR @v
    Versioned c <- fromCanonicalCBOR @v
    Versioned d <- fromCanonicalCBOR @v
    Versioned e <- fromCanonicalCBOR @v
    Versioned f <- fromCanonicalCBOR @v
    Versioned g <- fromCanonicalCBOR @v
    Versioned h <- fromCanonicalCBOR @v
    Versioned i <- fromCanonicalCBOR @v
    Versioned j <- fromCanonicalCBOR @v
    pure $ Versioned @v (a, b, c, d, e, f, g, h, i, j)

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

-- | We always decode lists with the definite length encoding.
instance (FromCanonicalCBOR v a) => FromCanonicalCBOR v [a] where
  fromCanonicalCBOR = do
    len <- assumeCanonicalDecoder $ D.decodeListLenCanonical
    decodeSequenceLenNCanonical
      (\acc (Versioned x) -> x : acc)
      []
      (Versioned @v . reverse)
      len
      (fromCanonicalCBOR @v)

instance (FromCanonicalCBOR v a) => FromCanonicalCBOR v (Seq.Seq a) where
  fromCanonicalCBOR = fmap Seq.fromList <$> fromCanonicalCBOR

--------------------------------------------------------------------------------
-- Maps
--------------------------------------------------------------------------------

-- | We always decode maps with the definite length encoding. This does not check canonical order of keys.
instance
  (Ord k, FromCanonicalCBOR v k, FromCanonicalCBOR v val) =>
  FromCanonicalCBOR v (Map.Map k val)
  where
  fromCanonicalCBOR = do
    len <- assumeCanonicalDecoder $ D.decodeMapLenCanonical
    asList <-
      decodeSequenceLenNCanonical
        (\acc x -> x : acc)
        []
        id
        len
        decodeEntry
    -- Use Map.fromList because encoding uses encoded key byte-order,
    -- which may not match deserialized order
    pure $ Versioned @v $ Map.fromList asList
   where
    decodeEntry = do
      Versioned a <- fromCanonicalCBOR @v
      Versioned b <- fromCanonicalCBOR @v
      return (a, b)

--------------------------------------------------------------------------------
-- Set
--------------------------------------------------------------------------------

-- | We always decode sets with the definite length encoding.
instance
  (Ord a, FromCanonicalCBOR v a) =>
  FromCanonicalCBOR v (Set.Set a)
  where
  fromCanonicalCBOR = do
    258 <- decodeTagCanonical
    n <- decodeListLenCanonical
    decodeSequenceLenNCanonical
      (\acc x -> x : acc)
      []
      (coerce Set.fromList :: [Versioned v a] -> Versioned v (Set.Set a))
      n
      fromCanonicalCBOR

decodeTextCanonicalOf :: Text -> CanonicalDecoder s ()
decodeTextCanonicalOf t = do
  t' <- assumeCanonicalDecoder D.decodeStringCanonical
  if t == t'
    then
      pure ()
    else
      fail $ "expected string " ++ show t

decodeMapLenCanonical :: CanonicalDecoder s Int
decodeMapLenCanonical =
  assumeCanonicalDecoder D.decodeMapLenCanonical

decodeMapLenCanonicalOf :: Int -> CanonicalDecoder s ()
decodeMapLenCanonicalOf len = do
  len' <- decodeMapLenCanonical
  if len == len'
    then
      pure ()
    else
      fail $ "expected map of length " ++ show len

decodeListLenCanonical :: CanonicalDecoder s Int
decodeListLenCanonical =
  assumeCanonicalDecoder D.decodeListLenCanonical

decodeListLenCanonicalOf :: Int -> CanonicalDecoder s ()
decodeListLenCanonicalOf =
  assumeCanonicalDecoder . D.decodeListLenCanonicalOf

decodeWordCanonicalOf :: Word -> CanonicalDecoder s ()
decodeWordCanonicalOf =
  assumeCanonicalDecoder . D.decodeWordCanonicalOf

decodeWord8Canonical :: CanonicalDecoder s Word8
decodeWord8Canonical =
  assumeCanonicalDecoder D.decodeWord8Canonical

peekTokenType :: CanonicalDecoder s TokenType
peekTokenType =
  assumeCanonicalDecoder D.peekTokenType

decodeTagCanonical :: CanonicalDecoder s Word
decodeTagCanonical =
  assumeCanonicalDecoder D.decodeTagCanonical

decodeSequenceLenNCanonical :: (r -> a -> r) -> r -> (r -> r') -> Int -> CanonicalDecoder s a -> CanonicalDecoder s r'
decodeSequenceLenNCanonical f i f' n d =
  assumeCanonicalDecoder
    $ D.decodeSequenceLenN
      f
      i
      f'
      n
    $ getRawDecoder d

{- | This is not a production instance as basically it just performs
a check that decoded instance is canonical.
-}
instance FromCanonicalCBOR v Term where
  fromCanonicalCBOR = assumeCanonicalDecoder $ do
    term <- decodeTerm
    let termBytes = CBOR.Write.toLazyByteString $ getRawEncoding $ toCanonicalCBOR (Proxy @v) term
    case CBOR.Read.deserialiseFromBytes decodeTerm termBytes of
      Right (rest, decoded)
        | BL.null rest -> do
            unless (term == decoded) $ fail "data is not in canonical form"
            return $ Versioned @v term
        | otherwise ->
            fail "FromCanonicalCBOR<Term> leftover found when decoding canonical structure"
      Left e -> fail $ "FromCanonicalCBOR<Term> unable to do roundtrip: " <> show e
