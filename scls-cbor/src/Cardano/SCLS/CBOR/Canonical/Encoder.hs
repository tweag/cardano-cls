{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

{- | Canonical CBOR representation

This module contains only instances for cases where there is a direct and
unambiguous instance that will remain the same across all versions. For
example, since there are multiple possible ways to encode 'Maybe' we do
not provide an instance here - it is better to specify the precise encoding
when defining the instances for specific types.
-}
module Cardano.SCLS.CBOR.Canonical.Encoder (
  ToCanonicalCBOR (..),
  encodeAsMap,
  SomeEncodablePair (..),
  mkEncodablePair,
  forceCanonical,
  canonicalizeTerm,
) where

import Cardano.SCLS.CBOR.Canonical (CanonicalEncoding (getRawEncoding), assumeCanonicalEncoding)
import Codec.CBOR.ByteArray.Sliced qualified as BAS
import Codec.CBOR.Encoding qualified as E
import Codec.CBOR.FlatTerm (fromFlatTerm, toFlatTerm)
import Codec.CBOR.Term
import Codec.CBOR.Write (toStrictByteString)
import Data.Array.Byte qualified as Prim
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short (ShortByteString (SBS))
import Data.ByteString.Short qualified as SBS
import Data.Foldable (toList)
import Data.Int
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Traversable (mapAccumL)
import Data.Word
import GHC.Stack (HasCallStack)
import GHC.TypeLits

-- | Encode data to CBOR corresponding with the SCLS format.
class ToCanonicalCBOR (v :: Symbol) a where
  -- | Encode to canonical CBOR at a given version
  toCanonicalCBOR :: proxy v -> a -> CanonicalEncoding

--------------------------------------------------------------------------------
-- Encoding, Term etc
--------------------------------------------------------------------------------

instance ToCanonicalCBOR v CanonicalEncoding where
  toCanonicalCBOR _ = id

--------------------------------------------------------------------------------
-- Primitive types
--------------------------------------------------------------------------------

instance ToCanonicalCBOR v () where
  toCanonicalCBOR _ = assumeCanonicalEncoding . const E.encodeNull

instance ToCanonicalCBOR v Bool where
  toCanonicalCBOR _ = assumeCanonicalEncoding . E.encodeBool

--------------------------------------------------------------------------------
-- Numeric data
--------------------------------------------------------------------------------

instance ToCanonicalCBOR v Integer where
  toCanonicalCBOR _ = assumeCanonicalEncoding . E.encodeInteger

instance ToCanonicalCBOR v Word where
  toCanonicalCBOR _ = assumeCanonicalEncoding . E.encodeWord

instance ToCanonicalCBOR v Word8 where
  toCanonicalCBOR _ = assumeCanonicalEncoding . E.encodeWord8

instance ToCanonicalCBOR v Word16 where
  toCanonicalCBOR _ = assumeCanonicalEncoding . E.encodeWord16

instance ToCanonicalCBOR v Word32 where
  toCanonicalCBOR _ = assumeCanonicalEncoding . E.encodeWord32

instance ToCanonicalCBOR v Word64 where
  toCanonicalCBOR _ = assumeCanonicalEncoding . E.encodeWord64

instance ToCanonicalCBOR v Int where
  toCanonicalCBOR _ = assumeCanonicalEncoding . E.encodeInt

instance ToCanonicalCBOR v Int32 where
  toCanonicalCBOR _ = assumeCanonicalEncoding . E.encodeInt32

instance ToCanonicalCBOR v Int64 where
  toCanonicalCBOR _ = assumeCanonicalEncoding . E.encodeInt64

instance ToCanonicalCBOR v Double where
  toCanonicalCBOR _ = assumeCanonicalEncoding . E.encodeDouble

instance ToCanonicalCBOR v Float where
  toCanonicalCBOR _ = assumeCanonicalEncoding . E.encodeFloat

--------------------------------------------------------------------------------
-- Bytes
--------------------------------------------------------------------------------

instance ToCanonicalCBOR v ByteString where
  toCanonicalCBOR _ = assumeCanonicalEncoding . E.encodeBytes

instance ToCanonicalCBOR v SBS.ShortByteString where
  toCanonicalCBOR _ sbs@(SBS ba) =
    assumeCanonicalEncoding $
      E.encodeByteArray $
        BAS.SBA (Prim.ByteArray ba) 0 (SBS.length sbs)

--------------------------------------------------------------------------------
-- Text
--------------------------------------------------------------------------------

instance ToCanonicalCBOR v Text where
  toCanonicalCBOR _ = assumeCanonicalEncoding . E.encodeString

--------------------------------------------------------------------------------
-- Tuples
--------------------------------------------------------------------------------

instance
  (ToCanonicalCBOR v a, ToCanonicalCBOR v b) =>
  ToCanonicalCBOR v (a, b)
  where
  toCanonicalCBOR v (a, b) =
    assumeCanonicalEncoding (E.encodeListLen 2)
      <> toCanonicalCBOR v a
      <> toCanonicalCBOR v b

instance
  ( ToCanonicalCBOR v a
  , ToCanonicalCBOR v b
  , ToCanonicalCBOR v c
  ) =>
  ToCanonicalCBOR v (a, b, c)
  where
  toCanonicalCBOR v (a, b, c) =
    assumeCanonicalEncoding (E.encodeListLen 3)
      <> toCanonicalCBOR v a
      <> toCanonicalCBOR v b
      <> toCanonicalCBOR v c

instance
  ( ToCanonicalCBOR v a
  , ToCanonicalCBOR v b
  , ToCanonicalCBOR v c
  , ToCanonicalCBOR v d
  ) =>
  ToCanonicalCBOR v (a, b, c, d)
  where
  toCanonicalCBOR v (a, b, c, d) =
    assumeCanonicalEncoding (E.encodeListLen 4)
      <> toCanonicalCBOR v a
      <> toCanonicalCBOR v b
      <> toCanonicalCBOR v c
      <> toCanonicalCBOR v d

instance
  ( ToCanonicalCBOR v a
  , ToCanonicalCBOR v b
  , ToCanonicalCBOR v c
  , ToCanonicalCBOR v d
  , ToCanonicalCBOR v e
  ) =>
  ToCanonicalCBOR v (a, b, c, d, e)
  where
  toCanonicalCBOR v (a, b, c, d, e) =
    assumeCanonicalEncoding (E.encodeListLen 5)
      <> toCanonicalCBOR v a
      <> toCanonicalCBOR v b
      <> toCanonicalCBOR v c
      <> toCanonicalCBOR v d
      <> toCanonicalCBOR v e

instance
  ( ToCanonicalCBOR v a
  , ToCanonicalCBOR v b
  , ToCanonicalCBOR v c
  , ToCanonicalCBOR v d
  , ToCanonicalCBOR v e
  , ToCanonicalCBOR v f
  ) =>
  ToCanonicalCBOR v (a, b, c, d, e, f)
  where
  toCanonicalCBOR v (a, b, c, d, e, f) =
    assumeCanonicalEncoding (E.encodeListLen 6)
      <> toCanonicalCBOR v a
      <> toCanonicalCBOR v b
      <> toCanonicalCBOR v c
      <> toCanonicalCBOR v d
      <> toCanonicalCBOR v e
      <> toCanonicalCBOR v f

instance
  ( ToCanonicalCBOR v a
  , ToCanonicalCBOR v b
  , ToCanonicalCBOR v c
  , ToCanonicalCBOR v d
  , ToCanonicalCBOR v e
  , ToCanonicalCBOR v f
  , ToCanonicalCBOR v g
  ) =>
  ToCanonicalCBOR v (a, b, c, d, e, f, g)
  where
  toCanonicalCBOR v (a, b, c, d, e, f, g) =
    assumeCanonicalEncoding (E.encodeListLen 7)
      <> toCanonicalCBOR v a
      <> toCanonicalCBOR v b
      <> toCanonicalCBOR v c
      <> toCanonicalCBOR v d
      <> toCanonicalCBOR v e
      <> toCanonicalCBOR v f
      <> toCanonicalCBOR v g

instance
  ( ToCanonicalCBOR v a
  , ToCanonicalCBOR v b
  , ToCanonicalCBOR v c
  , ToCanonicalCBOR v d
  , ToCanonicalCBOR v e
  , ToCanonicalCBOR v f
  , ToCanonicalCBOR v g
  , ToCanonicalCBOR v h
  ) =>
  ToCanonicalCBOR v (a, b, c, d, e, f, g, h)
  where
  toCanonicalCBOR v (a, b, c, d, e, f, g, h) =
    assumeCanonicalEncoding (E.encodeListLen 8)
      <> toCanonicalCBOR v a
      <> toCanonicalCBOR v b
      <> toCanonicalCBOR v c
      <> toCanonicalCBOR v d
      <> toCanonicalCBOR v e
      <> toCanonicalCBOR v f
      <> toCanonicalCBOR v g
      <> toCanonicalCBOR v h

instance
  ( ToCanonicalCBOR v a
  , ToCanonicalCBOR v b
  , ToCanonicalCBOR v c
  , ToCanonicalCBOR v d
  , ToCanonicalCBOR v e
  , ToCanonicalCBOR v f
  , ToCanonicalCBOR v g
  , ToCanonicalCBOR v h
  , ToCanonicalCBOR v i
  ) =>
  ToCanonicalCBOR v (a, b, c, d, e, f, g, h, i)
  where
  toCanonicalCBOR v (a, b, c, d, e, f, g, h, i) =
    assumeCanonicalEncoding (E.encodeListLen 9)
      <> toCanonicalCBOR v a
      <> toCanonicalCBOR v b
      <> toCanonicalCBOR v c
      <> toCanonicalCBOR v d
      <> toCanonicalCBOR v e
      <> toCanonicalCBOR v f
      <> toCanonicalCBOR v g
      <> toCanonicalCBOR v h
      <> toCanonicalCBOR v i

instance
  ( ToCanonicalCBOR v a
  , ToCanonicalCBOR v b
  , ToCanonicalCBOR v c
  , ToCanonicalCBOR v d
  , ToCanonicalCBOR v e
  , ToCanonicalCBOR v f
  , ToCanonicalCBOR v g
  , ToCanonicalCBOR v h
  , ToCanonicalCBOR v i
  , ToCanonicalCBOR v j
  ) =>
  ToCanonicalCBOR v (a, b, c, d, e, f, g, h, i, j)
  where
  toCanonicalCBOR v (a, b, c, d, e, f, g, h, i, j) =
    assumeCanonicalEncoding (E.encodeListLen 10)
      <> toCanonicalCBOR v a
      <> toCanonicalCBOR v b
      <> toCanonicalCBOR v c
      <> toCanonicalCBOR v d
      <> toCanonicalCBOR v e
      <> toCanonicalCBOR v f
      <> toCanonicalCBOR v g
      <> toCanonicalCBOR v h
      <> toCanonicalCBOR v i
      <> toCanonicalCBOR v j

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

-- | We always encode lists with the definite length encoding.
instance (ToCanonicalCBOR v a) => ToCanonicalCBOR v [a] where
  toCanonicalCBOR v xs =
    (assumeCanonicalEncoding (E.encodeListLen (fromIntegral $ length xs))) <> foldMap (toCanonicalCBOR v) xs

instance (ToCanonicalCBOR v a) => ToCanonicalCBOR v (Seq.Seq a) where
  toCanonicalCBOR v xs =
    (assumeCanonicalEncoding (E.encodeListLen (fromIntegral $ length xs))) <> foldMap (toCanonicalCBOR v) xs

--------------------------------------------------------------------------------
-- Maps
--------------------------------------------------------------------------------

-- | We always encode maps with the definite length encoding and ordered by encoded keys byte-order.
instance
  (ToCanonicalCBOR v k, ToCanonicalCBOR v val) =>
  ToCanonicalCBOR v (Map.Map k val)
  where
  toCanonicalCBOR _v m =
    encodeAsMap l
   where
    l =
      Map.foldlWithKey'
        (\acc k val -> SomeEncodablePair @v k val : acc)
        []
        m

{- | An existential wrapper for a key-value pair where both the key and value
can be canonically CBOR-encoded under the version @v@.

This type is useful for encoding heterogeneous collections of key-value pairs
as CBOR maps, where the key and value types may vary but all support
'ToCanonicalCBOR' for the same version.

Use 'mkEncodablePair' to construct values of this type.
-}
data SomeEncodablePair v where
  SomeEncodablePair :: (ToCanonicalCBOR v k, ToCanonicalCBOR v val) => k -> val -> SomeEncodablePair v

{- | Construct a 'SomeEncodablePair' from a key and value, given that both
support 'ToCanonicalCBOR' for the version `v`.
-}
mkEncodablePair :: (ToCanonicalCBOR v k, ToCanonicalCBOR v val) => proxy v -> k -> val -> SomeEncodablePair v
mkEncodablePair _ = SomeEncodablePair

{- |
  Helper for encoding map-like structures in canonical CBOR form.
  This function takes a Traversable collection of 'SomeEncodablePair' values and encodes them as a CBOR map,
  using definite length encoding. Keys are sorted by their canonical CBOR-encoded byte representation,
  as required by the canonical CBOR specification.
-}
encodeAsMap :: forall v t. (Traversable t) => t (SomeEncodablePair v) -> CanonicalEncoding
encodeAsMap f =
  (assumeCanonicalEncoding (E.encodeMapLen len))
    <> foldMap
      (\(kBytes, valEncoding) -> assumeCanonicalEncoding (E.encodePreEncoded kBytes) <> valEncoding)
      sorted
 where
  -- Order map by the byte-wise ordering of the canonically encoded map keys
  (len, l) =
    mapAccumL
      ( \(!n) (SomeEncodablePair k val) ->
          let !kBytes = toStrictByteString $ getRawEncoding $ toCanonicalCBOR (Proxy @v) k
              valEncoding = toCanonicalCBOR (Proxy @v) val
           in (n + 1, (kBytes, valEncoding))
      )
      0
      f
  sorted = List.sortOn fst $ toList l

--------------------------------------------------------------------------------
-- Set
--------------------------------------------------------------------------------

-- See `https://github.com/input-output-hk/cbor-sets-spec/blob/master/CBOR_SETS.md`
-- for details about the implementation.
instance (ToCanonicalCBOR v a) => (ToCanonicalCBOR v (Set.Set a)) where
  toCanonicalCBOR v s =
    assumeCanonicalEncoding $
      E.encodeTag 258
        <> E.encodeListLen (fromIntegral size)
        <> foldMap E.encodePreEncoded encSorted
   where
    size = Set.size s
    encSorted = List.sort $ map (toStrictByteString . getRawEncoding . toCanonicalCBOR v) $ Set.toList s

--------------------------------------------------------------------------------
-- Term
--------------------------------------------------------------------------------

-- | Encode CBOR term in canonical form up to the cardano definition
instance ToCanonicalCBOR v Term where
  toCanonicalCBOR v = \case
    TInt i -> toCanonicalCBOR v i
    TInteger i -> toCanonicalCBOR v i
    TBytes bytes -> toCanonicalCBOR v bytes
    TBytesI lbs -> toCanonicalCBOR v (BL.toStrict lbs)
    TString s -> toCanonicalCBOR v s
    TStringI si -> toCanonicalCBOR v (TL.toStrict si)
    TList tl -> toCanonicalCBOR v tl
    TListI tli -> toCanonicalCBOR v tli
    TTagged w t -> toCanonicalTagged v w t
    TMap ls -> encodeAsMap [SomeEncodablePair k val | (k, val) <- ls]
    TMapI ls -> encodeAsMap [SomeEncodablePair k val | (k, val) <- ls]
    TBool b -> toCanonicalCBOR v b
    TNull -> assumeCanonicalEncoding $ E.encodeNull
    TSimple w -> toCanonicalCBOR v w
    THalf f -> toCanonicalCBOR v f
    TFloat f -> toCanonicalCBOR v f
    TDouble d -> toCanonicalCBOR v d

{- | Convert tags that require a special care or rules to encode
in the canonical format
-}
toCanonicalTagged :: proxy (v :: Symbol) -> Word64 -> Term -> CanonicalEncoding
toCanonicalTagged v 258 (TList ns) = toCanonicalCBOR v (Set.fromList ns)
toCanonicalTagged v t term = assumeCanonicalEncoding (E.encodeTag64 t) <> toCanonicalCBOR v term

{- |
  Converts any CBOR encoding to its canonical form by first decoding it to a generic
  CBOR 'Term' and then re-encoding it canonically according to the Cardano specification.

  **When to use:**

  Use this function as a last resort when you have a CBOR encoding ('E.Encoding') that may not
  be in canonical form and you need to ensure canonicalization. It is primarily intended for
  situations where you do not have a more direct or efficient way to produce canonical CBOR.

  **Caveats:**

  This function is slow because it fully decodes the input encoding and then re-encodes it.
  Avoid using it in performance-critical code paths.
-}
forceCanonical :: (HasCallStack) => proxy (v :: Symbol) -> E.Encoding -> CanonicalEncoding
forceCanonical p x = case fromFlatTerm decodeTerm (toFlatTerm x) of
  Left e -> error $ "forceCanonical: unable to decode term, generated by Encoding: " <> show e
  Right y -> toCanonicalCBOR p y

{- |
  Converts any CBOR term to its canonical form by first canonically encoding it according
  to the Cardano specification and then decoding it back to a generic CBOR 'Term'.
-}
canonicalizeTerm :: proxy (v :: Symbol) -> Term -> Either String Term
canonicalizeTerm p = fromFlatTerm decodeTerm . toFlatTerm . getRawEncoding . toCanonicalCBOR p
