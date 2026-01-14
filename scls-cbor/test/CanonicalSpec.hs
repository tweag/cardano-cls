{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CanonicalSpec (
  tests,
) where

import Cardano.SCLS.CBOR.Canonical (CanonicalDecoder (getRawDecoder), CanonicalEncoding (getRawEncoding))
import Cardano.SCLS.CBOR.Canonical.Decoder (FromCanonicalCBOR (fromCanonicalCBOR))
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (toCanonicalCBOR))
import Cardano.SCLS.Versioned
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Decoding qualified as D
import Codec.CBOR.FlatTerm (fromFlatTerm, toFlatTerm)
import Codec.CBOR.Term (Term (..), decodeTerm)
import Codec.CBOR.Write (toLazyByteString)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short (ShortByteString)
import Data.Data (Typeable, typeRep)
import Data.Int
import Data.List (nubBy, sortOn)
import Data.Map qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString ()

tests :: Spec
tests =
  describe "Canonical CBOR encoding" do
    let versions = ["base/v0"]
    forM_
      versions
      ( \v ->
          describe (show v) $ do
            roundtrip Proxy (Proxy @())
            roundtrip Proxy (Proxy @(Int, Int))
            roundtrip Proxy (Proxy @(Int, Int, Int))
            roundtrip Proxy (Proxy @(Int, Int, Int, Int))
            roundtrip Proxy (Proxy @(Int, Int, Int, Int, Int))
            roundtrip Proxy (Proxy @(Int, Int, Int, Int, Int, Int))
            roundtrip Proxy (Proxy @(Int, Int, Int, Int, Int, Int, Int))
            roundtrip Proxy (Proxy @(Int, Int, Int, Int, Int, Int, Int, Int))
            roundtrip Proxy (Proxy @(Int, Int, Int, Int, Int, Int, Int, Int, Int))
            roundtrip Proxy (Proxy @(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int))
            roundtrip Proxy (Proxy @Integer)
            roundtrip Proxy (Proxy @Word)
            roundtrip Proxy (Proxy @Word8)
            roundtrip Proxy (Proxy @Word16)
            roundtrip Proxy (Proxy @Word32)
            roundtrip Proxy (Proxy @Word64)
            roundtrip Proxy (Proxy @Int)
            roundtrip Proxy (Proxy @Int32)
            roundtrip Proxy (Proxy @Int64)
            roundtrip Proxy (Proxy @ByteString)
            roundtrip Proxy (Proxy @ShortByteString)
            roundtrip Proxy (Proxy @(Int, ByteString))
            roundtrip Proxy (Proxy @[Int])
            roundtrip Proxy (Proxy @(Seq.Seq Int))
            roundtrip Proxy (Proxy @Bool)
            roundtrip Proxy (Proxy @Float)
            roundtrip Proxy (Proxy @Double)
            roundtrip Proxy (Proxy @(Map.Map Int ByteString))
            roundtrip Proxy (Proxy @(Set.Set ByteString))
            roundtripWith Proxy (Proxy @T.Text) T.pack
            roundtripWith
              Proxy
              (Proxy @Term)
              ( \x ->
                  let encodedBytes = toFlatTerm $ getRawEncoding $ toCanonicalCBOR Proxy (x :: Term)
                   in case fromFlatTerm decodeTerm encodedBytes of
                        Right decoded -> decoded
                        Left s -> error $ "Unexpected error, can't decode encoded term: " <> show s
              )
            roundtripWith
              Proxy
              (Proxy @Natural)
              ( \i ->
                  fromIntegral $ if i < (0 :: Integer) then -i else i
              )
      )
    prop "encoded map is ordered by encoded key byte-order" $
      forAll (genNonDuplicateList (arbitrary @Int) (arbitrary @ByteString)) $
        \list ->
          let m = Map.fromList list
              sortedList = sortOn (toLazyByteString . getRawEncoding . toCanonicalCBOR Proxy . fst) list
              decoded = fromFlatTerm (customMapDecoder Proxy) $ toFlatTerm $ getRawEncoding $ toCanonicalCBOR Proxy m
           in decoded `shouldBe` Right sortedList
 where
  roundtrip :: forall v a. (Arbitrary a, Typeable a, Show a, Eq a, ToCanonicalCBOR v a, FromCanonicalCBOR v a) => Proxy v -> Proxy a -> SpecWith ()
  roundtrip p p1 = roundtripWith p p1 id
  roundtripWith :: forall v a x. (Arbitrary a, Typeable x, Show a, Show x, Eq x, ToCanonicalCBOR v x, FromCanonicalCBOR v x) => Proxy v -> Proxy x -> (a -> x) -> SpecWith ()
  roundtripWith p p1 f = do
    describe (show $ typeRep p1) $ do
      prop "x == decode . encode x" $ do
        \(x :: a) -> do
          let encodedBytes = toFlatTerm $ getRawEncoding $ toCanonicalCBOR p (f x)
              decoded = fromFlatTerm (getRawDecoder $ fromCanonicalCBOR @v @x) encodedBytes
          -- Decoded value should match and no bytes left after decoding
          decoded `shouldBe` Right (Versioned (f x))
  -- Generate list of pairs with no duplicate first element
  genNonDuplicateList :: (Eq a) => Gen a -> Gen b -> Gen [(a, b)]
  genNonDuplicateList g1 g2 =
    nubBy (\(x, _) (y, _) -> x == y) <$> listOf g
   where
    g = do
      v1 <- g1
      v2 <- g2
      pure (v1, v2)
  -- Decode map as list of pairs
  customMapDecoder :: forall v s a b. (FromCanonicalCBOR v a, FromCanonicalCBOR v b) => Proxy v -> Decoder s ([(a, b)])
  customMapDecoder _ = do
    len <- D.decodeMapLenCanonical
    D.decodeSequenceLenN
      (\acc x -> x : acc)
      []
      reverse -- We prepend, so we must reverse at the end
      len
      ( getRawDecoder $ do
          Versioned a <- fromCanonicalCBOR @v
          Versioned b <- fromCanonicalCBOR @v
          return (a, b)
      )

-- Shameless copypaste from cborg test-suite.

instance Arbitrary Term where
  arbitrary =
    scale (pred') $
      oneof
        [ TInt <$> arbitrary
        , TInteger <$> arbitrary
        , TBytes <$> arbitrary
        , TBytesI <$> arbitrary
        , TString . T.pack <$> arbitrary
        , TStringI . TL.pack <$> arbitrary
        , TList <$> listOf_ arbitrary
        , TListI <$> listOf_ arbitrary
        , TMap <$> listOf_ arbitrary
        , TMapI <$> listOf_ arbitrary
        , TTagged <$> arbitrary `suchThat` (5 <) <*> scale (pred') arbitrary
        , TBool <$> scale (pred') arbitrary
        , pure TNull
        , TSimple <$> scale (pred') arbitrary
        , THalf <$> scale (pred') arbitrary
        , TFloat <$> scale (pred') arbitrary
        , TDouble <$> scale (pred') arbitrary
        ]
   where
    pred' 0 = 0
    pred' x = (x - 1)
    listOf_ gen = sized $ \n -> do
      k <- choose (0, n)
      vectorOf k (resize (n `div` (k + 1)) gen)

  shrink (TInt n) = [TInt n' | n' <- shrink n]
  shrink (TInteger n) = [TInteger n' | n' <- shrink n]
  shrink (TBytes ws) = [TBytes (BS.pack ws') | ws' <- shrink (BS.unpack ws)]
  shrink (TBytesI wss) =
    [ TBytesI (BSL.fromChunks (map BS.pack wss'))
    | wss' <- shrink (map BS.unpack (BSL.toChunks wss))
    ]
  shrink (TString cs) = [TString (T.pack cs') | cs' <- shrink (T.unpack cs)]
  shrink (TStringI css) =
    [ TStringI (TL.fromChunks (map T.pack css'))
    | css' <- shrink (map T.unpack (TL.toChunks css))
    ]
  shrink (TList xs@[x]) = x : [TList xs' | xs' <- shrink xs]
  shrink (TList xs) = [TList xs' | xs' <- shrink xs]
  shrink (TListI xs@[x]) = x : [TListI xs' | xs' <- shrink xs]
  shrink (TListI xs) = [TListI xs' | xs' <- shrink xs]
  shrink (TMap xys@[(x, y)]) = x : y : [TMap xys' | xys' <- shrink xys]
  shrink (TMap xys) = [TMap xys' | xys' <- shrink xys]
  shrink (TMapI xys@[(x, y)]) = x : y : [TMapI xys' | xys' <- shrink xys]
  shrink (TMapI xys) = [TMapI xys' | xys' <- shrink xys]
  shrink (TTagged w t) =
    t
      : [ TTagged w' t'
        | (w', t') <- shrink (w, t)
        , not (w <= 5)
        ]
  shrink (TBool _) = []
  shrink TNull = []
  shrink (TSimple w) =
    [ TSimple w'
    | w' <- shrink w
    , w < 20 || w > 31 || w == 23
    ]
  shrink (THalf _f) = []
  shrink (TFloat f) = [TFloat f' | f' <- shrink f]
  shrink (TDouble f) = [TDouble f' | f' <- shrink f]
