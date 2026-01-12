{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Tests for namespace-versioned encoding and decoding.

This module tests the namespace-versioned serialization mechanism that
associates data types with specific namespaces and versions.
-}
module NamespacedEncodingSpec where

import Cardano.SCLS.CBOR.Canonical (getRawDecoder, getRawEncoding)
import Cardano.SCLS.Internal.Serializer.Dump.Plan (addNamespacedChunks, defaultSerializationPlan)
import Cardano.SCLS.NamespaceCodec (CanonicalCBOREntryDecoder (decodeEntry), CanonicalCBOREntryEncoder (encodeEntry))
import Cardano.SCLS.Testlib
import Cardano.SCLS.Versioned (Versioned (Versioned))
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write
import Control.Monad (replicateM)
import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import Streaming.Prelude qualified as S
import Test.Hspec (Spec, describe, it, shouldBe)
import TestEntry

spec :: Spec
spec = do
  describe "CanonicalCBOREncoder" $ do
    it "should encode TestEntry in utxo/v0 namespace" $ do
      val <- genUTxO
      let _ = encodeEntry @"utxo/v0" val
      -- Just checking if the compiler and type-checker are fine
      pure ()

    it "should roundtrip encode/decode TestEntry successfully" $ do
      val <- genUTxO

      Right (_, decoded) <- pure $ deserialiseFromBytes (getRawDecoder $ decodeEntry @"utxo/v0") $ toLazyByteString $ getRawEncoding $ encodeEntry @"utxo/v0" val

      decoded `shouldBe` (Versioned val)

  describe "SerializationPlan" $ do
    it "should accept chunks of different namespaces" $ do
      utxos <- replicateM 10 (chunkEntryFromUTxO <$> genUTxO)
      blocks <- replicateM 10 (chunkEntryFromBlock <$> genBlock)
      let _plan =
            defaultSerializationPlan @IO
              & addNamespacedChunks (Proxy @"utxo/v0") (S.each utxos)
              -- type error: & addNamespacedChunks (Proxy @"utxo/v0") (S.each blocks)
              & addNamespacedChunks (Proxy @"blocks/v0") (S.each blocks)
      -- Just checking if the type-checker is happy
      pure ()

  describe "TestEntry passes tests" do
    testNS @"blocks/v0"
