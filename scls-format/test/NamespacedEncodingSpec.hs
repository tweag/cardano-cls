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
import Data.Function ((&))
import Data.Proxy (Proxy (Proxy))
import Streaming.Prelude qualified as S
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import TestEntry

spec :: Spec
spec = do
  describe "CanonicalCBOREncoder" $ do
    prop "should roundtrip encode/decode TestEntry successfully" $
      \(val :: TestUTxO) -> do
        let encoded = toLazyByteString $ getRawEncoding $ encodeEntry @"utxo/v0" val
        Right (_, Versioned decoded) <- pure $ deserialiseFromBytes (getRawDecoder $ decodeEntry @"utxo/v0") encoded

        decoded `shouldBe` val

  describe "SerializationPlan" $ do
    it "should accept chunks of different namespaces" $ do
      let utxos = map chunkEntryFromUTxO []
          blocks = map chunkEntryFromBlock []
          _plan =
            defaultSerializationPlan @IO
              & addNamespacedChunks (Proxy @"utxo/v0") (S.each utxos)
              -- type error: & addNamespacedChunks (Proxy @"utxo/v0") (S.each blocks)
              & addNamespacedChunks (Proxy @"blocks/v0") (S.each blocks)
      -- Just checking if the type-checker is happy
      pure () :: IO ()

  describe "TestEntry passes tests" do
    testNS @"blocks/v0"
