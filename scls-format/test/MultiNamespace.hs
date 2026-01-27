{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module MultiNamespace (
  tests,
) where

import Cardano.SCLS.Internal.Hash (Digest (..))
import Cardano.SCLS.Internal.Reader (extractNamespaceHash, extractNamespaceList, extractRootHash, withNamespacedData)
import Cardano.SCLS.Internal.Serializer.Dump.Plan (SerializationPlan, addChunks, defaultSerializationPlan)
import Cardano.SCLS.Internal.Serializer.External.Impl qualified as External (serialize)
import Cardano.SCLS.Internal.Serializer.Reference.Impl qualified as Reference (serialize)
import Cardano.Types.Namespace (Namespace)
import Cardano.Types.Namespace qualified as Namespace
import Cardano.Types.SlotNo (SlotNo (..))
import Control.Monad.Trans.Resource (ResIO, runResourceT)
import Crypto.Hash.MerkleTree.Incremental qualified as MT
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Function ((&))
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.MemPack.Extra
import Data.Traversable (for)
import Streaming.Prelude qualified as S
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.Expectations.Contrib

tests :: Spec
tests =
  describe "Multi-namespace serialization" do
    describe "reference" do
      mkTestsFor (Reference.serialize @RawBytes)
    describe "external" do
      mkTestsFor (External.serialize @RawBytes)

mkTestsFor :: SerializeF -> Spec
mkTestsFor serialize = do
  it "works for a single element in each namespace" do
    roundtrip
      serialize
      [("ns0", ["a"]), ("ns1", ["1"])]
  it "works for multiple elements ordered by namespace with duplicates" do
    roundtrip
      serialize
      [("ns0", ["a", "b", "b", "c"]), ("ns1", ["1", "2", "2", "3"])]
  it "works for multiple elements ordered by namespace" do
    roundtrip
      serialize
      [("ns0", ["a", "b", "c"]), ("ns1", ["1", "2", "3"])]
  it "works for multiple elements mixed order" do
    roundtrip
      serialize
      [("ns0", ["a", "b"]), ("ns1", ["1"]), ("ns0", ["c"]), ("ns1", ["2", "3"])]
  it "works for long sequence" do
    roundtrip
      serialize
      [ ("ns0", [BS8.pack (show (i :: Int)) | i <- [1 .. 2048]])
      , ("ns1", [BS8.pack (show (i :: Int)) | i <- [1 .. 2048]])
      ]
  it "works for unordered streams" do
    roundtrip
      serialize
      [ ("utxo/v0", ["d", "a", "c", "b"])
      , ("blocks/v0", ["3", "1", "4", "2"])
      , ("ns2", ["z", "x", "y"])
      ]

  it "handles empty namespaces correctly" do
    let input = [("ns0", []), ("ns1", ["data"]), ("ns2", [])]
    roundtrip serialize input

type SerializeF = FilePath -> SlotNo -> Map.Map String Int -> SerializationPlan RawBytes ResIO -> ResIO (Either [Namespace] ())

roundtrip :: SerializeF -> [(Namespace, [ByteString])] -> IO ()
roundtrip serialize input = do
  withSystemTempDirectory "scls-format-test-XXXXXX" $ \fn -> do
    let fileName = fn </> "input.data"
    _ <-
      runResourceT $
        serialize
          fileName
          (SlotNo 1)
          (Map.fromList [(Namespace.asString ns, 1) | (ns, _) <- input])
          mkPlan
    nsps <- extractNamespaceList fileName
    annotate "Namespaces are as expected" do
      (sort nsps) `shouldBe` (Map.keys nsData)

    ns_digests <-
      for (Map.toList nsData) \(n, q) -> do
        withNamespacedData
          fileName
          n
          ( \stream -> do
              decoded_data <- S.toList_ stream
              zs <- S.toList_ (S.each (sort q) & S.nubOrd)
              annotate
                (Namespace.asString n <> ": stream roundtrip successful")
                $ [b | RawBytes b <- decoded_data]
                  `shouldBe` zs
          )
        fileDigest <- extractNamespaceHash n fileName
        expectedDigest <-
          S.each (sort q)
            & S.nubOrd
            & S.fold_ MT.add (MT.empty undefined) (Digest . MT.merkleRootHash . MT.finalize)
        annotate (Namespace.asString n <> " hash matches expected") do
          fileDigest `shouldBe` (Just expectedDigest)
        pure expectedDigest
    fileDigest <- extractRootHash fileName
    expectedDigest <-
      S.each (ns_digests)
        & S.fold_ MT.add (MT.empty undefined) (Digest . MT.merkleRootHash . MT.finalize)

    annotate "File hash matches expected" do
      fileDigest `shouldBe` expectedDigest
 where
  mkPlan =
    defaultSerializationPlan
      & ( addChunks $
            S.each
              [ n S.:> (S.each q & S.map RawBytes)
              | (n, q) <- input
              ]
        )
  nsData = Map.fromListWith (<>) input
