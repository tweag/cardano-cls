{-# LANGUAGE OverloadedStrings #-}

import Crypto.Hash.Algorithms (Blake2b_224 (..), SHA3_256 (..))
import Crypto.Hash.MerkleTree.Incremental (
  add,
  empty,
  finalize,
  merkleRootHash,
 )
import Crypto.Hash.MerkleTree.Incremental.Internal (
  leafHash,
  nodeHash,
 )
import Reference (mkMerkleTree, mtHash)

import Control.Monad (forM_)
import Crypto.Hash (Digest, hash)
import Data.ByteArray (unpack)
import Data.ByteString (ByteString, pack, singleton)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.ByteString ()

main :: IO ()
main = do
  hspec $
    describe "Incremental Merkle Tree" $ do
      prop "computed Merkle root hash should equal reference implementation one" $ do
        \entries -> do
          let state = foldl add (empty SHA3_256) entries
              merkleTree1 = finalize state
              merkleTree2 = mkMerkleTree entries
          merkleRootHash merkleTree1 `shouldBe` mtHash merkleTree2
      prop "leaf hash of (prefix, entry) should H(0x01 || prefix || entry)" $ do
        \(prefix :: ByteString) (entry :: ByteString) -> do
          let computedHash = leafHash prefix entry
              expectedHash :: Digest Blake2b_224 = hash (singleton 0x01 <> prefix <> entry)
          computedHash `shouldBe` expectedHash
      prop "node hash of (left, right) should H(0x00 || H(left) || H(right))" $ do
        \(left :: ByteString) (right :: ByteString) -> do
          let leftHash = hash left :: Digest Blake2b_224
              rightHash = hash right :: Digest Blake2b_224
              computedHash = nodeHash leftHash rightHash
              leftHashBytes = pack $ unpack leftHash
              rightHashBytes = pack $ unpack rightHash
              expectedHash = hash (singleton 0x00 <> leftHashBytes <> rightHashBytes) :: Digest Blake2b_224
          computedHash `shouldBe` expectedHash
      describe "known test vectors" $ do
        let vectors =
              [ (0, "836cc68931c2e4e3e838602eca1902591d216837bafddfe6f0c8cb07")
              , (2, "d3af86eb3e383618180e9e5e67b458f351335462871682ee332e20f8")
              , (3, "37c91e733d32604abaf8ddff2b274f60e9b66a0311860ceddb5d43f4")
              , (4, "e291561d5aa6555c44991f28f70dccb86352a4cd505b894a7bf85c4c")
              , (5, "4aa703d6add1a49f1603328aa89db822122f174282c1637606c59f7c")
              , (7, "ff159b2b1d0c703bf65bb0ae36dac26420cab122cc09df37c28df3a5")
              ]
        forM_ vectors $ \(n, expectedHash) -> it ("tree with " <> show n <> " entries should have known root hash") $ do
          let entry = ("Test" :: ByteString)
          let state = foldl add (empty Blake2b_224) (replicate n entry)
              merkleTree = finalize state
          show (merkleRootHash merkleTree) `shouldBe` expectedHash
