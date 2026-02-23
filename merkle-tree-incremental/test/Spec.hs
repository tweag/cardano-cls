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
