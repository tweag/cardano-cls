{- |
Module      : Crypto.Hash.MerkleTree.Incremental
Description : Incremental construction of Merkle Trees
Copyright   : (c) 2025 Modus Create, Inc.
License     : Apache-2.0
Maintainer  : cardano-cls@tweag.io
Stability   : experimental

This module provides functionality for constructing Merkle Trees incrementally,
allowing you to build a tree by adding elements one at a time rather than
requiring all elements upfront.

== Overview

The incremental approach is particularly useful when:

* You need to compute a Merkle root as data arrives
* Memory usage needs to be kept minimal
* You want to avoid buffering all elements before tree construction

== Algorithm

The incremental construction maintains a state that represents partial subtrees
at different levels. When a new element is added:

1. It's hashed as a leaf node
2. The algorithm attempts to join it with existing subtrees of the same level
3. This process continues recursively, building up the tree from bottom to top

== Usage Example

@
import Crypto.Hash.Algorithms (SHA3_256(..))
import Crypto.Hash.MerkleTree.Incremental
import Data.ByteString (pack)
-- Create an empty tree state
let state0 = empty SHA3_256
-- Add elements incrementally
let state1 = add state0 (pack [0x01, 0x02, 0x03])
let state2 = add state1 (pack [0x04, 0x05, 0x06])
let state3 = add state2 (pack [0x07, 0x08, 0x09])
-- Finalize to get the complete tree
let tree = finalize state3
let rootHash = merkleRootHash tree
@
-}
module Crypto.Hash.MerkleTree.Incremental (
  MerkleTree (..),
  MerkleTreeState,
  MerkleHash,
  merkleRootHash,
  empty,
  add,
  finalize,
)
where

import Crypto.Hash (HashAlgorithm, hash, hashFinalize, hashUpdate)
import Crypto.Hash.MerkleTree.Incremental.Internal
import Data.ByteArray (ByteArrayAccess)
import Data.ByteString qualified as B

-- ----------------------------------------------------------------
-- Incremental Merkle Tree construction

{- | Create an empty Merkle tree construction state.

This function initializes a new state for incremental tree construction.
The hash algorithm type is used only to determine the type; the actual
value is ignored.
-}
empty ::
  (HashAlgorithm a) =>
  -- | A value of the hash algorithm type (typically a constructor like 'Crypto.Hash.Algorithms.SHA256')
  a ->
  -- | An empty incremental Merkle tree state
  MerkleTreeState a
empty _ = MerkleTreeState []

{- | Add a new element to the incremental Merkle tree construction.

This function adds a single element to the tree state. The element can be
any type that implements 'ByteArrayAccess'.

The algorithm works by:

1. Hashing the new element as a leaf node
2. Attempting to combine it with existing subtrees of the same level
3. Recursively building up larger subtrees when possible
-}
add ::
  (HashAlgorithm a, ByteArrayAccess b) =>
  -- | The current tree construction state
  MerkleTreeState a ->
  -- | The element to add (must implement 'ByteArrayAccess')
  b ->
  -- | The updated tree construction state with the element incorporated
  MerkleTreeState a
add state =
  addLeafHash state . hashFinalize . hashUpdate leafHashInit

{- | Convert an incremental construction state into a completed Merkle tree.

This function finalizes the incremental construction process and produces
a 'MerkleTree' that can be used to extract the root hash.

The finalization process:

1. If the state is empty, returns 'MerkleTreeEmpty'
2. If there's only one subtree, that becomes the root
3. If there are multiple subtrees, combines them by promoting smaller
   subtrees to higher levels until only one remains

__Note:__ You cannot add more elements to the resulting tree after
finalization. If you need to add more elements, keep the state before calling 'finalize'.
-}
finalize ::
  (HashAlgorithm a) =>
  -- | The construction state to finalize
  MerkleTreeState a ->
  -- | The completed Merkle tree
  MerkleTree a
finalize (MerkleTreeState []) = MerkleTreeEmpty
finalize (MerkleTreeState [MerkleTreeStateNode !_ !merkleHash]) =
  MerkleTreeRoot merkleHash
finalize (MerkleTreeState (MerkleTreeStateNode level1 hash1 : node2@(MerkleTreeStateNode level2 hash2) : xs))
  | level1 == level2 = finalize (MerkleTreeState (MerkleTreeStateNode (level1 + 1) (nodeHash hash2 hash1) : xs))
  | otherwise = finalize (MerkleTreeState (MerkleTreeStateNode (level1 + 1) hash1 : node2 : xs))

{- | Extract the root hash from a completed Merkle tree.

The root hash uniquely identifies the contents and structure of the tree.
Trees with the same elements in the same order will have identical root hashes.
-}
merkleRootHash ::
  forall a.
  (HashAlgorithm a) =>
  -- | The completed Merkle tree
  MerkleTree a ->
  {- | Returns the root hash of the tree:

       * For an empty tree ('MerkleTreeEmpty'), returns the hash of an empty bytestring
       * For a non-empty tree ('MerkleTreeRoot'), returns the stored root hash
  -}
  MerkleHash a
merkleRootHash MerkleTreeEmpty = hash B.empty
merkleRootHash (MerkleTreeRoot h) = h
