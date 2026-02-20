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
  addWithPrefix,
  finalize,
)
where

import Crypto.Hash (Digest, HashAlgorithm, hash, hashFinalize, hashInit, hashUpdate, hashUpdates)
import Data.ByteArray (ByteArrayAccess)
import Data.ByteString qualified as B

-- ----------------------------------------------------------------
-- Merkle Tree and Hashes

{- | Type alias for hash digests used in Merkle trees.

This represents the result of applying a cryptographic hash function
to some data. The type parameter @a@ specifies the hash algorithm used.
-}
type MerkleHash a = Digest a

{- | Represents a completed Merkle tree.

A Merkle tree can be in one of two states:

* 'MerkleTreeEmpty': No elements have been added
* 'MerkleTreeRoot': Contains the root hash of a non-empty tree

This is the final result of the incremental construction process.
Use 'merkleRootHash' to extract the root hash regardless of the state.
-}
data MerkleTree a
  = -- | An empty tree with no elements
    MerkleTreeEmpty
  | -- | A tree with the given root hash
    MerkleTreeRoot !(MerkleHash a)
  deriving (Show, Eq)

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

{- | Compute the hash for a leaf node (single element).

This function creates a hash for leaf nodes by:

1. Initializing a hash context
2. Adding a prefix byte 1 to distinguish leaves from internal nodes
3. Adding the element data
4. Finalizing the hash

The prefix byte ensures that leaf hashes are distinct from internal node
hashes, preventing certain types of attacks on the tree structure.
-}
leafHash :: (HashAlgorithm a, ByteArrayAccess b1, ByteArrayAccess b2) => b1 -> b2 -> MerkleHash a
leafHash prefix b =
  hashFinalize $ flip hashUpdate b $ flip hashUpdate prefix $ hashUpdate hashInit $ B.singleton 1

{- | Compute the hash for an internal node from two child hashes.

This function creates a hash for internal nodes by:

1. Initializing a hash context
2. Adding a prefix byte 1 to distinguish internal nodes from leaves
3. Adding the hexadecimal encoding of the first child hash
4. Adding the hexadecimal encoding of the second child hash
5. Finalizing the hash

The order of arguments matters: @nodeHash left right ≠ nodeHash right left@.
The first argument (@h1@) represents the left child, and the second (@h2@)
represents the right child in the tree structure.

The prefix byte (0) ensures that internal node hashes are distinct from
leaf hashes, maintaining the integrity of the tree structure.
-}
nodeHash :: forall a. (HashAlgorithm a) => MerkleHash a -> MerkleHash a -> MerkleHash a
nodeHash h1 h2 =
  hashFinalize $ flip hashUpdates [h1, h2] $ hashUpdate hashInit $ B.singleton 0

-- ----------------------------------------------------------------
-- Incremental Merkle Tree construction

{- | State for incremental Merkle tree construction.

This opaque type maintains the internal state during incremental tree
construction. It represents a collection of partial subtrees at different
levels that haven't been combined yet.

The state is designed to be space-efficient, using O(log n) space where
n is the number of elements added so far.

Create with 'empty', add elements with 'add', and finalize with 'finalize'.
-}
newtype MerkleTreeState a = MerkleTreeState [MerkleTreeStateNode a]

{- | Internal representation of a node in the incremental construction state.

Each node represents a complete subtree at a specific level:

* Level 0: Individual leaf nodes (single elements)
* Level 1: Subtrees with 2 elements
* Level 2: Subtrees with 4 elements
* Level k: Subtrees with 2^k elements

The incremental algorithm maintains at most one subtree per level,
ensuring logarithmic space complexity.
-}
data MerkleTreeStateNode a = MerkleTreeStateNode
  { cLevel :: !Int
  -- ^ The level of this subtree (0 for leaves)
  , cHash :: !(MerkleHash a)
  -- ^ The root hash of this subtree
  }
  deriving (Show)

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
add (MerkleTreeState state) bytes =
  addWithPrefix (MerkleTreeState state) B.empty bytes

{- | Add a new element with a prefix to the incremental Merkle tree construction.
This function is similar to 'add' but allows you to specify a prefix (e.g. a namespace) that is included in the leaf hash computation. The prefix can be used to create
distinct namespaces within the same tree, ensuring that elements with the same content but different prefixes produce different hashes.
-}
addWithPrefix ::
  (HashAlgorithm a, ByteArrayAccess b1, ByteArrayAccess b2) =>
  -- | The current tree construction state
  MerkleTreeState a ->
  -- | A prefix (e.g. namespace) (must implement 'ByteArrayAccess')
  b1 ->
  -- | The element to add (must implement 'ByteArrayAccess')
  b2 ->
  -- | The updated tree construction state with the element incorporated
  MerkleTreeState a
addWithPrefix (MerkleTreeState state) prefix bytes =
  join (MerkleTreeState (MerkleTreeStateNode{cLevel = 0, cHash = leafHash prefix bytes} : state))

{- | Combine adjacent subtrees of the same level in the construction state.

This function implements the core of the incremental algorithm. It examines
the construction state and combines any two adjacent subtrees that are at
the same level.

The process:

1. Check if the first two subtrees have the same level
2. If yes, combine them into a new subtree at level + 1
3. Recursively apply the process to handle cascading combinations
4. If no combination is possible, return the state unchanged

The combination uses @nodeHash hash2 hash1@, which means the second subtree
(older in insertion order) becomes the left child, and the first subtree
(newer) becomes the right child. This maintains the correct tree structure
for the incremental algorithm.

Example state transitions:
@
[Level0(A)]                         -> [Level0(A)]                        (no change)
[Level0(B), Level0(A)]              -> [Level1(nodeHash AB)]              (combine)
[Level0(C), Level1(AB)]             -> [Level0(C), Level1(AB)]            (no change)
[Level0(D), Level0(C), Level1(AB)]  -> [Level1(nodeHash C D), Level1(AB)] (combine)   -> [Level2(nodeHash AB CD)] (combine)
@
-}
join :: (HashAlgorithm a) => MerkleTreeState a -> MerkleTreeState a
join (MerkleTreeState ((MerkleTreeStateNode level1 hash1) : (MerkleTreeStateNode level2 hash2) : xs))
  | level1 == level2 = join (MerkleTreeState (MerkleTreeStateNode{cLevel = level1 + 1, cHash = nodeHash hash2 hash1} : xs))
join state = state

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
