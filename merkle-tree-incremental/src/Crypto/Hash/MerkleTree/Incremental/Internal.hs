{- |
Module      : Crypto.Hash.MerkleTree.Incremental.Internal
Description : Incremental construction of Merkle Trees
Copyright   : (c) 2025 Modus Create, Inc.
License     : Apache-2.0
Maintainer  : cardano-cls@tweag.io
Stability   : experimental
-}
module Crypto.Hash.MerkleTree.Incremental.Internal
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
2. Adding a prefix byte 0 to distinguish internal nodes from leaves
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

-- | Add a precomputed leaf hash to the incremental Merkle tree construction.
addLeafHash ::
  (HashAlgorithm a) =>
  -- | The current tree construction state
  MerkleTreeState a ->
  -- | The precomputed leaf hash for the new element
  MerkleHash a ->
  -- | The updated tree construction state with the element incorporated
  MerkleTreeState a
addLeafHash (MerkleTreeState state) cHash =
  MerkleTreeState (MerkleTreeStateNode{cLevel = 0, cHash} : state)

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
