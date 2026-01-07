{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Implementation of the state machine that fills current chunk in memory.

It manages proper filling of the buffers and emitting the values when
the next item can't be added.

Current implementation expects the incoming values in already sorted order.

Implementation is done in the way so it would be possible to use it with
any existing stream and effect system as long as they could carry a state.
-}
module Cardano.SCLS.Internal.Serializer.Builder.InMemory (
  BuilderItem (..),
  mkMachine,
  Command (..),
  BuilderMachine (..),
) where

import Cardano.SCLS.Internal.Hash
import Control.Monad.Primitive
import Crypto.Hash (Blake2b_224 (Blake2b_224))
import Crypto.Hash.MerkleTree.Incremental qualified as MT
import Data.MemPack
import Data.MemPack.Extra

import Data.Kind (Type)
import Data.Primitive.ByteArray
import Data.Typeable
import Foreign.Ptr
import Unsafe.Coerce (unsafeCoerce)

-- | Typeclass for items that can be emitted by the builder state machine.
class BuilderItem item where
  -- | Type of parameters needed to encode entries and build the item
  type Parameters item :: Type

  -- | Get the data payload of the item
  bItemData :: item -> ByteArray

  -- | Get the number of entries contained in the item
  bItemEntriesCount :: item -> Int

  -- | Construct an item from build parameters, data and entry count
  bMkItem :: Parameters item -> ByteArray -> Int -> item

  -- | Encode an entry for the item using the provided parameters
  bEncodeEntry :: (MemPack a, Typeable a) => Parameters item -> a -> a

-- | Command for the state machine
data Command item type_ where
  -- | Append a new item to the buffer.
  Append :: (MemPack u, Typeable u, MemPackHeaderOffset u) => u -> Command item (BuilderMachine item, [item])
  {- | Finalize building of the buffer. Calling this command does not

    It's up to the implementation if the state machine can be used
    after interpreting this command.
  -}
  Finalize :: Command item (Digest, Maybe item)

{- | State machine for building items in memory.

Basically it's an interpreter for the 'Command' type that is implemented
the way that it can be inserted into the different streaming pipelines
or effect libraries
-}
newtype BuilderMachine item = BuilderMachine
  { interpretCommand :: forall result. Command item result -> IO result
  }

-- | Create an instance of the state machine.
mkMachine ::
  forall item.
  (BuilderItem item) =>
  -- | Buffer size in bytes
  Int ->
  Parameters item ->
  IO (BuilderMachine item)
mkMachine bufferSize params = do
  -- We perform copying when we emit data outside of the state machine.
  -- So this buffer is reused for all the items, as a result we copy
  --
  -- We allocate pinned memory because we pass it to the digest code
  -- without copying by passing raw pointer.
  storage <- newPinnedByteArray bufferSize

  -- Use fix? We love fixed point combinators do we not?
  let machine (!entriesCount :: Int) (!offset :: Int) !merkleTreeState =
        BuilderMachine
          { interpretCommand = \case
              Finalize -> do
                let final = Digest $ MT.merkleRootHash $ MT.finalize merkleTreeState
                if offset == 0 -- no new data, nothing to emit
                  then
                    pure (final, Nothing)
                  else do
                    frozenData <- freezeByteArrayPinned storage 0 offset
                    pure (final, Just (bMkItem params frozenData entriesCount))
              Append (input :: a) -> do
                let entry = Entry $ bEncodeEntry @item params input
                let l = packedByteCount entry
                if offset + l <= bufferSize -- if we fit the current buffer we just need to write data and continue
                  then do
                    (merkleTreeState', newOffset) <-
                      unsafeAppendEntryToBuffer merkleTreeState storage offset entry
                    pure (machine (entriesCount + 1) newOffset merkleTreeState', [])
                  else do
                    -- We have no space in the current buffer, so we need to emit it first
                    frozenBuffer <- freezeByteArrayPinned storage 0 offset
                    if l > bufferSize
                      then do
                        let !tmpBuffer = pack entry
                            !merkleTreeState' = MT.add merkleTreeState (uncheckedByteArrayEntryContents @a tmpBuffer)
                        return
                          ( machine 0 0 merkleTreeState'
                          , mkDataToEmit [(params, frozenBuffer, entriesCount), (params, tmpBuffer, 1)]
                          )
                      else do
                        (merkleTreeState', newOffset) <-
                          unsafeAppendEntryToBuffer merkleTreeState storage 0 entry
                        pure
                          ( machine 1 newOffset merkleTreeState'
                          , mkDataToEmit [(params, frozenBuffer, entriesCount)]
                          )
          }
  return $! machine 0 0 (MT.empty Blake2b_224)

{- | Freeze a bytearray to the pinned immutable bytearray by copying its contents.

It's safe to use the source bytearray after this operation.
-}
freezeByteArrayPinned :: (PrimMonad m) => MutableByteArray (PrimState m) -> Int -> Int -> m ByteArray
freezeByteArrayPinned !src !off !len = do
  dst <- newPinnedByteArray len
  copyMutableByteArray dst 0 src off len
  unsafeFreezeByteArray dst

unsafeAppendEntryToBuffer :: forall u. (MemPack u, Typeable u, MemPackHeaderOffset u) => MT.MerkleTreeState Blake2b_224 -> MutableByteArray (PrimState IO) -> Int -> Entry u -> IO (MT.MerkleTreeState Blake2b_224, Int)
unsafeAppendEntryToBuffer !merkleTreeState !storage !offset u = do
  newOffset <- unsafeAppendToBuffer storage offset u
  let l = newOffset - offset
  merkleTreeState' <- withMutableByteArrayContents storage $ \ptr -> do
    let csb = CStringLenBuffer (ptr `plusPtr` (offset + headerSizeOffset @u), l - headerSizeOffset @u)
    return $! MT.add merkleTreeState csb
  return (merkleTreeState', newOffset)

{- | Helper to get access to the entry contents.
This method should be used on the pinned 'ByteArray' only, but the function does
not enforce this.
-}
uncheckedByteArrayEntryContents :: forall a. (MemPackHeaderOffset a) => ByteArray -> CStringLenBuffer
uncheckedByteArrayEntryContents !buffer = CStringLenBuffer (byteArrayContents buffer `plusPtr` (headerSizeOffset @a), sizeofByteArray buffer - (headerSizeOffset @a))

{- | Unsafe helper that we need because MemPack interface only allows ST, and
no other PrimMonad.

There is unsafe prefix, because this function uses 'unsafeCoerce' internally,
but it ensures everything to make it safe to use.

This functions prepends the packed values with its lengths.
-}
unsafeAppendToBuffer :: (MemPack u) => MutableByteArray (PrimState IO) -> Int -> u -> IO Int
unsafeAppendToBuffer !storage !offset u = stToPrim $ do
  let uInST = unsafeCoerce storage
  (_, offset') <-
    runStateT (runPack (packM u) uInST) offset
  pure offset'

{- | Helper to create the list of items to emit from the list of
  (data, count) tuples.

  This function filters out items with 0 entries.
-}
mkDataToEmit :: (BuilderItem item) => [(Parameters item, ByteArray, Int)] -> [item]
mkDataToEmit = mkDataToEmit' []
 where
  mkDataToEmit' acc [] = reverse acc
  mkDataToEmit' acc ((_, _, 0) : xs) = mkDataToEmit' acc xs
  mkDataToEmit' acc ((params, u, count) : xs) =
    mkDataToEmit' (bMkItem params u count : acc) xs
