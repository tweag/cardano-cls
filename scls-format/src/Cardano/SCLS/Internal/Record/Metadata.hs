{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Record to hold metadata information.
-}
module Cardano.SCLS.Internal.Record.Metadata (
  Metadata (..),
  MetadataEntry (..),
  mkMetadata,
  entryDigest,
) where

import Cardano.SCLS.Internal.Hash (Digest (Digest), hashDigestSize)
import Cardano.SCLS.Internal.Record.Internal.Class

import Control.Monad.Fix (fix)
import Control.Monad.Trans.Fail (errorFail)
import Crypto.Hash (hashFinalize, hashUpdate)
import Crypto.Hash.MerkleTree.Incremental.Internal (leafHashInit)
import Data.ByteArray (ByteArrayAccess)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.MemPack
import Data.MemPack.ByteOrdered (packWord32beM, packWord64beM, unpackBigEndianM)
import Data.MemPack.Extra
import Data.Word (Word32, Word64)

data MetadataEntry = MetadataEntry
  { subject :: BS.ByteString -- Should be a URI
  , value :: BS.ByteString -- CBOR encoded value
  }
  deriving (Show, Eq, Ord)

instance MemPack MetadataEntry where
  packedByteCount MetadataEntry{..} =
    4 + 4 + BS.length subject + BS.length value

  packM MetadataEntry{..} = do
    let subjectLen = BS.length subject
        valueLen = BS.length value

    packWord32beM (fromIntegral subjectLen)
    packByteStringM subject
    packWord32beM (fromIntegral valueLen)
    packByteStringM value

  unpackM = do
    subjectLen :: Word32 <- unpackBigEndianM
    subject <- unpackByteStringM (fromIntegral subjectLen)
    valueLen :: Word32 <- unpackBigEndianM
    value <- unpackByteStringM (fromIntegral valueLen)
    return MetadataEntry{..}

instance MemPackHeaderOffset MetadataEntry where
  headerSizeOffset = 8

data MetadataFooter = MetadataFooter
  { totalEntries :: !Word64
  , entriesHash :: !Digest
  }
  deriving (Show, Eq)

instance MemPack MetadataFooter where
  packedByteCount _ = 8 + hashDigestSize

  packM MetadataFooter{..} = do
    packWord64beM totalEntries
    packM entriesHash

  unpackM = do
    totalEntries <- unpackBigEndianM
    entriesHash <- unpackM
    pure MetadataFooter{..}

data Metadata = Metadata
  { metadataEntries :: [MetadataEntry]
  , metadataFooter :: MetadataFooter
  }
  deriving (Show, Eq)

instance IsFrameRecord 0x31 Metadata where
  frameRecordSize Metadata{..} =
    sum (map packedByteCount metadataEntries) + packedByteCount metadataFooter

  encodeRecordContents Metadata{..} = do
    for_ metadataEntries packM
    packM metadataFooter

  decodeRecordContents size = do
    let entriesSize = fromIntegral size - 1 - 8 - hashDigestSize
    entriesBytes <- unpackByteStringM entriesSize
    let metadataEntries =
          fix
            ( \rec bs ->
                if BS.null bs
                  then []
                  else
                    let (e, n) = errorFail $ unpackLeftOver bs
                     in e : rec (BS.drop n bs)
            )
            entriesBytes

    metadataFooter <- unpackM
    pure Metadata{..}

mkMetadata :: BS.ByteString -> Word64 -> Digest -> Metadata
mkMetadata metadataBytes totalEntries entriesHash = do
  let
    metadataFooter = MetadataFooter{..}
    metadataEntries =
      fix
        ( \rec bs ->
            if BS.null bs
              then []
              else
                let (Entry e, n) = errorFail $ unpackLeftOver bs
                 in e : rec (BS.drop n bs)
        )
        metadataBytes

  Metadata{..}

{- | Compute the digest for a metadata entry given its raw data.
The digest is computed as H(0x01 || entry_bytes), where:
- 0x01 is a prefix byte to distinguish entry hashes from other types of hashes
- entry_bytes is the raw bytes of the metadata entry (including both subject and value)
-}
entryDigest :: (ByteArrayAccess ba) => ba -> Digest
entryDigest = Digest . hashFinalize . hashUpdate leafHashInit
