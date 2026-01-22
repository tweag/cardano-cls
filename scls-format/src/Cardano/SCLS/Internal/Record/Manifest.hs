{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Manifest record for file integrity and summary information.
-}
module Cardano.SCLS.Internal.Record.Manifest (
  Manifest (..),
  ManifestSummary (..),
  NamespaceInfo (..),
) where

import Data.ByteString qualified as BS
import Data.Function (fix)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.MemPack (MemPack (..), packByteStringM, unpackByteStringM)
import Data.MemPack.ByteOrdered (packWord32beM, packWord64beM, unpackBigEndianM)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Word

import Cardano.SCLS.Internal.Frame (frameHeaderSize)
import Cardano.SCLS.Internal.Hash
import Cardano.SCLS.Internal.Record.Internal.Class
import Cardano.Types.Namespace (Namespace)
import Cardano.Types.Namespace qualified as Namespace
import Cardano.Types.SlotNo

-- | Manifest summary information
data ManifestSummary = ManifestSummary
  { createdAt :: Text
  -- ^ ISO 8601 date string
  , tool :: Text
  -- ^ tool info
  , comment :: Maybe Text
  -- ^ optional comment
  }
  deriving (Show)

instance MemPack ManifestSummary where
  packedByteCount ManifestSummary{..} =
    let createdAtBytes = T.encodeUtf8 createdAt
        toolBytes = T.encodeUtf8 tool
        createdAtLength = BS.length createdAtBytes
        toolBytesLength = BS.length toolBytes
        cBytes = maybe BS.empty T.encodeUtf8 comment
        cBytesLength = BS.length cBytes
     in 4 + createdAtLength + 4 + toolBytesLength + 4 + cBytesLength

  packM ManifestSummary{..} = do
    let createdAtBytes = T.encodeUtf8 createdAt
        toolBytes = T.encodeUtf8 tool
        createdAtLength = BS.length createdAtBytes
        toolBytesLength = BS.length toolBytes
    packWord32beM (fromIntegral createdAtLength)
    packByteStringM createdAtBytes
    packWord32beM (fromIntegral toolBytesLength)
    packByteStringM toolBytes
    let cBytes = maybe BS.empty T.encodeUtf8 comment
        cBytesLength = BS.length cBytes
    packWord32beM (fromIntegral cBytesLength)
    packByteStringM cBytes

  unpackM = do
    createdAtLen :: Word32 <- unpackBigEndianM
    createdAt <- T.decodeUtf8 <$> unpackByteStringM (fromIntegral createdAtLen)
    toolLen :: Word32 <- unpackBigEndianM
    tool <- T.decodeUtf8 <$> unpackByteStringM (fromIntegral toolLen)
    comment <-
      do
        comment_len :: Word32 <- unpackBigEndianM
        if comment_len == 0
          then return Nothing
          else do
            c <- T.decodeUtf8 <$> unpackByteStringM (fromIntegral comment_len)
            pure (Just c)
    pure ManifestSummary{..}

data NamespaceInfo = NamespaceInfo
  { namespaceEntries :: {-# UNPACK #-} !Word64
  -- ^ number of entries
  , namespaceChunks :: {-# UNPACK #-} !Word64
  -- ^ number of chunks
  , namespaceHash :: Digest
  -- ^ multihash of root entry
  }
  deriving (Show, Eq)

-- | Manifest record
data Manifest = Manifest
  { slotNo :: SlotNo
  -- ^ slot number of the manifest
  , totalEntries :: Word64
  -- ^ number of entries
  , totalChunks :: Word64
  -- ^ number of chunks
  , rootHash :: Digest
  -- ^ multihash of root entry
  , nsInfo :: Map Namespace NamespaceInfo
  -- ^ map from namespace to its entries, chunks and multihash
  , prevManifestOffset :: Word64
  -- ^ offset of previous manifest
  , summary :: ManifestSummary
  -- ^ summary information
  }
  deriving (Show)

instance IsFrameRecord 0x01 Manifest where
  frameRecordSize Manifest{..} =
    packedByteCount totalEntries
      + packedByteCount slotNo
      + packedByteCount totalChunks
      + packedByteCount summary
      + (sum $ map nsInfoSize $ Map.keys nsInfo)
      + 4
      + packedByteCount prevManifestOffset
      + hashDigestSize
      + 4
   where
    nsInfoSize ns =
      let nsBytes = Namespace.asBytes ns
          bytesLength = BS.length nsBytes
       in 4 + 8 + 8 + bytesLength + hashDigestSize

  encodeRecordContents Manifest{..} = do
    packM slotNo
    packWord64beM totalEntries
    packWord64beM totalChunks
    packM summary
    (sum -> nsSize) <- traverse packNsInfo (Map.toList nsInfo)
    packWord32beM 0
    packWord64beM prevManifestOffset
    packM rootHash
    packWord32beM (fromIntegral $ 8 + 8 + packedByteCount summary + nsSize + 4 + 8 + hashDigestSize + frameHeaderSize)
   where
    packNsInfo (ns, h) = do
      let nsBytes = Namespace.asBytes ns
          bytesLength = BS.length nsBytes
      packWord32beM (fromIntegral bytesLength)
      packWord64beM (namespaceEntries h)
      packWord64beM (namespaceChunks h)
      packByteStringM nsBytes
      packM (namespaceHash h)
      return (4 + 8 + 8 + bytesLength + hashDigestSize)

  decodeRecordContents _size = do
    slotNo <- unpackM
    totalEntries <- unpackBigEndianM
    totalChunks <- unpackBigEndianM
    summary <- unpackM
    (Map.fromList -> nsInfo) <-
      flip fix [] $ \next current -> do
        getNsRoot >>= \case
          Nothing -> pure current
          Just n -> next (n : current)
    prevManifestOffset <- unpackBigEndianM
    rootHash <- unpackM
    pure Manifest{..}
   where
    getNsRoot = do
      nsLen :: Word32 <- unpackBigEndianM
      if nsLen == 0
        then return Nothing
        else do
          namespaceEntries <- unpackBigEndianM
          namespaceChunks <- unpackBigEndianM
          ns <- T.decodeUtf8 <$> unpackByteStringM (fromIntegral nsLen)
          namespaceHash <- unpackM
          pure (Just (Namespace.fromText ns, NamespaceInfo{..}))
