{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Instance of the state machine that fills the current metadata chunk in memory.
module Cardano.SCLS.Internal.Serializer.MetadataBuilder.InMemory (
  mkMachine,
  B.Command (..),
  BuilderMachine,
  MetadataItem (..),
  B.interpretCommand,
) where

import Cardano.SCLS.Internal.Hash (Digest)
import Cardano.SCLS.Internal.Record.Metadata (entryDigest)
import Cardano.SCLS.Internal.Serializer.Builder.InMemory qualified as B

import Data.Primitive.ByteArray

data MetadataItem = MetadataItem
  { metadataItemData :: ByteArray
  , metadataItemEntriesCount :: Int
  , metadataItemHash :: Digest
  }

instance B.BuilderItem MetadataItem where
  type Parameters MetadataItem = ()
  bEncodeEntry _ = id
  bItemData = metadataItemData
  bItemEntriesCount = metadataItemEntriesCount
  bMkItem _ = MetadataItem
  bEntryDigest _ = entryDigest

type BuilderMachine = (B.BuilderMachine MetadataItem)

mkMachine :: Int -> IO (B.BuilderMachine MetadataItem)
mkMachine = flip B.mkMachine ()
