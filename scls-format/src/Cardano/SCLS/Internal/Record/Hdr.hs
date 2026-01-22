{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.SCLS.Internal.Record.Hdr (
  Hdr (..),
  mkHdr,
) where

import Control.Monad.State (MonadState (put))
import Data.MemPack (MemPack (..))
import Foreign

import Cardano.SCLS.Internal.Record.Internal.Class
import Cardano.SCLS.Internal.Version (Version (..))

-- TODO: switch to non-pure interface instead

-- | Header record.
data Hdr = Hdr
  { magic :: Word64
  , version :: Version
  }
  deriving (Show, Eq)

-- deriving MemPack via (AsStorable Hdr)

instance IsFrameRecord 0 Hdr where
  frameRecordSize Hdr{..} =
    4
      + packedByteCount version

  encodeRecordContents Hdr{..} = do
    packM magic
    put 4
    packM version

  decodeRecordContents _size = do
    magic_pre :: Word64 <- unpackM
    let magic = magic_pre .&. 0xffffffff -- We are interested only in the first 4 bytes
    put 5
    version <- unpackM
    pure Hdr{..}

-- | Creates header record for the current version.
mkHdr :: Hdr
mkHdr =
  Hdr
    { magic = 1397506899 -- "SCLS"
    , version = V1
    }
