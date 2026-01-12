{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.SCLS.Internal.Serializer.Reference.Impl (
  serialize,
  InputChunk,
) where

import Cardano.SCLS.Internal.Record.Hdr
import Cardano.SCLS.Internal.Serializer.Dump (dumpToHandle)
import Cardano.SCLS.Internal.Serializer.Dump.Plan (ChunkStream, InputChunk, SerializationPlan, mkSortedSerializationPlan)
import Cardano.SCLS.Internal.Serializer.HasKey (HasKey (getKey))
import Cardano.Types.Namespace (Namespace (..))
import Cardano.Types.Network
import Cardano.Types.SlotNo
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.ST (runST)
import Control.Monad.Trans.Resource (ResIO, allocate, runResourceT)
import Data.Function (on)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.MemPack
import Data.MemPack.Extra
import Data.Typeable (Typeable)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Tim qualified as Tim
import Streaming.Prelude qualified as S
import System.IO (IOMode (WriteMode), hClose, openBinaryFile)
import VectorBuilder.Builder qualified as Builder
import VectorBuilder.MVector qualified as Builder

{- | Reference serialization interface. Performs all operations in memory

At this point it accepts values from one namespace only.
-}
serialize ::
  (MemPack a, Typeable a, HasKey a, MemPackHeaderOffset a) =>
  -- | path to resulting file
  FilePath ->
  -- | Network identifier
  NetworkId ->
  -- | Slot of the current transaction
  SlotNo ->
  SerializationPlan a ResIO ->
  ResIO ()
serialize resultFilePath network slotNo plan = do
  (_, handle) <- allocate (openBinaryFile resultFilePath WriteMode) hClose
  let hdr = mkHdr network slotNo
  dumpToHandle handle hdr $
    mkSortedSerializationPlan
      plan
      ( \s -> do
          !orderedStream <- liftIO $ runResourceT $ mkVectors s
          S.each [n S.:> S.each v | (n, v) <- Map.toList orderedStream]
      )
 where
  mkVectors :: (HasKey a, Monad m) => ChunkStream a m -> m (Map Namespace (V.Vector a))
  mkVectors = do
    S.foldM_
      do
        \m (ns S.:> vecStream) -> do
          v <- mkVector vecStream
          pure $! Map.insertWith (<>) ns v m
      do pure Map.empty
      do
        traverse \builder -> pure $ runST do
          mv <- Builder.build builder
          Tim.sortBy (compare `on` getKey) mv
          V.unsafeFreeze mv

  mkVector :: (Monad m) => S.Stream (S.Of a) m () -> m (Builder.Builder a)
  mkVector = S.fold_
    do \x e -> x <> Builder.singleton e
    do Builder.empty
    do id
