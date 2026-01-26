{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.SCLS.Internal.Serializer.Reference.Impl (
  serialize,
  InputChunk,
) where

import Cardano.SCLS.Internal.Serializer.Dump qualified as Dump
import Cardano.SCLS.Internal.Serializer.Dump.Plan (ChunkStream, InputChunk, SerializationPlan)
import Cardano.SCLS.Internal.Serializer.HasKey (HasKey (getKey))
import Cardano.Types.Namespace (Namespace (..))
import Cardano.Types.SlotNo
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.ST (runST)
import Control.Monad.Trans.Resource (ResIO, runResourceT)
import Data.Function (on)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.MemPack
import Data.MemPack.Extra
import Data.Typeable (Typeable)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Tim qualified as Tim
import Streaming.Prelude qualified as S
import VectorBuilder.Builder qualified as Builder
import VectorBuilder.MVector qualified as Builder

-- | Reference serialization interface. Performs all operations in memory
serialize ::
  (MemPack a, Typeable a, HasKey a, MemPackHeaderOffset a) =>
  -- | path to resulting file
  FilePath ->
  -- | Slot of the current transaction
  SlotNo ->
  -- | Key sizes for namespaces
  Map String Int ->
  -- | Serialization plan to use
  SerializationPlan a ResIO ->
  ResIO (Either [Namespace] ())
serialize resultFilePath slotNo namespaceKeySizes plan =
  Dump.serialize resultFilePath slotNo namespaceKeySizes plan $
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
