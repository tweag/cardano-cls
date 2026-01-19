{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.SCLS.Internal.Reader (
  withHeader,
  withNamespacedData,
  withLatestManifestFrame,
  withRecordData,
  extractRootHash,
  extractNamespaceList,
  extractNamespaceHash,
  withKnownNamespacedData,
  withNamespacedDataHandle,

  -- * Stream API
  -- $stream
  namespacedData,
  knownNamespacedData,

  -- * Low-level functions
  streamChunkEntries,
) where

import Cardano.SCLS.Internal.Frame
import Cardano.SCLS.Internal.Hash (Digest)
import Cardano.SCLS.Internal.Record.Chunk
import Cardano.SCLS.Internal.Record.Hdr
import Cardano.SCLS.Internal.Record.Manifest
import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Control.Monad.Trans.Fail
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Function (fix, (&))
import Data.Map.Strict qualified as Map
import Data.MemPack (MemPack, unpack, unpackLeftOver)
import Data.MemPack.ByteOrdered (BigEndian (BigEndian))
import Data.MemPack.Extra
import Data.Typeable
import System.IO
import System.IO qualified as IO

import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry, decodeChunkEntry)
import Cardano.SCLS.Internal.Record.Internal.Class (IsFrameRecord)
import Cardano.SCLS.NamespaceCodec (KnownNamespace (NamespaceEntry, NamespaceKey), NamespaceKeySize)
import Cardano.Types.Namespace (Namespace)
import Cardano.Types.Namespace qualified as Namespace
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (fromJust)
import GHC.TypeLits (KnownSymbol)
import Streaming qualified as S
import Streaming.Prelude qualified as S

{- | This function provides a stream of the Chunk entries
stored in the data field of the 'Chunk'.
-}
streamChunkEntries :: (Typeable u, MemPack u, Monad m) => BS.ByteString -> S.Stream (S.Of u) m ()
streamChunkEntries = go
 where
  go !bs
    | BS.null bs = pure ()
    | otherwise = do
        let (Entry userData, off) = errorFail $ unpackLeftOver bs
        S.yield userData
        go (BS.drop off bs)

-- | Stream all data chunks for the given namespace from the given file.
withNamespacedData :: (MemPack u, Typeable u) => FilePath -> Namespace -> (S.Stream (S.Of u) IO () -> IO a) -> IO a
withNamespacedData filePath namespace f =
  IO.withBinaryFile filePath ReadMode (\handle -> withNamespacedDataHandle handle namespace f)

-- | Stream all data chunks for the given namespace using the provided handle.
withNamespacedDataHandle :: (MemPack u, Typeable u, MonadIO m) => Handle -> Namespace -> (S.Stream (S.Of u) m () -> m a) -> m a
withNamespacedDataHandle handle namespace f =
  f (namespacedData handle namespace)

withKnownNamespacedData :: forall ns r. (KnownSymbol ns, KnownNamespace ns) => FilePath -> Proxy ns -> (S.Stream (S.Of (ChunkEntry (NamespaceKey ns) (NamespaceEntry ns))) IO () -> IO r) -> IO r
withKnownNamespacedData filePath p f =
  withNamespacedData
    @(ChunkEntry (ByteStringSized (NamespaceKeySize ns)) RawBytes)
    filePath
    namespace
    $ f . S.map (fromJust . decodeChunkEntry p)
 where
  namespace = Namespace.fromSymbol p

{- | Stream all records for a particular record type in the file.
No optimized access, just a full scan of the file.
-}
withRecordData :: forall t b r. (IsFrameRecord t b) => FilePath -> (S.Stream (S.Of b) IO () -> IO r) -> IO r
withRecordData filePath f =
  IO.withBinaryFile filePath ReadMode \handle -> f (stream handle)
 where
  stream handle = do
    flip fix headerOffset \go record -> do
      next <- S.liftIO do
        fetchNextFrame handle record
      for_ next \next_record -> do
        dataRecord <- S.liftIO do
          fetchOffsetFrame handle next_record
        for_ (decodeFrame dataRecord) \metadataRecord -> do
          S.yield (frameViewContent metadataRecord)
        go next_record

{- | Extract the root hash from the file at the given offset.

This function does not provide additional checks.
-}
extractRootHash :: FilePath -> IO Digest
extractRootHash = withLatestManifestFrame \Manifest{..} ->
  pure rootHash

extractNamespaceHash :: Namespace -> FilePath -> IO (Maybe Digest)
extractNamespaceHash ns = withLatestManifestFrame \Manifest{..} ->
  pure (namespaceHash <$> Map.lookup ns nsInfo)

data NotSCLSFile = NotSCLSFile
  deriving (Show, Exception)

withLatestManifestFrame :: (Manifest -> IO r) -> FilePath -> IO r
withLatestManifestFrame f filePath = do
  IO.withBinaryFile filePath ReadMode \handle -> do
    h <- hFileSize handle
    hSeek handle AbsoluteSeek (h - 4)
    bs <- BS.hGet handle 4
    offset <- case unpack bs of
      Right (BigEndian d) -> return d
      Left _ -> throwIO NotSCLSFile
    frameData <- fetchOffsetFrame handle (FrameView (offset) (mkRecordType @Manifest) (fromIntegral h - fromIntegral offset))
    case decodeFrame frameData of
      Right FrameView{frameViewContent = m@Manifest{}} -> f m
      Left _ -> throwIO NotSCLSFile

extractNamespaceList :: FilePath -> IO [Namespace]
extractNamespaceList = withLatestManifestFrame \Manifest{..} ->
  pure (Map.keys nsInfo)

withHeader :: FilePath -> (Hdr -> IO r) -> IO r
withHeader filePath f = do
  IO.withBinaryFile filePath ReadMode \handle -> do
    hSeek handle AbsoluteSeek 0
    bs <- BS.hGet handle 4
    offset <- case unpack bs of
      Right (BigEndian d) -> return d
      Left _ -> throwIO NotSCLSFile
    frameData <- fetchOffsetFrame handle (FrameView offset (mkRecordType @Hdr) 4)
    case decodeFrame frameData of
      Right FrameView{frameViewContent = hdr@Hdr{}} -> f hdr
      Left _ -> error "Failed to decode header"

{- $stream
Stream API is used to provide an interface for the streaming framework.
In this case usage of the resources is responsibility of the caller, however
such interface is more composable.
-}

{- | Create a stream of the entries from the given namespace.

Attention this method does work on the provided handle and it
will not be safe to use in case if other parts of the stream
use this handle as well.
-}
namespacedData :: (MemPack u, Typeable u, MonadIO m) => Handle -> Namespace -> S.Stream (S.Of u) m ()
namespacedData handle namespace = stream
 where
  stream = do
    S.liftIO $ hSeek handle AbsoluteSeek 0
    flip fix headerOffset \go record -> do
      next <- S.liftIO do
        fetchNextFrame handle record
      for_ next \next_record -> do
        dataRecord <- S.liftIO do
          fetchOffsetFrame handle next_record
        for_ (decodeFrame dataRecord) \chunkRecord -> do
          when (chunkNamespace (frameViewContent (chunkRecord)) == namespace) do
            streamChunkEntries (chunkData $ frameViewContent chunkRecord)
        go next_record

knownNamespacedData :: forall ns io. (KnownSymbol ns, KnownNamespace ns, MonadIO io) => Handle -> Proxy ns -> S.Stream (S.Of (ChunkEntry (NamespaceKey ns) (NamespaceEntry ns))) io ()
knownNamespacedData handle p =
  namespacedData
    @(ChunkEntry (ByteStringSized (NamespaceKeySize ns)) RawBytes)
    handle
    namespace
    & S.map (fromJust . decodeChunkEntry p)
 where
  namespace = Namespace.fromSymbol p
