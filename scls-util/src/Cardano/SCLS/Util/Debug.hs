{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.SCLS.Util.Debug where

import Cardano.SCLS.CDDL

import Cardano.SCLS.Internal.Reader
import Cardano.SCLS.Internal.Serializer.Dump.Plan (addChunks, defaultSerializationPlan)
import Cardano.SCLS.Internal.Serializer.External.Impl qualified as External (serialize)
import Cardano.Types.Namespace qualified as Namespace
import Cardano.Types.Network (NetworkId (..))
import Cardano.Types.SlotNo (SlotNo (..))
import Codec.CBOR.Cuddle.CBOR.Gen (generateCBORTerm')
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot)
import Codec.CBOR.Cuddle.CDDL.Resolve (
  MonoReferenced,
  asMap,
  buildMonoCTree,
  buildRefCTree,
  buildResolvedCTree,
 )
import Codec.CBOR.Cuddle.Huddle
import Codec.CBOR.Cuddle.IndexMappable (mapCDDLDropExt)
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as B8
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.MemPack.Extra
import Data.Text qualified as T
import GHC.TypeNats (KnownNat)
import Streaming.Prelude qualified as S
import System.Random.Stateful (applyAtomicGen, globalStdGen, uniformByteStringM)

import Cardano.SCLS.CBOR.Canonical.Encoder (canonicalizeTerm)
import Cardano.SCLS.Internal.Entry.CBOREntry (GenericCBOREntry (GenericCBOREntry), SomeCBOREntry (SomeCBOREntry))
import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry (..))
import Cardano.SCLS.NamespaceCodec (NamespaceKeySize, namespaceKeySize)
import Cardano.SCLS.NamespaceSymbol (KnownSpec (namespaceSpec), SomeNamespaceSymbol (SomeNamespaceSymbol))
import Cardano.SCLS.Util.Result
import Cardano.Types.Namespace (Namespace)
import Control.Monad.Trans.Resource (runResourceT)

-- | Generate a scls file with random data for debugging purposes.
generateDebugFile :: FilePath -> [(Namespace, Maybe Int)] -> IO Result
generateDebugFile outputFile namespaceEntries = do
  _ <-
    runResourceT $
      External.serialize
        outputFile
        Mainnet
        (SlotNo 1)
        ( defaultSerializationPlan
            & addChunks do
              S.each
                ( namespaceEntries <&> \(namespace, mCount) -> do
                    let namespaceSymbol = namespaceSymbolFromText (Namespace.asText namespace)
                    case namespaceSymbol of
                      Nothing -> error $ "Unknown namespace: " ++ Namespace.asString namespace
                      Just (SomeNamespaceSymbol p) -> do
                        case buildMonoCTree =<< buildResolvedCTree (buildRefCTree $ asMap $ mapCDDLDropExt $ toCDDL (namespaceSpec p)) of
                          Left err -> error $ "Failed to parse cuddle specification: " ++ show err
                          Right mt ->
                            ( namespace
                                S.:> (generateNamespaceEntries p (fromMaybe 16 mCount) mt & S.map SomeCBOREntry)
                            )
                )
        )
  pure Ok

generateNamespaceEntries :: (KnownNat (NamespaceKeySize ns), MonadIO m, MonadFail m) => proxy ns -> Int -> CTreeRoot MonoReferenced -> S.Stream (S.Of (GenericCBOREntry (NamespaceKeySize ns))) m ()
generateNamespaceEntries (p :: proxy ns) count spec = replicateM_ count do
  let size = namespaceKeySize @ns
  keyIn <- liftIO $ uniformByteStringM (fromIntegral size) globalStdGen
  term <- liftIO $ applyAtomicGen (generateCBORTerm' spec (Name (T.pack "record_entry"))) globalStdGen
  Right canonicalTerm <- pure $ canonicalizeTerm p term
  S.yield $ GenericCBOREntry $ ChunkEntry (ByteStringSized @(NamespaceKeySize ns) keyIn) (mkCBORTerm canonicalTerm)

printHexEntries :: FilePath -> T.Text -> Int -> IO Result
printHexEntries filePath ns_name@(Namespace.fromText -> ns) entryNo = do
  case namespaceSymbolFromText ns_name of
    Nothing -> do
      putStrLn "Can't decode namespace, I don't know its key size"
      pure OtherError
    Just (SomeNamespaceSymbol (_ :: proxy ns)) -> do
      withNamespacedData @(ChunkEntry (ByteStringSized (NamespaceKeySize ns)) RawBytes) filePath ns $ \stream -> do
        stream
          & S.drop entryNo
          & S.take 1
          & S.mapM_ \ChunkEntry{chunkEntryKey = ByteStringSized k, chunkEntryValue = RawBytes b} -> do
            B8.putStrLn $ "Key: " <> Base16.encode k
            putStrLn "Data (hex):"
            B8.putStrLn $ Base16.encode b
      pure Ok
