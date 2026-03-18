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
import Cardano.Types.SlotNo (SlotNo (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot)
import Codec.CBOR.Cuddle.CDDL.Resolve (
  MonoReferenced,
  asMap,
  buildMonoCTree,
  buildRefCTree,
  buildResolvedCTree,
 )
import Codec.CBOR.Cuddle.Huddle
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..), mapCDDLDropExt)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits (shiftL, shiftR, (.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as B8
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.MemPack.Extra
import Data.Text qualified as T
import GHC.TypeNats (KnownNat)
import Streaming.Prelude qualified as S
import System.Random.Stateful (globalStdGen, uniformByteStringM)
import Test.AntiGen (runAntiGen)
import Test.QuickCheck (generate)

import Cardano.SCLS.CBOR.Canonical.Encoder (canonicalizeTerm)
import Cardano.SCLS.Internal.Entry.CBOREntry (GenericCBOREntry (GenericCBOREntry), SomeCBOREntry (SomeCBOREntry))
import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry (..))
import Cardano.SCLS.NamespaceCodec (NamespaceKeySize, namespaceKeySize)
import Cardano.SCLS.NamespaceSymbol (KnownSpec (namespaceSpec), SomeNamespaceSymbol (SomeNamespaceSymbol))
import Cardano.SCLS.Util.Result
import Cardano.Types.Namespace (Namespace)
import Codec.CBOR.Cuddle.CBOR.Gen (generateFromName)
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Control.Monad.Trans.Resource (runResourceT)

-- | Generate a scls file with random data for debugging purposes.
generateDebugFile :: (MonadIO m) => FilePath -> Bool -> [(Namespace, Maybe Int)] -> m Result
generateDebugFile outputFile useRandomKeys namespaceEntries = liftIO do
  _ <-
    runResourceT $
      External.serialize
        outputFile
        (SlotNo 1)
        knownNamespaceKeySizes
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
                                S.:> (generateNamespaceEntries p (fromMaybe 16 mCount) useRandomKeys mt & S.map SomeCBOREntry)
                            )
                )
        )
  pure Ok

-- | Enumerate all byte strings of a given size in lexicographic order.
enumerateKeys :: (Monad m) => Int -> S.Stream (S.Of BS.ByteString) m ()
enumerateKeys size = S.unfoldr go 0
  where
    maxKey = 1 `shiftL` (8 * size) :: Integer
    go i
      | i >= maxKey = pure (Left ())
      | otherwise = pure (Right (integerToByteString size i, i + 1))

-- | Convert a non-negative integer to a big-endian byte string of exactly @n@ bytes.
integerToByteString :: Int -> Integer -> BS.ByteString
integerToByteString n i = BS.pack [fromIntegral ((i `shiftR` (8 * k)) .&. 0xFF) | k <- [n - 1, n - 2 .. 0]]

generateNamespaceEntries :: (KnownNat (NamespaceKeySize ns), MonadIO m, MonadFail m) => proxy ns -> Int -> Bool -> CTreeRoot MonoReferenced -> S.Stream (S.Of (GenericCBOREntry (NamespaceKeySize ns))) m ()
generateNamespaceEntries (p :: proxy ns) count useRandomKeys spec =
  keyStream
    & S.take count
    & S.mapM \keyIn -> do
        term <- liftIO . generate . runAntiGen $ generateFromName (mapIndex spec) (Name (T.pack "record_entry"))
        Right canonicalTerm <- pure $ canonicalizeTerm p term
        pure $ GenericCBOREntry $ ChunkEntry (ByteStringSized @(NamespaceKeySize ns) keyIn) (mkCBORTerm canonicalTerm)
  where
    size = namespaceKeySize @ns
    keyStream
      | useRandomKeys =
          ( forever $ do
              keyIn <- liftIO $ uniformByteStringM (fromIntegral size) globalStdGen
              S.yield keyIn
          )
            & S.nubOrdOn id
      | otherwise = enumerateKeys size

printHexEntries :: (MonadIO m) => FilePath -> T.Text -> Int -> m Result
printHexEntries filePath ns_name@(Namespace.fromText -> ns) entryNo = liftIO do
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
