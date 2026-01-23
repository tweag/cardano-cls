{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module QuerySpec (queryTests) where

import Cardano.SCLS.Entry.IsKey (IsKey (..))
import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry (..))
import Cardano.SCLS.Internal.Serializer.Dump.Plan (addChunks, defaultSerializationPlan)
import Cardano.SCLS.Internal.Serializer.Reference.Impl qualified as Reference (serialize)
import Cardano.SCLS.Query (queryEntry)
import Cardano.Types.Namespace (Namespace, asString)
import Cardano.Types.SlotNo (SlotNo (..))
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Map qualified as Map
import Data.MemPack (packM, unpackM)
import Data.MemPack.ByteOrdered (BigEndian (..))
import Data.MemPack.Extra (RawBytes (..))
import Data.String (fromString)
import Data.Word (Word32, Word64)
import Streaming.Prelude qualified as S
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

data TestKey = TestKey
  { tkTxId :: Word64
  , tkTxIx :: Word32
  }
  deriving (Eq, Ord, Show)

instance IsKey TestKey where
  keySize = 12
  packKeyM (TestKey txId txIx) = do
    packM (BigEndian (fromIntegral txId :: Word64))
    packM (BigEndian (fromIntegral txIx :: Word32))
  unpackKeyM = do
    BigEndian txId <- unpackM
    BigEndian txIx <- unpackM
    return (TestKey txId txIx)

type TestEntry = ChunkEntry TestKey RawBytes

mkTestEntry :: Word64 -> Word32 -> BS.ByteString -> TestEntry
mkTestEntry txId txIx value = ChunkEntry (TestKey txId txIx) (RawBytes value)

serializeTestData ::
  FilePath ->
  [(Namespace, [TestEntry])] ->
  IO ()
serializeTestData fileName namespaceData = do
  let plan =
        defaultSerializationPlan
          & addChunks
            ( S.each
                [ ns S.:> S.each entries
                | (ns, entries) <- namespaceData
                ]
            )
  runResourceT $
    Reference.serialize
      fileName
      (SlotNo 1)
      (Map.fromList [(asString ns, keySize @TestKey) | (ns, _) <- namespaceData])
      plan
  return ()

queryTests :: Spec
queryTests =
  describe "Query.queryEntry" $ do
    basicTests
    edgeCaseTests
    multiNamespaceTests

basicTests :: Spec
basicTests =
  describe "Basic functionality" $ do
    it "finds an existing entry in a namespace" $
      withSystemTempDirectory "query-test-XXXXXX" $ \tmpDir -> do
        let fileName = tmpDir </> "test.scls"
            ns = fromString "test-ns"
            key = TestKey 100 1
            value = "test-value"
            entries = [mkTestEntry 100 1 value, mkTestEntry 200 2 "other-value"]

        serializeTestData fileName [(ns, entries)]

        queryEntry fileName ns key `shouldReturn` Just (mkTestEntry 100 1 value)

    it "returns Nothing when entry doesn't exist" $
      withSystemTempDirectory "query-test-XXXXXX" $ \tmpDir -> do
        let fileName = tmpDir </> "test.scls"
            ns = fromString "test-ns"
            searchKey = TestKey 999 99
            entries = [mkTestEntry 100 1 "value1", mkTestEntry 200 2 "value2"]

        serializeTestData fileName [(ns, entries)]

        queryEntry @TestEntry fileName ns searchKey `shouldReturn` Nothing

    it "finds entry among multiple entries" $
      withSystemTempDirectory "query-test-XXXXXX" $ \tmpDir -> do
        let fileName = tmpDir </> "test.scls"
            ns = fromString "test-ns"
            searchKey = TestKey 150 5
            value = "middle-value"
            entries =
              [ mkTestEntry 100 1 "first"
              , mkTestEntry 150 5 value
              , mkTestEntry 200 2 "last"
              ]

        serializeTestData fileName [(ns, entries)]

        queryEntry fileName ns searchKey `shouldReturn` Just (mkTestEntry 150 5 value)

edgeCaseTests :: Spec
edgeCaseTests =
  describe "Edge cases" $ do
    it "returns Nothing for empty namespace" $
      withSystemTempDirectory "query-test-XXXXXX" $ \tmpDir -> do
        let fileName = tmpDir </> "test.scls"
            ns = fromString "empty-ns"
            searchKey = TestKey 100 1

        serializeTestData fileName [(ns, [])]

        queryEntry @TestEntry fileName ns searchKey `shouldReturn` Nothing

    it "returns Nothing for non-existent namespace" $
      withSystemTempDirectory "query-test-XXXXXX" $ \tmpDir -> do
        let fileName = tmpDir </> "test.scls"
            existingNs = fromString "existing-ns"
            nonExistentNs = fromString "non-existent-ns"
            searchKey = TestKey 100 1
            entries = [mkTestEntry 100 1 "value"]

        serializeTestData fileName [(existingNs, entries)]

        queryEntry @TestEntry fileName nonExistentNs searchKey `shouldReturn` Nothing

    it "returns first match when there are duplicate keys" $
      withSystemTempDirectory "query-test-XXXXXX" $ \tmpDir -> do
        let fileName = tmpDir </> "test.scls"
            ns = fromString "test-ns"
            searchKey = TestKey 100 1
            firstValue = "first-occurrence"
            entries =
              [ mkTestEntry 100 1 firstValue
              , mkTestEntry 100 1 "second-occurrence"
              , mkTestEntry 100 1 "third-occurrence"
              ]

        serializeTestData fileName [(ns, entries)]

        queryEntry fileName ns searchKey `shouldReturn` Just (mkTestEntry 100 1 firstValue)

    it "finds single entry in namespace" $
      withSystemTempDirectory "query-test-XXXXXX" $ \tmpDir -> do
        let fileName = tmpDir </> "test.scls"
            ns = fromString "single-ns"
            searchKey = TestKey 42 7
            value = "only-value"
            entries = [mkTestEntry 42 7 value]

        serializeTestData fileName [(ns, entries)]

        queryEntry fileName ns searchKey `shouldReturn` Just (mkTestEntry 42 7 value)

multiNamespaceTests :: Spec
multiNamespaceTests =
  describe "Multiple namespaces" $ do
    it "finds entry in correct namespace when multiple namespaces exist" $
      withSystemTempDirectory "query-test-XXXXXX" $ \tmpDir -> do
        let fileName = tmpDir </> "test.scls"
            ns1 = fromString "namespace-1"
            ns2 = fromString "namespace-2"
            searchKey = TestKey 100 1
            value1 = "value-from-ns1"
            value2 = "value-from-ns2"
            entries1 = [mkTestEntry 100 1 value1, mkTestEntry 200 2 "other1"]
            entries2 = [mkTestEntry 100 1 value2, mkTestEntry 300 3 "other2"]

        serializeTestData fileName [(ns1, entries1), (ns2, entries2)]

        queryEntry fileName ns1 searchKey `shouldReturn` Just (mkTestEntry 100 1 value1)

        queryEntry fileName ns2 searchKey `shouldReturn` Just (mkTestEntry 100 1 value2)

    it "correctly isolates namespaces - entry exists in one but not another" $
      withSystemTempDirectory "query-test-XXXXXX" $ \tmpDir -> do
        let fileName = tmpDir </> "test.scls"
            ns1 = fromString "has-entry"
            ns2 = fromString "no-entry"
            searchKey = TestKey 100 1
            entries1 = [mkTestEntry 100 1 "exists-here"]
            entries2 = [mkTestEntry 200 2 "different-key"]

        serializeTestData fileName [(ns1, entries1), (ns2, entries2)]

        queryEntry fileName ns1 searchKey `shouldReturn` Just (mkTestEntry 100 1 "exists-here")

        queryEntry @TestEntry fileName ns2 searchKey `shouldReturn` Nothing

    it "handles many namespaces efficiently" $
      withSystemTempDirectory "query-test-XXXXXX" $ \tmpDir -> do
        let fileName = tmpDir </> "test.scls"
            namespaces =
              [ ( fromString ("ns-" ++ show i)
                , [mkTestEntry (fromIntegral i) 1 (BS.pack [fromIntegral i])]
                )
              | i <- [0 .. 9 :: Int]
              ]
            searchNs = fromString "ns-5"
            searchKey = TestKey 5 1

        serializeTestData fileName namespaces

        queryEntry fileName searchNs searchKey `shouldReturn` Just (mkTestEntry 5 1 (BS.pack [5]))
