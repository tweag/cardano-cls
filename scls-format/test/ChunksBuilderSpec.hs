{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ChunksBuilderSpec (chunksBuilderTests) where

import Cardano.SCLS.Internal.Hash (Digest (..))

import Cardano.SCLS.Internal.Record.Chunk
import Cardano.SCLS.Internal.Serializer.ChunksBuilder.InMemory
import Cardano.Types.Namespace (Namespace, asBytes)
import Control.Monad
import Crypto.Hash (hash, hashFinalize, hashInit, hashUpdate)
import Crypto.Hash.MerkleTree.Incremental qualified as MT
import Crypto.Hash.MerkleTree.Incremental.Internal qualified as MT
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Maybe
import Data.MemPack (packByteString)
import Data.MemPack.Extra
import Data.Primitive.ByteArray
import Streaming.Prelude qualified as S
import Test.HUnit
import Test.Hspec
import Test.Hspec.Expectations.Contrib
import Test.Hspec.QuickCheck
import Test.QuickCheck
import TestEntry (TestEntry (TestEntry), TestUTxO (..), chunkEntryFromUTxO)

testNamespace :: Namespace
testNamespace = "test"

mkMachine' :: Int -> IO BuilderMachine
mkMachine' = flip mkMachine (testNamespace, ChunkFormatRaw)

chunksBuilderTests :: Spec
chunksBuilderTests =
  describe "ChunksBuilder.InMemory" $ do
    bufferBoundaryTests
    finalizationTests
    hashTests

bufferFittingChunks :: Gen (Int, [Int])
bufferFittingChunks = do
  chunkCount <- choose (1, 10)
  -- Buffer size between chunkCount*4 (minimum for empty chunks) and 200 bytes
  bufferSize <- choose (chunkCount * 4, 200)
  -- Each chunk size is chosen to ensure total fits in buffer
  let maxChunkSize = bufferSize `div` chunkCount - 4
  chunkSizes <- vectorOf chunkCount (choose (0, maxChunkSize))
  return (bufferSize, chunkSizes)

bufferFillingChunks :: Gen (Int, [Int])
bufferFillingChunks = do
  bufferSize <- choose (20, 200)
  chunkSizes <- fillChunkSizes [] bufferSize
  return (bufferSize, chunkSizes)
 where
  -- Generate chunks that will exactly fill the buffer
  fillChunkSizes acc 0 = return (reverse acc)
  fillChunkSizes acc remaining = do
    let maxChunkSize = remaining - 4
    if maxChunkSize < 8
      then return (maxChunkSize : acc) -- Last chunk to fill exactly
      else do
        chunkSize <- choose (0, maxChunkSize - 4)
        fillChunkSizes (chunkSize : acc) (remaining - (chunkSize + 4))

bufferFittingAndOversizedChunks :: Gen (Int, Int, Int)
bufferFittingAndOversizedChunks = do
  bufferLength <- choose (50, 200)
  smallDataLength <- choose (1, bufferLength - 4)
  largeDataLength <- choose (bufferLength - 3, bufferLength + 100)
  return (bufferLength, smallDataLength, largeDataLength)

foldAppendChunks :: BuilderMachine -> [RawBytes] -> IO (BuilderMachine, [ChunkItem])
foldAppendChunks machine =
  foldM
    ( \(machine', acc) chunkData -> do
        (machine'', chunks') <- interpretCommand machine' (Append chunkData)
        return (machine'', acc ++ chunks')
    )
    (machine, [])

bufferBoundaryTests :: Spec
bufferBoundaryTests =
  describe "Buffer Boundary Tests" $ do
    prop "should not emit chunks when data fits" $
      forAll bufferFittingChunks $ \(bufferLength, chunkLengths) -> do
        machine <- mkMachine' bufferLength
        let entries = map (RawBytes . flip BS.replicate 0x43) chunkLengths
        (machine', emittedChunks) <-
          foldAppendChunks machine entries
        annotate "no chunks should be emitted since all data fits" $ length emittedChunks `shouldBe` 0
        validateRootHash entries machine'
        validateChunksHashes entries emittedChunks

    prop "should not emit chunk when data exactly fills buffer, only after" $
      forAll bufferFillingChunks $ \(bufferLength, chunkLengths) ->
        do
          machine <- mkMachine' bufferLength
          let entries = map (RawBytes . flip BS.replicate 0x43) chunkLengths
          (machine', emittedChunks) <-
            foldAppendChunks machine entries
          annotate "after appending exact fit data should not emit" $ length emittedChunks `shouldBe` 0
          validateRootHash entries machine'
          validateChunksHashes entries emittedChunks
          let oneByteData = RawBytes (BS.singleton 0x45)
          (machine'', chunks') <- interpretCommand machine' (Append oneByteData)
          case chunks' of
            [chunk] -> do
              chunkItemEntriesCount chunk `shouldBe` length chunkLengths
              chunkItemFormat chunk `shouldBe` ChunkFormatRaw
              (sizeofByteArray $ chunkItemData chunk) `shouldBe` (sum chunkLengths + 4 * length chunkLengths)
            l -> length l `shouldBe` 1
          validateRootHash (entries ++ [oneByteData]) machine''
          validateChunksHashes (entries ++ [oneByteData]) chunks'

    describe "oversized append when buffer empty" $ do
      forM_ [0, 1, 4, 31, 128] $ \chunkDataLength -> do
        it ("should emit one chunk when buffer is empty and data is oversized (dataLen =" ++ show chunkDataLength) $ do
          let bufferLength = chunkDataLength + 3
          machine <- mkMachine' bufferLength
          let chunkData = RawBytes (BS.replicate chunkDataLength 0x46)
          (machine', chunks) <- interpretCommand machine (Append chunkData)
          case chunks of
            [oversizedChunk] -> do
              annotate "oversized chunk should have one entry" $ chunkItemEntriesCount oversizedChunk `shouldBe` 1
              annotate "oversized chunk size should match input size" $ (sizeofByteArray $ chunkItemData oversizedChunk) `shouldBe` chunkDataLength + 4
            l -> annotate "should emit one chunk" $ length l `shouldBe` 1
          validateRootHash [chunkData] machine'
          validateChunksHashes [chunkData] chunks

    prop "should emit oversized chunk and buffer when buffer is not empty" $
      forAll bufferFittingAndOversizedChunks $
        \(bufferLength, smallDataLength, largeDataLength) -> do
          machine <- mkMachine' bufferLength
          -- Add data that fits first
          let smallData = RawBytes (BS.replicate smallDataLength 0x47)
          (machine', chunks) <- interpretCommand machine (Append smallData)
          annotate "after adding small data should not emit" $
            length chunks `shouldBe` 0
          validateRootHash [smallData] machine'
          validateChunksHashes [smallData] chunks

          -- Now add oversized data
          let largeData = RawBytes (BS.replicate largeDataLength 0x48)
          (machine'', chunks') <- interpretCommand machine' (Append largeData)
          case chunks' of
            [firstChunk, secondChunk] -> do
              -- First chunk should contain the small data
              annotate "first chunk should have correct entries count" $ chunkItemEntriesCount firstChunk `shouldBe` 1
              annotate "first chunk data should match small input size" $ (sizeofByteArray $ chunkItemData firstChunk) `shouldBe` smallDataLength + 4
              -- Second chunk should contain the large data
              annotate "second chunk should have correct entries count" $ chunkItemEntriesCount secondChunk `shouldBe` 1
              annotate "second chunk data should match large input size" $ (sizeofByteArray $ chunkItemData secondChunk) `shouldBe` largeDataLength + 4
            l -> annotate "after adding oversized data should emit two chunks" $ length l `shouldBe` 2
          validateRootHash [smallData, largeData] machine''
          validateChunksHashes [smallData, largeData] chunks'

    it "should handle multiple boundary crossings correctly" $ do
      let bufferLength = 50
      machine <- mkMachine' bufferLength
      -- Add data that will cause multiple boundary crossings
      -- 26 bytes total with prefix
      let dataChunk1Length = 22
      let dataChunk1 = RawBytes (BS.replicate dataChunk1Length 0x4A)
      -- 14 bytes total with prefix
      let dataChunk2Length = 10
      let dataChunk2 = RawBytes (BS.replicate dataChunk2Length 0x4A)

      (machine1, chunks1) <- interpretCommand machine (Append dataChunk1)
      -- First add: 0 + 26 = 26, fits (total = 26)
      annotate "first addition" $ do
        annotate "should not emit" $ length chunks1 `shouldBe` 0
        validateRootHash [dataChunk1] machine1

      (machine2, chunks2) <- interpretCommand machine1 (Append dataChunk1)
      -- Second add: 26 + 26 = 52 > 50, so emit buffer with one entry (total = 26)
      -- Start new buffer with new data (total = 26)
      annotate "second addition" $ do
        let chunkEntries = [dataChunk1]
        case chunks2 of
          [chunk] -> do
            annotate "should emit one chunk with one entry" $ chunkItemEntriesCount chunk `shouldBe` 1
            validateChunkDataSize chunkEntries chunk
          l -> annotate "should emit one chunk" $ length l `shouldBe` 1
        validateRootHash [dataChunk1, dataChunk1] machine2
        validateChunksHashes [dataChunk1] chunks2

      (machine3, chunks3) <- interpretCommand machine2 (Append dataChunk2)
      -- Third add: 26 + 14 = 40, fits (total = 40)
      annotate "third addition" $ do
        annotate "should not emit" $ length chunks3 `shouldBe` 0
        validateRootHash [dataChunk1, dataChunk1, dataChunk2] machine3

      (machine4, chunks4) <- interpretCommand machine3 (Append dataChunk2)
      -- Fourth add: 40 + 14 = 54 > 50, so emit buffer with two entries (total = 40)
      -- Start new buffer with new data (total = 14)
      annotate "fourth addition" $ do
        let chunkEntries = [dataChunk1, dataChunk2]
        case chunks4 of
          [chunk] -> do
            annotate "should emit one chunk with two entries" $ chunkItemEntriesCount chunk `shouldBe` 2
            validateChunkDataSize chunkEntries chunk
          l -> annotate "should emit one chunk" $ length l `shouldBe` 1
        validateRootHash [dataChunk1, dataChunk1, dataChunk2, dataChunk2] machine4
        validateChunksHashes [dataChunk1, dataChunk2] chunks4

      (machine5, chunks5) <- interpretCommand machine4 (Append dataChunk2)
      -- Fifth add: 14 + 14 = 28, fits (total = 28)
      annotate "fifth addition" $ do
        annotate "should not emit" $ length chunks5 `shouldBe` 0
        validateRootHash [dataChunk1, dataChunk1, dataChunk2, dataChunk2, dataChunk2] machine5

      (machine6, chunks6) <- interpretCommand machine5 (Append dataChunk2)
      -- Sixth add: 28 + 14 = 42, fits (total = 42)
      annotate "sixth addition" $ do
        annotate "should not emit" $ length chunks6 `shouldBe` 0
        validateRootHash [dataChunk1, dataChunk1, dataChunk2, dataChunk2, dataChunk2, dataChunk2] machine6
      (_digest, finalChunk) <- interpretCommand machine6 Finalize
      annotate "final chunk" $ do
        let chunkEntries = [dataChunk2, dataChunk2, dataChunk2]
        case finalChunk of
          Just chunk -> do
            annotate "should have three entries" $ chunkItemEntriesCount chunk `shouldBe` 3
            validateChunkDataSize chunkEntries chunk
            validateChunkHash chunkEntries chunk
          Nothing -> assertFailure "Expected final chunk on finalization"

    describe "zero buffer length should always emit" $
      forM_ [0, 1, 4, 64, 255] $ \dataChunkLength ->
        it ("should emit chunk immediately when buffer length is zero (dataLen =" ++ show dataChunkLength ++ ")") $ do
          let bufferLength = 0
          let dataChunk = RawBytes (BS.replicate dataChunkLength 0x4B)
          machine <- mkMachine' bufferLength
          (machine', chunks) <- interpretCommand machine (Append dataChunk)
          case chunks of
            [chunk] -> do
              annotate "should emit one chunk with one entry" $ chunkItemEntriesCount chunk `shouldBe` 1
              annotate "chunk data size should match" $ (sizeofByteArray $ chunkItemData chunk) `shouldBe` dataChunkLength + 4
            l -> annotate "should emit one chunk" $ length l `shouldBe` 1
          validateRootHash [dataChunk] machine'
          validateChunksHashes [dataChunk] chunks

finalizationTests :: Spec
finalizationTests =
  describe "Finalization Tests" $ do
    prop "should not emit chunk when finalizing empty buffer" $
      \(Positive bufferLength) -> do
        machine <- mkMachine' bufferLength
        (digest, maybeChunk) <- interpretCommand machine Finalize
        isNothing maybeChunk `shouldBe` True
        -- Digest should still be computed (even if empty)
        annotate "digest should be present" $ case digest of Digest _ -> True `shouldBe` True

    prop "should emit fitting chunks only on finalize" $
      forAll bufferFittingChunks $ \(bufferLength, chunkLengths) -> do
        machine <- mkMachine' bufferLength
        (machine', chunks) <- foldAppendChunks machine (map (RawBytes . flip BS.replicate 0x49) chunkLengths)
        annotate "should not emit chunks before finalization" $ (length chunks) `shouldBe` 0

        (_digest, maybeChunk) <- interpretCommand machine' Finalize
        case maybeChunk of
          Just chunk -> do
            annotate "finalized chunk should have correct entries count" $ chunkItemEntriesCount chunk `shouldBe` length chunkLengths
            annotate "finalized chunk should use correct format" $ chunkItemFormat chunk `shouldBe` ChunkFormatRaw
            annotate "finalized chunk data should have correct size" $ (sizeofByteArray $ chunkItemData chunk) `shouldBe` (sum $ map (+ 4) chunkLengths)
          Nothing -> assertFailure "Expected chunk on finalization with data"

hashTests :: Spec
hashTests =
  describe "Hashes and Merkle Tree Tests" $ do
    prop "entry digest is H(0x01 || ns_str || key || value)" $
      \(entry@(TestUTxO (TestEntry k v)) :: TestUTxO) -> do
        let ns = "utxo/v0"
            expectedHash = Digest (hash $ BS.singleton 1 <> asBytes ns <> k <> packByteString v)
        entryDigest ns (packByteString $ chunkEntryFromUTxO entry) `shouldBe` expectedHash
    prop "entry digest used by chunks is H(0x01 || ns_str || key || value)" $
      \(entries :: [TestUTxO]) -> do
        let ns = "utxo/v0"
        (computedMerkleHash, _) <-
          S.each entries
            & S.foldM_
              ( \acc entry -> do
                  (machine', _) <- interpretCommand acc (Append entry)
                  return machine'
              )
              (mkMachine (16 * 1024 * 1024) (ns, ChunkFormatRaw))
              (\m -> interpretCommand m Finalize)
        let expectedMerkleHash =
              Digest $
                MT.merkleRootHash $
                  MT.finalize $
                    foldl'
                      (\acc (TestUTxO (TestEntry k v)) -> MT.addLeafHash acc $ hash $ BS.singleton 1 <> asBytes ns <> k <> packByteString v)
                      (MT.empty undefined)
                      entries
        computedMerkleHash `shouldBe` expectedMerkleHash

    prop "chunk hash should be H(concat [ digest(e) | e in entries ])" $
      forAll (listOf1 arbitrary) $
        \(entries :: [TestUTxO]) -> do
          let ns = "utxo/v0"
          (_, Just ChunkItem{..}) <-
            S.each entries
              & S.foldM_
                ( \acc entry -> do
                    (machine', _) <- interpretCommand acc (Append entry)
                    return machine'
                )
                (mkMachine (16 * 1024 * 1024) (ns, ChunkFormatRaw))
                (\m -> interpretCommand m Finalize)
          let expectedChunkHash = Digest $ hashFinalize $ foldl' (\acc -> hashUpdate acc . entryDigest ns . packByteString . chunkEntryFromUTxO) (hashInit) entries
          chunkItemHash `shouldBe` expectedChunkHash

validateChunkDataSize :: [RawBytes] -> ChunkItem -> IO ()
validateChunkDataSize entries chunk =
  annotate "chunk data size should match" $ computedSize `shouldBe` expectedSize
 where
  computedSize = sizeofByteArray $ chunkItemData chunk
  expectedSize = sum (map (\(RawBytes b) -> BS.length b + 4) entries)

validateRootHash :: [RawBytes] -> BuilderMachine -> IO ()
validateRootHash entries machine = do
  (computedRootHash, _) <- interpretCommand machine Finalize
  let expectedRootHash =
        Digest $
          MT.merkleRootHash $
            MT.finalize $
              foldl'
                ( \acc (RawBytes b) ->
                    MT.add acc $ asBytes testNamespace <> b
                )
                (MT.empty undefined)
                entries
  annotate "root hash should match expected" $ computedRootHash `shouldBe` expectedRootHash

validateChunkHash :: [RawBytes] -> ChunkItem -> IO ()
validateChunkHash entries (ChunkItem{..}) = do
  let expectedChunkHash =
        Digest $
          hashFinalize $
            foldl'
              (\acc (RawBytes b) -> hashUpdate acc $ entryDigest testNamespace b)
              (hashInit)
              entries
  annotate "chunk item hash should match expected" $ chunkItemHash `shouldBe` expectedChunkHash

validateChunksHashes :: [RawBytes] -> [ChunkItem] -> IO ()
validateChunksHashes =
  foldM_ $
    \entries chunkItem -> do
      let (chunkEntries, remainingEntries) = splitAt (chunkItemEntriesCount chunkItem) entries
      validateChunkHash chunkEntries chunkItem
      pure remainingEntries
