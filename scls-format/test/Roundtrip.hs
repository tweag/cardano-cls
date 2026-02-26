{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Roundtrip (
  tests,
) where

import Cardano.SCLS.CBOR.Canonical.Encoder (canonicalizeTerm)
import Cardano.SCLS.CDDL (knownNamespaceKeySizes, knownNamespaces)
import Cardano.SCLS.Internal.Entry.CBOREntry (GenericCBOREntry (GenericCBOREntry), SomeCBOREntry (SomeCBOREntry))
import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry (ChunkEntry))
import Cardano.SCLS.Internal.Hash (Digest (..))
import Cardano.SCLS.Internal.Reader (extractRootHash, withHeader, withLatestManifestFrame, withNamespacedData, withRecordData)
import Cardano.SCLS.Internal.Record.Hdr (mkHdr)
import Cardano.SCLS.Internal.Record.Manifest (Manifest (..), ManifestSummary (..))
import Cardano.SCLS.Internal.Record.Metadata (Metadata (..), MetadataEntry (MetadataEntry))
import Cardano.SCLS.Internal.Serializer.Dump.Plan (SerializationPlan, addChunks, addMetadata, defaultSerializationPlan, withManifestComment, withTimestamp)
import Cardano.SCLS.Internal.Serializer.External.Impl qualified as External (serialize)
import Cardano.SCLS.Internal.Serializer.HasKey (nubByKey, sortByKey)
import Cardano.SCLS.Internal.Serializer.Reference.Impl qualified as Reference (serialize)
import Cardano.SCLS.NamespaceCodec (NamespaceKeySize, namespaceKeySize)
import Cardano.SCLS.NamespaceSymbol (KnownSpec (namespaceSpec), SomeNamespaceSymbol (..), toString)
import Cardano.Types.Namespace (Namespace)
import Cardano.Types.Namespace qualified as Namespace
import Cardano.Types.SlotNo (SlotNo (..))
import Codec.CBOR.Cuddle.CBOR.Gen (generateFromName)
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.Resolve (
  asMap,
  buildMonoCTree,
  buildRefCTree,
  buildResolvedCTree,
 )
import Codec.CBOR.Cuddle.Huddle (toCDDL)
import Codec.CBOR.Cuddle.IndexMappable (mapCDDLDropExt, mapIndex)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResIO, runResourceT)
import Crypto.Hash.MerkleTree.Incremental qualified as MT
import Data.Function ((&))
import Data.Map (Map)
import Data.MemPack
import Data.MemPack.Extra
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), formatParseM)
import Streaming.Prelude qualified as S
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Random.Stateful (globalStdGen, uniformByteStringM)
import Test.AntiGen (runAntiGen)
import Test.Hspec
import Test.Hspec.Expectations.Contrib
import Test.QuickCheck (generate)

tests :: Spec
tests =
  describe "Roundtrip test" do
    mkRoundtripTestsFor "Reference" (Reference.serialize @SomeCBOREntry)
    mkRoundtripTestsFor "External" (External.serialize @SomeCBOREntry)

mkRoundtripTestsFor :: String -> SerializeF -> Spec
mkRoundtripTestsFor groupName serialize =
  describe groupName $ do
    sequence_
      [ context (toString n) $ it "should succeed with stream roundtrip" $ roundtrip n (mapCDDLDropExt $ toCDDL (namespaceSpec p))
      | n@(SomeNamespaceSymbol p) <- knownNamespaces
      ]
    it "should write/read manifest comment" $ do
      withSystemTempDirectory "scls-format-test-XXXXXX" $ \fn -> do
        let fileName = (fn </> "data.scls")
            testComment = T.pack "This is a file comment."
        _ <-
          runResourceT $
            serialize
              fileName
              (SlotNo 1)
              knownNamespaceKeySizes
              ( defaultSerializationPlan
                  & withManifestComment testComment
              )

        withLatestManifestFrame
          ( \Manifest{summary = ManifestSummary{..}} ->
              comment `shouldBe` (Just testComment)
          )
          fileName
    it "should write/read manifest timestamp" $ do
      withSystemTempDirectory "scls-format-test-XXXXXX" $ \fn -> do
        let fileName = (fn </> "data.scls")
        timestamp <- getCurrentTime
        _ <-
          runResourceT $
            serialize
              fileName
              (SlotNo 1)
              knownNamespaceKeySizes
              ( defaultSerializationPlan
                  & withTimestamp timestamp
              )

        withLatestManifestFrame
          ( \Manifest{summary = ManifestSummary{..}} ->
              formatParseM iso8601Format (T.unpack createdAt) `shouldReturn` timestamp
          )
          fileName
 where
  roundtrip (SomeNamespaceSymbol (p :: proxy ns)) cddl = do
    case buildMonoCTree =<< buildResolvedCTree (buildRefCTree $ asMap cddl) of
      Left err -> expectationFailure $ "Failed to build CTree: " ++ show err
      Right mt -> withSystemTempDirectory "scls-format-test-XXXXXX" $ \fn -> do
        let kSize = namespaceKeySize @ns
            namespace = Namespace.fromSymbol p
        entries <-
          fmap nubByKey $ replicateM 1024 $ do
            key <- uniformByteStringM kSize globalStdGen
            term <- liftIO . generate . runAntiGen $ generateFromName (mapIndex mt) (Name (T.pack "record_entry"))
            Right canonicalTerm <- pure $ canonicalizeTerm p term
            pure $! SomeCBOREntry (GenericCBOREntry $ ChunkEntry (ByteStringSized @(NamespaceKeySize ns) key) (mkCBORTerm canonicalTerm))
        mEntries <-
          replicateM 1024 $ do
            MetadataEntry
              <$> (uniformByteStringM 20 globalStdGen)
              <*> (uniformByteStringM 100 globalStdGen)
        let fileName = (fn </> "data.scls")
        _ <-
          runResourceT $
            serialize
              fileName
              (SlotNo 1)
              knownNamespaceKeySizes
              ( defaultSerializationPlan
                  & addChunks (S.each [namespace S.:> S.each entries])
                  & addMetadata (S.each mEntries)
              )
        withHeader
          fileName
          ( \hdr ->
              annotate
                "header roundtrip successful"
                $ hdr
                  `shouldBe` mkHdr
          )
        withNamespacedData
          fileName
          namespace
          ( \stream -> do
              decoded_data <- S.toList_ stream
              annotate
                "Stream roundtrip successful"
                $ [SomeCBOREntry e | (e :: GenericCBOREntry (NamespaceKeySize ns)) <- decoded_data]
                  `shouldBe` (sortByKey entries)
          )
        -- Check roundtrip of root hash
        file_digest <- extractRootHash fileName
        expected_digest <-
          S.each (sortByKey entries)
            & S.map ((<>) (Namespace.asBytes namespace) . packByteString)
            & S.fold_ MT.add (MT.empty undefined) (Digest . MT.merkleRootHash . MT.finalize)
        annotate
          "Root hash roundtrip successful"
          $ file_digest
            `shouldBe` (Digest $ MT.merkleRootHash $ MT.finalize $ MT.add (MT.empty undefined) expected_digest)

        withRecordData
          fileName
          ( \stream -> do
              decoded_metadata <- S.toList_ stream
              annotate
                "Metadata stream roundtrip successful"
                $ mconcat [metadataEntries | Metadata{metadataEntries} <- decoded_metadata]
                  `shouldBe` mEntries
          )

type SerializeF = FilePath -> SlotNo -> Map String Int -> SerializationPlan SomeCBOREntry ResIO -> ResIO (Either [Namespace] ())
