{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DiffSpec (diffCommandTests) where

import Cardano.SCLS.CBOR.Canonical.Decoder
import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.CDDL (knownNamespaceKeySizes)
import Cardano.SCLS.Entry.IsKey (IsKey (keySize, packKeyM, unpackKeyM))
import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry (..))
import Cardano.SCLS.Internal.Serializer.Dump.Plan (addChunks, defaultSerializationPlan)
import Cardano.SCLS.Internal.Serializer.Reference.Impl qualified as Reference
import Cardano.SCLS.NamespaceCodec (CanonicalCBOREntryDecoder (..), CanonicalCBOREntryEncoder (..), KnownNamespace (..))
import Cardano.SCLS.NamespaceKey (NamespaceKeySize)
import Cardano.Types.Namespace qualified as Namespace
import Cardano.Types.SlotNo (SlotNo (SlotNo))
import Codec.CBOR.Term (Term (..))
import Common (runSclsUtil)
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Base16 qualified as Base16
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.MemPack
import Data.MemPack.Extra (CBORTerm, mkCBORTerm)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word8)
import Streaming qualified as S
import Streaming.Prelude qualified as S
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

type instance NamespaceKeySize "test/v0" = 1

instance KnownNamespace "test/v0" where
  type NamespaceKey "test/v0" = K
  type NamespaceEntry "test/v0" = Term

instance CanonicalCBOREntryEncoder "test/v0" Term where
  encodeEntry = toCanonicalCBOR Proxy

instance CanonicalCBOREntryDecoder "test/v0" Term where
  decodeEntry = fromCanonicalCBOR

type instance NamespaceKeySize "test/v1" = 1

instance KnownNamespace "test/v1" where
  type NamespaceKey "test/v1" = K
  type NamespaceEntry "test/v1" = Term

instance CanonicalCBOREntryEncoder "test/v1" Term where
  encodeEntry = toCanonicalCBOR Proxy

instance CanonicalCBOREntryDecoder "test/v1" Term where
  decodeEntry = fromCanonicalCBOR

newtype K = K Word8
  deriving (Eq, Ord, Show)
  deriving newtype (MemPack)

instance IsKey K where
  keySize = 1
  packKeyM (K w) = do
    packM w
  unpackKeyM = do
    w <- unpackM
    return (K w)

diffCommandTests :: Maybe FilePath -> Spec
diffCommandTests mSclsUtil = describe "diff command" do
  it "returns OK in silent mode when files are identical" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      let file1 = dir </> "file1.scls"
      let file2 = dir </> "file2.scls"
      _ <- writeNamespaceFile file1 [("test/v0", [(K 1, TInt 1)])]
      _ <- writeNamespaceFile file2 [("test/v0", [(K 1, TInt 1)])]

      (exitCode, stdout, _) <-
        runSclsUtil mSclsUtil ["diff", file1, file2, "--depth", "silent", "--namespaces", "test/v0", "--namespace-keysize", "test/v0:1"]

      exitCode `shouldBe` ExitSuccess
      stdout `shouldContain` "OK"

  it "returns DIFF in silent mode when values differ" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      let file1 = dir </> "file1.scls"
      let file2 = dir </> "file2.scls"
      _ <- writeNamespaceFile file1 [("test/v0", [(K 1, TInt 1)])]
      _ <- writeNamespaceFile file2 [("test/v0", [(K 1, TInt 2)])]

      (exitCode, stdout, _) <-
        runSclsUtil mSclsUtil ["diff", file1, file2, "--depth", "silent", "--namespace-keysize", "test/v0:1"]

      exitCode `shouldBe` ExitFailure 1
      stdout `shouldContain` "DIFF"

  it "reports namespace-only differences in reference mode" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      let file1 = dir </> "file1.scls"
      let file2 = dir </> "file2.scls"
      _ <-
        writeNamespaceFile
          file1
          [ ("test/v0", [(K 1, TInt 1)])
          , ("test/v2", [(K 1, TInt 1)])
          ]
      _ <-
        writeNamespaceFile
          file2
          [ ("test/v1", [(K 1, TInt 1), (K 2, TInt 2)])
          , ("test/v2", [(K 1, TInt 1)])
          ]

      (exitCode, stdout, _) <-
        runSclsUtil
          mSclsUtil
          [ "diff"
          , file1
          , file2
          , "--depth"
          , "reference"
          , "--namespace-keysize"
          , "test/v0:1"
          , "--namespace-keysize"
          , "test/v1:1"
          , "--namespace-keysize"
          , "test/v2:1"
          ]

      exitCode `shouldBe` ExitFailure 1
      stdout `shouldContain` "- test/v0"
      stdout `shouldContain` "+ test/v1"
      stdout `shouldNotContain` "test/v2"

  it "reports key additions, removals, and value changes in reference mode" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      let file1 = dir </> "file1.scls"
      let file2 = dir </> "file2.scls"
      _ <- writeNamespaceFile file1 [("test/v0", [(K 1, TInt 1), (K 2, TInt 2)])]
      _ <- writeNamespaceFile file2 [("test/v0", [(K 2, TInt 3), (K 3, TInt 4)])]

      (exitCode, stdout, _) <-
        runSclsUtil
          mSclsUtil
          [ "diff"
          , file1
          , file2
          , "--depth"
          , "reference"
          , "--namespace-keysize"
          , "test/v0:1"
          ]

      exitCode `shouldBe` ExitFailure 1
      stdout `shouldContain` ("- " <> renderKeyRef "test/v0" (K 1))
      stdout `shouldContain` ("* " <> renderKeyRef "test/v0" (K 2))
      stdout `shouldContain` ("+ " <> renderKeyRef "test/v0" (K 3))

  it "prints full diff output for value changes" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      let file1 = dir </> "file1.scls"
      let file2 = dir </> "file2.scls"
      _ <- writeNamespaceFile file1 [("test/v0", [(K 1, TInt 1)])]
      _ <- writeNamespaceFile file2 [("test/v0", [(K 1, TInt 2)])]

      (exitCode, stdout, _) <-
        runSclsUtil
          mSclsUtil
          [ "diff"
          , file1
          , file2
          , "--depth"
          , "full"
          , "--namespace-keysize"
          , "test/v0:1"
          ]

      exitCode `shouldBe` ExitFailure 1
      stdout `shouldContain` ("* " <> renderKeyRef "test/v0" (K 1))
      stdout `shouldContain` ("--- " <> file1)
      stdout `shouldContain` ("+++ " <> file2)

  it "filters out second-only entries with --only-first" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      let file1 = dir </> "file1.scls"
      let file2 = dir </> "file2.scls"
      _ <- writeNamespaceFile file1 [("test/v0", [(K 1, TInt 1), (K 2, TInt 2)]), ("test/v1", [(K 1, TInt 1)])]
      _ <- writeNamespaceFile file2 [("test/v0", [(K 1, TInt 2), (K 2, TInt 1)]), ("test/v1", [(K 1, TInt 2)])]

      (exitCode, stdout, _) <-
        runSclsUtil mSclsUtil ["diff", file1, file2, "--depth", "reference", "--only-first", "--namespace-keysize", "test/v0:1", "--namespace-keysize", "test/v1:1"]

      exitCode `shouldBe` ExitFailure 1
      stdout `shouldContain` ("* " <> renderKeyRef "test/v0" (K 1))
      stdout `shouldNotContain` ("* " <> renderKeyRef "test/v0" (K 2))
      stdout `shouldNotContain` ("* " <> renderKeyRef "test/v1" (K 1))

  it "restricts comparison to --namespaces selection" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      let file1 = dir </> "file1.scls"
      let file2 = dir </> "file2.scls"
      _ <- writeNamespaceFile file1 [("test/v0", [(K 1, TInt 1)])]
      _ <- writeNamespaceFile file2 [("test/v0", [(K 1, TInt 2)])]

      (exitCode, stdout, _) <-
        runSclsUtil mSclsUtil ["diff", file1, file2, "--depth", "silent", "--namespaces", "gov/pparams/v0", "--namespace-keysize", "test/v0:1"]

      exitCode `shouldBe` ExitSuccess
      stdout `shouldContain` "OK"

writeNamespaceFile :: FilePath -> [(T.Text, [(K, Term)])] -> IO FilePath
writeNamespaceFile filePath nsEntries = do
  runResourceT $
    Reference.serialize @(ChunkEntry K CBORTerm)
      filePath
      (SlotNo 1)
      testNamespaces
      ( defaultSerializationPlan
          & addChunks
            (S.each $ nsEntries <&> \(namespace, entries) -> (Namespace.fromText namespace S.:> S.each (map mkEntry entries)))
      )
  pure filePath
 where
  mkEntry (key, term) = ChunkEntry key (mkCBORTerm term)

testNamespaces :: Map.Map String Int
testNamespaces =
  knownNamespaceKeySizes
    & Map.insert "test/v0" 1
    & Map.insert "test/v1" 1
    & Map.insert "test/v2" 1

renderKeyRef :: T.Text -> K -> String
renderKeyRef ns key =
  T.unpack ns <> "/" <> T.unpack (decodeUtf8 (Base16.encode $ packByteString key))
