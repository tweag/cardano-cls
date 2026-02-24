{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module MergeSpec (mergeCommandTests) where

import Cardano.SCLS.Internal.Reader (
  extractNamespaceList,
  extractRootHash,
 )
import Cardano.SCLS.Internal.Serializer.Dump.Plan (addChunks, defaultSerializationPlan)
import Cardano.SCLS.Internal.Serializer.Reference.Impl qualified as Reference
import Cardano.Types.Namespace (Namespace (..))
import Cardano.Types.Namespace qualified as Namespace
import Cardano.Types.SlotNo (SlotNo (SlotNo))
import Common (generateTestFile, runSclsUtil)
import Control.Monad (forM)
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Char8 qualified as BS8
import Data.Function ((&))
import Data.Map qualified as Map
import Data.MemPack.Extra (RawBytes (..))
import Streaming qualified as S
import Streaming.Prelude qualified as S
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.Expectations.Contrib (annotate)

generateSplitTestFiles :: FilePath -> IO ([(FilePath, Namespace)])
generateSplitTestFiles dir = do
  let testData =
        [ ("namespace1", [BS8.pack (show i) | i <- [1 :: Int .. 100]])
        , ("namespace2", [BS8.pack (show i) | i <- [1 :: Int .. 50]])
        , ("namespace3", [BS8.pack (show i) | i <- [1 :: Int .. 75]])
        ]

  forM testData \(ns, entries) -> do
    let fileName = dir </> Namespace.humanFileNameFor ns
        mkStream = S.yield (ns S.:> S.each (map RawBytes entries))

    _ <-
      runResourceT $
        Reference.serialize @RawBytes
          fileName
          (SlotNo 1)
          (Map.fromList [(Namespace.asString ns, 1)])
          (defaultSerializationPlan & addChunks mkStream)

    pure (fileName, ns)

generateOverlappingNsSplitTestFiles :: FilePath -> IO ([(FilePath, [Namespace])])
generateOverlappingNsSplitTestFiles dir = do
  let testData =
        [
          ( 1 :: Integer
          ,
            [ ("namespace1", [BS8.pack (show i) | i <- [1 :: Int .. 100]])
            , ("namespace2", [BS8.pack (show i) | i <- [1 :: Int .. 50]])
            ]
          )
        ,
          ( 2
          ,
            [ ("namespace2", [BS8.pack (show i) | i <- [51 :: Int .. 100]])
            , ("namespace3", [BS8.pack (show i) | i <- [1 :: Int .. 75]])
            ]
          )
        ,
          ( 3
          ,
            [ ("namespace3", [BS8.pack (show i) | i <- [76 :: Int .. 150]])
            ]
          )
        ]

  forM testData \(i, nsEntries) -> do
    let fileName = dir </> "file" ++ show i ++ ".scls"

    let stream =
          S.each nsEntries
            & S.map \(ns, entries) -> ns S.:> S.each (map RawBytes entries)

    _ <-
      runResourceT $
        Reference.serialize @RawBytes
          fileName
          (SlotNo 1)
          (Map.fromList [(Namespace.asString ns, 1) | (ns, _) <- nsEntries])
          (defaultSerializationPlan & addChunks stream)

    pure (fileName, map fst nsEntries)

runSclsUtilExtraNs :: Maybe FilePath -> [String] -> IO (ExitCode, String, String)
runSclsUtilExtraNs mSclsUtil args =
  runSclsUtil mSclsUtil $ ["--namespace-keysize", "namespace1:1", "--namespace-keysize", "namespace2:1", "--namespace-keysize", "namespace3:1"] ++ args

mergeCommandTests :: Maybe FilePath -> Spec
mergeCommandTests mSclsUtil = describe "merge command" do
  it "merges multiple files into one" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      testData <- generateSplitTestFiles dir

      let mergedFile = dir </> "merged.scls"

      (exitCode, _, _) <- runSclsUtilExtraNs mSclsUtil (["file", mergedFile, "merge"] ++ map fst testData)

      exitCode `shouldBe` ExitSuccess

  it "handles files with overlapping namespaces" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      files <- generateOverlappingNsSplitTestFiles dir
      let mergedFile = dir </> "merged.scls"

      (exitCode, _, stderr) <- runSclsUtilExtraNs mSclsUtil $ ["file", mergedFile, "merge"] <> map fst files

      exitCode `shouldBe` ExitSuccess
      stderr `shouldContain` "3 unique namespace(s)"

  it "fails for non-existent source files" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      let mergedFile = dir </> "merged.scls"
      (exitCode, _, _) <- runSclsUtilExtraNs mSclsUtil ["file", mergedFile, "merge", "/nonexistent/file.scls"]

      exitCode `shouldBe` ExitFailure 1

  it "roundtrips" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      (originalFile, namespaces) <- generateTestFile dir

      let splitDir = dir </> "split"
      (splitExitCode, _, _) <- runSclsUtilExtraNs mSclsUtil ["file", originalFile, "split", splitDir]
      annotate "splits successfully" $ splitExitCode `shouldBe` ExitSuccess

      let mergedFile = dir </> "merged.scls"
      let splitFiles = [splitDir </> Namespace.humanFileNameFor ns | ns <- namespaces]
      (mergeExitCode, _, _) <- runSclsUtilExtraNs mSclsUtil (["file", mergedFile, "merge"] ++ splitFiles)
      annotate "merges successfully" $ mergeExitCode `shouldBe` ExitSuccess

      originalNamespaces <- extractNamespaceList originalFile
      mergedNamespaces <- extractNamespaceList mergedFile
      annotate "namespace lists match" $ mergedNamespaces `shouldBe` originalNamespaces

      originalHash <- extractRootHash originalFile
      mergedHash <- extractRootHash mergedFile
      annotate "root hashes match" $ mergedHash `shouldBe` originalHash
