{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SplitSpec where

import Cardano.SCLS.Internal.Reader (
  extractNamespaceList,
  withLatestManifestFrame,
 )
import Cardano.SCLS.Internal.Record.Manifest
import Cardano.Types.Namespace qualified as Namespace
import Common
import Control.Monad (forM_)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.Expectations.Contrib

splitCommandTests :: Maybe FilePath -> Spec
splitCommandTests mSclsUtil = describe "split command" do
  it "splits a file by namespace" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      (sourceFile, namespaces) <- generateTestFile dir
      let outputDir = dir </> "split"

      (exitCode, _, _) <- runSclsUtil mSclsUtil ["file", sourceFile, "split", outputDir]

      exitCode `shouldBe` ExitSuccess

      -- Verify each namespace was split into its own file
      forM_ namespaces \ns -> do
        let splitFile = outputDir </> Namespace.humanFileNameFor ns
        fileExists <- doesFileExist splitFile
        fileExists `shouldBe` True

        -- Verify the split file contains only one namespace
        splitNamespaces <- extractNamespaceList splitFile
        splitNamespaces `shouldBe` [ns]

  it "creates correct number of output files" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      (sourceFile, namespaces) <- generateTestFile dir
      let outputDir = dir </> "split"

      (exitCode, _, _) <- runSclsUtil mSclsUtil ["file", sourceFile, "split", outputDir]

      annotate "exit code" $ exitCode `shouldBe` ExitSuccess

      withLatestManifestFrame
        ( \Manifest{nsInfo = originalNsInfo} -> do
            forM_ namespaces \ns -> do
              let splitFile = outputDir </> Namespace.humanFileNameFor ns
              withLatestManifestFrame
                ( \Manifest{..} -> do
                    annotate "only one namespace" $ Map.size nsInfo `shouldBe` 1
                    annotate "namespace info matches original" $ Map.lookup ns nsInfo `shouldBe` Map.lookup ns originalNsInfo
                )
                splitFile
        )
        sourceFile

  it "fails for non-existent file" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      let outputDir = dir </> "split"
      (exitCode, _, _) <- runSclsUtil mSclsUtil ["file", "/nonexistent/file.scls", "split", outputDir]

      exitCode `shouldBe` ExitFailure 1

  it "extracts namespaces correctly" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      (sourceFile, namespaces) <- generateTestFile dir
      let outputFile = dir </> "extracted.scls"

      let namespacesToExtract = [namespaces !! 0, namespaces !! 2]

      (exitCode, _, _) <- runSclsUtil mSclsUtil ["file", sourceFile, "extract", outputFile, "--namespaces", T.unpack $ T.intercalate "," (Namespace.asText <$> namespacesToExtract)]

      exitCode `shouldBe` ExitSuccess

      withLatestManifestFrame
        ( \Manifest{nsInfo = originalNsInfo} -> do
            withLatestManifestFrame
              ( \Manifest{nsInfo = extractedNsInfo} -> do
                  annotate "extracted namespaces should match" $ Map.keys extractedNsInfo `shouldMatchList` namespacesToExtract
                  forM_ namespacesToExtract \ns -> do
                    annotate "namespace info should match" $ Map.lookup ns extractedNsInfo `shouldBe` Map.lookup ns originalNsInfo
              )
              outputFile
        )
        sourceFile

  it "skips verification with --no-verify flag" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      (sourceFile, _) <- generateTestFile dir
      let outputDir = dir </> "split"

      (exitCode, _, stderr) <- runSclsUtil mSclsUtil ["file", sourceFile, "split", outputDir, "--no-verify"]

      annotate "exit code" $ exitCode `shouldBe` ExitSuccess
      annotate "skips verification" $ stderr `shouldContain` "Skipping verification"
      annotate "no verification PASSED message" $ stderr `shouldNotContain` "All namespace verifications PASSED"

  it "skips verification for extract with --no-verify flag" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      (sourceFile, namespaces) <- generateTestFile dir
      let outputFile = dir </> "extracted.scls"
          namespacesToExtract = [namespaces !! 0]

      (exitCode, _, stderr) <- runSclsUtil mSclsUtil ["file", sourceFile, "extract", outputFile, "--namespaces", T.unpack $ T.intercalate "," (Namespace.asText <$> namespacesToExtract), "--no-verify"]

      annotate "exit code" $ exitCode `shouldBe` ExitSuccess
      annotate "skips verification" $ stderr `shouldContain` "Skipping verification"
      annotate "no verification PASSED message" $ stderr `shouldNotContain` "All namespace verifications PASSED"
