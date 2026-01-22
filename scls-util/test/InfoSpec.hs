{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module InfoSpec (infoCommandTests, listNsCommandTests) where

import Cardano.SCLS.Internal.Serializer.Dump.Plan (addChunks, defaultSerializationPlan)
import Cardano.SCLS.Internal.Serializer.Reference.Impl qualified as Reference
import Cardano.Types.Namespace qualified as Namespace
import Cardano.Types.SlotNo (SlotNo (SlotNo))
import Common
import Control.Monad (forM_)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Function ((&))
import Data.MemPack.Extra (RawBytes (..))
import Streaming.Prelude qualified as S
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

infoCommandTests :: Spec
infoCommandTests = describe "info command" do
  it "displays file information" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      (fileName, _) <- generateTestFile dir
      (exitCode, _, _) <- runSclsUtil ["file", fileName, "info"]

      exitCode `shouldBe` ExitSuccess

  it "lists all namespaces with their hashes" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      (fileName, namespaces) <- generateTestFile dir
      (exitCode, stdout, _) <- runSclsUtil ["file", fileName, "info"]

      exitCode `shouldBe` ExitSuccess

      forM_ namespaces \ns -> do
        stdout `shouldContain` Namespace.asString ns

  it "fails for non-existent file" do
    (exitCode, stdout, stderr) <- runSclsUtil ["file", "/nonexistent/file.scls", "info"]

    exitCode `shouldBe` ExitFailure 1

    (stdout <> stderr) `shouldContain` "Error"

listNsCommandTests :: Spec
listNsCommandTests = describe "list-ns command" do
  it "includes all expected namespaces" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      (fileName, expectedNamespaces) <- generateTestFile dir

      (exitCode, stdout, _) <- runSclsUtil ["file", fileName, "list-ns"]

      exitCode `shouldBe` ExitSuccess

      forM_ expectedNamespaces \ns -> do
        stdout `shouldContain` Namespace.asString ns

  it "fails for non-existent file" do
    (exitCode, stdout, stderr) <- runSclsUtil ["file", "/nonexistent/file.scls", "list-ns"]

    exitCode `shouldBe` ExitFailure 1

    (stdout <> stderr) `shouldContain` "Error"

  it "handles empty namespace list" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      let fileName = dir </> "empty.scls"
      -- Create file with no namespaces
      _ <-
        runResourceT $
          Reference.serialize @RawBytes
            fileName
            (SlotNo 1)
            (defaultSerializationPlan & addChunks (S.each []))

      (exitCode, _, stderr) <- runSclsUtil ["file", fileName, "list-ns"]

      exitCode `shouldBe` ExitSuccess

      stderr `shouldContain` "No namespaces found"
