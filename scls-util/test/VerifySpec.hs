{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module VerifySpec (verifyCommandTests) where

import Common
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

verifyCommandTests :: Spec
verifyCommandTests = describe "check command" do
  it "verify command verifies a valid SCLS file" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      -- arrange
      (generateExitCode, _stdout, _) <- runSclsUtil ["debug", "generate", dir </> "output.scls", "--namespace", "utxo/v0:10"]
      generateExitCode `shouldBe` ExitSuccess
      -- act

      (verifyExitCode, _stdout, _) <- runSclsUtil ["checksum", dir </> "output.scls"]

      -- assert
      verifyExitCode `shouldBe` ExitSuccess

  it "verify command fails for non-existent file" do
    (exitCode, _, _) <- runSclsUtil ["verify", "/nonexistent/file.scls"]

    exitCode `shouldBe` ExitFailure 1
