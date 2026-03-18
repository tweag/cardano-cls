{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module DebugSpec (debugCommandTests) where

import Cardano.SCLS.Internal.Reader (extractNamespaceList)
import Common
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

debugCommandTests :: Maybe FilePath -> Spec
debugCommandTests mSclsUtil = describe "debug generate command" do
  it "generates a file successfully for a known namespace" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      let fileName = dir </> "debug.scls"
      (exitCode, _, _) <-
        runSclsUtil
          mSclsUtil
          ["debug", "generate", fileName, "--namespace", "utxo/v0:1"]
      exitCode `shouldBe` ExitSuccess

  it "generates namespaces in lexicographic order when supplied in reverse order" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      let fileName = dir </> "debug.scls"
      (exitCode, _, _) <-
        runSclsUtil
          mSclsUtil
          [ "debug"
          , "generate"
          , fileName
          , "--namespace"
          , "utxo/v0:1"
          , "--namespace"
          , "gov/constitution/v0:1"
          ]
      exitCode `shouldBe` ExitSuccess
      nsps <- extractNamespaceList fileName
      nsps `shouldBe` ["gov/constitution/v0", "utxo/v0"]

  it "generates namespaces in lexicographic order for multiple out-of-order namespaces" do
    withSystemTempDirectory "scls-util-test-XXXXXX" \dir -> do
      let fileName = dir </> "debug.scls"
      (exitCode, _, _) <-
        runSclsUtil
          mSclsUtil
          [ "debug"
          , "generate"
          , fileName
          , "--namespace"
          , "utxo/v0:1"
          , "--namespace"
          , "gov/constitution/v0:1"
          , "--namespace"
          , "accounts/v0:1"
          ]
      exitCode `shouldBe` ExitSuccess
      nsps <- extractNamespaceList fileName
      nsps `shouldBe` ["accounts/v0", "gov/constitution/v0", "utxo/v0"]
