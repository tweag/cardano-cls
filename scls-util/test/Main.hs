{-# LANGUAGE BlockArguments #-}

module Main (main) where

import ChecksumSpec
import DiffSpec
import InfoSpec
import MergeSpec
import SplitSpec
import System.Environment (lookupEnv)
import TermDiffSpec
import Test.Hspec
import VerifySpec

main :: IO ()
main = do
  sclsUtilPath <- lookupEnv "SCLS_UTIL_PATH"
  hspec $
    describe "scls-util binary tests" do
      checksumCommandTests sclsUtilPath
      verifyNsCommandTests sclsUtilPath
      infoCommandTests sclsUtilPath
      listNsCommandTests sclsUtilPath
      splitCommandTests sclsUtilPath
      mergeCommandTests sclsUtilPath
      diffCommandTests sclsUtilPath
      termDiffTests
      verifyCommandTests sclsUtilPath
