{-# LANGUAGE BlockArguments #-}

module Main (main) where

import ChecksumSpec
import InfoSpec
import MergeSpec
import SplitSpec
import System.Environment (lookupEnv)
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
      verifyCommandTests sclsUtilPath
