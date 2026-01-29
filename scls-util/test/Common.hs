{-# LANGUAGE OverloadedStrings #-}

module Common (generateTestFile, runSclsUtil) where

import Cardano.SCLS.Internal.Serializer.Dump.Plan (addChunks, defaultSerializationPlan)
import Cardano.SCLS.Internal.Serializer.Reference.Impl qualified as Reference
import Cardano.Types.Namespace (Namespace, asString)
import Cardano.Types.SlotNo (SlotNo (SlotNo))
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Char8 qualified as BS8
import Data.Function ((&))
import Data.Map qualified as Map
import Data.MemPack.Extra (RawBytes (..))
import Streaming qualified as S
import Streaming.Prelude qualified as S
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

generateTestFile :: FilePath -> IO (FilePath, [Namespace])
generateTestFile dir = do
  let fileName = dir </> "test.scls"
      testData =
        [ ("namespace1", [BS8.pack (show i) | i <- [1 :: Int .. 100]])
        , ("namespace2", [BS8.pack (show i) | i <- [1 :: Int .. 50]])
        , ("namespace3", [BS8.pack (show i) | i <- [1 :: Int .. 75]])
        ]
      mkStream =
        S.each
          [ ns S.:> S.each (map RawBytes entries)
          | (ns, entries) <- testData
          ]

  _ <-
    runResourceT $
      Reference.serialize @RawBytes
        fileName
        (SlotNo 1)
        (Map.fromList [(asString ns, 1) | (ns, _) <- testData])
        (defaultSerializationPlan & addChunks mkStream)

  return (fileName, map fst testData)

runSclsUtil :: Maybe FilePath -> [String] -> IO (ExitCode, String, String)
runSclsUtil Nothing args = do
  readProcessWithExitCode "cabal" (["run", "scls-util", "--"] ++ args) ""
runSclsUtil (Just path) args = do
  readProcessWithExitCode path args ""
