{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.SCLS.Util.File (FileCmd (..), ExtractOptions (..), runFileCmd) where

import Cardano.SCLS.Util.File.Info
import Cardano.SCLS.Util.File.Tool
import Cardano.SCLS.Util.Result

data FileCmd
  = ListNamespaces
  | Merge [FilePath]
  | Extract FilePath ExtractOptions
  | Split FilePath
  | Info

runFileCmd :: FilePath -> FileCmd -> IO Result
runFileCmd fileName = \case
  ListNamespaces -> do
    listNamespaces fileName
  Merge inputFiles -> do
    mergeFiles fileName inputFiles
  Extract outputFile extractOptions -> do
    extract fileName outputFile extractOptions
  Split outputDir -> do
    splitFile fileName outputDir
  Info -> displayInfo fileName
