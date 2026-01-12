{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.SCLS.Util.File (FileCmd (..), ExtractOptions (..), runFileCmd, SplitOptions (..), UnpackOptions (..)) where

import Cardano.SCLS.Util.File.Info
import Cardano.SCLS.Util.File.Tool
import Cardano.SCLS.Util.Result
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.Text qualified as T

data FileCmd
  = ListNamespaces
  | Merge [FilePath]
  | Extract FilePath ExtractOptions
  | Split FilePath SplitOptions
  | Info
  | Unpack FilePath T.Text UnpackOptions

runFileCmd :: (MonadLogger m, MonadUnliftIO m, MonadCatch m) => FilePath -> FileCmd -> m Result
runFileCmd fileName = \case
  ListNamespaces -> do
    listNamespaces fileName
  Merge inputFiles -> do
    mergeFiles fileName inputFiles
  Extract outputFile extractOptions -> do
    extract fileName outputFile extractOptions
  Split outputDir splitOptions -> do
    splitFile fileName outputDir splitOptions
  Info -> displayInfo fileName
  Unpack unpackDest namespace unpackOptions -> do
    unpack fileName unpackDest namespace unpackOptions
