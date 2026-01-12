{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.SCLS.Util.Info (InfoCmd (..), runInfoCmd) where

import Cardano.SCLS.CDDL
import Cardano.SCLS.NamespaceSymbol (KnownSpec (namespaceSpec), SomeNamespaceSymbol (SomeNamespaceSymbol), toString)
import Cardano.SCLS.Util.Result
import Codec.CBOR.Cuddle.CDDL (CDDL)
import Codec.CBOR.Cuddle.Huddle qualified as Cuddle
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (mapIndex))
import Codec.CBOR.Cuddle.Pretty (PrettyStage)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger
import Data.Foldable (for_)
import Data.Text qualified as T
import Prettyprinter (pretty)
import Prettyprinter.Render.Text (hPutDoc)
import System.IO

data InfoCmd
  = Namespaces
  | CDDL T.Text

runInfoCmd :: (MonadIO m, MonadLogger m) => InfoCmd -> m Result
runInfoCmd = \case
  Namespaces -> do
    liftIO $ for_ namespaces (putStrLn . toString)
    return Ok
  CDDL namespace -> do
    case namespaceSymbolFromText namespace of
      Nothing -> do
        logErrorN $ "Unknown namespace: " <> namespace
        return OtherError
      Just (SomeNamespaceSymbol p) -> do
        let cddl :: CDDL PrettyStage = mapIndex $ Cuddle.toCDDLNoRoot (namespaceSpec p)
        let outputHandle = stdout
        liftIO $ hPutDoc outputHandle (pretty cddl)
        -- Write an empty line at the end of the file
        liftIO $ hPutStrLn outputHandle ""
        return Ok
