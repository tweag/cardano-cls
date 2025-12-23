{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.SCLS.Util.Info (InfoCmd (..), runInfoCmd) where

import Cardano.SCLS.CDDL
import Cardano.SCLS.Util.Result
import Codec.CBOR.Cuddle.CDDL (CDDL)
import Codec.CBOR.Cuddle.Huddle qualified as Cuddle
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (mapIndex))
import Codec.CBOR.Cuddle.Pretty (PrettyStage)
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Prettyprinter (pretty)
import Prettyprinter.Render.Text (hPutDoc)
import System.IO

data InfoCmd
  = Namespaces
  | CDDL T.Text

runInfoCmd :: InfoCmd -> IO Result
runInfoCmd = \case
  Namespaces -> do
    for_ (Map.keys namespaces) (putStrLn . T.unpack)
    return Ok
  CDDL namespace -> do
    case Map.lookup namespace namespaces of
      Nothing -> do
        putStrLn $ "Unknown namespace: " ++ T.unpack namespace
        return OtherError
      Just NamespaceInfo{namespaceSpec = hddl} -> do
        let cddl :: CDDL PrettyStage = mapIndex $ Cuddle.toCDDLNoRoot hddl
        let outputHandle = stdout
        hPutDoc outputHandle (pretty cddl)
        -- Write an empty line at the end of the file
        hPutStrLn outputHandle ""
        return Ok
