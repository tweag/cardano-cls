module Cardano.SCLS.Util.Diff.Entry (
  DiffEntry (..),
) where

import Cardano.SCLS.Util.Diff.TermDiff (Token)
import Cardano.Types.Namespace (Namespace)
import Data.ByteString (ByteString)
import Data.TreeDiff.Tree (Edit, EditTree)

data DiffEntry
  = CpyNamespace Namespace
  | InsNamespace Namespace
  | DelNamespace Namespace
  | CpyKey Namespace ByteString
  | InsKey Namespace ByteString
  | DelKey Namespace ByteString
  | SwpValue Namespace ByteString (Edit (EditTree Token))
  deriving (Show)
