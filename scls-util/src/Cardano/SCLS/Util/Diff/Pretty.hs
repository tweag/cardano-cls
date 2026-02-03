{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.SCLS.Util.Diff.Pretty (
  ppDiffEditTree,
  ppToken,
  ppTokenTree,
  ppEditDiffEntry,
) where

import Cardano.SCLS.Util.Diff.Entry (DiffEntry (..))
import Cardano.SCLS.Util.Diff.TermDiff (Token (..))
import Cardano.Types.Namespace (Namespace)
import Cardano.Types.Namespace qualified as Namespace
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Tree (Tree (Node))
import Data.TreeDiff (Edit (..))
import Data.TreeDiff.Tree (EditTree (EditNode))
import Prettyprinter (Doc, Pretty (pretty), annotate, emptyDoc, nest, vsep, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle, Color (Green, Red), color)
import Text.Printf (printf)

ppIns :: Doc AnsiStyle -> Doc AnsiStyle
ppIns = \d -> annotate (color Green) (pretty '+' <+> d)
ppDel :: Doc AnsiStyle -> Doc AnsiStyle
ppDel = \d -> annotate (color Red) (pretty '-' <+> d)

ppEditDiffEntry :: DiffEntry -> Doc AnsiStyle
ppEditDiffEntry (CpyNamespace ns) =
  pretty $ Namespace.asText ns
ppEditDiffEntry (InsNamespace ns) =
  ppIns (pretty $ Namespace.asText ns)
ppEditDiffEntry (DelNamespace ns) =
  ppDel (pretty $ Namespace.asText ns)
ppEditDiffEntry (CpyKey ns key) =
  pretty $ renderKeyRef ns key
ppEditDiffEntry (InsKey ns key) =
  ppIns (pretty $ renderKeyRef ns key)
ppEditDiffEntry (DelKey ns key) =
  ppDel (pretty $ renderKeyRef ns key)
ppEditDiffEntry (SwpValue ns key diff) =
  nest 2 $ vsep [pretty (renderKeyRef ns key), ppDiffEditTree diff]

ppOffset :: Word -> Doc ann
ppOffset offset = pretty (printf "0x%04x" offset :: String)

ppByteString :: ByteString -> Doc ann
ppByteString = pretty . decodeUtf8 . Base16.encode

renderKeyRef :: Namespace -> ByteString -> Text
renderKeyRef ns key =
  Namespace.asText ns <> "/" <> decodeUtf8 (Base16.encode key)

ppToken :: Token -> Doc ann
ppToken (TInt n) = "int(" <> pretty n <> ")"
ppToken (TInteger n) = "integer(" <> pretty n <> ")"
ppToken (TBytesLen n) = "bytes(" <> pretty n <> ")"
ppToken (TString s) = "string(" <> pretty s <> ")"
ppToken (TStringLen n) = "string(" <> pretty n <> ")"
ppToken (TByteOffset n) = "@ " <> pretty (printf "0x%04x" n :: String)
ppToken (TBytes offset bs) = nest 2 $ vsep ["@ " <> ppOffset offset, ppByteString bs]
ppToken TNull = "null"
ppToken (TBool b) = "bool(" <> pretty b <> ")"
ppToken (TBreak) = "break"
ppToken (TBytesBegin) = "bytes"
ppToken (TStringBegin) = "string"
ppToken (TListBegin) = "list"
ppToken (TMapBegin) = "map"
ppToken (TListLen n) = "list(" <> pretty n <> ")"
ppToken (TMapLen n) = "map(" <> pretty n <> ")"
ppToken (TTag n) = "tag(" <> pretty n <> ")"
ppToken (TSimple n) = "simple(" <> pretty n <> ")"
ppToken (TFloat16 f) = "float16(" <> pretty f <> ")"
ppToken (TFloat32 f) = "float32(" <> pretty f <> ")"
ppToken (TFloat64 f) = "float64(" <> pretty f <> ")"

ppTokenTree :: Tree Token -> Doc ann
ppTokenTree (Node t subtrees) =
  vsep [ppToken t, nest 2 $ vsep $ map ppTokenTree subtrees]

ppEditTree :: (EditTree Token) -> Doc AnsiStyle
ppEditTree (EditNode t@(TBytesLen _) edits) =
  nest 2 $ vsep $ ppToken t : map ppDiffEditTreeBytes edits
ppEditTree (EditNode t@TBytesBegin edits) =
  nest 2 $ vsep $ ppToken t : map ppDiffEditTreeBytes edits
ppEditTree (EditNode t edits) =
  nest 2 $ vsep $ ppToken t : map ppDiffEditTree edits

ppDiffEditTreeBytes :: Edit (EditTree Token) -> Doc AnsiStyle
ppDiffEditTreeBytes (Cpy _) = emptyDoc
ppDiffEditTreeBytes t = ppDiffEditTree t

ppDiffEditTree :: Edit (EditTree Token) -> Doc AnsiStyle
ppDiffEditTree = \case
  Cpy t -> ppEditTree t
  Ins t -> ppIns $ ppEditTree t
  Del t -> ppDel $ ppEditTree t
  Swp (EditNode (TBytes offset1 bs1) []) (EditNode (TBytes offset2 b2) [])
    | offset1 == offset2 ->
        nest 2 $ vsep ["@ " <> ppOffset offset1, ppDel (ppByteString bs1), ppIns (ppByteString b2)]
  Swp t1 t2 -> vsep [ppDiffEditTree (Del t1), ppDiffEditTree (Ins t2)]
