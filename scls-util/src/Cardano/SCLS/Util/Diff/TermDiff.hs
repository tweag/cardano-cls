{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.SCLS.Util.Diff.TermDiff (
  termToTree,
  Token (..),
) where

import Codec.CBOR.FlatTerm (FlatTerm, TermToken (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Tree (Tree (Node))
import Data.Word (Word64, Word8)

data Token
  = TInt !Int
  | TInteger !Integer
  | TBytesLen !Word
  | TByteOffset !Word
  | TBytes !Word !ByteString
  | TBytesBegin
  | TStringLen !Word
  | TString !T.Text
  | TStringBegin
  | TListLen !Word
  | TListBegin
  | TMapLen !Word
  | TMapBegin
  | TBreak
  | TTag !Word64
  | TBool !Bool
  | TNull
  | TSimple !Word8
  | TFloat16 !Float
  | TFloat32 !Float
  | TFloat64 !Double
  deriving (Eq, Show)

termTokenToDiffTreeTokenNode :: TermToken -> Tree Token
termTokenToDiffTreeTokenNode (TkInt n) = Node (TInt n) []
termTokenToDiffTreeTokenNode (TkInteger n) = Node (TInteger n) []
termTokenToDiffTreeTokenNode (TkBytes bs) = Node (TBytesLen (fromIntegral $ BS.length bs)) (map (flip Node [] . uncurry TBytes) (chunksOf 16 bs))
termTokenToDiffTreeTokenNode (TkString s) = Node (TStringLen (fromIntegral $ T.length s)) [Node (TString s) []]
termTokenToDiffTreeTokenNode TkBytesBegin = Node TBytesBegin []
termTokenToDiffTreeTokenNode TkStringBegin = Node TStringBegin []
termTokenToDiffTreeTokenNode (TkListLen n) = Node (TListLen n) []
termTokenToDiffTreeTokenNode TkListBegin = Node TListBegin []
termTokenToDiffTreeTokenNode (TkMapLen n) = Node (TMapLen n) []
termTokenToDiffTreeTokenNode TkMapBegin = Node TMapBegin []
termTokenToDiffTreeTokenNode TkBreak = Node TBreak []
termTokenToDiffTreeTokenNode (TkTag n) = Node (TTag n) []
termTokenToDiffTreeTokenNode (TkBool b) = Node (TBool b) []
termTokenToDiffTreeTokenNode TkNull = Node TNull []
termTokenToDiffTreeTokenNode (TkSimple n) = Node (TSimple n) []
termTokenToDiffTreeTokenNode (TkFloat16 f) = Node (TFloat16 f) []
termTokenToDiffTreeTokenNode (TkFloat32 f) = Node (TFloat32 f) []
termTokenToDiffTreeTokenNode (TkFloat64 f) = Node (TFloat64 f) []

chunksOf :: Int -> ByteString -> [(Word, ByteString)]
chunksOf n = go [] 0
 where
  go acc offset bs
    | BS.null chunk = reverse acc
    | otherwise = go ((offset, chunk) : acc) (offset + fromIntegral (BS.length chunk)) rest
   where
    (chunk, rest) = BS.splitAt n bs

termToTree :: FlatTerm -> Tree Token
termToTree [] = error "Unexpected end of tokens"
termToTree (xt : ts) = go xt restEmptyOrError [] ts
 where
  go TkListBegin =
    lenIndef TListBegin
  go (TkListLen len) =
    fixedLength (TListLen len) len
  go TkBytesBegin =
    lenIndef TBytesBegin
  go TkStringBegin =
    lenIndef TStringBegin
  go TkMapBegin =
    lenIndef TMapBegin
  go (TkMapLen len) =
    fixedLength (TMapLen len) len
  go t = \f _ xs ->
    f (termTokenToDiffTreeTokenNode t) xs

  restEmptyOrError t = \case
    [] -> t
    (_ : _) -> error $ "Unexpected tokens"

lenIndef :: Token -> (Tree Token -> [TermToken] -> t) -> [Tree Token] -> [TermToken] -> t
lenIndef token f acc = \case
  [] -> error "Unexpected end of tokens in indefinite-length structure"
  TkBreak : xs ->
    f (Node token (reverse acc)) xs
  TkListBegin : xs ->
    lenIndef TListBegin cont [] xs
  TkBytesBegin : xs ->
    lenIndef TBytesBegin cont [] xs
  TkStringBegin : xs ->
    lenIndef TStringBegin cont [] xs
  TkMapBegin : xs ->
    lenIndef TMapBegin cont [] xs
  TkListLen len : xs ->
    fixedLength (TListLen len) len cont [] xs
  TkMapLen len : xs ->
    fixedLength (TMapLen len) len cont [] xs
  t : xs ->
    lenIndef token f (termTokenToDiffTreeTokenNode t : acc) xs
 where
  cont = lenIndef token f . flip (:) acc

fixedLength :: Token -> Word -> (Tree Token -> [TermToken] -> t) -> [Tree Token] -> [TermToken] -> t
fixedLength token@(TMapLen _) len f acc
  | length acc == fromIntegral len * 2 =
      f (Node token (reverse acc))
fixedLength token@(TListLen _) len f acc
  | length acc == fromIntegral len =
      f (Node token (reverse acc))
fixedLength token len f acc = \case
  [] -> error "Unexpected end of tokens in fixed-length structure"
  TkListBegin : xs ->
    lenIndef TListBegin cont [] xs
  TkBytesBegin : xs ->
    lenIndef TBytesBegin cont [] xs
  TkStringBegin : xs ->
    lenIndef TStringBegin cont [] xs
  TkMapBegin : xs ->
    lenIndef TMapBegin cont [] xs
  TkListLen l : xs ->
    fixedLength (TListLen l) l cont [] xs
  TkMapLen l : xs ->
    fixedLength (TMapLen l) l cont [] xs
  t : xs ->
    fixedLength token len f (termTokenToDiffTreeTokenNode t : acc) xs
 where
  cont = fixedLength token len f . flip (:) acc
