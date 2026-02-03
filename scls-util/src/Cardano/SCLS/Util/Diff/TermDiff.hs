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

  restEmptyOrError t [] = t
  restEmptyOrError _ (_ : _) = error "Unexpected tokens"

  lenIndef _ _ _ [] = error "Unexpected end of tokens in indefinite-length structure"
  lenIndef token f acc (TkBreak : xs) = f (Node token (reverse acc)) xs
  lenIndef token f acc (TkListBegin : xs) =
    lenIndef TListBegin (lenIndef token f . flip (:) acc) [] xs
  lenIndef token f acc (TkBytesBegin : xs) =
    lenIndef TBytesBegin (lenIndef token f . flip (:) acc) [] xs
  lenIndef token f acc (TkStringBegin : xs) =
    lenIndef TStringBegin (lenIndef token f . flip (:) acc) [] xs
  lenIndef token f acc (TkMapBegin : xs) =
    lenIndef TMapBegin (lenIndef token f . flip (:) acc) [] xs
  lenIndef token f acc (TkListLen len : xs) =
    fixedLength (TListLen len) len (lenIndef token f . flip (:) acc) [] xs
  lenIndef token f acc (TkMapLen len : xs) =
    fixedLength (TMapLen len) len (lenIndef token f . flip (:) acc) [] xs
  lenIndef token f acc (t : xs) =
    lenIndef token f (termTokenToDiffTreeTokenNode t : acc) xs

  fixedLength token@(TMapLen _) len f acc xs
    | length acc == fromIntegral len * 2 =
        f (Node token (reverse acc)) xs
  fixedLength token len f acc xs
    | length acc == fromIntegral len =
        f (Node token (reverse acc)) xs
  fixedLength _ _ _ _ [] =
    error "Unexpected end of tokens in fixed-length structure"
  fixedLength token len f acc (TkListBegin : xs) =
    lenIndef TListBegin (fixedLength token len f . flip (:) acc) [] xs
  fixedLength token len f acc (TkBytesBegin : xs) =
    lenIndef TBytesBegin (fixedLength token len f . flip (:) acc) [] xs
  fixedLength token len f acc (TkStringBegin : xs) =
    lenIndef TStringBegin (fixedLength token len f . flip (:) acc) [] xs
  fixedLength token len f acc (TkMapBegin : xs) =
    lenIndef TMapBegin (fixedLength token len f . flip (:) acc) [] xs
  fixedLength token len f acc (TkListLen l : xs) =
    fixedLength (TListLen l) l (fixedLength token len f . flip (:) acc) [] xs
  fixedLength token len f acc (TkMapLen l : xs) =
    fixedLength (TMapLen l) l (fixedLength token len f . flip (:) acc) [] xs
  fixedLength token len f acc (t : xs) =
    fixedLength token len f (termTokenToDiffTreeTokenNode t : acc) xs
