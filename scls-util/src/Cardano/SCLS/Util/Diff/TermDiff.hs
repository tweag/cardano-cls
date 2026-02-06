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
import Control.Monad (replicateM)
import Control.Monad.Loops (whileJust)
import Control.Monad.Trans.State.Strict
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.Text qualified as T
import Data.Tree (Tree (Node))
import Data.Word (Word64, Word8)
import GHC.Stack (HasCallStack)

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

chunksOf :: Int -> ByteString -> [(Word, ByteString)]
chunksOf n = go 0
 where
  go !offset bs
    | BS.null bs = []
    | otherwise =
        let (chunk, rest) = BS.splitAt n bs
         in (offset, chunk) : go (offset + fromIntegral n) rest

termToTree :: (HasCallStack) => FlatTerm -> Tree Token
termToTree [] = error "Unexpected end of tokens"
termToTree term = case runState (go "Unexpected end of tokens") term of
  (tree, []) -> tree
  _ -> error "Unexpected tokens"
 where
  go s = getToken s >>= convert
  getToken s =
    get >>= \case
      [] -> error s
      (t : ts) -> put ts >> return t
  ensureLength n = replicateM n (go "Unexpected end of tokens in fixed-length structure")
  consumeUntilBreak :: Token -> State FlatTerm (Tree Token)
  consumeUntilBreak k =
    Node k
      <$> whileJust
        do
          getToken "Unexpected end of tokens in indefinite-length structure" <&> \case
            TkBreak -> Nothing
            t -> Just t
        do convert
  convert :: TermToken -> State FlatTerm (Tree Token)
  convert (TkInt n) = return $ Node (TInt n) []
  convert (TkInteger n) = return $ Node (TInteger n) []
  convert (TkBytes bs) = return $ Node (TBytesLen (fromIntegral $ BS.length bs)) (map (flip Node [] . uncurry TBytes) (chunksOf 16 bs))
  convert (TkString s) = return $ Node (TStringLen (fromIntegral $ T.length s)) [Node (TString s) []]
  convert TkBytesBegin = consumeUntilBreak TBytesBegin
  convert TkStringBegin = consumeUntilBreak TStringBegin
  convert (TkListLen n) = Node (TListLen n) <$> ensureLength (fromIntegral n)
  convert TkListBegin = consumeUntilBreak TListBegin
  convert (TkMapLen n) = Node (TMapLen n) <$> ensureLength (fromIntegral n * 2)
  convert TkMapBegin = consumeUntilBreak TMapBegin
  convert TkBreak = error "Unexpected break token"
  convert (TkTag n) = Node (TTag n) <$> fmap (: []) (go "Expected token after tag")
  convert (TkBool b) = return $ Node (TBool b) []
  convert TkNull = return $ Node TNull []
  convert (TkSimple n) = return $ Node (TSimple n) []
  convert (TkFloat16 f) = return $ Node (TFloat16 f) []
  convert (TkFloat32 f) = return $ Node (TFloat32 f) []
  convert (TkFloat64 f) = return $ Node (TFloat64 f) []
