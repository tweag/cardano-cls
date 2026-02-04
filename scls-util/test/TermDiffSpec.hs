{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module TermDiffSpec (termDiffTests) where

import Cardano.SCLS.Util.Diff.TermDiff (Token (..), termToTree)
import Codec.CBOR.FlatTerm (TermToken (..))
import Control.Exception (evaluate)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Tree (Tree (Node))
import Test.Hspec

termDiffTests :: Spec
termDiffTests = do
  describe "termToTree" do
    describe "leaf/scalar tokens" do
      it "single int" do
        termToTree [TkInt 42] `shouldBe` Node (TInt 42) []

      it "single integer" do
        termToTree [TkInteger 999999] `shouldBe` Node (TInteger 999999) []

      it "single bool" do
        termToTree [TkBool True] `shouldBe` Node (TBool True) []

      it "single null" do
        termToTree [TkNull] `shouldBe` Node TNull []

      it "tagged value" do
        termToTree [TkTag 24] `shouldBe` Node (TTag 24) []

      it "simple value" do
        termToTree [TkSimple 0] `shouldBe` Node (TSimple 0) []

      it "float 16" do
        termToTree [TkFloat16 1.5] `shouldBe` Node (TFloat16 1.5) []

      it "float 32" do
        termToTree [TkFloat32 3.14] `shouldBe` Node (TFloat32 3.14) []

      it "float 64" do
        termToTree [TkFloat64 2.718] `shouldBe` Node (TFloat64 2.718) []

    describe "byte strings and text strings" do
      it "bytes empty" do
        termToTree [TkBytes ""]
          `shouldBe` Node (TBytesLen 0) []

      it "bytes short (<=16 bytes)" do
        let bs = BS.pack [0 .. 4] :: ByteString
        termToTree [TkBytes bs]
          `shouldBe` Node (TBytesLen 5) [Node (TBytes 0 bs) []]
      it "bytes long (>16 bytes, exact multiple)" do
        let bs = BS.pack [0 .. 31] :: ByteString
        termToTree [TkBytes bs]
          `shouldBe` Node
            (TBytesLen 32)
            [ Node (TBytes 0 (BS.pack [0 .. 15])) []
            , Node (TBytes 16 (BS.pack [16 .. 31])) []
            ]

      it "bytes long (>16 bytes, not multiple of 16)" do
        let bs = BS.pack [0 .. 19] :: ByteString
        termToTree [TkBytes bs]
          `shouldBe` Node
            (TBytesLen 20)
            [ Node (TBytes 0 (BS.pack [0 .. 15])) []
            , Node (TBytes 16 (BS.pack [16 .. 19])) []
            ]

      it "bytes indefinite-length" do
        termToTree [TkBytesBegin, TkBytes "ab", TkBytes "cd", TkBreak]
          `shouldBe` Node
            TBytesBegin
            [ Node (TBytesLen 2) [Node (TBytes 0 "ab") []]
            , Node (TBytesLen 2) [Node (TBytes 0 "cd") []]
            ]

      it "string" do
        let str = "hello"
        termToTree [TkString str]
          `shouldBe` Node (TStringLen 5) [Node (TString str) []]

      it "string indefinite-length" do
        termToTree [TkStringBegin, TkString "ab", TkString "cd", TkBreak]
          `shouldBe` Node
            TStringBegin
            [ Node (TStringLen 2) [Node (TString "ab") []]
            , Node (TStringLen 2) [Node (TString "cd") []]
            ]

    describe "fixed-length lists" do
      it "empty list" do
        termToTree [TkListLen 0] `shouldBe` Node (TListLen 0) []

      it "single-element list" do
        termToTree [TkListLen 1, TkInt 42]
          `shouldBe` Node (TListLen 1) [Node (TInt 42) []]

      it "multi-element list" do
        termToTree [TkListLen 3, TkInt 1, TkInt 2, TkInt 3]
          `shouldBe` Node
            (TListLen 3)
            [ Node (TInt 1) []
            , Node (TInt 2) []
            , Node (TInt 3) []
            ]

    describe "indefinite-length lists" do
      it "empty" do
        termToTree [TkListBegin, TkBreak] `shouldBe` Node TListBegin []

      it "single-element" do
        termToTree [TkListBegin, TkInt 42, TkBreak]
          `shouldBe` Node TListBegin [Node (TInt 42) []]

      it "multi-element" do
        termToTree [TkListBegin, TkInt 1, TkInt 2, TkBreak]
          `shouldBe` Node
            TListBegin
            [ Node (TInt 1) []
            , Node (TInt 2) []
            ]

    describe "fixed-length maps" do
      it "empty map" do
        termToTree [TkMapLen 0] `shouldBe` Node (TMapLen 0) []

      it "single pair" do
        termToTree [TkMapLen 1, TkString "k", TkInt 1]
          `shouldBe` Node
            (TMapLen 1)
            [ Node (TStringLen 1) [Node (TString "k") []]
            , Node (TInt 1) []
            ]

      it "two pairs" do
        termToTree [TkMapLen 2, TkString "a", TkInt 1, TkString "b", TkInt 2]
          `shouldBe` Node
            (TMapLen 2)
            [ Node (TStringLen 1) [Node (TString "a") []]
            , Node (TInt 1) []
            , Node (TStringLen 1) [Node (TString "b") []]
            , Node (TInt 2) []
            ]

    describe "indefinite-length maps" do
      it "empty" do
        termToTree [TkMapBegin, TkBreak] `shouldBe` Node TMapBegin []

      it "single pair" do
        termToTree [TkMapBegin, TkString "k", TkInt 1, TkBreak]
          `shouldBe` Node
            TMapBegin
            [ Node (TStringLen 1) [Node (TString "k") []]
            , Node (TInt 1) []
            ]

    describe "indefinite-length byte/string streams" do
      it "bytes stream" do
        termToTree [TkBytesBegin, TkBytes "ab", TkBytes "cd", TkBreak]
          `shouldBe` Node
            TBytesBegin
            [ Node (TBytesLen 2) [Node (TBytes 0 "ab") []]
            , Node (TBytesLen 2) [Node (TBytes 0 "cd") []]
            ]

      it "string stream" do
        termToTree [TkStringBegin, TkString "ab", TkString "cd", TkBreak]
          `shouldBe` Node
            TStringBegin
            [ Node (TStringLen 2) [Node (TString "ab") []]
            , Node (TStringLen 2) [Node (TString "cd") []]
            ]

    describe "nesting" do
      it "list in list (fixed)" do
        termToTree [TkListLen 1, TkListLen 2, TkInt 1, TkInt 2]
          `shouldBe` Node
            (TListLen 1)
            [ Node
                (TListLen 2)
                [ Node (TInt 1) []
                , Node (TInt 2) []
                ]
            ]

      it "map in map (fixed)" do
        termToTree [TkMapLen 2, TkInt 1, TkMapLen 2, TkInt 2, TkInt 1, TkInt 3, TkInt 4, TkInt 5, TkInt 6]
          `shouldBe` Node
            (TMapLen 2)
            [ Node
                (TInt 1)
                []
            , Node
                (TMapLen 2)
                [ Node (TInt 2) []
                , Node (TInt 1) []
                , Node (TInt 3) []
                , Node (TInt 4) []
                ]
            , Node
                (TInt 5)
                []
            , Node
                (TInt 6)
                []
            ]

      it "indef inside fixed" do
        termToTree [TkListLen 1, TkListBegin, TkInt 1, TkBreak]
          `shouldBe` Node
            (TListLen 1)
            [Node TListBegin [Node (TInt 1) []]]

      it "fixed inside indef" do
        termToTree [TkListBegin, TkListLen 1, TkInt 1, TkBreak]
          `shouldBe` Node
            TListBegin
            [Node (TListLen 1) [Node (TInt 1) []]]

      it "map containing list" do
        termToTree [TkMapLen 1, TkString "k", TkListLen 2, TkInt 1, TkInt 2]
          `shouldBe` Node
            (TMapLen 1)
            [ Node (TStringLen 1) [Node (TString "k") []]
            , Node
                (TListLen 2)
                [ Node (TInt 1) []
                , Node (TInt 2) []
                ]
            ]

      it "list containing map" do
        termToTree [TkListLen 1, TkMapLen 1, TkInt 1, TkInt 2]
          `shouldBe` Node
            (TListLen 1)
            [ Node
                (TMapLen 1)
                [ Node (TInt 1) []
                , Node (TInt 2) []
                ]
            ]

      it "deeply nested" do
        termToTree [TkListLen 1, TkListLen 1, TkListLen 1, TkInt 42]
          `shouldBe` Node
            (TListLen 1)
            [ Node
                (TListLen 1)
                [ Node
                    (TListLen 1)
                    [Node (TInt 42) []]
                ]
            ]

    describe "error cases" do
      it "empty input" do
        evaluate (termToTree []) `shouldThrow` errorCall "Unexpected end of tokens"

      it "leftover tokens" do
        evaluate (termToTree [TkInt 1, TkInt 2])
          `shouldThrow` errorCall "Unexpected tokens"

      it "truncated fixed-length" do
        evaluate (termToTree [TkListLen 3, TkInt 1])
          `shouldThrow` errorCall "Unexpected end of tokens in fixed-length structure"

      it "truncated indefinite" do
        evaluate (termToTree [TkListBegin, TkInt 1])
          `shouldThrow` errorCall "Unexpected end of tokens in indefinite-length structure"
