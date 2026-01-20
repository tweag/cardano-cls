{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Cardano.SCLS.Namespace.GovConstitution where

import Cardano.SCLS.Common
import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Text.Heredoc (str)

record_entry :: Rule
record_entry =
  comment
    [str| Constitution record entry
        |
        | ```
        | meta:
        |   endian: be
        |
        | seq:
        |   - id: key
        |     type: gov_constitution
        |
        | gov_constitution:
        |   seq:
        |     - id: epoch
        |       doc: Current epoch.
        |       type: u8
        | ```
        |]
    $ "record_entry" =:= constitution

constitution :: Rule
constitution =
  comment
    [str| address of the constition
        |]
    $ "constitution"
      =:= arr
        [ a anchor
        , a (script_hash / VNil)
        ]
