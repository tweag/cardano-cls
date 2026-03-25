{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Cardano.SCLS.Namespace.GovCommittee where

import Cardano.SCLS.Common
import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Text.Heredoc (str)

record_entry :: Rule
record_entry =
  comment
    [str| The key for the namespace
        |
        | ```
        | meta:
        |   endian: be
        |
        | seq:
        |   - id: key
        |     type: gov_committee
        |
        | types:
        |   gov_committee:
        |     seq:
        |       - id: epoch
        |         doc: epoch
        |         type: u8
        | ```
        |]
    $ "record_entry" =:= committee / VNil

committee :: Rule
committee =
  comment
    [str| Storage of the committee members
        |]
    $ "committee"
      =:= arr
        [ a (mp [0 <+ asKey credential ==> epoch_no])
        , a unit_interval
        ]
