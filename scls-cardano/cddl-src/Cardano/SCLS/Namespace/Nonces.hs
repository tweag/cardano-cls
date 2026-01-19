{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Cardano.SCLS.Namespace.Nonces where

import Cardano.SCLS.Common
import Codec.CBOR.Cuddle.Huddle
import Text.Heredoc (str)
import Prelude (($))

record_entry :: Rule
record_entry =
  comment
    [str| Key is just zero
        |
        | ```
        | meta:
        |   endian: be
        |
        | seq:
        |   - id: key
        |     type: nonce
        |
        | types:
        |   nonce:
        |     doc: Nonce value
        |     size: 1
        |     type: u1
        |     const: 0
        | ```
        |
        |]
    $ "record_entry" =:= nonces

nonces :: Rule
nonces =
  comment
    [str| List of nonces used in the protocol state
        |
        |]
    $ "nonces"
      =:= mp
        [ "lab_nonce" ==> nonce
        , "last_slot" ==> with_origin slot_no
        , "epoch_nonce" ==> nonce
        , "cert_counters" ==> mp [0 <+ asKey keyhash28 ==> word64]
        , "evolving_nonce" ==> nonce
        , "candidate_nonce" ==> nonce
        , "last_epoch_block_nonce" ==> nonce
        ]

nonce :: Rule
nonce = "nonce" =:= neutral_nonce / just_nonce

neutral_nonce :: Rule
neutral_nonce = "neutral_nonce" =:= arr [0]

just_nonce :: Rule
just_nonce = "just_nonce" =:= arr [1, a hash32]

with_origin :: (IsType0 t0) => t0 -> GRuleCall
with_origin = binding $ \x -> "with_origin" =:= arr [0] / arr [1, a x]
