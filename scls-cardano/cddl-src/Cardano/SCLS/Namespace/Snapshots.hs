{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Cardano.SCLS.Namespace.Snapshots where

import Cardano.SCLS.Common
import Codec.CBOR.Cuddle.Huddle
import Text.Heredoc (str)
import Prelude (($))

record_entry :: Rule
record_entry =
  comment
    [str| The key for the entry is one of the following:
        |
        | ```
        | meta:
        |   endian: be
        |
        | seq:
        |   - id: key
        |     type: snapshot
        |
        | types:
        |   snapshot:
        |     seq:
        |       - id: key_type
        |         type: u1
        |       - id: keyhash
        |         size: 29
        |         type:
        |           switch-on: key_type
        |           cases:
        |             0: credential
        |             1: keyhash
        |       - id: value_type
        |         type: u1
        |         enum: snapshot_value
        |
        |   credential:
        |     seq:
        |       - id: cred_data
        |         size: 28
        |
        |   keyhash:
        |     seq:
        |       - id: keyhash_data
        |         size: 28
        |       - id: dummy
        |         type: u1
        |         const: 0
        |
        | enums:
        |   snapshot_value:
        |     0: coin
        |     1: address
        | ```
        |]
    $ "record_entry" =:= snapshot_out

record_key :: Rule
record_key =
  "record_key"
    =:= credential
    / keyhash32

snapshot_out :: Rule
snapshot_out =
  comment
    [str| Value may be one of the following:
         |  - Coin value
         |  - Keyhash of a delegation address
         |]
    $ "snapshot_out"
      =:= arr [0, a coin]
      / arr [1, a keyhash28]
