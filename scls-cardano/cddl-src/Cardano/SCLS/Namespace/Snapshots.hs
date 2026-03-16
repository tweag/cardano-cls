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
    [str| Value maybe be one of the following:
         |  - Coin value
         |  - Keyhash of an delegation address
         |]
    $ "snapshot_out"
      =:= arr [0, a coin]
      / arr [1, a keyhash28]

relay :: Rule
relay =
  "relay"
    =:= sarr [0, a single_host_addr]
    / sarr [1, a single_host_name]
    / sarr [2, a multi_host_name]

single_host_addr :: GroupDef
single_host_addr =
  comment [str| A single host address relay |] $
    "single_host_addr"
      =:~ grp
        [ a (port / VNil)
        , a (ipv4 / VNil)
        , a (ipv6 / VNil)
        ]

single_host_name :: GroupDef
single_host_name =
  "single_host_name"
    =:~ grp [a (port / VNil), a dns_name]

multi_host_name :: GroupDef
multi_host_name =
  "multi_host_name" =:~ grp [a dns_name]
