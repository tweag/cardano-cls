{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Cardano.SCLS.Namespace.Accounts where

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
        |     type: section
        |
        | types:
        |   section:
        |     seq:
        |       - id: section_type
        |         type: u1
        |         enum: accounts_section
        | enums:
        |   accounts_section:
        |     0: dstate
        |     1: pstate
        | ```
        |]
    $ "record_entry" =:= accounts_out

accounts_out :: Rule
accounts_out =
  comment
    [str| Information about accounts in the ledger state |]
    $ "accounts_out"
      =:= mp ["dstate" ==> dstate, "pstate" ==> pstate]

pstate :: Rule
pstate =
  "pstate"
    =:= mp
      [ "retiring" ==> mp [0 <+ asKey pool_keyhash ==> epoch_no]
      , "vrf_key_hashes" ==> mp [0 <+ asKey vrf_keyhash ==> word64_non_zero]
      , "future_stake_pool_params" ==> mp [0 <+ asKey pool_keyhash ==> pool_params]
      ]

dstate :: Rule
dstate =
  "dstate"
    =:= mp
      [ "delegation_map" ==> mp [0 <+ asKey vkey_genesis ==> vkey]
      , "last_delegation" ==> mp [0 <+ asKey vkey_genesis ==> slot_no]
      ]

vkey_genesis :: Rule
vkey_genesis = "vkey_genesis" =:= int64

pool_params :: Rule
pool_params =
  "pool_params"
    =:= mp
      [ "cost" ==> coin
      , "margin" ==> unit_interval
      , "pledge" ==> coin
      , "relays" ==> arr [0 <+ a relay]
      , "operator" ==> pool_keyhash
      , "pool_owners" ==> set addr_keyhash
      , "vrf_keyhash" ==> vrf_keyhash
      , "pool_metadata" ==> (pool_metadata / VNil)
      , "reward_account" ==> reward_account
      ]

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

pool_metadata :: Rule
pool_metadata = "pool_metadata" =:= arr [a url, a pool_metadata_hash]

pool_metadata_hash :: Rule
pool_metadata_hash = "pool_metadata_hash" =:= VBytes
