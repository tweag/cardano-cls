{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Cardano.SCLS.Namespace.EntitiesStakePools where

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
        |   - id: keyhash_stakepool
        |         doc: keyhash of the stake pool
        |         size: 28
        | ```
        |]
    $ "record_entry" =:= stake_pool

stake_pool :: Rule
stake_pool =
  "stake_pool"
    =:= mp
      [ "stake_pool_state" ==> stake_pool_state / VNil
      , "retiring_epoch_no" ==> epoch_no / VNil
      ]

stake_pool_state :: Rule
stake_pool_state =
  "stake_pool_state"
    =:= mp
      [ "vrf" ==> vrf_keyhash
      , "cost" ==> coin
      , "margin" ==> unit_interval
      , "owners" ==> set staking_keyhash
      , "pledge" ==> coin
      , "relays" ==> arr [0 <+ a relay]
      , "deposit" ==> coin
      , "metadata" ==> pool_metadata / VNil
      , "account_id" ==> account_id
      , "delegators" ==> set credential
      ]

relay :: Rule
relay =
  "relay"
    =:= arr
      [ 0
      , a (port / VNil)
      , a (ipv4 / VNil)
      , a (ipv6 / VNil)
      ]
    / arr [1, a (port / VNil), a dns_name]
    / arr [2, a dns_name]

pool_metadata :: Rule
pool_metadata =
  "pool_metadata"
    =:= arr
      [a url, a VBytes]
