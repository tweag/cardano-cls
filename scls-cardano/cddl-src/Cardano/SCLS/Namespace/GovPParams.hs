{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Cardano.SCLS.Namespace.GovPParams where

import Cardano.SCLS.Common
import Codec.CBOR.Cuddle.Huddle
import Data.Word (Word64)
import Text.Heredoc (str)
import Prelude (Integer, ($))

record_entry :: Rule
record_entry =
  comment
    [str| Specification for parameters
        |
        | ```
        | meta:
        |   endian: be
        |
        | seq:
        |   - id: key
        |     type: gov_pparams
        |
        | types:
        |   gov_pparams:
        |     seq:
        |       - id: value
        |         type: strz
        |         size: 4
        |         encoding: ASCII
        |       - id: valiant
        |         size: 0
        |         type:
        |           switch-on: value
        |           cases:
        |             '"prev"': kprev
        |             '"curr"': kcurr
        |             '"fut0"': kfut0
        |             '"fut1"': kfut1
        |   kprev:
        |     doc: previous values
        |   kcurr:
        |     doc: current values
        |   kfut0:
        |     doc: possible future
        |   kfut1:
        |     doc: definitive future
        | ```
        |
        | fut0 with missig parameters should be omitted.
        |]
    $ "record_entry" =:= gov_pparams_out

gov_pparams_out :: Rule
gov_pparams_out =
  comment
    [str| Governance protocol parameters
              |   - min_fee_a: the linear factor for the minimum fee calculation
              |   - min_fee_b: the constant factor for the minimum fee calculation
              |   - max_block_size: maximal block body size in bytes
              |   - max_block_header_size: maximal block header size in bytes
              |   - max_tx_size: maximal transaction size in bytes
              |   - key_deposit: The amount of a key registration deposit
              |   - pool_deposit: The amount of a pool registration deposit
              |   - epoch_max: epoch bound on pool retirement
              |   - n_opt: desired number of pools
              |   - a0: pool influence factor
              |   - rho: monetary expansion
              |   - tau: treasury expansion
              |   - d: decentralisation parameter
              |   - min_pool_cost: minimum pool cost
              |   - min_utxo_value: Minimum Lovelace in a UTxO deprecated by AdaPerUTxOWord
              |   - ada_per_utxo_byte: Cost in ada per 1 byte of UTxO storage instead of _coinsPerUTxOWord
              |   - cost_models: Cost models for non-native script languages
              |   - prices: Prices of execution units for non-native script languages
              |   - max_tx_ex_units: Max total script execution resources units allowed per tx
              |   - max_block_ex_units: Max total script execution resources units allowed per block
              |   - max_value_size: Max size of a Value in an output
              |   - collateral_percentage: The scaling percentage of the collateral relative to the fee
              |   - max_collateral_inputs: Maximum number of collateral inputs allowed in a transaction
              |   - min_fee_ref_script_cost_per_byte: Reference scripts fee for the minimum fee calculation
              |]
    $ "gov_pparams_out"
      =:= mp
        [ "a0" ==> nonnegative_interval
        , "rho" ==> unit_interval
        , "tau" ==> unit_interval
        , "n_opt" ==> VUInt `sized` (2 :: Word64)
        , "prices" ==> ex_unit_prices
        , "epoch_max" ==> epoch_interval
        , "min_fee_a" ==> coin
        , "min_fee_b" ==> coin
        , "cost_models" ==> cost_models
        , "key_deposit" ==> coin
        , "max_tx_size" ==> VUInt `sized` (4 :: Word64)
        , "drep_deposit" ==> coin
        , "max_val_size" ==> VUInt `sized` (4 :: Word64)
        , "pool_deposit" ==> coin
        , "drep_activity" ==> epoch_interval
        , "min_pool_cost" ==> coin
        , "max_block_size" ==> VUInt `sized` (4 :: Word64)
        , "max_tx_ex_units" ==> ex_units
        , "protocol_version" ==> protocol_version
        , "coin_per_utxo_byte" ==> coin
        , "gov_action_deposit" ==> coin
        , "max_block_ex_units" ==> ex_units
        , "min_committee_size" ==> VUInt `sized` (2 :: Word64)
        , "gov_action_lifetime" ==> epoch_interval
        , "committee_term_limit" ==> epoch_interval
        , "collateral_percentage" ==> VUInt `sized` (2 :: Word64)
        , "max_block_header_size" ==> VUInt `sized` (2 :: Word64)
        , "max_collateral_inputs" ==> VUInt `sized` (2 :: Word64)
        , "drep_voting_thresholds" ==> drep_voting_thresholds
        , "pool_voting_thresholds" ==> pool_voting_thresholds
        , "min_fee_ref_script_cost_per_byte" ==> (nonnegative_interval / VNil)
        ]

epoch_interval :: Rule
epoch_interval = "epoch_interval" =:= VUInt `sized` (4 :: Word64)

cost_models :: Rule
cost_models =
  comment
    [str|The format for cost_models is flexible enough to allow adding
        |Plutus built-ins and language versions in the future.
        |
        |Plutus v1: only 166 integers are used, but more are accepted (and ignored)
        |Plutus v2: only 175 integers are used, but more are accepted (and ignored)
        |Plutus v3: only 223 integers are used, but more are accepted (and ignored)
        |
        |Any 8-bit unsigned number can be used as a key.
        |]
    $ "cost_models"
      =:= mp
        [ opt $ idx 0 ==> arr [0 <+ a int64]
        , opt $ idx 1 ==> arr [0 <+ a int64]
        , opt $ idx 2 ==> arr [0 <+ a int64]
        , 0 <+ asKey ((3 :: Integer) ... (255 :: Integer)) ==> arr [0 <+ a int64]
        ]

ex_unit_prices :: Rule
ex_unit_prices =
  "ex_unit_prices"
    =:= arr
      [ "mem_price" ==> nonnegative_interval
      , "step_price" ==> nonnegative_interval
      ]

ex_units :: Rule
ex_units = "ex_units" =:= arr ["mem" ==> VUInt, "steps" ==> VUInt]

pool_voting_thresholds :: Rule
pool_voting_thresholds =
  comment
    [str|
    | 0 - motion no confidence
    | 1 - committee normal
    | 2 - committee no confidence
    | 3 - hard fork initiation
    | 4 - security relevant parameter voting threshold
    |]
    $ "pool_voting_thresholds"
      =:= arr
        [ a unit_interval -- motion no confidence
        , a unit_interval -- committee normal
        , a unit_interval -- committee no confidence
        , a unit_interval -- hard fork initiation
        , a unit_interval -- security relevant parameter voting threshold
        ]

drep_voting_thresholds :: Rule
drep_voting_thresholds =
  comment
    [str|
    | 0 - motion no confidence
    | 1 - committee normal
    | 2 - committee no confidence
    | 3 - update constitution
    | 4 - hard fork initiation
    | 5 - PP network group
    | 6 - PP economic group
    | 7 - PP technical group
    | 8 - PP governance group
    | 9 - treasury withdrawal
    |]
    $ "drep_voting_thresholds"
      =:= arr
        [ a unit_interval -- motion no confidence
        , a unit_interval -- committee normal
        , a unit_interval -- committee no confidence
        , a unit_interval -- update constitution
        , a unit_interval -- hard fork initiation
        , a unit_interval -- PP network group
        , a unit_interval -- PP economic group
        , a unit_interval -- PP technical group
        , a unit_interval -- PP governance group
        , a unit_interval -- treasury withdrawal
        ]
