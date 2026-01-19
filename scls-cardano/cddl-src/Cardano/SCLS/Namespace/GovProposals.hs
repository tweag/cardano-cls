{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Cardano.SCLS.Namespace.GovProposals where

import Cardano.SCLS.Common
import Cardano.SCLS.Namespace.GovConstitution (constitution)
import Cardano.SCLS.Namespace.GovPParams
import Codec.CBOR.Cuddle.Comments ((//-))
import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Data.Word (Word64)
import Text.Heredoc (str)
import Prelude (Integer)

record_entry :: Rule
record_entry =
  comment
    [str| Size of the key
        |
        | ```
        | meta:
        |   endian: be
        |
        | seq:
        |   - id: key
        |     type: gov_proposals
        |
        | gov_proposals:
        |   seq:
        |     - id: tx_addr
        |       doc: transaction
        |       type: bytes
        |       size: 28
        |     - id: tx_idx
        |       doc: index inside transaction
        |       type: u4
        |     - id: gov_action_idx
        |       doc: governance action index
        |       type: u2
        | ```
        |
        |]
    $ "record_entry" =:= proposal

committee_cold_credential :: Rule
committee_cold_credential = "committee_cold_credential" =:= credential

proposal :: Rule
proposal =
  "proposal"
    =:= mp
      [ "drep_votes" ==> mp [0 <+ asKey credential ==> vote]
      , "proposed_in" ==> epoch_no
      , "expires_after" ==> epoch_no
      , "committee_votes" ==> mp [0 <+ asKey committee_cold_credential ==> vote]
      , "stake_pool_votes" ==> mp [0 <+ asKey pool_keyhash ==> vote]
      , "proposal_procedure" ==> proposal_procedure
      ]

vote :: Rule
vote = "vote" =:= (0 :: Integer) ... (2 :: Integer)

proposal_procedure :: Rule
proposal_procedure =
  "proposal_procedure"
    =:= mp
      [ "anchor" ==> anchor
      , "deposit" ==> coin
      , "gov_action" ==> gov_action
      , "return_address" ==> reward_account
      ]

gov_action :: Rule
gov_action =
  "gov_action"
    =:= ( arr
            [ 0
            , "purpose" ==> (gov_action_id / VNil)
            , "update" ==> gov_params_update
            , "hash" ==> (script_hash / VNil)
            ]
            //- "Params update"
        )
    / (arr [1, a (gov_action_id / VNil), a protocol_version] //- "Hard fork")
    / ( arr
          [ 2
          , "withdrawls" ==> mp [0 <+ asKey reward_account ==> coin]
          , a (script_hash / VNil)
          ]
          //- "Treasury withdraw"
      )
    / (arr [3, "purpose" ==> (gov_action_id / VNil)] //- "No confidence")
    / ( arr
          [ 4
          , "purpose" ==> (gov_action_id / VNil)
          , "removed" ==> set (credential)
          , "added" ==> mp [0 <+ asKey credential ==> epoch_no]
          , "threshold" ==> unit_interval
          ]
          //- "Committee membership update"
      )
    / (arr [5, "purpose" ==> (gov_action_id / VNil), "constitution" ==> constitution] //- "New constitution")
    / (arr [6, a VNil] //- "Info action")

gov_action_id :: Rule
gov_action_id =
  "gov_action_id"
    =:= arr ["transaction_id" ==> hash32, "gov_action_index" ==> VUInt `sized` (2 :: Word64)]

gov_params_update :: Rule
gov_params_update =
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
              |   - min_fee_ref_script_cost_per_byte : reference scripts fee for the minimum fee calculation
              |]
    $ "gov_params_update"
      =:= mp
        [ "a0" ==> (nonnegative_interval / VNil)
        , "rho" ==> (unit_interval / VNil)
        , "tau" ==> (unit_interval / VNil)
        , "n_opt" ==> (VUInt `sized` (2 :: Word64) / VNil)
        , "prices" ==> (ex_unit_prices / VNil)
        , "epoch_max" ==> (epoch_interval / VNil)
        , "min_fee_a" ==> (coin / VNil)
        , "min_fee_b" ==> (coin / VNil)
        , "cost_models" ==> (cost_models / VNil)
        , "key_deposit" ==> (coin / VNil)
        , "max_tx_size" ==> (VUInt `sized` (4 :: Word64) / VNil)
        , "drep_deposit" ==> (coin / VNil)
        , "max_val_size" ==> (VUInt `sized` (4 :: Word64) / VNil)
        , "pool_deposit" ==> (coin / VNil)
        , "drep_activity" ==> (epoch_interval / VNil)
        , "min_pool_cost" ==> (coin / VNil)
        , "max_block_size" ==> (VUInt `sized` (4 :: Word64) / VNil)
        , "max_tx_ex_units" ==> (ex_units / VNil)
        , "coin_per_utxo_byte" ==> (coin / VNil)
        , "gov_action_deposit" ==> (coin / VNil)
        , "max_block_ex_units" ==> (ex_units / VNil)
        , "min_committee_size" ==> (VUInt `sized` (2 :: Word64) / VNil)
        , "gov_action_lifetime" ==> (epoch_interval / VNil)
        , "committee_term_limit" ==> (epoch_interval / VNil)
        , "collateral_percentage" ==> (VUInt `sized` (2 :: Word64) / VNil)
        , "max_block_header_size" ==> (VUInt `sized` (2 :: Word64) / VNil)
        , "max_collateral_inputs" ==> (VUInt `sized` (2 :: Word64) / VNil)
        , "drep_voting_thresholds" ==> (drep_voting_thresholds / VNil)
        , "pool_voting_thresholds" ==> (pool_voting_thresholds / VNil)
        , "min_fee_ref_script_cost_per_byte" ==> (nonnegative_interval / VNil)
        ]
