{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Cardano.SCLS.Namespace.EntitiesAccounts where

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
        |     type: credential
        |
        | types:
        |   credential:
        |     seq:
        |       - id: cred_data
        |         size: 28
        | ```
        |]
    $ "record_entry" =:= account_state

account_state :: Rule
account_state =
  comment
    [str| Account state consists of the following:
              | 1 - account balance
              | 2 - account deposit
              | 3 - stake pool delegation (if the account is delegated to a stake pool)
              | 4 - drep delegation (if the account is delegated to a drep)
        |]
    $ "account_state"
      =:= arr [a coin, a coin, a (pool_keyhash / VNil), a (drep / VNil)]

drep :: Rule
drep =
  comment
    [str| 0 - key hash
              | 1 - script hash
              | 2 - always abstain
              | 3 - always no confidence
        |]
    $ "drep"
      =:= arr [0, a keyhash28]
      / arr [1, a script_hash]
      / arr [2]
      / arr [3]
