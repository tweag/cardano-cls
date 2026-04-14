{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Cardano.SCLS.Namespace.EntitiesDormantEpochs where

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
        |     type: singleton
        |
        | types:
        |   singleton:
        |     size: 1
        |     type: u1
        |     const: 0
        | ```
        |]
    $ "record_entry" =:= epoch_no
