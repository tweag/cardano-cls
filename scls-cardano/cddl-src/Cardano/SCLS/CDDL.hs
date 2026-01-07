{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.SCLS.CDDL (
  namespaces,
  namespaceSymbolFromText,
) where

import Cardano.SCLS.Namespace.Blocks qualified as Blocks
import Cardano.SCLS.Namespace.GovCommittee qualified as GovCommittee
import Cardano.SCLS.Namespace.GovConstitution qualified as GovConstitution
import Cardano.SCLS.Namespace.GovPParams qualified as GovPParams
import Cardano.SCLS.Namespace.GovProposals qualified as GovProposals
import Cardano.SCLS.Namespace.Nonces qualified as Nonces
import Cardano.SCLS.Namespace.PoolStake qualified as PoolStake
import Cardano.SCLS.Namespace.Pots qualified as Pots
import Cardano.SCLS.Namespace.Snapshots qualified as Snapshots
import Cardano.SCLS.Namespace.UTxO qualified as UTxO
import Cardano.SCLS.NamespaceKey qualified as Spec
import Cardano.SCLS.NamespaceSymbol (SomeNamespaceSymbol (..), toString)
import Codec.CBOR.Cuddle.Huddle (Huddle, HuddleItem (HIRule), Rule, collectFromInit)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import GHC.TypeLits (KnownNat, KnownSymbol)

{- | Lookup a namespace symbol from its text representation.
| Returns 'Nothing' if the namespace is not known.
-}
namespaceSymbolFromText :: Text -> Maybe SomeNamespaceSymbol
namespaceSymbolFromText t =
  find (\ns -> T.pack (toString ns) == t) (Map.keys namespaces)

-- | List of the namespaces known to the SCLS utilities.
namespaces :: Map.Map SomeNamespaceSymbol Huddle
namespaces =
  Map.fromList
    [ mkDefinition @"utxo/v0" UTxO.record_entry
    , mkDefinition @"blocks/v0" Blocks.record_entry
    , mkDefinition @"pots/v0" Pots.record_entry
    , mkDefinition @"pool_stake/v0" PoolStake.record_entry
    , mkDefinition @"snapshots/v0" Snapshots.record_entry
    , mkDefinition @"nonces/v0" Nonces.record_entry
    , mkDefinition @"gov/committee/v0" GovCommittee.record_entry
    , mkDefinition @"gov/constitution/v0" GovConstitution.record_entry
    , mkDefinition @"gov/pparams/v0" GovPParams.record_entry
    , mkDefinition @"gov/proposals/v0" GovProposals.record_entry
    ]

mkDefinition :: forall ns. (KnownSymbol ns, KnownNat (Spec.NamespaceKeySize ns)) => Rule -> (SomeNamespaceSymbol, Huddle)
mkDefinition r = (n, (collectFromInit [HIRule r]))
 where
  n = SomeNamespaceSymbol (Proxy @ns)

type instance Spec.NamespaceKeySize "utxo/v0" = 34
type instance Spec.NamespaceKeySize "blocks/v0" = 36 -- 28 bytes for key, and 8 for epoch in BE
type instance Spec.NamespaceKeySize "nonces/v0" = 1 -- Just zero
type instance Spec.NamespaceKeySize "pots/v0" = 8 -- Key is epoch number
type instance Spec.NamespaceKeySize "pool_stake/v0" = 28 -- 28 bytes for key
type instance Spec.NamespaceKeySize "snapshots/v0" = 32 -- 1 byte for hash type, 1 byte for stage, 29 bytes for hash (cred 29, key 28+1),  1 for value type
type instance Spec.NamespaceKeySize "gov/committee/v0" = 8
type instance Spec.NamespaceKeySize "gov/constitution/v0" = 8
type instance Spec.NamespaceKeySize "gov/pparams/v0" = 4
type instance Spec.NamespaceKeySize "gov/proposals/v0" = 34 -- 32 bytes txid+tx index + 2 bytes for proposal index
