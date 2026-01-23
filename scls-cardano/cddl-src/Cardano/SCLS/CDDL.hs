{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.SCLS.CDDL (
  namespaceSymbolFromText,
  knownNamespaces,
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
import Cardano.SCLS.NamespaceSymbol (KnownSpec (..), SomeNamespaceSymbol (..), mkNamespaceSymbol, toString)
import Codec.CBOR.Cuddle.Huddle (Huddle, HuddleItem (HIRule), Rule, collectFromInit)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T

{- | Lookup a namespace symbol from its text representation.
| Returns 'Nothing' if the namespace is not known.
-}
namespaceSymbolFromText :: Text -> Maybe SomeNamespaceSymbol
namespaceSymbolFromText t =
  find (\ns -> T.pack (toString ns) == t) knownNamespaces

instance KnownSpec "utxo/v0" where
  namespaceSpec _ = mkDefinition UTxO.record_entry

instance KnownSpec "blocks/v0" where
  namespaceSpec _ = mkDefinition Blocks.record_entry

instance KnownSpec "pots/v0" where
  namespaceSpec _ = mkDefinition Pots.record_entry

instance KnownSpec "pool_stake/v0" where
  namespaceSpec _ = mkDefinition PoolStake.record_entry

instance KnownSpec "snapshots/v0" where
  namespaceSpec _ = mkDefinition Snapshots.record_entry

instance KnownSpec "nonces/v0" where
  namespaceSpec _ = mkDefinition Nonces.record_entry

instance KnownSpec "gov/committee/v0" where
  namespaceSpec _ = mkDefinition GovCommittee.record_entry

instance KnownSpec "gov/constitution/v0" where
  namespaceSpec _ = mkDefinition GovConstitution.record_entry

instance KnownSpec "gov/pparams/v0" where
  namespaceSpec _ = mkDefinition GovPParams.record_entry

instance KnownSpec "gov/proposals/v0" where
  namespaceSpec _ = mkDefinition GovProposals.record_entry

mkDefinition :: Rule -> Huddle
mkDefinition r = collectFromInit [HIRule r]

knownNamespaces :: [SomeNamespaceSymbol]
knownNamespaces =
  [ mkNamespaceSymbol @"utxo/v0"
  , mkNamespaceSymbol @"blocks/v0"
  , mkNamespaceSymbol @"pots/v0"
  , mkNamespaceSymbol @"pool_stake/v0"
  , mkNamespaceSymbol @"snapshots/v0"
  , mkNamespaceSymbol @"nonces/v0"
  , mkNamespaceSymbol @"gov/committee/v0"
  , mkNamespaceSymbol @"gov/constitution/v0"
  , mkNamespaceSymbol @"gov/pparams/v0"
  , mkNamespaceSymbol @"gov/proposals/v0"
  ]

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
