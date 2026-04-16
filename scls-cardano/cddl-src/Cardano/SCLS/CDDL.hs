{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.SCLS.CDDL (
  namespaceSymbolFromText,
  knownNamespaceKeySizes,
  knownNamespaces,
) where

import Cardano.SCLS.Namespace.Blocks qualified as Blocks
import Cardano.SCLS.Namespace.EntitiesAccounts qualified as EntitiesAccounts
import Cardano.SCLS.Namespace.EntitiesCommittee qualified as EntitiesCommittee
import Cardano.SCLS.Namespace.EntitiesDReps qualified as EntitiesDReps
import Cardano.SCLS.Namespace.EntitiesStakePools qualified as EntitiesStakePools
import Cardano.SCLS.Namespace.EntitiesStakePoolsFutureParams qualified as EntitiesStakePoolsFutureParams
import Cardano.SCLS.Namespace.EntitiesStakePoolsVRFKeyHashes qualified as EntitiesStakePoolsVRFKeyHashes
import Cardano.SCLS.Namespace.GovCommittee qualified as GovCommittee
import Cardano.SCLS.Namespace.GovConstitution qualified as GovConstitution
import Cardano.SCLS.Namespace.GovPParams qualified as GovPParams
import Cardano.SCLS.Namespace.GovProposals qualified as GovProposals
import Cardano.SCLS.Namespace.Nonces qualified as Nonces
import Cardano.SCLS.Namespace.Snapshots qualified as Snapshots
import Cardano.SCLS.Namespace.UTxO qualified as UTxO
import Cardano.SCLS.NamespaceKey qualified as Spec
import Cardano.SCLS.NamespaceSymbol (KnownSpec (..), SomeNamespaceSymbol (..), mkNamespaceSymbol, toString)
import Codec.CBOR.Cuddle.Huddle (Huddle, HuddleItem (HIRule), Rule, collectFromInit)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.TypeLits (symbolVal)

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

instance KnownSpec "snapshots/mark/v0" where
  namespaceSpec _ = mkDefinition Snapshots.record_entry

instance KnownSpec "snapshots/set/v0" where
  namespaceSpec _ = mkDefinition Snapshots.record_entry

instance KnownSpec "snapshots/go/v0" where
  namespaceSpec _ = mkDefinition Snapshots.record_entry

instance KnownSpec "nonces/v0" where
  namespaceSpec _ = mkDefinition Nonces.record_entry

instance KnownSpec "entities/accounts/v0" where
  namespaceSpec _ = mkDefinition EntitiesAccounts.record_entry

instance KnownSpec "entities/committee/v0" where
  namespaceSpec _ = mkDefinition EntitiesCommittee.record_entry

instance KnownSpec "entities/dreps/v0" where
  namespaceSpec _ = mkDefinition EntitiesDReps.record_entry

instance KnownSpec "entities/stake_pools/v0" where
  namespaceSpec _ = mkDefinition EntitiesStakePools.record_entry

instance KnownSpec "entities/stake_pools/future_params/v0" where
  namespaceSpec _ = mkDefinition EntitiesStakePoolsFutureParams.record_entry

instance KnownSpec "entities/stake_pools/vrf_key_hashes/v0" where
  namespaceSpec _ = mkDefinition EntitiesStakePoolsVRFKeyHashes.record_entry

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

knownNamespaceKeySizes :: Map String Int
knownNamespaceKeySizes =
  Map.fromList $ map (\(SomeNamespaceSymbol (p :: proxy ns)) -> (symbolVal p, Spec.namespaceKeySize @ns)) knownNamespaces

knownNamespaces :: [SomeNamespaceSymbol]
knownNamespaces =
  [ mkNamespaceSymbol @"utxo/v0"
  , mkNamespaceSymbol @"blocks/v0"
  , mkNamespaceSymbol @"snapshots/mark/v0"
  , mkNamespaceSymbol @"snapshots/set/v0"
  , mkNamespaceSymbol @"snapshots/go/v0"
  , mkNamespaceSymbol @"nonces/v0"
  , mkNamespaceSymbol @"entities/accounts/v0"
  , mkNamespaceSymbol @"entities/committee/v0"
  , mkNamespaceSymbol @"entities/dreps/v0"
  , mkNamespaceSymbol @"entities/stake_pools/v0"
  , mkNamespaceSymbol @"entities/stake_pools/future_params/v0"
  , mkNamespaceSymbol @"entities/stake_pools/vrf_key_hashes/v0"
  , mkNamespaceSymbol @"gov/committee/v0"
  , mkNamespaceSymbol @"gov/constitution/v0"
  , mkNamespaceSymbol @"gov/pparams/v0"
  , mkNamespaceSymbol @"gov/proposals/v0"
  ]

type instance Spec.NamespaceKeySize "utxo/v0" = 34
type instance Spec.NamespaceKeySize "blocks/v0" = 36 -- 28 bytes for key, and 8 for epoch in BE
type instance Spec.NamespaceKeySize "nonces/v0" = 1 -- Just zero
type instance Spec.NamespaceKeySize "snapshots/mark/v0" = 31 -- 1 byte for hash type, 29 bytes for key hash (including 1-byte discriminator/padding; cred 29, key 28+1), 1 byte for value type
type instance Spec.NamespaceKeySize "snapshots/set/v0" = 31 -- 1 byte for hash type, 29 bytes for key hash (including 1-byte discriminator/padding; cred 29, key 28+1), 1 byte for value type
type instance Spec.NamespaceKeySize "snapshots/go/v0" = 31 -- 1 byte for hash type, 29 bytes for key hash (including 1-byte discriminator/padding; cred 29, key 28+1), 1 byte for value type
type instance Spec.NamespaceKeySize "entities/accounts/v0" = 29 -- 1 byte for tag, 28 bytes for hash
type instance Spec.NamespaceKeySize "entities/committee/v0" = 8
type instance Spec.NamespaceKeySize "entities/dreps/v0" = 29 -- 1 byte for tag, 28 bytes for hash
type instance Spec.NamespaceKeySize "entities/stake_pools/v0" = 28
type instance Spec.NamespaceKeySize "entities/stake_pools/future_params/v0" = 28
type instance Spec.NamespaceKeySize "entities/stake_pools/vrf_key_hashes/v0" = 32
type instance Spec.NamespaceKeySize "gov/committee/v0" = 8
type instance Spec.NamespaceKeySize "gov/constitution/v0" = 8
type instance Spec.NamespaceKeySize "gov/pparams/v0" = 4
type instance Spec.NamespaceKeySize "gov/proposals/v0" = 34 -- 32 bytes txid+tx index + 2 bytes for proposal index
