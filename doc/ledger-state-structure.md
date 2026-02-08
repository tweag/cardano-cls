# The Ledger State Structure

This file documents the structure of the existing Haskell node ledger state,
in order to establish which components we must support in the canonical
ledger state.

```haskell
data NewEpochState era = NewEpochState
  { nesEL :: !EpochNo
  -- ^ Number of the epoch when this NewEpochState was modified last. With respect to
  -- block and transactions validation this will always be the current epoch
  -- number. However, when it comes to the TICK rule, it will be the epoch number of the
  -- previous epoch whenever we are crossing the epoch boundary.
  , nesBprev :: !BlocksMade
  -- ^ Blocks made before current epoch
  , nesBcur :: !BlocksMade
  -- ^ Blocks made in current epoch
  , nesEs :: !(EpochState era)
  -- ^ Epoch state
  , nesRu :: !(StrictMaybe PulsingRewUpdate)
  -- ^ Possible reward update
  , nesPd :: !PoolDistr
  -- ^ Stake distribution within the stake pool
  , stashedAVVMAddresses :: !(StashedAVVMAddresses era)
  -- ^ AVVM addresses to be removed at the end of the Shelley era. Note that
  -- the existence of this field is a hack, related to the transition of UTxO
  -- to disk. We remove AVVM addresses from the UTxO on the Shelley/Allegra
  -- boundary. However, by this point the UTxO will be moved to disk, and
  -- hence doing a scan of the UTxO for AVVM addresses will be expensive. Our
  -- solution to this is to do a scan of the UTxO on the Byron/Shelley
  -- boundary (since Byron UTxO are still on disk), stash the results here,
  -- and then remove them at the Shelley/Allegra boundary.
  --
  -- This is very much an awkward implementation hack, and hence we hide it
  -- from as many places as possible.
  }
  deriving (Generic)
```
Out of this, we care about the following:

- Currently we only store the SlotNo. But actually converting from SlotNo to
  EpochNo can be tricky without knowing history, so we might want to add this
  (as well as BlockNo?) to the header.
- We need the blocks made in both epochs
- We need the epoch state
- We need the pool distribution

We do not need to care about the reward update (internal Haskell node detail)
nor the AVVM addresses


```haskell
data EpochState era = EpochState
  { esChainAccountState :: !ChainAccountState
  , esLState :: !(LedgerState era)
  , esSnapshots :: !SnapShots
  , esNonMyopic :: !NonMyopic
  -- ^ This field, esNonMyopic, does not appear in the formal spec
  -- and is not a part of the protocol. It is only used for providing
  -- data to the stake pool ranking calculation @getNonMyopicMemberRewards@.
  -- See https://github.com/intersectmbo/cardano-ledger/releases/latest/download/pool-ranking.pdf
  }
  deriving (Generic)
```

From the Epoch state, we need:

- The 'ChainAccountState' - otherwise called the pots. Treasury and Reserves
- The 'LedgerState'
- The snapshots

```haskell
data SnapShots = SnapShots
  { ssStakeMark :: SnapShot -- Lazy on purpose
  , ssStakeMarkPoolDistr :: PoolDistr -- Lazy on purpose
  , ssStakeSet :: !SnapShot
  , ssStakeGo :: !SnapShot
  , ssFee :: !Coin
  }
```

- TODO Look into the fee and mark pool distribution - how should these be included?

```haskell

data LedgerState era = LedgerState
  { lsUTxOState :: !(UTxOState era)
  -- ^ The current unspent transaction outputs.
  , lsCertState :: !(CertState era)
  }
  deriving (Generic)

-- | There is a serious invariant that we must maintain in the UTxOState.
--   Given (UTxOState utxo _ _ _ istake) it must be the case that
--   Of course computing the RHS of the above equality can be very expensive, so we only
--   use this route in the testing function smartUTxO. But we are very careful, wherever
--   we update the UTxO, we carefully make INCREMENTAL changes to istake to maintain
--   this invariant. This happens in the UTxO rule.
data UTxOState era = UTxOState
  { utxosUtxo :: !(UTxO era)
  , utxosDeposited :: !Coin
  , utxosFees :: !Coin
  , utxosGovState :: !(GovState era)
  , utxosInstantStake :: !(InstantStake era)
  , utxosDonation :: !Coin
  }
  deriving (Generic)

```
The instant stake can be ignored, since it's an optimisation.

## Governance

```haskell

data ConwayGovState era = ConwayGovState
  { cgsProposals :: !(Proposals era)
  , cgsCommittee :: !(StrictMaybe (Committee era))
  , cgsConstitution :: !(Constitution era)
  , cgsCurPParams :: !(PParams era)
  , cgsPrevPParams :: !(PParams era)
  , cgsFuturePParams :: !(FuturePParams era)
  , cgsDRepPulsingState :: !(DRepPulsingState era)
  -- ^ The 'cgsDRepPulsingState' field is a pulser that incrementally computes the stake
  -- distribution of the DReps over the Epoch following the close of voting at end of
  -- the previous Epoch. It assembles this with some of its other internal components
  -- into a (RatifyEnv era) when it completes, and then calls the RATIFY rule and
  -- eventually returns the updated RatifyState. The pulser is created at the Epoch
  -- boundary, but does no work until it is pulsed in the 'NEWEPOCH' rule, whenever the
  -- system is NOT at the epoch boundary.
  }
  deriving (Generic, Show)

```

```haskell

-- | A map of stake pool IDs (the hash of the stake pool operator's
-- verification key) to 'IndividualPoolStake'. Also holds absolute values
-- necessary for the calculations in the `computeDRepDistr`.
data PoolDistr = PoolDistr
  { unPoolDistr :: !(Map (KeyHash 'StakePool) IndividualPoolStake)
  , pdTotalActiveStake :: !(CompactForm Coin)
  -- ^ Total stake delegated to registered stake pools. In addition to
  -- the stake considered for the `individualPoolStake` Rational, we add
  -- proposal-deposits to this field.
  }

```

I think we can ignore the total active stake here, it is derivable from
other components

# Namespaces

- blocks/v0
  key: keyhash
  value: (Nat, Nat) - blocks made previous epoch, blocks made this epoch
  type: BlocksMade

- pots/v0 (not canonical)
  key: Treasury / Reserves / Deposit / Fee / Donation
  value: Coin
  type: ChainAccountState, UTxOState

- snapshot/v0/{mark,set,go}
  key: credential / keyhash
  value: (Coin, keyhash) / PoolParams
  type: Snapshots, Snapshot

- utxo/v0
  key: txin
  value: txout
  type: UTxO

- gov/pparams/v0
  key: Prev / Current / Future
  value: PParams
  type: PParams

- gov/committee/v0
  key: 0
  value : Committee
  type: Committee

- gov/proposals/v0
  key: 0
  value : Proposals
  type: Proposals

- gov/constitution/v0
  key: 0
  value: Constitution
  type: Consistution

- poolStake/v0
  key: keyhash
  value: Coin
  type: PoolDistr
