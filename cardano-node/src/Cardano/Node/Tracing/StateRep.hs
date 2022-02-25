{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Tracing.StateRep
  ( NodeState (..)
  , traceNodeStateChainDB
  , traceNodeStateStartup
  , traceNodeStateShutdown
  ) where

import           Cardano.Prelude
import           Cardano.Logging
import           Data.Aeson

import qualified Ouroboros.Consensus.Block.RealPoint as RP
import qualified Ouroboros.Consensus.Node.NetworkProtocolVersion as NPV
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LgrDb
import           Ouroboros.Network.Block (pointSlot)

import           Cardano.Node.Handlers.Shutdown (ShutdownTrace)
import qualified Cardano.Node.Startup as Startup
import           Cardano.Slotting.Slot (EpochNo, SlotNo (..), WithOrigin)
import           Cardano.Tracing.OrphanInstances.Network ()

instance ToJSON ChunkNo
instance ToJSON (WithOrigin SlotNo)

data OpeningDbs
  = StartedOpeningImmutableDB
  | OpenedImmutableDB (WithOrigin SlotNo) ChunkNo
  | StartedOpeningVolatileDB
  | OpenedVolatileDB
  | StartedOpeningLgrDB
  | OpenedLgrDB
  deriving (Generic, ToJSON)

data Replays
  = ReplayFromGenesis  (WithOrigin SlotNo)
  | ReplayFromSnapshot SlotNo (WithOrigin SlotNo) (WithOrigin SlotNo)
  | ReplayedBlock      SlotNo (WithOrigin SlotNo) (WithOrigin SlotNo)
  deriving (Generic, ToJSON)

data InitChainSelection
  = InitChainStartedSelection
  | InitChainSelected
  deriving (Generic, ToJSON)

data AddedToCurrentChain
  = AddedToCurrentChain EpochNo SlotNo
  deriving (Generic, ToJSON)

data StartupState
  = StartupSocketConfigError Text
  | StartupDBValidation
  | NetworkConfigUpdate
  | NetworkConfigUpdateError Text
  | P2PWarning
  | P2PWarningDevelopementNetworkProtocols
  | WarningDevelopmentNetworkProtocols [NPV.NodeToNodeVersion] [NPV.NodeToClientVersion]
  deriving (Generic, ToJSON)

-- | The representation of the current state of node.
--   All node states prior to tracing system going online are effectively invisible.
data NodeState
  = NodeTracingOnlineConfiguring
  | NodeOpeningDbs OpeningDbs
  | NodeReplays Replays
  | NodeInitChainSelection InitChainSelection
  | NodeAddBlock AddedToCurrentChain
  | NodeStartup StartupState
  | NodeShutdown ShutdownTrace
  deriving (Generic, ToJSON)

traceNodeStateChainDB
  :: Trace IO NodeState
  -> ChainDB.TraceEvent blk
  -> IO ()
traceNodeStateChainDB tr ev =
  case ev of
    ChainDB.TraceOpenEvent ev' ->
      case ev' of
        ChainDB.StartedOpeningImmutableDB ->
          traceWith tr $ NodeOpeningDbs StartedOpeningImmutableDB
        ChainDB.OpenedImmutableDB p chunk ->
          traceWith tr $ NodeOpeningDbs $ OpenedImmutableDB (pointSlot p) chunk
        ChainDB.StartedOpeningVolatileDB ->
          traceWith tr $ NodeOpeningDbs StartedOpeningVolatileDB
        ChainDB.OpenedVolatileDB ->
          traceWith tr $ NodeOpeningDbs OpenedVolatileDB
        ChainDB.StartedOpeningLgrDB ->
          traceWith tr $ NodeOpeningDbs StartedOpeningLgrDB
        ChainDB.OpenedLgrDB ->
          traceWith tr $ NodeOpeningDbs OpenedLgrDB
        _ -> return ()
    ChainDB.TraceLedgerReplayEvent ev' ->
      case ev' of
        LgrDb.ReplayFromGenesis (LgrDb.ReplayGoal p) ->
          traceWith tr $ NodeReplays $ ReplayFromGenesis (pointSlot p)
        LgrDb.ReplayFromSnapshot _ (RP.RealPoint s _) (LgrDb.ReplayStart rs) (LgrDb.ReplayGoal rp) ->
          traceWith tr $ NodeReplays $ ReplayFromSnapshot s (pointSlot rs) (pointSlot rp)
        LgrDb.ReplayedBlock (RP.RealPoint s _) _ (LgrDb.ReplayStart rs) (LgrDb.ReplayGoal rp) ->
          traceWith tr $ NodeReplays $ ReplayedBlock s (pointSlot rs) (pointSlot rp)
    ChainDB.TraceInitChainSelEvent ev' ->
      case ev' of
        ChainDB.StartedInitChainSelection ->
          traceWith tr $ NodeInitChainSelection InitChainStartedSelection
        ChainDB.InitalChainSelected ->
          traceWith tr $ NodeInitChainSelection InitChainSelected
        _ -> return ()
    ChainDB.TraceAddBlockEvent ev' ->
      case ev' of
        ChainDB.AddedToCurrentChain _ (ChainDB.NewTipInfo _ epoch sn _) _ _ ->
          traceWith tr $ NodeAddBlock $ AddedToCurrentChain epoch (SlotNo sn)
        _ -> return ()
    _ -> return ()

traceNodeStateStartup
  :: Trace IO NodeState
  -> Startup.StartupTrace blk
  -> IO ()
traceNodeStateStartup tr ev =
  case ev of
    Startup.StartupSocketConfigError e ->
      traceWith tr $ NodeStartup $ StartupSocketConfigError (show e)
    Startup.StartupDBValidation ->
      traceWith tr $ NodeStartup StartupDBValidation
    Startup.NetworkConfigUpdate ->
      traceWith tr $ NodeStartup NetworkConfigUpdate
    Startup.NetworkConfigUpdateError e ->
      traceWith tr $ NodeStartup $ NetworkConfigUpdateError e
    Startup.P2PWarning ->
      traceWith tr $ NodeStartup P2PWarning
    Startup.P2PWarningDevelopementNetworkProtocols ->
      traceWith tr $ NodeStartup P2PWarningDevelopementNetworkProtocols
    Startup.WarningDevelopmentNetworkProtocols n2ns n2cs ->
      traceWith tr $ NodeStartup $ WarningDevelopmentNetworkProtocols n2ns n2cs
    _ -> return ()

traceNodeStateShutdown
  :: Trace IO NodeState
  -> ShutdownTrace
  -> IO ()
traceNodeStateShutdown tr = traceWith tr . NodeShutdown
