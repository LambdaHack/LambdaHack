{-# LANGUAGE DeriveGeneric #-}
-- | A set of atomic commands shared by client and server.
-- These are the largest building blocks that have no components
-- that can be observed in isolation.
--
-- We try to make atomic commands respect the laws of energy and mass
-- conservation, unless they really can't, e.g., monster spawning.
-- For example item removal from inventory is not an atomic command,
-- but item dropped from the inventory to the ground is. This makes
-- it easier to undo the commands. In principle, the commands are the only
-- way to affect the basic game state (@State@).
--
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Atomic.CmdAtomic
  ( CmdAtomic(..), UpdAtomic(..), SfxAtomic(..), SfxMsg(..)
  , undoUpdAtomic, undoSfxAtomic, undoCmdAtomic
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import Data.Int (Int64)
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.TileKind (TileKind)

-- | Abstract syntax of atomic commands, that is, atomic game state
-- transformations.
data CmdAtomic =
    UpdAtomic !UpdAtomic  -- ^ atomic updates
  | SfxAtomic !SfxAtomic  -- ^ atomic special effects
  deriving (Show, Eq, Generic)

instance Binary CmdAtomic

-- | Abstract syntax of atomic updates, that is, atomic commands
-- that really change the state. Most of them are an encoding of a game
-- state diff, though they also carry some intentional hints
-- that help clients determine whether and how to communicate them to players.
data UpdAtomic =
  -- Create/destroy actors and items.
    UpdCreateActor !ActorId !Actor ![(ItemId, Item)]
  | UpdDestroyActor !ActorId !Actor ![(ItemId, Item)]
  | UpdCreateItem !ItemId !Item !ItemQuant !Container
  | UpdDestroyItem !ItemId !Item !ItemQuant !Container
  | UpdSpotActor !ActorId !Actor ![(ItemId, Item)]
  | UpdLoseActor !ActorId !Actor ![(ItemId, Item)]
  | UpdSpotItem !Bool !ItemId !Item !ItemQuant !Container
  | UpdLoseItem !Bool !ItemId !Item !ItemQuant !Container
  -- Move actors and items.
  | UpdMoveActor !ActorId !Point !Point
  | UpdWaitActor !ActorId !Bool
  | UpdDisplaceActor !ActorId !ActorId
  | UpdMoveItem !ItemId !Int !ActorId !CStore !CStore
  -- Change actor attributes.
  | UpdRefillHP !ActorId !Int64
  | UpdRefillCalm !ActorId !Int64
  | UpdTrajectory !ActorId !(Maybe ([Vector], Speed)) !(Maybe ([Vector], Speed))
  -- Change faction attributes.
  | UpdQuitFaction !FactionId !(Maybe Status) !(Maybe Status)
  | UpdLeadFaction !FactionId !(Maybe ActorId) !(Maybe ActorId)
  | UpdDiplFaction !FactionId !FactionId !Diplomacy !Diplomacy
  | UpdTacticFaction !FactionId !Tactic !Tactic
  | UpdAutoFaction !FactionId !Bool
  | UpdRecordKill !ActorId !(Kind.Id ItemKind) !Int
  -- Alter map.
  | UpdAlterTile !LevelId !Point !(Kind.Id TileKind) !(Kind.Id TileKind)
  | UpdAlterExplorable !LevelId !Int
  | UpdSearchTile !ActorId !Point !(Kind.Id TileKind)
  | UpdHideTile !ActorId !Point !(Kind.Id TileKind)
  | UpdSpotTile !LevelId ![(Point, Kind.Id TileKind)]
  | UpdLoseTile !LevelId ![(Point, Kind.Id TileKind)]
  | UpdAlterSmell !LevelId !Point !Time !Time
  | UpdSpotSmell !LevelId ![(Point, Time)]
  | UpdLoseSmell !LevelId ![(Point, Time)]
  -- Assorted.
  | UpdTimeItem !ItemId !Container !ItemTimer !ItemTimer
  | UpdAgeGame ![LevelId]
  | UpdUnAgeGame ![LevelId]
  | UpdDiscover !Container !ItemId !(Kind.Id ItemKind) !ItemSeed
  | UpdCover !Container !ItemId !(Kind.Id ItemKind) !ItemSeed
  | UpdDiscoverKind !Container !ItemId !(Kind.Id ItemKind)
  | UpdCoverKind !Container !ItemId !(Kind.Id ItemKind)
  | UpdDiscoverSeed !Container !ItemId !ItemSeed
  | UpdCoverSeed !Container !ItemId !ItemSeed
  | UpdPerception !LevelId !Perception !Perception
  | UpdRestart !FactionId !DiscoveryKind !PerLid !State !Challenge !DebugModeCli
  | UpdRestartServer !State
  | UpdResume !FactionId !PerLid
  | UpdResumeServer !State
  | UpdKillExit !FactionId
  | UpdWriteSave
  | UpdMsgAll !Text
  deriving (Show, Eq, Generic)

instance Binary UpdAtomic

-- | Abstract syntax of atomic special effects, that is, atomic commands
-- that only display special effects and don't change the state.
data SfxAtomic =
    SfxStrike !ActorId !ActorId !ItemId !CStore
  | SfxRecoil !ActorId !ActorId !ItemId !CStore
  | SfxSteal !ActorId !ActorId !ItemId !CStore
  | SfxRelease !ActorId !ActorId !ItemId !CStore
  | SfxProject !ActorId !ItemId !CStore
  | SfxReceive !ActorId !ItemId !CStore
  | SfxApply !ActorId !ItemId !CStore
  | SfxCheck !ActorId !ItemId !CStore
  | SfxTrigger !ActorId !Point
  | SfxShun !ActorId !Point
  | SfxEffect !FactionId !ActorId !IK.Effect !Int64
  | SfxMsgFid !FactionId !SfxMsg
  deriving (Show, Eq, Generic)

instance Binary SfxAtomic

data SfxMsg =
    SfxUnexpected !ReqFailure
  | SfxLoudUpd !Bool !UpdAtomic
  | SfxLoudStrike !Bool !(Kind.Id ItemKind) !Int
  | SfxFizzles
  | SfxVoidDetection
  | SfxSummonLackCalm !ActorId
  | SfxLevelNoMore
  | SfxLevelPushed
  | SfxBracedImmune !ActorId
  | SfxEscapeImpossible
  | SfxTransImpossible
  | SfxIdentifyNothing !CStore
  | SfxPurposeNothing !CStore
  | SfxPurposeTooFew !Int !Int
  | SfxPurposeUnique
  | SfxColdFish
  deriving (Show, Eq, Generic)

instance Binary SfxMsg

undoUpdAtomic :: UpdAtomic -> Maybe UpdAtomic
undoUpdAtomic cmd = case cmd of
  UpdCreateActor aid body ais -> Just $ UpdDestroyActor aid body ais
  UpdDestroyActor aid body ais -> Just $ UpdCreateActor aid body ais
  UpdCreateItem iid item k c -> Just $ UpdDestroyItem iid item k c
  UpdDestroyItem iid item k c -> Just $ UpdCreateItem iid item k c
  UpdSpotActor aid body ais -> Just $ UpdLoseActor aid body ais
  UpdLoseActor aid body ais -> Just $ UpdSpotActor aid body ais
  UpdSpotItem verbose iid item k c -> Just $ UpdLoseItem verbose iid item k c
  UpdLoseItem verbose iid item k c -> Just $ UpdSpotItem verbose iid item k c
  UpdMoveActor aid fromP toP -> Just $ UpdMoveActor aid toP fromP
  UpdWaitActor aid toWait -> Just $ UpdWaitActor aid (not toWait)
  UpdDisplaceActor source target -> Just $ UpdDisplaceActor target source
  UpdMoveItem iid k aid c1 c2 -> Just $ UpdMoveItem iid k aid c2 c1
  UpdRefillHP aid n -> Just $ UpdRefillHP aid (-n)
  UpdRefillCalm aid n -> Just $ UpdRefillCalm aid (-n)
  UpdTrajectory aid fromT toT -> Just $ UpdTrajectory aid toT fromT
  UpdQuitFaction fid fromSt toSt -> Just $ UpdQuitFaction fid toSt fromSt
  UpdLeadFaction fid source target -> Just $ UpdLeadFaction fid target source
  UpdDiplFaction fid1 fid2 fromDipl toDipl ->
    Just $ UpdDiplFaction fid1 fid2 toDipl fromDipl
  UpdTacticFaction fid toT fromT -> Just $ UpdTacticFaction fid fromT toT
  UpdAutoFaction fid st -> Just $ UpdAutoFaction fid (not st)
  UpdRecordKill aid ikind k -> Just $ UpdRecordKill aid ikind (-k)
  UpdAlterTile lid p fromTile toTile ->
    Just $ UpdAlterTile lid p toTile fromTile
  UpdAlterExplorable lid delta -> Just $ UpdAlterExplorable lid (-delta)
  UpdSearchTile aid p toTile -> Just $ UpdHideTile aid p toTile
  UpdHideTile aid p toTile -> Just $ UpdSearchTile aid p toTile
  UpdSpotTile lid ts -> Just $ UpdLoseTile lid ts
  UpdLoseTile lid ts -> Just $ UpdSpotTile lid ts
  UpdAlterSmell lid p fromSm toSm -> Just $ UpdAlterSmell lid p toSm fromSm
  UpdSpotSmell lid sms -> Just $ UpdLoseSmell lid sms
  UpdLoseSmell lid sms -> Just $ UpdSpotSmell lid sms
  UpdTimeItem iid c fromIt toIt -> Just $ UpdTimeItem iid c toIt fromIt
  UpdAgeGame lids -> Just $ UpdUnAgeGame lids
  UpdUnAgeGame lids -> Just $ UpdAgeGame lids
  UpdDiscover c iid ik seed -> Just $ UpdCover c iid ik seed
  UpdCover c iid ik seed -> Just $ UpdDiscover c iid ik seed
  UpdDiscoverKind c iid ik -> Just $ UpdCoverKind c iid ik
  UpdCoverKind c iid ik -> Just $ UpdDiscoverKind c iid ik
  UpdDiscoverSeed c iid seed -> Just $ UpdCoverSeed c iid seed
  UpdCoverSeed c iid seed -> Just $ UpdDiscoverSeed c iid seed
  UpdPerception lid outPer inPer -> Just $ UpdPerception lid inPer outPer
  UpdRestart{} -> Just cmd  -- here history ends; change direction
  UpdRestartServer{} -> Just cmd  -- here history ends; change direction
  UpdResume{} -> Nothing
  UpdResumeServer{} -> Nothing
  UpdKillExit{} -> Nothing
  UpdWriteSave -> Nothing
  UpdMsgAll{} -> Nothing  -- only generated by @cmdAtomicFilterCli@ or as a hack

undoSfxAtomic :: SfxAtomic -> SfxAtomic
undoSfxAtomic cmd = case cmd of
  SfxStrike source target iid cstore -> SfxRecoil source target iid cstore
  SfxRecoil source target iid cstore -> SfxStrike source target iid cstore
  SfxSteal source target iid cstore -> SfxRelease source target iid cstore
  SfxRelease source target iid cstore -> SfxSteal source target iid cstore
  SfxProject aid iid cstore -> SfxReceive aid iid cstore
  SfxReceive aid iid cstore -> SfxProject aid iid cstore
  SfxApply aid iid cstore -> SfxCheck aid iid cstore
  SfxCheck aid iid cstore -> SfxApply aid iid cstore
  SfxTrigger aid p -> SfxShun aid p
  SfxShun aid p -> SfxTrigger aid p
  SfxEffect{} -> cmd  -- not ideal?
  SfxMsgFid{} -> cmd

undoCmdAtomic :: CmdAtomic -> Maybe CmdAtomic
undoCmdAtomic (UpdAtomic cmd) = UpdAtomic <$> undoUpdAtomic cmd
undoCmdAtomic (SfxAtomic sfx) = Just $ SfxAtomic $ undoSfxAtomic sfx
