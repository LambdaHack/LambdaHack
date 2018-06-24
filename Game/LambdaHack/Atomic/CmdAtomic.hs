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
-- way to affect the basic game state ('State').
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

-- Dependence on ClientOptions is an anomaly. Instead, probably the raw
-- remaining commandline should be passed and parsed by the client to extract
-- client and ui options from and singnal an error if anything was left.

import           Game.LambdaHack.Client.ClientOptions
import           Game.LambdaHack.Common.Actor
import qualified Game.LambdaHack.Common.Dice as Dice
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Perception
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.ReqFailure
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Vector
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.TileKind (TileKind)

-- | Abstract syntax of atomic commands, that is, atomic game state
-- transformations.
data CmdAtomic =
    UpdAtomic UpdAtomic  -- ^ atomic updates
  | SfxAtomic SfxAtomic  -- ^ atomic special effects
  deriving (Show, Eq, Generic)

instance Binary CmdAtomic

-- | Abstract syntax of atomic updates, that is, atomic commands
-- that really change the 'State'. Most of them are an encoding of a game
-- state diff, though they also carry some intentional hints
-- that help clients determine whether and how to communicate it to players.
data UpdAtomic =
  -- Create/destroy actors and items.
    UpdCreateActor ActorId Actor [(ItemId, Item)]
  | UpdDestroyActor ActorId Actor [(ItemId, Item)]
  | UpdCreateItem ItemId Item ItemQuant Container
  | UpdDestroyItem ItemId Item ItemQuant Container
  | UpdSpotActor ActorId Actor [(ItemId, Item)]
  | UpdLoseActor ActorId Actor [(ItemId, Item)]
  | UpdSpotItem Bool ItemId Item ItemQuant Container
  | UpdLoseItem Bool ItemId Item ItemQuant Container
  | UpdSpotItemBag Container ItemBag [(ItemId, Item)]
  | UpdLoseItemBag Container ItemBag [(ItemId, Item)]
  -- Move actors and items.
  | UpdMoveActor ActorId Point Point
  | UpdWaitActor ActorId Bool
  | UpdDisplaceActor ActorId ActorId
  | UpdMoveItem ItemId Int ActorId CStore CStore
  -- Change actor attributes.
  | UpdRefillHP ActorId Int64
  | UpdRefillCalm ActorId Int64
  | UpdTrajectory ActorId (Maybe ([Vector], Speed)) (Maybe ([Vector], Speed))
  -- Change faction attributes.
  | UpdQuitFaction FactionId (Maybe Status) (Maybe Status)
  | UpdLeadFaction FactionId (Maybe ActorId) (Maybe ActorId)
  | UpdDiplFaction FactionId FactionId Diplomacy Diplomacy
  | UpdTacticFaction FactionId Tactic Tactic
  | UpdAutoFaction FactionId Bool
  | UpdRecordKill ActorId (ContentId ItemKind) Int
  -- Alter map.
  | UpdAlterTile LevelId Point (ContentId TileKind) (ContentId TileKind)
  | UpdAlterExplorable LevelId Int
  | UpdAlterGold Int
  | UpdSearchTile ActorId Point (ContentId TileKind)
  | UpdHideTile ActorId Point (ContentId TileKind)
  | UpdSpotTile LevelId [(Point, ContentId TileKind)]
  | UpdLoseTile LevelId [(Point, ContentId TileKind)]
  | UpdAlterSmell LevelId Point Time Time
  | UpdSpotSmell LevelId [(Point, Time)]
  | UpdLoseSmell LevelId [(Point, Time)]
  -- Assorted.
  | UpdTimeItem ItemId Container ItemTimer ItemTimer
  | UpdAgeGame [LevelId]
  | UpdUnAgeGame [LevelId]
  | UpdDiscover Container ItemId (ContentId ItemKind) IA.AspectRecord
  | UpdCover Container ItemId (ContentId ItemKind) IA.AspectRecord
  | UpdDiscoverKind Container ItemKindIx (ContentId ItemKind)
  | UpdCoverKind Container ItemKindIx (ContentId ItemKind)
  | UpdDiscoverAspect Container ItemId IA.AspectRecord
  | UpdCoverAspect Container ItemId IA.AspectRecord
  | UpdDiscoverServer ItemId IA.AspectRecord
  | UpdCoverServer ItemId IA.AspectRecord
  | UpdPerception LevelId Perception Perception
  | UpdRestart FactionId PerLid State Challenge ClientOptions
  | UpdRestartServer State
  | UpdResume FactionId PerLid
  | UpdResumeServer State
  | UpdKillExit FactionId
  | UpdWriteSave
  deriving (Show, Eq, Generic)

instance Binary UpdAtomic

-- | Abstract syntax of atomic special effects, that is, atomic commands
-- that only display special effects and don't change 'State'.
data SfxAtomic =
    SfxStrike ActorId ActorId ItemId CStore
  | SfxRecoil ActorId ActorId ItemId CStore
  | SfxSteal ActorId ActorId ItemId CStore
  | SfxRelease ActorId ActorId ItemId CStore
  | SfxProject ActorId ItemId CStore
  | SfxReceive ActorId ItemId CStore
  | SfxApply ActorId ItemId CStore
  | SfxCheck ActorId ItemId CStore
  | SfxTrigger ActorId Point
  | SfxShun ActorId Point
  | SfxEffect FactionId ActorId IK.Effect Int64
  | SfxMsgFid FactionId SfxMsg
  | SfxSortSlots
  | SfxCollideTile ActorId Point
  deriving (Show, Eq, Generic)

instance Binary SfxAtomic

-- | Symbolic representation of text messages sent by server to clients
-- and shown to players.
data SfxMsg =
    SfxUnexpected ReqFailure
  | SfxExpected Text ReqFailure
  | SfxLoudUpd Bool UpdAtomic
  | SfxLoudStrike Bool (ContentId ItemKind) Int
  | SfxLoudSummon Bool (GroupName ItemKind) Dice.Dice
  | SfxFizzles
  | SfxNothingHappens
  | SfxVoidDetection IK.DetectKind
  | SfxUnimpressed ActorId
  | SfxSummonLackCalm ActorId
  | SfxLevelNoMore
  | SfxLevelPushed
  | SfxBracedImmune ActorId
  | SfxEscapeImpossible
  | SfxStasisProtects
  | SfxTransImpossible
  | SfxIdentifyNothing
  | SfxPurposeNothing CStore
  | SfxPurposeTooFew Int Int
  | SfxPurposeUnique
  | SfxPurposeNotCommon
  | SfxColdFish
  | SfxTimerExtended LevelId ActorId ItemId CStore
  | SfxCollideActor LevelId ActorId ActorId
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
  UpdSpotItemBag c bag ais -> Just $ UpdLoseItemBag c bag ais
  UpdLoseItemBag c bag ais -> Just $ UpdSpotItemBag c bag ais
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
  UpdAlterGold delta -> Just $ UpdAlterGold (-delta)
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
  UpdDiscover c iid ik aspectRecord -> Just $ UpdCover c iid ik aspectRecord
  UpdCover c iid ik aspectRecord -> Just $ UpdDiscover c iid ik aspectRecord
  UpdDiscoverKind c ix ik -> Just $ UpdCoverKind c ix ik
  UpdCoverKind c ix ik -> Just $ UpdDiscoverKind c ix ik
  UpdDiscoverAspect c iid aspectRecord ->
    Just $ UpdCoverAspect c iid aspectRecord
  UpdCoverAspect c iid aspectRecord ->
    Just $ UpdDiscoverAspect c iid aspectRecord
  UpdDiscoverServer iid aspectRecord -> Just $ UpdCoverServer iid aspectRecord
  UpdCoverServer iid aspectRecord -> Just $ UpdDiscoverServer iid aspectRecord
  UpdPerception lid outPer inPer -> Just $ UpdPerception lid inPer outPer
  UpdRestart{} -> Just cmd  -- here history ends; change direction
  UpdRestartServer{} -> Just cmd  -- here history ends; change direction
  UpdResume{} -> Nothing
  UpdResumeServer{} -> Nothing
  UpdKillExit{} -> Nothing
  UpdWriteSave -> Nothing

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
  SfxSortSlots -> cmd
  SfxCollideTile{} -> cmd

undoCmdAtomic :: CmdAtomic -> Maybe CmdAtomic
undoCmdAtomic (UpdAtomic cmd) = UpdAtomic <$> undoUpdAtomic cmd
undoCmdAtomic (SfxAtomic sfx) = Just $ SfxAtomic $ undoSfxAtomic sfx
