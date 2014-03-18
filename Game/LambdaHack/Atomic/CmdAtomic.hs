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
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Atomic.CmdAtomic
  ( CmdAtomic(..), UpdAtomic(..), SfxAtomic(..), HitAtomic(..)
  , undoUpdAtomic, undoSfxAtomic, undoCmdAtomic
  ) where

import Data.Binary
import Data.Text (Text)
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ItemKind as ItemKind
import Game.LambdaHack.Content.TileKind as TileKind

data CmdAtomic =
    UpdAtomic !UpdAtomic
  | SfxAtomic !SfxAtomic
  deriving (Show, Eq, Generic)

instance Binary CmdAtomic

-- | Abstract syntax of atomic commands.
data UpdAtomic =
  -- Create/destroy actors and items.
    UpdCreateActor !ActorId !Actor ![(ItemId, Item)]
  | UpdDestroyActor !ActorId !Actor ![(ItemId, Item)]
  | UpdCreateItem !ItemId !Item !Int !Container
  | UpdDestroyItem !ItemId !Item !Int !Container
  | UpdSpotActor !ActorId !Actor ![(ItemId, Item)]
  | UpdLoseActor !ActorId !Actor ![(ItemId, Item)]
  | UpdSpotItem !ItemId !Item !Int !Container
  | UpdLoseItem !ItemId !Item !Int !Container
  -- Move actors and items.
  | UpdMoveActor !ActorId !Point !Point
  | UpdWaitActor !ActorId !Bool !Bool
  | UpdDisplaceActor !ActorId !ActorId
  | UpdMoveItem !ItemId !Int !Container !Container
  -- Change actor attributes.
  | UpdAgeActor !ActorId !Time
  | UpdHealActor !ActorId !Int
  | UpdCalmActor !ActorId !Int
  | UpdHasteActor !ActorId !Speed
  | UpdTrajectoryActor !ActorId !(Maybe [Vector]) !(Maybe [Vector])
  | UpdColorActor !ActorId !Color.Color !Color.Color
  -- Change faction attributes.
  | UpdQuitFaction !FactionId !(Maybe Actor) !(Maybe Status) !(Maybe Status)
  | UpdLeadFaction !FactionId !(Maybe ActorId) !(Maybe ActorId)
  | UpdDiplFaction !FactionId !FactionId !Diplomacy !Diplomacy
  | UpdAutoFaction !FactionId !Bool
  | UpdRecordKill !ActorId !Int
  -- Alter map.
  | UpdAlterTile !LevelId !Point !(Kind.Id TileKind) !(Kind.Id TileKind)
  | UpdSearchTile !ActorId !Point !(Kind.Id TileKind) !(Kind.Id TileKind)
  | UpdSpotTile !LevelId ![(Point, Kind.Id TileKind)]
  | UpdLoseTile !LevelId ![(Point, Kind.Id TileKind)]
  | UpdAlterSmell !LevelId !Point !(Maybe Time) !(Maybe Time)
  | UpdSpotSmell !LevelId ![(Point, Time)]
  | UpdLoseSmell !LevelId ![(Point, Time)]
  -- Assorted.
  | UpdAgeGame !Time ![LevelId]
  | UpdDiscover !LevelId !Point !ItemId !(Kind.Id ItemKind)
  | UpdCover !LevelId !Point !ItemId !(Kind.Id ItemKind)
  | UpdPerception !LevelId !Perception !Perception
  | UpdRestart !FactionId !Discovery !FactionPers !State !DebugModeCli !Text
  | UpdRestartServer !State
  | UpdResume !FactionId !FactionPers
  | UpdResumeServer !State
  | UpdKillExit !FactionId
  | UpdSaveBkp
  | UpdMsgAll !Msg
  | UpdRecordHistory !FactionId
  deriving (Show, Eq, Generic)

instance Binary UpdAtomic

data SfxAtomic =
    SfxStrike !ActorId !ActorId !Item !HitAtomic
  | SfxRecoil !ActorId !ActorId !Item !HitAtomic
  | SfxProject !ActorId !ItemId
  | SfxCatch !ActorId !ItemId
  | SfxActivate !ActorId !ItemId
  | SfxCheck !ActorId !ItemId
  | SfxTrigger !ActorId !Point !F.Feature
  | SfxShun !ActorId !Point !F.Feature
  | SfxEffect !ActorId !(Effect.Effect Int)
  | SfxMsgFid !FactionId !Msg
  | SfxMsgAll !Msg
  | SfxActorStart !ActorId
  deriving (Show, Eq, Generic)

instance Binary SfxAtomic

data HitAtomic = Hit | HitBlock | MissBlock
  deriving (Show, Eq, Generic)

instance Binary HitAtomic

undoUpdAtomic :: UpdAtomic -> Maybe UpdAtomic
undoUpdAtomic cmd = case cmd of
  UpdCreateActor aid body ais -> Just $ UpdDestroyActor aid body ais
  UpdDestroyActor aid body ais -> Just $ UpdCreateActor aid body ais
  UpdCreateItem iid item k c -> Just $ UpdDestroyItem iid item k c
  UpdDestroyItem iid item k c -> Just $ UpdCreateItem iid item k c
  UpdSpotActor aid body ais -> Just $ UpdLoseActor aid body ais
  UpdLoseActor aid body ais -> Just $ UpdSpotActor aid body ais
  UpdSpotItem iid item k c -> Just $ UpdLoseItem iid item k c
  UpdLoseItem iid item k c -> Just $ UpdSpotItem iid item k c
  UpdMoveActor aid fromP toP -> Just $ UpdMoveActor aid toP fromP
  UpdWaitActor aid fromWait toWait -> Just $ UpdWaitActor aid toWait fromWait
  UpdDisplaceActor source target -> Just $ UpdDisplaceActor target source
  UpdMoveItem iid k c1 c2 -> Just $ UpdMoveItem iid k c2 c1
  UpdAgeActor aid t -> Just $ UpdAgeActor aid (timeNegate t)
  UpdHealActor aid n -> Just $ UpdHealActor aid (-n)
  UpdCalmActor aid n -> Just $ UpdCalmActor aid (-n)
  UpdHasteActor aid delta -> Just $ UpdHasteActor aid (speedNegate delta)
  UpdTrajectoryActor aid fromT toT -> Just $ UpdTrajectoryActor aid toT fromT
  UpdColorActor aid fromCol toCol -> Just $ UpdColorActor aid toCol fromCol
  UpdQuitFaction fid mb fromSt toSt -> Just $ UpdQuitFaction fid mb toSt fromSt
  UpdLeadFaction fid source target -> Just $ UpdLeadFaction fid target source
  UpdDiplFaction fid1 fid2 fromDipl toDipl ->
    Just $ UpdDiplFaction fid1 fid2 toDipl fromDipl
  UpdAutoFaction fid st -> Just $ UpdAutoFaction fid (not st)
  UpdRecordKill aid k -> Just $ UpdRecordKill aid (-k)
  UpdAlterTile lid p fromTile toTile ->
    Just $ UpdAlterTile lid p toTile fromTile
  UpdSearchTile aid p fromTile toTile ->
    Just $ UpdSearchTile aid p toTile fromTile
  UpdSpotTile lid ts -> Just $ UpdLoseTile lid ts
  UpdLoseTile lid ts -> Just $ UpdSpotTile lid ts
  UpdAlterSmell lid p fromSm toSm -> Just $ UpdAlterSmell lid p toSm fromSm
  UpdSpotSmell lid sms -> Just $ UpdLoseSmell lid sms
  UpdLoseSmell lid sms -> Just $ UpdSpotSmell lid sms
  UpdAgeGame t lids -> Just $ UpdAgeGame (timeNegate t) lids
  UpdDiscover lid p iid ik -> Just $ UpdCover lid p iid ik
  UpdCover lid p iid ik -> Just $ UpdDiscover lid p iid ik
  UpdPerception lid outPer inPer -> Just $ UpdPerception lid inPer outPer
  UpdRestart{} -> Just cmd  -- here history ends; change direction
  UpdRestartServer{} -> Just cmd  -- here history ends; change direction
  UpdResume{} -> Nothing
  UpdResumeServer{} -> Nothing
  UpdKillExit{} -> Nothing
  UpdSaveBkp -> Nothing
  UpdMsgAll{} -> Nothing  -- only generated by @cmdAtomicFilterCli@
  UpdRecordHistory{} -> Just cmd

undoSfxAtomic :: SfxAtomic -> SfxAtomic
undoSfxAtomic cmd = case cmd of
  SfxStrike source target item b -> SfxRecoil source target item b
  SfxRecoil source target item b -> SfxStrike source target item b
  SfxProject aid iid -> SfxCatch aid iid
  SfxCatch aid iid -> SfxProject aid iid
  SfxActivate aid iid -> SfxCheck aid iid
  SfxCheck aid iid -> SfxActivate aid iid
  SfxTrigger aid p feat -> SfxShun aid p feat
  SfxShun aid p feat -> SfxTrigger aid p feat
  SfxEffect{} -> cmd  -- not ideal?
  SfxMsgFid{} -> cmd
  SfxMsgAll{} -> cmd
  SfxActorStart{} -> cmd

undoCmdAtomic :: CmdAtomic -> Maybe CmdAtomic
undoCmdAtomic (UpdAtomic cmd) = fmap UpdAtomic $ undoUpdAtomic cmd
undoCmdAtomic (SfxAtomic sfx) = Just $ SfxAtomic $ undoSfxAtomic sfx
