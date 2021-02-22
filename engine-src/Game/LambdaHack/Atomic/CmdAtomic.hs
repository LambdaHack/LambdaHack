-- | A set of atomic commands shared by client and server.
-- These are the largest building blocks that have no components
-- that can be observed in isolation.
--
-- We try to make atomic commands respect the laws of energy and mass
-- conservation, unless they really can't, e.g., monster spawning.
-- For example item removal from equipment, in isolation, is not an atomic
-- command, but item dropped from equipment to the ground is. This makes
-- it easier to undo the commands. In principle, the commands are the only
-- way to affect the basic game state ('State').
--
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Atomic.CmdAtomic
  ( CmdAtomic(..), UpdAtomic(..), HearMsg(..), SfxAtomic(..), SfxMsg(..)
  , undoUpdAtomic, undoSfxAtomic, undoCmdAtomic
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumSet as ES
import           Data.Int (Int64)
import qualified System.Random.SplitMix32 as SM

-- Dependence on ClientOptions is an anomaly. Instead, probably the raw
-- remaining commandline should be passed and parsed by the client to extract
-- client and ui options from and singnal an error if anything was left.

import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.Analytics
import           Game.LambdaHack.Common.ClientOptions
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Perception
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.ReqFailure
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Vector
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import qualified Game.LambdaHack.Content.PlaceKind as PK
import           Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Core.Dice as Dice
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs

-- | Abstract syntax of atomic commands, that is, atomic game state
-- transformations.
data CmdAtomic =
    UpdAtomic UpdAtomic  -- ^ atomic updates
  | SfxAtomic SfxAtomic  -- ^ atomic special effects
  deriving Show

--  deriving (Show, Eq, Generic)
--
-- instance Binary CmdAtomic

-- | Abstract syntax of atomic updates, that is, atomic commands
-- that really change the 'State'. Most of them are an encoding of a game
-- state diff, though they also carry some intentional hints
-- that help clients determine whether and how to communicate it to players.
data UpdAtomic =
  -- Create/destroy actors and items.
    UpdRegisterItems [(ItemId, Item)]
  | UpdCreateActor ActorId Actor [(ItemId, Item)]
  | UpdDestroyActor ActorId Actor [(ItemId, Item)]
  | UpdCreateItem Bool ItemId Item ItemQuant Container
  | UpdDestroyItem Bool ItemId Item ItemQuant Container
  | UpdSpotActor ActorId Actor
  | UpdLoseActor ActorId Actor
  | UpdSpotItem Bool ItemId ItemQuant Container
  | UpdLoseItem Bool ItemId ItemQuant Container
  | UpdSpotItemBag Bool Container ItemBag
  | UpdLoseItemBag Bool Container ItemBag
  -- Move actors and items.
  | UpdMoveActor ActorId Point Point
  | UpdWaitActor ActorId Watchfulness Watchfulness
  | UpdDisplaceActor ActorId ActorId
  | UpdMoveItem ItemId Int ActorId CStore CStore
  -- Change actor attributes.
  | UpdRefillHP ActorId Int64
  | UpdRefillCalm ActorId Int64
  | UpdTrajectory ActorId (Maybe ([Vector], Speed)) (Maybe ([Vector], Speed))
  -- Change faction attributes.
  | UpdQuitFaction FactionId (Maybe Status) (Maybe Status)
                   (Maybe (FactionAnalytics, GenerationAnalytics))
  | UpdSpotStashFaction Bool FactionId LevelId Point
  | UpdLoseStashFaction Bool FactionId LevelId Point
  | UpdLeadFaction FactionId (Maybe ActorId) (Maybe ActorId)
  | UpdDiplFaction FactionId FactionId Diplomacy Diplomacy
  | UpdDoctrineFaction FactionId Ability.Doctrine Ability.Doctrine
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
  | UpdSpotEntry LevelId [(Point, PK.PlaceEntry)]
  | UpdLoseEntry LevelId [(Point, PK.PlaceEntry)]
  | UpdAlterSmell LevelId Point Time Time
  | UpdSpotSmell LevelId [(Point, Time)]
  | UpdLoseSmell LevelId [(Point, Time)]
  -- Assorted.
  | UpdTimeItem ItemId Container ItemTimers ItemTimers
  | UpdAgeGame (ES.EnumSet LevelId)
  | UpdUnAgeGame (ES.EnumSet LevelId)
  | UpdDiscover Container ItemId (ContentId ItemKind) IA.AspectRecord
      -- Here and below @Container@ is only used for presentation
      -- and when @CStash@ is not visible, but the item is, it won't
      -- break anything, because item identification is not registered globally.
  | UpdCover Container ItemId (ContentId ItemKind) IA.AspectRecord
  | UpdDiscoverKind Container ItemKindIx (ContentId ItemKind)
  | UpdCoverKind Container ItemKindIx (ContentId ItemKind)
  | UpdDiscoverAspect Container ItemId IA.AspectRecord
  | UpdCoverAspect Container ItemId IA.AspectRecord
  | UpdDiscoverServer ItemId IA.AspectRecord
  | UpdCoverServer ItemId IA.AspectRecord
  | UpdPerception LevelId Perception Perception
  | UpdRestart FactionId PerLid State Challenge ClientOptions SM.SMGen
  | UpdRestartServer State
  | UpdResume FactionId PerLid
  | UpdResumeServer State
  | UpdKillExit FactionId
  | UpdWriteSave
  | UpdHearFid FactionId (Maybe Int) HearMsg
      -- in @UpdAtomic@ to let AI analyze and count
  deriving Show

-- | Symbolic representation of text messages about heard noises,
-- sent by server to clients and shown to players and used by AI.
data HearMsg =
    HearUpd UpdAtomic
  | HearStrike (ContentId ItemKind)
  | HearSummon Bool (GroupName ItemKind) Dice.Dice
  | HearCollideTile
  | HearTaunt Text
  deriving Show

-- | Abstract syntax of atomic special effects, that is, atomic commands
-- that only display special effects and don't change 'State' nor client state.
data SfxAtomic =
    SfxStrike ActorId ActorId ItemId
  | SfxRecoil ActorId ActorId ItemId
  | SfxSteal ActorId ActorId ItemId
  | SfxRelease ActorId ActorId ItemId
  | SfxProject ActorId ItemId
  | SfxReceive ActorId ItemId
  | SfxApply ActorId ItemId
  | SfxCheck ActorId ItemId
  | SfxTrigger ActorId LevelId Point (ContentId TileKind)
  | SfxShun ActorId LevelId Point (ContentId TileKind)
  | SfxEffect FactionId ActorId ItemId IK.Effect Int64
  | SfxMsgFid FactionId SfxMsg
  | SfxRestart
  | SfxCollideTile ActorId Point
  | SfxTaunt Bool ActorId
  deriving Show

-- | Symbolic representation of text messages sent by server to clients
-- and shown to players.
data SfxMsg =
    SfxUnexpected ReqFailure
  | SfxExpected Text ReqFailure
  | SfxExpectedEmbed ItemId LevelId ReqFailure
  | SfxFizzles
  | SfxNothingHappens
  | SfxNoItemsForTile [[(Int, GroupName ItemKind)]]
  | SfxVoidDetection IK.DetectKind
  | SfxUnimpressed ActorId
  | SfxSummonLackCalm ActorId
  | SfxSummonTooManyOwn ActorId
  | SfxSummonTooManyAll ActorId
  | SfxSummonFailure ActorId
  | SfxLevelNoMore
  | SfxLevelPushed
  | SfxBracedImmune ActorId
  | SfxEscapeImpossible
  | SfxStasisProtects
  | SfxWaterParalysisResisted
  | SfxTransImpossible
  | SfxIdentifyNothing
  | SfxPurposeNothing
  | SfxPurposeTooFew Int Int
  | SfxPurposeUnique
  | SfxPurposeNotCommon
  | SfxRerollNothing
  | SfxRerollNotRandom
  | SfxDupNothing
  | SfxDupUnique
  | SfxDupValuable
  | SfxColdFish
  | SfxReadyGoods
  | SfxTimerExtended ActorId ItemId CStore (Delta Time)
      -- This @CStore@ is only printed, so even @CStash@ is safe.
  | SfxCollideActor ActorId ActorId
  | SfxItemYield ItemId Int LevelId
  deriving Show

undoUpdAtomic :: UpdAtomic -> Maybe UpdAtomic
undoUpdAtomic cmd = case cmd of
  UpdRegisterItems{} -> Nothing  -- harmless and never forgotten
  UpdCreateActor aid body ais -> Just $ UpdDestroyActor aid body ais
  UpdDestroyActor aid body ais -> Just $ UpdCreateActor aid body ais
  UpdCreateItem verbose iid item k c ->
    Just $ UpdDestroyItem verbose iid item k c
  UpdDestroyItem verbose iid item k c ->
    Just $ UpdCreateItem verbose iid item k c
  UpdSpotActor aid body -> Just $ UpdLoseActor aid body
  UpdLoseActor aid body -> Just $ UpdSpotActor aid body
  UpdSpotItem verbose iid k c -> Just $ UpdLoseItem verbose iid k c
  UpdLoseItem verbose iid k c -> Just $ UpdSpotItem verbose iid k c
  UpdSpotItemBag verbose c bag -> Just $ UpdLoseItemBag verbose c bag
  UpdLoseItemBag verbose c bag -> Just $ UpdSpotItemBag verbose c bag
  UpdMoveActor aid fromP toP -> Just $ UpdMoveActor aid toP fromP
  UpdWaitActor aid fromWS toWS -> Just $ UpdWaitActor aid toWS fromWS
  UpdDisplaceActor source target -> Just $ UpdDisplaceActor target source
  UpdMoveItem iid k aid store1 store2 ->
    Just $ UpdMoveItem iid k aid store2 store1
  UpdRefillHP aid n -> Just $ UpdRefillHP aid (-n)
  UpdRefillCalm aid n -> Just $ UpdRefillCalm aid (-n)
  UpdTrajectory aid fromT toT -> Just $ UpdTrajectory aid toT fromT
  UpdQuitFaction fid fromSt toSt manalytics ->
    Just $ UpdQuitFaction fid toSt fromSt manalytics
  UpdSpotStashFaction verbose fid lid pos ->
    Just $ UpdLoseStashFaction verbose fid lid pos
  UpdLoseStashFaction verbose fid lid pos ->
    Just $ UpdSpotStashFaction verbose fid lid pos
  UpdLeadFaction fid source target -> Just $ UpdLeadFaction fid target source
  UpdDiplFaction fid1 fid2 fromDipl toDipl ->
    Just $ UpdDiplFaction fid1 fid2 toDipl fromDipl
  UpdDoctrineFaction fid toT fromT -> Just $ UpdDoctrineFaction fid fromT toT
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
  UpdSpotEntry lid ts -> Just $ UpdLoseEntry lid ts
  UpdLoseEntry lid ts -> Just $ UpdSpotEntry lid ts
  UpdAlterSmell lid p fromSm toSm -> Just $ UpdAlterSmell lid p toSm fromSm
  UpdSpotSmell lid sms -> Just $ UpdLoseSmell lid sms
  UpdLoseSmell lid sms -> Just $ UpdSpotSmell lid sms
  UpdTimeItem iid c fromIt toIt -> Just $ UpdTimeItem iid c toIt fromIt
  UpdAgeGame lids -> Just $ UpdUnAgeGame lids
  UpdUnAgeGame lids -> Just $ UpdAgeGame lids
  UpdDiscover c iid ik arItem -> Just $ UpdCover c iid ik arItem
  UpdCover c iid ik arItem -> Just $ UpdDiscover c iid ik arItem
  UpdDiscoverKind c ix ik -> Just $ UpdCoverKind c ix ik
  UpdCoverKind c ix ik -> Just $ UpdDiscoverKind c ix ik
  UpdDiscoverAspect c iid arItem -> Just $ UpdCoverAspect c iid arItem
  UpdCoverAspect c iid arItem -> Just $ UpdDiscoverAspect c iid arItem
  UpdDiscoverServer iid arItem -> Just $ UpdCoverServer iid arItem
  UpdCoverServer iid arItem -> Just $ UpdDiscoverServer iid arItem
  UpdPerception lid outPer inPer -> Just $ UpdPerception lid inPer outPer
  UpdRestart{} -> Just cmd  -- here history ends; change direction
  UpdRestartServer{} -> Just cmd  -- here history ends; change direction
  UpdResume{} -> Nothing
  UpdResumeServer{} -> Nothing
  UpdKillExit{} -> Nothing
  UpdWriteSave -> Nothing
  UpdHearFid{} -> Nothing

undoSfxAtomic :: SfxAtomic -> SfxAtomic
undoSfxAtomic cmd = case cmd of
  SfxStrike source target iid -> SfxRecoil source target iid
  SfxRecoil source target iid -> SfxStrike source target iid
  SfxSteal source target iid -> SfxRelease source target iid
  SfxRelease source target iid -> SfxSteal source target iid
  SfxProject aid iid -> SfxReceive aid iid
  SfxReceive aid iid -> SfxProject aid iid
  SfxApply aid iid -> SfxCheck aid iid
  SfxCheck aid iid -> SfxApply aid iid
  SfxTrigger aid lid p tile -> SfxShun aid lid p tile
  SfxShun aid lid p tile -> SfxTrigger aid lid p tile
  SfxEffect{} -> cmd  -- not ideal?
  SfxMsgFid{} -> cmd
  SfxRestart -> cmd
  SfxCollideTile{} -> cmd
  SfxTaunt{} -> cmd

undoCmdAtomic :: CmdAtomic -> Maybe CmdAtomic
undoCmdAtomic (UpdAtomic cmd) = UpdAtomic <$> undoUpdAtomic cmd
undoCmdAtomic (SfxAtomic sfx) = Just $ SfxAtomic $ undoSfxAtomic sfx
