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
module Game.LambdaHack.Common.AtomicCmd
  ( Atomic(..), CmdAtomic(..), SfxAtomic(..), HitAtomic(..)
  , undoCmdAtomic, undoSfxAtomic, undoAtomic
  ) where

import Data.Binary
import Data.Text (Text)
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Actor
import qualified Game.LambdaHack.Common.Color as Color
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ItemKind as ItemKind
import Game.LambdaHack.Content.TileKind as TileKind

data Atomic =
    CmdAtomic CmdAtomic
  | SfxAtomic SfxAtomic
  deriving (Show, Eq, Generic)

instance Binary Atomic

-- | Abstract syntax of atomic commands.
data CmdAtomic =
  -- Create/destroy actors and items.
    CreateActorA ActorId Actor [(ItemId, Item)]
  | DestroyActorA ActorId Actor [(ItemId, Item)]
  | CreateItemA ItemId Item Int Container
  | DestroyItemA ItemId Item Int Container
  | SpotActorA ActorId Actor [(ItemId, Item)]
  | LoseActorA ActorId Actor [(ItemId, Item)]
  | SpotItemA ItemId Item Int Container
  | LoseItemA ItemId Item Int Container
  -- Move actors and items.
  | MoveActorA ActorId Point Point
  | WaitActorA ActorId Time Time
  | DisplaceActorA ActorId ActorId
  | MoveItemA ItemId Int Container Container
  -- Change actor attributes.
  | AgeActorA ActorId Time
  | HealActorA ActorId Int
  | HasteActorA ActorId Speed
  | PathActorA ActorId (Maybe [Vector]) (Maybe [Vector])
  | ColorActorA ActorId (Maybe Color.Color) (Maybe Color.Color)
  -- Change faction attributes.
  | QuitFactionA FactionId (Maybe Actor) (Maybe Status) (Maybe Status)
  | LeadFactionA FactionId (Maybe ActorId) (Maybe ActorId)
  | DiplFactionA FactionId FactionId Diplomacy Diplomacy
  -- Alter map.
  | AlterTileA LevelId Point (Kind.Id TileKind) (Kind.Id TileKind)
  | SearchTileA LevelId Point (Kind.Id TileKind) (Kind.Id TileKind)
  | SpotTileA LevelId [(Point, Kind.Id TileKind)]
  | LoseTileA LevelId [(Point, Kind.Id TileKind)]
  | AlterSmellA LevelId Point (Maybe Time) (Maybe Time)
  | SpotSmellA LevelId [(Point, Time)]
  | LoseSmellA LevelId [(Point, Time)]
  -- Assorted.
  | AgeLevelA LevelId Time
  | AgeGameA Time
  | DiscoverA LevelId Point ItemId (Kind.Id ItemKind)
  | CoverA LevelId Point ItemId (Kind.Id ItemKind)
  | PerceptionA LevelId PerActor PerActor
  | RestartA FactionId Discovery FactionPers State Bool Text
  | RestartServerA State
  | ResumeA FactionId FactionPers
  | ResumeServerA State
  | KillExitA FactionId
  | SaveExitA
  | SaveBkpA
  deriving (Show, Eq, Generic)

instance Binary CmdAtomic

data SfxAtomic =
    StrikeD ActorId ActorId Item HitAtomic
  | RecoilD ActorId ActorId Item HitAtomic
  | ProjectD ActorId ItemId
  | CatchD ActorId ItemId
  | ActivateD ActorId ItemId
  | CheckD ActorId ItemId
  | TriggerD ActorId Point F.Feature Bool
  | ShunD ActorId Point F.Feature Bool
  | EffectD ActorId (Effect.Effect Int)
  | MsgFidD FactionId Msg
  | MsgAllD Msg
  | DisplayPushD FactionId
  | DisplayDelayD FactionId
  deriving (Show, Eq, Generic)

instance Binary SfxAtomic

data HitAtomic = HitD | HitBlockD | MissBlockD
  deriving (Show, Eq, Generic)

instance Binary HitAtomic

undoCmdAtomic :: CmdAtomic -> Maybe CmdAtomic
undoCmdAtomic cmd = case cmd of
  CreateActorA aid body ais -> Just $ DestroyActorA aid body ais
  DestroyActorA aid body ais -> Just $ CreateActorA aid body ais
  CreateItemA iid item k c -> Just $ DestroyItemA iid item k c
  DestroyItemA iid item k c -> Just $ CreateItemA iid item k c
  SpotActorA aid body ais -> Just $ LoseActorA aid body ais
  LoseActorA aid body ais -> Just $ SpotActorA aid body ais
  SpotItemA iid item k c -> Just $ LoseItemA iid item k c
  LoseItemA iid item k c -> Just $ SpotItemA iid item k c
  MoveActorA aid fromP toP -> Just $ MoveActorA aid toP fromP
  WaitActorA aid fromWait toWait -> Just $ WaitActorA aid toWait fromWait
  DisplaceActorA source target -> Just $ DisplaceActorA target source
  MoveItemA iid k c1 c2 ->Just $  MoveItemA iid k c2 c1
  AgeActorA aid t -> Just $ AgeActorA aid (timeNegate t)
  HealActorA aid n -> Just $ HealActorA aid (-n)
  HasteActorA aid delta -> Just $ HasteActorA aid (speedNegate delta)
  PathActorA aid fromPath toPath -> Just $ PathActorA aid toPath fromPath
  ColorActorA aid fromCol toCol -> Just $ ColorActorA aid toCol fromCol
  QuitFactionA fid mb fromSt toSt -> Just $ QuitFactionA fid mb toSt fromSt
  LeadFactionA fid source target -> Just $ LeadFactionA fid target source
  DiplFactionA fid1 fid2 fromDipl toDipl ->
    Just $ DiplFactionA fid1 fid2 toDipl fromDipl
  AlterTileA lid p fromTile toTile -> Just $ AlterTileA lid p toTile fromTile
  SearchTileA lid p fromTile toTile -> Just $ SearchTileA lid p toTile fromTile
  SpotTileA lid ts -> Just $ LoseTileA lid ts
  LoseTileA lid ts -> Just $ SpotTileA lid ts
  AlterSmellA lid p fromSm toSm -> Just $ AlterSmellA lid p toSm fromSm
  SpotSmellA lid sms -> Just $ LoseSmellA lid sms
  LoseSmellA lid sms -> Just $ SpotSmellA lid sms
  AgeLevelA lid t -> Just $ AgeLevelA lid (timeNegate t)
  AgeGameA t -> Just $ AgeGameA (timeNegate t)
  DiscoverA lid p iid ik -> Just $ CoverA lid p iid ik
  CoverA lid p iid ik -> Just $ DiscoverA lid p iid ik
  PerceptionA lid outPer inPer -> Just $ PerceptionA lid inPer outPer
  RestartA{} -> Just cmd  -- here history ends; change direction
  RestartServerA{} -> Just cmd  -- here history ends; change direction
  ResumeA{} -> Nothing
  ResumeServerA{} -> Nothing
  KillExitA{} -> Nothing
  SaveExitA -> Nothing
  SaveBkpA -> Nothing

undoSfxAtomic :: SfxAtomic -> SfxAtomic
undoSfxAtomic cmd = case cmd of
  StrikeD source target item b -> RecoilD source target item b
  RecoilD source target item b -> StrikeD source target item b
  ProjectD aid iid -> CatchD aid iid
  CatchD aid iid -> ProjectD aid iid
  ActivateD aid iid -> CheckD aid iid
  CheckD aid iid -> ActivateD aid iid
  TriggerD aid p feat b -> ShunD aid p feat b
  ShunD aid p feat b -> TriggerD aid p feat b
  EffectD{} -> cmd  -- not ideal?
  MsgFidD{} -> cmd
  MsgAllD{} -> cmd
  DisplayPushD{} -> cmd
  DisplayDelayD{} -> cmd

undoAtomic :: Atomic -> Maybe Atomic
undoAtomic (CmdAtomic cmd) = fmap CmdAtomic $ undoCmdAtomic cmd
undoAtomic (SfxAtomic sfx) = Just $ SfxAtomic $ undoSfxAtomic sfx
