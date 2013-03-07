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
module Game.LambdaHack.CmdAtomic
  ( tellCmdAtomic, tellDescAtomic
  , Atomic(..), CmdAtomic(..), DescAtomic(..), HitAtomic(..)
  , undoCmdAtomic, undoDescAtomic, undoAtomic
  ) where

import Control.Arrow (second)
import Control.Monad.Writer.Strict (WriterT, tell)
import Data.Tuple (swap)

import Game.LambdaHack.Actor
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Content.ItemKind as ItemKind
import Game.LambdaHack.Content.TileKind as TileKind
import qualified Game.LambdaHack.Effect as Effect
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Misc
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.State
import Game.LambdaHack.Time
import Game.LambdaHack.Vector

tellCmdAtomic :: Monad m => CmdAtomic -> WriterT [Atomic] m ()
tellCmdAtomic cmd = tell [CmdAtomic cmd]

tellDescAtomic :: Monad m => DescAtomic -> WriterT [Atomic] m ()
tellDescAtomic desc = tell [DescAtomic desc]

data Atomic =
    CmdAtomic CmdAtomic
  | DescAtomic DescAtomic
  deriving (Show, Eq)

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
  | DominateActorA ActorId FactionId FactionId
  | PathActorA ActorId (Maybe [Vector]) (Maybe [Vector])
  | ColorActorA ActorId (Maybe Color.Color) (Maybe Color.Color)
  -- Change faction attributes.
  | QuitFactionA FactionId (Maybe (Bool, Status)) (Maybe (Bool, Status))
  | LeadFactionA FactionId (Maybe ActorId) (Maybe ActorId)
  -- Alter map.
  | AlterTileA LevelId Point (Kind.Id TileKind) (Kind.Id TileKind)
  | SpotTileA LevelId [(Point, Kind.Id TileKind)]
  | LoseTileA LevelId [(Point, Kind.Id TileKind)]
  | AlterSecretA LevelId (DiffEM Point Time)
  | AlterSmellA LevelId (DiffEM Point Time)
  -- Assorted.
  | AgeLevelA LevelId Time
  | DiscoverA LevelId Point ItemId (Kind.Id ItemKind)
  | CoverA LevelId Point ItemId (Kind.Id ItemKind)
  | PerceptionA LevelId PerActor PerActor
  | RestartA FactionId Discovery FactionPers State
  | ResumeA FactionId FactionPers
  | SaveExitA
  | SaveBkpA
  deriving (Show, Eq)

data DescAtomic =
    StrikeD ActorId ActorId Item HitAtomic
  | RecoilD ActorId ActorId Item HitAtomic
  | ProjectD ActorId ItemId
  | CatchD ActorId ItemId
  | ActivateD ActorId ItemId
  | CheckD ActorId ItemId
  | TriggerD ActorId Point F.Feature Bool
  | ShunD ActorId Point F.Feature Bool
  | EffectD ActorId (Effect.Effect Int)
  | FailureD FactionId Msg
  | BroadcastD Msg
  | DisplayPushD FactionId
  | DisplayDelayD FactionId
  | FlushFramesD FactionId
  | FadeoutD FactionId Bool
  | FadeinD FactionId Bool
-- TODO: SearchA
  deriving (Show, Eq)

data HitAtomic = HitD | HitBlockD | MissBlockD
  deriving (Show, Eq)

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
  DominateActorA target fromFid toFid ->
    Just $ DominateActorA target toFid fromFid
  PathActorA aid fromPath toPath -> Just $ PathActorA aid toPath fromPath
  ColorActorA aid fromCol toCol -> Just $ ColorActorA aid toCol fromCol
  QuitFactionA fid fromSt toSt -> Just $ QuitFactionA fid toSt fromSt
  LeadFactionA fid source target -> Just $ LeadFactionA fid target source
  AlterTileA lid p fromTile toTile -> Just $ AlterTileA lid p toTile fromTile
  SpotTileA lid ts -> Just $ LoseTileA lid ts
  LoseTileA lid ts -> Just $ SpotTileA lid ts
  AlterSecretA lid diffL -> Just $ AlterSecretA lid $ map (second swap) diffL
  AlterSmellA lid diffL -> Just $ AlterSmellA lid $ map (second swap) diffL
  AgeLevelA lid t -> Just $ AgeLevelA lid (timeNegate t)
  DiscoverA lid p iid ik -> Just $ CoverA lid p iid ik
  CoverA lid p iid ik -> Just $ DiscoverA lid p iid ik
  PerceptionA lid outPer inPer -> Just $ PerceptionA lid inPer outPer
  RestartA{} -> Just $ cmd  -- here history ends; change direction
  ResumeA{} -> Nothing
  SaveExitA -> Nothing
  SaveBkpA -> Nothing

undoDescAtomic :: DescAtomic -> DescAtomic
undoDescAtomic cmd = case cmd of
  StrikeD source target item b -> RecoilD source target item b
  RecoilD source target item b -> StrikeD source target item b
  ProjectD aid iid -> CatchD aid iid
  CatchD aid iid -> ProjectD aid iid
  ActivateD aid iid -> CheckD aid iid
  CheckD aid iid -> ActivateD aid iid
  TriggerD aid p feat b -> ShunD aid p feat b
  ShunD aid p feat b -> TriggerD aid p feat b
  EffectD{} -> cmd  -- not ideal?
  FailureD{} -> cmd
  BroadcastD{} -> cmd
  DisplayPushD{} -> cmd
  DisplayDelayD{} -> cmd
  FlushFramesD{} -> cmd
  FadeoutD{} -> cmd
  FadeinD{} -> cmd

undoAtomic :: Atomic -> Maybe Atomic
undoAtomic (CmdAtomic cmd) = fmap CmdAtomic $ undoCmdAtomic cmd
undoAtomic (DescAtomic desc) = Just $ DescAtomic $ undoDescAtomic desc
