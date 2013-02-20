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
  , Atomic, CmdAtomic(..), DescAtomic(..), HitAtomic(..)
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
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.Time
import Game.LambdaHack.Vector

tellCmdAtomic :: Monad m => CmdAtomic -> WriterT [Atomic] m ()
tellCmdAtomic cmd = tell [Left cmd]

tellDescAtomic :: Monad m => DescAtomic -> WriterT [Atomic] m ()
tellDescAtomic desc = tell [Right desc]

type Atomic = Either CmdAtomic DescAtomic

-- | Abstract syntax of atomic commands.
data CmdAtomic =
  -- Create/destroy actors and items.
    CreateActorA ActorId Actor
  | DestroyActorA ActorId Actor
  | CreateItemA ItemId Item Int Container
  | DestroyItemA ItemId Item Int Container
  | SpotActorA ActorId Actor
  | LoseActorA ActorId Actor
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
  | SpotTileA LevelId [(Point, (Kind.Id TileKind, Kind.Id TileKind))]
  | AlterSecretA LevelId (DiffEM Point Time)
  | AlterSmellA LevelId (DiffEM Point Time)
  -- Assorted.
  | DiscoverA LevelId Point ItemId (Kind.Id ItemKind)
  | CoverA LevelId Point ItemId (Kind.Id ItemKind)
  | PerceptionA LevelId PActors PActors
  deriving (Show, Eq)

data DescAtomic =
    StrikeA ActorId ActorId Item HitAtomic
  | RecoilA ActorId ActorId Item HitAtomic
  | ProjectA ActorId ItemId
  | CatchA ActorId ItemId
  | ActivateA ActorId ItemId
  | CheckA ActorId ItemId
  | TriggerA ActorId Point F.Feature Bool
  | ShunA ActorId Point F.Feature Bool
  | EffectA ActorId Effect.Effect
-- TODO: SearchA
  deriving (Show, Eq)

data HitAtomic = HitA | HitBlockA | MissBlockA
  deriving (Show, Eq)

undoCmdAtomic :: CmdAtomic -> CmdAtomic
undoCmdAtomic cmd = case cmd of
  CreateActorA aid body -> DestroyActorA aid body
  DestroyActorA aid body -> CreateActorA aid body
  CreateItemA iid item k c -> DestroyItemA iid item k c
  DestroyItemA iid item k c -> CreateItemA iid item k c
  SpotActorA aid body -> LoseActorA aid body
  LoseActorA aid body -> SpotActorA aid body
  SpotItemA iid item k c -> LoseItemA iid item k c
  LoseItemA iid item k c -> SpotItemA iid item k c
  MoveActorA aid fromP toP -> MoveActorA aid toP fromP
  WaitActorA aid fromWait toWait -> WaitActorA aid toWait fromWait
  DisplaceActorA source target -> DisplaceActorA target source
  MoveItemA iid k c1 c2 -> MoveItemA iid k c2 c1
  AgeActorA aid t -> AgeActorA aid (timeNegate t)
  HealActorA aid n -> HealActorA aid (-n)
  HasteActorA aid delta -> HasteActorA aid (speedNegate delta)
  DominateActorA target fromFid toFid -> DominateActorA target toFid fromFid
  PathActorA aid fromPath toPath -> PathActorA aid toPath fromPath
  ColorActorA aid fromCol toCol -> ColorActorA aid toCol fromCol
  QuitFactionA fid fromSt toSt -> QuitFactionA fid toSt fromSt
  LeadFactionA fid source target -> LeadFactionA fid target source
  AlterTileA lid p fromTile toTile -> AlterTileA lid p toTile fromTile
  SpotTileA lid diff -> SpotTileA lid $ map (second swap) diff
  AlterSecretA lid diffL -> AlterSecretA lid $ map (second swap) diffL
  AlterSmellA lid diffL -> AlterSmellA lid $ map (second swap) diffL
  DiscoverA lid p iid ik -> CoverA lid p iid ik
  CoverA lid p iid ik -> DiscoverA lid p iid ik
  PerceptionA lid outPer inPer -> PerceptionA lid inPer outPer

undoDescAtomic :: DescAtomic -> DescAtomic
undoDescAtomic cmd = case cmd of
  StrikeA source target item b -> RecoilA source target item b
  RecoilA source target item b -> StrikeA source target item b
  ProjectA aid iid -> CatchA aid iid
  CatchA aid iid -> ProjectA aid iid
  ActivateA aid iid -> CheckA aid iid
  CheckA aid iid -> ActivateA aid iid
  TriggerA aid p feat b -> ShunA aid p feat b
  ShunA aid p feat b -> TriggerA aid p feat b
  EffectA aid effect -> EffectA aid effect  -- not ideal

undoAtomic :: Atomic -> Atomic
undoAtomic (Left cmd) = Left $ undoCmdAtomic cmd
undoAtomic (Right desc) = Right $ undoDescAtomic desc
-- TODO
--undoAtomic (Right (desc, as)) =
--  Right (undoDescAtomic desc, reverse $ map undoCmdAtomic as)
