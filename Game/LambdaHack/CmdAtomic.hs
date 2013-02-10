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
  ( CmdAtomic(..), undoCmdAtomic
  ) where

import Control.Arrow (second)
import Data.Tuple (swap)

import Game.LambdaHack.Actor
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Content.TileKind as TileKind
import Game.LambdaHack.Faction
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Misc
import Game.LambdaHack.Point
import Game.LambdaHack.Time
import Game.LambdaHack.Vector

-- | Abstract syntax of atomic commands.
data CmdAtomic =
    HealAtomic Int ActorId
  | HasteAtomic ActorId Speed
  | DominateAtomic FactionId FactionId ActorId
  | SpawnAtomic ActorId Actor
  | KillAtomic ActorId Actor
  | CreateItemAtomic ItemId Item Int Container
  | DestroyItemAtomic ItemId Item Int Container
  | MoveItemAtomic ItemId Int Container Container
  | WaitAtomic ActorId Time Time
  | ChangeTileAtomic Point (Kind.Id TileKind) (Kind.Id TileKind)
  | MoveActorAtomic ActorId Point Point
  | DisplaceActorAtomic ActorId ActorId
  | AlterSecretAtomic (DiffEM Point Time)
  | AlterSmellAtomic (DiffEM Point Time)
  | AlterPathAtomic ActorId (Maybe [Vector]) (Maybe [Vector])
  | ColorActorAtomic ActorId (Maybe Color.Color) (Maybe Color.Color)
  | FactionQuitAtomic FactionId (Maybe (Bool, Status)) (Maybe (Bool, Status))
  | SyncAtomic
  deriving Show

undoCmdAtomic :: CmdAtomic -> CmdAtomic
undoCmdAtomic cmd = case cmd of
  HealAtomic n aid -> HealAtomic (-n) aid
  HasteAtomic aid delta -> HasteAtomic aid (speedNegate delta)
  DominateAtomic fromFid toFid target -> DominateAtomic toFid fromFid target
  SpawnAtomic aid body -> KillAtomic aid body
  KillAtomic aid body -> SpawnAtomic aid body
  CreateItemAtomic iid item k c -> DestroyItemAtomic iid item k c
  DestroyItemAtomic iid item k c -> CreateItemAtomic iid item k c
  MoveItemAtomic iid k c1 c2 -> MoveItemAtomic iid k c2 c1
  WaitAtomic actor fromWait toWait -> WaitAtomic actor toWait fromWait
  ChangeTileAtomic p fromTile toTile -> ChangeTileAtomic p toTile fromTile
  MoveActorAtomic aid fromP toP -> MoveActorAtomic aid toP fromP
  DisplaceActorAtomic source target -> DisplaceActorAtomic target source
  AlterSecretAtomic diffL -> AlterSecretAtomic $ map (second swap) diffL
  AlterSmellAtomic diffL -> AlterSmellAtomic $ map (second swap) diffL
  AlterPathAtomic aid fromPath toPath -> AlterPathAtomic aid toPath fromPath
  ColorActorAtomic aid fromCol toCol -> ColorActorAtomic aid toCol fromCol
  FactionQuitAtomic fid fromSt toSt -> FactionQuitAtomic fid toSt fromSt
  SyncAtomic -> SyncAtomic
