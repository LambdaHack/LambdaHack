-- | A set of atomic commands. These are the largest building blocks
-- that have no components that can be observed in isolation.
-- We also try to make them respect the laws of energy and mass conservation.
-- So, e.g., item removed from inventory is not an atomic commands,
-- but item dropped from the inventory to the ground is. This makes
-- it easier to undo the commands. In principle, the commands are the only
-- way to affect game state. Clients should be sent state updates
-- after each atomic command they can observe.
module Game.LambdaHack.Server.CmdAtomic
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
  | SetSmellAtomic SmellMap SmellMap
  | AlterPathAtomic ActorId (Maybe [Vector]) (Maybe [Vector])
  | ColorActorAtomic ActorId (Maybe Color.Color) (Maybe Color.Color)
  | SyncAtomic
  deriving Show

undoCmdAtomic :: CmdAtomic -> CmdAtomic
undoCmdAtomic cmd = case cmd of
  HealAtomic n aid -> HealAtomic (-n) aid
  HasteAtomic aid delta -> HasteAtomic aid (speedNegate delta)
  DominateAtomic fromFaction toFaction target ->
    DominateAtomic toFaction fromFaction target
  SpawnAtomic aid body -> KillAtomic aid body
  KillAtomic aid body -> SpawnAtomic aid body
  CreateItemAtomic iid item k container ->
    DestroyItemAtomic iid item k container
  DestroyItemAtomic iid item k container ->
    CreateItemAtomic iid item k container
  MoveItemAtomic iid k c1 c2 -> MoveItemAtomic iid k c2 c1
  WaitAtomic actor fromWait toWait -> WaitAtomic actor toWait fromWait
  ChangeTileAtomic p fromTile toTile -> ChangeTileAtomic p toTile fromTile
  MoveActorAtomic aid fromP toP -> MoveActorAtomic aid toP fromP
  DisplaceActorAtomic source target -> DisplaceActorAtomic target source
  AlterSecretAtomic diffL -> AlterSecretAtomic $ map (second swap) diffL
  AlterSmellAtomic diffL -> AlterSmellAtomic $ map (second swap) diffL
  SetSmellAtomic fromSmell toSmell -> SetSmellAtomic toSmell fromSmell
  AlterPathAtomic aid fromPath toPath -> AlterPathAtomic aid toPath fromPath
  ColorActorAtomic aid fromColor toColor ->
    ColorActorAtomic aid toColor fromColor
  SyncAtomic -> SyncAtomic
