-- | Abstract syntax of server commands.
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Common.Request
  ( Request(..), RequestTimed(..), aidOfRequest, aidOfRequestTimed
  , ReqFailure(..), showReqFailure
  ) where

import Data.Text (Text)

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Vector

-- | Abstract syntax of server commands.
data Request =
    ReqTimed !RequestTimed
  | ReqGameRestart !ActorId !Text !Int ![(Int, Text)]
  | ReqGameExit !ActorId !Int
  | ReqGameSave !ActorId
  | ReqAutomate !ActorId
  deriving (Show, Eq)

data RequestTimed =
    ReqMove !ActorId !Vector
  | ReqMelee !ActorId !ActorId
  | ReqDisplace !ActorId !ActorId
  | ReqAlter !ActorId !Point !(Maybe F.Feature)
  | ReqWait !ActorId
  | ReqMoveItem !ActorId !ItemId !Int !CStore !CStore
  | ReqProject !ActorId !Point !Int !ItemId !CStore
  | ReqApply !ActorId !ItemId !CStore
  | ReqTrigger !ActorId !(Maybe F.Feature)
  | ReqPongHack [CmdAtomic]
  deriving (Show, Eq)

-- | The actor that starts performing the command (may be dead, after
-- the command is performed).
aidOfRequest :: Request -> ActorId
aidOfRequest cmd = case cmd of
  ReqTimed cmd2 -> aidOfRequestTimed cmd2
  ReqGameRestart aid _ _ _ -> aid
  ReqGameExit aid _ -> aid
  ReqGameSave aid -> aid
  ReqAutomate aid -> aid

aidOfRequestTimed :: RequestTimed -> ActorId
aidOfRequestTimed cmd = case cmd of
  ReqMove aid _ -> aid
  ReqMelee aid _ -> aid
  ReqDisplace aid _ -> aid
  ReqAlter aid _ _ -> aid
  ReqWait aid -> aid
  ReqMoveItem aid _ _ _ _ -> aid
  ReqProject aid _ _ _ _ -> aid
  ReqApply aid _ _ -> aid
  ReqTrigger aid _ -> aid
  ReqPongHack _ -> toEnum (-1)  -- needed for --sniffIn

data ReqFailure =
    MoveNothing
  | MeleeSelf
  | MeleeDistant
  | DisplaceDistant
  | DisplaceAccess
  | DisplaceProjectiles
  | DisplaceDying
  | DisplaceSupported
  | AlterDistant
  | AlterBlockActor
  | AlterBlockItem
  | AlterNothing
  | PickupOverfull
  | ItemNothing
  | ItemNotCalm
  | ProjectAimOnself
  | ProjectBlockTerrain
  | ProjectBlockActor
  | ProjectBlind
  | ProjectNotCalm
  | TriggerNothing

showReqFailure :: ReqFailure -> Msg
showReqFailure reqFailure = case reqFailure of
  MoveNothing -> "wasting time on moving into obstacle"
  MeleeSelf -> "trying to melee oneself"
  MeleeDistant -> "trying to melee a distant foe"
  DisplaceDistant -> "trying to switch places with a distant actor"
  DisplaceAccess -> "switching places without access"
  DisplaceProjectiles -> "trying to switch places with multiple projectiles"
  DisplaceDying -> "trying to switch places with a dying foe"
  DisplaceSupported -> "trying to switch places with a supported foe"
  AlterDistant -> "trying to alter a distant tile"
  AlterBlockActor -> "blocked by an actor"
  AlterBlockItem -> "jammed by an item"
  AlterNothing -> "wasting time on altering nothing"
  PickupOverfull -> "cannot carry any more"
  ItemNothing -> "wasting time on void item manipulation"
  ItemNotCalm -> "you are too alarmed to sort through inventory"
  ProjectAimOnself -> "cannot aim at oneself"
  ProjectBlockTerrain -> "aiming obstructed by terrain"
  ProjectBlockActor -> "aiming blocked by an actor"
  ProjectBlind -> "blind actors cannot aim"
  ProjectNotCalm -> "your hands are shaking too much to aim"
  TriggerNothing -> "wasting time on triggering nothing"
