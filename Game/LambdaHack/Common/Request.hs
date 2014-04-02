-- | Abstract syntax of server commands.
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Common.Request
  ( RequestAI(..), RequestUI(..), RequestTimed(..)
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

-- | Cclient-server requests sent by AI clients.
data RequestAI =
    ReqAITimed !ActorId !RequestTimed
  | ReqAIPong
  deriving (Show, Eq)

-- | Client-server requests sent by UI clients.
data RequestUI =
    ReqUITimed !ActorId !RequestTimed
  | ReqUIGameRestart !ActorId !Text !Int ![(Int, Text)]
  | ReqUIGameExit !ActorId !Int
  | ReqUIGameSave
  | ReqUIAutomate !FactionId
  | ReqUIPong [CmdAtomic]
  deriving (Show, Eq)

-- | Client-server requests that take game time. Sent by both AI and UI clients.
data RequestTimed =
    ReqMove !Vector
  | ReqMelee !ActorId
  | ReqDisplace !ActorId
  | ReqAlter !Point !(Maybe F.Feature)
  | ReqWait
  | ReqMoveItem !ItemId !Int !CStore !CStore
  | ReqProject !Point !Int !ItemId !CStore
  | ReqApply !ItemId !CStore
  | ReqTrigger !(Maybe F.Feature)
  deriving (Show, Eq)

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
