-- | Abstract syntax of server commands.
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Common.Request
  ( RequestAI(..), RequestUI(..), RequestTimed(..)
  , aidOfRequestAI, aidOfRequestUI
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
  | ReqUIGameSave !ActorId
  | ReqUIAutomate !ActorId
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

-- | The actor that starts performing the request (may be dead, after
-- the command is performed).
aidOfRequestAI :: RequestAI -> ActorId
aidOfRequestAI cmd = case cmd of
  ReqAITimed aid _ -> aid
  ReqAIPong -> toEnum (-1)  -- needed for --sniffIn

-- | The actor that starts performing the request (may be dead, after
-- the command is performed).
aidOfRequestUI :: RequestUI -> ActorId
aidOfRequestUI cmd = case cmd of
  ReqUITimed aid _ -> aid
  ReqUIGameRestart aid _ _ _ -> aid
  ReqUIGameExit aid _ -> aid
  ReqUIGameSave aid -> aid
  ReqUIAutomate aid -> aid
  ReqUIPong _ -> toEnum (-1)  -- needed for --sniffIn

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
