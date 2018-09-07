-- | Abstract syntax of requests.
--
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Client.Request
  ( RequestAI, ReqAI(..), RequestUI, ReqUI(..), RequestTimed(..)
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Game.LambdaHack.Common.Ability as Ability
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.Vector
import           Game.LambdaHack.Content.ModeKind

-- | Requests sent by AI clients to the server. If faction leader is to be
-- changed, it's included as the second component.
type RequestAI = (ReqAI, Maybe ActorId)

-- | Possible forms of requests sent by AI clients.
data ReqAI =
    ReqAINop
  | ReqAITimed RequestTimed
  deriving Show

-- | Requests sent by UI clients to the server. If faction leader is to be
-- changed, it's included as the second component.
type RequestUI = (ReqUI, Maybe ActorId)

-- | Possible forms of requests sent by UI clients.
data ReqUI =
    ReqUINop
  | ReqUITimed RequestTimed
  | ReqUIGameRestart (GroupName ModeKind) Challenge
  | ReqUIGameDropAndExit
  | ReqUIGameSaveAndExit
  | ReqUIGameSave
  | ReqUITactic Ability.Tactic
  | ReqUIAutomate
  deriving Show

-- | Requests that take game time.
data RequestTimed =
    ReqMove Vector
  | ReqMelee ActorId ItemId CStore
  | ReqDisplace ActorId
  | ReqAlter Point
  | ReqWait
  | ReqWait10
  | ReqYell
  | ReqMoveItems [(ItemId, Int, CStore, CStore)]
  | ReqProject Point Int ItemId CStore
  | ReqApply ItemId CStore
  deriving Show
