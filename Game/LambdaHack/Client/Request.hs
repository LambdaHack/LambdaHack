{-# LANGUAGE DataKinds, GADTs, KindSignatures, StandaloneDeriving #-}
-- | Abstract syntax of requests.
--
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Client.Request
  ( RequestAI, ReqAI(..), RequestUI, ReqUI(..)
  , RequestAnyAbility(..), RequestTimed(..)
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ModeKind

-- | Requests sent by AI clients to the server. If faction leader is to be
-- changed, it's included as the second component.
type RequestAI = (ReqAI, Maybe ActorId)

-- | Possible forms of requests sent by AI clients.
data ReqAI =
    ReqAINop
  | ReqAITimed RequestAnyAbility
  deriving Show

-- | Requests sent by UI clients to the server. If faction leader is to be
-- changed, it's included as the second component.
type RequestUI = (ReqUI, Maybe ActorId)

-- | Possible forms of requests sent by UI clients.
data ReqUI =
    ReqUINop
  | ReqUITimed RequestAnyAbility
  | ReqUIGameRestart (GroupName ModeKind) Challenge
  | ReqUIGameExit
  | ReqUIGameSave
  | ReqUITactic Tactic
  | ReqUIAutomate
  deriving Show

-- | Basic form of requests, sent by both AI and UI clients to the server.
data RequestAnyAbility = forall a. RequestAnyAbility (RequestTimed a)

deriving instance Show RequestAnyAbility

-- | Requests that take game time, indexed by actor ability
-- that is needed for performing the corresponding actions.
data RequestTimed :: Ability -> * where
  ReqMove :: Vector -> RequestTimed 'AbMove
  ReqMelee :: ActorId -> ItemId -> CStore -> RequestTimed 'AbMelee
  ReqDisplace :: ActorId -> RequestTimed 'AbDisplace
  ReqAlter :: Point -> RequestTimed 'AbAlter
  ReqWait :: RequestTimed 'AbWait
  ReqWait10 :: RequestTimed 'AbWait
  ReqMoveItems :: [(ItemId, Int, CStore, CStore)] -> RequestTimed 'AbMoveItem
  ReqProject :: Point -> Int -> ItemId -> CStore -> RequestTimed 'AbProject
  ReqApply :: ItemId -> CStore -> RequestTimed 'AbApply

deriving instance Show (RequestTimed a)
