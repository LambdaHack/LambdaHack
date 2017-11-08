{-# LANGUAGE DataKinds, GADTs, KindSignatures, StandaloneDeriving #-}
-- | Abstract syntax of server commands.
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Client.Request
  ( RequestAI, ReqAI(..), RequestUI, ReqUI(..)
  , RequestTimed(..), RequestAnyAbility(..)
  , timedToUI
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

-- | Client-server requests sent by AI clients.
data ReqAI =
    ReqAITimed RequestAnyAbility
  | ReqAINop
  deriving Show

type RequestAI = (ReqAI, Maybe ActorId)

-- | Client-server requests sent by UI clients.
data ReqUI =
    ReqUINop
  | ReqUITimed RequestAnyAbility
  | ReqUIGameRestart (GroupName ModeKind) Challenge
  | ReqUIGameExit
  | ReqUIGameSave
  | ReqUITactic Tactic
  | ReqUIAutomate
  deriving Show

type RequestUI = (ReqUI, Maybe ActorId)

data RequestAnyAbility = forall a. RequestAnyAbility (RequestTimed a)

deriving instance Show RequestAnyAbility

timedToUI :: RequestTimed a -> ReqUI
timedToUI = ReqUITimed . RequestAnyAbility

-- | Client-server requests that take game time. Sent by both AI and UI clients.
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
