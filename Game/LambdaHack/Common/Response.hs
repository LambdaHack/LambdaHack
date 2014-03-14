-- | Abstract syntax of client commands.
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Common.Response
  ( ResponseAI(..), ResponseUI(..)
  ) where

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor

-- | Abstract syntax of client commands that don't use the UI.
data ResponseAI =
    RespUpdAtomicAI !UpdAtomic
  | RespQueryAI !ActorId
  | RespPingAI
  deriving Show

-- | Abstract syntax of client commands that use the UI.
data ResponseUI =
    RespUpdAtomicUI !UpdAtomic
  | RespSfxAtomicUI !SfxAtomic
  | RespQueryUI !ActorId
  | RespPingUI
  deriving Show
