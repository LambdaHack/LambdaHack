-- | Abstract syntax of client commands.
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Common.Response
  ( Response(..), CliSerQueue, ChanServer(..)
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Concurrent

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State

-- | Abstract syntax of client commands for both AI and UI clients.
data Response =
    RespUpdAtomic State UpdAtomic
  | RespQueryAI ActorId
  | RespSfxAtomic SfxAtomic
  | RespQueryUI
  deriving Show

type CliSerQueue = MVar

-- | Connection channel between the server and a single client.
data ChanServer = ChanServer
  { responseS  :: CliSerQueue Response
  , requestAIS :: CliSerQueue RequestAI
  , requestUIS :: Maybe (CliSerQueue RequestUI)
  }
