-- | Abstract syntax of responses.
--
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Client.Response
  ( Response(..)
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Types

-- | Abstract syntax of responses sent by server to an AI or UI client
-- (or a universal client that can handle both roles, which is why
-- this type is not separated into distinct AI and UI types).
-- A response tells a client how to update game state or what information
-- to send to the server.
data Response =
    RespUpdAtomicNoState UpdAtomic
    -- ^ change @State@ by performing this atomic update
  | RespUpdAtomic State UpdAtomic
    -- ^ put the given @State@, which results from performing the atomic update
  | RespQueryAI ActorId
    -- ^ compute an AI move for the actor and send (the semantics of) it
  | RespSfxAtomic SfxAtomic
    -- ^ perform special effects (animations, messages, etc.)
  | RespQueryUI
    -- ^ prompt the human player for a command and send (the semantics of) it
  deriving Show
