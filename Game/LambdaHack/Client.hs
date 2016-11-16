-- | Semantics of responses that are sent to clients.
--
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Client
  ( -- * Re-exported from "Game.LambdaHack.Client.LoopClient"
#ifdef CLIENTS_AS_THREADS
    loopAI, loopUI
#endif
  ) where

import Prelude ()

#ifdef CLIENTS_AS_THREADS
import Game.LambdaHack.Client.LoopM
#endif
