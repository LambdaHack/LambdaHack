-- | Debug output for requests and responseQs.
module Game.LambdaHack.Server.DebugServer
  ( debugResponseAI, debugResponseUI
  , debugRequestTimed, debugRequest
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.Response
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Server.MonadServer

-- We debug these on the server, not on the clients, because we want
-- a single log, knowing the order in which the server received requests
-- and sent responseQs. Clients interleave and block non-deterministically
-- so their logs would be harder to interpret.

debugResponseAI :: MonadServer m => ResponseAI -> m ()
debugResponseAI cmd = case cmd of
  RespUpdAtomicAI cmdA@UpdPerception{} -> debugPlain cmd cmdA
  RespUpdAtomicAI cmdA@UpdResume{} -> debugPlain cmd cmdA
  RespUpdAtomicAI cmdA@UpdSpotTile{} -> debugPlain cmd cmdA
  RespUpdAtomicAI cmdA -> debugPretty cmd cmdA
  RespQueryAI aid -> do
    d <- debugAid aid "RespQueryAI" cmd
    debugPrint d
  RespPingAI -> debugPrint $ tshow cmd

debugResponseUI :: MonadServer m => ResponseUI -> m ()
debugResponseUI cmd = case cmd of
  RespUpdAtomicUI cmdA@UpdPerception{} -> debugPlain cmd cmdA
  RespUpdAtomicUI cmdA@UpdResume{} -> debugPlain cmd cmdA
  RespUpdAtomicUI cmdA@UpdSpotTile{} -> debugPlain cmd cmdA
  RespUpdAtomicUI cmdA -> debugPretty cmd cmdA
  RespSfxAtomicUI sfx -> do
    ps <- posSfxAtomic sfx
    debugPrint $ tshow (cmd, ps)
  RespQueryUI aid -> do
    d <- debugAid aid "RespQueryUI" cmd
    debugPrint d
  RespPingUI -> debugPrint $ tshow cmd

debugPretty :: (MonadServer m, Show a) => a -> UpdAtomic -> m ()
debugPretty cmd cmdA = do
  ps <- posUpdAtomic cmdA
  debugPrint $ tshow (cmd, ps)

debugPlain :: (MonadServer m, Show a) => a -> UpdAtomic -> m ()
debugPlain cmd cmdA = do
  ps <- posUpdAtomic cmdA
  debugPrint $ T.pack $ show (cmd, ps)  -- too large for pretty show

debugRequestTimed :: MonadServer m => RequestTimed -> m ()
debugRequestTimed cmd = do
  let aid = aidOfRequestTimed cmd
  d <- debugAid aid "RequestTimed" cmd
  debugPrint d

debugRequest :: MonadServer m => Request -> m ()
debugRequest cmd = do
  let aid = aidOfRequest cmd
  d <- debugAid aid "Request" cmd
  debugPrint d

data DebugAid a = DebugAid
  { label   :: !Text
  , cmd     :: !a
  , lid     :: !LevelId
  , time    :: !Time
  , aid     :: !ActorId
  , faction :: !FactionId
  }
  deriving Show

debugAid :: (MonadReadState m, Show a) => ActorId -> Text -> a -> m Text
debugAid aid label cmd =
  if aid == toEnum (-1) then
    return $ "Pong:" <+> tshow label <+> tshow cmd
  else do
    b <- getsState $ getActorBody aid
    time <- getsState $ getLocalTime (blid b)
    return $! tshow DebugAid { label
                             , cmd
                             , lid = blid b
                             , time
                             , aid
                             , faction = bfid b }
