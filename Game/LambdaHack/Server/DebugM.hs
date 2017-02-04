-- | Debug output for requests and responseQs.
module Game.LambdaHack.Server.DebugM
  ( debugResponse
  , debugRequestAI, debugRequestUI
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Text.Show.Pretty as Show.Pretty

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.Response
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.State

-- We debug these on the server, not on the clients, because we want
-- a single log, knowing the order in which the server received requests
-- and sent responseQs. Clients interleave and block non-deterministically
-- so their logs would be harder to interpret.

debugShow :: Show a => a -> Text
debugShow = T.pack . Show.Pretty.ppShow

debugResponse :: MonadServer m => FactionId -> Response -> m ()
debugResponse fid cmd = case cmd of
  RespUpdAtomic cmdA@UpdPerception{} -> debugPlain fid cmd cmdA
  RespUpdAtomic cmdA@UpdResume{} -> debugPlain fid cmd cmdA
  RespUpdAtomic cmdA@UpdSpotTile{} -> debugPlain fid cmd cmdA
  RespUpdAtomic cmdA -> debugPretty fid cmd cmdA
  RespQueryAI aid -> do
    d <- debugAid aid "RespQueryAI" cmd
    serverPrint d
  RespSfxAtomic sfx -> do
    ps <- posSfxAtomic sfx
    serverPrint $ debugShow (fid, cmd, ps)
  RespQueryUI -> serverPrint "RespQueryUI"

debugPretty :: (MonadServer m, Show a) => FactionId -> a -> UpdAtomic -> m ()
debugPretty fid cmd cmdA = do
  ps <- posUpdAtomic cmdA
  serverPrint $ debugShow (fid, cmd, ps)

debugPlain :: (MonadServer m, Show a) => FactionId -> a -> UpdAtomic -> m ()
debugPlain fid cmd cmdA = do
  ps <- posUpdAtomic cmdA
  serverPrint $ T.pack $ show (fid, cmd, ps)  -- too large for pretty printing

debugRequestAI :: MonadServer m => ActorId -> RequestAI -> m ()
debugRequestAI aid cmd = do
  d <- debugAid aid "AI request" cmd
  serverPrint d

debugRequestUI :: MonadServer m => ActorId -> RequestUI -> m ()
debugRequestUI aid cmd = do
  d <- debugAid aid "UI request" cmd
  serverPrint d

data DebugAid a = DebugAid
  { label   :: !Text
  , aid     :: !ActorId
  , cmd     :: !a
  , faction :: !FactionId
  , lid     :: !LevelId
  , bHP     :: !Int64
  , btime   :: !Time
  , time    :: !Time
  }
  deriving Show

debugAid :: (MonadServer m, Show a) => ActorId -> Text -> a -> m Text
debugAid aid label cmd = do
  b <- getsState $ getActorBody aid
  time <- getsState $ getLocalTime (blid b)
  btime <- getsServer $ (EM.! aid) . (EM.! blid b) . (EM.! bfid b) . sactorTime
  return $! debugShow DebugAid { label
                               , aid
                               , cmd
                               , faction = bfid b
                               , lid = blid b
                               , bHP = bhp b
                               , btime
                               , time }
