-- | Debug output for requests and responses.
module Game.LambdaHack.Server.DebugM
  ( debugResponse
  , debugRequestAI, debugRequestUI
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , debugShow, debugPretty, debugPlain, DebugAid(..), debugAid
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Int (Int64)
import qualified Data.Text as T
import qualified Text.Show.Pretty as Show.Pretty

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client (Response (..))
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Types
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.State

-- We debug these on the server, not on the clients, because we want
-- a single log, knowing the order in which the server received requests
-- and sent responseQs. Clients interleave and block non-deterministically
-- so their logs would be harder to interpret.

debugShow :: Show a => a -> Text
debugShow = T.pack . Show.Pretty.ppShow

debugResponse :: MonadServer m => FactionId -> Response -> m ()
debugResponse fid resp = case resp of
  RespUpdAtomic _ cmd@UpdPerception{} -> debugPlain fid "RespUpdAtomic" cmd
  RespUpdAtomic _ cmd@UpdResume{} -> debugPlain fid "RespUpdAtomic" cmd
  RespUpdAtomic _ cmd@UpdRestart{} -> debugPlain fid "RespUpdAtomic" cmd
  RespUpdAtomic _ cmd@UpdSpotTile{} -> debugPlain fid "RespUpdAtomic" cmd
  RespUpdAtomic _ cmd@(UpdCreateActor aid _ _) -> do
    d <- debugAid aid "UpdCreateActor"
    serverPrint d
    debugPretty fid "RespUpdAtomic" cmd
  RespUpdAtomic _ cmd@(UpdSpotActor aid _) -> do
    d <- debugAid aid "UpdSpotActor"
    serverPrint d
    debugPretty fid "RespUpdAtomic" cmd
  RespUpdAtomic _ cmd -> debugPretty fid "RespUpdAtomic" cmd
  RespUpdAtomicNoState cmd@UpdPerception{} ->
    debugPlain fid "RespUpdAtomicNoState" cmd
  RespUpdAtomicNoState cmd@UpdResume{} ->
    debugPlain fid "RespUpdAtomicNoState" cmd
  RespUpdAtomicNoState cmd@UpdSpotTile{} ->
    debugPlain fid "RespUpdAtomicNoState" cmd
  RespUpdAtomicNoState cmd ->
    debugPretty fid "RespUpdAtomicNoState" cmd
  RespQueryAI aid -> do
    d <- debugAid aid "RespQueryAI"
    serverPrint d
  RespSfxAtomic sfx -> do  -- not so crucial so no details
    ps <- posSfxAtomic sfx
    serverPrint $ debugShow (fid, "RespSfxAtomic" :: Text, ps)
  RespQueryUIunderAI -> serverPrint "RespQueryUIunderAI"
  RespQueryUI -> serverPrint "RespQueryUI"

debugPretty :: MonadServer m => FactionId -> Text -> UpdAtomic -> m ()
debugPretty fid t cmd = do
  ps <- posUpdAtomic cmd
  serverPrint $ debugShow (fid, t, ps, cmd)

debugPlain :: MonadServer m => FactionId -> Text -> UpdAtomic -> m ()
debugPlain fid t cmd = do
  ps <- posUpdAtomic cmd
  serverPrint $ T.pack $ show (fid, t, ps, cmd)
    -- too large for pretty printing

debugRequestAI :: MonadServer m => ActorId -> m ()
debugRequestAI aid = do
  d <- debugAid aid "AI request"
  serverPrint d

debugRequestUI :: MonadServer m => ActorId -> m ()
debugRequestUI aid = do
  d <- debugAid aid "UI request"
  serverPrint d

data DebugAid = DebugAid
  { label   :: Text
  , aid     :: ActorId
  , faction :: FactionId
  , lid     :: LevelId
  , bHP     :: Int64
  , btime   :: Maybe Time
  , btrTime :: Maybe Time
  , time    :: Time
  }
  deriving Show

debugAid :: MonadServer m => ActorId -> Text -> m Text
debugAid aid label = do
  b <- getsState $ getActorBody aid
  time <- getsState $ getLocalTime (blid b)
  btime <- getsServer $ lookupActorTime (bfid b) (blid b) aid . sactorTime
  btrTime <- getsServer $ lookupActorTime (bfid b) (blid b) aid . strajTime
  return $! debugShow DebugAid { label
                               , aid
                               , faction = bfid b
                               , lid = blid b
                               , bHP = bhp b
                               , btime
                               , btrTime
                               , time }
