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
{-# INLINABLE debugShow #-}
debugShow = T.pack . Show.Pretty.ppShow

debugResponse :: MonadServer m => Response -> m ()
{-# INLINABLE debugResponse #-}
debugResponse cmd = case cmd of
  RespUpdAtomic _ cmdA@UpdPerception{} -> debugPlain cmd cmdA
  RespUpdAtomic _ cmdA@UpdResume{} -> debugPlain cmd cmdA
  RespUpdAtomic _ cmdA@UpdSpotTile{} -> debugPlain cmd cmdA
  RespUpdAtomic _ cmdA -> debugPretty cmd cmdA
  RespQueryAI aid -> do
    d <- debugAid aid "RespQueryAI" cmd
    serverPrint d
  RespSfxAtomic sfx -> do
    ps <- posSfxAtomic sfx
    serverPrint $ debugShow (cmd, ps)
  RespQueryUI -> serverPrint "RespQueryUI"

debugPretty :: (MonadServer m, Show a) => a -> UpdAtomic -> m ()
{-# INLINABLE debugPretty #-}
debugPretty cmd cmdA = do
  ps <- posUpdAtomic cmdA
  serverPrint $ debugShow (cmd, ps)

debugPlain :: (MonadServer m, Show a) => a -> UpdAtomic -> m ()
{-# INLINABLE debugPlain #-}
debugPlain cmd cmdA = do
  ps <- posUpdAtomic cmdA
  serverPrint $ T.pack $ show (cmd, ps)  -- too large for pretty printing

debugRequestAI :: MonadServer m => ActorId -> RequestAI -> m ()
{-# INLINABLE debugRequestAI #-}
debugRequestAI aid cmd = do
  d <- debugAid aid "AI request" cmd
  serverPrint d

debugRequestUI :: MonadServer m => ActorId -> RequestUI -> m ()
{-# INLINABLE debugRequestUI #-}
debugRequestUI aid cmd = do
  d <- debugAid aid "UI request" cmd
  serverPrint d

data DebugAid a = DebugAid
  { label   :: !Text
  , cmd     :: !a
  , lid     :: !LevelId
  , bHP     :: !Int64
  , btime   :: !Time
  , time    :: !Time
  , aid     :: !ActorId
  , faction :: !FactionId
  }
  deriving Show

debugAid :: (MonadServer m, Show a) => ActorId -> Text -> a -> m Text
{-# INLINABLE debugAid #-}
debugAid aid label cmd = do
  b <- getsState $ getActorBody aid
  time <- getsState $ getLocalTime (blid b)
  btime <- getsServer $ (EM.! aid) . (EM.! blid b) . (EM.! bfid b) . sactorTime
  return $! debugShow DebugAid { label
                               , cmd
                               , lid = blid b
                               , bHP = bhp b
                               , btime
                               , time
                               , aid
                               , faction = bfid b }
