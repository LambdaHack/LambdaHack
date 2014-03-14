-- | Abstract syntax of client commands.
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Common.Response
  ( ResponseAI(..), ResponseUI(..)
  , debugResponseAI, debugResponseUI, debugAid
  , ChanServer(..), ConnServerFaction, ConnServerDict
  ) where

import Control.Concurrent.STM.TQueue
import qualified Data.EnumMap.Strict as EM
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
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Frontend

-- | Abstract syntax of client commands that don't use the UI.
data ResponseAI =
    RespCmdAtomicAI !CmdAtomic
  | RespQueryAI !ActorId
  | RespPingAI
  deriving Show

-- | Abstract syntax of client commands that use the UI.
data ResponseUI =
    RespCmdAtomicUI !CmdAtomic
  | RespSfxAtomicUI !SfxAtomic
  | RespQueryUI !ActorId
  | RespPingUI
  deriving Show

-- These can't use MonadClient and print more information,
-- because they are used by the server, because we want a single log
-- knowing the order server received requests and sent responses
-- and clients interleave and block non-deterministically so their logs
-- would not be so valuable.
debugResponseAI :: MonadReadState m => ResponseAI -> m Text
debugResponseAI cmd = case cmd of
  RespCmdAtomicAI cmdA@PerceptionA{} -> debugPlain cmd cmdA
  RespCmdAtomicAI cmdA@ResumeA{} -> debugPlain cmd cmdA
  RespCmdAtomicAI cmdA@SpotTileA{} -> debugPlain cmd cmdA
  RespCmdAtomicAI cmdA -> debugPretty cmd cmdA
  RespQueryAI aid -> debugAid aid "RespQueryAI" cmd
  RespPingAI -> return $! tshow cmd

debugResponseUI :: MonadReadState m => ResponseUI -> m Text
debugResponseUI cmd = case cmd of
  RespCmdAtomicUI cmdA@PerceptionA{} -> debugPlain cmd cmdA
  RespCmdAtomicUI cmdA@ResumeA{} -> debugPlain cmd cmdA
  RespCmdAtomicUI cmdA@SpotTileA{} -> debugPlain cmd cmdA
  RespCmdAtomicUI cmdA -> debugPretty cmd cmdA
  RespSfxAtomicUI sfx -> do
    ps <- posSfxAtomic sfx
    return $! tshow (cmd, ps)
  RespQueryUI aid -> debugAid aid "RespQueryUI" cmd
  RespPingUI -> return $! tshow cmd

debugPretty :: (MonadReadState m, Show a) => a -> CmdAtomic -> m Text
debugPretty cmd cmdA = do
  ps <- posCmdAtomic cmdA
  return $! tshow (cmd, ps)

debugPlain :: (MonadReadState m, Show a) => a -> CmdAtomic -> m Text
debugPlain cmd cmdA = do
  ps <- posCmdAtomic cmdA
  return $! T.pack $ show (cmd, ps)  -- too large for pretty show

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

-- | Connection channels between the server and a single client.
data ChanServer c d = ChanServer
  { fromServer :: !(TQueue c)
  , toServer   :: !(TQueue d)
  }

-- | Connections to the human-controlled client of a faction and
-- to the AI client for the same faction.
type ConnServerFaction = ( Maybe (ChanFrontend, ChanServer ResponseUI Request)
                         , ChanServer ResponseAI RequestTimed )

-- | Connection information for all factions, indexed by faction identifier.
type ConnServerDict = EM.EnumMap FactionId ConnServerFaction
