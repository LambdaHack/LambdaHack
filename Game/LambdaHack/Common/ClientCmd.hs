-- | Abstract syntax of client commands.
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Common.ClientCmd
  ( CmdClientAI(..), CmdClientUI(..)
  , debugCmdClientAI, debugCmdClientUI, debugAid
  , ChanServer(..), ConnServerFaction, ConnServerDict
  ) where

import Control.Concurrent.STM.TQueue
import qualified Data.EnumMap.Strict as EM
import Data.Text (Text)
import qualified Data.Text as T

import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.AtomicCmd
import Game.LambdaHack.Common.AtomicPos
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.ServerCmd
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Frontend

-- | Abstract syntax of client commands that don't use the UI.
data CmdClientAI =
    CmdAtomicAI !CmdAtomic
  | CmdQueryAI !ActorId
  | CmdPingAI
  deriving Show

-- | Abstract syntax of client commands that use the UI.
data CmdClientUI =
    CmdAtomicUI !CmdAtomic
  | SfxAtomicUI !SfxAtomic
  | CmdQueryUI !ActorId
  | CmdPingUI
  deriving Show

debugCmdClientAI :: MonadActionRO m => CmdClientAI -> m Text
debugCmdClientAI cmd = case cmd of
  CmdAtomicAI cmdA@PerceptionA{} -> debugPlain cmd cmdA
  CmdAtomicAI cmdA@ResumeA{} -> debugPlain cmd cmdA
  CmdAtomicAI cmdA@SpotTileA{} -> debugPlain cmd cmdA
  CmdAtomicAI cmdA -> debugPretty cmd cmdA
  CmdQueryAI aid -> debugAid aid "CmdQueryAI" cmd
  CmdPingAI -> return $! tshow cmd

debugCmdClientUI :: MonadActionRO m => CmdClientUI -> m Text
debugCmdClientUI cmd = case cmd of
  CmdAtomicUI cmdA@PerceptionA{} -> debugPlain cmd cmdA
  CmdAtomicUI cmdA@ResumeA{} -> debugPlain cmd cmdA
  CmdAtomicUI cmdA@SpotTileA{} -> debugPlain cmd cmdA
  CmdAtomicUI cmdA -> debugPretty cmd cmdA
  SfxAtomicUI sfx -> do
    ps <- posSfxAtomic sfx
    return $! tshow (cmd, ps)
  CmdQueryUI aid -> debugAid aid "CmdQueryUI" cmd
  CmdPingUI -> return $! tshow cmd

debugPretty :: (MonadActionRO m, Show a) => a -> CmdAtomic -> m Text
debugPretty cmd cmdA = do
  ps <- posCmdAtomic cmdA
  return $! tshow (cmd, ps)

debugPlain :: (MonadActionRO m, Show a) => a -> CmdAtomic -> m Text
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

debugAid :: (MonadActionRO m, Show a) => ActorId -> Text -> a -> m Text
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
type ConnServerFaction = ( Maybe (ChanFrontend, ChanServer CmdClientUI CmdSer)
                         , ChanServer CmdClientAI CmdTakeTimeSer )

-- | Connection information for all factions, indexed by faction identifier.
type ConnServerDict = EM.EnumMap FactionId ConnServerFaction
