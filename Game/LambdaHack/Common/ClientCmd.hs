{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Abstract syntax of client commands.
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Common.ClientCmd
  ( CmdClientAI(..), CmdClientUI(..)
  , debugCmdClientAI, debugCmdClientUI, debugAid
  , Conn(..), ConnFaction, ConnDict
  ) where

import Control.Concurrent.STM.TQueue
import qualified Data.EnumMap.Strict as EM
import Data.Text (Text)

import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.AtomicCmd
import Game.LambdaHack.Common.AtomicPos
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.ServerCmd
import Game.LambdaHack.Common.State

-- | Abstract syntax of client commands that don't use the UI.
data CmdClientAI =
    CmdAtomicAI CmdAtomic
  | CmdQueryAI ActorId
  deriving Show

-- | Abstract syntax of client commands that use the UI.
data CmdClientUI =
    CmdAtomicUI CmdAtomic
  | SfxAtomicUI SfxAtomic
  | CmdQueryUI ActorId
  deriving Show

debugCmdClientAI :: MonadActionRO m => CmdClientAI -> m Text
debugCmdClientAI cmd = case cmd of
  CmdAtomicAI cmdA -> do
    ps <- posCmdAtomic cmdA
    return $ showT (showT cmd, ps)
  CmdQueryAI aid -> debugAid aid $ showT cmd

debugCmdClientUI :: MonadActionRO m => CmdClientUI -> m Text
debugCmdClientUI cmd = case cmd of
  CmdAtomicUI cmdA -> do
    ps <- posCmdAtomic cmdA
    return $ showT (showT cmd, ps)
  SfxAtomicUI sfx -> do
    ps <- posSfxAtomic sfx
    return $ showT (showT cmd, ps)
  CmdQueryUI aid -> debugAid aid $ showT cmd

debugAid :: MonadActionRO m => ActorId -> Text -> m Text
debugAid aid s = do
  b <- getsState $ getActorBody aid
  time <- getsState $ getLocalTime (blid b)
  return $
    showT (s, "lid", blid b, "time", time, "aid", aid, "faction", bfaction b)

-- | Connection channels between the server and a single client.
data Conn c = Conn
  { toClient :: TQueue c
  , toServer :: TQueue CmdSer
  }

instance Show (Conn c) where
  show _ = "client-server connection channels"

-- | Connection to the human-controlled client of a faction and/or
-- to the AI client for the same faction.
type ConnFaction = (Maybe (Conn CmdClientUI), Maybe (Conn CmdClientAI))

-- | Connection information for all factions, indexed by faction identifier.
type ConnDict = EM.EnumMap FactionId ConnFaction
