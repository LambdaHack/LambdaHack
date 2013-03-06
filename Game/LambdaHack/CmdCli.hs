{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Abstract syntax of client commands.
module Game.LambdaHack.CmdCli
  ( CmdCli(..), CmdUI(..)
  , ConnCli(..), ConnFaction, ConnDict
  , debugCli, debugUI, debugAid
  ) where

import Control.Concurrent.STM.TQueue
import qualified Data.EnumMap.Strict as EM
import Data.Text (Text)

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.CmdAtomic
import Game.LambdaHack.CmdAtomicSem
import Game.LambdaHack.CmdSer
import Game.LambdaHack.Faction
import Game.LambdaHack.Msg
import Game.LambdaHack.State

-- | Abstract syntax of client commands that don't use the UI.
data CmdCli =
    CmdAtomicCli CmdAtomic
  | CmdQueryAICli ActorId
  deriving Show

-- | Abstract syntax of client commands that use the UI.
data CmdUI =
    CmdAtomicUI CmdAtomic
  | DescAtomicUI DescAtomic
  | CmdQueryHumanUI ActorId
  deriving Show

-- | Connection channels between server and a single client.
data ConnCli c = ConnCli
  { toClient :: TQueue c
  , toServer :: TQueue CmdSer
  }

instance Show (ConnCli c) where
  show _ = "client channels"

-- | Human-controlled client, UI of the client, AI of the client.
type ConnFaction = (Maybe (ConnCli CmdUI), Maybe (ConnCli CmdCli))

-- | Connection information for each client and an optional AI client
-- for the same faction, indexed by faction identifier.
type ConnDict = EM.EnumMap FactionId ConnFaction

debugCli :: MonadActionRO m => CmdCli -> m Text
debugCli cmd = case cmd of
  CmdAtomicCli cmdA -> do
    ps <- posCmdAtomic cmdA
    return $ showT (ps, "CmdAtomicCli", cmdA)
  CmdQueryAICli aid ->
    debugAid aid "CmdHandleAICli"

debugUI :: MonadActionRO m => CmdUI -> m Text
debugUI cmd = case cmd of
  CmdAtomicUI cmdA -> do
    ps <- posCmdAtomic cmdA
    return $ showT (ps, "CmdAtomicUI", cmdA)
  DescAtomicUI desc -> do
    ps <- posDescAtomic desc
    return $ showT (ps, "DescAtomicUI", desc)
  CmdQueryHumanUI aid ->
    debugAid aid "CmdHandleHumanUI"

debugAid :: MonadActionRO m => ActorId -> Text -> m Text
debugAid aid s = do
  b <- getsState $ getActorBody aid
  time <- getsState $ getTime (blid b)
  return $ showT ( "lid", blid b, "time", time, "aid", aid
                               , "faction", bfaction b, s)
