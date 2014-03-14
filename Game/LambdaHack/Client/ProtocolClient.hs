{-# LANGUAGE FlexibleContexts, FunctionalDependencies, RankNTypes, TupleSections
             #-}
-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
-- Does not export the @liftIO@ operation nor a few other implementation
-- details.
module Game.LambdaHack.Client.ProtocolClient
  ( -- * The client-server communication monads
    MonadClientReadResponse(..), MonadClientWriteRequest(..)
    -- * Protocol
  , pongAI, pongUI
  ) where

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.MonadClientUI
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.ModeKind

class MonadClient m => MonadClientReadResponse resp m | m -> resp where
  receiveResponse  :: m resp

class MonadClient m => MonadClientWriteRequest req m | m -> req where
  sendRequest  :: req -> m ()

pongAI :: (MonadClientWriteRequest RequestTimed m) => m ()
pongAI = sendRequest $ ReqPongHack []

pongUI :: (MonadClientUI m, MonadClientWriteRequest Request m) => m ()
pongUI = do
  -- Ping the frontend, too.
  syncFrames
  escPressed <- tryTakeMVarSescMVar
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  let sendPong ats = sendRequest $ ReqTimed $ ReqPongHack ats
      hasAiLeader = playerAiLeader $ gplayer fact
  if escPressed && hasAiLeader then do
    -- Ask server to turn off AI for the faction's leader.
    let atomicCmd = UpdAtomic $ UpdAutoFaction side False
    sendPong [atomicCmd]
  else
    -- Respond to the server normally.
    sendPong []
