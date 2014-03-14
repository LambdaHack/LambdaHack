{-# LANGUAGE FlexibleContexts, FunctionalDependencies, RankNTypes, TupleSections
             #-}
-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
-- Does not export the @liftIO@ operation nor a few other implementation
-- details.
module Game.LambdaHack.Client.ProtocolClient
  ( -- * The client-server communication monads
    MonadClientReadServer(..), MonadClientWriteServer(..)
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

class MonadClient m => MonadClientReadServer c m | m -> c where
  readServer  :: m c

class MonadClient m => MonadClientWriteServer d m | m -> d where
  writeServer  :: d -> m ()

pongAI :: (MonadClientWriteServer RequestTimed m) => m ()
pongAI = writeServer $ ReqPongHack []

pongUI :: (MonadClientUI m, MonadClientWriteServer Request m) => m ()
pongUI = do
  -- Ping the frontend, too.
  syncFrames
  escPressed <- tryTakeMVarSescMVar
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  let sendPong ats = writeServer $ ReqTimed $ ReqPongHack ats
      hasAiLeader = playerAiLeader $ gplayer fact
  if escPressed && hasAiLeader then do
    -- Ask server to turn off AI for the faction's leader.
    let atomicCmd = UpdAtomic $ UpdAutoFaction side False
    sendPong [atomicCmd]
  else
    -- Respond to the server normally.
    sendPong []
