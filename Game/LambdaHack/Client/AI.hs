{-# LANGUAGE CPP #-}
-- | Ways for the client to use AI to produce server requests, based on
-- the client's view of the game state.
module Game.LambdaHack.Client.AI
  ( queryAI, nonLeaderQueryAI
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , refreshTarget, pickAction
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.Text as T

import Game.LambdaHack.Client.AI.HandleAbilityM
import Game.LambdaHack.Client.AI.PickActorM
import Game.LambdaHack.Client.AI.PickTargetM
import Game.LambdaHack.Client.AI.Strategy
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State

-- | Handle the move of an AI player.
queryAI :: MonadClient m => m RequestAI
queryAI = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  let mleader = gleader fact
      !_A = assert (isJust mleader) ()
  (aidToMove, bToMove) <- pickActorToMove refreshTarget
  req <- ReqAITimed <$> pickAction (aidToMove, bToMove)
  mtgt2 <- getsClient $ fmap fst . EM.lookup aidToMove . stargetD
  if mleader /= Just (aidToMove, mtgt2)
    then return (req, Just (aidToMove, mtgt2))
    else return (req, Nothing)

nonLeaderQueryAI :: MonadClient m => ActorId -> m RequestAI
nonLeaderQueryAI oldAid = do
  mleader <- getsClient _sleader
  let !_A = assert (mleader /= Just oldAid) ()
      aidToMove = oldAid
  useTactics refreshTarget oldAid
  bToMove <- getsState $ getActorBody aidToMove
  req <- ReqAITimed <$> pickAction (aidToMove, bToMove)
  return (req, Nothing)

-- | Verify and possibly change the target of an actor. This function both
-- updates the target in the client state and returns the new target explicitly.
refreshTarget :: MonadClient m
              => (ActorId, Actor)  -- ^ the actor to refresh
              -> m (Maybe (Target, PathEtc))
refreshTarget (aid, body) = do
  side <- getsClient sside
  let !_A = assert (bfid body == side
                    `blame` "AI tries to move an enemy actor"
                    `twith` (aid, body, side)) ()
  let !_A = assert (not (bproj body)
                    `blame` "AI gets to manually move its projectiles"
                    `twith` (aid, body, side)) ()
  stratTarget <- targetStrategy aid
  tgtMPath <-
    if nullStrategy stratTarget then  -- equiv to nullFreq
      -- No sensible target; wipe out the old one.
      return Nothing
    else do
      -- Choose a target from those proposed by AI for the actor.
      tmp <- rndToAction $ frequency $ bestVariant stratTarget
      return $ Just tmp
  oldTgt <- getsClient $ EM.lookup aid . stargetD
  let _debug = T.unpack
          $ "\nHandleAI symbol:"    <+> tshow (bsymbol body)
          <> ", aid:"               <+> tshow aid
          <> ", pos:"               <+> tshow (bpos body)
          <> "\nHandleAI oldTgt:"   <+> tshow oldTgt
          <> "\nHandleAI strTgt:"   <+> tshow stratTarget
          <> "\nHandleAI target:"   <+> tshow tgtMPath
--  trace _debug skip
  modifyClient $ \cli ->
    cli {stargetD = EM.alter (const tgtMPath) aid (stargetD cli)}
  return $! case tgtMPath of
    Just (tgt, Just pathEtc) -> Just (tgt, pathEtc)
    _ -> Nothing

-- | Pick an action the actor will perfrom this turn.
pickAction :: MonadClient m => (ActorId, Actor) -> m RequestAnyAbility
pickAction (aid, body) = do
  -- Pathing finished, randomize paths for next moves.
  modifyClient $ \cli -> cli {seps = seps cli + 773}
  side <- getsClient sside
  let !_A = assert (bfid body == side
                    `blame` "AI tries to move enemy actor"
                    `twith` (aid, bfid body, side)) ()
  let !_A = assert (not (bproj body)
                    `blame` "AI gets to manually move its projectiles"
                    `twith` (aid, bfid body, side)) ()
  stratAction <- actionStrategy aid
  let bestAction = bestVariant stratAction
      !_A = assert (not (nullFreq bestAction)  -- equiv to nullStrategy
                    `blame` "no AI action for actor"
                    `twith` (stratAction, aid, body)) ()
  -- Run the AI: chose an action from those given by the AI strategy.
  rndToAction $ frequency bestAction
