{-# LANGUAGE GADTs #-}
-- | Ways for the client to use AI to produce server requests, based on
-- the client's view of the game state.
module Game.LambdaHack.Client.AI
  ( queryAI
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , refreshTargetS, pickAction
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Client.AI.ConditionM
import Game.LambdaHack.Client.AI.HandleAbilityM
import Game.LambdaHack.Client.AI.PickActorM
import Game.LambdaHack.Client.AI.PickTargetM
import Game.LambdaHack.Client.AI.Strategy
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Request

-- | Handle the move of an AI player.
queryAI :: forall m. MonadClient m => ActorId -> m RequestAI
queryAI aid = do
  let refreshTarget :: (ActorId, Actor) -> m (Maybe TgtAndPath)
      {-# NOINLINE refreshTarget #-}
      refreshTarget (aid2, b2) = refreshTargetS aid2 b2
  udpdateCondInMelee aid
  mleader <- getsClient _sleader
  (aidToMove, bToMove) <-
    if mleader == Just aid
    then pickActorToMove Nothing refreshTarget
    else do
      useTactics refreshTarget aid
      b <- getsState $ getActorBody aid
      return (aid, b)
  treq <- pickAction (aidToMove, bToMove)
  (aidToMove2, treq2) <-
    case treq of
      RequestAnyAbility ReqWait | mleader == Just aid -> do
        -- leader waits; a waste; try once to change leader
        (aidToMove2, bToMove2) <- pickActorToMove (Just aidToMove) refreshTarget
        if aidToMove2 /= aidToMove
        then do
          treq2 <- pickAction (aidToMove2, bToMove2)
          return (aidToMove2, treq2)
        else return (aidToMove, treq)  -- no luck; wait anyway
      _ -> return (aidToMove, treq)
  if aidToMove2 /= aid
  then return (ReqAITimed treq2, Just aidToMove2)
  else return (ReqAITimed treq2, Nothing)

udpdateCondInMelee :: MonadClient m => ActorId -> m ()
{-# INLINE udpdateCondInMelee #-}
udpdateCondInMelee aid = do
  b <- getsState $ getActorBody aid
  condInMelee <- getsClient $ (EM.! blid b) . scondInMelee
  case condInMelee of
    Right{} -> return ()  -- still up to date
    Left oldCond -> do
      -- Lag the chages, to avoid frequent invalidation of targets, etc.
      let dist = if oldCond then 3 else 1
      newCond <- condInMeleeM dist b
      modifyClient $ \cli ->
        cli {scondInMelee =
               EM.adjust (const $ Right (newCond, oldCond)) (blid b)
                         (scondInMelee cli)}

-- | Verify and possibly change the target of an actor. This function both
-- updates the target in the client state and returns the new target explicitly.
refreshTargetS :: MonadClient m => ActorId -> Actor -> m (Maybe TgtAndPath)
{-# INLINE refreshTargetS #-}
refreshTargetS aid body = do
  side <- getsClient sside
  let !_A = assert (bfid body == side
                    `blame` "AI tries to move an enemy actor"
                    `twith` (aid, body, side)) ()
  let !_A = assert (isNothing (btrajectory body)
                    `blame` "AI gets to manually move its projectiles"
                    `twith` (aid, body, side)) ()
  stratTarget <- targetStrategy aid
  if nullStrategy stratTarget then do
    -- Melee in progress and the actor can't contribute
    -- and would slow down others if he acted.
    modifyClient $ \cli -> cli {stargetD = EM.delete aid (stargetD cli)}
    return Nothing
  else do
    -- _debugoldTgt <- getsClient $ EM.lookup aid . stargetD
    -- Choose a target from those proposed by AI for the actor.
    tgtMPath <- rndToAction $ frequency $ bestVariant stratTarget
    modifyClient $ \cli ->
      cli {stargetD = EM.insert aid tgtMPath (stargetD cli)}
    return $ Just tgtMPath
    -- let _debug = T.unpack
    --       $ "\nHandleAI symbol:"    <+> tshow (bsymbol body)
    --       <> ", aid:"               <+> tshow aid
    --       <> ", pos:"               <+> tshow (bpos body)
    --       <> "\nHandleAI oldTgt:"   <+> tshow _debugoldTgt
    --       <> "\nHandleAI strTgt:"   <+> tshow stratTarget
    --       <> "\nHandleAI target:"   <+> tshow tgtMPath
    -- trace _debug $ return $ Just tgtMPath

-- | Pick an action the actor will perform this turn.
pickAction :: MonadClient m => (ActorId, Actor) -> m RequestAnyAbility
{-# INLINE pickAction #-}
pickAction (aid, body) = do
  side <- getsClient sside
  let !_A = assert (bfid body == side
                    `blame` "AI tries to move enemy actor"
                    `twith` (aid, bfid body, side)) ()
  let !_A = assert (isNothing (btrajectory body)
                    `blame` "AI gets to manually move its projectiles"
                    `twith` (aid, bfid body, side)) ()
  stratAction <- actionStrategy aid
  let bestAction = bestVariant stratAction
      !_A = assert (not (nullFreq bestAction)  -- equiv to nullStrategy
                    `blame` "no AI action for actor"
                    `twith` (stratAction, aid, body)) ()
  -- Run the AI: chose an action from those given by the AI strategy.
  rndToAction $ frequency bestAction
