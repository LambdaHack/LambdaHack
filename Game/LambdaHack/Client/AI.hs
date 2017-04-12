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
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State

-- | Handle the move of an AI player.
queryAI :: MonadClient m => ActorId -> m RequestAI
queryAI aid = do
  udpdateCondInMelee aid
  mleader <- getsClient _sleader
  (aidToMove, treq) <- pickAI Nothing aid
  (aidToMove2, treq2) <-
    case treq of
      RequestAnyAbility ReqWait | mleader == Just aid -> do
        -- leader waits; a waste; try once to pick a yet different leader
        modifyClient $ \cli -> cli {_sleader = mleader}  -- undo previous choice
        pickAI (Just (aidToMove, treq)) aid
      _ -> return (aidToMove, treq)
  if aidToMove2 /= aid
  then return (ReqAITimed treq2, Just aidToMove2)
  else return (ReqAITimed treq2, Nothing)

pickAI :: forall m. MonadClient m
       => Maybe (ActorId, RequestAnyAbility) -> ActorId
       -> m (ActorId, RequestAnyAbility)
-- This inline speeds up execution by 10%, despite bloating executable:
{-# INLINE pickAI #-}
pickAI maid aid = do
  let refreshTarget :: (ActorId, Actor) -> m (Maybe TgtAndPath)
      {-# NOINLINE refreshTarget #-}
      refreshTarget (aid2, b2) = refreshTargetS aid2 b2
  mleader <- getsClient _sleader
  aidToMove <-
    if mleader == Just aid
    then pickActorToMove (fst <$> maid) refreshTarget
    else do
      useTactics refreshTarget aid
      return aid
  treq <- case maid of
    Just (aidOld, treqOld) | aidToMove == aidOld ->
      return treqOld  -- no better leader found
    _ -> pickAction aidToMove
  return (aidToMove, treq)

udpdateCondInMelee :: MonadClient m => ActorId -> m ()
udpdateCondInMelee aid = do
  b <- getsState $ getActorBody aid
  condInMelee <- getsClient $ (EM.! blid b) . scondInMelee
  case condInMelee of
    Just{} -> return ()  -- still up to date
    Nothing -> do
      newCond <- condInMeleeM b
      modifyClient $ \cli ->
        cli {scondInMelee =
               EM.adjust (const $ Just newCond) (blid b) (scondInMelee cli)}

-- | Check if any non-dying foe (projectile or not) is adjacent
-- to any of our normal actors (whether they can melee or just need to flee,
-- in which case alert is needed so that they are not slowed down by others).
condInMeleeM :: MonadClient m => Actor -> m Bool
condInMeleeM bodyOur = do
  fact <- getsState $ (EM.! bfid bodyOur) . sfactionD
  let f !b = blid b == blid bodyOur && isAtWar fact (bfid b) && bhp b > 0
  -- We assume foes are less numerous, because usually they are heroes,
  -- and so we compute them once and use many times.
  allFoes <- getsState $ filter f . EM.elems . sactorD
  getsState $ any (\body ->
    bfid bodyOur == bfid body
    && blid bodyOur == blid body
    && not (bproj body)
    && bhp body > 0
    && any (\b -> adjacent (bpos b) (bpos body)) allFoes) . sactorD

-- | Verify and possibly change the target of an actor. This function both
-- updates the target in the client state and returns the new target explicitly.
refreshTargetS :: MonadClient m => ActorId -> Actor -> m (Maybe TgtAndPath)
-- This inline is surprisingly beneficial:
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
pickAction :: MonadClient m => ActorId -> m RequestAnyAbility
{-# INLINE pickAction #-}
pickAction aid = do
  side <- getsClient sside
  body <- getsState $ getActorBody aid
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
