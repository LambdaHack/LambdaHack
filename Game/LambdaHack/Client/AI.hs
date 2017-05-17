{-# LANGUAGE GADTs #-}
-- | Ways for the client to use AI to produce server requests, based on
-- the client's view of the game state.
module Game.LambdaHack.Client.AI
  ( queryAI
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , pickAI, pickAction, udpdateCondInMelee, condInMeleeM
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Client.AI.HandleAbilityM
import Game.LambdaHack.Client.AI.PickActorM
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

-- | Handle the move of an actor under AI control (of UI or AI player).
queryAI :: MonadClient m => ActorId -> m RequestAI
queryAI aid = do
  -- @_sleader@ may be different from @_gleader@ due to @stopPlayBack@,
  -- but only leaders may change faction leader, so we fix that:
  side <- getsClient sside
  mleader <- getsState $ _gleader . (EM.! side) . sfactionD
  mleaderCli <- getsClient _sleader
  unless (Just aid == mleader || mleader == mleaderCli) $
    -- @aid@ is not the leader, so he can't change leader
    modifyClient $ \cli -> cli {_sleader = mleader}
  -- @condInMelee@ will most proably be needed in this functions,
  -- but even if not, it's OK, it's not forced, because wrapped in @Maybe@:
  udpdateCondInMelee aid
  (aidToMove, treq) <- pickAI Nothing aid
  (aidToMove2, treq2) <-
    case treq of
      RequestAnyAbility ReqWait | mleader == Just aid -> do
        -- leader waits; a waste; try once to pick a yet different leader
        modifyClient $ \cli -> cli {_sleader = mleader}  -- undo previous choice
        pickAI (Just (aidToMove, treq)) aid
      _ -> return (aidToMove, treq)
  return ( ReqAITimed treq2
         , if aidToMove2 /= aid then Just aidToMove2 else Nothing )

pickAI :: MonadClient m
       => Maybe (ActorId, RequestAnyAbility) -> ActorId
       -> m (ActorId, RequestAnyAbility)
-- This inline speeds up execution by 10% and increases allocation by 15%,
-- despite probably bloating executable:
{-# INLINE pickAI #-}
pickAI maid aid = do
  mleader <- getsClient _sleader
  aidToMove <-
    if mleader == Just aid
    then pickActorToMove (fst <$> maid)
    else do
      useTactics aid
      return aid
  treq <- case maid of
    Just (aidOld, treqOld) | aidToMove == aidOld ->
      return treqOld  -- no better leader found
    _ -> pickAction aidToMove (isJust maid)
  return (aidToMove, treq)

-- | Pick an action the actor will perform this turn.
pickAction :: MonadClient m => ActorId -> Bool -> m RequestAnyAbility
{-# INLINE pickAction #-}
pickAction aid retry = do
  side <- getsClient sside
  body <- getsState $ getActorBody aid
  let !_A = assert (bfid body == side
                    `blame` "AI tries to move enemy actor"
                    `twith` (aid, bfid body, side)) ()
  let !_A = assert (isNothing (btrajectory body)
                    `blame` "AI gets to manually move its projectiles"
                    `twith` (aid, bfid body, side)) ()
  stratAction <- actionStrategy aid retry
  let bestAction = bestVariant stratAction
      !_A = assert (not (nullFreq bestAction)  -- equiv to nullStrategy
                    `blame` "no AI action for actor"
                    `twith` (stratAction, aid, body)) ()
  -- Run the AI: chose an action from those given by the AI strategy.
  rndToAction $ frequency bestAction

udpdateCondInMelee :: MonadClient m => ActorId -> m ()
udpdateCondInMelee aid = do
  b <- getsState $ getActorBody aid
  condInMelee <- getsClient $ (EM.! blid b) . scondInMelee
  case condInMelee of
    Just{} -> return ()  -- still up to date
    Nothing -> do
      newCond <- condInMeleeM b  -- lazy and kept that way due to @Maybe@
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
  -- For the same reason @anyFoeAdj@ would not speed up this computation
  -- in normal gameplay (as opposed to AI vs AI benchmarks).
  allFoes <- getsState $ filter f . EM.elems . sactorD
  getsState $ any (\body ->
    bfid bodyOur == bfid body
    && blid bodyOur == blid body
    && not (bproj body)
    && bhp body > 0
    && any (\b -> adjacent (bpos b) (bpos body)) allFoes) . sactorD
