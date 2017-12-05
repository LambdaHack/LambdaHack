{-# LANGUAGE GADTs #-}
-- | Ways for the client to use AI to produce server requests, based on
-- the client's view of the game state.
module Game.LambdaHack.Client.AI
  ( queryAI
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , pickActorAndAction, udpdateCondInMelee
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Client.AI.HandleAbilityM
import Game.LambdaHack.Client.AI.PickActorM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.Request
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.State

-- | Handle the move of an actor under AI control (regardless if the whole
-- faction is under human or computer control).
queryAI :: MonadClient m => ActorId -> m RequestAI
queryAI aid = do
  -- @sleader@ may be different from @gleader@ due to @stopPlayBack@,
  -- but only leaders may change faction leader, so we fix that:
  side <- getsClient sside
  mleader <- getsState $ gleader . (EM.! side) . sfactionD
  mleaderCli <- getsClient sleader
  unless (Just aid == mleader || mleader == mleaderCli) $
    -- @aid@ is not the leader, so he can't change leader
    modifyClient $ \cli -> cli {_sleader = mleader}
  -- @condInMelee@ will most proably be needed in the following functions,
  -- but even if not, it's OK, it's not forced, because wrapped in @Maybe@:
  udpdateCondInMelee aid
  (aidToMove, treq) <- pickActorAndAction Nothing aid
  (aidToMove2, treq2) <-
    case treq of
      RequestAnyAbility ReqWait | mleader == Just aid -> do
        -- leader waits; a waste; try once to pick a yet different leader
        modifyClient $ \cli -> cli {_sleader = mleader}  -- undo previous choice
        pickActorAndAction (Just (aidToMove, treq)) aid
      _ -> return (aidToMove, treq)
  return ( ReqAITimed treq2
         , if aidToMove2 /= aid then Just aidToMove2 else Nothing )

-- | Pick an actor to move and an action for him to perform, given an optional
-- previous candidate actor and action and the server-proposed actor.
pickActorAndAction :: MonadClient m
                     => Maybe (ActorId, RequestAnyAbility) -> ActorId
                     -> m (ActorId, RequestAnyAbility)
-- This inline speeds up execution by 10% and increases allocation by 15%,
-- despite probably bloating executable:
{-# INLINE pickActorAndAction #-}
pickActorAndAction maid aid = do
  mleader <- getsClient sleader
  aidToMove <-
    if mleader == Just aid
    then pickActorToMove (fst <$> maid)
    else do
      setTargetFromTactics aid
      return aid
  treq <- case maid of
    Just (aidOld, treqOld) | aidToMove == aidOld ->
      return treqOld  -- no better leader found
    _ -> pickAction aidToMove (isJust maid)
  return (aidToMove, treq)

-- | Check if any non-dying foe (projectile or not) is adjacent
-- to any of our normal actors (whether they can melee or just need to flee,
-- in which case alert is needed so that they are not slowed down by others)
-- and record this per-level. This is needed only by AI and computed
-- as lazily as possible before each round of AI deliberations.
udpdateCondInMelee :: MonadClient m => ActorId -> m ()
udpdateCondInMelee aid = do
  b <- getsState $ getActorBody aid
  condInMelee <- getsClient $ (EM.! blid b) . scondInMelee
  case condInMelee of
    Just{} -> return ()  -- still up to date
    Nothing -> do
      newCond <- getsState $ inMelee b
        -- lazy and kept that way due to @Maybe@
      modifyClient $ \cli ->
        cli {scondInMelee =
               EM.adjust (const $ Just newCond) (blid b) (scondInMelee cli)}
