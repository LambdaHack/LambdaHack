-- | Ways for the client to use AI to produce server requests, based on
-- the client's view of the game state.
module Game.LambdaHack.Client.AI
  ( queryAI
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , pickActorAndAction
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Client.AI.PickActionM
import Game.LambdaHack.Client.AI.PickActorM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.Request
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Types

-- | Handle the move of an actor under AI control (regardless if the whole
-- faction is under human or computer control).
queryAI :: MonadClient m => ActorId -> m RequestAI
queryAI aid = do
  -- @sleader@ may be different from @gleader@ due to @stopPlayBack@,
  -- but only leaders may change faction leader, so we fix that beforehand:
  side <- getsClient sside
  body <- getsState $ getActorBody aid
  friendAssocs <- getsState $ friendRegularAssocs (bfid body) (blid body)
  mleader <- getsState $ gleader . (EM.! side) . sfactionD
  mleaderCli <- getsClient sleader
  unless (Just aid == mleader || mleader == mleaderCli) $
    -- @aid@ is not the leader, so he can't change leader later on,
    -- so we match the leaders here
    modifyClient $ \cli -> cli {_sleader = mleader}
  (aidToMove, treq, oldFlee) <- pickActorAndAction friendAssocs Nothing aid
  (aidToMove2, treq2) <-
    case treq of
      ReqWait | mleader == Just aid -> do
        -- Leader waits; a waste; try once to pick a yet different leader
        -- or at least a non-waiting action. Undo state changes in @pickAction@:
        modifyClient $ \cli -> cli
          { _sleader = mleader
          , sfleeD = EM.alter (const oldFlee) aidToMove $ sfleeD cli }
        (a, t, _) <- pickActorAndAction friendAssocs (Just aidToMove) aid
        return (a, t)
      _ -> return (aidToMove, treq)
  return ( ReqAITimed treq2
         , if aidToMove2 /= aid then Just aidToMove2 else Nothing )

-- | Pick an actor to move and an action for him to perform, given an optional
-- previous candidate actor and action and the server-proposed actor.
pickActorAndAction :: MonadClient m
                   => [(ActorId, Actor)] -> Maybe ActorId -> ActorId
                   -> m (ActorId, RequestTimed, Maybe (Point, Time))
pickActorAndAction friendAssocs maid aid = do
  mleader <- getsClient sleader
  aidToMove <-
    if mleader == Just aid
    then pickActorToMove friendAssocs maid
    else do
      setTargetFromDoctrines friendAssocs aid
      return aid
  oldFlee <- getsClient $ EM.lookup aidToMove . sfleeD
  -- Trying harder (@retry@) whenever no better leader found and so at least
  -- a non-waiting action should be found.
  -- If a new leader found, there is hope (but we don't check)
  -- that he gets a non-waiting action without any desperate measures.
  let retry = maybe False (aidToMove ==) maid
  treq <- pickAction friendAssocs aidToMove retry
  return (aidToMove, treq, oldFlee)
