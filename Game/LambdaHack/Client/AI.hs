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

import Game.LambdaHack.Client.AI.ConditionM
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
{-# INLINE queryAI #-}
queryAI = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  let mleader = gleader fact
      oldAid = case mleader of
        Nothing -> assert `failure` fact
        Just aid -> aid
  udpdateCondInMelee oldAid
  (aidToMove, bToMove) <- pickActorToMove refreshTarget
  req <- ReqAITimed <$> pickAction (aidToMove, bToMove)
  if mleader /= Just aidToMove
    then return (req, Just aidToMove)
    else return (req, Nothing)

nonLeaderQueryAI :: MonadClient m => ActorId -> m RequestAI
{-# INLINE nonLeaderQueryAI #-}
nonLeaderQueryAI aid = do
  mleader <- getsClient _sleader
  let !_A = assert (mleader /= Just aid) ()
  udpdateCondInMelee aid
  useTactics refreshTarget aid
  bToMove <- getsState $ getActorBody aid
  req <- ReqAITimed <$> pickAction (aid, bToMove)
  return (req, Nothing)

udpdateCondInMelee :: MonadClient m => ActorId -> m ()
udpdateCondInMelee aid = do
  b <- getsState $ getActorBody aid
  condInMelee <- getsClient $ (EM.! blid b) . scondInMelee
  case condInMelee of
    Right{} -> return ()  -- still up to date
    Left oldCond -> do
      -- Lag the chages, to to avoid frequent invalidation of targets, etc.
      let dist = if oldCond then 3 else 1
      newCond <- condInMeleeM dist b
      modifyClient $ \cli ->
        cli {scondInMelee =
               EM.adjust (const $ Right (newCond, oldCond)) (blid b)
                         (scondInMelee cli)}

-- | Verify and possibly change the target of an actor. This function both
-- updates the target in the client state and returns the new target explicitly.
refreshTarget :: MonadClient m
              => (ActorId, Actor)  -- ^ the actor to refresh
              -> m (Maybe TgtAndPath)
refreshTarget (aid, body) = do
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
    -- Choose a target from those proposed by AI for the actor.
    tgtMPath <- rndToAction $ frequency $ bestVariant stratTarget
    modifyClient $ \cli ->
      cli {stargetD = EM.insert aid tgtMPath (stargetD cli)}
    return $ Just tgtMPath
  -- oldTgt <- getsClient $ EM.lookup aid . stargetD
  -- let _debug = T.unpack
  --         $ "\nHandleAI symbol:"    <+> tshow (bsymbol body)
  --         <> ", aid:"               <+> tshow aid
  --         <> ", pos:"               <+> tshow (bpos body)
  --         <> "\nHandleAI oldTgt:"   <+> tshow oldTgt
  --         <> "\nHandleAI strTgt:"   <+> tshow stratTarget
  --         <> "\nHandleAI target:"   <+> tshow tgtMPath
  --  trace _debug skip

-- | Pick an action the actor will perform this turn.
pickAction :: MonadClient m => (ActorId, Actor) -> m RequestAnyAbility
pickAction (aid, body) = do
  -- Pathing finished, randomize paths for next moves.
  modifyClient $ \cli -> cli {seps = seps cli + 773}
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
