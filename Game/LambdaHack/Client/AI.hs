-- | Semantics of most 'ResponseAI' client commands.
module Game.LambdaHack.Client.AI
  ( queryAI, pongAI
  ) where

import Control.Exception.Assert.Sugar
import qualified Data.EnumMap.Strict as EM
import qualified Data.Text as T

import Game.LambdaHack.Client.AI.HandleAbilityClient
import Game.LambdaHack.Client.AI.PickActorClient
import Game.LambdaHack.Client.AI.PickTargetClient
import Game.LambdaHack.Client.AI.Strategy
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Request

queryAI :: MonadClient m => ActorId -> m RequestAI
queryAI oldAid = do
  (aidToMove, bToMove) <- pickActorToMove refreshTarget oldAid
  RequestAnyAbility reqAny <- pickAction (aidToMove, bToMove)
  let req = ReqAITimed reqAny
  if aidToMove /= oldAid
    then return $! ReqAILeader aidToMove req
    else return $! req

pongAI :: MonadClient m => m RequestAI
pongAI = return ReqAIPong

refreshTarget :: MonadClient m
              => ActorId -> (ActorId, Actor)
              -> m (Maybe ((ActorId, Actor), (Target, PathEtc)))
refreshTarget oldLeader (aid, body) = do
  side <- getsClient sside
  assert (bfid body == side `blame` "AI tries to move an enemy actor"
                            `twith` (aid, body, side)) skip
  assert (not (bproj body) `blame` "AI gets to manually move its projectiles"
                           `twith` (aid, body, side)) skip
  stratTarget <- targetStrategy oldLeader aid
  tgtMPath <-
    if nullStrategy stratTarget then
      -- No sensible target; wipe out the old one.
      return Nothing
    else do
      -- Choose a target from those proposed by AI for the actor.
      (tgt, path) <- rndToAction $ frequency $ bestVariant stratTarget
      return $ Just (tgt, Just path)
  let _debug = T.unpack
          $ "\nHandleAI symbol:"    <+> tshow (bsymbol body)
          <> ", aid:"               <+> tshow aid
          <> ", pos:"               <+> tshow (bpos body)
          <> "\nHandleAI starget:"  <+> tshow stratTarget
          <> "\nHandleAI target:"   <+> tshow tgtMPath
--  trace _debug skip
  modifyClient $ \cli ->
    cli {stargetD = EM.alter (const $ tgtMPath) aid (stargetD cli)}
  return $! case tgtMPath of
    Just (tgt, Just pathEtc) -> Just ((aid, body), (tgt, pathEtc))
    _ -> Nothing

pickAction :: MonadClient m => (ActorId, Actor) -> m RequestAnyAbility
pickAction (aid, body) = do
  side <- getsClient sside
  assert (bfid body == side `blame` "AI tries to move enemy actor"
                            `twith` (aid, bfid body, side)) skip
  assert (not (bproj body) `blame` "AI gets to manually move its projectiles"
                           `twith` (aid, bfid body, side)) skip
  stratAction <- actionStrategy aid
  -- Run the AI: chose an action from those given by the AI strategy.
  rndToAction $ frequency $ bestVariant stratAction
