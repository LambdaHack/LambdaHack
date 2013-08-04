{-# LANGUAGE OverloadedStrings #-}
-- | Semantics of most 'CmdClientAI' client commands.
module Game.LambdaHack.Client.ClientSem where

import Control.Monad.Writer.Strict (WriterT, runWriterT)
import qualified Data.EnumMap.Strict as EM
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T

import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.Binding
import Game.LambdaHack.Client.HumanCmd
import Game.LambdaHack.Client.HumanLocal
import Game.LambdaHack.Client.HumanSem
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.RunAction
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.Strategy
import Game.LambdaHack.Client.StrategyAction
import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.ServerCmd
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.StrategyKind
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Utils.Frequency

queryAI :: MonadClient m => ActorId -> m CmdSer
queryAI oldAid = do
  Kind.COps{costrat=Kind.Ops{okind}} <- getsState scops
  side <- getsClient sside
  fact <- getsState $ \s -> sfactionD s EM.! side
  let aiMember = fmap okind $ gAiMember fact
  mleader <- getsClient _sleader
  if -- Keep the leader: only a leader is allowed to pick another leader.
     mleader /= Just oldAid
     -- Keep the leader: AIs are the same.
     || gAiLeader fact == gAiMember fact
     -- Keep the leader: other members can't melee.
     || Ability.Melee `notElem` maybe [] sabilities aiMember
    then queryAIPick oldAid
    else do
      fper <- getsClient sfper
      visFoes <- visibleFoes fper oldAid
      oldBody <- getsState $ getActorBody oldAid
      btarget <- getsClient $ getTarget oldAid
      let arena = blid oldBody
      ours <- getsState $ actorNotProjAssocs (== side) arena
      Level{lxsize} <- getsState $ \s -> sdungeon s EM.! arena
      if -- Keep the leader: he is alone on the level.
         length ours == 1
         -- Keep the leader: it still has an enemy target (even if not visible)
         || case btarget of Just TEnemy{} -> True; _ -> False
            -- ... and he is not yet adjacent to any foe.
            && all (not . adjacent lxsize (bpos oldBody))
                   (map (bpos . snd) visFoes)
        then queryAIPick oldAid
        else do
          -- Visibility ignored --- every foe is visible by somebody.
          foes <- getsState $ actorNotProjAssocs (isAtWar fact) arena
          let f (aid, b) =
                let distB p = chessDist lxsize (bpos b) p
                    foeDist = map (\(_, body) -> distB (bpos body)) foes
                    minDist | null foeDist = maxBound
                            | otherwise = minimum foeDist
                    maxChaseDist = 30
                    maxProximity = max 1 $ maxChaseDist - minDist
                in if aid == oldAid || minDist == 1
                   then Nothing  -- ignore, leader or already in melee range
                   else Just (maxProximity, aid)
              candidates = mapMaybe f ours
              freq | null candidates = toFreq "old leader" [(1, oldAid)]
                   | otherwise = toFreq "candidates for AI leader" candidates
          aid <- rndToAction $ frequency freq
          s <- getState
          modifyClient $ updateLeader aid s
          queryAIPick aid

queryAIPick :: MonadClient m => ActorId -> m CmdSer
queryAIPick aid = do
  side <- getsClient sside
  body <- getsState $ getActorBody aid
  assert (bfid body == side `blame` (aid, bfid body, side)) skip
  Kind.COps{costrat=Kind.Ops{okind}} <- getsState scops
  leader <- getsClient _sleader
  fact <- getsState $ (EM.! bfid body) . sfactionD
  let factionAI | Just aid /= leader = fromJust $ gAiMember fact
                | otherwise = fromJust $ gAiLeader fact
      factionAbilities = sabilities (okind factionAI)
  stratTarget <- targetStrategy aid factionAbilities
  -- Choose a target from those proposed by AI for the actor.
  btarget <- rndToAction $ frequency $ bestVariant stratTarget
  modifyClient $ updateTarget aid (const btarget)
  stratAction <- actionStrategy aid factionAbilities
  -- Run the AI: chose an action from those given by the AI strategy.
  action <- rndToAction $ frequency $ bestVariant $ stratAction
  let _debug = T.unpack
        $ "HandleAI abilities:"  <+> showT factionAbilities
        <>          ", symbol:"  <+> showT (bsymbol body)
        <>          ", aid:"     <+> showT aid
        <>          ", pos:"     <+> showT (bpos body)
        <> "\nHandleAI starget:" <+> showT stratTarget
        <> "\nHandleAI target:"  <+> showT btarget
        <> "\nHandleAI saction:" <+> showT stratAction
        <> "\nHandleAI action:"  <+> showT action
  -- trace _debug skip
  return action

-- | Handle the move of the hero.
queryUI :: (MonadClientAbort m, MonadClientUI m) => ActorId -> m CmdSer
queryUI aid = do
  -- When running, stop if aborted by a disturbance. Otherwise let
  -- the human player issue commands, until any of them takes time.
  leader <- getLeaderUI
  assert (leader == aid `blame` (leader, aid)) skip
  let inputHumanCmd msg = do
        stopRunning
        humanCommand msg
  tryWith inputHumanCmd $ do
    srunning <- getsClient srunning
    maybe abort (continueRun leader) srunning

-- | Continue running in the given direction.
continueRun :: MonadClientAbort m => ActorId -> (Vector, Int) -> m CmdSer
continueRun leader dd = do
  (dir, distNew) <- continueRunDir leader dd
  modifyClient $ \cli -> cli {srunning = Just (dir, distNew)}
  -- Attacks and opening doors disallowed when continuing to run.
  return $ RunSer leader dir

-- | Determine and process the next human player command. The argument is
-- the last abort message due to running, if any.
humanCommand :: forall m. (MonadClientAbort m, MonadClientUI m)
             => Msg
             -> m CmdSer
humanCommand msgRunAbort = do
  let loop :: Overlay -> m CmdSer
      loop overlay = do
        km <- getKeyOverlayCommand overlay
        -- Messages shown, so update history and reset current report.
        recordHistory
        -- On abort, just reset state and call loop again below.
        -- Each abort that gets this far generates a slide to be shown.
        (mcmdS, slides) <- runWriterT $ tryWithSlide (return Nothing) $ do
          -- Look up the key.
          Binding{kcmd} <- askBinding
          case M.lookup km kcmd of
            Just (_, _, cmd) -> do
              -- Query and clear the last command key.
              lastKey <- getsClient slastKey
              -- TODO: perhaps replace slastKey
              -- with test 'kmNext == km'
              -- or an extra arg to 'loop'.
              -- Depends on whether slastKey
              -- is needed in other parts of code.
              modifyClient (\st -> st {slastKey = Just km})
              if (Just km == lastKey)
                then cmdHumanSem Clear
                else cmdHumanSem cmd
            Nothing -> let msgKey = "unknown command <" <> K.showKM km <> ">"
                       in abortWith msgKey
        -- The command was aborted or successful and if the latter,
        -- possibly took some time.
        case mcmdS of
          Just cmdS -> assert (null (runSlideshow slides) `blame` slides) $ do
            -- Exit the loop and let other actors act. No next key needed
            -- and no slides could have been generated.
            modifyClient (\st -> st {slastKey = Nothing})
            return cmdS
          Nothing ->
            -- If no time taken, rinse and repeat.
            -- Analyse the obtained slides.
            case reverse (runSlideshow slides) of
              [] -> do
                -- Nothing special to be shown; by default draw current state.
                modifyClient (\st -> st {slastKey = Nothing})
                sli <- promptToSlideshow ""
                let overlayNext = head $ runSlideshow sli
                loop overlayNext
              sLast : sls -> do
                -- Show, one by one, all but the last slide.
                -- Note: the code that generates the slides is responsible
                -- for inserting the @more@ prompt.
                b <- getAllConfirms [km] $ toSlideshow $ reverse sls
                -- Display the last slide while waiting for the next key,
                -- or display current state if slideshow interrupted.
                overlayNext <-
                  if b
                  then return sLast
                  else do
                    modifyClient (\st -> st {slastKey = Nothing})
                    sli <- promptToSlideshow ""
                    return $! head $ runSlideshow sli
                -- Look up and perform the next command.
                loop overlayNext
  sli <- promptToSlideshow msgRunAbort
  let overlayInitial = head $ runSlideshow sli
  loop overlayInitial
