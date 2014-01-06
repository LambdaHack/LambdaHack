-- | Semantics of most 'CmdClientAI' client commands.
module Game.LambdaHack.Client.ClientSem where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T

import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.Binding
import Game.LambdaHack.Client.Config
import Game.LambdaHack.Client.Draw
import Game.LambdaHack.Client.HumanCmd
import Game.LambdaHack.Client.HumanLocal
import Game.LambdaHack.Client.HumanSem
import Game.LambdaHack.Client.RunAction
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.Strategy
import Game.LambdaHack.Client.StrategyAction
import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Key as K
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.ServerCmd
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Utils.Frequency

queryAI :: MonadClient m => ActorId -> m CmdTakeTimeSer
queryAI oldAid = do
  Kind.COps{cofaction=Kind.Ops{okind}, corule} <- getsState scops
  side <- getsClient sside
  fact <- getsState $ \s -> sfactionD s EM.! side
  let abilityLeader = fAbilityLeader $ okind $ gkind fact
      abilityOther = fAbilityOther $ okind $ gkind fact
  mleader <- getsClient _sleader
  if -- Keep the leader: only a leader is allowed to pick another leader.
     mleader /= Just oldAid
     -- Keep the leader: abilities are the same (we assume leader can do
     -- at least as much as others).
     || abilityLeader == abilityOther
     -- Keep the leader: other members can't melee.
     || Ability.Melee `notElem` abilityOther
    then queryAIPick oldAid
    else do
      fper <- getsClient sfper
      oldBody <- getsState $ getActorBody oldAid
      oldAis <- getsState $ getActorItem oldAid
      btarget <- getsClient $ getTarget oldAid
      let arena = blid oldBody
      -- Visibility ignored --- every foe is visible by somebody.
      foes <- getsState $ actorNotProjList (isAtWar fact) arena
      ours <- getsState $ actorNotProjAssocs (== side) arena
      time <- getsState $ getLocalTime arena
      Level{lxsize, lysize} <- getsState $ \s -> sdungeon s EM.! arena
      actorD <- getsState sactorD
      let oldPos = bpos oldBody
          per = fper EM.! arena
          posOnLevel b | blid b /= arena = Nothing
                       | otherwise = Just $ bpos b
          mfoePos foe = maybe Nothing posOnLevel
                        $ EM.lookup foe actorD
          canSee foe = maybe False (actorSeesPos per oldAid) $ mfoePos foe
          isAmmo i = jsymbol i `elem` ritemProject (Kind.stdRuleset corule)
          hasAmmo = any (isAmmo . snd) oldAis
          isAdjacent = foesAdjacent lxsize lysize oldPos foes
      if -- Keep the leader: he is alone on the level.
         length ours == 1
         -- Keep the leader: he has an enemy target.
         || case btarget of
              Just (TEnemy foe _) ->
                -- and he can shoot it.
                canSee foe && hasAmmo && not isAdjacent
              _ -> False
         -- Keep the leader: he probably just used stairs.
         || bpos oldBody == boldpos oldBody
            && not (waitedLastTurn oldBody time)
        then queryAIPick oldAid
        else do
          let countMinFoeDist (aid, b) =
                let distB = chessDist (bpos b)
                    foeDist = map (distB . bpos) foes
                    minFoeDist | null foeDist = maxBound
                               | otherwise = minimum foeDist
                in ((aid, b), minFoeDist)
              oursMinFoeDist = map countMinFoeDist ours
              inMelee (_, minFoeDist) = minFoeDist == 1
              oursMeleePos = map (bpos . snd . fst)
                             $ filter inMelee oursMinFoeDist
          let f ((aid, b), minFoeDist) =
                let distB = chessDist (bpos b)
                    meleeDist = map distB oursMeleePos
                    minMeleeDist | null meleeDist = maxBound
                                 | otherwise = minimum meleeDist
                    proximityMelee = max 0 $ 10 - minMeleeDist
                    proximityFoe = max 0 $ 20 - minFoeDist
                    distToLeader = distB oldPos
                    proximityLeader = max 0 $ 10 - distToLeader
                in if minFoeDist == 1
                      || bhp b <= 0
                      || aid == oldAid && waitedLastTurn b time
                   then -- Ignore: in melee range or incapacitated or stuck.
                        Nothing
                   else -- Help in melee, shoot or chase foes,
                        -- fan out away from each other, if too close.
                        Just ( 1
                               + proximityMelee * 9
                               + proximityFoe * 6
                               + proximityLeader * 3
                             , aid )
              candidates = mapMaybe f oursMinFoeDist
              freq | null candidates = toFreq "old leader" [(1, oldAid)]
                   | otherwise = toFreq "candidates for AI leader" candidates
          aid <- rndToAction $ frequency freq
          s <- getState
          modifyClient $ updateLeader aid s
          queryAIPick aid

queryAIPick :: MonadClient m => ActorId -> m CmdTakeTimeSer
queryAIPick aid = do
  Kind.COps{cofaction=Kind.Ops{okind}} <- getsState scops
  side <- getsClient sside
  body <- getsState $ getActorBody aid
  assert (bfid body == side `blame` "AI tries to move enemy actor"
                            `twith` (aid, bfid body, side)) skip
  mleader <- getsClient _sleader
  fact <- getsState $ (EM.! bfid body) . sfactionD
  let factionAbilities
        | Just aid == mleader = fAbilityLeader $ okind $ gkind fact
        | otherwise = fAbilityOther $ okind $ gkind fact
  unless (bproj body) $ do
    stratTarget <- targetStrategy aid factionAbilities
    -- Choose a target from those proposed by AI for the actor.
    btarget <- rndToAction $ frequency $ bestVariant stratTarget
    let _debug = T.unpack
          $ "\nHandleAI abilities:" <+> showT factionAbilities
          <> ", symbol:"            <+> showT (bsymbol body)
          <> ", aid:"               <+> showT aid
          <> ", pos:"               <+> showT (bpos body)
          <> "\nHandleAI starget:"  <+> showT stratTarget
          <> "\nHandleAI target:"   <+> showT btarget
--    trace _debug skip
    modifyClient $ updateTarget aid (const btarget)
  stratAction <- actionStrategy aid factionAbilities
  -- Run the AI: chose an action from those given by the AI strategy.
  action <- rndToAction $ frequency $ bestVariant stratAction
  let _debug = T.unpack
          $ "HandleAI saction:"   <+> showT stratAction
          <> "\nHandleAI action:" <+> showT action
--  trace _debug skip
  return action

-- | Handle the move of a UI player.
queryUI :: MonadClientUI m => ActorId -> m CmdSer
queryUI aid = do
  -- When running, stop if disturbed. Otherwise let
  -- the human player issue commands, until any of them takes time.
  leader <- getLeaderUI
  assert (leader == aid `blame` "player moves not his leader"
                        `twith` (leader, aid)) skip
  srunning <- getsClient srunning
  case srunning of
    Nothing -> humanCommand ""
    Just runParams -> do
      runOutcome <- continueRun runParams
      case runOutcome of
        Left stopMsg -> do
          stopRunning
          ConfigUI{configRunStopMsgs} <- getsClient sconfigUI
          let msg = if configRunStopMsgs
                    then "Run stop:" <+> stopMsg
                    else ""
          humanCommand msg
        Right (paramNew, runCmd) -> do
          modifyClient $ \cli -> cli {srunning = Just paramNew}
          return $ CmdTakeTimeSer runCmd

-- | Determine and process the next human player command. The argument is
-- the last stop message due to running, if any.
humanCommand :: forall m. MonadClientUI m
             => Msg
             -> m CmdSer
humanCommand msgRunStop = do
  let loop :: Maybe Overlay -> m CmdSer
      loop mover = do
        over <- case mover of
          Nothing -> do
            -- Display current state if no slideshow or if interrupted.
            modifyClient $ \cli -> cli {slastKey = Nothing}
            sli <- promptToSlideshow ""
            return $! head $! slideshow sli
          Just sLast ->
            -- (Re-)display the last slide while waiting for the next key.
            return sLast
        modifyClient $ \cli -> cli {slastRepeat = slastRepeat cli - 1}
        slastRepeat <- getsClient slastRepeat
        if slastRepeat <= 0
          then modifyClient $ \cli -> cli { slastSeq1 = []
                                          , slastSeq2 = if slastSeq1 cli == []
                                                        then slastSeq2 cli
                                                        else slastSeq1 cli}
          else modifyClient $ \cli -> cli {slastSeq1 = reverse $ slastSeq2 cli}
        km <- getKeyOverlayCommand over
        -- Messages shown, so update history and reset current report.
        recordHistory
        abortOrCmd <- do
          -- Look up the key.
          Binding{kcmd} <- askBinding
          case M.lookup km kcmd of
            Just (_, _, cmd) -> do
              -- Query and clear the last command key.
              lastKey <- getsClient slastKey
              stgtMode <- getsClient stgtMode
              if Just km == lastKey
                 || km == K.escKey && isNothing stgtMode && isJust mover
                then do
                  modifyClient $ \cli -> cli {slastKey = Nothing}
                  cmdHumanSem Clear
                else do
                  modifyClient $ \cli -> cli {slastKey = Just km}
                  cmdHumanSem cmd
            Nothing -> let msgKey = "unknown command <" <> K.showKM km <> ">"
                       in fmap Left $ promptToSlideshow msgKey
        -- The command was failed or successful and if the latter,
        -- possibly took some time.
        case abortOrCmd of
          Right cmdS -> do
            -- Exit the loop and let other actors act. No next key needed
            -- and no slides could have been generated.
            modifyClient $ \cli -> cli {slastKey = Nothing}
            case cmdS of
              CmdTakeTimeSer cmd ->
                modifyClient $ \cli -> cli {slastCmd = Just cmd}
              _ -> return ()
            return cmdS
          Left slides -> do
            -- If no time taken, rinse and repeat.
            -- Analyse the obtained slides.
            mLast <- case reverse $ slideshow slides of
              [] -> return Nothing
              [sLast] -> return $ Just sLast
              sls@(sLast : _) -> do
                -- Show, one by one, all slides, awaiting confirmation
                -- for all but the last one.
                -- Note: the code that generates the slides is responsible
                -- for inserting the @more@ prompt.
                go <- getInitConfirms ColorFull [km]
                      $ toSlideshow $ reverse $ map overlay sls
                return $! if go then Just sLast else Nothing
            loop mLast
  sli <- promptToSlideshow msgRunStop
  let overlayInitial = head $ slideshow sli
  loop $ Just overlayInitial
