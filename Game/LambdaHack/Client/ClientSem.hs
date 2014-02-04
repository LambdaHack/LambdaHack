-- | Semantics of most 'CmdClientAI' client commands.
module Game.LambdaHack.Client.ClientSem where

import Control.Arrow ((&&&))
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import qualified Data.Text as T

import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.Binding
import Game.LambdaHack.Client.Config
import Game.LambdaHack.Client.Draw
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
import Game.LambdaHack.Common.HumanCmd
import qualified Game.LambdaHack.Common.Key as K
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.ServerCmd
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Utils.Frequency

queryAI :: MonadClient m => ActorId -> m CmdTakeTimeSer
queryAI oldAid = do
  Kind.COps{cofaction=Kind.Ops{okind}} <- getsState scops
  oldBody <- getsState $ getActorBody oldAid
  let side = bfid oldBody
      arena = blid oldBody
  fact <- getsState $ \s -> sfactionD s EM.! side
  let abilityLeader = fAbilityLeader $ okind $ gkind fact
      abilityOther = fAbilityOther $ okind $ gkind fact
  mleader <- getsClient _sleader
  ours <- getsState $ actorNotProjAssocs (== side) arena
  if -- Keep the leader: only a leader is allowed to pick another leader.
     mleader /= Just oldAid
     -- Keep the leader: abilities are the same (we assume leader can do
     -- at least as much as others).
     || abilityLeader == abilityOther
     -- Keep the leader: other members can't melee. -- TODO: but can explore
     || Ability.Melee `notElem` abilityOther
     -- Keep the leader: he probably used stairs right now
     -- and we don't want to clog stairs or get pushed to another level.
     || bpos oldBody == boldpos oldBody
     -- Keep the leader: he is alone on the level.
     || length ours == 1
    then do
      void $ refreshTarget (oldAid, oldBody)
      queryAIPick (oldAid, oldBody)
    else do
      -- At this point we almost forget who the old leader was
      -- and treat all party actors the same, eliminating candidates
      -- until we can't distinguish them any more, at which point we prefer
      -- the old leader, if he is among the best candidates
      -- (to make the AI appear more human-like to easier to observe).
      -- TODO: this also takes melee into account, not shooting.
      time <- getsState $ getLocalTime arena
      oursTgt <- fmap catMaybes $ mapM refreshTarget ours
      let targetTEnemy (_, (TEnemy{}, _)) = True
          targetTEnemy _ = False
          (oursTEnemy, oursOther) = partition targetTEnemy oursTgt
          -- These are not necessarily stuck (perhaps can go around),
          -- but their current path is blocked by friends.
          targetBlocked our@((_aid, _b), (_tgt, (path, _etc))) =
            let next = case path of
                  [] -> assert `failure` our
                  [_goal] -> Nothing
                  _ : q : _ -> Just q
            in any ((== next) . Just . bpos . snd) ours
          (oursBlocked, oursPos) = partition targetBlocked oursOther
          valueOurs :: ((ActorId, Actor), (Target, PathEtc)) -> (Int, Bool)
          valueOurs = snd . snd . snd . snd &&& (/= oldAid) . fst . fst
          sortOurs = sortBy $ comparing valueOurs
          goodGeneric _our@((aid, b), (_tgt, _pathEtc)) =
            bhp b > 0  -- not incapacitated
            && not (aid == oldAid && waitedLastTurn b time)  -- not stuck
          goodTEnemy our@((_aid, b), (_tgt, (_path, (goal, _d)))) =
            not (adjacent (bpos b) goal) -- not in melee range already
            && goodGeneric our
          oursTEnemyGood = filter goodTEnemy oursTEnemy
          oursPosGood = filter goodGeneric oursPos
          oursBlockedGood = filter goodGeneric oursBlocked
          blurDistance (ab, (tgt, (path, (goal, d)))) =
            (ab, (tgt, (path, (goal, d `div` 4))))
          oursPosGoodCoarse = map blurDistance oursPosGood
          oursBlockedGoodCoarse = map blurDistance oursBlockedGood
          candidates = sortOurs oursTEnemyGood
                       ++ sortOurs oursPosGoodCoarse
                       ++ sortOurs oursBlockedGoodCoarse
      case candidates of
        [] -> queryAIPick (oldAid, oldBody)
        c : _ -> do
          let best = takeWhile ((== valueOurs c) . valueOurs) candidates
              freq = uniformFreq "candidates for AI leader" best
          ((aid, b), _) <- rndToAction $ frequency freq
          s <- getState
          modifyClient $ updateLeader aid s
          queryAIPick (aid, b)

refreshTarget :: MonadClient m
              => (ActorId, Actor)
              -> m (Maybe ((ActorId, Actor), (Target, PathEtc)))
refreshTarget (aid, body) = do
  Kind.COps{cofaction=Kind.Ops{okind}} <- getsState scops
  side <- getsClient sside
  assert (bfid body == side `blame` "AI tries to move an enemy actor"
                            `twith` (aid, body, side)) skip
  assert (not (bproj body) `blame` "AI gets to manually move its projectiles"
                           `twith` (aid, body, side)) skip
  mleader <- getsClient _sleader
  fact <- getsState $ (EM.! bfid body) . sfactionD
  let factionAbilities
        | Just aid == mleader = fAbilityLeader $ okind $ gkind fact
        | otherwise = fAbilityOther $ okind $ gkind fact
  stratTarget <- targetStrategy aid
  tgtMPath <-
    if nullStrategy stratTarget then
      -- No sensible target, wipe out the old one  .
      return Nothing
    else do
      -- Choose a target from those proposed by AI for the actor.
      (tgt, path) <- rndToAction $ frequency $ bestVariant stratTarget
      return $ Just (tgt, Just path)
  let _debug = T.unpack
          $ "\nHandleAI abilities:" <+> tshow factionAbilities
          <> ", symbol:"            <+> tshow (bsymbol body)
          <> ", aid:"               <+> tshow aid
          <> ", pos:"               <+> tshow (bpos body)
          <> "\nHandleAI starget:"  <+> tshow stratTarget
          <> "\nHandleAI target:"   <+> tshow tgtMPath
--  trace _debug skip
  modifyClient $ \cli ->
    cli {stargetD = EM.alter (const $ tgtMPath) aid (stargetD cli)}
--  trace _debug skip
  return $! case tgtMPath of
    Just (tgt, Just pathEtc) -> Just ((aid, body), (tgt, pathEtc))
    _ -> Nothing

queryAIPick :: MonadClient m => (ActorId, Actor) -> m CmdTakeTimeSer
queryAIPick (aid, body) = do
  Kind.COps{cofaction=Kind.Ops{okind}} <- getsState scops
  side <- getsClient sside
  assert (bfid body == side `blame` "AI tries to move enemy actor"
                            `twith` (aid, bfid body, side)) skip
  assert (not (bproj body) `blame` "AI gets to manually move its projectiles"
                           `twith` (aid, bfid body, side)) skip
  mleader <- getsClient _sleader
  fact <- getsState $ (EM.! bfid body) . sfactionD
  let factionAbilities
        | Just aid == mleader = fAbilityLeader $ okind $ gkind fact
        | otherwise = fAbilityOther $ okind $ gkind fact
  stratAction <- actionStrategy aid factionAbilities
  -- Run the AI: chose an action from those given by the AI strategy.
  rndToAction $ frequency $ bestVariant stratAction

-- | Handle the move of a UI player.
queryUI :: MonadClientUI m => ActorId -> m CmdSer
queryUI aid = do
  -- When running, stop if disturbed. If not running, let the human
  -- player issue commands, until any command takes time.
  leader <- getLeaderUI
  assert (leader == aid `blame` "player moves not his leader"
                        `twith` (leader, aid)) skip
  srunning <- getsClient srunning
  case srunning of
    Nothing -> humanCommand Nothing
    Just runParams -> do
      runOutcome <- continueRun runParams
      case runOutcome of
        Left stopMsg -> do
          stopRunning
          ConfigUI{configRunStopMsgs} <- getsClient sconfigUI
          let msg = if configRunStopMsgs
                    then Just $ "Run stop:" <+> stopMsg
                    else Nothing
          humanCommand msg
        Right (paramNew, runCmd) -> do
          modifyClient $ \cli -> cli {srunning = Just paramNew}
          return $! CmdTakeTimeSer runCmd

-- | Determine and process the next human player command. The argument is
-- the last stop message due to running, if any.
humanCommand :: forall m. MonadClientUI m
             => Maybe Msg
             -> m CmdSer
humanCommand msgRunStop = do
  -- For human UI we invalidate whole @sbfsD@ at the start of each
  -- UI player input, which is an overkill, but doesn't affects
  -- screensavers, because they are UI, but not human.
  modifyClient $ \cli -> cli {sbfsD = EM.empty}
  let loop :: Maybe (Bool, Overlay) -> m CmdSer
      loop mover = do
        (lastBlank, over) <- case mover of
          Nothing -> do
            -- Display current state if no slideshow or if interrupted.
            modifyClient $ \cli -> cli {slastKey = Nothing}
            sli <- promptToSlideshow ""
            return (False, head . snd $! slideshow sli)
          Just bLast ->
            -- (Re-)display the last slide while waiting for the next key.
            return bLast
        (seqCurrent, seqPrevious, k) <- getsClient slastRecord
        case k of
          0 -> do
            let slastRecord = ([], seqCurrent, 0)
            modifyClient $ \cli -> cli {slastRecord}
          _ -> do
            let slastRecord = ([], seqCurrent ++ seqPrevious, k - 1)
            modifyClient $ \cli -> cli {slastRecord}
        km <- getKeyOverlayCommand lastBlank over
        -- Messages shown, so update history and reset current report.
        recordHistory
        abortOrCmd <- do
          -- Look up the key.
          Binding{bcmdMap} <- askBinding
          case M.lookup km bcmdMap of
            Just (_, _, cmd) -> do
              -- Query and clear the last command key.
              lastKey <- getsClient slastKey
              stgtMode <- getsClient stgtMode
              modifyClient $ \cli -> cli
                {swaitTimes = if swaitTimes cli > 0
                              then - swaitTimes cli
                              else 0}
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
            let (onBlank, sli) = slideshow slides
            mLast <- case reverse sli  of
              [] -> return Nothing
              [sLast] -> return $ Just (onBlank, sLast)
              sls@(sLast : _) -> do
                -- Show, one by one, all slides, awaiting confirmation
                -- for all but the last one.
                -- Note: the code that generates the slides is responsible
                -- for inserting the @more@ prompt.
                go <- getInitConfirms ColorFull [km]
                      $ toSlideshow onBlank $ reverse $ map overlay sls
                return $! if go then Just (onBlank, sLast) else Nothing
            loop mLast
  case msgRunStop of
    Nothing -> loop Nothing
    Just msg -> do
      sli <- promptToSlideshow msg
      loop $ Just (False, head . snd $ slideshow sli)
