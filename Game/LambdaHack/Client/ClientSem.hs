-- | Semantics of most 'CmdClientAI' client commands.
module Game.LambdaHack.Client.ClientSem where

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
import Game.LambdaHack.Client.ConfigUI
import Game.LambdaHack.Client.Draw
import Game.LambdaHack.Client.HumanSem
import Game.LambdaHack.Client.RunAction
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.Strategy
import Game.LambdaHack.Client.StrategyAction
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.HumanCmd
import qualified Game.LambdaHack.Common.Key as K
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Utils.Frequency

queryAI :: MonadClient m => ActorId -> m RequestTimed
queryAI oldAid = do
  Kind.COps{cotile, cofaction=Kind.Ops{okind}} <- getsState scops
  oldBody <- getsState $ getActorBody oldAid
  let side = bfid oldBody
      arena = blid oldBody
  fact <- getsState $ (EM.! side) . sfactionD
  lvl <- getLevel arena
  let leaderStuck = waitedLastTurn oldBody
      t = lvl `at` bpos oldBody
      abilityLeader = fAbilityLeader $ okind $ gkind fact
      abilityOther = fAbilityOther $ okind $ gkind fact
  mleader <- getsClient _sleader
  ours <- getsState $ actorNotProjAssocs (== side) arena
  let pickOld = do
        void $ refreshTarget oldAid (oldAid, oldBody)
        queryAIPick (oldAid, oldBody)
  case ours of
    _ | -- Keep the leader: only a leader is allowed to pick another leader.
        mleader /= Just oldAid
        -- Keep the leader: abilities are the same (we assume leader can do
        -- at least as much as others). TODO: check not accurate,
        -- instead define 'movesThisTurn' and use elsehwere.
        || abilityLeader == abilityOther
        -- Keep the leader: spawners can't change leaders themselves.
        || isSpawnFact fact
        -- Keep the leader: he is on stairs and not stuck
        -- and we don't want to clog stairs or get pushed to another level.
        || not leaderStuck && Tile.isStair cotile t
      -> pickOld
    [] -> assert `failure` (oldAid, oldBody)
    [_] -> pickOld  -- Keep the leader: he is alone on the level.
    (captain, captainBody) : (sergeant, sergeantBody) : _ -> do
      -- At this point we almost forget who the old leader was
      -- and treat all party actors the same, eliminating candidates
      -- until we can't distinguish them any more, at which point we prefer
      -- the old leader, if he is among the best candidates
      -- (to make the AI appear more human-like to easier to observe).
      -- TODO: this also takes melee into account, not shooting.
      oursTgt <- fmap catMaybes $ mapM (refreshTarget oldAid) ours
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
-- TODO: stuck actors are picked while others close could approach an enemy;
-- we should detect stuck actors (or one-sided stuck)
-- so far we only detect blocked and only in Other mode
--             && not (aid == oldAid && waitedLastTurn b time)  -- not stuck
-- this only prevents staying stuck
          (oursBlocked, oursPos) = partition targetBlocked oursOther
          valueOurs :: ((ActorId, Actor), (Target, PathEtc))
                    -> (Int, Int, Bool)
          valueOurs our@((aid, b), (TEnemy{}, (_, (_, d)))) =
            -- TODO: take weapon, walk and fight speed, etc. into account
            ( d + if targetBlocked our then 2 else 0  -- possible delay, hacky
            , - 10 * (bhp b `div` 10)
            , aid /= oldAid )
          valueOurs ((aid, b), (_tgt, (_path, (goal, d)))) =
            -- Keep proper formation, not too dense, not to sparse.
            let -- TODO: vary the parameters according to the stage of game,
                -- enough equipment or not, game mode, level map, etc.
                minSpread = 7
                maxSpread = 12 * 2
                dcaptain p =
                  chessDistVector $ displacement p (bpos captainBody)
                dsergeant p =
                  chessDistVector $ displacement p (bpos sergeantBody)
                minDist | aid == captain = dsergeant (bpos b)
                        | aid == sergeant = dcaptain (bpos b)
                        | otherwise = dsergeant (bpos b)
                                      `min` dcaptain (bpos b)
                pDist p = dcaptain p + dsergeant p
                sumDist = pDist (bpos b)
                -- Positive, if the goal gets us closer to the party.
                diffDist = sumDist - pDist goal
                minCoeff | minDist < minSpread =
                  (minDist - minSpread) `div` 3
                  - if aid == oldAid then 3 else 0
                         | otherwise = 0
                explorationValue = diffDist * (sumDist `div` 4)
-- TODO: this half is not yet ready:
-- instead spread targets between actors; moving many actors
-- to a single target and stopping and starting them
-- is very wasteful; also, pick targets not closest to the actor in hand,
-- but to the sum of captain and sergant or something
                sumCoeff | sumDist > maxSpread = - explorationValue
                         | otherwise = 0
            in ( if d == 0 then d
                 else max 1 $ minCoeff + if d < 10
                                         then 3 + d `div` 4
                                         else 9 + d `div` 10
               , sumCoeff
               , aid /= oldAid )
          sortOurs = sortBy $ comparing valueOurs
          goodGeneric _our@((aid, b), (_tgt, _pathEtc)) =
            bhp b > 0  -- not incapacitated
            && not (aid == oldAid && waitedLastTurn b)  -- not stuck
          goodTEnemy our@((_aid, b), (_tgt, (_path, (goal, _d)))) =
            not (adjacent (bpos b) goal) -- not in melee range already
            && goodGeneric our
          oursTEnemyGood = filter goodTEnemy oursTEnemy
          oursPosGood = filter goodGeneric oursPos
          oursBlockedGood = filter goodGeneric oursBlocked
          candidates = sortOurs oursTEnemyGood
                       ++ sortOurs oursPosGood
                       ++ sortOurs oursBlockedGood
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
      -- No sensible target, wipe out the old one  .
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

queryAIPick :: MonadClient m => (ActorId, Actor) -> m RequestTimed
queryAIPick (aid, body) = do
  side <- getsClient sside
  assert (bfid body == side `blame` "AI tries to move enemy actor"
                            `twith` (aid, bfid body, side)) skip
  assert (not (bproj body) `blame` "AI gets to manually move its projectiles"
                           `twith` (aid, bfid body, side)) skip
  stratAction <- actionStrategy aid
  -- Run the AI: chose an action from those given by the AI strategy.
  rndToAction $ frequency $ bestVariant stratAction

-- | Handle the move of a UI player.
queryUI :: MonadClientUI m => ActorId -> m Request
queryUI aid = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  -- When running, stop if disturbed. If not running, let the human
  -- player issue commands, until any command takes time.
  leader <- getLeaderUI
  assert (leader == aid `blame` "player moves not his leader"
                        `twith` (leader, aid)) skip
  srunning <- getsClient srunning
  case srunning of
    Nothing -> humanCommand Nothing
    Just RunParams{runMembers} | isSpawnFact fact && runMembers /= [aid] -> do
      stopRunning
      ConfigUI{configRunStopMsgs} <- getsClient sconfigUI
      let msg = if configRunStopMsgs
                then Just $ "Run stop: spawner leader change"
                else Nothing
      humanCommand msg
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
          return $! ReqTimed runCmd

-- | Determine and process the next human player command. The argument is
-- the last stop message due to running, if any.
humanCommand :: forall m. MonadClientUI m
             => Maybe Msg
             -> m Request
humanCommand msgRunStop = do
  -- For human UI we invalidate whole @sbfsD@ at the start of each
  -- UI player input, which is an overkill, but doesn't affects
  -- screensavers, because they are UI, but not human.
  modifyClient $ \cli -> cli {sbfsD = EM.empty}
  let loop :: Maybe (Bool, Overlay) -> m Request
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
              ReqTimed cmd ->
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
