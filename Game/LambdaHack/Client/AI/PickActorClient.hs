{-# LANGUAGE TupleSections #-}
-- | Semantics of most 'ResponseAI' client commands.
module Game.LambdaHack.Client.AI.PickActorClient
  ( pickActorToMove
  ) where

import Control.Applicative
import Control.Arrow
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.List
import Data.Maybe
import Data.Ord

import Game.LambdaHack.Client.AI.ConditionClient
import Game.LambdaHack.Client.AI.PickTargetClient
import Game.LambdaHack.Client.BfsClient
import Game.LambdaHack.Client.CommonClient
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ModeKind

pickActorToMove :: MonadClient m
                => ((ActorId, Actor) -> m (Maybe (Target, PathEtc)))
                -> ActorId
                -> m (ActorId, Actor)
pickActorToMove refreshTarget oldAid = do
  Kind.COps{cotile} <- getsState scops
  oldBody <- getsState $ getActorBody oldAid
  let side = bfid oldBody
      arena = blid oldBody
  fact <- getsState $ (EM.! side) . sfactionD
  lvl <- getLevel arena
  let leaderStuck = waitedLastTurn oldBody
      t = lvl `at` bpos oldBody
  mleader <- getsClient _sleader
  ours <- getsState $ actorRegularAssocs (== side) arena
  let explore = void $ refreshTarget (oldAid, oldBody)
      pickOld = do
        if mleader == Just oldAid then explore
        else case ftactic $ gplayer fact of
          TBlock -> return ()  -- no point refreshing target
          TFollow ->
            case mleader of
              -- If no leader at all (forced @TFollow@ tactic on an actor
              -- from a leaderless faction), fall back to @TExplore@.
              Nothing -> explore
              Just leader -> do
                onLevel <- getsState $ memActor leader arena
                -- If leader not on this level, fall back to @TExplore@.
                if not onLevel then explore
                else do
                  -- Copy over the leader's target, if any, or follow his bpos.
                  tgtLeader <- do
                    mtgt <- getsClient $ EM.lookup leader . stargetD
                    case mtgt of
                      Nothing -> return $! TEnemy leader True
                      Just (tgtLeader, _) -> return tgtLeader
                  modifyClient $ \cli ->
                    cli { sbfsD = invalidateBfs oldAid (sbfsD cli)
                        , seps = seps cli + 773 }  -- randomize paths
                  mpath <- createPath oldAid tgtLeader
                  let tgtMPath =
                        maybe (tgtLeader, Nothing) (second Just) mpath
                  modifyClient $ \cli ->
                    cli {stargetD = EM.alter (const $ Just tgtMPath)
                                             oldAid (stargetD cli)}
          TExplore -> explore
          TRoam -> explore  -- @TRoam@ is checked again inside @explore@
          TPatrol -> explore  -- TODO
        return (oldAid, oldBody)
  case ours of
    _ | -- Keep the leader: only a leader is allowed to pick another leader.
        mleader /= Just oldAid
        -- Keep the leader: the faction forbids client leader change on level.
        || snd (autoDungeonLevel fact)
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
      -- (to make the AI appear more human-like and easier to observe).
      -- TODO: this also takes melee into account, but not shooting.
      let refresh aidBody = do
            mtgt <- refreshTarget aidBody
            return $! (aidBody,) <$> mtgt
      oursTgt <- catMaybes <$> mapM refresh ours
      let actorVulnerable ((aid, body), _) = do
            activeItems <- activeItemsClient aid
            condMeleeBad <- condMeleeBadM aid
            threatDistL <- threatDistList aid
            (fleeL, _) <- fleeList aid
            let actorMaxSk = sumSkills activeItems
                abInMaxSkill ab = EM.findWithDefault 0 ab actorMaxSk > 0
                condNoUsableWeapon = all (not . isMelee) activeItems
                canMelee = abInMaxSkill AbMelee && not condNoUsableWeapon
                condCanFlee = not (null fleeL)
                condThreatAtHandVeryClose =
                  not $ null $ takeWhile ((<= 2) . fst) threatDistL
                threatAdj = takeWhile ((== 1) . fst) threatDistL
                condThreatAdj = not $ null threatAdj
                condFastThreatAdj =
                  any (\(_, (_, b)) ->
                         bspeed b activeItems > bspeed body activeItems)
                      threatAdj
                heavilyDistressed =
                  -- Actor hit by a projectile or similarly distressed.
                  deltaSerious (bcalmDelta body)
            return $! not (canMelee && condThreatAdj)
                      && if condThreatAtHandVeryClose
                         then condCanFlee && condMeleeBad
                              && not condFastThreatAdj
                         else heavilyDistressed  -- shot at
                           -- TODO: modify when reaction fire is possible
          actorHearning (_, (TEnemyPos{}, (_, (_, d)))) | d <= 2 =
            return False  -- noise probably due to fleeing target
          actorHearning ((_aid, b), _) = do
            allFoes <- getsState $ actorRegularList (isAtWar fact) (blid b)
            let closeFoes = filter ((<= 3) . chessDist (bpos b) . bpos) allFoes
                mildlyDistressed = deltaMild (bcalmDelta b)
            return $! mildlyDistressed  -- e.g., actor hears an enemy
                      && null closeFoes  -- the enemy not visible; a trap!
          -- AI has to be prudent and not lightly waste leader for meleeing,
          -- even if his target is distant
          actorMeleeing ((aid, _), _) = condAnyFoeAdjM aid
          actorMeleeBad ((aid, _), _) = do
            threatDistL <- threatDistList aid
            let condThreatMedium =  -- if foes far, friends may still come
                  not $ null $ takeWhile ((<= 5) . fst) threatDistL
            condMeleeBad <- condMeleeBadM aid
            return $! condThreatMedium && condMeleeBad
      oursVulnerable <- filterM actorVulnerable oursTgt
      oursSafe <- filterM (fmap not . actorVulnerable) oursTgt
        -- TODO: partitionM
      oursMeleeing <- filterM actorMeleeing oursSafe
      oursNotMeleeing <- filterM (fmap not . actorMeleeing) oursSafe
      oursHearing <- filterM actorHearning oursNotMeleeing
      oursNotHearing <- filterM (fmap not . actorHearning) oursNotMeleeing
      oursMeleeBad <- filterM actorMeleeBad oursNotHearing
      oursNotMeleeBad <- filterM (fmap not . actorMeleeBad) oursNotHearing
      let targetTEnemy (_, (TEnemy{}, _)) = True
          targetTEnemy (_, (TEnemyPos{}, _)) = True
          targetTEnemy _ = False
          (oursTEnemy, oursOther) = partition targetTEnemy oursNotMeleeBad
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
          (oursBlocked, oursPos) =
            partition targetBlocked $ oursOther ++ oursMeleeBad
          -- Lower overhead is better.
          overheadOurs :: ((ActorId, Actor), (Target, PathEtc))
                       -> (Int, Int, Bool)
          overheadOurs our@((aid, b), (_, (_, (goal, d)))) =
            if targetTEnemy our then
              -- TODO: take weapon, walk and fight speed, etc. into account
              ( d + if targetBlocked our then 2 else 0  -- possible delay, hacky
              , - 10 * fromIntegral (bhp b `div` (10 * oneM))
              , aid /= oldAid )
            else
              -- Keep proper formation, not too dense, not to sparse.
              let
                -- TODO: vary the parameters according to the stage of game,
                -- enough equipment or not, game mode, level map, etc.
                minSpread = 7
                maxSpread = 12 * 2
                dcaptain p =
                  chessDistVector $ bpos captainBody `vectorToFrom` p
                dsergeant p =
                  chessDistVector $ bpos sergeantBody `vectorToFrom` p
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
          sortOurs = sortBy $ comparing overheadOurs
          goodGeneric ((aid, b), (_tgt, _pathEtc)) =
            not (aid == oldAid && waitedLastTurn b)  -- not stuck
          goodTEnemy our@((_aid, b), (TEnemy{}, (_path, (goal, _d)))) =
            not (adjacent (bpos b) goal) -- not in melee range already
            && goodGeneric our
          goodTEnemy our = goodGeneric our
          oursVulnerableGood = filter goodTEnemy oursVulnerable
          oursTEnemyGood = filter goodTEnemy oursTEnemy
          oursPosGood = filter goodGeneric oursPos
          oursMeleeingGood = filter goodGeneric oursMeleeing
          oursHearingGood = filter goodTEnemy oursHearing
          oursBlockedGood = filter goodGeneric oursBlocked
          candidates = [ sortOurs oursVulnerableGood
                       , sortOurs oursTEnemyGood
                       , sortOurs oursPosGood
                       , sortOurs oursMeleeingGood
                       , sortOurs oursHearingGood
                       , sortOurs oursBlockedGood
                       ]
      case filter (not . null) candidates of
        l@(c : _) : _ -> do
          let best = takeWhile ((== overheadOurs c) . overheadOurs) l
              freq = uniformFreq "candidates for AI leader" best
          ((aid, b), _) <- rndToAction $ frequency freq
          s <- getState
          modifyClient $ updateLeader aid s
          return (aid, b)
        _ -> return (oldAid, oldBody)
