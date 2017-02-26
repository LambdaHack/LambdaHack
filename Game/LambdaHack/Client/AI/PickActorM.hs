{-# LANGUAGE TupleSections #-}
-- | Semantics of most 'ResponseAI' client commands.
module Game.LambdaHack.Client.AI.PickActorM
  ( pickActorToMove, useTactics
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import Data.Ord

import Game.LambdaHack.Client.AI.ConditionM
import Game.LambdaHack.Client.AI.PickTargetM
import Game.LambdaHack.Client.Bfs
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.ModeKind

-- Pick a new leader from among the actors on the current level.
-- Refresh the target of the new leader, even if unchanged.
pickActorToMove :: MonadClient m
                => Maybe ActorId -> ((ActorId, Actor) -> m (Maybe TgtAndPath))
                -> m (ActorId, Actor)
{-# INLINE pickActorToMove #-}
pickActorToMove maidToAvoid refreshTarget = do
  actorAspect <- getsClient sactorAspect
  mleader <- getsClient _sleader
  let oldAid = case mleader of
        Just aid -> aid
        Nothing -> assert `failure` mleader
  oldBody <- getsState $ getActorBody oldAid
  let side = bfid oldBody
      arena = blid oldBody
  fact <- getsState $ (EM.! side) . sfactionD
  -- Find our actors on the current level only.
  ours <- getsState $ filter (isNothing . btrajectory . snd)
                      . actorRegularAssocs (== side) arena
  let pickOld = do
        void $ refreshTarget (oldAid, oldBody)
        return (oldAid, oldBody)
  case ours of
    _ | -- Keep the leader: faction discourages client leader change on level,
        -- so will only be changed if waits (maidToAvoid)
        -- to avoid wasting his higher mobility.
        snd (autoDungeonLevel fact) && isNothing maidToAvoid
      -> pickOld
    [] -> assert `failure` (oldAid, oldBody)
    [_] -> pickOld  -- Keep the leader: he is alone on the level.
    _ -> do
      -- At this point we almost forget who the old leader was
      -- and treat all party actors the same, eliminating candidates
      -- until we can't distinguish them any more, at which point we prefer
      -- the old leader, if he is among the best candidates
      -- (to make the AI appear more human-like and easier to observe).
      let refresh aidBody = do
            mtgt <- refreshTarget aidBody
            return $ (\tgt -> (aidBody, tgt)) <$> mtgt
          goodGeneric (_, TgtAndPath{tapPath=NoPath}) = False
          goodGeneric ((aid, b), _) = case maidToAvoid of
            Nothing ->  -- not the old leader that was stuck last turn
                        -- because he is likely to be still stuck
              not (aid == oldAid && waitedLastTurn b)
            Just aidToAvoid ->  -- not an attempted leader stuck this turn
              aid /= aidToAvoid
      oursTgt <- filter goodGeneric . catMaybes <$> mapM refresh ours
      let actorVulnerable ((aid, body), _) = do
            let ar = case EM.lookup aid actorAspect of
                  Just aspectRecord -> aspectRecord
                  Nothing -> assert `failure` aid
                actorMaxSk = aSkills ar
            condMeleeBad <- condMeleeBadM aid
            threatDistL <- threatDistList aid
            (fleeL, _) <- fleeList aid
            let abInMaxSkill ab = EM.findWithDefault 0 ab actorMaxSk > 0
                condNoUsableWeapon = bweapon body == 0
                canMelee = abInMaxSkill AbMelee && not condNoUsableWeapon
                condCanFlee = not (null fleeL)
                condThreatAtHandVeryClose =
                  not $ null $ takeWhile ((<= 2) . fst) threatDistL
                threatAdj = takeWhile ((== 1) . fst) threatDistL
                condThreatAdj = not $ null threatAdj
                condFastThreatAdj =
                  any (\(_, (aid2, b2)) ->
                        let ar2 = actorAspect EM.! aid2
                        in bspeed b2 ar2 > bspeed body ar)
                      threatAdj
                heavilyDistressed =
                  -- Actor hit by a projectile or similarly distressed.
                  deltaSerious (bcalmDelta body)
            return $! not (canMelee && condThreatAdj)
                      && if condThreatAtHandVeryClose
                         then condCanFlee && condMeleeBad
                              && not condFastThreatAdj
                         else heavilyDistressed  -- shot at
          actorHearning (_, TgtAndPath{ tapTgt=TPoint TEnemyPos{} _ _
                                      , tapPath=NoPath }) =
            return False
          actorHearning (_, TgtAndPath{ tapTgt=TPoint TEnemyPos{} _ _
                                      , tapPath=AndPath{pathLen} })
            | pathLen <= 2 =
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
      oursMeleeing <- filterM actorMeleeing oursSafe
      oursNotMeleeing <- filterM (fmap not . actorMeleeing) oursSafe
      oursHearing <- filterM actorHearning oursNotMeleeing
      oursNotHearing <- filterM (fmap not . actorHearning) oursNotMeleeing
      oursMeleeBad <- filterM actorMeleeBad oursNotHearing
      oursNotMeleeBad <- filterM (fmap not . actorMeleeBad) oursNotHearing
      let targetTEnemy (_, TgtAndPath{tapTgt=TEnemy{}}) = True
          targetTEnemy (_, TgtAndPath{tapTgt=TPoint TEnemyPos{} _ _}) = True
          targetTEnemy _ = False
          (oursTEnemy, oursOther) = partition targetTEnemy oursNotMeleeBad
          -- These are not necessarily stuck (perhaps can go around),
          -- but their current path is blocked by friends.
          targetBlocked (_, TgtAndPath{tapPath}) =
            let next = case tapPath of
                  AndPath{pathList= q : _} -> Just q
                  _ -> Nothing
            in any ((== next) . Just . bpos . snd) ours
          (oursBlocked, oursPos) =
            partition targetBlocked $ oursOther ++ oursMeleeBad
          -- Lower overhead is better.
          overheadOurs :: ((ActorId, Actor), TgtAndPath) -> Int
          overheadOurs ((aid, _), TgtAndPath{tapPath=NoPath}) =
            1000 + if aid == oldAid then 1 else 0
          overheadOurs abt@( (aid, b)
                           , TgtAndPath{tapPath=AndPath{pathLen=d,pathGoal}} ) =
            -- Keep proper formation. Too dense and exploration takes
            -- too long; too sparse and actors fight alone.
            -- Note that right now, while we set targets separately for each
            -- hero, perhaps on opposite borders of the map,
            -- we can't help that sometimes heroes are separated.
            let maxSpread = 3 + length ours
                pDist p = minimum [ chessDist (bpos b2) p
                                  | (aid2, b2) <- ours, aid2 /= aid]
                aidDist = pDist (bpos b)
                -- Negative, if the goal gets us closer to the party.
                diffDist = pDist pathGoal - aidDist
                -- If actor already at goal or equidistant, count it as closer.
                sign = if diffDist <= 0 then -1 else 1
                formationValue =
                  sign * (abs diffDist `max` maxSpread)
                  * (aidDist `max` maxSpread) ^ (2 :: Int)
                fightValue | targetTEnemy abt =
                  - fromEnum (bhp b `div` (10 * oneM))
                           | otherwise = 0
            in formationValue `div` 3 + fightValue
               + (if targetBlocked abt then 1000 else 0)
               + (if d < 8 then d `div` 4 else 2 + d `div` 10)
               + if aid == oldAid then 1 else 0
          sortOurs = sortBy $ comparing overheadOurs
          goodTEnemy ((_aid, b), TgtAndPath{ tapTgt=TEnemy{}
                                           , tapPath=AndPath{pathGoal} }) =
            not (adjacent (bpos b) pathGoal) -- not in melee range already
          goodTEnemy _ = True
          oursVulnerableGood = filter goodTEnemy oursVulnerable
          oursTEnemyGood = filter goodTEnemy oursTEnemy
          oursPosGood = oursPos
          oursMeleeingGood = oursMeleeing
          oursHearingGood = filter goodTEnemy oursHearing
          oursBlockedGood = oursBlocked
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

useTactics :: MonadClient m
           => ((ActorId, Actor) -> m (Maybe TgtAndPath))
           -> ActorId
           -> m ()
{-# INLINE useTactics #-}
useTactics refreshTarget oldAid = do
  mleader <- getsClient _sleader
  let !_A = assert (mleader /= Just oldAid) ()
  oldBody <- getsState $ getActorBody oldAid
  let side = bfid oldBody
      arena = blid oldBody
  fact <- getsState $ (EM.! side) . sfactionD
  let explore = void $ refreshTarget (oldAid, oldBody)
      setPath mtgt = case mtgt of
        Nothing -> return False
        Just TgtAndPath{tapTgt} -> do
          tap <- createPath oldAid tapTgt
          case tap of
            TgtAndPath{tapPath=NoPath} -> return False
            _ -> do
              modifyClient $ \cli ->
                cli {stargetD = EM.insert oldAid tap (stargetD cli)}
              return True
      follow = case mleader of
        -- If no leader at all (forced @TFollow@ tactic on an actor
        -- from a leaderless faction), fall back to @TExplore@.
        Nothing -> explore
        Just leader -> do
          onLevel <- getsState $ memActor leader arena
          -- If leader not on this level, fall back to @TExplore@.
          if not onLevel then explore
          else do
            -- Copy over the leader's target, if any, or follow his bpos.
            mtgt <- getsClient $ EM.lookup leader . stargetD
            tgtPathSet <- setPath mtgt
            let enemyPath = Just TgtAndPath{ tapTgt = TEnemy leader True
                                           , tapPath = NoPath }
            unless tgtPathSet $ do
               enemyPathSet <- setPath enemyPath
               unless enemyPathSet $
                 -- If no path even to the leader himself, explore.
                 explore
  case ftactic $ gplayer fact of
    TExplore -> explore
    TFollow -> follow
    TFollowNoItems -> follow
    TMeleeAndRanged -> explore  -- needs to find ranged targets
    TMeleeAdjacent -> explore  -- probably not needed, but may change
    TBlock -> return ()  -- no point refreshing target
    TRoam -> explore  -- @TRoam@ is checked again inside @explore@
    TPatrol -> explore  -- WIP
