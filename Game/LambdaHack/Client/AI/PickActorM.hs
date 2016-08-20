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
import Game.LambdaHack.Client.CommonM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.Item
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

-- Pick a new leader from among the actors on the current level.
-- Refresh the target of the new leader, even if unchanged.
pickActorToMove :: MonadClient m
                => ((ActorId, Actor) -> m TgtAndPath)
                -> m (ActorId, Actor)
pickActorToMove refreshTarget = do
  Kind.COps{cotile} <- getsState scops
  mleader <- getsClient _sleader
  let oldAid = case mleader of
        Just aid -> aid
        Nothing -> assert `failure` mleader
  oldBody <- getsState $ getActorBody oldAid
  let side = bfid oldBody
      arena = blid oldBody
  fact <- getsState $ (EM.! side) . sfactionD
  lvl <- getLevel arena
  let leaderStuck = waitedLastTurn oldBody
      t = lvl `at` bpos oldBody
  -- Find our actors on the current level only.
  ours <- getsState $ actorRegularAssocs (== side) arena
  let pickOld = do
        void $ refreshTarget (oldAid, oldBody)
        return (oldAid, oldBody)
  case ours of
    _ | -- Keep the leader: the faction forbids client leader change on level.
        snd (autoDungeonLevel fact)
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
            tgt <- refreshTarget aidBody
            return (aidBody, tgt)
          goodGeneric (_, TgtAndPath{tapPath=NoPath}) = False
          goodGeneric ((aid, b), _) =
            not (aid == oldAid && waitedLastTurn b)  -- not stuck
      oursTgt <- filter goodGeneric <$> mapM refresh ours
      let actorVulnerable ((aid, body), _) = do
            actorAspect <- getsClient sactorAspect
            let ar = case EM.lookup aid actorAspect of
                  Just aspectRecord -> aspectRecord
                  Nothing -> assert `failure` aid
                actorMaxSk = aAbility ar
            activeItems <- activeItemsClient aid
            condMeleeBad <- condMeleeBadM aid
            threatDistL <- threatDistList aid
            (fleeL, _) <- fleeList aid
            activeI <- activeItemsFunClient
            let abInMaxSkill ab = EM.findWithDefault 0 ab actorMaxSk > 0
                condNoUsableWeapon = all (not . isMelee) activeItems
                canMelee = abInMaxSkill AbMelee && not condNoUsableWeapon
                condCanFlee = not (null fleeL)
                condThreatAtHandVeryClose =
                  not $ null $ takeWhile ((<= 2) . fst) threatDistL
                threatAdj = takeWhile ((== 1) . fst) threatDistL
                condThreatAdj = not $ null threatAdj
                condFastThreatAdj =
                  any (\(_, (aid2, b2)) ->
                        let activeItems2 = activeI aid2
                        in bspeedFromItems b2 activeItems2 > bspeed body ar)
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
          actorHearning (_, TgtAndPath{tapTgt=TEnemyPos{},tapPath=NoPath}) =
            return False
          actorHearning (_, TgtAndPath{ tapTgt=TEnemyPos{}
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
        -- TODO: partitionM
      oursMeleeing <- filterM actorMeleeing oursSafe
      oursNotMeleeing <- filterM (fmap not . actorMeleeing) oursSafe
      oursHearing <- filterM actorHearning oursNotMeleeing
      oursNotHearing <- filterM (fmap not . actorHearning) oursNotMeleeing
      oursMeleeBad <- filterM actorMeleeBad oursNotHearing
      oursNotMeleeBad <- filterM (fmap not . actorMeleeBad) oursNotHearing
      let targetTEnemy (_, TgtAndPath{tapTgt=TEnemy{}}) = True
          targetTEnemy (_, TgtAndPath{tapTgt=TEnemyPos{}}) = True
          targetTEnemy _ = False
          (oursTEnemy, oursOther) = partition targetTEnemy oursNotMeleeBad
          -- These are not necessarily stuck (perhaps can go around),
          -- but their current path is blocked by friends.
          targetBlocked (_, TgtAndPath{tapPath}) =
            let next = case tapPath of
                  NoPath -> Nothing
                  AndPath{pathList=[]} -> Nothing
                  AndPath{pathList=q : _} -> Just q
            in any ((== next) . Just . bpos . snd) ours
-- TODO: stuck actors are picked while others close could approach an enemy;
-- we should detect stuck actors (or one-sided stuck)
-- so far we only detect blocked and only in Other mode
--             && not (aid == oldAid && waitedLastTurn b time)  -- not stuck
-- this only prevents staying stuck
          (oursBlocked, oursPos) =
            partition targetBlocked $ oursOther ++ oursMeleeBad
          -- Lower overhead is better.
          overheadOurs :: ((ActorId, Actor), TgtAndPath)
                       -> (Int, Int, Bool)
          overheadOurs (_, TgtAndPath{tapPath=NoPath}) = (1000, 0, False)
          overheadOurs our@( (aid, b)
                           , TgtAndPath{tapPath=AndPath{pathLen=d,pathGoal}} ) =
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
                diffDist = sumDist - pDist pathGoal
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
           => ((ActorId, Actor) -> m TgtAndPath)
           -> ActorId
           -> m ()
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
    TPatrol -> explore  -- TODO
