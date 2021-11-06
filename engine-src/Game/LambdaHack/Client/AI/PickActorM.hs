-- | Picking the AI actor to move and refreshing leader and non-leader targets.
module Game.LambdaHack.Client.AI.PickActorM
  ( pickActorToMove, setTargetFromDoctrines
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import           Data.Ratio

import           Game.LambdaHack.Client.AI.ConditionM
import           Game.LambdaHack.Client.AI.PickTargetM
import           Game.LambdaHack.Client.Bfs
import           Game.LambdaHack.Client.BfsM
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Content.FactionKind (fskillsOther)
import           Game.LambdaHack.Core.Frequency
import           Game.LambdaHack.Core.Random
import qualified Game.LambdaHack.Definition.Ability as Ability

-- | Pick a new leader from among the actors on the current level.
-- Refresh the target of the new leader, even if unchanged.
pickActorToMove :: MonadClient m
                => [(ActorId, Actor)] -> [(ActorId, Actor)] -> Maybe ActorId
                -> m ActorId
pickActorToMove foeAssocs friendAssocs maidToAvoid = do
  COps{coTileSpeedup} <- getsState scops
  actorMaxSkills <- getsState sactorMaxSkills
  mleader <- getsClient sleader
  let oldAid = fromMaybe (error $ "" `showFailure` maidToAvoid) mleader
  oldBody <- getsState $ getActorBody oldAid
  let side = bfid oldBody
      arena = blid oldBody
  lvl <- getLevel arena
  localTime <- getsState $ getLocalTime arena
  condInMelee <- condInMeleeM arena
  fact <- getsState $ (EM.! side) . sfactionD
  -- Find our actors on the current level only.
  ours <- getsState $ fidActorRegularAssocs side arena
  let pickOld = do
        void $ refreshTarget foeAssocs friendAssocs (oldAid, oldBody)
        return oldAid
      oursNotSleeping = filter (\(_, b) -> bwatch b /= WSleep) ours
      -- Faction discourages client leader change on level, because
      -- non-leader actors have the same skills as leader, so no point.
      -- Server is guaranteed to switch leader within a level occasionally,
      -- e.g., when the old leader dies, so this works fine.
      discouragedPointmanSwitchOnLevel =
        fskillsOther (gplayer fact) == Ability.zeroSkills
  case oursNotSleeping of
    _ | -- Keep the leader: client is discouraged from leader switching,
        -- so it will only be changed if pointman waits (maidToAvoid)
        -- to avoid wasting his higher mobility.
        -- This is OK for monsters even if in melee, because both having
        -- a meleeing actor a leader (and higher DPS) and rescuing actor
        -- a leader (and so faster to get in melee range) is good.
        -- And we are guaranteed that only the two classes of actors are
        -- not waiting, with some exceptions (urgent unequip, flee via starts,
        -- melee-less trying to flee, first aid, etc.).
       discouragedPointmanSwitchOnLevel && isNothing maidToAvoid -> pickOld
    [] -> pickOld
    [(aidNotSleeping, bNotSleeping)] -> do
      -- Target of asleep actors won't change unless foe adjacent,
      -- which is caught without recourse to targeting.
      void $ refreshTarget foeAssocs friendAssocs (aidNotSleeping, bNotSleeping)
      return aidNotSleeping
    _ -> do
      -- At this point we almost forget who the old leader was
      -- and treat all party actors the same, eliminating candidates
      -- until we can't distinguish them any more, at which point we slightly
      -- prefer the old leader, if he is among the best candidates
      -- (to make the AI appear more human-like and easier to observe).
      let refresh aidBody = do
            mtgt <- refreshTarget foeAssocs friendAssocs aidBody
            return (aidBody, mtgt)
      oursTgtRaw <- mapM refresh oursNotSleeping
      oldFleeD <- getsClient sfleeD
      let recentlyFled aid = maybe False
                                   (\(_, time) -> timeRecent5 localTime time)
                                   (aid `EM.lookup` oldFleeD)
          goodGeneric (_, Nothing) = Nothing
          goodGeneric (_, Just TgtAndPath{tapPath=Nothing}) = Nothing
            -- this case means melee-less heroes adjacent to foes, etc.
            -- will never flee if melee is happening; but this is rare;
            -- this also ensures even if a lone actor melees and nobody
            -- can come to rescue, he will become and remain the leader,
            -- because otherwise an explorer would need to become a leader
            -- and fighter will be 1 clip slower for the whole fight,
            -- just for a few turns of exploration in return;
            --
            -- also note that when the fighter then becomes a leader
            -- he may gain quite a lot of time via @swapTime@,
            -- and so be able to get a double blow on opponents
            -- or a safe blow and a withdraw (but only once); this is a mild
            -- exploit that encourages ambush camping (with a non-leader),
            -- but it's also a rather fun exploit and a straightforward
            -- consequence of the game mechanics, so it's OK for now
          goodGeneric ((aid, b), Just tgt) = case maidToAvoid of
            _ | aid == oldAid && actorWaits b -> Nothing
                  -- Not the old leader that was stuck last turn
                  -- because he is likely to be still stuck.
            Nothing -> Just ((aid, b), tgt)
            Just aidToAvoid ->
              if aid == aidToAvoid
              then Nothing  -- not an attempted leader stuck this turn
              else Just ((aid, b), tgt)
          oursTgt = mapMaybe goodGeneric oursTgtRaw
          -- This should be kept in sync with @actionStrategy@,
          -- because it's a part of the condition for @flee@ in @PickActionM@.
          -- Comments are in the full copy.
          actorVulnerable ((aid, body), _) = do
            let actorMaxSk = actorMaxSkills EM.! aid
            condAnyHarmfulFoeAdj <-
              getsState $ anyHarmfulFoeAdj actorMaxSkills aid
            threatDistL <- getsState $ meleeThreatDistList foeAssocs aid
            (fleeL, _) <- fleeList foeAssocs aid
            condSupport1 <- condSupport friendAssocs 1 aid
            condSolo <- condAloneM friendAssocs aid
            let condCanFlee = not (null fleeL)
                heavilyDistressed =
                  deltasSerious (bcalmDelta body)
                speed1_5 = speedScale (3%2) (gearSpeed actorMaxSk)
                condCanMelee = actorCanMelee actorMaxSkills aid body
                threatAdj = takeWhile ((== 1) . fst) threatDistL
                condManyThreatAdj = length threatAdj >= 2
                condFastThreatAdj =
                  any (\(_, (aid2, _)) ->
                        let ar2 = actorMaxSkills EM.! aid2
                        in gearSpeed ar2 > speed1_5)
                      threatAdj
                condNonStealthyThreatAdj =
                  any (\(_, (aid2, b2)) ->
                        let ar2 = actorMaxSkills EM.! aid2
                        in Ability.getSk Ability.SkShine ar2 > 0
                           || isLit (bpos b2))
                      threatAdj
                isLit pos = Tile.isLit coTileSpeedup (lvl `at` pos)
                fleeingMakesSense =
                  not condCanMelee
                  || (Ability.getSk Ability.SkSight actorMaxSk > 2
                      || Ability.getSk Ability.SkNocto actorMaxSk > 2)
                     && (Ability.getSk Ability.SkShine actorMaxSk > 2
                         || condNonStealthyThreatAdj || null threatAdj)
            return $!
              not condFastThreatAdj
              && fleeingMakesSense
              && if | condAnyHarmfulFoeAdj ->
                      not condCanMelee
                      || condManyThreatAdj && not condSupport1 && not condSolo
                    | condInMelee -> False
                    | heavilyDistressed -> True
                        -- Different from @PickActionM@:
                        -- If under fire, do something quickly, always,
                        -- because the actor clearly vulnerable,
                        -- but don't make a leader only because threats close.
                    | otherwise -> False
              && condCanFlee
          actorFled ((aid, _), _) = recentlyFled aid
          actorHearning (_, TgtAndPath{ tapTgt=TPoint TEnemyPos{} _ _
                                      , tapPath=Nothing }) =
            return False
          actorHearning (_, TgtAndPath{ tapTgt=TPoint TEnemyPos{} _ _
                                      , tapPath=Just AndPath{pathLen} })
            | pathLen <= 2 =
            return False  -- noise probably due to fleeing target
          actorHearning ((_aid, b), _) = do
            let closeFoes = filter ((<= 3) . chessDist (bpos b) . bpos . snd)
                                   foeAssocs
                actorHears = deltasHears (bcalmDelta b)
            return $! actorHears  -- e.g., actor hears an enemy
                      && null closeFoes  -- the enemy not visible; a trap!
          -- AI has to be prudent and not lightly waste leader for meleeing.
          actorMeleeing ((aid, _), _) =
            getsState $ anyHarmfulFoeAdj actorMaxSkills aid
      (oursVulnerable, oursSafe) <- partitionM actorVulnerable oursTgt
      let (oursFled, oursNotFled) = partition actorFled oursSafe
      (oursMeleeingRaw, oursNotMeleeingRaw) <-
         partitionM actorMeleeing oursNotFled
      let actorMeleeingCanDisplace ( (aid, sb)
                                   , TgtAndPath{tapTgt=TEnemy target} ) = do
            tb <- getsState $ getActorBody target
            let actorMaxSk = actorMaxSkills EM.! target
            dEnemy <- getsState $ dispEnemy aid target actorMaxSk
            -- Some usual conditions ignored, because transient or rare.
            return $! checkAdjacent sb tb && dEnemy
          actorMeleeingCanDisplace _ = return False
      (oursMeleeingCanDisplace, oursMeleeing) <-
         partitionM actorMeleeingCanDisplace oursMeleeingRaw
      let adjStash
            ( (_, b)
            , TgtAndPath{tapTgt=TPoint TStash{} lid pos} ) =
                lid == arena
                && adjacent pos (bpos b)
                && isNothing (posToBigLvl pos lvl)
          adjStash _ = False
          (oursAdjStash, oursNotMeleeing) =
            partition adjStash oursNotMeleeingRaw
      (oursHearing, oursNotHearing) <- partitionM actorHearning oursNotMeleeing
      let actorRanged ((aid, body), _) =
            not $ actorCanMelee actorMaxSkills aid body
          targetTEnemy (_, TgtAndPath{tapTgt=TEnemy _}) = True
          targetTEnemy (_, TgtAndPath{tapTgt=TPoint TEnemyPos{} lid _}) =
            lid == arena
          targetTEnemy ((_, b), TgtAndPath{tapTgt=TPoint TStash{} lid pos}) =
            lid == arena && pos /= bpos b
              -- stashes as crucial as enemies. except when guarding them
          targetTEnemy _ = False
          actorNoSupport ((aid, _), _) = do
            threatDistL <- getsState $ meleeThreatDistList foeAssocs aid
            condSupport2 <- condSupport friendAssocs 2 aid
            let condThreat n = not $ null $ takeWhile ((<= n) . fst) threatDistL
            -- If foes far, friends may still come, so we let him move.
            -- The net effect is that lone heroes close to foes freeze
            -- until support comes.
            return $! condThreat 5 && not condSupport2
          (oursRanged, oursNotRanged) = partition actorRanged oursNotHearing
          (oursTEnemyAll, oursOther) = partition targetTEnemy oursNotRanged
          notSwapReady ((_, b), TgtAndPath{tapTgt=TPoint TStash{} lid pos}) _ =
            lid == arena && pos == bpos b
              -- not ready to follow goal if already guarding the stash
          notSwapReady abt@((_, b), _)
                       (ab2, Just t2@TgtAndPath{tapPath=
                                       Just AndPath{pathList=q : _}}) =
            let source = bpos b
                tenemy = targetTEnemy abt
                tenemy2 = targetTEnemy (ab2, t2)
            -- Copied from 'displaceTowards':
            in not (q == source  -- friend wants to swap
                    || tenemy && not tenemy2)
          notSwapReady _ _ = True
          -- These are not necessarily stuck (perhaps can go around),
          -- but their current path is blocked by friends.
          -- As soon as friends move, path is recalcuated and they may
          -- become unstuck.
          targetBlocked abt@((aid, _), TgtAndPath{tapPath}) = case tapPath of
            Just AndPath{pathList= q : _} ->
              any (\abt2@((aid2, body2), _) ->
                     aid2 /= aid  -- in case pushed on goal
                     && bpos body2 == q
                     && notSwapReady abt abt2)
                  oursTgtRaw
            _ -> False
          (oursTEnemyBlocked, oursTEnemy) =
            partition targetBlocked oursTEnemyAll
      (oursNoSupportRaw, oursSupportRaw) <-
        if length oursTEnemy <= 2
        then return ([], oursTEnemy)
        else partitionM actorNoSupport oursTEnemy
      let (oursNoSupport, oursSupport) =
            if length oursSupportRaw <= 1  -- make sure picks random enough
            then ([], oursTEnemy)
            else (oursNoSupportRaw, oursSupportRaw)
          (oursBlocked, oursPos) =
            partition targetBlocked $ oursRanged ++ oursOther
          guarding ((_, b), Just TgtAndPath{tapTgt=TPoint TStash{} lid pos}) =
            lid == arena && pos == bpos b
          guarding _ = False
          -- Don't try to include a stash guard in formation, even if attacking
          -- or being attacked. Attackers would be targetted anyway.
          oursNotSleepingNorGuarding = filter (not . guarding) oursTgtRaw
          -- Lower overhead is better.
          overheadOurs :: ((ActorId, Actor), TgtAndPath) -> Int
          overheadOurs (_, TgtAndPath{tapPath=Nothing}) = 100
          overheadOurs ((_, b), TgtAndPath{tapTgt=TPoint TStash{} lid pos})
            | lid == arena && pos == bpos b = 200  -- guarding, poor choice
          overheadOurs abt@( (aid, b)
                           , TgtAndPath{tapPath=Just AndPath{pathLen=d, ..}} ) =
            -- Keep proper formation. Too dense and exploration takes
            -- too long; too sparse and actors fight alone.
            -- Note that right now, while we set targets separately for each
            -- hero, perhaps on opposite borders of the map,
            -- we can't help that sometimes heroes are separated.
            let maxSpread = 3 + length oursNotSleepingNorGuarding
                lDist p = [ chessDist (bpos b2) p
                          | ((aid2, b2), _) <- oursNotSleepingNorGuarding
                          , aid2 /= aid ]
                pDist p = let ld = lDist p
                          in if null ld then 0 else minimum ld
                aidDist = pDist (bpos b)
                -- Negative, if the goal gets us closer to the party.
                diffDist = pDist pathGoal - aidDist
                -- If actor already at goal or equidistant, count it as closer.
                sign = if diffDist <= 0 then -1 else 1
                formationValue =
                  sign * (abs diffDist `max` maxSpread)
                  * (aidDist `max` maxSpread) ^ (2 :: Int)
                targetsEnemy = targetTEnemy abt
                fightValue = if targetsEnemy
                             then - fromEnum (bhp b `div` (10 * oneM))
                             else 0
                isLit pos = Tile.isLit coTileSpeedup (lvl `at` pos)
                  -- solid tiles ignored, because not obvious if dark
                  -- after removed
                actorMaxSk = actorMaxSkills EM.! aid
                actorShines = Ability.getSk Ability.SkShine actorMaxSk > 0
                stepsIntoLight =
                  not actorShines
                  && not (isLit $ bpos b)
                  && case pathList of
                    [] -> False
                    q : _ -> isLit q
                      -- shortest path is through light even though may
                      -- sidestep through dark in @chase@ or @flee@
            in formationValue `div` 3
               + fightValue
               + (case d of
                    0 -> -400  -- do your thing ASAP and retarget
                    1 | not targetsEnemy -> -200
                      -- prevent others from trying to occupy the tile;
                      -- TStash that obscures a foe correctly handled here
                    _ -> if d < 8 then d `div` 4 else 2 + d `div` 10)
               + (if aid == oldAid then 0 else 10)
               + (if stepsIntoLight then 30 else 0)
          -- Overheads above @maxBoundInt32@ are unlikely (and unsuppored in JS)
          -- and also capping the value does not distort the choice too much.
          positiveOverhead abt =
            min maxBoundInt32 $ max 1 $ 200 - overheadOurs abt
          candidates = [ oursAdjStash
                       , oursVulnerable
                       , oursSupport
                       , oursNoSupport
                       , oursPos
                       , oursFled  -- if just fled, but not vulnerable,
                                   -- keep him passive and safe, out of action
                       , oursMeleeingCanDisplace
                           -- prefer melee actors displacing than blocked
                           -- actors trying to walk around them
                       , oursTEnemyBlocked
                           -- prefer blocked actors trying to walk around
                           -- even if that causes overhead for the meleeing
                       , oursMeleeing
                       , oursHearing
                       , oursBlocked
                       ]
      case filter (not . null) candidates of
        l : _ -> do
          let freq = toFreq "candidates for AI leader"
                     $ map (positiveOverhead &&& id) l
          ((aid, b), _) <- rndToAction $ frequency freq
          s <- getState
          modifyClient $ updateLeader aid s
          -- When you become a leader, stop following old leader, but follow
          -- his target, if still valid, to avoid distraction.
          when (gdoctrine fact `elem` [Ability.TFollow, Ability.TFollowNoItems]
                && not condInMelee) $
            void $ refreshTarget foeAssocs friendAssocs (aid, b)
          return aid
        _ -> return oldAid

-- | Inspect the doctrines of the actor and set his target according to it.
setTargetFromDoctrines :: MonadClient m
                        => [(ActorId, Actor)] -> [(ActorId, Actor)] -> ActorId
                        -> m ()
setTargetFromDoctrines foeAssocs friendAssocs oldAid = do
  mleader <- getsClient sleader
  let !_A = assert (mleader /= Just oldAid) ()
  oldBody <- getsState $ getActorBody oldAid
  moldTgt <- getsClient $ EM.lookup oldAid . stargetD
  let side = bfid oldBody
      arena = blid oldBody
  fact <- getsState $ (EM.! side) . sfactionD
  let explore = void $ refreshTarget foeAssocs friendAssocs (oldAid, oldBody)
      setPath mtgt = case (mtgt, moldTgt) of
        (Nothing, _) -> return False
        ( Just TgtAndPath{tapTgt=leaderTapTgt},
          Just TgtAndPath{tapTgt=oldTapTgt,tapPath=Just oldTapPath} )
          | leaderTapTgt == oldTapTgt  -- targets agree
            && bpos oldBody == pathSource oldTapPath -> do  -- nominal path
            void $ refreshTarget foeAssocs friendAssocs (oldAid, oldBody)
            return True  -- already on target
        (Just TgtAndPath{tapTgt=leaderTapTgt}, _) -> do
            tap <- createPath oldAid leaderTapTgt
            case tap of
              TgtAndPath{tapPath=Nothing} -> return False
              _ -> do
                modifyClient $ \cli ->
                  cli {stargetD = EM.insert oldAid tap (stargetD cli)}
                return True
      follow = case mleader of
        -- If no leader at all (forced @TFollow@ doctrine on an actor
        -- from a leaderless faction), fall back to @TExplore@.
        Nothing -> explore
        _ | bwatch oldBody == WSleep ->
          -- We could check skills, but it would be more complex.
          explore
        Just leader -> do
          onLevel <- getsState $ memActor leader arena
          condInMelee <- condInMeleeM arena
          -- If leader not on this level or if we are meleeing,
          -- and so following is not important, fall back to @TExplore@.
          if not onLevel || condInMelee then explore
          else do
            -- Copy over the leader's target, if any, or follow his position.
            mtgt <- getsClient $ EM.lookup leader . stargetD
            tgtPathSet <- setPath mtgt
            unless tgtPathSet $ do
              let nonEnemyPath = Just TgtAndPath { tapTgt = TNonEnemy leader
                                                 , tapPath = Nothing }
              nonEnemyPathSet <- setPath nonEnemyPath
              unless nonEnemyPathSet
                -- If no path even to the leader himself, explore.
                explore
  case gdoctrine fact of
    Ability.TExplore -> explore
    Ability.TFollow -> follow
    Ability.TFollowNoItems -> follow
    Ability.TMeleeAndRanged -> explore  -- needs to find ranged targets
    Ability.TMeleeAdjacent -> explore  -- probably not needed, but may change
    Ability.TBlock -> return ()  -- no point refreshing target
    Ability.TRoam -> explore  -- @TRoam@ is checked again inside @explore@
    Ability.TPatrol -> explore  -- WIP
