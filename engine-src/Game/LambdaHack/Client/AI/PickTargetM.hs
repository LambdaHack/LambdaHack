{-# LANGUAGE TupleSections #-}
-- | Let AI pick the best target for an actor.
module Game.LambdaHack.Client.AI.PickTargetM
  ( refreshTarget
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , computeTarget
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES

import           Game.LambdaHack.Client.AI.ConditionM
import           Game.LambdaHack.Client.Bfs
import           Game.LambdaHack.Client.BfsM
import           Game.LambdaHack.Client.CommonM
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.CaveKind as CK
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Content.RuleKind
import           Game.LambdaHack.Content.TileKind (isUknownSpace)
import           Game.LambdaHack.Core.Frequency
import           Game.LambdaHack.Core.Random
import qualified Game.LambdaHack.Definition.Ability as Ability

-- | Verify and possibly change the target of an actor. This function both
-- updates the target in the client state and returns the new target explicitly.
refreshTarget :: MonadClient m
              => [(ActorId, Actor)] -> (ActorId, Actor) -> m (Maybe TgtAndPath)
refreshTarget friendAssocs (aid, body) = do
  side <- getsClient sside
  let !_A = assert (bfid body == side
                    `blame` "AI tries to move an enemy actor"
                    `swith` (aid, body, side)) ()
  let !_A = assert (not (bproj body)
                    `blame` "AI gets to manually move its projectiles"
                    `swith` (aid, body, side)) ()
  mtarget <- computeTarget friendAssocs aid
  case mtarget of
    Nothing -> do
      -- Melee in progress and the actor can't contribute
      -- and would slow down others if he acted.
      -- Or he's just asleep.
      modifyClient $ \cli -> cli {stargetD = EM.delete aid (stargetD cli)}
      return Nothing
    Just tgtMPath -> do
      -- _debugoldTgt <- getsClient $ EM.lookup aid . stargetD
      modifyClient $ \cli ->
        cli {stargetD = EM.insert aid tgtMPath (stargetD cli)}
      return mtarget
      -- let _debug = T.unpack
      --       $ "\nHandleAI symbol:"    <+> tshow (bsymbol body)
      --       <> ", aid:"               <+> tshow aid
      --       <> ", pos:"               <+> tshow (bpos body)
      --       <> "\nHandleAI oldTgt:"   <+> tshow _debugoldTgt
      --       <> "\nHandleAI strTgt:"   <+> tshow stratTarget
      --       <> "\nHandleAI target:"   <+> tshow tgtMPath
      -- trace _debug $ return $ Just tgtMPath

computeTarget :: forall m. MonadClient m
              => [(ActorId, Actor)] -> ActorId -> m (Maybe TgtAndPath)
computeTarget friendAssocs aid = do
  cops@COps{cocave, corule=RuleContent{rXmax, rYmax, rnearby}, coTileSpeedup}
    <- getsState scops
  b <- getsState $ getActorBody aid
  mleader <- getsClient sleader
  salter <- getsClient salter
  -- We assume the actor eventually becomes a leader (or has the same
  -- set of skills as the leader, anyway) and set his target accordingly.
  actorMaxSkills <- getsState sactorMaxSkills
  condInMelee <- condInMeleeM $ blid b
  let lalter = salter EM.! blid b
      actorMaxSk = actorMaxSkills EM.! aid
      alterSkill = Ability.getSk Ability.SkAlter actorMaxSk
  lvl <- getLevel $ blid b
  localTime <- getsState $ getLocalTime (blid b)
  let stepAccesible :: [Point] -> Bool
      stepAccesible (q : _) =
        -- Effectively, only @alterMinWalk@ is checked, because real altering
        -- is not done via target path, but action after end of path.
        alterSkill >= fromEnum (lalter PointArray.! q)
      stepAccesible [] = False
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  oldTgtUpdatedPath <- case mtgtMPath of
    Just TgtAndPath{tapTgt,tapPath=Nothing} ->
      -- This case is especially for TEnemyPos that would be lost otherwise.
      -- This is also triggered by @UpdLeadFaction@.
      Just <$> createPath aid tapTgt
    Just tap@TgtAndPath{tapTgt,tapPath=Just AndPath{..}} -> do
      mvalidPos <- getsState $ aidTgtToPos aid (blid b) (Just tapTgt)
      return $!
        if | isNothing mvalidPos -> Nothing  -- wrong level
           | bpos b == pathGoal ->
               mtgtMPath  -- goal reached; stay there picking up items
                          -- or hiding in ambush or in panic
           | pathSource == bpos b ->  -- no move
               -- If next step not accessible, something serious happened,
               -- so reconsider the target, not only path.
               if stepAccesible pathList then mtgtMPath else Nothing
           | otherwise -> case break (== bpos b) pathList of
               (crossed, _ : rest) ->  -- step or many steps along path
                 if null rest
                 then Nothing  -- path to the goal was partial, so tiles
                               -- discovered or altered, so reconsider target
                 else let newPath =
                            AndPath{ pathSource = bpos b
                                   , pathList = rest
                                   , pathGoal
                                   , pathLen = pathLen - length crossed - 1 }
                      in if stepAccesible rest
                         then Just tap{tapPath=Just newPath}
                         else Nothing
               (_, []) -> Nothing  -- veered off the path, e.g., due to push
                                   -- by enemy or congestion, so serious,
                                   -- so reconsider target, not only path
    Nothing -> return Nothing  -- no target assigned yet
  allFoes <- getsState $ foeRegularAssocs (bfid b) (blid b)
  factionD <- getsState sfactionD
  seps <- getsClient seps
  let fact = factionD EM.! bfid b
      slackDoctrine =
        fdoctrine (gplayer fact)
          `elem` [ Ability.TMeleeAndRanged, Ability.TMeleeAdjacent
                 , Ability.TBlock, Ability.TRoam, Ability.TPatrol ]
      canMove = Ability.getSk Ability.SkMove actorMaxSk > 0
      canReach = canMove
                 || Ability.getSk Ability.SkDisplace actorMaxSk > 0
                 -- Needed for now, because AI targets and shoots enemies
                 -- based on the path to them, not LOS to them:
                 || Ability.getSk Ability.SkProject actorMaxSk > 0
      canAlter = Ability.getSk Ability.SkAlter actorMaxSk
                 >= if slackDoctrine then 2 else 4
      canMoveItem = Ability.getSk Ability.SkMoveItem actorMaxSk > 0
      calmE = calmEnough b actorMaxSk
      heavilyDistressed =  -- actor hit by a proj or similarly distressed
        deltasSerious (bcalmDelta b)
  -- Speedup compared to @currentSkillsClient@.
  actorMinSk <- getsState $ actorCurrentSkills Nothing aid
  condCanProject <-
    condCanProjectM (Ability.getSk Ability.SkProject actorMaxSk) aid
  fleeD <- getsClient sfleeD
  let condCanMelee = actorCanMelee actorMaxSkills aid b
      condHpTooLow = hpTooLow b actorMaxSk
      mfled = aid `EM.lookup` fleeD
      recentlyFled =
        maybe False (\(_, time) -> timeRecent5 localTime time) mfled
      recentlyFled20 =
        maybe False (\(_, time) -> timeRecent5 localTime time) mfled
      actorTurn = (ticksPerMeter $ gearSpeed actorMaxSk)
  let canEscape = fcanEscape (gplayer fact)
      canSmell = Ability.getSk Ability.SkSmell actorMaxSk > 0
      meleeNearby | canEscape = rnearby `div` 2
                  | otherwise = rnearby
      rangedNearby = 2 * meleeNearby
      -- We do target foes that already attack ours or have benign weapons.
      -- We assume benign weapons run out if they are the sole cause
      -- of targeting, to avoid stalemate.
      worthTargeting aidE body =
        let attacksFriends =
              any (adjacent (bpos body) . bpos . snd) friendAssocs
              && actorCanMeleeToHarm actorMaxSkills aidE body
        in attacksFriends
           || bweapBenign body > 0
           || actorWorthChasing actorMaxSkills aidE body
      targetableMelee body =
        let attacksFriends =
              any (adjacent (bpos body) . bpos . snd) friendAssocs
            -- 3 is
            -- 1 from condSupport1
            -- + 2 from foe being 2 away from friend before he closed in
            -- + 1 for as a margin for ambush, given than actors exploring
            -- can't physically keep adjacent all the time
            n | Ability.getSk Ability.SkAggression actorMaxSk >= 2
              = rangedNearby
                  -- boss never waits
              | condInMelee = if attacksFriends then 8 else 4
                  -- attack even if foe not in melee, to create another
                  -- skirmish and perhaps overwhelm them in this one;
                  -- also, this looks more natural; also sometimes the foe
                  -- would attack our friend in a couple of turns anyway,
                  -- but we may be too far from him at that time
              | otherwise = meleeNearby
        in canMove
           && condCanMelee
           && chessDist (bpos body) (bpos b) <= n
      -- Even when missiles run out, the non-moving foe will still be
      -- targeted, which is fine, since he is weakened by ranged, so should be
      -- meleed ASAP, even if without friends.
      targetableRanged body =
        (not condInMelee || Ability.getSk Ability.SkAggression actorMaxSk >= 2)
          -- boss fires at will
        && chessDist (bpos body) (bpos b) < rangedNearby
        && condCanProject
        && (canMove || targetableLine body)
              -- prevent the exploit of using cover against non-moving shooters
              -- causing them to ignore any other distant foes
      targetableLine body = isJust $ makeLine False b (bpos body) seps cops lvl
      targetableEnemy (aidE, body) = worthTargeting aidE body
                                     && (adjacent (bpos body) (bpos b)
                                            -- target regardless of anything,
                                            -- e.g., to flee if helpless
                                         || targetableMelee body
                                         || targetableRanged body)
      targetableFoes = filter targetableEnemy allFoes
      canMeleeEnemy (aidE, body) = actorCanMeleeToHarm actorMaxSkills aidE body
      nearbyFoes = if recentlyFled && not condInMelee
                   then filter (not . canMeleeEnemy) targetableFoes
                   else targetableFoes
  discoBenefit <- getsClient sdiscoBenefit
  getKind <- getsState $ flip getIidKind
  getArItem <- getsState $ flip aspectRecordFromIid
  cstashes <- if canMove
                 && (calmE || null nearbyFoes) -- danger or risk of defecting
                 && not heavilyDistressed
              then closestStashes aid
              else return []
  let desirableIid (iid, (k, _)) =
        let Benefit{benPickup} = discoBenefit EM.! iid
        in desirableItem cops canEscape benPickup
                         (getArItem iid) (getKind iid) k
      desirableBagFloor bag = any desirableIid $ EM.assocs bag
      desirableFloor (_, (_, bag)) = desirableBagFloor bag
      focused = gearSpeed actorMaxSk < speedWalk || condHpTooLow
      couldMoveLastTurn =  -- approximated; could have changed
        let actorSk = if mleader == Just aid then actorMaxSk else actorMinSk
        in Ability.getSk Ability.SkMove actorSk > 0
      isStuck = actorWaits b && couldMoveLastTurn
      setPath :: Target -> m (Maybe TgtAndPath)
      setPath tgt = do
        let take6 tap@TgtAndPath{tapTgt=TEnemy{}} = tap
              -- @TEnemy@ needed for projecting, even by roaming actors;
              -- however, CStash not as binding, so excursions possible
            take6 TgtAndPath{tapPath=Just AndPath{..}} =
              -- Path followed for up to 6 moves regardless if the target valid
              -- and then target forgot and a new one picked.
              let path6 = take 6 pathList
                  vOld = if bpos b /= pathGoal
                         then towards (bpos b) pathGoal
                         else Vector 0 0
                  tapTgt = TVector vOld
                  tapPath = Just AndPath{pathList=path6, ..}
              in TgtAndPath{..}
            take6 tap = tap
        tgtpath <- createPath aid tgt
        return $ Just $ if slackDoctrine then take6 tgtpath else tgtpath
      pickNewTarget = pickNewTargetIgnore Nothing
      pickNewTargetIgnore :: Maybe ActorId -> m (Maybe TgtAndPath)
      pickNewTargetIgnore maidToIgnore =
        case cstashes of
          (_, (fid2, pos2)) : _ -> setPath $ TPoint (TStash fid2) (blid b) pos2
          [] -> do
            let f aidToIgnore = filter ((/= aidToIgnore) . fst) nearbyFoes
                notIgnoredFoes = maybe nearbyFoes f maidToIgnore
            cfoes <- closestFoes notIgnoredFoes aid
            case cfoes of
             (_, (aid2, _)) : _ -> setPath $ TEnemy aid2
             [] | condInMelee -> return Nothing  -- don't slow down fighters
               -- this looks a bit strange, because teammates stop
               -- in their tracks all around the map (unless very close
               -- to the combatant), but the intuition is, not being able
               -- to help immediately, and not being too friendly
               -- to each other, they just wait and see and also shout
               -- to the teammate to flee and lure foes into ambush
             [] -> do
              mhideout <- if recentlyFled20
                          then closestHideout aid
                          else return Nothing
              case (mhideout, mfled) of
               (Just (p, dist), Just (_, time))
                 | timeDeltaToFrom localTime time
                   <= timeDeltaScale actorTurn (20 - dist) ->
                -- Only target if can reach the hideout 20 turns from fleeing
                -- start, given the actor speed as a leader.
                setPath $ TPoint THideout (blid b) p
               _ -> do
                citemsRaw <- if canMoveItem && canMove
                             then closestItems aid
                             else return []
                let citems = toFreq "closestItems"
                             $ filter desirableFloor citemsRaw
                if nullFreq citems then do
                  -- Tracking enemies is more important than exploring,
                  -- but smell is unreliable and may lead to allies,
                  -- not foes, so avoid it. However, let's keep smell
                  -- more imporant than getting to stairs, to let smelling
                  -- monsters follow cues even on explored levels.
                  smpos <- if canSmell
                           then closestSmell aid
                           else return []
                  case smpos of
                    [] -> do
                      ctriggersRaw <- closestTriggers ViaAnything aid
                      let ctriggers = toFreq "ctriggers" ctriggersRaw
                      if nullFreq ctriggers then do
                        let oldpos = fromMaybe (bpos b) (boldpos b)
                            vOld = bpos b `vectorToFrom` oldpos
                            pNew = shiftBounded rXmax rYmax (bpos b) vOld
                        if slackDoctrine && not isStuck && calmE && not focused
                           && isUnit vOld && bpos b /= pNew
                                -- both are needed, e.g., when just teleported
                                -- or when the shift bounded by level borders
                        then do
                          let vFreq = toFreq "vFreq"
                                      $ (20, vOld) : map (1,) moves
                          v <- rndToAction $ frequency vFreq
                          -- Once the most pressing targets exhaused,
                          -- wander around for 7 steps and only then,
                          -- or if blocked or derailed, consider again
                          -- the old and new targets.
                          --
                          -- Together with depending on heroes or aliens
                          -- to keep arean, sleepiness, inability to displace
                          -- and chasing random smells, this makes it very hard
                          -- to fully explore and change levels for, e.g.,
                          -- animals. Heroes idling on the level help a lot.
                          let pathSource = bpos b
                              traSlack7 = trajectoryToPathBounded
                                            rXmax rYmax pathSource
                                            (replicate 7 v)  -- > 6 from take6
                              pathList = map head $ group traSlack7
                              pathGoal = last pathList
                              pathLen = length pathList
                              tapTgt = TVector v
                              tapPath = Just AndPath{..}
                          return $ Just TgtAndPath {..}
                        else do
                          upos <- if canMove
                                  then closestUnknown aid
                                  else return Nothing
                          case upos of
                            Nothing -> do
                              -- If can't move (and so no BFS data),
                              -- no info gained. Or if can't open doors.
                              -- If stuck among ice pillars, we can't help it.
                              when (canMove && canAlter) $
                                modifyClient $ \cli -> cli {sexplored =
                                  ES.insert (blid b) (sexplored cli)}
                              ctriggersRaw2 <- closestTriggers ViaExit aid
                              let ctriggers2 = toFreq "ctriggers2" ctriggersRaw2
                              if nullFreq ctriggers2 then do
                                let toKill = actorWorthKilling actorMaxSkills
                                    worthyFoes = filter (uncurry toKill) allFoes
                                afoes <- closestFoes worthyFoes aid
                                case afoes of
                                  (_, (aid2, _)) : _ ->
                                    -- All stones turned, time to win or die.
                                    setPath $ TEnemy aid2
                                  [] -> do
                                    furthest <- furthestKnown aid
                                    setPath $ TPoint TKnown (blid b) furthest
                              else do
                                (p, (p0, bag)) <-
                                  rndToAction $ frequency ctriggers2
                                setPath $ TPoint (TEmbed bag p0) (blid b) p
                            Just p -> setPath $ TPoint TUnknown (blid b) p
                      else do
                        (p, (p0, bag)) <- rndToAction $ frequency ctriggers
                        setPath $ TPoint (TEmbed bag p0) (blid b) p
                    (_, (p, _)) : _ -> setPath $ TPoint TSmell (blid b) p
                else do
                  (p, bag) <- rndToAction $ frequency citems
                  setPath $ TPoint (TItem bag) (blid b) p
      tellOthersNothingHere = do
        let f TgtAndPath{tapTgt} = case tapTgt of
              TPoint _ lid p -> p /= bpos b || lid /= blid b
              _ -> True
        modifyClient $ \cli -> cli {stargetD = EM.filter f (stargetD cli)}
        pickNewTarget
      updateTgt :: TgtAndPath -> m (Maybe TgtAndPath)
      updateTgt TgtAndPath{tapPath=Nothing} = pickNewTarget
      updateTgt tap@TgtAndPath{tapPath=Just AndPath{..},tapTgt} = case tapTgt of
        TEnemy a -> do
          body <- getsState $ getActorBody a
          if | (condInMelee  -- fight close foes or nobody at all
                || bweapon body <= 0  -- not dangerous
                || not focused && not (null nearbyFoes))  -- prefers closer foes
               && a `notElem` map fst nearbyFoes  -- old one not close enough
               || blid body /= blid b  -- wrong level
               || actorDying body  -- foe already dying
               || not (worthTargeting a body)
               || recentlyFled ->
                    -- forget enemy positions to prevent attacking them
                    -- again soon after flight
               pickNewTarget
             | otherwise -> do
               -- If there are no unwalkable tiles on the path to enemy,
               -- he gets target @TEnemy@ and then, even if such tiles emerge,
               -- the target updated by his moves remains @TEnemy@.
               -- Conversely, he is stuck with @TBlock@ if initial target had
               -- unwalkable tiles, for as long as they remain. Harmless quirk.
               mpath <- getCachePath aid $ bpos body
               case mpath of
                 Nothing -> pickNewTargetIgnore (Just a)
                   -- enemy became unreachable
                 Just AndPath{pathList=[]} -> pickNewTarget
                   -- he is his own enemy
                 Just AndPath{pathList= q : _} ->
                   -- If in melee and path blocked by actors (even proj.)
                   -- change target for this turn due to urgency.
                   -- Because of @condInMelee@ new target will be stash
                   -- or enemy if any other is left, or empty target.
                   -- If not in melee, keep target and consider your options
                   -- (wait until blocking actors move or displace or melee
                   -- or sidestep). We don't want to wander away
                   -- in search of loot, only to turn around next turn
                   -- when the enemy is again considered.
                   if not condInMelee
                      || q == bpos body  -- blocked by the enemy, great!
                      || not (occupiedBigLvl q lvl)
                         && not (occupiedProjLvl q lvl)
                   then return $ Just tap{tapPath=mpath}
                   else pickNewTargetIgnore (Just a)
        TPoint _ lid _ | lid /= blid b -> pickNewTarget  -- wrong level
        TPoint tgoal lid pos -> case tgoal of
          TStash fid2 -> do
            oursExploring <- getsState $ oursExploringAssocs (bfid b)
            let oursExploringLid =
                  filter (\(_, body) -> blid body == lid) oursExploring
                spawnFreqs = CK.cactorFreq $ okind cocave $ lkind lvl
                hasGroup grp = fromMaybe 0 (lookup grp spawnFreqs) > 0
                lvlSpawnsUs = any hasGroup $ fgroups (gplayer fact)
           -- Even if made peace with the faction, loot stash one last time.
            if (calmE || null nearbyFoes)  -- no risk or can't defend anyway
               && not heavilyDistressed  -- not under heavy fire
               && gstash (factionD EM.! fid2) == Just (lid, pos)
               -- The condition below is more lenient than in @closestStashes@
               -- to avoid wasting time on guard's movement.
               && (fid2 == bfid b
                   && (pos == bpos b  -- guarded by me, so keep guarding
                       && (null nearbyFoes  -- if no foes nearby
                           || length oursExploringLid > 1) -- or buddies nearby
                       || isNothing (posToBigLvl pos lvl))  -- or unguarded
                   && (length oursExploring > 1  -- other actors able to explore
                       || lvlSpawnsUs)  -- or future spawned will be able
                   || isFoe (bfid b) fact fid2)
            then return $ Just tap
            else pickNewTarget
          -- In this case, need to retarget, to focus on foes that melee ours
          -- and not, e.g., on remembered foes or items.
          _ | condInMelee || not (null cstashes) -> pickNewTarget
          TEnemyPos _  -- chase last position even if foe hides
            | bpos b == pos -> tellOthersNothingHere
            | recentlyFled -> pickNewTarget
                -- forget enemy positions to prevent attacking them again soon
            | otherwise -> do
              -- Here pick the closer enemy, the remembered or seen, to avoid
              -- loops when approaching new enemy obscures him behind obstacle
              -- but reveals the previously remembered one, etc.
              let remainingDist = chessDist (bpos b) pos
              if any (\(_, b3) -> chessDist (bpos b) (bpos b3) < remainingDist)
                     nearbyFoes
              then pickNewTarget
              else return $ Just tap
          -- Don't stop fleeing into hideout after 5 turns even if foes appear.
          THideout ->
            -- Approach or stay in the hideout until 20 turns pass.
            if not recentlyFled20
            then pickNewTarget
            else return $ Just tap
          -- Prefer close foes to anything else below.
          _ | not (null nearbyFoes) -> pickNewTarget
          -- Below we check the target could not be picked again in
          -- pickNewTarget (e.g., an item got picked up by our teammate)
          -- and only in this case it is invalidated.
          -- This ensures targets are eventually reached (unless a foe
          -- shows up) and not changed all the time mid-route
          -- to equally interesting, but perhaps a bit closer targets,
          -- most probably already targeted by other actors.
          TEmbed bag p -> assert (adjacent pos p) $ do
            -- First, stairs and embedded items from @closestTriggers@.
            -- We don't check skills, because they normally don't change
            -- or we can put some equipment back and recover them.
            -- We don't determine if the stairs or embed are interesting
            -- (this changes with time), but allow the actor
            -- to reach them and then retarget. The two things we check
            -- is whether the embedded bag is still there, or used up
            -- and whether we happen to be already adjacent to @p@,
            -- even though not necessarily at @pos@.
            bag2 <- getsState $ getEmbedBag lid p  -- not @pos@
            if | bag /= bag2 -> pickNewTarget  -- others will notice soon enough
               | adjacent (bpos b) p ->  -- regardless if at @pos@ or not
                   setPath $ TPoint TKnown lid (bpos b)
                     -- stay there one turn (high chance to become leader)
                     -- to enable triggering; if trigger fails
                     -- (e.g, changed skills), will retarget next turn (@TAny@)
               | otherwise -> return $ Just tap
          TItem bag -> do
            bag2 <- getsState $ getFloorBag lid pos
            if | bag /= bag2 -> pickNewTarget  -- others will notice soon enough
               | bpos b == pos ->
                   setPath $ TPoint TKnown lid (bpos b)
                     -- stay there one turn (high chance to become leader)
                     -- to enable pickup; if pickup fails, will retarget
               | otherwise -> return $ Just tap
          TSmell ->
            if not canSmell
               || let sml = EM.findWithDefault timeZero pos (lsmell lvl)
                  in sml <= ltime lvl
            then pickNewTarget  -- others will notice soon enough
            else return $ Just tap
          TBlock -> do  -- e.g., door or first unknown tile of an area
            let t = lvl `at` pos
            if isStuck  -- not a very important target, because blocked
               || alterSkill < fromEnum (lalter PointArray.! pos)
                    -- tile was searched or altered or skill lowered
               || Tile.isWalkable coTileSpeedup t
                    -- tile is no longer unwalkable, so was explored
                    -- so time to recalculate target
            then pickNewTarget  -- others will notice soon enough
            else return $ Just tap
          TUnknown ->
            let t = lvl `at` pos
            in if lexpl lvl <= lseen lvl
                  || not (isUknownSpace t)
               then pickNewTarget  -- others will notice soon enough
               else return $ Just tap
          TKnown ->
            if bpos b == pos
               || isStuck
               || alterSkill < fromEnum (lalter PointArray.! pos)
                    -- tile was searched or altered or skill lowered
            then pickNewTarget  -- others unconcerned
            else return $ Just tap
        _ | condInMelee || not (null nearbyFoes && null cstashes) ->
            pickNewTarget
        TNonEnemy _ | mleader == Just aid ->  -- a leader, never follow
          pickNewTarget
        TNonEnemy a -> do
          body <- getsState $ getActorBody a
          if blid body /= blid b  -- wrong level
          then pickNewTarget
          else do
            -- Update path. If impossible, pick another target.
            mpath <- getCachePath aid $ bpos body
            case mpath of
              Nothing -> pickNewTarget
              Just AndPath{pathList=[]} -> pickNewTarget
              _ -> return $ Just tap{tapPath=mpath}
        TVector{} -> if bpos b /= pathGoal
                     then return $ Just tap
                     else pickNewTarget
  if canReach
  then maybe pickNewTarget updateTgt oldTgtUpdatedPath
  else return Nothing
