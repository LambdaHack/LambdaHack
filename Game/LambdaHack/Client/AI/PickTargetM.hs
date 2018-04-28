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

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES

import           Game.LambdaHack.Client.AI.ConditionM
import           Game.LambdaHack.Client.Bfs
import           Game.LambdaHack.Client.BfsM
import           Game.LambdaHack.Client.CommonM
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Common.Ability
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Frequency
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.Random
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Vector
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Content.RuleKind
import           Game.LambdaHack.Content.TileKind (isUknownSpace)

-- | Verify and possibly change the target of an actor. This function both
-- updates the target in the client state and returns the new target explicitly.
refreshTarget :: MonadClient m => (ActorId, Actor) -> m (Maybe TgtAndPath)
-- This inline speeds up execution by 5% and decreases allocation by 10%,
-- despite probably bloating executable:
{-# INLINE refreshTarget #-}
refreshTarget (aid, body) = do
  side <- getsClient sside
  let !_A = assert (bfid body == side
                    `blame` "AI tries to move an enemy actor"
                    `swith` (aid, body, side)) ()
  let !_A = assert (isNothing (btrajectory body) && not (bproj body)
                    `blame` "AI gets to manually move its trajectory actors"
                    `swith` (aid, body, side)) ()
  mtarget <- computeTarget aid
  case mtarget of
    Nothing -> do
      -- Melee in progress and the actor can't contribute
      -- and would slow down others if he acted.
      modifyClient $ \cli -> cli {stargetD = EM.delete aid (stargetD cli)}
      return Nothing
    Just tgtMPath -> do
      -- _debugoldTgt <- getsClient $ EM.lookup aid . stargetD
      -- Choose a target from those proposed by AI for the actor.
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

computeTarget :: forall m. MonadClient m => ActorId -> m (Maybe TgtAndPath)
{-# INLINE computeTarget #-}
computeTarget aid = do
  cops@COps{coTileSpeedup} <- getsState scops
  b <- getsState $ getActorBody aid
  mleader <- getsClient sleader
  scondInMelee <- getsClient scondInMelee
  salter <- getsClient salter
  -- We assume the actor eventually becomes a leader (or has the same
  -- set of abilities as the leader, anyway) and set his target accordingly.
  actorAspect <- getsState sactorAspect
  let lalter = salter EM.! blid b
      condInMelee = fromMaybe (error $ "" `showFailure` condInMelee)
                              (scondInMelee EM.! blid b)
      stdRuleset = getStdRuleset cops
      nearby = rnearby stdRuleset
      ar = fromMaybe (error $ "" `showFailure` aid) (EM.lookup aid actorAspect)
      actorMaxSk = IA.aSkills ar
      alterSkill = EM.findWithDefault 0 AbAlter actorMaxSk
  lvl@Level{lxsize, lysize} <- getLevel $ blid b
  let stepAccesible :: AndPath -> Bool
      stepAccesible AndPath{pathList=q : _} =
        -- Effectively, only @alterMinWalk@ is checked, because real altering
        -- is not done via target path, but action after end of path.
        alterSkill >= fromEnum (lalter PointArray.! q)
      stepAccesible _ = False
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  oldTgtUpdatedPath <- case mtgtMPath of
    Just TgtAndPath{tapTgt,tapPath=NoPath} ->
      -- This case is especially for TEnemyPos that would be lost otherwise.
      -- This is also triggered by @UpdLeadFaction@.
      Just <$> createPath aid tapTgt
    Just tap@TgtAndPath{..} -> do
      mvalidPos <- getsState $ aidTgtToPos aid (blid b) tapTgt
      if | isNothing mvalidPos -> return Nothing  -- wrong level
         | bpos b == pathGoal tapPath ->
             return mtgtMPath  -- goal reached; stay there picking up items
         | otherwise -> return $! case tapPath of
             AndPath{pathList=q : rest,..} -> case chessDist (bpos b) q of
               0 ->  -- step along path
                 let newPath = AndPath{ pathList = rest
                                      , pathGoal
                                      , pathLen = pathLen - 1 }
                 in if stepAccesible newPath
                    then Just tap{tapPath=newPath}
                    else Nothing
               1 ->  -- no move or a sidestep last turn
                 if stepAccesible tapPath
                 then mtgtMPath
                 else Nothing
               _ -> Nothing  -- veered off the path
             AndPath{pathList=[],..}->
               Nothing  -- path to the goal was partial; let's target again
             NoPath -> error $ "" `showFailure` tap
    Nothing -> return Nothing  -- no target assigned yet
  fact <- getsState $ (EM.! bfid b) . sfactionD
  allFoes <- getsState $ foeRegularAssocs (bfid b) (blid b)
  dungeon <- getsState sdungeon
  let canMove = EM.findWithDefault 0 AbMove actorMaxSk > 0
                || EM.findWithDefault 0 AbDisplace actorMaxSk > 0
                -- Needed for now, because AI targets and shoots enemies
                -- based on the path to them, not LOS to them:
                || EM.findWithDefault 0 AbProject actorMaxSk > 0
  actorMinSk <- getsState $ actorSkills Nothing aid
  condCanProject <-
    condCanProjectM (EM.findWithDefault 0 AbProject actorMaxSk) aid
  condEnoughGear <- condEnoughGearM aid
  let condCanMelee = actorCanMelee actorAspect aid b
      condHpTooLow = hpTooLow b ar
  friends <- getsState $ friendRegularList (bfid b) (blid b)
  let canEscape = fcanEscape (gplayer fact)
      canSmell = IA.aSmell ar > 0
      meleeNearby | canEscape = nearby `div` 2
                  | otherwise = nearby
      rangedNearby = 2 * meleeNearby
      -- Don't melee-target nonmoving actors, unless they attack ours,
      -- because nonmoving can't be lured nor ambushed nor can't chase.
      -- This is especially important for fences, tower defense actors, etc.
      -- If content gives nonmoving actor loot, this becomes problematic.
      targetableMelee aidE body = do
        actorMaxSkE <- maxActorSkillsClient aidE
        let attacksFriends = any (adjacent (bpos body) . bpos) friends
            -- 3 is
            -- 1 from condSupport1
            -- + 2 from foe being 2 away from friend before he closed in
            -- + 1 for as a margin for ambush, given than actors exploring
            -- can't physically keep adjacent all the time
            n | IA.aAggression ar >= 2 = rangedNearby  -- boss never waits
              | condInMelee = if attacksFriends then 4 else 0
              | otherwise = meleeNearby
            nonmoving = EM.findWithDefault 0 AbMove actorMaxSkE <= 0
        return {-keep lazy-} $
          case chessDist (bpos body) (bpos b) of
            1 -> True  -- if adjacent, target even if can't melee, to flee
            cd -> condCanMelee && cd <= n && (not nonmoving || attacksFriends)
      -- Even when missiles run out, the non-moving foe will still be
      -- targeted, which is fine, since he is weakened by ranged, so should be
      -- meleed ASAP, even if without friends.
      targetableRanged body =
        (not condInMelee || IA.aAggression ar >= 2)  -- boss fires at will
        && chessDist (bpos body) (bpos b) < rangedNearby
        && condCanProject
      targetableEnemy (aidE, body) = do
        tMelee <- targetableMelee aidE body
        return $! targetableRanged body || tMelee
  nearbyFoes <- filterM targetableEnemy allFoes
  isStairPos <- getsState $ \s lid p -> isStair lid p s
  discoBenefit <- getsClient sdiscoBenefit
  fleeD <- getsClient sfleeD
  s <- getState
  getKind <- getsState $ flip getIidKind
  let desirableBagFloor bag = any (\iid ->
        let Benefit{benPickup} = discoBenefit EM.! iid
        in desirableItem canEscape benPickup (getKind iid)) $ EM.keys bag
      desirableFloor (_, (_, bag)) = desirableBagFloor bag
      focused = gearSpeed ar < speedWalk || condHpTooLow
      couldMoveLastTurn =
        let actorSk = if mleader == Just aid then actorMaxSk else actorMinSk
        in EM.findWithDefault 0 AbMove actorSk > 0
      isStuck = waitedLastTurn b && couldMoveLastTurn
      slackTactic =
        ftactic (gplayer fact)
          `elem` [TMeleeAndRanged, TMeleeAdjacent, TBlock, TRoam, TPatrol]
      setPath :: Target -> m (Maybe TgtAndPath)
      setPath tgt = do
        let take7 tap@TgtAndPath{tapTgt=TEnemy{}} =
              tap  -- @TEnemy@ needed for projecting, even by roaming actors
            take7 tap@TgtAndPath{tapTgt,tapPath=AndPath{..}} =
              if slackTactic then
                -- Best path only followed 7 moves; then straight on. Cheaper.
                let path7 = take 7 pathList
                    vtgt | bpos b == pathGoal = tapTgt  -- goal reached
                         | otherwise = TVector $ towards (bpos b) pathGoal
                in TgtAndPath{tapTgt=vtgt, tapPath=AndPath{pathList=path7, ..}}
              else tap
            take7 tap = tap
        tgtpath <- createPath aid tgt
        return $ Just $ take7 tgtpath
      pickNewTarget :: m (Maybe TgtAndPath)
      pickNewTarget = do
        cfoes <- closestFoes nearbyFoes aid
        case cfoes of
          (_, (aid2, _)) : _ -> setPath $ TEnemy aid2 False
          [] | condInMelee -> return Nothing  -- don't slow down fighters
            -- this looks a bit strange, because teammates stop in their tracks
            -- all around the map (unless very close to the combatant),
            -- but the intuition is, not being able to help immediately,
            -- and not being too friendly to each other, they just wait and see
            -- and also shout to the teammate to flee and lure foes into ambush
          [] -> do
            -- Tracking enemies is more important than exploring,
            -- and smelling actors are usually blind, so bad at exploring.
            smpos <- if canSmell
                     then closestSmell aid
                     else return []
            case smpos of
              [] -> do
                citemsRaw <- closestItems aid
                let citems = toFreq "closestItems"
                             $ filter desirableFloor citemsRaw
                if nullFreq citems then do
                  -- This is mostly lazy and referred to a few times below.
                  ctriggersRaw <- closestTriggers ViaAnything aid
                  let ctriggers = toFreq "closestTriggers" ctriggersRaw
                  if nullFreq ctriggers then do
                      let vToTgt v0 = do
                            let vFreq = toFreq "vFreq"
                                        $ (20, v0) : map (1,) moves
                            v <- rndToAction $ frequency vFreq
                            -- Items and smells, etc. considered every 7 moves.
                            let pathSource = bpos b
                                tra = trajectoryToPathBounded
                                        lxsize lysize pathSource (replicate 7 v)
                                pathList = nub tra
                                pathGoal = last pathList
                                pathLen = length pathList
                            return $ Just $
                              TgtAndPath
                                { tapTgt = TVector v
                                , tapPath = if pathLen == 0
                                            then NoPath
                                            else AndPath{..} }
                          oldpos = fromMaybe originPoint (boldpos b)
                          vOld = bpos b `vectorToFrom` oldpos
                          pNew = shiftBounded lxsize lysize (bpos b) vOld
                      if slackTactic && not isStuck
                         && isUnit vOld && bpos b /= pNew
                         && Tile.isWalkable coTileSpeedup (lvl `at` pNew)
                              -- if initial altering, consider carefully below
                      then vToTgt vOld
                      else do
                        upos <- closestUnknown aid
                        case upos of
                          Nothing -> do
                            modifyClient $ \cli -> cli {sexplored =
                              ES.insert (blid b) (sexplored cli)}
                            explored <- getsClient sexplored
                            let allExplored =
                                  ES.size explored == EM.size dungeon
                            if allExplored || nullFreq ctriggers then do
                              -- All stones turned, time to win or die.
                              afoes <- closestFoes allFoes aid
                              case afoes of
                                (_, (aid2, _)) : _ ->
                                  setPath $ TEnemy aid2 False
                                [] ->
                                  if nullFreq ctriggers then do
                                    furthest <- furthestKnown aid
                                    setPath $ TPoint TKnown (blid b) furthest
                                  else do
                                    (p, (p0, bag)) <-
                                      rndToAction $ frequency ctriggers
                                    setPath $ TPoint (TEmbed bag p0) (blid b) p
                            else do
                              (p, (p0, bag)) <-
                                rndToAction $ frequency ctriggers
                              setPath $ TPoint (TEmbed bag p0) (blid b) p
                          Just p -> setPath $ TPoint TUnknown (blid b) p
                  else do
                    (p, (p0, bag)) <- rndToAction $ frequency ctriggers
                    setPath $ TPoint (TEmbed bag p0) (blid b) p
                else do
                  (p, bag) <- rndToAction $ frequency citems
                  setPath $ TPoint (TItem bag) (blid b) p
              (_, (p, _)) : _ -> setPath $ TPoint TSmell (blid b) p
      tellOthersNothingHere pos = do
        let f TgtAndPath{tapTgt} = case tapTgt of
              TPoint _ lid p -> p /= pos || lid /= blid b
              _ -> True
        modifyClient $ \cli -> cli {stargetD = EM.filter f (stargetD cli)}
        pickNewTarget
      tileAdj :: (Point -> Bool) -> Point -> Bool
      tileAdj f p = any f $ vicinityUnsafe p
      followingWrong permit =
        permit && (condInMelee  -- in melee, stop following
                   || mleader == Just aid)  -- a leader, never follow
      updateTgt :: TgtAndPath -> m (Maybe TgtAndPath)
      updateTgt TgtAndPath{tapPath=NoPath} = pickNewTarget
      updateTgt _ | EM.member aid fleeD = pickNewTarget
        -- forget enemy positions to prevent attacking them again soon
      updateTgt tap@TgtAndPath{tapPath=AndPath{..},tapTgt} = case tapTgt of
        TEnemy a permit -> do
          body <- getsState $ getActorBody a
          if | (condInMelee  -- fight close foes or nobody at all
                || not focused && not (null nearbyFoes))  -- prefers closer foes
               && a `notElem` map fst nearbyFoes  -- old one not close enough
               || blid body /= blid b  -- wrong level
               || actorDying body -> -- foe already dying
               pickNewTarget
             | followingWrong permit -> pickNewTarget
             | bpos body == pathGoal ->
               return $ Just tap
                 -- The enemy didn't move since the target acquired.
                 -- If any walls were added that make the enemy
                 -- unreachable, AI learns that the hard way,
                 -- as soon as it bumps into them.
             | otherwise -> do
               -- If there are no unwalkable tiles on the path to enemy,
               -- he gets target @TEnemy@ and then, even if such tiles emerge,
               -- the target updated by his moves remains @TEnemy@.
               -- Conversely, he is stuck with @TKnown@ if initial target had
               -- unwalkable tiles, for as long as they remain. Harmless quirk.
               mpath <- getCachePath aid $ bpos body
               case mpath of
                 NoPath -> pickNewTarget  -- enemy became unreachable
                 AndPath{pathLen=0} -> pickNewTarget  -- he is his own enemy
                 AndPath{} -> return $ Just tap{tapPath=mpath}
          -- In this case, need to retarget, to focus on foes that melee ours
          -- and not, e.g., on remembered foes or items.
        _ | condInMelee -> pickNewTarget
        TPoint _ lid _ | lid /= blid b -> pickNewTarget  -- wrong level
        TPoint tgoal lid pos -> case tgoal of
          _ | not $ null nearbyFoes ->
            pickNewTarget  -- prefer close foes to anything else
          TEnemyPos _ permit  -- chase last position even if foe hides
            | bpos b == pos -> tellOthersNothingHere pos
            | followingWrong permit -> pickNewTarget
            | otherwise -> return $ Just tap
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
                   setPath $ TPoint TAny lid (bpos b)
                     -- stay there one turn (high chance to become leader)
                     -- to enable triggering; if trigger fails
                     -- (e.g, changed skills), will retarget next turn (@TAny@)
               | otherwise -> return $ Just tap
          TItem bag -> do
            bag2 <- getsState $ getFloorBag lid pos
            if | bag /= bag2 -> pickNewTarget  -- others will notice soon enough
               | bpos b == pos ->
                   setPath $ TPoint TAny lid (bpos b)
                     -- stay there one turn (high chance to become leader)
                     -- to enable pickup; if pickup fails, will retarget
               | otherwise -> return $ Just tap
          TSmell ->
            let lvl2 = sdungeon s EM.! lid
            in if not canSmell
                  || let sml = EM.findWithDefault timeZero pos (lsmell lvl2)
                     in sml <= ltime lvl2
               then pickNewTarget  -- others will notice soon enough
               else return $ Just tap
          TUnknown ->
            let lvl2 = sdungeon s EM.! lid
                t = lvl2 `at` pos
            in if lexpl lvl2 <= lseen lvl2
                  || not (isUknownSpace t)
                  || condEnoughGear && tileAdj (isStairPos lid) pos
                       -- the unknown may be on the other side of the level
                       -- and getting there only to explore 1 tile and get back
                       -- looks silly
               then pickNewTarget  -- others will notice soon enough
               else return $ Just tap
          TKnown -> do  -- e.g., staircase or first unknown tile of an area
            explored <- getsClient sexplored
            let allExplored = ES.size explored == EM.size dungeon
                lvl2 = sdungeon s EM.! lid
            if bpos b == pos
               || isStuck
               || alterSkill < fromEnum (lalter PointArray.! pos)
                    -- tile was searched or altered or skill lowered
               || Tile.isWalkable coTileSpeedup (lvl2 `at` pos)
                  && not allExplored  -- not patrolling explored dungeon
                    -- tile is no longer unwalkable, so was explored
                    -- so time to recalculate target
            then pickNewTarget  -- others unconcerned
            else return $ Just tap
          TAny -> pickNewTarget  -- reset elsewhere or carried over from UI
        TVector{} -> if pathLen > 1
                     then return $ Just tap
                     else pickNewTarget
  if canMove
  then case oldTgtUpdatedPath of
    Nothing -> pickNewTarget
    Just tap -> updateTgt tap
  else return $ Just $ TgtAndPath (TEnemy aid True) NoPath
