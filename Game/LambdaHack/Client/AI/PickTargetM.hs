{-# LANGUAGE TupleSections #-}
-- | Let AI pick the best target for an actor.
module Game.LambdaHack.Client.AI.PickTargetM
  ( targetStrategy, createPath
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES

import Game.LambdaHack.Client.AI.ConditionM
import Game.LambdaHack.Client.AI.Preferences
import Game.LambdaHack.Client.AI.Strategy
import Game.LambdaHack.Client.Bfs
import Game.LambdaHack.Client.BfsM
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
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind (isUknownSpace)

-- | AI proposes possible targets for the actor. Never empty.
targetStrategy :: forall m. MonadClient m
               => ActorId -> m (Strategy TgtAndPath)
{-# INLINE targetStrategy #-}
targetStrategy aid = do
  cops@Kind.COps{corule} <- getsState scops
  b <- getsState $ getActorBody aid
  mleader <- getsClient _sleader
  condInMelee <- getsClient scondInMelee
  let (newCondInMelee, oldCondInMelee) = case condInMelee EM.! blid b of
        Right conds -> if mleader == Just aid then (False, False) else conds
        Left{} -> assert `failure` condInMelee
      stdRuleset = Kind.stdRuleset corule
      nearby = rnearby stdRuleset
  itemToF <- itemToFullClient
  lvl@Level{lxsize, lysize} <- getLevel $ blid b
  let stepAccesible :: AndPath -> Bool
      stepAccesible AndPath{pathList=q : _ : _} =  -- goal not adjacent
        accessible cops lvl q  -- non-goal has to be accessible
      stepAccesible AndPath{} = True  -- ok if goal inaccessible
      stepAccesible NoPath = False
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  oldTgtUpdatedPath <- case mtgtMPath of
    Just TgtAndPath{tapTgt,tapPath=NoPath} ->
      -- This case is especially for TEnemyPos that would be lost otherwise.
      -- This is also triggered by @UpdLeadFaction@. The recreated path can be
      -- different than on the other client (AI or UI), but we don't care
      -- as long as the target stays the same at least for a moment.
      Just <$> createPath aid tapTgt
    Just tap@TgtAndPath{..} -> do
      mvalidPos <- aidTgtToPos aid (blid b) tapTgt
      if isNothing mvalidPos then return Nothing  -- wrong level
      else return $! case tapPath of
        AndPath{pathList=q : rest,..} ->
          if | adjacent (bpos b) q ->
               if stepAccesible tapPath
               then mtgtMPath  -- no move or sidestep last turn
               else Nothing
             | bpos b == q ->
               let newPath = AndPath{ pathList = rest
                                    , pathGoal
                                    , pathLen = pathLen - 1 }
               in if stepAccesible newPath
                  then Just tap{tapPath=newPath}  -- step along path
                  else Nothing
             | otherwise -> Nothing  -- veered off the path
        AndPath{pathList=[],..}->
          if bpos b == pathGoal then
            mtgtMPath  -- goal reached; stay there picking up items
          else
            Nothing  -- somebody pushed us off the goal or the path to the goal
                     -- was partial; let's target again
        NoPath -> assert `failure` ()
    Nothing -> return Nothing  -- no target assigned yet
  let !_A = assert (not $ bproj b) ()  -- would work, but is probably a bug
  fact <- getsState $ (EM.! bfid b) . sfactionD
  allFoes <- getsState $ actorRegularAssocs (isAtWar fact) (blid b)
  dungeon <- getsState sdungeon
  -- We assume the actor eventually becomes a leader (or has the same
  -- set of abilities as the leader, anyway) and set his target accordingly.
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup aid actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` aid
      actorMaxSk = aSkills ar
      canMove = EM.findWithDefault 0 AbMove actorMaxSk > 0
                || EM.findWithDefault 0 AbDisplace actorMaxSk > 0
                -- Needed for now, because AI targets and shoots enemies
                -- based on the path to them, not LOS to them:
                || EM.findWithDefault 0 AbProject actorMaxSk > 0
  actorMinSk <- getsState $ actorSkills Nothing aid ar
  condCanProject <- condCanProjectM True aid
  condHpTooLow <- condHpTooLowM aid
  condEnoughGear <- condEnoughGearM aid
  condMeleeBad <- condMeleeBadM aid
  let friendlyFid fid = fid == bfid b || isAllied fact fid
  friends <- getsState $ actorRegularList friendlyFid (blid b)
  let canEscape = fcanEscape (gplayer fact)
      condNoUsableWeapon = bweapon b == 0
      canSmell = aSmell ar > 0
      meleeNearby | newCondInMelee = nearby `div` 6  -- x2 in targetableMelee
                  | canEscape = nearby `div` 2
                  | otherwise = nearby
      rangedNearby = 2 * meleeNearby
      -- Don't target nonmoving actors at all if bad melee,
      -- because nonmoving can't be lured nor ambushed.
      -- This is especially important for fences, tower defense actors, etc.
      -- If content gives nonmoving actor loot, this becomes problematic.
      targetableMelee aidE body = do
        actorMaxSkE <- enemyMaxAb aidE
        let attacksFriends = any (adjacent (bpos body) . bpos) friends
            n = if attacksFriends then rangedNearby else meleeNearby
            nonmoving = EM.findWithDefault 0 AbMove actorMaxSkE <= 0
        return {-keep lazy-} $
          chessDist (bpos body) (bpos b) < n
          && not condNoUsableWeapon
          && EM.findWithDefault 0 AbMelee actorMaxSk > 0
          && not (hpTooLow b ar)
          && not (nonmoving && condMeleeBad)
      targetableRangedOrSpecial body =
        chessDist (bpos body) (bpos b) < rangedNearby
        && condCanProject
      targetableEnemy (aidE, body) = do
        tMelee <- targetableMelee aidE body
        return $! targetableRangedOrSpecial body || tMelee
  nearbyFoes <- filterM targetableEnemy allFoes
  explored <- getsClient sexplored
  isStairPos <- getsState $ \s lid p -> isStair lid p s
  let lidExplored = ES.member (blid b) explored
      allExplored = ES.size explored == EM.size dungeon
      itemUsefulness itemFull =
        fst <$> totalUsefulness cops b ar fact itemFull
      desirableBagFloor bag = any (\(iid, k) ->
        let itemFull = itemToF iid k
            use = itemUsefulness itemFull
        in desirableItem canEscape use itemFull) $ EM.assocs bag
      desirableBagEmbed bag = any (\(iid, k) ->
        let itemFull = itemToF iid k
            use = itemUsefulness itemFull
        in maybe False (> 0) use) $ EM.assocs bag  -- mixed blessing OK; caches
      desirableFloor (_, (_, bag)) = desirableBagFloor bag
      desirableEmbed (_, (_, (_, bag))) = desirableBagEmbed bag
      focused = bspeed b ar < speedWalk || condHpTooLow
      couldMoveLastTurn =
        let actorSk = if mleader == Just aid then actorMaxSk else actorMinSk
        in EM.findWithDefault 0 AbMove actorSk > 0
      isStuck = waitedLastTurn b && couldMoveLastTurn
      slackTactic =
        ftactic (gplayer fact)
          `elem` [TMeleeAndRanged, TMeleeAdjacent, TBlock, TRoam, TPatrol]
      setPath :: Target -> m (Strategy TgtAndPath)
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
        return $! returN "setPath" $ take7 tgtpath
      pickNewTarget :: m (Strategy TgtAndPath)
      pickNewTarget = do
        cfoes <- closestFoes nearbyFoes aid
        case cfoes of
          (_, (aid2, _)) : _ -> setPath $ TEnemy aid2 False
          [] | newCondInMelee -> return reject  -- don't slow down fighters
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
                  ctriggersRaw <- closestTriggers Nothing aid
                  let ctriggers = toFreq "closestTriggers"
                                  $ filter desirableEmbed ctriggersRaw
                  if not condEnoughGear || nullFreq ctriggers then do
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
                            return $! returN "tgt with no exploration"
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
                         && accessible cops lvl pNew
                      then vToTgt vOld
                      else do
                        upos <- if lidExplored
                                then return Nothing
                                else closestUnknown aid -- modifies sexplored
                        case upos of
                          Nothing -> do
                            explored2 <- getsClient sexplored
                            let allExplored2 = ES.size explored2
                                               == EM.size dungeon
                            if allExplored2 || nullFreq ctriggers then do
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
              TPoint (TEnemyPos _ _) lid p -> p /= pos || lid /= blid b
              _ -> True
        modifyClient $ \cli -> cli {stargetD = EM.filter f (stargetD cli)}
        pickNewTarget
      tileAdj :: (Point -> Bool) -> Point -> Bool
      tileAdj f p = any f $ vicinityUnsafe p
      updateTgt :: TgtAndPath -> m (Strategy TgtAndPath)
      updateTgt TgtAndPath{tapPath=NoPath} = pickNewTarget
      updateTgt tap@TgtAndPath{tapPath=AndPath{..},tapTgt} = case tapTgt of
        TEnemy a permit -> do
          body <- getsState $ getActorBody a
          if | (not focused || newCondInMelee)  -- prefers closer foes
               && a `notElem` map fst nearbyFoes  -- old one not close enough
               || blid body /= blid b  -- wrong level
               || actorDying body  -- foe already dying
               || permit ->  -- never follow a friend more than 1 step
               pickNewTarget
             | bpos body == pathGoal ->
               return $! returN "TEnemy" tap
                 -- The enemy didn't move since the target acquired.
                 -- If any walls were added that make the enemy
                 -- unreachable, AI learns that the hard way,
                 -- as soon as it bumps into them.
             | otherwise -> do
               let p = bpos body
               mpath <- getCachePath aid p
               case mpath of
                 NoPath -> pickNewTarget  -- enemy became unreachable
                 AndPath{pathLen=0} -> pickNewTarget  -- he is his own enemy
                 AndPath{} -> return $! returN "TEnemy" tap{tapPath=mpath}
        _ | newCondInMelee && not oldCondInMelee -> pickNewTarget
        TPoint _ lid _ | lid /= blid b -> pickNewTarget  -- wrong level
        TPoint _ _ pos | pos == bpos b -> tellOthersNothingHere pos
        TPoint tgoal lid pos -> case tgoal of
          TEnemyPos _ permit  -- chase last position even if foe hides
            | permit -> pickNewTarget  -- never follow a friend more than 1 step
            | otherwise -> return $! returN "TEnemyPos" tap
          _ | not $ null nearbyFoes ->
            pickNewTarget  -- prefer close foes to anything else
          -- Below we check the target could not be picked again in
          -- pickNewTarget (e.g., an item got picked up by our teammate)
          -- and only in this case it is invalidated.
          -- This ensures targets are eventually reached (unless a foe
          -- shows up) and not changed all the time mid-route
          -- to equally interesting, but perhaps a bit closer targets,
          -- most probably already targeted by other actors.
          TEmbed bag p -> assert (adjacent pos p) $ do
            -- We start with stairs and embedded items from @closestTriggers@.
            -- We don't check skills, because they normally don't change
            -- or we can put some equipment back and recover them.
            -- We don't determine if the stairs are interesting
            -- (this changes with time), but allow the actor
            -- to reach them and then retarget. The only thing we check
            -- is whether the embedded bag is still there, or used up.
            bag2 <- getsState $ getEmbedBag lid p  -- not @pos@
            if bag /= bag2
            then pickNewTarget
            else return $! returN "TEmbed" tap
          TItem bag -> do
            -- We don't check skill nor desirability of the bag,
            -- because the skill and the bag were OK when target was set.
            bag2 <- getsState $ getFloorBag lid pos
            if bag /= bag2
            then pickNewTarget
            else return $! returN "TItem" tap
          TSmell ->
            if not canSmell
               || let sml = EM.findWithDefault timeZero pos (lsmell lvl)
                  in sml <= ltime lvl
            then pickNewTarget
            else return $! returN "TSmell" tap
          TUnknown ->
            let t = lvl `at` pos
            in if lidExplored
                  || not (isUknownSpace t)
                  || condEnoughGear && tileAdj (isStairPos lid) pos
               then pickNewTarget
               else return $! returN "TUnknown" tap
          TKnown ->
            if isStuck || not allExplored  -- new levels created, etc.
            then pickNewTarget
            else return $! returN "TKnown" tap
          TAny -> pickNewTarget  -- reset elsewhere or carried over from UI
        TVector{} -> if pathLen > 1
                     then return $! returN "TVector" tap
                     else pickNewTarget
  if canMove
  then case oldTgtUpdatedPath of
    Nothing -> pickNewTarget
    Just tap -> updateTgt tap
  else return $! returN "NoMove" $ TgtAndPath (TEnemy aid True) NoPath

createPath :: MonadClient m => ActorId -> Target -> m TgtAndPath
createPath aid tapTgt = do
  b <- getsState $ getActorBody aid
  mpos <- aidTgtToPos aid (blid b) tapTgt
  case mpos of
    Nothing -> return TgtAndPath{tapTgt, tapPath=NoPath}
    Just p -> do
      tapPath <- getCachePath aid p
      return $! TgtAndPath{..}
