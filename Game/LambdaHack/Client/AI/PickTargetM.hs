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
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind (isUknownSpace)

-- | AI proposes possible targets for the actor. Never empty.
targetStrategy :: forall m. MonadClient m
               => ActorId -> m (Strategy TgtAndPath)
targetStrategy aid = do
  cops@Kind.COps{corule, cotile, coTileSpeedup} <- getsState scops
  let stdRuleset = Kind.stdRuleset corule
      nearby = rnearby stdRuleset
  itemToF <- itemToFullClient
  b <- getsState $ getActorBody aid
  lvl@Level{lxsize, lysize} <- getLevel $ blid b
  let stepAccesible :: AndPath -> Bool
      stepAccesible AndPath{pathList=q : _ : _} =  -- goal not adjacent
        accessible cops lvl q  -- non-goal has to be accessible
      stepAccesible AndPath{} = True  -- ok if goal inaccessible, e.g., suspect
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
      mvalidPos <- aidTgtToPos aid (blid b) (Just tapTgt)
      if isNothing mvalidPos then return Nothing  -- wrong level
      else return $! case tapPath of
        AndPath{pathSource=p,pathList=q : rest,..} ->
          if | bpos b == p -> if stepAccesible tapPath
                              then mtgtMPath  -- no move last turn
                              else Nothing
             | bpos b == q ->
               let newPath = AndPath{ pathSource = q
                                    , pathList = rest
                                    , pathGoal
                                    , pathLen = pathLen - 1 }
               in if stepAccesible newPath
                  then Just tap{tapPath=newPath}  -- step along path
                  else Nothing
             | otherwise -> Nothing  -- veered off the path
        AndPath{pathList=[],..}->
          if pathSource == pathGoal && bpos b == pathSource then
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
      actorMaxSk = aAbility ar
      canMove = EM.findWithDefault 0 AbMove actorMaxSk > 0
                || EM.findWithDefault 0 AbDisplace actorMaxSk > 0
                -- TODO: needed for now, because AI targets and shoots enemies
                -- based on the path to them, not LOS to them.
                || EM.findWithDefault 0 AbProject actorMaxSk > 0
  actorMinSk <- getsState $ actorSkills Nothing aid ar
  condCanProject <- condCanProjectM True aid
  condHpTooLow <- condHpTooLowM aid
  condEnoughGear <- condEnoughGearM aid
  condMeleeBad <- condMeleeBadM aid
  let friendlyFid fid = fid == bfid b || isAllied fact fid
  friends <- getsState $ actorRegularList friendlyFid (blid b)
  -- TODO: refine all this when some actors specialize in ranged attacks
  -- (then we have to target, but keep the distance, we can do similarly for
  -- wounded or alone actors, perhaps only until they are shot first time,
  -- and only if they can shoot at the moment)
  canEscape <- factionCanEscape (bfid b)
  explored <- getsClient sexplored
  smellRadius <- sumOrganEqpClient IK.EqpSlotAddSmell aid
  activeItems <- activeItemsClient aid
  let condNoUsableWeapon = all (not . isMelee) activeItems
      lidExplored = ES.member (blid b) explored
      allExplored = ES.size explored == EM.size dungeon
      canSmell = smellRadius > 0
      meleeNearby | canEscape = nearby `div` 2  -- not aggresive
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
           && not (hpTooLow b activeItems)
           && not (nonmoving && condMeleeBad)
      targetableRangedOrSpecial body =
        chessDist (bpos body) (bpos b) < rangedNearby
        && condCanProject
      targetableEnemy (aidE, body) = do
        tMelee <- targetableMelee aidE body
        return $! targetableRangedOrSpecial body || tMelee
  nearbyFoes <- filterM targetableEnemy allFoes
  mleader <- getsClient _sleader
  let itemUsefulness itemFull =
        fst <$> totalUsefulness cops b activeItems fact itemFull
      desirableBag bag = any (\(iid, k) ->
        let itemFull = itemToF iid k
            use = itemUsefulness itemFull
        in desirableItem canEscape use itemFull) $ EM.assocs bag
      desirable (_, (_, Nothing)) = True
      desirable (_, (_, Just bag)) = desirableBag bag
      -- TODO: make more common when weak ranged foes preferred, etc.
      focused = bspeed b activeItems < speedNormal || condHpTooLow
      couldMoveLastTurn =
        let axtorSk = if mleader == Just aid then actorMaxSk else actorMinSk
        in EM.findWithDefault 0 AbMove axtorSk > 0
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
        -- This is mostly lazy and used between 0 and 3 times below.
        ctriggers <- closestTriggers Nothing aid
        -- TODO: for foes, items, etc. consider a few nearby, not just one
        cfoes <- closestFoes nearbyFoes aid
        case cfoes of
          (_, (aid2, _)) : _ -> setPath $ TEnemy aid2 False
          [] -> do
            -- Tracking enemies is more important than exploring,
            -- and smelling actors are usually blind, so bad at exploring.
            -- TODO: prefer closer items to older smells
            smpos <- if canSmell
                     then closestSmell aid
                     else return []
            case smpos of
              [] -> do
                let ctriggersEarly =
                      if EM.findWithDefault 0 AbTrigger actorMaxSk > 0
                         && condEnoughGear
                      then ctriggers
                      else mzero
                if nullFreq ctriggersEarly then do
                  citems <-
                    if EM.findWithDefault 0 AbMoveItem actorMaxSk > 0
                    then closestItems aid
                    else return []
                  case filter desirable citems of
                    [] -> do
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
                                else closestUnknown aid
                        case upos of
                          Nothing -> do
                            csuspect <- if lidExplored
                                        then return []
                                        else closestSuspect aid
                            case csuspect of
                              [] -> do
                                let ctriggersMiddle =
                                      if EM.findWithDefault 0 AbTrigger
                                                            actorMaxSk > 0
                                         && not allExplored
                                      then ctriggers
                                      else mzero
                                if nullFreq ctriggersMiddle then do
                                  -- All stones turned, time to win or die.
                                  afoes <- closestFoes allFoes aid
                                  case afoes of
                                    (_, (aid2, _)) : _ ->
                                      setPath $ TEnemy aid2 False
                                    [] ->
                                      if nullFreq ctriggers then do
                                        furthest <- furthestKnown aid
                                        setPath $ TPoint (blid b) furthest
                                      else do
                                        p <- rndToAction $ frequency ctriggers
                                        setPath $ TPoint (blid b) p
                                else do
                                  p <- rndToAction $ frequency ctriggers
                                  setPath $ TPoint (blid b) p
                              p : _ -> setPath $ TPoint (blid b) p
                          Just p -> setPath $ TPoint (blid b) p
                    (_, (p, _)) : _ -> setPath $ TPoint (blid b) p
                else do
                  p <- rndToAction $ frequency ctriggers
                  setPath $ TPoint (blid b) p
              (_, (p, _)) : _ -> setPath $ TPoint (blid b) p
      tellOthersNothingHere pos = do
        let f TgtAndPath{tapTgt} = case tapTgt of
              TEnemyPos _ lid p _ -> p /= pos || lid /= blid b
              _ -> True
        modifyClient $ \cli -> cli {stargetD = EM.filter f (stargetD cli)}
        pickNewTarget
      updateTgt :: TgtAndPath -> m (Strategy TgtAndPath)
      updateTgt TgtAndPath{tapPath=NoPath} = pickNewTarget
      updateTgt tap@TgtAndPath{tapPath=AndPath{..},tapTgt} = case tapTgt of
        TEnemy a permit -> do
          body <- getsState $ getActorBody a
          if | not focused  -- prefers closer foes
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
        TEnemyPos _ lid p permit  -- chase last position even if foe hides
          | lid /= blid b  -- wrong level
            || chessDist (bpos b) p >= nearby  -- too far and not visible
            || permit  -- never follow a friend more than 1 step
            -> pickNewTarget
          | p == bpos b -> tellOthersNothingHere p
          | otherwise ->
              return $! returN "TEnemyPos" tap
        _ | not $ null nearbyFoes ->
          pickNewTarget  -- prefer close foes to anything
        TPoint lid pos -> do
          bag <- getsState $ getCBag $ CFloor lid pos
          let t = lvl `at` pos
          if lid /= blid b  -- wrong level
             -- Below we check the target could not be picked again in
             -- pickNewTarget, and only in this case it is invalidated.
             -- This ensures targets are eventually reached (unless a foe
             -- shows up) and not changed all the time mid-route
             -- to equally interesting, but perhaps a bit closer targets,
             -- most probably already targeted by other actors.
             ||
               (EM.findWithDefault 0 AbMoveItem actorMaxSk <= 0
                || not (desirableBag bag))  -- closestItems
               &&
               (pos == bpos b
                || (not canSmell  -- closestSmell
                    || let sml = EM.findWithDefault timeZero pos (lsmell lvl)
                       in sml <= ltime lvl)
                   && if not lidExplored
                      then not (isUknownSpace t)  -- closestUnknown
                           && not (Tile.isSuspect coTileSpeedup t)  -- closestSuspect
                           && not (condEnoughGear && Tile.isStair cotile t)
                      else  -- closestTriggers
                        -- Try to kill that very last enemy for his loot before
                        -- leaving the level or dungeon.
                        not (null allFoes)
                        || -- If all explored, escape/block escapes.
                           (not (Tile.isEscape cotile t)
                            || not allExplored)
                           -- The next case is stairs in closestTriggers.
                           -- We don't determine if the stairs are interesting
                           -- (this changes with time), but allow the actor
                           -- to reach them and then retarget, unless he can't
                           -- trigger them at all.
                           && (EM.findWithDefault 0 AbTrigger actorMaxSk <= 0
                               || not (Tile.isStair cotile t))
                           -- The remaining case is furthestKnown. This is
                           -- always an unimportant target, so we forget it
                           -- if the actor is stuck (waits, though could move;
                           -- or has zeroed individual moving skill,
                           -- but then should change targets often anyway).
                           && (isStuck
                               || not allExplored))
          then pickNewTarget
          else return $! returN "TPoint" tap
        TVector{} -> if pathLen > 1
                     then return $! returN "TVector" tap
                     else pickNewTarget
  if canMove
  then case oldTgtUpdatedPath of
    Nothing -> pickNewTarget
    Just tap -> updateTgt tap
  else return $! returN "NoMove" $ TgtAndPath (TEnemy aid True) $ NoPath

createPath :: MonadClient m => ActorId -> Target -> m TgtAndPath
createPath aid tapTgt = do
  b <- getsState $ getActorBody aid
  mpos <- aidTgtToPos aid (blid b) (Just tapTgt)
  case mpos of
    Nothing -> return TgtAndPath{tapTgt, tapPath=NoPath}
-- TODO: for now, an extra turn at target is needed, e.g., to pick up items
--  Just p | p == bpos b -> return Nothing
    Just p -> do
      tapPath <- getCachePath aid p
      return $! TgtAndPath{..}
