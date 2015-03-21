-- | Let AI pick the best target for an actor.
module Game.LambdaHack.Client.AI.PickTargetClient
  ( targetStrategy, createPath
  ) where

import Control.Applicative
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import Data.Maybe

import Game.LambdaHack.Client.AI.ConditionClient
import Game.LambdaHack.Client.AI.Preferences
import Game.LambdaHack.Client.AI.Strategy
import Game.LambdaHack.Client.Bfs
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
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind

-- | AI proposes possible targets for the actor. Never empty.
targetStrategy :: forall m. MonadClient m
               => ActorId -> m (Strategy (Target, Maybe PathEtc))
targetStrategy aid = do
  cops@Kind.COps{corule, cotile=cotile@Kind.Ops{ouniqGroup}} <- getsState scops
  let stdRuleset = Kind.stdRuleset corule
      nearby = rnearby stdRuleset
  itemToF <- itemToFullClient
  modifyClient $ \cli -> cli { sbfsD = invalidateBfs aid (sbfsD cli)
                             , seps = seps cli + 773 }  -- randomize paths
  b <- getsState $ getActorBody aid
  activeItems <- activeItemsClient aid
  lvl@Level{lxsize, lysize} <- getLevel $ blid b
  let stepAccesible mtgt@(Just (_, (p : q : _ : _, _))) = -- goal not adjacent
        if accessible cops lvl p q then mtgt else Nothing
      stepAccesible mtgt = mtgt  -- goal can be inaccessible, e.g., suspect
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  oldTgtUpdatedPath <- case mtgtMPath of
    Just (tgt, Nothing) ->
      -- This case is especially for TEnemyPos that would be lost otherwise.
      -- This is also triggered by @UpdLeadFaction@. The recreated path can be
      -- different than on the other client (AI or UI), but we don't care
      -- as long as the target stays the same at least for a moment.
      createPath aid tgt
    Just (tgt, Just path) -> do
      mvalidPos <- aidTgtToPos aid (blid b) (Just tgt)
      if isNothing mvalidPos then return Nothing  -- wrong level
      else return $! case path of
        (p : q : rest, (goal, len)) -> stepAccesible $
          if bpos b == p
          then Just (tgt, path)  -- no move last turn
          else if bpos b == q
               then Just (tgt, (q : rest, (goal, len - 1)))  -- step along path
               else Nothing  -- veered off the path
        ([p], (goal, _)) -> do
          let !_A = assert (p == goal `blame` (aid, b, mtgtMPath)) ()
          if bpos b == p then
            Just (tgt, path)  -- goal reached; stay there picking up items
          else
            Nothing  -- somebody pushed us off the goal; let's target again
        ([], _) -> assert `failure` (aid, b, mtgtMPath)
    Nothing -> return Nothing  -- no target assigned yet
  let !_A = assert (not $ bproj b) ()  -- would work, but is probably a bug
  fact <- getsState $ (EM.! bfid b) . sfactionD
  allFoes <- getsState $ actorRegularAssocs (isAtWar fact) (blid b)
  dungeon <- getsState sdungeon
  -- We assume the actor eventually becomes a leader (or has the same
  -- set of abilities as the leader, anyway) and set his target accordingly.
  let actorMaxSk = sumSkills activeItems
  actorMinSk <- getsState $ actorSkills Nothing aid activeItems
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
  allAssocs <- fullAssocsClient aid [COrgan, CEqp]
  let condNoUsableWeapon = all (not . isMelee . snd) allAssocs
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
        activeItemsE <- activeItemsClient aidE
        let actorMaxSkE = sumSkills activeItemsE
            attacksFriends = any (adjacent (bpos body) . bpos) friends
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
  let unknownId = ouniqGroup "unknown space"
      itemUsefulness itemFull =
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
        let axtorSk = if (fst <$> gleader fact) == Just aid
                      then actorMaxSk
                      else actorMinSk
        in EM.findWithDefault 0 AbMove axtorSk > 0
      isStuck = waitedLastTurn b && couldMoveLastTurn
      slackTactic = ftactic (gplayer fact) `elem` [TBlock, TRoam, TPatrol]
      setPath :: Target -> m (Strategy (Target, Maybe PathEtc))
      setPath tgt = do
        mpath <- createPath aid tgt
        let take5 (_, pgl@(path, (goal, _))) =
              if slackTactic then
                -- Best path only followed 5 moves; then straight on.
                let path5 = take 5 path
                    v = towards (bpos b) goal  -- only for UI
                in (TVector v, Just (path5, (last path5, length path5 - 1)))
              else (tgt, Just pgl)
        return $! returN "setPath" $ maybe (tgt, Nothing) take5 mpath
      pickNewTarget :: m (Strategy (Target, Maybe PathEtc))
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
                let vToTgt v = do
                      -- Items and smells, etc. considered every 7 moves.
                      -- Thanks to sentinels, @path@ is never null.
                      let path = nub $
                            bpos b : trajectoryToPathBounded
                                       lxsize lysize (bpos b) (replicate 7 v)
                      return $! returN "tgt with no exploration"
                        ( TVector v
                        , if length path == 1
                          then Nothing
                          else Just (path, (last path, length path - 1)) )
                    vOld = bpos b `vectorToFrom` boldpos b
                    pNew = shiftBounded lxsize lysize (bpos b) vOld
                if slackTactic && not isStuck && bpos b /= pNew
                   && accessible cops lvl (bpos b) pNew
                then vToTgt vOld
                else do
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
                                      [] -> do
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
        let f (tgt, _) = case tgt of
              TEnemyPos _ lid p _ -> p /= pos || lid /= blid b
              _ -> True
        modifyClient $ \cli -> cli {stargetD = EM.filter f (stargetD cli)}
        pickNewTarget
      updateTgt :: Target -> PathEtc
                -> m (Strategy (Target, Maybe PathEtc))
      updateTgt oldTgt updatedPath@(_, (_, len)) = case oldTgt of
        TEnemy a permit -> do
          body <- getsState $ getActorBody a
          if not focused  -- prefers closer foes
             && a `notElem` map fst nearbyFoes  -- old one not close enough
             || blid body /= blid b  -- wrong level
             || actorDying body  -- foe already dying
             || permit  -- never follow a friend more than 1 step
          then pickNewTarget
          else if bpos body == fst (snd updatedPath)
               then return $! returN "TEnemy" (oldTgt, Just updatedPath)
                      -- The enemy didn't move since the target acquired.
                      -- If any walls were added that make the enemy
                      -- unreachable, AI learns that the hard way,
                      -- as soon as it bumps into them.
               else do
                 let p = bpos body
                 (bfs, mpath) <- getCacheBfsAndPath aid p
                 case mpath of
                   Nothing -> pickNewTarget  -- enemy became unreachable
                   Just path ->
                      return $! returN "TEnemy"
                        (oldTgt, Just ( bpos b : path
                                      , (p, fromMaybe (assert `failure` mpath)
                                            $ accessBfs bfs p) ))
        TEnemyPos _ lid p permit
          -- Chase last position even if foe hides or dies,
          -- to find his companions, loot, etc.
          | lid /= blid b  -- wrong level
            || chessDist (bpos b) p >= nearby  -- too far and not visible
            || permit  -- never follow a friend more than 1 step
            -> pickNewTarget
          | p == bpos b -> tellOthersNothingHere p
          | otherwise ->
              return $! returN "TEnemyPos" (oldTgt, Just updatedPath)
        _ | not $ null nearbyFoes ->
          pickNewTarget  -- prefer close foes to anything
        TPoint lid pos -> do
          bag <- getsState $ getCBag $ CFloor lid pos
          if lid /= blid b  -- wrong level
             -- Below we check the target could not be picked again in
             -- pickNewTarget, and only in this case it is invalidated.
             -- This ensures targets are eventually reached (unless a foe
             -- shows up) and not changed all the time mid-route
             -- to equally interesting, but perhaps a bit closer targets,
             -- most probably already targeted by other actors.
             || (EM.findWithDefault 0 AbMoveItem
                                    actorMaxSk <= 0  -- closestItems
                 || not (desirableBag bag))
                && (not canSmell  -- closestSmell
                    || pos == bpos b  -- in case server resends deleted smell
                    || let sml = EM.findWithDefault timeZero pos (lsmell lvl)
                       in sml <= ltime lvl)
                && let t = lvl `at` pos
                   in if not lidExplored
                      then t /= unknownId  -- closestUnknown
                           && not (Tile.isSuspect cotile t)  -- closestSuspect
                           && not (condEnoughGear && (Tile.isStair cotile t))
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
                               || pos == bpos b
                               || not (Tile.isStair cotile t))
                           -- The remaining case is furthestKnown. This is
                           -- always an unimportant target, so we forget it
                           -- if the actor is stuck (waits, though could move;
                           -- or has zeroed individual moving skill,
                           -- but then should change targets often anyway).
                           && (pos == bpos b
                               || isStuck
                               || not allExplored)
          then pickNewTarget
          else return $! returN "TPoint" (oldTgt, Just updatedPath)
        TVector{} | len > 1 ->
          return $! returN "TVector" (oldTgt, Just updatedPath)
        TVector{} -> pickNewTarget
  case oldTgtUpdatedPath of
    Just (oldTgt, updatedPath) -> updateTgt oldTgt updatedPath
    Nothing -> pickNewTarget

createPath :: MonadClient m
           => ActorId -> Target -> m (Maybe (Target, PathEtc))
createPath aid tgt = do
  b <- getsState $ getActorBody aid
  mpos <- aidTgtToPos aid (blid b) (Just tgt)
  case mpos of
    Nothing -> return Nothing
    Just p | p == bpos b -> return Nothing
    Just p -> do
      (bfs, mpath) <- getCacheBfsAndPath aid p
      return $! case mpath of
        Nothing -> Nothing
        Just path -> Just (tgt, ( bpos b : path
                                , (p, fromMaybe (assert `failure` mpath)
                                      $ accessBfs bfs p) ))
