-- | Semantics of abilities in terms of actions and the AI procedure
-- for picking the best action for an actor.
module Game.LambdaHack.Client.AI.HandleAbilityClient
  ( actionStrategy
  ) where

import Control.Arrow (second, (&&&))
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Traversable as Traversable

import Game.LambdaHack.Client.AI.ConditionClient
import Game.LambdaHack.Client.AI.Preferences
import Game.LambdaHack.Client.AI.Strategy
import Game.LambdaHack.Client.BfsClient
import Game.LambdaHack.Client.CommonClient
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Ability (Ability)
import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind as TileKind

-- | AI strategy based on actor's sight, smell, intelligence, etc.
-- Never empty.
actionStrategy :: forall m. MonadClient m
               => ActorId -> m (Strategy RequestTimed)
actionStrategy aid = do
  condTgtEnemyPresent <- condTgtEnemyPresentM aid
  condAnyFoeAdjacent <- condAnyFoeAdjacentM aid
  condHpTooLow <- condHpTooLowM aid
  condOnTriggerable <- condOnTriggerableM aid
  condEnemiesClose <- condEnemiesCloseM aid
  condNoFriends <- condNoFriendsM aid
  condBlocksFriends <- condBlocksFriendsM aid
  condNoWeapon <- condNoWeaponM aid
  condWeaponAvailable <- condWeaponAvailableM aid
  mleader <- getsClient _sleader
  actorAbs <- actorAbilities aid mleader
  let stratToFreq :: MonadStateRead m
                  => Int -> m (Strategy RequestTimed)
                  -> m (Frequency RequestTimed)
      stratToFreq scale mstrat = do
        st <- mstrat
        return $! scaleFreq scale $ bestVariant st  -- TODO: flatten instead?
      prefix, suffix :: [([Ability], Bool, m (Strategy RequestTimed))]
      prefix =
        [ ( [Ability.FirstAid, Ability.UseTool]
          , not condAnyFoeAdjacent && condHpTooLow
          , useTool aid True )  -- use only healing tools
        , ( [Ability.Trigger, Ability.Flee]
          , condOnTriggerable && condHpTooLow && condEnemiesClose
          , trigger aid True ) -- flee via stairs, even if to wrong level
        , ( [Ability.Flee]
          , condAnyFoeAdjacent && condHpTooLow && condNoFriends
          , flee aid )
        , ( [Ability.Displace, Ability.Melee]
          , condAnyFoeAdjacent && condBlocksFriends
          , displaceFoe aid )  -- only swap with an enemy to expose him
        , ( [Ability.Pickup, Ability.Melee]
          , condNoWeapon && condWeaponAvailable
          , pickup aid True )
        , ( [Ability.Melee]
          , condAnyFoeAdjacent
          , meleeBlocker aid )  -- only melee target or blocker
        , ( [Ability.Pickup]
          , True  -- unconditionally, e.g., to give to other party members
          , pickup aid False )
        , ( [Ability.Trigger]
          , condOnTriggerable
          , trigger aid False )
        , ( [Ability.Displace, Ability.Chase]
          , True  -- prevents some looping movement
          , displaceBlocker aid ) ]  -- fires up only when path blocked
      distant :: [([Ability], Bool, m (Frequency RequestTimed))]
      distant =
        [ ( [Ability.Ranged]
          , condTgtEnemyPresent  -- for high-value target, shoot even in melee
          , stratToFreq 2 (ranged aid) )
        , ( [Ability.UseTool]
          , condTgtEnemyPresent  -- tools can affect the target enemy
          , stratToFreq 1 (useTool aid False) )  -- use any tool
        , ( [Ability.Chase]  -- careful, even if no Ability.Flee
          , condTgtEnemyPresent && not condHpTooLow && not condNoFriends
          , stratToFreq 20 (chase aid True) ) ]
      suffix =
        [ ( [Ability.Flee]
          , condHpTooLow && condNoFriends && condEnemiesClose
          , flee aid )  -- we assume fleeing usually gets us closer to friends
        , ( [Ability.Melee]
          , condAnyFoeAdjacent
          , meleeAny aid )  -- melee any, to avoid being wounded for naught
        , ( [Ability.Wander]
          , True
          , chase aid False ) ]
      -- TODO: don't msum not to evaluate until needed
      checkAction :: ([Ability], Bool, m a) -> Bool
      checkAction (abts, cond, _) = cond && all (`elem` actorAbs) abts
      sumS abAction = do
        let as = filter checkAction abAction
        strats <- sequence $ map (\(_, _, m) -> m) as
        return $! msum strats
      sumF abFreq = do
        let as = filter checkAction abFreq
        strats <- sequence $ map (\(_, _, m) -> m) as
        return $! msum strats
      combineDistant as = fmap liftFrequency $ sumF as
  sumPrefix <- sumS prefix
  comDistant <- combineDistant distant
  sumSuffix <- sumS suffix
  return $! sumPrefix .| comDistant .| sumSuffix
            -- Wait until friends sidestep; ensures strategy is never empty.
            -- TODO: try to switch leader away before that (we already
            -- switch him afterwards)
            .| waitBlockNow aid

-- | A strategy to always just wait.
waitBlockNow :: ActorId -> Strategy RequestTimed
waitBlockNow aid = returN "wait" $ ReqWait aid

-- TODO: (most?) animals don't pick up. Everybody else does.
-- TODO: pick up best weapons first
pickup :: MonadClient m => ActorId -> Bool -> m (Strategy RequestTimed)
pickup aid onlyWeapon = do
  cops@Kind.COps{coactor=Kind.Ops{okind}, corule} <- getsState scops
  body <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid body) . sfactionD
  dungeon <- getsState sdungeon
  itemD <- getsState sitemD
  disco <- getsClient sdisco
  floorItems <- getsState $ getActorBag aid CGround
  invAssocs <- getsState $ getInvAssocs body
  let fightsAgainstSpawners =
        let escape = any lescape $ EM.elems dungeon
            isSpawner = isSpawnFact fact
        in escape && not isSpawner
      itemUsefulness item =
        case jkind disco item of
          Nothing -> 5  -- experimenting is fun
          Just _ki -> effectToBenefit cops body $ jeffect item
      desirableItem item k | fightsAgainstSpawners = itemUsefulness item /= 0
                                                     || itemPrice (item, k) > 0
                           | otherwise = itemUsefulness item /= 0
      mapDesirable (iid, k) = let item = itemD EM.! iid
                              in if desirableItem item k
                                 then Just (iid, k)
                                 else Nothing
      kind = okind $ bkind body
  case mapMaybe mapDesirable $ EM.assocs floorItems of
    (iid, k) : _ -> do  -- pick up first desirable item, if any
      updateItemSlot (Just aid) iid
      return $! returN "pickup" $ ReqMoveItem aid iid k CGround CEqp
    [] | calmEnough body kind -> do
      let RuleKind{ritemEqp, ritemMelee, rsharedInventory} =
            Kind.stdRuleset corule
      eqpKA <- getsState $ getEqpKA body
      let improve symbol =
            let bestInv = strongestItem invAssocs $ pSymbol cops symbol
                bestEqp = strongestItems eqpKA $ pSymbol cops symbol
            in case (bestInv, bestEqp) of
              (Just (_, (iidInv, _)), []) ->
                returN "wield" $ ReqMoveItem aid iidInv 1 CInv CEqp
              (Just (vInv, (iidInv, _)), (vEqp, _) : _)
                | vInv > vEqp ->
                returN "wield" $ ReqMoveItem aid iidInv 1 CInv CEqp
              (_, (_, (k, (iidEqp, _))) : _) | k > 1 && rsharedInventory ->
                -- To share the best items with others.
                returN "yield" $ ReqMoveItem aid iidEqp (k - 1) CEqp CInv
              (_, _ : (_, (k, (iidEqp, _))) : _) ->
                -- To make room in limited equipment store or to share.
                returN "yield" $ ReqMoveItem aid iidEqp k CEqp CInv
              _ -> reject
      return $ msum $ map improve (if onlyWeapon then ritemMelee else ritemEqp)
    _ -> return reject

pSymbol :: Kind.COps -> Char -> Item -> Maybe Int
pSymbol cops c = case c of
  ')' -> pMelee cops
  '\"' -> pRegen
  '=' -> pStead
  _ -> \_ -> Just 0

-- Everybody melees in a pinch, even though some prefer ranged attacks.
meleeBlocker :: MonadClient m => ActorId -> m (Strategy RequestTimed)
meleeBlocker aid = do
  b <- getsState $ getActorBody aid
  fact <- getsState $ \s -> sfactionD s EM.! bfid b
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  case mtgtMPath of
    Just (_, Just (_ : q : _, (goal, _))) -> do
      -- We prefer the goal (e.g., when no accessible, but adjacent),
      -- but accept @q@ even if it's only a blocking enemy position.
      let maim = if adjacent (bpos b) goal then Just goal
                 else if adjacent (bpos b) q then Just q
                 else Nothing  -- MeleeDistant
      mBlocker <- case maim of
        Nothing -> return Nothing
        Just aim -> getsState $ posToActor aim (blid b)
      case mBlocker of
        Just ((aid2, _), _) -> do
          -- No problem if there are many projectiles at the spot. We just
          -- attack the first one.
          body2 <- getsState $ getActorBody aid2
          if isAtWar fact (bfid body2) && not (actorDying body2) then
            return $! returN "melee in the way" (ReqMelee aid aid2)
          else return reject
        Nothing -> return reject
    _ -> return reject  -- probably no path to the enemy, if any

-- Everybody melees in a pinch, even though some prefer ranged attacks.
meleeAny :: MonadClient m => ActorId -> m (Strategy RequestTimed)
meleeAny aid = do
  b <- getsState $ getActorBody aid
  fact <- getsState $ \s -> sfactionD s EM.! bfid b
  allFoes <- getsState $ filter (not . actorDying . snd)
                         . actorNotProjAssocs (isAtWar fact) (blid b)
  let adjFoes = filter (adjacent (bpos b) . bpos . snd) allFoes
      -- TODO: prioritize somehow
      freq = uniformFreq "melee adjacent" $ map (ReqMelee aid . fst) adjFoes
  return $ liftFrequency freq

-- Fast monsters don't pay enough attention to features.
trigger :: MonadClient m => ActorId -> Bool -> m (Strategy RequestTimed)
trigger aid fleeViaStairs = do
  cops@Kind.COps{cotile=Kind.Ops{okind}} <- getsState scops
  dungeon <- getsState sdungeon
  explored <- getsClient sexplored
  b <- getsState $ getActorBody aid
  fact <- getsState $ \s -> sfactionD s EM.! bfid b
  lvl <- getLevel $ blid b
  unexploredD <- unexploredDepth
  s <- getState
  per <- getPerFid $ blid b
  let canSee = ES.member (bpos b) (totalVisible per)
      unexploredCurrent = ES.notMember (blid b) explored
      allExplored = ES.size explored == EM.size dungeon
      isHero = isHeroFact cops fact
      t = lvl `at` bpos b
      feats = TileKind.tfeature $ okind t
      ben feat = case feat of
        F.Cause (Effect.Ascend k) ->  -- change levels sensibly, in teams
          let expBenefit =
                if unexploredCurrent
                then 0  -- don't leave the level until explored
                else if unexploredD (signum k) (blid b)
                then 1000
                else if unexploredD (- signum k) (blid b)
                then 0  -- wait for stairs in the opposite direciton
                else if lescape lvl
                then 0  -- all explored, stay on the escape level
                else 2  -- no escape anywhere, switch levels occasionally
              (lid2, pos2) = whereTo (blid b) (bpos b) k dungeon
              actorsThere = posToActors pos2 lid2 s
          in if boldpos b == bpos b   -- probably used stairs last turn
                && boldlid b == lid2  -- in the opposite direction
             then 0  -- avoid trivial loops (pushing, being pushed, etc.)
             else
               if fleeViaStairs then 1 else 0
               + case actorsThere of
                   [] | canSee -> expBenefit
                   _ -> min 1 expBenefit  -- risk pushing, if no better option
        F.Cause ef@Effect.Escape{} ->  -- flee via this way, too
          -- Only heroes escape but they first explore all for high score.
          if not (isHero && allExplored) then 0 else effectToBenefit cops b ef
        F.Cause ef | not fleeViaStairs -> effectToBenefit cops b ef
        _ -> 0
      benFeat = zip (map ben feats) feats
  return $! liftFrequency $ toFreq "trigger"
         $ [ (benefit, ReqTrigger aid (Just feat))
           | (benefit, feat) <- benFeat
           , benefit > 0 ]

-- Actors require sight to use ranged combat and intelligence to throw
-- or zap anything else than obvious physical missiles.
ranged :: MonadClient m => ActorId -> m (Strategy RequestTimed)
ranged aid = do
  cops@Kind.COps{ coactor=Kind.Ops{okind}
                , coitem=coitem@Kind.Ops{okind=iokind}
                , corule
                } <- getsState scops
  btarget <- getsClient $ getTarget aid
  b@Actor{bkind, bpos, blid} <- getsState $ getActorBody aid
  floorItems <- getsState $ getActorBag aid CGround
  let eqpBag = beqp b
  mfpos <- aidTgtToPos aid blid btarget
  case (btarget, mfpos) of
    (Just TEnemy{}, Just fpos) -> do
      disco <- getsClient sdisco
      itemD <- getsState sitemD
      let mk = okind bkind
          initalEps = 0
      (steps, eps) <- makeLine b fpos initalEps
      let permitted = (if aiq mk >= 10 then ritemProject else ritemRanged)
                      $ Kind.stdRuleset corule
          itemReaches item =
            let lingerPercent = isLingering coitem disco item
                toThrow = maybe 0 (itoThrow . iokind) $ jkind disco item
                speed = speedFromWeight (jweight item) toThrow
                range = rangeFromSpeed speed
                totalRange = lingerPercent * range `div` 100
            in steps <= totalRange  -- probably enough range
                 -- TODO: make sure itoThrow identified after a single throw
          getItemB iid =
            fromMaybe (assert `failure` "item body not found"
                              `twith` (iid, itemD)) $ EM.lookup iid itemD
          throwFreq bag multi container =
            [ (- benefit * multi,
              ReqProject aid fpos eps iid container)
            | (iid, i) <- map (\iid -> (iid, getItemB iid))
                          $ EM.keys bag
            , let benefit =
                    case jkind disco i of
                      Nothing -> 5  -- experimenting is fun
                      Just _ki ->
                        let _kik = iokind _ki
                            _unneeded = isymbol _kik
                        in effectToBenefit cops b (jeffect i)
            , benefit < 0
            , jsymbol i `elem` permitted
            , itemReaches i ]
          freq =
            if asight mk  -- ProjectBlind
               && calmEnough b mk  -- ProjectNotCalm
               -- ProjectAimOnself, ProjectBlockActor, ProjectBlockTerrain
               -- and no actors or obstracles along the path
               && steps == chessDist bpos fpos
            then toFreq "ranged"
                 $ throwFreq eqpBag 1 CEqp
                   ++ throwFreq floorItems 2 CGround
            else toFreq "ranged: not possible" []
      return $! liftFrequency freq
    _ -> return reject

-- Tools use requires significant intelligence and sometimes literacy.
useTool :: MonadClient m => ActorId -> Bool -> m (Strategy RequestTimed)
useTool aid onlyFirstAid = do
  cops@Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  disco <- getsClient sdisco
  b <- getsState $ getActorBody aid
  floorItems <- getsState $ getActorBag aid CGround
  let eqpBag = beqp b
  s <- getState
  let mk = okind $ bkind b
      mastered | aiq mk < 5 = ""
               | aiq mk < 10 = "!"
               | otherwise = "!?"  -- literacy required
      legal item | onlyFirstAid = case jeffect item of
                                    Effect.Heal p | p > 0 -> True
                                    _ -> False
                 | otherwise = True
      useFreq bag multi container =
        [ (benefit * multi, ReqApply aid iid container)
        | (iid, i) <- map (\iid -> (iid, getItemBody iid s))
                      $ EM.keys bag
        , let benefit =
                case jkind disco i of
                  Nothing -> 5  -- experimenting is fun
                  Just _ki | legal i -> effectToBenefit cops b $ jeffect i
                  Just _ -> 0
        , benefit > 0
        , jsymbol i `elem` mastered ]
  return $! liftFrequency $ toFreq "useTool"
         $ useFreq eqpBag 1 CEqp
           ++ useFreq floorItems 2 CGround

-- If low on health, flee in panic --- not along path to target,
-- but just as far from the attackers, as possible. Usually fleeing from
-- foes will lead towards friends, but we don't insist on that.
flee :: MonadClient m => ActorId -> m (Strategy RequestTimed)
flee aid = do
  cops <- getsState scops
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  let tgtPath = case mtgtMPath of  -- prefer displacing along the path to target
        Just (_, Just (path, _)) -> path
        _ -> []
  b <- getsState $ getActorBody aid
  fact <- getsState $ \s -> sfactionD s EM.! bfid b
  allFoes <- getsState $ filter (not . actorDying)
                         . actorNotProjList (isAtWar fact) (blid b)
  lvl@Level{lxsize, lysize} <- getLevel $ blid b
  let posFoes = map bpos allFoes
      accessibleHere = accessible cops lvl $ bpos b
      myVic = vicinity lxsize lysize $ bpos b
      dist p | null posFoes = assert `failure` b
             | otherwise = minimum $ map (chessDist p) posFoes
      dVic = map (dist &&& id) myVic
      -- If can't flee, at least hit. Access not needed for melee.
      hitVic = filter ((== 0) . fst) dVic
      -- Flee, if possible. Access required.
      accVic = filter (accessibleHere . snd) $ dVic
      gtVic = filter ((> dist (bpos b)) . fst) accVic
      -- At least don't get closer to enemies.
      eqVic = filter ((== dist (bpos b)) . fst) accVic
      goodVic = if not $ null gtVic then gtVic
                else if not $ null hitVic then hitVic
                else eqVic
      rewardPath (d, p) = if p `elem` tgtPath then (2 * d - 1, p) else (d, p)
      pathVic = map rewardPath goodVic
      vVic = map (second (`vectorToFrom` bpos b)) pathVic
      str = liftFrequency $ toFreq "flee" vVic
  Traversable.mapM (moveOrRunAid True aid) str

displaceFoe :: MonadClient m => ActorId -> m (Strategy RequestTimed)
displaceFoe aid = do
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  let tgtPath = case mtgtMPath of  -- prefer displacing along the path to target
        Just (_, Just (path, _)) -> path
        _ -> []
  b <- getsState $ getActorBody aid
  fact <- getsState $ \s -> sfactionD s EM.! bfid b
  allFoes <- getsState $ filter (not . actorDying)
                         . actorNotProjList (isAtWar fact) (blid b)
  let posFoes = map bpos allFoes
      adjFoes = filter (adjacent (bpos b)) posFoes
      pathFoes = filter (`elem` tgtPath) adjFoes
      dispFoes = if null pathFoes then adjFoes else pathFoes
      vFoes = map (`vectorToFrom` bpos b) dispFoes
      str = liftFrequency $ uniformFreq "displaceFoe" vFoes
  Traversable.mapM (moveOrRunAid True aid) str

displaceBlocker :: MonadClient m => ActorId -> m (Strategy RequestTimed)
displaceBlocker aid = do
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  str <- case mtgtMPath of
    Just (_, Just (p : q : _, _)) -> displaceTowards aid p q
    _ -> return reject  -- goal reached
  Traversable.mapM (moveOrRunAid True aid) str

-- TODO: perhaps modify target when actually moving, not when
-- producing the strategy, even if it's a unique choice in this case.
displaceTowards :: MonadClient m
                => ActorId -> Point -> Point -> m (Strategy Vector)
displaceTowards aid source target = do
  cops <- getsState scops
  b <- getsState $ getActorBody aid
  assert (source == bpos b && adjacent source target) skip
  lvl <- getLevel $ blid b
  if boldpos b /= target -- avoid trivial loops
     && accessible cops lvl source target then do
    mBlocker <- getsState $ posToActors target (blid b)
    case mBlocker of
      [] -> return reject
      [((aid2, _), _)] -> do
        mtgtMPath <- getsClient $ EM.lookup aid2 . stargetD
        case mtgtMPath of
          Just (tgt, Just (p : q : rest, (goal, len)))
            | q == source && p == target -> do
              let newTgt = Just (tgt, Just (q : rest, (goal, len - 1)))
              modifyClient $ \cli ->
                cli {stargetD = EM.alter (const $ newTgt) aid (stargetD cli)}
              return $! returN "displace friend"
                     $ target `vectorToFrom` source
          Just _ -> return reject
          Nothing ->
            return $! returN "displace other"
                   $ target `vectorToFrom` source
      _ -> return reject  -- many projectiles, can't displace
  else return reject

chase :: MonadClient m => ActorId -> Bool -> m (Strategy RequestTimed)
chase aid doDisplace = do
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  str <- case mtgtMPath of
    Just (_, Just (p : q : _, (goal, _))) -> moveTowards aid p q goal
    _ -> return reject  -- goal reached
  -- If @doDisplace@: don't pick fights, assuming the target is more important.
  -- We'd normally melee the target earlier on via @Ability.Melee@, but for
  -- actors that don't have this ability (and so melee only when forced to),
  -- this is meaningul.
  Traversable.mapM (moveOrRunAid doDisplace aid) str

moveTowards :: MonadClient m
            => ActorId -> Point -> Point -> Point -> m (Strategy Vector)
moveTowards aid source target goal = do
  cops@Kind.COps{coactor=Kind.Ops{okind}, cotile} <- getsState scops
  b <- getsState $ getActorBody aid
  assert (source == bpos b && adjacent source target) skip
  lvl <- getLevel $ blid b
  fact <- getsState $ (EM.! bfid b) . sfactionD
  friends <- getsState $ actorList (not . isAtWar fact) $ blid b
  let _mk = okind $ bkind b
      noFriends = unoccupied friends
      -- Was:
      -- noFriends | asight mk = unoccupied friends
      --           | otherwise = const True
      -- but this should be implemented on the server or, if not,
      -- restricted to AI-only factions (e.g., animals).
      -- Otherwise human players are tempted to tweak their AI clients
      -- (as soon as we let them register their AI clients with the server).
      accessibleHere = accessible cops lvl source
      bumpableHere p =
        let t = lvl `at` p
        in Tile.isOpenable cotile t || Tile.isSuspect cotile t
      enterableHere p = accessibleHere p || bumpableHere p
  if noFriends target && enterableHere target then
    return $! returN "moveTowards adjacent" $ target `vectorToFrom` source
  else do
    let goesBack v = v == boldpos b `vectorToFrom` source
        nonincreasing p = chessDist source goal >= chessDist p goal
        isSensible p = nonincreasing p && noFriends p && enterableHere p
        sensible = [ ((goesBack v, chessDist p goal), v)
                   | v <- moves, let p = source `shift` v, isSensible p ]
        sorted = sortBy (comparing fst) sensible
        groups = map (map snd) $ groupBy ((==) `on` fst) sorted
        freqs = map (liftFrequency . uniformFreq "moveTowards") groups
    return $! foldr (.|) reject freqs

-- | Actor moves or searches or alters or attacks. Displaces if @run@.
-- This function is very general, even though it's often used in contexts
-- when only one or two of the many cases can possibly occur.
moveOrRunAid :: MonadStateRead m
             => Bool -> ActorId -> Vector -> m RequestTimed
moveOrRunAid run source dir = do
  cops@Kind.COps{cotile} <- getsState scops
  sb <- getsState $ getActorBody source
  let lid = blid sb
  lvl <- getLevel lid
  let spos = bpos sb           -- source position
      tpos = spos `shift` dir  -- target position
      t = lvl `at` tpos
  -- We start by checking actors at the the target position,
  -- which gives a partial information (actors can be invisible),
  -- as opposed to accessibility (and items) which are always accurate
  -- (tiles can't be invisible).
  tgts <- getsState $ posToActors tpos lid
  case tgts of
    [((target, _), _)] | run ->  -- can be a foe, as well as a friend
      if accessible cops lvl spos tpos
         && boldpos sb /= tpos -- avoid trivial Displace loops
      then
        -- Displacing requires accessibility.
        return $! ReqDisplace source target
      else
        -- If cannot displace, hit. No DisplaceAccess failure.
        return $! ReqMelee source target
    ((target, _), _) : _ ->  -- can be a foe, as well as a friend (e.g., proj.)
      -- No problem if there are many projectiles at the spot. We just
      -- attack the first one.
      -- Attacking does not require full access, adjacency is enough.
      return $! ReqMelee source target
    [] -> do  -- move or search or alter
      if accessible cops lvl spos tpos then
        -- Movement requires full access.
        return $! ReqMove source dir
        -- The potential invisible actor is hit.
      else if not $ EM.null $ lvl `atI` tpos then
        -- This is, e.g., inaccessible open door with an item in it.
        assert `failure` "AI causes AlterBlockItem" `twith` (run, source, dir)
      else if not (Tile.isWalkable cotile t)  -- not implied
              && (Tile.isSuspect cotile t
                  || Tile.isOpenable cotile t
                  || Tile.isClosable cotile t
                  || Tile.isChangeable cotile t) then
        -- No access, so search and/or alter the tile.
        return $! ReqAlter source tpos Nothing
      else
        -- Boring tile, no point bumping into it, do WaitSer if really idle.
        assert `failure` "AI causes MoveNothing or AlterNothing"
               `twith` (run, source, dir)
