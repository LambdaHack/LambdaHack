{-# LANGUAGE DataKinds #-}
-- | Semantics of abilities in terms of actions and the AI procedure
-- for picking the best action for an actor.
module Game.LambdaHack.Client.AI.HandleAbilityClient
  ( actionStrategy
  ) where

import Control.Applicative
import Control.Arrow (second)
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
import Game.LambdaHack.Common.Ability
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

type ToAny a = Strategy (RequestTimed a) -> Strategy RequestAnyAbility

toAny :: ToAny a
toAny strat = RequestAnyAbility <$> strat

-- | AI strategy based on actor's sight, smell, intelligence, etc.
-- Never empty.
actionStrategy :: forall m. MonadClient m
               => ActorId -> m (Strategy RequestAnyAbility)
actionStrategy aid = do
  body <- getsState $ getActorBody aid
  condTgtEnemyPresent <- condTgtEnemyPresentM aid
  condAnyFoeAdj <- condAnyFoeAdjM aid
  threatDistL <- threatDistList aid
  condHpTooLow <- condHpTooLowM aid
  condOnTriggerable <- condOnTriggerableM aid
  condBlocksFriends <- condBlocksFriendsM aid
  condNoWeapon <- condNoWeaponM aid
  condFloorWeapon <- condFloorWeaponM aid
  condCanProject <- condCanProjectM aid
  condNotCalmEnough <- condNotCalmEnoughM aid
  condDesirableFloorItem <- condDesirableFloorItemM aid
  condMeleeBad <- condMeleeBadM aid
  fleeL <- fleeList aid
  let condThreatAdj = not $ null $ takeWhile ((<= 1) . fst) threatDistL
      condThreatAtHand = not $ null $ takeWhile ((<= 2) . fst) threatDistL
      condThreatNearby = not $ null $ takeWhile ((<= nearby) . fst) threatDistL
      condFastThreatAdj = any (\(_, (_, b)) -> bspeed b > bspeed body)
                          $ takeWhile ((<= 1) . fst) threatDistL
      condCanFlee = not (null fleeL || condFastThreatAdj)
  mleader <- getsClient _sleader
  actorAbs <- actorAbilities aid mleader
  let stratToFreq :: MonadStateRead m
                  => Int -> m (Strategy RequestAnyAbility)
                  -> m (Frequency RequestAnyAbility)
      stratToFreq scale mstrat = do
        st <- mstrat
        return $! scaleFreq scale $ bestVariant st  -- TODO: flatten instead?
      prefix, suffix :: [([Ability], m (Strategy RequestAnyAbility), Bool)]
      prefix =
        [ ( [AbApply], (toAny :: ToAny AbApply)
            <$> useTool aid True  -- use only healing tools
          , condHpTooLow && not condAnyFoeAdj
            && not condOnTriggerable )  -- don't block stairs, perhaps ascend
        , ( [AbTrigger], (toAny :: ToAny AbTrigger)
            <$> trigger aid True
              -- flee via stairs, even if to wrong level
              -- may return via different stairs
          , condOnTriggerable && (condNotCalmEnough || condHpTooLow)
            && condThreatNearby && not condTgtEnemyPresent )
        , ( [AbMove]
          , flee aid fleeL
          , condMeleeBad && condThreatAdj && condCanFlee )
        , ( [AbDisplace]
          , displaceFoe aid  -- only swap with an enemy to expose him
          , condBlocksFriends && condAnyFoeAdj
            && not condOnTriggerable && not condDesirableFloorItem )
        , ( [AbMoveItem], (toAny :: ToAny AbMoveItem)
            <$> pickup aid True
          , condNoWeapon && condFloorWeapon && not condHpTooLow )
        , ( [AbMelee], (toAny :: ToAny AbMelee)
            <$> meleeBlocker aid  -- only melee target or blocker
          , condAnyFoeAdj )
        , ( [AbTrigger], (toAny :: ToAny AbTrigger)
            <$> trigger aid False
          , condOnTriggerable && not condDesirableFloorItem )
        , ( [AbDisplace]  -- prevents some looping movement
          , displaceBlocker aid  -- fires up only when path blocked
          , not condDesirableFloorItem )
        , ( [AbMoveItem], (toAny :: ToAny AbMoveItem)
            <$> manageEqp aid  -- doesn't take long, very useful if safe
                               -- only if calm enough, so high priority
          , not condAnyFoeAdj && not condDesirableFloorItem ) ]
      distant :: [([Ability], m (Frequency RequestAnyAbility), Bool)]
      distant =
        [ ( [AbProject]  -- for high-value target, shoot even in melee
          , stratToFreq 5 $ (toAny :: ToAny AbProject)
            <$> ranged aid
          , condTgtEnemyPresent && condCanProject )
        , ( [AbApply]
          , stratToFreq 2 $ (toAny :: ToAny AbApply)
            <$> useTool aid False  -- use any tool
          , condTgtEnemyPresent || condThreatNearby )  -- can affect enemies
        , ( [AbMove]
          , stratToFreq (if condMeleeBad then 1 else 100)
            $ chase aid True
          , condTgtEnemyPresent && not condDesirableFloorItem ) ]
      suffix =
        [ ( [AbMoveItem], (toAny :: ToAny AbMoveItem)
            <$> pickup aid False
          , True )  -- unconditionally, e.g., to give to other party members
        , ( [AbMove]
          , flee aid fleeL
          , condMeleeBad && (condNotCalmEnough && condThreatNearby
                             || condThreatAtHand)
            && condCanFlee )
        , ( [AbMelee], (toAny :: ToAny AbMelee)
            <$> meleeAny aid  -- avoid getting damaged for naught
          , condAnyFoeAdj )
        , ( [AbMove]
          , chase aid False
          , True )
        , ( [AbWait], (toAny :: ToAny AbWait)
            <$> waitBlockNow
            -- Wait until friends sidestep; ensures strategy is never empty.
            -- TODO: try to switch leader away before that (we already
            -- switch him afterwards)
          , True ) ]
      -- TODO: don't msum not to evaluate until needed
      checkAction :: ([Ability], m a, Bool) -> Bool
      checkAction (abts, _, cond) = cond && all (`elem` actorAbs) abts
      sumS abAction = do
        let as = filter checkAction abAction
        strats <- sequence $ map (\(_, m, _) -> m) as
        return $! msum strats
      sumF abFreq = do
        let as = filter checkAction abFreq
        strats <- sequence $ map (\(_, m, _) -> m) as
        return $! msum strats
      combineDistant as = fmap liftFrequency $ sumF as
  sumPrefix <- sumS prefix
  comDistant <- combineDistant distant
  sumSuffix <- sumS suffix
  return $! sumPrefix .| comDistant .| sumSuffix

-- | A strategy to always just wait.
waitBlockNow :: MonadClient m => m (Strategy (RequestTimed AbWait))
waitBlockNow = return $! returN "wait" ReqWait

-- TODO: (most?) animals don't pick up. Everybody else does.
pickup :: MonadClient m
       => ActorId -> Bool -> m (Strategy (RequestTimed AbMoveItem))
pickup aid onlyWeapon = do
  Kind.COps{corule} <- getsState scops
  let RuleKind{ritemMelee} = Kind.stdRuleset corule
  benItemL <- benGroundItems aid
  let isWeapon (_, (_, item)) = jsymbol item `elem` ritemMelee
      filterWeapon | onlyWeapon = filter isWeapon
                   | otherwise = id
  case filterWeapon benItemL of
    ((_, k), (iid, _)) : _ -> do  -- pick up the best desirable item, if any
      updateItemSlot (Just aid) iid
      return $! returN "pickup" $ ReqMoveItem iid k CGround CEqp
    [] -> return reject

manageEqp :: MonadClient m => ActorId -> m (Strategy (RequestTimed AbMoveItem))
manageEqp aid = do
  cops@Kind.COps{coactor=Kind.Ops{okind}, corule} <- getsState scops
  let RuleKind{ritemEqp, rsharedInventory} = Kind.stdRuleset corule
  body <- getsState $ getActorBody aid
  invAssocs <- getsState $ getInvAssocs body
  let kind = okind $ bkind body
  if calmEnough body kind then do
    eqpKA <- getsState $ getEqpKA body
    let improve symbol =
          let bestInv = strongestItem invAssocs $ pSymbol cops symbol
              bestEqp = strongestItems eqpKA $ pSymbol cops symbol
          in case (bestInv, bestEqp) of
            (Just (_, (iidInv, _)), []) ->
              returN "wield" $ ReqMoveItem iidInv 1 CInv CEqp
            (Just (vInv, (iidInv, _)), (vEqp, _) : _)
              | vInv > vEqp ->
              returN "wield" $ ReqMoveItem iidInv 1 CInv CEqp
            (_, (_, (k, (iidEqp, _))) : _) | k > 1 && rsharedInventory ->
              -- To share the best items with others.
              returN "yield" $ ReqMoveItem iidEqp (k - 1) CEqp CInv
            (_, _ : (_, (k, (iidEqp, _))) : _) ->
              -- To make room in limited equipment store or to share.
              returN "yield" $ ReqMoveItem iidEqp k CEqp CInv
            _ -> reject
    return $ msum $ map improve ritemEqp
  else return reject

pSymbol :: Kind.COps -> Char -> Item -> Maybe Int
pSymbol cops c = case c of
  ')' -> pMelee cops
  '\"' -> pRegen
  '=' -> pStead
  _ -> \_ -> Just 0

-- Everybody melees in a pinch, even though some prefer ranged attacks.
meleeBlocker :: MonadClient m => ActorId -> m (Strategy (RequestTimed AbMelee))
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
            return $! returN "melee in the way" (ReqMelee aid2)
          else return reject
        Nothing -> return reject
    _ -> return reject  -- probably no path to the enemy, if any

-- Everybody melees in a pinch, even though some prefer ranged attacks.
meleeAny :: MonadClient m => ActorId -> m (Strategy (RequestTimed AbMelee))
meleeAny aid = do
  b <- getsState $ getActorBody aid
  fact <- getsState $ \s -> sfactionD s EM.! bfid b
  allFoes <- getsState $ actorRegularAssocs (isAtWar fact) (blid b)
  let adjFoes = filter (adjacent (bpos b) . bpos . snd) allFoes
      -- TODO: prioritize somehow
      freq = uniformFreq "melee adjacent" $ map (ReqMelee . fst) adjFoes
  return $ liftFrequency freq

-- Fast monsters don't pay enough attention to features.
trigger :: MonadClient m
        => ActorId -> Bool -> m (Strategy (RequestTimed AbTrigger))
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
             else let eben = case actorsThere of
                        [] | canSee -> expBenefit
                        _ -> min 1 expBenefit  -- risk pushing
                  in if fleeViaStairs
                     then 1000 * eben + 1  -- strongly prefer correct direction
                     else eben
        F.Cause ef@Effect.Escape{} ->  -- flee via this way, too
          -- Only heroes escape but they first explore all for high score.
          if not (isHero && allExplored) then 0 else effectToBenefit cops b ef
        F.Cause ef | not fleeViaStairs -> effectToBenefit cops b ef
        _ -> 0
      benFeat = zip (map ben feats) feats
  return $! liftFrequency $ toFreq "trigger"
         $ [ (benefit, ReqTrigger (Just feat))
           | (benefit, feat) <- benFeat
           , benefit > 0 ]

-- Actors require sight to use ranged combat and intelligence to throw
-- or zap anything else than obvious physical missiles.
ranged :: MonadClient m => ActorId -> m (Strategy (RequestTimed AbProject))
ranged aid = do
  Kind.COps{ coactor=Kind.Ops{okind}
           , coitem=coitem@Kind.Ops{okind=iokind}
           , corule
           } <- getsState scops
  btarget <- getsClient $ getTarget aid
  b@Actor{bkind, bpos, blid} <- getsState $ getActorBody aid
  mfpos <- aidTgtToPos aid blid btarget
  case (btarget, mfpos) of
    (Just TEnemy{}, Just fpos) -> do
      disco <- getsClient sdisco
      let mk = okind bkind
          initalEps = 0
      (steps, eps) <- makeLine b fpos initalEps
      let permitted = (if True  -- aiq mk >= 10 -- TODO; let server enforce?
                       then ritemProject
                       else ritemRanged)
                      $ Kind.stdRuleset corule
      benList <- benAvailableItems aid permitted
      let itemReaches item =
            let lingerPercent = isLingering coitem disco item
                toThrow = maybe 0 (itoThrow . iokind) $ jkind disco item
                speed = speedFromWeight (jweight item) toThrow
                range = rangeFromSpeed speed
                totalRange = lingerPercent * range `div` 100
            in steps <= totalRange  -- probably enough range
                 -- TODO: make sure itoThrow identified after a single throw
          fRanged ((mben, cstore), (iid, item)) =
            let benR = (if cstore == CGround then 2 else 1)
                       * case mben of
                           Nothing -> -5  -- experimenting is fun
                           Just ben -> ben
            in if benR < 0 && itemReaches item
               then Just (-benR, ReqProject fpos eps iid cstore)
               else Nothing
          benRanged = mapMaybe fRanged benList
          freq =
            if asight mk  -- ProjectBlind
               && calmEnough b mk  -- ProjectNotCalm
               -- ProjectAimOnself, ProjectBlockActor, ProjectBlockTerrain
               -- and no actors or obstracles along the path
               && steps == chessDist bpos fpos
            then toFreq "ranged" benRanged
            else toFreq "ranged: not possible" []
      return $! liftFrequency freq
    _ -> return reject

-- Tools use requires significant intelligence and sometimes literacy.
useTool :: MonadClient m
        => ActorId -> Bool -> m (Strategy (RequestTimed AbApply))
useTool aid onlyFirstAid = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  b <- getsState $ getActorBody aid
  let mk = okind $ bkind b
      permitted | aiq mk < 5 = ""
               | aiq mk < 10 = "!"
               | otherwise = "!?"  -- literacy required
  benList <- benAvailableItems aid permitted
  let itemLegal item | onlyFirstAid = case jeffect item of
                                        Effect.Heal p | p > 0 -> True
                                        _ -> False
                     | otherwise = True
      fTool ((mben, cstore), (iid, item)) =
        let benR = (if cstore == CGround then 2 else 1)
                   * case mben of
                       Nothing -> 5  -- experimenting is fun
                       Just ben -> ben
        in if benR > 0 && itemLegal item
           then Just (-benR, ReqApply iid cstore)
           else Nothing
      benTool = mapMaybe fTool benList
  return $! liftFrequency $ toFreq "useTool" benTool

-- If low on health or alone, flee in panic, close to the path to target
-- and as far from the attackers, as possible. Usually fleeing from
-- foes will lead towards friends, but we don't insist on that.
-- We use chess distances, not pathfinding, because melee can happen
-- at path distance 2.
flee :: MonadClient m
     => ActorId -> [(Int, Point)] -> m (Strategy RequestAnyAbility)
flee aid fleeL = do
  b <- getsState $ getActorBody aid
  let vVic = map (second (`vectorToFrom` bpos b)) fleeL
      str = liftFrequency $ toFreq "flee" vVic
  Traversable.mapM (moveOrRunAid True aid) str

displaceFoe :: MonadClient m => ActorId -> m (Strategy RequestAnyAbility)
displaceFoe aid = do
  cops <- getsState scops
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  let tgtPath = case mtgtMPath of  -- prefer displacing along the path to target
        Just (_, Just (_ : path, _)) -> path
        _ -> []
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  fact <- getsState $ \s -> sfactionD s EM.! bfid b
  allFoes <- getsState $ actorRegularList (isAtWar fact) (blid b)
  factionD <- getsState sfactionD
  let foeFactionD = EM.filter (flip isAtWar (bfid b)) factionD
      doSup (ffid, ffact) = do
        let friendlyFid fid = fid == ffid || isAllied ffact fid
        sup <- getsState $ actorRegularList friendlyFid (blid b)
        return (ffid, sup)
  sups <- fmap EM.fromAscList $ mapM doSup $ EM.assocs foeFactionD
  let accessibleHere = accessible cops lvl $ bpos b  -- DisplaceAccess
      displaceable body =  -- DisplaceAccess, DisplaceDying, DisplaceSupported
        accessibleHere (bpos body)
        && not (actorDying body)
        && all (not . adjacent (bpos body) . bpos) (sups EM.! bfid body)
      posFoes = map bpos $ filter displaceable allFoes
      adjFoes = filter (adjacent (bpos b)) posFoes
      pathFoes = filter (`elem` tgtPath) adjFoes
      dispFoes = if null pathFoes then adjFoes else pathFoes
      vFoes = map (`vectorToFrom` bpos b) dispFoes
      str = liftFrequency $ uniformFreq "displaceFoe" vFoes
  Traversable.mapM (moveOrRunAid True aid) str

displaceBlocker :: MonadClient m => ActorId -> m (Strategy RequestAnyAbility)
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
     && accessible cops lvl source target then do  -- DisplaceAccess
    mBlocker <- getsState $ posToActors target (blid b)
    case mBlocker of
      [] -> return reject
      [((aid2, b2), _)] -> do
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
          Nothing -> do
            tfact <- getsState $ (EM.! bfid b2) . sfactionD
            let friendlyFid fid = fid == bfid b2 || isAllied tfact fid
            sup <- getsState $ actorRegularList friendlyFid (blid b2)
            let displaceable =  -- DisplaceDying, DisplaceSupported
                  not (isAtWar tfact (bfid b))
                  || not (actorDying b2)
                     && all (not . adjacent (bpos b2) . bpos) sup
            if displaceable then
              return $! returN "displace other" $ target `vectorToFrom` source
            else return reject  -- DisplaceDying, DisplaceSupported
      _ -> return reject  -- DisplaceProjectiles
  else return reject

chase :: MonadClient m => ActorId -> Bool -> m (Strategy RequestAnyAbility)
chase aid doDisplace = do
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  str <- case mtgtMPath of
    Just (_, Just (p : q : _, (goal, _))) -> moveTowards aid p q goal
    _ -> return reject  -- goal reached
  -- If @doDisplace@: don't pick fights, assuming the target is more important.
  -- We'd normally melee the target earlier on via @AbMelee@, but for
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
             => Bool -> ActorId -> Vector -> m RequestAnyAbility
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
    [((target, b2), _)] | run ->  do -- can be a foe, as well as a friend
      tfact <- getsState $ (EM.! bfid b2) . sfactionD
      let friendlyFid fid = fid == bfid b2 || isAllied tfact fid
      sup <- getsState $ actorRegularList friendlyFid (blid b2)
      if boldpos sb /= tpos -- avoid trivial Displace loops
         && accessible cops lvl spos tpos -- DisplaceAccess
         && (not (isAtWar tfact (bfid sb))
                  || not (actorDying b2)  -- DisplaceDying
                     && all (not . adjacent (bpos b2) . bpos) sup)
                          -- DisplaceSupported
      then
        return $! RequestAnyAbility $ ReqDisplace target
      else
        -- If cannot displace, hit.
        return $! RequestAnyAbility $ ReqMelee target
    ((target, _), _) : _ ->  -- can be a foe, as well as a friend (e.g., proj.)
      -- No problem if there are many projectiles at the spot. We just
      -- attack the first one.
      -- Attacking does not require full access, adjacency is enough.
      return $! RequestAnyAbility $ ReqMelee target
    [] -> do  -- move or search or alter
      if accessible cops lvl spos tpos then
        -- Movement requires full access.
        return $! RequestAnyAbility $ ReqMove dir
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
        return $! RequestAnyAbility $ ReqAlter tpos Nothing
      else
        -- Boring tile, no point bumping into it, do WaitSer if really idle.
        assert `failure` "AI causes MoveNothing or AlterNothing"
               `twith` (run, source, dir)
