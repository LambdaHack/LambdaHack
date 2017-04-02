-- | Semantics of abilities in terms of actions and the AI procedure
-- for picking the best action for an actor.
module Game.LambdaHack.Client.AI.ConditionM
  ( condAimEnemyPresentM
  , condAimEnemyRememberedM
  , condTgtNonmovingM
  , condAnyFoeAdjM
  , condNonProjFoeAdjM
  , condInMeleeM
  , condHpTooLowM
  , condAdjTriggerableM
  , condBlocksFriendsM
  , condFloorWeaponM
  , condNoEqpWeaponM
  , condCanProjectM
  , condNotCalmEnoughM
  , condDesirableFloorItemM
  , condMeleeBadM
  , condShineWouldBetrayM
  , benAvailableItems
  , hinders
  , benGroundItems
  , desirableItem
  , threatDistList
  , fleeList
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import Data.Ord

import Game.LambdaHack.Client.AI.Preferences
import Game.LambdaHack.Client.Bfs
import Game.LambdaHack.Client.CommonM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind

-- | Require that the target enemy is visible by the party.
condAimEnemyPresentM :: MonadClient m => ActorId -> m Bool
condAimEnemyPresentM aid = do
  btarget <- getsClient $ getTarget aid
  return $! case btarget of
    Just (TEnemy _ permit) -> not permit
    _ -> False

-- | Require that the target enemy is remembered on the actor's level.
condAimEnemyRememberedM :: MonadClient m => ActorId -> m Bool
condAimEnemyRememberedM aid = do
  b <- getsState $ getActorBody aid
  btarget <- getsClient $ getTarget aid
  return $! case btarget of
    Just (TPoint (TEnemyPos _ permit) lid _) | lid == blid b -> not permit
    _ -> False

-- | Check if the target is nonmoving.
condTgtNonmovingM :: MonadClient m => ActorId -> m Bool
condTgtNonmovingM aid = do
  btarget <- getsClient $ getTarget aid
  case btarget of
    Just (TEnemy enemy _) -> do
      actorMaxSk <- enemyMaxAb enemy
      return $! EM.findWithDefault 0 Ability.AbMove actorMaxSk <= 0
    _ -> return False

-- | Require that any non-dying foe is adjacent, except projectiles
-- that (possibly) explode upon contact.
condAnyFoeAdjM :: MonadStateRead m => ActorId -> m Bool
condAnyFoeAdjM aid = do
  s <- getState
  return $! anyFoeAdj aid s

-- | Require that any non-dying, non-projectile foe is adjacent.
condNonProjFoeAdjM :: MonadStateRead m => ActorId -> m Bool
condNonProjFoeAdjM aid = do
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  allFoes <- getsState $ actorRegularList (isAtWar fact) (blid b)
  return $ any (adjacent (bpos b) . bpos) allFoes  -- keep it lazy

-- | Check if any non-dying, non-projectile foe is dist-close
-- to any of ours.
-- Even if our actor can't melee, he's under melee attack, so has to flee,
-- so full alert is justified as well.
condInMeleeM :: MonadClient m => Int -> Actor -> m Bool
condInMeleeM dist b = do
  fact <- getsState $ (EM.! bfid b) . sfactionD
  allFoes <- getsState $ actorRegularList (isAtWar fact) (blid b)
  ours <- getsState $ actorRegularAssocs (== bfid b) (blid b)
  return $! any (\(_, body) -> any (\bFoe ->
    chessDist (bpos bFoe) (bpos body) <= dist) allFoes) ours

-- | Require the actor's HP is low enough.
condHpTooLowM :: MonadClient m => ActorId -> m Bool
condHpTooLowM aid = do
  b <- getsState $ getActorBody aid
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup aid actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` aid
  return $! hpTooLow b ar

-- | Require the actor stands adjacent to a triggerable tile (e.g., stairs).
condAdjTriggerableM :: MonadStateRead m => ActorId -> m Bool
condAdjTriggerableM aid = do
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  let hasTriggerable p = p `EM.member` lembed lvl
  return $! any hasTriggerable $ vicinityUnsafe $ bpos b

-- | Produce the chess-distance-sorted list of non-low-HP foes on the level.
-- We don't consider path-distance, because we are interested in how soon
-- the foe can hit us, which can diverge greately from path distance
-- for short distances, e.g., when terrain gets revealed.
threatDistList :: MonadClient m => ActorId -> m [(Int, (ActorId, Actor))]
threatDistList aid = do
  actorAspect <- getsClient sactorAspect
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  allAtWar <- getsState $ actorRegularAssocs (isAtWar fact) (blid b)
  let strongActor (aid2, b2) =
        let ar = actorAspect EM.! aid2
            actorMaxSkE = aSkills ar
            nonmoving = EM.findWithDefault 0 Ability.AbMove actorMaxSkE <= 0
        in not (hpTooLow b2 ar || nonmoving)
      allThreats = filter strongActor allAtWar
      addDist (aid2, b2) = (chessDist (bpos b) (bpos b2), (aid2, b2))
  return $ sortBy (comparing fst) $ map addDist allThreats

-- | Require the actor blocks the paths of any of his party members.
condBlocksFriendsM :: MonadClient m => ActorId -> m Bool
condBlocksFriendsM aid = do
  b <- getsState $ getActorBody aid
  ours <- getsState $ actorRegularIds (== bfid b) (blid b)
  targetD <- getsClient stargetD
  let blocked aid2 = aid2 /= aid &&
        case EM.lookup aid2 targetD of
          Just TgtAndPath{tapPath=AndPath{pathList=q : _}} | q == bpos b -> True
          _ -> False
  return $ any blocked ours  -- keep it lazy

-- | Require the actor stands over a weapon that would be auto-equipped.
condFloorWeaponM :: MonadClient m => ActorId -> m Bool
condFloorWeaponM aid = do
  floorAssocs <- getsState $ getActorAssocs aid CGround
  let lootIsWeapon = any (isMelee . snd) floorAssocs
  return lootIsWeapon  -- keep it lazy

-- | Check whether the actor has no weapon in equipment.
condNoEqpWeaponM :: MonadClient m => ActorId -> m Bool
condNoEqpWeaponM aid = do
  eqpAssocs <- getsState $ getActorAssocs aid CEqp
  return $ all (not . isMelee . snd) eqpAssocs  -- keep it lazy

-- | Require that the actor can project any items.
condCanProjectM :: MonadClient m => Bool -> ActorId -> m Bool
condCanProjectM maxSkills aid = do
  actorSk <- if maxSkills
             then do
               actorAspect <- getsClient sactorAspect
               let ar = case EM.lookup aid actorAspect of
                     Just aspectRecord -> aspectRecord
                     Nothing -> assert `failure` aid
               return $! aSkills ar
             else
               actorSkillsClient aid
  let skill = EM.findWithDefault 0 Ability.AbProject actorSk
      q _ itemFull b ar =
        either (const False) id
        $ permittedProject False skill b ar "" itemFull
  benList <- benAvailableItems aid q [CEqp, CInv, CGround]
  let missiles = filter (maybe True ((< 0) . snd) . fst . fst) benList
  return $ not (null missiles)
    -- keep it lazy

-- | Produce the list of items with a given property available to the actor
-- and the items' values.
benAvailableItems :: MonadClient m
                  => ActorId
                  -> (Maybe Int -> ItemFull -> Actor -> AspectRecord -> Bool)
                  -> [CStore]
                  -> m [( (Maybe (Int, Int), (Int, CStore))
                        , (ItemId, ItemFull) )]
benAvailableItems aid permitted cstores = do
  cops <- getsState scops
  itemToF <- itemToFullClient
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  condAnyFoeAdj <- condAnyFoeAdjM aid
  condShineWouldBetray <- condShineWouldBetrayM aid
  condAimEnemyPresent <- condAimEnemyPresentM aid
  condNotCalmEnough <- condNotCalmEnoughM aid
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup aid actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` aid
      heavilyDistressed =  -- Actor hit by a projectile or similarly distressed.
        deltaSerious (bcalmDelta b)
      ben cstore bag =
        [ ((benefit, (k, cstore)), (iid, itemFull))
        | (iid, kit@(k, _)) <- EM.assocs bag
        , let itemFull = itemToF iid kit
              benefit = totalUsefulness cops b ar fact itemFull
              hind = hinders condAnyFoeAdj condShineWouldBetray
                             condAimEnemyPresent
                             heavilyDistressed condNotCalmEnough
                             b ar itemFull
        , permitted (fst <$> benefit) itemFull b ar
          && (cstore /= CEqp || hind) ]
      benCStore cs = do
        bag <- getsState $ getBodyStoreBag b cs
        return $! ben cs bag
  perBag <- mapM benCStore cstores
  return $ concat perBag
    -- keep it lazy

hinders :: Bool -> Bool -> Bool -> Bool -> Bool
        -> Actor -> AspectRecord -> ItemFull
        -> Bool
hinders condAnyFoeAdj condShineWouldBetray condAimEnemyPresent
        heavilyDistressed condNotCalmEnough
          -- guess that enemies have projectiles and used them now or recently
        body ar itemFull =
  let itemShine = 0 < aShine (aspectRecordFull itemFull)
      itemShineBad = itemShine && condShineWouldBetray && not condAnyFoeAdj
  in -- Fast actors want to hide in darkness to ambush opponents and want
     -- to hit hard for the short span they get to survive melee.
     bspeed body ar > speedWalk
     && (itemShineBad
         || 0 > aHurtMelee (aspectRecordFull itemFull))
     -- In the presence of enemies (seen, or unseen but distressing)
     -- actors want to hide in the dark.
     || itemShineBad
        && (heavilyDistressed || condNotCalmEnough || condAimEnemyPresent)

-- | Require the actor is not calm enough.
condNotCalmEnoughM :: MonadClient m => ActorId -> m Bool
condNotCalmEnoughM aid = do
  b <- getsState $ getActorBody aid
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup aid actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` aid
  return $! not (calmEnough b ar)

-- | Require that the actor stands over a desirable item.
condDesirableFloorItemM :: MonadClient m => ActorId -> m Bool
condDesirableFloorItemM aid = do
  benItemL <- benGroundItems aid
  return $ not $ null benItemL  -- keep it lazy

-- | Produce the list of items on the ground beneath the actor
-- that are worth picking up.
benGroundItems :: MonadClient m
               => ActorId
               -> m [( (Maybe (Int, Int)
                     , (Int, CStore)), (ItemId, ItemFull) )]
benGroundItems aid = do
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  let canEsc = fcanEscape (gplayer fact)
  benAvailableItems aid (\use itemFull _ _ ->
                           desirableItem canEsc use itemFull) [CGround]

desirableItem :: Bool -> Maybe Int -> ItemFull -> Bool
desirableItem canEsc use itemFull =
  let item = itemBase itemFull
      freq = case itemDisco itemFull of
        Nothing -> []
        Just ItemDisco{itemKind} -> IK.ifreq itemKind
  in if canEsc
     then use /= Just 0
          || IK.Precious `elem` jfeature item
     else
       -- A hack to prevent monsters from picking up unidentified treasure.
       let preciousNotUseful =
             IK.Precious `elem` jfeature item  -- risk from treasure hunters
             && IK.Equipable `notElem` jfeature item  -- unlikely to be useful
       in use /= Just 0
          && not (isNothing use  -- needs resources to id
                  && preciousNotUseful)
          && maybe True (<= 0) (lookup "gem" freq)

-- | Require the actor is in a bad position to melee or can't melee at all.
condMeleeBadM :: MonadClient m => ActorId -> m Bool
condMeleeBadM aid = do
  actorAspect <- getsClient sactorAspect
  b <- getsState $ getActorBody aid
  btarget <- getsClient $ getTarget aid
  mtgtPos <- case btarget of
    Nothing -> return Nothing
    Just target -> aidTgtToPos aid (blid b) target
  condAimEnemyPresent <- condAimEnemyPresentM aid
  condAimEnemyRemembered <- condAimEnemyRememberedM aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  let friendlyFid fid = fid == bfid b || isAllied fact fid
      ar = actorAspect EM.! aid
  friends <- getsState $ actorRegularAssocs friendlyFid (blid b)
  let -- 3 is the condThreatAtHand distance that AI keeps when alone.
      approaching = case mtgtPos of
        Just tgtPos | condAimEnemyPresent || condAimEnemyRemembered ->
          \b2 -> chessDist (bpos b2) tgtPos <= 3
        _ -> const False
      closeEnough b2 = let dist = chessDist (bpos b) (bpos b2)
                       in dist > 0 && (dist <= 2 || approaching b2)
      closeAndStrong (aid2, b2) = closeEnough b2
                                  && actorCanMelee actorAspect aid2 b2
      closeAndStrongFriends = filter closeAndStrong friends
      noFriendlyHelp = length closeAndStrongFriends < 2 - aAggression ar
                       && length friends > 1  -- solo fighters aggresive
  return $ actorCanMelee actorAspect aid b
           || noFriendlyHelp  -- still not getting friends' help
    -- no $!; keep it lazy

-- | Require that the actor stands in the dark and so would be betrayed
-- by his own equipped light,
condShineWouldBetrayM :: MonadClient m => ActorId -> m Bool
condShineWouldBetrayM aid = do
  b <- getsState $ getActorBody aid
  aInAmbient <- getsState $ actorInAmbient b
  return $! not aInAmbient  -- tile is dark, so actor could hide

-- | Produce a list of acceptable adjacent points to flee to.
fleeList :: MonadClient m => ActorId -> m ([(Int, Point)], [(Int, Point)])
fleeList aid = do
  Kind.COps{coTileSpeedup} <- getsState scops
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  let tgtPath = case mtgtMPath of  -- prefer fleeing along the path to target
        Just TgtAndPath{tapTgt=TEnemy{}} -> []  -- don't flee towards an enemy
        Just TgtAndPath{tapPath=AndPath{pathList}} -> pathList
        _ -> []
  b <- getsState $ getActorBody aid
  fact <- getsState $ \s -> sfactionD s EM.! bfid b
  allFoes <- getsState $ actorRegularList (isAtWar fact) (blid b)
  lvl@Level{lxsize, lysize} <- getLevel $ blid b
  s <- getState
  let posFoes = map bpos allFoes
      myVic = vicinity lxsize lysize $ bpos b
      dist p | null posFoes = 100
             | otherwise = minimum $ map (chessDist p) posFoes
      dVic = map (dist &&& id) myVic
      -- Flee, if possible. Direct access required; not enough time to open.
      -- Can't be occupied.
      accUnocc p = Tile.isWalkable coTileSpeedup (lvl `at` p)
                   && null (posToAssocs p (blid b) s)
      accVic = filter (accUnocc . snd) dVic
      gtVic = filter ((> dist (bpos b)) . fst) accVic
      eqVic = filter ((== dist (bpos b)) . fst) accVic
      ltVic = filter ((< dist (bpos b)) . fst) accVic
      rewardPath mult (d, p)
        | p `elem` tgtPath = (100 * mult * d, p)
        | any (\q -> chessDist p q == 1) tgtPath = (10 * mult * d, p)
        | otherwise = (mult * d, p)
      goodVic = map (rewardPath 10000) gtVic
                ++ map (rewardPath 100) eqVic
      badVic = map (rewardPath 1) ltVic
  return (goodVic, badVic)  -- keep it lazy
