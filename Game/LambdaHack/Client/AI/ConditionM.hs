-- | Semantics of abilities in terms of actions and the AI procedure
-- for picking the best action for an actor.
module Game.LambdaHack.Client.AI.ConditionM
  ( condAimEnemyPresentM
  , condAimEnemyRememberedM
  , condAimEnemyAdjFriendM
  , condTgtNonmovingM
  , condAnyFoeAdjM
  , condNonProjFoeAdjM
  , condHpTooLowM
  , condOnTriggerableM
  , condBlocksFriendsM
  , condFloorWeaponM
  , condNoEqpWeaponM
  , condEnoughGearM
  , condCanProjectM
  , condNotCalmEnoughM
  , condDesirableFloorItemM
  , condMeleeBadM
  , condShineBetraysM
  , benAvailableItems
  , hinders
  , benGroundItems
  , desirableItem
  , threatDistList
  , fleeList
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Arrow ((&&&))
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
    Just (TEnemyPos _ lid _ permit) | lid == blid b -> not permit
    _ -> False

-- | Require that the target enemy is adjacent to at least one friend.
condAimEnemyAdjFriendM :: MonadClient m => ActorId -> m Bool
condAimEnemyAdjFriendM aid = do
  btarget <- getsClient $ getTarget aid
  case btarget of
    Just (TEnemy enemy _) -> do
      be <- getsState $ getActorBody enemy
      b <- getsState $ getActorBody aid
      fact <- getsState $ (EM.! bfid b) . sfactionD
      let friendlyFid fid = fid == bfid b || isAllied fact fid
      friends <- getsState $ actorRegularList friendlyFid (blid b)
      return $ any (adjacent (bpos be) . bpos) friends  -- keep it lazy
    _ -> return False

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
  body <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid body) . sfactionD
  s <- getState
  let isFragile bag = case EM.keys bag of
        [iid] -> let itemBase = getItemBody iid s
                 in IK.Fragile `elem` jfeature itemBase
        _ -> assert `failure` bag
      f b = blid b == blid body && isAtWar fact (bfid b) && bhp b > 0
            && adjacent (bpos b) (bpos body)
            && not (bproj b && isFragile (beqp b))
  allAdjFoes <- getsState $ filter f . EM.elems . sactorD
  return $! not $ null allAdjFoes

-- | Require that any non-dying, non-projectile foe is adjacent.
condNonProjFoeAdjM :: MonadStateRead m => ActorId -> m Bool
condNonProjFoeAdjM aid = do
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  allFoes <- getsState $ actorRegularList (isAtWar fact) (blid b)
  return $ any (adjacent (bpos b) . bpos) allFoes  -- keep it lazy

-- | Require the actor's HP is low enough.
condHpTooLowM :: MonadClient m => ActorId -> m Bool
condHpTooLowM aid = do
  b <- getsState $ getActorBody aid
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup aid actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` aid
  return $! hpTooLow b ar

-- | Require the actor stands over a triggerable tile.
condOnTriggerableM :: MonadStateRead m => ActorId -> m Bool
condOnTriggerableM aid = do
  Kind.COps{cotile} <- getsState scops
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  let t = lvl `at` bpos b
  return $! not $ null $ Tile.causeEffects cotile t

-- | Produce the chess-distance-sorted list of non-low-HP foes on the level.
-- We don't consider path-distance, because we are interested in how soon
-- the foe can hit us, which can diverge greately from path distance
-- for short distances.
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

-- | Check whether the actor has enough gear to go look for enemies.
-- We assume weapons is equipment are better than any among organs
-- or at least provide some essential diversity.
condEnoughGearM :: MonadClient m => ActorId -> m Bool
condEnoughGearM aid = do
  eqpAssocs <- getsState $ getActorAssocs aid CEqp
  invAssocs <- getsState $ getActorAssocs aid CInv
  return $ any (isMelee . snd) eqpAssocs
           || length (eqpAssocs ++ invAssocs) >= 5
    -- keep it lazy

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
        $ permittedProject False skill b ar " " itemFull
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
  condShineBetrays <- condShineBetraysM aid
  condAimEnemyPresent <- condAimEnemyPresentM aid
  condNotCalmEnough <- condNotCalmEnoughM aid
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup aid actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` aid
      ben cstore bag =
        [ ((benefit, (k, cstore)), (iid, itemFull))
        | (iid, kit@(k, _)) <- EM.assocs bag
        , let itemFull = itemToF iid kit
              benefit = totalUsefulness cops b ar fact itemFull
              hind = hinders condAnyFoeAdj condShineBetrays
                             condAimEnemyPresent condNotCalmEnough
                             b ar itemFull
        , permitted (fst <$> benefit) itemFull b ar
          && (cstore /= CEqp || hind) ]
      benCStore cs = do
        bag <- getsState $ getBodyStoreBag b cs
        return $! ben cs bag
  perBag <- mapM benCStore cstores
  return $ concat perBag
    -- keep it lazy

-- TODO: also take into account dynamic lights *not* wielded by the actor
hinders :: Bool -> Bool -> Bool -> Bool -> Actor -> AspectRecord -> ItemFull
        -> Bool
hinders condAnyFoeAdj condShineBetrays condAimEnemyPresent
        condNotCalmEnough  -- perhaps enemies don't have projectiles
        body ar itemFull =
  let itemShine = 0 < aShine (aspectRecordFull itemFull)
      itemShineBad = itemShine && condNotCalmEnough && not condAnyFoeAdj
  in -- Fast actors want to hide in darkness to ambush opponents and want
     -- to hit hard for the short span they get to survive melee.
     bspeed body ar > speedNormal
     && (itemShineBad
         || 0 > aHurtMelee (aspectRecordFull itemFull))
     -- In the presence of enemies (seen, or unseen but distressing)
     -- actors want to hide in the dark.
     || let heavilyDistressed =  -- actor hit by a proj or similarly distressed
              deltaSerious (bcalmDelta body)
        in itemShineBad && condShineBetrays
           && (heavilyDistressed || condAimEnemyPresent)
  -- TODO:
  -- teach AI to turn shields OFF (or stash) when ganging up on an enemy
  -- (friends close, only one enemy close)
  -- and turning on afterwards (AI plays for time, especially spawners
  -- so shields are preferable by default;
  -- also, turning on when no friends and enemies close is too late,
  -- AI should flee or fire at such times, not muck around with eqp)

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
  canEscape <- factionCanEscape (bfid b)
  benAvailableItems aid (\use itemFull _ _ ->
                           desirableItem canEscape use itemFull) [CGround]

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
          -- TODO: terrible hack for the identified healing gems and normal
          -- gems identified with a scroll
          && maybe True (<= 0) (lookup "gem" freq)

-- | Require the actor is in a bad position to melee or can't melee at all.
condMeleeBadM :: MonadClient m => ActorId -> m Bool
condMeleeBadM aid = do
  actorAspect <- getsClient sactorAspect
  b <- getsState $ getActorBody aid
  btarget <- getsClient $ getTarget aid
  mtgtPos <- aidTgtToPos aid (blid b) btarget
  condAimEnemyPresent <- condAimEnemyPresentM aid
  condAimEnemyRemembered <- condAimEnemyRememberedM aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  let condNoUsableWeapon = bweapon b == 0
      friendlyFid fid = fid == bfid b || isAllied fact fid
  friends <- getsState $ actorRegularAssocs friendlyFid (blid b)
  let closeEnough b2 = let dist = chessDist (bpos b) (bpos b2)
                       in dist > 0 && (dist <= 2 || approaching b2)
      -- 3 is the condThreatAtHand distance that AI keeps when alone.
      approaching = case mtgtPos of
        Just tgtPos | condAimEnemyPresent || condAimEnemyRemembered ->
          \b1 -> chessDist (bpos b1) tgtPos <= 3
        _ -> const False
      closeFriends = filter (closeEnough . snd) friends
      strongActor (aid2, b2) =
        let ar = actorAspect EM.! aid2
            actorMaxSk2 = aSkills ar
            condUsableWeapon2 = bweapon b >= 0
            canMelee2 = EM.findWithDefault 0 Ability.AbMelee actorMaxSk2 > 0
            hpGood = not $ hpTooLow b2 ar
        in hpGood && condUsableWeapon2 && canMelee2
      strongCloseFriends = filter strongActor closeFriends
      noFriendlyHelp = length closeFriends < 3
                       && null strongCloseFriends
                       && length friends > 1  -- solo fighters aggresive
                       && not (hpHuge b)  -- uniques, etc., aggresive
      actorMaxSk = aSkills $ actorAspect EM.! aid
  return $ condNoUsableWeapon
           || EM.findWithDefault 0 Ability.AbMelee actorMaxSk <= 0
           || noFriendlyHelp  -- still not getting friends' help
    -- no $!; keep it lazy

-- | Require that the actor stands in the dark, but is betrayed
-- by his own equipped light,
condShineBetraysM :: MonadClient m => ActorId -> m Bool
condShineBetraysM aid = do
  b <- getsState $ getActorBody aid
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup aid actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` aid
  let actorShines = aShine ar > 0
  aInAmbient <- getsState $ actorInAmbient b
  return $! not aInAmbient  -- tile is dark, so actor could hide
            && actorShines  -- but actor betrayed by his eqp or organ light

-- | Produce a list of acceptable adjacent points to flee to.
fleeList :: MonadClient m => ActorId -> m ([(Int, Point)], [(Int, Point)])
fleeList aid = do
  cops <- getsState scops
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  let tgtPath = case mtgtMPath of  -- prefer fleeing along the path to target
        Just TgtAndPath{tapTgt=TEnemy{}} -> []  -- don't flee towards an enemy
        Just TgtAndPath{tapTgt=TEnemyPos{}} -> []
        Just TgtAndPath{tapPath=AndPath{pathList}} -> pathList
        _ -> []
  b <- getsState $ getActorBody aid
  fact <- getsState $ \s -> sfactionD s EM.! bfid b
  allFoes <- getsState $ actorRegularList (isAtWar fact) (blid b)
  lvl@Level{lxsize, lysize} <- getLevel $ blid b
  let posFoes = map bpos allFoes
      myVic = vicinity lxsize lysize $ bpos b
      dist p | null posFoes = assert `failure` b
             | otherwise = minimum $ map (chessDist p) posFoes
      dVic = map (dist &&& id) myVic
      -- Flee, if possible. Access required.
      accVic = filter (accessible cops lvl . snd) dVic
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
