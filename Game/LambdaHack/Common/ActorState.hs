{-# LANGUAGE TupleSections #-}
-- | Operations on the 'Actor' type that need the 'State' type,
-- but not our custom monad types.
module Game.LambdaHack.Common.ActorState
  ( fidActorNotProjAssocs, actorAssocs, actorRegularAssocs
  , warActorRegularList, friendlyActorRegularList, fidActorRegularIds
  , bagAssocs, bagAssocsK, calculateTotal
  , mergeItemQuant, sharedEqp, sharedAllOwnedFid, findIid
  , getContainerBag, getFloorBag, getEmbedBag, getBodyStoreBag
  , mapActorItems_, getActorAssocs
  , nearbyFreePoints, getCarriedAssocs, getCarriedIidCStore
  , posToAidsLvl, posToAids, posToAssocs
  , getItemBody, memActor, getActorBody, getLocalTime, regenCalmDelta
  , actorInAmbient, canDeAmbientList, actorSkills, dispEnemy, fullAssocs
  , storeFromC, lidFromC, posFromC, aidFromC, isEscape, isStair
  , anyFoeAdj, actorAdjacentAssocs, armorHurtBonus
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import Data.Int (Int64)
import GHC.Exts (inline)

import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.TileKind (TileKind)

fidActorNotProjAssocs :: FactionId -> State -> [(ActorId, Actor)]
fidActorNotProjAssocs fid s =
  let f (_, b) = not (bproj b) && bfid b == fid
  in filter f $ EM.assocs $ sactorD s

actorAssocs :: (FactionId -> Bool) -> LevelId -> State
            -> [(ActorId, Actor)]
actorAssocs p lid s =
  let f (_, b) = blid b == lid && p (bfid b)
  in filter f $ EM.assocs $ sactorD s

actorRegularAssocs :: (FactionId -> Bool) -> LevelId -> State
                   -> [(ActorId, Actor)]
{-# INLINE actorRegularAssocs #-}
actorRegularAssocs p lid s =
  let f (_, b) = not (bproj b) && blid b == lid && p (bfid b) && bhp b > 0
  in filter f $ EM.assocs $ sactorD s

warActorRegularList :: FactionId -> LevelId -> State -> [Actor]
warActorRegularList fid lid s =
  let fact = (EM.! fid) . sfactionD $ s
  in map snd $ actorRegularAssocs (inline isAtWar fact) lid s

friendlyActorRegularList :: FactionId -> LevelId -> State -> [Actor]
friendlyActorRegularList fid lid s =
  let fact = (EM.! fid) . sfactionD $ s
      friendlyFid fid2 = fid2 == fid || inline isAllied fact fid2
  in map snd $ actorRegularAssocs friendlyFid lid s

fidActorRegularIds :: FactionId -> LevelId -> State -> [ActorId]
fidActorRegularIds fid lid s =
  map fst $ actorRegularAssocs (== fid) lid s

getItemBody :: ItemId -> State -> Item
getItemBody iid s =
  let assFail = assert `failure` "item body not found" `twith` (iid, s)
  in EM.findWithDefault assFail iid $ sitemD s

bagAssocs :: State -> ItemBag -> [(ItemId, Item)]
bagAssocs s bag =
  let iidItem iid = (iid, getItemBody iid s)
  in map iidItem $ EM.keys bag

bagAssocsK :: State -> ItemBag -> [(ItemId, (Item, ItemQuant))]
bagAssocsK s bag =
  let iidItem (iid, kit) = (iid, (getItemBody iid s, kit))
  in map iidItem $ EM.assocs bag

posToAidsLvl :: Point -> Level -> [ActorId]
{-# INLINE posToAidsLvl #-}
posToAidsLvl pos lvl = EM.findWithDefault [] pos $ lactor lvl

posToAids :: Point -> LevelId -> State -> [ActorId]
posToAids pos lid s = posToAidsLvl pos $ sdungeon s EM.! lid

posToAssocs :: Point -> LevelId -> State -> [(ActorId, Actor)]
posToAssocs pos lid s =
  let l = posToAidsLvl pos $ sdungeon s EM.! lid
  in map (\aid -> (aid, getActorBody aid s)) l

nearbyFreePoints :: (Kind.Id TileKind -> Bool) -> Point -> LevelId -> State
                 -> [Point]
nearbyFreePoints f start lid s =
  let lvl@Level{lxsize, lysize} = sdungeon s EM.! lid
      good p = f (lvl `at` p)
               && Tile.isWalkable (Kind.coTileSpeedup $ scops s) (lvl `at` p)
               && null (posToAidsLvl p lvl)
      ps = nub $ start : concatMap (vicinity lxsize lysize) ps
  in filter good ps

-- | Calculate loot's worth for a given faction.
calculateTotal :: FactionId -> State -> (ItemBag, Int)
calculateTotal fid s =
  let bag = sharedAllOwned fid s
      items = map (\(iid, (k, _)) -> (getItemBody iid s, k)) $ EM.assocs bag
  in (bag, sum $ map itemPrice items)

mergeItemQuant :: ItemQuant -> ItemQuant -> ItemQuant
mergeItemQuant (k2, it2) (k1, it1) = (k1 + k2, it1 ++ it2)

sharedInv :: FactionId -> State -> ItemBag
sharedInv fid s =
  let bs = inline fidActorNotProjAssocs fid s
  in EM.unionsWith mergeItemQuant $ map (binv . snd) bs

sharedEqp :: FactionId -> State -> ItemBag
sharedEqp fid s =
  let bs = inline fidActorNotProjAssocs fid s
  in EM.unionsWith mergeItemQuant $ map (beqp . snd) bs

sharedAllOwned :: FactionId -> State -> ItemBag
sharedAllOwned fid s =
  let shaBag = gsha $ sfactionD s EM.! fid
  in EM.unionsWith mergeItemQuant [sharedEqp fid s, sharedInv fid s, shaBag]

sharedAllOwnedFid :: Bool -> FactionId -> State -> ItemBag
sharedAllOwnedFid onlyOrgans fid s =
  let shaBag = gsha $ sfactionD s EM.! fid
      bs = map snd $ inline fidActorNotProjAssocs fid s
  in EM.unionsWith mergeItemQuant
     $ if onlyOrgans
       then map borgan bs
       else map binv bs ++ map beqp bs ++ [shaBag]

findIid :: ActorId -> FactionId -> ItemId -> State
        -> [(ActorId, (Actor, CStore))]
findIid leader fid iid s =
  let actors = fidActorNotProjAssocs fid s
      itemsOfActor (aid, b) =
        let itemsOfCStore store =
              let bag = getBodyStoreBag b store s
              in map (\iid2 -> (iid2, (aid, (b, store)))) (EM.keys bag)
            stores = [CInv, CEqp, COrgan] ++ [CSha | aid == leader]
        in concatMap itemsOfCStore stores
      items = concatMap itemsOfActor actors
  in map snd $ filter ((== iid) . fst) items

getActorBody :: ActorId -> State -> Actor
{-# INLINE getActorBody #-}
getActorBody aid s = sactorD s EM.! aid

getCarriedAssocs :: Actor -> State -> [(ItemId, Item)]
getCarriedAssocs b s =
  -- The trunk is important for a case of spotting a caught projectile
  -- that is one with a stolen trunk organ (the projectile item).
  -- This actually does happen.
  let trunk = EM.singleton (btrunk b) (1, [])
  in bagAssocs s $ EM.unionsWith const [binv b, beqp b, borgan b, trunk]

getCarriedIidCStore :: Actor -> [(ItemId, CStore)]
getCarriedIidCStore b =
  -- The trunk is important for a case of dominating an actor with stolen
  -- trunk organ.
  let trunk = EM.singleton (btrunk b) (1, [])
      bagCarried (cstore, bag) = map (,cstore) $ EM.keys bag
  in concatMap bagCarried [ (CInv, binv b)
                          , (CEqp, beqp b)
                          , (COrgan, EM.unionWith const (borgan b) trunk) ]

getContainerBag :: Container -> State -> ItemBag
getContainerBag c s = case c of
  CFloor lid p -> getFloorBag lid p s
  CEmbed lid p -> getEmbedBag lid p s
  CActor aid cstore -> let b = getActorBody aid s
                       in getBodyStoreBag b cstore s
  CTrunk{} -> assert `failure` c

getFloorBag :: LevelId -> Point -> State -> ItemBag
getFloorBag lid p s = EM.findWithDefault EM.empty p
                      $ lfloor (sdungeon s EM.! lid)

getEmbedBag :: LevelId -> Point -> State -> ItemBag
getEmbedBag lid p s = EM.findWithDefault EM.empty p
                      $ lembed (sdungeon s EM.! lid)

getBodyStoreBag :: Actor -> CStore -> State -> ItemBag
getBodyStoreBag b cstore s =
  case cstore of
    CGround -> getFloorBag (blid b) (bpos b) s
    COrgan -> borgan b
    CEqp -> beqp b
    CInv -> binv b
    CSha -> gsha $ sfactionD s EM.! bfid b

mapActorItems_ :: Monad m
               => (CStore -> ItemId -> ItemQuant -> m a) -> Actor
               -> State
               -> m ()
mapActorItems_ f b s = do
  let notProcessed = [CGround]
      sts = [minBound..maxBound] \\ notProcessed
      g cstore = do
        let bag = getBodyStoreBag b cstore s
        mapM_ (uncurry $ f cstore) $ EM.assocs bag
  mapM_ g sts

getActorAssocs :: ActorId -> CStore -> State -> [(ItemId, Item)]
getActorAssocs aid cstore s =
  let b = getActorBody aid s
  in bagAssocs s $ getBodyStoreBag b cstore s

getActorAssocsK :: ActorId -> CStore -> State -> [(ItemId, (Item, ItemQuant))]
getActorAssocsK aid cstore s =
  let b = getActorBody aid s
  in bagAssocsK s $ getBodyStoreBag b cstore s

-- | Checks if the actor is present on the current level.
-- The order of argument here and in other functions is set to allow
--
-- > b <- getsState (memActor a)
memActor :: ActorId -> LevelId -> State -> Bool
memActor aid lid s =
  maybe False ((== lid) . blid) $ EM.lookup aid $ sactorD s

-- | Get current time from the dungeon data.
getLocalTime :: LevelId -> State -> Time
getLocalTime lid s = ltime $ sdungeon s EM.! lid

regenCalmDelta :: Actor -> AspectRecord -> State -> Int64
regenCalmDelta body AspectRecord{aMaxCalm} s =
  let calmIncr = oneM  -- normal rate of calm regen
      maxDeltaCalm = xM aMaxCalm - bcalm body
      fact = (EM.! bfid body) . sfactionD $ s
      -- Worry actor by (even projectile) enemies felt (even if not seen)
      -- on the level within 3 steps. Even dying, but not hiding in wait.
      isHeardFoe b = blid b == blid body
                     && chessDist (bpos b) (bpos body) <= 3  -- a bit costly
                     && not (waitedLastTurn b)  -- uncommon
                     && inline isAtWar fact (bfid b)  -- costly
  in if any isHeardFoe $ EM.elems $ sactorD s
     then minusM  -- even if all calmness spent, keep informing the client
     else min calmIncr (max 0 maxDeltaCalm)  -- in case Calm is over max

actorInAmbient :: Actor -> State -> Bool
actorInAmbient b s =
  let lvl = (EM.! blid b) . sdungeon $ s
  in Tile.isLit (Kind.coTileSpeedup $ scops s) (lvl `at` bpos b)

canDeAmbientList :: Actor -> State -> [Point]
canDeAmbientList b s =
  let Kind.COps{coTileSpeedup} = scops s
      lvl = (EM.! blid b) . sdungeon $ s
      posDeAmbient p =
        let t = lvl `at` p
        in Tile.isWalkable coTileSpeedup t  -- no time to waste altering
           && not (Tile.isLit coTileSpeedup t)
  in if Tile.isLit coTileSpeedup (lvl `at` bpos b)
     then filter posDeAmbient (vicinityUnsafe $ bpos b)
     else []

actorSkills :: Maybe ActorId -> ActorId -> AspectRecord -> State
            -> Ability.Skills
actorSkills mleader aid ar s =
  let body = getActorBody aid s
      player = gplayer . (EM.! bfid body) . sfactionD $ s
      skillsFromTactic = Ability.tacticSkills $ ftactic player
      factionSkills
        | Just aid == mleader = Ability.zeroSkills
        | otherwise = fskillsOther player `Ability.addSkills` skillsFromTactic
      itemSkills = aSkills ar
  in itemSkills `Ability.addSkills` factionSkills

-- Check whether an actor can displace an enemy. We assume they are adjacent.
-- If the actor is not, in fact, an enemy, we let it displace.
dispEnemy :: ActorId -> ActorId -> Ability.Skills -> State -> Bool
dispEnemy source target actorMaxSk s =
  let hasSupport b =
        let adjacentAssocs = actorAdjacentAssocs b s
            fact = (EM.! bfid b) . sfactionD $ s
            friendlyFid fid = fid == bfid b || isAllied fact fid
            friend (_, b2) =
              not (bproj b2) && friendlyFid (bfid b2) && bhp b2 > 0
        in any friend adjacentAssocs
      sb = getActorBody source s
      tb = getActorBody target s
  in bproj tb
     || not (isAtWar ((EM.! bfid tb) . sfactionD $ s) (bfid sb))
     || not (actorDying tb
             || braced tb
             || EM.findWithDefault 0 Ability.AbMove actorMaxSk <= 0
             || hasSupport sb && hasSupport tb)  -- solo actors are flexible

fullAssocs :: Kind.COps -> DiscoveryKind -> DiscoveryAspect
           -> ActorId -> [CStore] -> State
           -> [(ItemId, ItemFull)]
fullAssocs cops disco discoAspect aid cstores s =
  let allAssocs = concatMap (\cstore -> getActorAssocsK aid cstore s) cstores
      iToFull (iid, (item, kit)) =
        (iid, itemToFull cops disco discoAspect iid item kit)
  in map iToFull allAssocs

storeFromC :: Container -> CStore
storeFromC c = case c of
  CFloor{} -> CGround
  CEmbed{} -> CGround
  CActor _ cstore -> cstore
  CTrunk{} -> assert `failure` c

aidFromC :: Container -> Maybe ActorId
aidFromC CFloor{} = Nothing
aidFromC CEmbed{} = Nothing
aidFromC (CActor aid _) = Just aid
aidFromC c@CTrunk{} = assert `failure` c

-- | Determine the dungeon level of the container. If the item is in a shared
-- stash, the level depends on which actor asks.
lidFromC :: Container -> State -> LevelId
lidFromC (CFloor lid _) _ = lid
lidFromC (CEmbed lid _) _ = lid
lidFromC (CActor aid _) s = blid $ getActorBody aid s
lidFromC c@CTrunk{} _ = assert `failure` c

posFromC :: Container -> State -> Point
posFromC (CFloor _ pos) _ = pos
posFromC (CEmbed _ pos) _ = pos
posFromC (CActor aid _) s = bpos $ getActorBody aid s
posFromC c@CTrunk{} _ = assert `failure` c

isEscape :: LevelId -> Point -> State -> Bool
isEscape lid p s =
  let bag = getEmbedBag lid p s
      is = map (`getItemBody` s) $ EM.keys bag
      -- Contrived, for now.
      isE Item{jname} = jname == "escape"
  in any isE is

isStair :: LevelId -> Point -> State -> Bool
isStair lid p s =
  let bag = getEmbedBag lid p s
      is = map (`getItemBody` s) $ EM.keys bag
      -- Contrived, for now.
      isE Item{jname} = jname == "staircase up" || jname == "staircase down"
  in any isE is

-- | Require that any non-dying foe is adjacent. We include even
-- projectiles that explode when stricken down, because they can be caught
-- and then they don't explode, so it makes sense to focus on handling them.
-- If there are many projectiles in a single adjacent position, we only test
-- the first one, the one that would be hit in melee (this is not optimal
-- if the actor would need to flee instead of meleeing, but fleeing
-- with *many* projectiles adjacent is a possible waste of a move anyway).
anyFoeAdj :: ActorId -> State -> Bool
anyFoeAdj aid s =
  let body = getActorBody aid s
      lvl = (EM.! blid body) . sdungeon $ s
      fact = (EM.! bfid body) . sfactionD $ s
      f !mv = case posToAidsLvl (shift (bpos body) mv) lvl of
        [] -> False
        aid2 : _ -> g $ getActorBody aid2 s
      g !b = isAtWar fact (bfid b) && bhp b > 0
  in any f moves

actorAdjacentAssocs :: Actor -> State -> [(ActorId, Actor)]
{-# INLINE actorAdjacentAssocs #-}
actorAdjacentAssocs body s =
  let lvl = (EM.! blid body) . sdungeon $ s
      f !mv = posToAidsLvl (shift (bpos body) mv) lvl
      g !aid = (aid, getActorBody aid s)
  in map g $ concatMap f moves

armorHurtBonus :: ActorAspect -> ActorId -> ActorId -> State -> Int
armorHurtBonus actorAspect source target s =
  let sb = getActorBody source s
      tb = getActorBody target s
      trim200 n = min 200 $ max (-200) n
      block200 b n = min 200 $ max (-200) $ n + if braced tb then b else 0
      sar = actorAspect EM.! source
      tar = actorAspect EM.! target
      itemBonus = trim200 (aHurtMelee sar) - if bproj sb
                                             then block200 25 (aArmorRanged tar)
                                             else block200 50 (aArmorMelee tar)
  in 100 + min 99 (max (-99) itemBonus)  -- at least 1% of damage gets through
