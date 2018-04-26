{-# LANGUAGE TupleSections #-}
-- | Operations on the 'Actor' type, and related, that need the 'State' type,
-- but not our custom monad types.
module Game.LambdaHack.Common.ActorState
  ( fidActorNotProjAssocs, actorAssocs, actorRegularAssocs
  , warActorRegularList, friendlyActorRegularList, fidActorRegularIds
  , bagAssocs, bagAssocsK, posToAidsLvl, posToAids, posToAssocs
  , nearbyFreePoints, calculateTotal, itemPrice, mergeItemQuant, findIid
  , combinedInv, combinedEqp, combinedOrgan, combinedItems, combinedFromLore
  , getActorBody, getActorAspect, canTraverse
  , getCarriedAssocsAndTrunk, getCarriedIidCStore, getContainerBag
  , getFloorBag, getEmbedBag, getBodyStoreBag
  , mapActorItems_, getActorAssocs, getActorAssocsK
  , memActor, getLocalTime, regenCalmDelta, actorInAmbient, canDeAmbientList
  , actorSkills, dispEnemy, itemToFull, fullAssocs, kitAssocs
  , getItemKindId, getIidKindId, getItemKind, getIidKind
  , getItemKindIdServer, getIidKindIdServer, getItemKindServer, getIidKindServer
  , storeFromC, aidFromC, lidFromC, posFromC
  , isStair, anyFoeAdj, actorAdjacentAssocs
  , armorHurtBonus, inMelee
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import           Data.Int (Int64)
import           GHC.Exts (inline)

import qualified Game.LambdaHack.Common.Ability as Ability
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Content.TileKind as TK

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

nearbyFreePoints :: (ContentId TileKind -> Bool) -> Point -> LevelId -> State
                 -> [Point]
nearbyFreePoints f start lid s =
  let lvl@Level{lxsize, lysize} = sdungeon s EM.! lid
      good p = f (lvl `at` p)
               && Tile.isWalkable (coTileSpeedup $ scops s) (lvl `at` p)
               && null (posToAidsLvl p lvl)
      ps = nub $ start : concatMap (vicinity lxsize lysize) ps
  in filter good ps

-- | Calculate loot's worth for a given faction.
calculateTotal :: FactionId -> State -> (ItemBag, Int)
calculateTotal fid s =
  let bag = combinedItems fid s
      items = map (\(iid, (k, _)) -> (getItemBody iid s, k)) $ EM.assocs bag
      price (item, k) = itemPrice k $ getItemKind item s
  in (bag, sum $ map price items)

-- | Price an item, taking count into consideration.
itemPrice :: Int -> IK.ItemKind -> Int
itemPrice jcount itemKind = case lookup "valuable" $ IK.ifreq itemKind of
  Just k -> jcount * k
  Nothing -> 0

mergeItemQuant :: ItemQuant -> ItemQuant -> ItemQuant
mergeItemQuant (k2, it2) (k1, it1) = (k1 + k2, it1 ++ it2)

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

combinedInv :: FactionId -> State -> ItemBag
combinedInv fid s =
  let bs = inline fidActorNotProjAssocs fid s
  in EM.unionsWith mergeItemQuant $ map (binv . snd) bs

combinedEqp :: FactionId -> State -> ItemBag
combinedEqp fid s =
  let bs = inline fidActorNotProjAssocs fid s
  in EM.unionsWith mergeItemQuant $ map (beqp . snd) bs

-- Trunk not considered (if stolen).
combinedOrgan :: FactionId -> State -> ItemBag
combinedOrgan fid s =
  let bs = inline fidActorNotProjAssocs fid s
  in EM.unionsWith mergeItemQuant $ map (borgan . snd) bs

-- Trunk not considered (if stolen).
combinedItems :: FactionId -> State -> ItemBag
combinedItems fid s =
  let shaBag = gsha $ sfactionD s EM.! fid
      bs = map snd $ inline fidActorNotProjAssocs fid s
  in EM.unionsWith mergeItemQuant $ map binv bs ++ map beqp bs ++ [shaBag]

combinedFromLore :: SLore -> FactionId -> State -> ItemBag
combinedFromLore slore fid s = case slore of
  SItem -> combinedItems fid s
  SOrgan -> combinedOrgan fid s
  STrunk -> combinedOrgan fid s
  STmp -> combinedOrgan fid s
  SBlast -> EM.empty
  SEmbed -> EM.empty

getActorBody :: ActorId -> State -> Actor
{-# INLINE getActorBody #-}
getActorBody aid s = sactorD s EM.! aid

getActorAspect :: ActorId -> State -> IA.AspectRecord
{-# INLINE getActorAspect #-}
getActorAspect aid s = sactorAspect s EM.! aid

-- Check that the actor can move, also between levels and through doors.
-- Otherwise, it's too awkward for human player to control, e.g.,
-- being stuck in a room with revolving doors closing after one turn
-- and the player needing to micromanage opening such doors with
-- another actor all the time. Completely immovable actors
-- e.g., an impregnable surveillance camera in a crowded corridor,
-- are less of a problem due to micromanagment, but more due to
-- the constant disturbing of other actor's running, etc..
canTraverse :: ActorId -> State -> Bool
canTraverse aid s =
  let actorMaxSk = IA.aSkills $ getActorAspect aid s
  in EM.findWithDefault 0 Ability.AbMove actorMaxSk > 0
     && EM.findWithDefault 0 Ability.AbAlter actorMaxSk
          >= fromEnum TK.talterForStairs

getCarriedAssocsAndTrunk :: Actor -> State -> [(ItemId, Item)]
getCarriedAssocsAndTrunk b s =
  -- The trunk is important for a case of spotting a caught projectile
  -- with a stolen projecting item. This actually does happen.
  let trunk = EM.singleton (btrunk b) (1, [])
  in bagAssocs s $ EM.unionsWith const [binv b, beqp b, borgan b, trunk]

getCarriedIidCStore :: Actor -> [(ItemId, CStore)]
getCarriedIidCStore b =
  let bagCarried (cstore, bag) = map (,cstore) $ EM.keys bag
  in concatMap bagCarried [(CInv, binv b), (CEqp, beqp b), (COrgan, borgan b)]

getContainerBag :: Container -> State -> ItemBag
getContainerBag c s = case c of
  CFloor lid p -> getFloorBag lid p s
  CEmbed lid p -> getEmbedBag lid p s
  CActor aid cstore -> let b = getActorBody aid s
                       in getBodyStoreBag b cstore s
  CTrunk{} -> error $ "" `showFailure` c

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
               => (CStore -> ItemId -> ItemQuant -> m a) -> Actor -> State
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

regenCalmDelta :: ActorId -> Actor -> State -> Int64
regenCalmDelta aid body s =
  let calmIncr = oneM  -- normal rate of calm regen
      IA.AspectRecord{aMaxCalm} = getActorAspect aid s
      maxDeltaCalm = xM aMaxCalm - bcalm body
      fact = (EM.! bfid body) . sfactionD $ s
      -- Worry actor by (even projectile) enemies felt (even if not seen)
      -- on the level within 3 steps. Even dying, but not hiding in wait.
      isHeardFoe !b = bfid b /= bfid body  -- shortcut
                      && blid b == blid body
                      && inline chessDist (bpos b) (bpos body) <= 3
                      && not (waitedLastTurn b)  -- uncommon
                      && inline isAtWar fact (bfid b)  -- costly
  in if any isHeardFoe $ EM.elems $ sactorD s
     then minusM  -- even if all calmness spent, keep informing the client
     else min calmIncr (max 0 maxDeltaCalm)  -- in case Calm is over max

actorInAmbient :: Actor -> State -> Bool
actorInAmbient b s =
  let lvl = (EM.! blid b) . sdungeon $ s
  in Tile.isLit (coTileSpeedup $ scops s) (lvl `at` bpos b)

canDeAmbientList :: Actor -> State -> [Point]
canDeAmbientList b s =
  let COps{coTileSpeedup} = scops s
      lvl = (EM.! blid b) . sdungeon $ s
      posDeAmbient p =
        let t = lvl `at` p
        in Tile.isWalkable coTileSpeedup t  -- no time to waste altering
           && not (Tile.isLit coTileSpeedup t)
  in if Tile.isLit coTileSpeedup (lvl `at` bpos b)
     then filter posDeAmbient (vicinityUnsafe $ bpos b)
     else []

actorSkills :: Maybe ActorId -> ActorId -> State -> Ability.Skills
actorSkills mleader aid s =
  let body = getActorBody aid s
      ar = getActorAspect aid s
      player = gplayer . (EM.! bfid body) . sfactionD $ s
      skillsFromTactic = Ability.tacticSkills $ ftactic player
      factionSkills
        | Just aid == mleader = Ability.zeroSkills
        | otherwise = fskillsOther player `Ability.addSkills` skillsFromTactic
      itemSkills = IA.aSkills ar
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

itemToFull :: ItemId -> State -> ItemFull
itemToFull iid s =
  itemToFull6 (scops s) (sdiscoKind s) (sdiscoAspect s) iid (getItemBody iid s)

fullAssocs :: ActorId -> [CStore] -> State -> [(ItemId, ItemFull)]
fullAssocs aid cstores s =
  let allAssocs = concatMap (\cstore -> getActorAssocsK aid cstore s) cstores
      iToFull (iid, (item, _kit)) =
        (iid, itemToFull6 (scops s) (sdiscoKind s) (sdiscoAspect s) iid item)
  in map iToFull allAssocs

kitAssocs :: ActorId -> [CStore] -> State -> [(ItemId, ItemFullKit)]
kitAssocs aid cstores s =
  let allAssocs = concatMap (\cstore -> getActorAssocsK aid cstore s) cstores
      iToFull (iid, (item, kit)) =
        (iid, ( itemToFull6 (scops s) (sdiscoKind s) (sdiscoAspect s) iid item
              , kit ))
  in map iToFull allAssocs

getItemKindId :: Item -> State -> ContentId IK.ItemKind
getItemKindId item s = case jkind item of
  IdentityObvious ik -> ik
  IdentityCovered ix ik -> fromMaybe ik $ EM.lookup ix $ sdiscoKind s

getIidKindId :: ItemId -> State -> ContentId IK.ItemKind
getIidKindId iid s = getItemKindId (getItemBody iid s) s

getItemKind :: Item -> State -> IK.ItemKind
getItemKind item s = okind (coitem $ scops s) $ getItemKindId item s

getIidKind :: ItemId -> State -> IK.ItemKind
getIidKind iid s = getItemKind (getItemBody iid s) s

getItemKindIdServer :: Item -> State -> ContentId IK.ItemKind
getItemKindIdServer item s = case jkind item of
  IdentityObvious ik -> ik
  IdentityCovered ix _ik -> fromJust $ EM.lookup ix $ sdiscoKind s

getIidKindIdServer :: ItemId -> State -> ContentId IK.ItemKind
getIidKindIdServer iid s = getItemKindIdServer (getItemBody iid s) s

getItemKindServer :: Item -> State -> IK.ItemKind
getItemKindServer item s = okind (coitem $ scops s) $ getItemKindIdServer item s

getIidKindServer :: ItemId -> State -> IK.ItemKind
getIidKindServer iid s = getItemKindServer (getItemBody iid s) s

storeFromC :: Container -> CStore
storeFromC c = case c of
  CFloor{} -> CGround
  CEmbed{} -> CGround
  CActor _ cstore -> cstore
  CTrunk{} -> error $ "" `showFailure` c

aidFromC :: Container -> Maybe ActorId
aidFromC CFloor{} = Nothing
aidFromC CEmbed{} = Nothing
aidFromC (CActor aid _) = Just aid
aidFromC c@CTrunk{} = error $ "" `showFailure` c

-- | Determine the dungeon level of the container. If the item is in a shared
-- stash, the level depends on which actor asks.
lidFromC :: Container -> State -> LevelId
lidFromC (CFloor lid _) _ = lid
lidFromC (CEmbed lid _) _ = lid
lidFromC (CActor aid _) s = blid $ getActorBody aid s
lidFromC c@CTrunk{} _ = error $ "" `showFailure` c

posFromC :: Container -> State -> Point
posFromC (CFloor _ pos) _ = pos
posFromC (CEmbed _ pos) _ = pos
posFromC (CActor aid _) s = bpos $ getActorBody aid s
posFromC c@CTrunk{} _ = error $ "" `showFailure` c

isStair :: LevelId -> Point -> State -> Bool
isStair lid p s =
  let bag = getEmbedBag lid p s
      ks = map (flip getIidKind s) $ EM.keys bag
  in any (any IK.isEffAscend . IK.ieffects) ks

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
      g !b = bfid b /= bfid body  -- shortcut
             && isAtWar fact (bfid b)
             && bhp b > 0  -- uncommon
  in any f moves

actorAdjacentAssocs :: Actor -> State -> [(ActorId, Actor)]
{-# INLINE actorAdjacentAssocs #-}
actorAdjacentAssocs body s =
  let lvl = (EM.! blid body) . sdungeon $ s
      f !mv = posToAidsLvl (shift (bpos body) mv) lvl
      g !aid = (aid, getActorBody aid s)
  in map g $ concatMap f moves

armorHurtBonus :: ActorId -> ActorId -> State -> Int
armorHurtBonus source target s =
  let sb = getActorBody source s
      tb = getActorBody target s
      trim200 n = min 200 $ max (-200) n
      block200 b n = min 200 $ max (-200) $ n + if braced tb then b else 0
      sar = sactorAspect s EM.! source
      tar = sactorAspect s EM.! target
      itemBonus = trim200 (IA.aHurtMelee sar)
                  - if bproj sb
                    then block200 25 (IA.aArmorRanged tar)
                    else block200 50 (IA.aArmorMelee tar)
  in 100 + min 99 (max (-99) itemBonus)  -- at least 1% of damage gets through

inMelee :: Actor -> State -> Bool
inMelee bodyOur s =
  let fact = sfactionD s EM.! bfid bodyOur
      f !b = bfid b /= bfid bodyOur  -- shortcut
             && blid b == blid bodyOur
             && inline isAtWar fact (bfid b)  -- costly
             && bhp b > 0  -- uncommon
      -- We assume foes are less numerous, because usually they are heroes,
      -- and so we compute them once and use many times.
      -- For the same reason @anyFoeAdj@ would not speed up this computation
      -- in normal gameplay (as opposed to AI vs AI benchmarks).
      allFoes = filter f $ EM.elems $ sactorD s
  in any (\body ->
    bfid bodyOur == bfid body
    && blid bodyOur == blid body
    && not (bproj body)
    && bhp body > 0
    && any (\b -> adjacent (bpos b) (bpos body)) allFoes) $ sactorD s
