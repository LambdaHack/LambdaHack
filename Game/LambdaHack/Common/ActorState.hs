{-# LANGUAGE TupleSections #-}
-- | Operations on the 'Actor' type that need the 'State' type,
-- but not the 'Action' type.
-- TODO: Document an export list after it's rewritten according to #17.
module Game.LambdaHack.Common.ActorState
  ( fidActorNotProjAssocs, fidActorNotProjList
  , actorAssocs, actorList
  , actorRegularAssocs, actorRegularList, actorRegularIds
  , bagAssocs, bagAssocsK, calculateTotal
  , mergeItemQuant, sharedAllOwnedFid, findIid
  , getContainerBag, getFloorBag, getEmbedBag, getBodyStoreBag
  , mapActorItems_, getActorAssocs
  , nearbyFreePoints, whereTo, getCarriedAssocs, getCarriedIidCStore
  , posToAidsLvl, posToAids, posToAssocs
  , getItemBody, memActor, getActorBody
  , tryFindHeroK, getLocalTime, itemPrice, regenCalmDelta
  , actorInAmbient, actorSkills, dispEnemy, fullAssocs, itemToFull
  , goesIntoEqp, goesIntoInv, goesIntoSha, eqpOverfull, eqpFreeN
  , storeFromC, lidFromC, aidFromC
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import Data.Int (Int64)

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
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.TileKind (TileKind)

fidActorNotProjAssocs :: FactionId -> State -> [(ActorId, Actor)]
fidActorNotProjAssocs fid s =
  let f (_, b) = not (bproj b) && bfid b == fid
  in filter f $ EM.assocs $ sactorD s

fidActorNotProjList :: FactionId -> State -> [Actor]
fidActorNotProjList fid s =
  let f b = not (bproj b) && bfid b == fid
  in filter f $ EM.elems $ sactorD s

actorAssocs :: (FactionId -> Bool) -> LevelId -> State
            -> [(ActorId, Actor)]
actorAssocs p lid s =
  let f (_, b) = blid b == lid && p (bfid b)
  in filter f $ EM.assocs $ sactorD s

actorList :: (FactionId -> Bool) -> LevelId -> State -> [Actor]
actorList p lid s =
  let f b = blid b == lid && p (bfid b)
  in filter f $ EM.elems $ sactorD s

actorRegularAssocs :: (FactionId -> Bool) -> LevelId -> State
                   -> [(ActorId, Actor)]
actorRegularAssocs p lid s =
  let f (_, b) = not (bproj b) && blid b == lid && p (bfid b) && bhp b > 0
  in filter f $ EM.assocs $ sactorD s

actorRegularList :: (FactionId -> Bool) -> LevelId -> State
                 -> [Actor]
actorRegularList p lid s =
  let f b = not (bproj b) && blid b == lid && p (bfid b) && bhp b > 0
  in filter f $ EM.elems $ sactorD s

actorRegularIds :: (FactionId -> Bool) -> LevelId -> State
                -> [ActorId]
actorRegularIds p lid s = map fst $ actorRegularAssocs p lid s

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

nearbyFreePoints :: Int
                 -> (Kind.Id TileKind -> Bool) -> Point -> LevelId -> State
                 -> [Point]
nearbyFreePoints ntries f start lid s =
  let lvl@Level{lxsize, lysize} = sdungeon s EM.! lid
      good p = f (lvl `at` p)
               && Tile.isWalkable (Kind.coTileSpeedup $ scops s) (lvl `at` p)
               && null (posToAidsLvl p lvl)
      ps = nub $ start : concatMap (vicinity lxsize lysize) ps
  in filter good $ take ntries ps

-- | Calculate loot's worth for a faction of a given actor.
calculateTotal :: Actor -> State -> (ItemBag, Int)
calculateTotal body s =
  let bag = sharedAllOwned body s
      items = map (\(iid, (k, _)) -> (getItemBody iid s, k)) $ EM.assocs bag
  in (bag, sum $ map itemPrice items)

mergeItemQuant :: ItemQuant -> ItemQuant -> ItemQuant
{-# INLINE mergeItemQuant #-}
mergeItemQuant (k1, it1) (k2, it2) = (k1 + k2, it1 ++ it2)

sharedInv :: Actor -> State -> ItemBag
sharedInv body s =
  let bs = fidActorNotProjList (bfid body) s
  in EM.unionsWith mergeItemQuant
     $ map binv $ if null bs then [body] else bs

sharedEqp :: Actor -> State -> ItemBag
sharedEqp body s =
  let bs = fidActorNotProjList (bfid body) s
  in EM.unionsWith mergeItemQuant
     $ map beqp $ if null bs then [body] else bs

sharedAllOwned :: Actor -> State -> ItemBag
sharedAllOwned body s =
  let shaBag = gsha $ sfactionD s EM.! bfid body
  in EM.unionsWith mergeItemQuant [sharedEqp body s, sharedInv body s, shaBag]

sharedAllOwnedFid :: Bool -> FactionId -> State -> ItemBag
sharedAllOwnedFid onlyOrgans fid s =
  let shaBag = gsha $ sfactionD s EM.! fid
      bs = fidActorNotProjList fid s
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

-- | Price an item, taking count into consideration.
itemPrice :: (Item, Int) -> Int
itemPrice (item, jcount) =
  case jsymbol item of
    '$' -> jcount
    '*' -> jcount * 100
    _   -> 0

-- * These few operations look at, potentially, all levels of the dungeon.

-- | Tries to finds an actor body satisfying a predicate on any level.
tryFindActor :: State -> (Actor -> Bool) -> Maybe (ActorId, Actor)
tryFindActor s p =
  find (p . snd) $ EM.assocs $ sactorD s

tryFindHeroK :: FactionId -> Int -> State -> Maybe (ActorId, Actor)
tryFindHeroK fact k s =
  let c | k == 0          = '@'
        | k > 0 && k < 10 = Char.intToDigit k
        | otherwise       = assert `failure` "no digit" `twith` k
  in tryFindActor s (\body -> bsymbol body == c
                              && not (bproj body)
                              && bfid body == fact)

-- | Compute the level identifier and starting position on the level,
-- after a level change.
whereTo :: LevelId  -- ^ level of the stairs
        -> Point    -- ^ position of the stairs
        -> Int      -- ^ jump up this many levels
        -> Dungeon  -- ^ current game dungeon
        -> (LevelId, Point)
                    -- ^ destination level and the pos of its receiving stairs
whereTo lid pos k dungeon = assert (k /= 0) $
  let lvl = dungeon EM.! lid
      stairs = (if k < 0 then snd else fst) (lstair lvl)
      defaultStairs = 0  -- for ascending via, e.g., spells
      mindex = elemIndex pos stairs
      i = fromMaybe defaultStairs mindex
  in case ascendInBranch dungeon k lid of
    [] | isNothing mindex -> (lid, pos)  -- spell fizzles
    [] -> assert `failure` "no dungeon level to go to" `twith` (lid, pos, k)
    ln : _ -> let lvlDest = dungeon EM.! ln
                  stairsDest = (if k < 0 then fst else snd) (lstair lvlDest)
              in if length stairsDest < i + 1
                 then assert `failure` "no stairs at index"
                             `twith` (lid, pos, k, ln, stairsDest, i)
                 else (ln, stairsDest !! i)

-- * The operations below disregard levels other than the current.

-- Inlining slows it down.
-- | Gets actor body from the current level. Error if not found.
getActorBody :: ActorId -> State -> Actor
getActorBody !aid !s =
  let assFail = assert `failure` "body not found" `twith` (aid, s)
  in EM.findWithDefault assFail aid $ sactorD s

getCarriedAssocs :: Actor -> State -> [(ItemId, Item)]
getCarriedAssocs b s =
  bagAssocs s $ EM.unionsWith const [binv b, beqp b, borgan b]

getCarriedIidCStore :: Actor -> [(ItemId, CStore)]
getCarriedIidCStore b =
  let bagCarried (cstore, bag) = map (,cstore) $ EM.keys bag
  in concatMap bagCarried
               [(CInv, binv b), (CEqp, beqp b), (COrgan, borgan b)]

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
getLocalTime !lid !s = ltime $ sdungeon s EM.! lid

regenCalmDelta :: Actor -> AspectRecord -> State -> Int64
regenCalmDelta b AspectRecord{aMaxCalm} s =
  let calmIncr = oneM  -- normal rate of calm regen
      maxDeltaCalm = xM aMaxCalm - bcalm b
      -- Worry actor by enemies felt (even if not seen)
      -- on the level within 3 steps.
      fact = (EM.! bfid b) . sfactionD $ s
      allFoes = actorRegularList (isAtWar fact) (blid b) s
      isHeard body = not (waitedLastTurn body)
                     && chessDist (bpos b) (bpos body) <= 3
      noisyFoes = filter isHeard allFoes
  in if null noisyFoes
     then min calmIncr maxDeltaCalm
     else minusM  -- even if all calmness spent, keep informing the client

actorInAmbient :: Actor -> State -> Bool
actorInAmbient b s =
  let lvl = (EM.! blid b) . sdungeon $ s
  in Tile.isLit (Kind.coTileSpeedup $ scops s) (lvl `at` bpos b)

actorSkills :: Maybe ActorId -> ActorId -> AspectRecord -> State
            -> Ability.Skills
actorSkills mleader aid ar s =
  let body = getActorBody aid s
      player = gplayer . (EM.! bfid body) . sfactionD $ s
      skillsFromTactic = tacticSkills $ ftactic player
      factionSkills
        | Just aid == mleader = Ability.zeroSkills
        | otherwise = fskillsOther player `Ability.addSkills` skillsFromTactic
      itemSkills = aSkills ar
  in itemSkills `Ability.addSkills` factionSkills

tacticSkills :: Tactic -> Ability.Skills
tacticSkills TExplore = Ability.zeroSkills
tacticSkills TFollow = Ability.zeroSkills
tacticSkills TFollowNoItems = Ability.ignoreItems
tacticSkills TMeleeAndRanged = Ability.meleeAndRanged
tacticSkills TMeleeAdjacent = Ability.meleeAdjacent
tacticSkills TBlock = Ability.blockOnly
tacticSkills TRoam = Ability.zeroSkills
tacticSkills TPatrol = Ability.zeroSkills

-- Check whether an actor can displace an enemy. We assume they are adjacent.
dispEnemy :: ActorId -> ActorId -> Ability.Skills -> State -> Bool
dispEnemy source target actorMaxSk s =
  let hasSupport b =
        let fact = (EM.! bfid b) . sfactionD $ s
            friendlyFid fid = fid == bfid b || isAllied fact fid
            sup = actorRegularList friendlyFid (blid b) s
        in any (adjacent (bpos b) . bpos) sup
      sb = getActorBody source s
      tb = getActorBody target s
  in bproj tb
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

itemToFull :: Kind.COps -> DiscoveryKind -> DiscoveryAspect -> ItemId -> Item
           -> ItemQuant
           -> ItemFull
itemToFull Kind.COps{coitem=Kind.Ops{okind}}
           disco discoAspect iid itemBase (itemK, itemTimer) =
  let itemDisco = case EM.lookup (jkindIx itemBase) disco of
        Nothing -> Nothing
        Just KindMean{..} -> Just ItemDisco{ itemKindId = kmKind
                                           , itemKind = okind kmKind
                                           , itemAspectMean = kmMean
                                           , itemAspect = EM.lookup iid discoAspect }
  in ItemFull {..}

-- Non-durable item that hurts doesn't go into equipment by default,
-- but if it is in equipment or among organs, it's used for melee
-- nevertheless, e.g., thorns.
goesIntoEqp :: Item -> Bool
goesIntoEqp item = IK.Equipable `elem` jfeature item

goesIntoInv :: Item -> Bool
goesIntoInv item = IK.Precious `notElem` jfeature item
                   && not (goesIntoEqp item)

goesIntoSha :: Item -> Bool
goesIntoSha item = IK.Precious `elem` jfeature item
                   && not (goesIntoEqp item)

eqpOverfull :: Actor -> Int -> Bool
eqpOverfull b n = let size = sum $ map fst $ EM.elems $ beqp b
                  in assert (size <= 10 `blame` (b, n, size))
                     $ size + n > 10

eqpFreeN :: Actor -> Int
eqpFreeN b = let size = sum $ map fst $ EM.elems $ beqp b
             in assert (size <= 10 `blame` (b, size))
                $ 10 - size

storeFromC :: Container -> CStore
storeFromC c = case c of
  CFloor{} -> CGround
  CEmbed{} -> CGround
  CActor _ cstore -> cstore
  CTrunk{} -> assert `failure` c

-- | Determine the dungeon level of the container. If the item is in a shared
-- stash, the level depends on which actor asks.
lidFromC :: Container -> State -> LevelId
lidFromC (CFloor lid _) _ = lid
lidFromC (CEmbed lid _) _ = lid
lidFromC (CActor aid _) s = blid $ getActorBody aid s
lidFromC c@CTrunk{} _ = assert `failure` c

aidFromC :: Container -> Maybe ActorId
aidFromC CFloor{} = Nothing
aidFromC CEmbed{} = Nothing
aidFromC (CActor aid _) = Just aid
aidFromC c@CTrunk{} = assert `failure` c
