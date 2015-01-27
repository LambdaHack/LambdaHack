-- | Operations on the 'Actor' type that need the 'State' type,
-- but not the 'Action' type.
-- TODO: Document an export list after it's rewritten according to #17.
module Game.LambdaHack.Common.ActorState
  ( fidActorNotProjAssocs, fidActorNotProjList
  , actorAssocsLvl, actorAssocs, actorList
  , actorRegularAssocsLvl, actorRegularAssocs, actorRegularList
  , bagAssocs, bagAssocsK, calculateTotal
  , mergeItemQuant, sharedAllOwned, sharedAllOwnedFid, findIid
  , getCBag, getActorBag, getBodyActorBag, getActorAssocs
  , nearbyFreePoints, whereTo, getCarriedAssocs
  , posToActors, posToActor, getItemBody, memActor, getActorBody
  , tryFindHeroK, getLocalTime, itemPrice, regenCalmDelta
  , actorInAmbient, actorSkills, maxActorSkills, dispEnemy
  , fullAssocs, itemToFull, goesIntoInv, goesIntoSha, eqpOverfull
  , storeFromC, lidFromC, aidFromC, hasCharge
  , strongestMelee, isMelee, isMeleeEqp
  ) where

import Control.Applicative
import Control.Exception.Assert.Sugar
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import Data.Int (Int64)
import Data.List
import Data.Maybe
import qualified Data.Ord as Ord

import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Actor
import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
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
fidActorNotProjList fid s = map snd $ fidActorNotProjAssocs fid s

actorAssocsLvl :: (FactionId -> Bool) -> Level -> ActorDict
               -> [(ActorId, Actor)]
actorAssocsLvl p lvl actorD =
  mapMaybe (\aid -> let b = actorD EM.! aid
                    in if p (bfid b)
                       then Just (aid, b)
                       else Nothing)
  $ concat $ EM.elems $ lprio lvl

actorAssocs :: (FactionId -> Bool) -> LevelId -> State
            -> [(ActorId, Actor)]
actorAssocs p lid s =
  actorAssocsLvl p (sdungeon s EM.! lid) (sactorD s)

actorList :: (FactionId -> Bool) -> LevelId -> State
          -> [Actor]
actorList p lid s = map snd $ actorAssocs p lid s

actorRegularAssocsLvl :: (FactionId -> Bool) -> Level -> ActorDict
                      -> [(ActorId, Actor)]
actorRegularAssocsLvl p lvl actorD =
  mapMaybe (\aid -> let b = actorD EM.! aid
                    in if not (bproj b) && bhp b > 0 && p (bfid b)
                       then Just (aid, b)
                       else Nothing)
  $ concat $ EM.elems $ lprio lvl

actorRegularAssocs :: (FactionId -> Bool) -> LevelId -> State
                   -> [(ActorId, Actor)]
actorRegularAssocs p lid s =
  actorRegularAssocsLvl p (sdungeon s EM.! lid) (sactorD s)

actorRegularList :: (FactionId -> Bool) -> LevelId -> State
                 -> [Actor]
actorRegularList p lid s = map snd $ actorRegularAssocs p lid s

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

-- | Finds an actor at a position on the current level.
posToActor :: Point -> LevelId -> State
           -> Maybe ((ActorId, Actor), [(ItemId, Item)])
posToActor pos lid s = listToMaybe $ posToActors pos lid s

posToActors :: Point -> LevelId -> State
            -> [((ActorId, Actor), [(ItemId, Item)])]
posToActors pos lid s =
  let as = actorAssocs (const True) lid s
      aps = filter (\(_, b) -> bpos b == pos) as
      g (aid, b) = ( (aid, b)
                   , bagAssocs s (binv b)
                     ++ bagAssocs s (beqp b)
                     ++ bagAssocs s (borgan b) )
      l = map g aps
  in assert (length l <= 1 || all (bproj . snd . fst) l
             `blame` "many actors at the same position" `twith` l)
     l

nearbyFreePoints :: (Kind.Id TileKind -> Bool) -> Point -> LevelId -> State
                 -> [Point]
nearbyFreePoints f start lid s =
  let Kind.COps{cotile} = scops s
      lvl@Level{lxsize, lysize} = sdungeon s EM.! lid
      as = actorList (const True) lid s
      good p = f (lvl `at` p)
               && Tile.isWalkable cotile (lvl `at` p)
               && unoccupied as p
      ps = nub $ start : concatMap (vicinity lxsize lysize) ps
  in filter good ps

-- | Calculate loot's worth for a faction of a given actor.
calculateTotal :: Actor -> State -> (ItemBag, Int)
calculateTotal body s =
  let bag = sharedAllOwned False body s
      items = map (\(iid, (k, _)) -> (getItemBody iid s, k)) $ EM.assocs bag
  in (bag, sum $ map itemPrice items)

mergeItemQuant :: ItemQuant -> ItemQuant -> ItemQuant
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

sharedOrgan :: Actor -> State -> ItemBag
sharedOrgan body s =
  let bs = fidActorNotProjList (bfid body) s
  in EM.unionsWith mergeItemQuant
     $ map borgan $ if null bs then [body] else bs

sharedAllOwned :: Bool -> Actor -> State -> ItemBag
sharedAllOwned organs body s =
  let shaBag = gsha $ sfactionD s EM.! bfid body
  in EM.unionsWith mergeItemQuant
     $ [sharedEqp body s, sharedInv body s, shaBag]
       ++ [sharedOrgan body s | organs]

sharedAllOwnedFid :: Bool -> FactionId -> State -> ItemBag
sharedAllOwnedFid organs fid s =
  let shaBag = gsha $ sfactionD s EM.! fid
      bs = fidActorNotProjList fid s
  in EM.unionsWith mergeItemQuant
     $ map binv bs ++ map beqp bs ++ [shaBag]
       ++ if organs then map borgan bs else []

findIid :: ActorId -> FactionId -> ItemId -> State -> [(Actor, CStore)]
findIid leader fid iid s =
  let actors = fidActorNotProjAssocs fid s
      itemsOfActor (aid, b) =
        let itemsOfCStore store =
              let bag = getBodyActorBag b store s
              in map (\iid2 -> (iid2, (b, store))) (EM.keys bag)
            stores = [CInv, CEqp] ++ [CSha | aid == leader]
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

tryFindHeroK :: State -> FactionId -> Int -> Maybe (ActorId, Actor)
tryFindHeroK s fact k =
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
                    -- ^ target level and the position of its receiving stairs
whereTo lid pos k dungeon = assert (k /= 0) $
  let lvl = dungeon EM.! lid
      stairs = (if k < 0 then snd else fst) (lstair lvl)
      defaultStairs = 0  -- for ascending via, e.g., spells
      mindex = elemIndex pos stairs
      i = fromMaybe defaultStairs mindex
  in case ascendInBranch dungeon k lid of
    [] | isNothing mindex -> (lid, pos)  -- spell fizzles
    [] -> assert `failure` "no dungeon level to go to" `twith` (lid, pos, k)
    ln : _ -> let lvlTgt = dungeon EM.! ln
                  stairsTgt = (if k < 0 then fst else snd) (lstair lvlTgt)
              in if length stairsTgt < i + 1
                 then assert `failure` "no stairs at index"
                             `twith` (lid, pos, k, ln, stairsTgt, i)
                 else (ln, stairsTgt !! i)

-- * The operations below disregard levels other than the current.

-- | Gets actor body from the current level. Error if not found.
getActorBody :: ActorId -> State -> Actor
getActorBody aid s =
  let assFail = assert `failure` "body not found" `twith` (aid, s)
  in EM.findWithDefault assFail aid $ sactorD s

getCarriedAssocs :: Actor -> State -> [(ItemId, Item)]
getCarriedAssocs b s =
  bagAssocs s $ EM.unionsWith const [binv b, beqp b, borgan b]

getCBag :: Container -> State -> ItemBag
{-# INLINE getCBag #-}
getCBag c s = case c of
  CFloor lid p -> EM.findWithDefault EM.empty p
                  $ lfloor (sdungeon s EM.! lid)
  CEmbed lid p -> EM.findWithDefault EM.empty p
                  $ lembed (sdungeon s EM.! lid)
  CActor aid cstore -> getActorBag aid cstore s
  CTrunk{} -> assert `failure` c

getActorBag :: ActorId -> CStore -> State -> ItemBag
{-# INLINE getActorBag #-}
getActorBag aid cstore s =
  let b = getActorBody aid s
  in getBodyActorBag b cstore s

getBodyActorBag :: Actor -> CStore -> State -> ItemBag
{-# INLINE getBodyActorBag #-}
getBodyActorBag b cstore s =
  case cstore of
    CGround -> EM.findWithDefault EM.empty (bpos b)
               $ lfloor (sdungeon s EM.! blid b)
    COrgan -> borgan b
    CEqp -> beqp b
    CInv -> binv b
    CSha -> gsha $ sfactionD s EM.! bfid b

getActorAssocs :: ActorId -> CStore -> State -> [(ItemId, Item)]
getActorAssocs aid cstore s = bagAssocs s $ getActorBag aid cstore s

getActorAssocsK :: ActorId -> CStore -> State -> [(ItemId, (Item, ItemQuant))]
getActorAssocsK aid cstore s = bagAssocsK s $ getActorBag aid cstore s

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

regenCalmDelta :: Actor -> [ItemFull] -> State -> Int64
regenCalmDelta b activeItems s =
  let calmMax = sumSlotNoFilter IK.EqpSlotAddMaxCalm activeItems
      calmIncr = oneM  -- normal rate of calm regen
      maxDeltaCalm = xM calmMax - bcalm b
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
  let Kind.COps{cotile} = scops s
      lvl = (EM.! blid b) . sdungeon $ s
  in Tile.isLit cotile (lvl `at` bpos b)

actorSkills :: Maybe ActorId -> ActorId -> [ItemFull] -> State -> Ability.Skills
actorSkills mleader aid activeItems s =
  let body = getActorBody aid s
      fact = (EM.! bfid body) . sfactionD $ s
      factionSkills
        | Just aid == mleader = Ability.zeroSkills
        | otherwise = fskillsOther $ gplayer fact
      itemSkills = sumSkills activeItems
  in itemSkills `Ability.addSkills` factionSkills

maxActorSkills :: ActorId -> [ItemFull] -> State -> Ability.Skills
maxActorSkills aid activeItems s =
  let body = getActorBody aid s
      fact = (EM.! bfid body) . sfactionD $ s
      factionSkills = Ability.maxSkills Ability.zeroSkills
                                        (fskillsOther $ gplayer fact)
      itemSkills = sumSkills activeItems
  in itemSkills `Ability.addSkills` factionSkills

-- Check whether an actor can displace an enemy. We assume they are adjacent.
dispEnemy :: ActorId -> ActorId -> [ItemFull] -> State -> Bool
dispEnemy source target activeItems s =
  let hasSupport b =
        let fact = (EM.! bfid b) . sfactionD $ s
            friendlyFid fid = fid == bfid b || isAllied fact fid
            sup = actorRegularList friendlyFid (blid b) s
        in any (adjacent (bpos b) . bpos) sup
      actorSk = maxActorSkills target activeItems s
      sb = getActorBody source s
      tb = getActorBody target s
  in bproj tb
     || not (actorDying tb
             || braced tb
             || EM.findWithDefault 0 Ability.AbDisplace actorSk <= 0
                && EM.findWithDefault 0 Ability.AbMove actorSk <= 0
             || hasSupport sb && hasSupport tb)  -- solo actors are flexible

fullAssocs :: Kind.COps -> DiscoveryKind -> DiscoveryEffect
           -> ActorId -> [CStore] -> State
           -> [(ItemId, ItemFull)]
fullAssocs cops disco discoEffect aid cstores s =
  let allAssocs = concatMap (\cstore -> getActorAssocsK aid cstore s) cstores
      iToFull (iid, (item, kit)) =
        (iid, itemToFull cops disco discoEffect iid item kit)
  in map iToFull allAssocs

itemToFull :: Kind.COps -> DiscoveryKind -> DiscoveryEffect -> ItemId -> Item
           -> ItemQuant
           -> ItemFull
itemToFull Kind.COps{coitem=Kind.Ops{okind}}
           disco discoEffect iid itemBase (itemK, itemTimer) =
  let itemDisco = case EM.lookup (jkindIx itemBase) disco of
        Nothing -> Nothing
        Just itemKindId -> Just ItemDisco{ itemKindId
                                         , itemKind = okind itemKindId
                                         , itemAE = EM.lookup iid discoEffect }
  in ItemFull {..}

-- Non-durable item that hurts doesn't go into equipment by default,
-- but if it is in equipment, it's used for melee nevertheless, e.g., thorns.
goesIntoInv :: ItemFull -> Bool
goesIntoInv itemFull = not (isJust (strengthEqpSlot $ itemBase itemFull)
                            || isMeleeEqp itemFull)

goesIntoSha :: ItemFull -> Bool
goesIntoSha itemFull = IK.Precious `elem` jfeature (itemBase itemFull)
                       && goesIntoInv itemFull

eqpOverfull :: Actor -> Int -> Bool
eqpOverfull b n = let size = sum $ map fst $ EM.elems $ beqp b
                  in assert (size <= 10 `blame` (b, n, size))
                     $ size + n > 10

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

hasCharge :: Time -> ItemFull -> Bool
hasCharge localTime itemFull@ItemFull{..} =
  let it1 = case strengthFromEqpSlot IK.EqpSlotTimeout itemFull of
        Nothing -> []  -- if item not IDed, assume no timeout, to ID by use
        Just timeout ->
          let timeoutTurns = timeDeltaScale (Delta timeTurn) timeout
              charging startT = timeShift startT timeoutTurns > localTime
          in filter charging itemTimer
      len = length it1
  in len < itemK

strengthMelee :: Bool -> Time -> ItemFull -> Maybe Int
strengthMelee effectBonus localTime itemFull =
  let durable = IK.Durable `elem` jfeature (itemBase itemFull)
      recharged = hasCharge localTime itemFull
      -- We assume extra weapon effects are useful and so such
      -- weapons are preferred over weapons with no effects.
      -- If the player doesn't like a particular weapon's extra effect,
      -- he has to manage this manually.
      p (IK.Hurt d) = [Dice.meanDice d]
      p (IK.Burn d) = [Dice.meanDice d]
      p IK.NoEffect{} = []
      p IK.OnSmash{} = []
      -- We assume the weapon is still worth using, even if some effects
      -- are charging; in particular, we assume Hurt or Burn are not
      -- under Recharging.
      p IK.Recharging{} = [100 | recharged && effectBonus]
      p IK.Temporary{} = []
      p _ = [100 | effectBonus]
      psum = sum (strengthEffect p itemFull)
  in if not (isMelee itemFull) || psum == 0
     then Nothing
     else Just $ psum + if durable then 1000 else 0

strongestMelee :: Bool -> Time -> [(ItemId, ItemFull)]
               -> [(Int, (ItemId, ItemFull))]
strongestMelee effectBonus localTime is =
  let f = strengthMelee effectBonus localTime
      g (iid, itemFull) = (\v -> (v, (iid, itemFull))) <$> f itemFull
  in sortBy (flip $ Ord.comparing fst) $ mapMaybe g is

isMelee :: ItemFull -> Bool
isMelee itemFull =
  let p IK.Hurt{} = True
      p IK.Burn{} = True
      p _ = False
  in case itemDisco itemFull of
    Just ItemDisco{itemAE=Just ItemAspectEffect{jeffects}} ->
      any p jeffects
    Just ItemDisco{itemKind=IK.ItemKind{IK.ieffects}} ->
      any p ieffects
    Nothing -> False

-- Melee weapon so good (durable) that goes into equipment by default.
isMeleeEqp :: ItemFull -> Bool
isMeleeEqp itemFull =
  let durable = IK.Durable `elem` jfeature (itemBase itemFull)
  in isMelee itemFull && durable
