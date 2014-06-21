-- | Operations on the 'Actor' type that need the 'State' type,
-- but not the 'Action' type.
-- TODO: Document an export list after it's rewritten according to #17.
module Game.LambdaHack.Common.ActorState
  ( fidActorNotProjAssocs, fidActorNotProjList
  , actorAssocsLvl, actorAssocs, actorList
  , actorRegularAssocsLvl, actorRegularAssocs, actorRegularList
  , bagAssocs, bagAssocsK, calculateTotal
  , sharedAllOwned, sharedAllOwnedFid
  , getCBag, getActorBag, getActorAssocs
  , nearbyFreePoints, whereTo, getCarriedAssocs
  , posToActors, posToActor, getItemBody, memActor, getActorBody
  , tryFindHeroK, getLocalTime, isSpawnFaction
  , itemPrice, calmEnough, hpEnough, regenCalmDelta
  , actorShines, actorInAmbient, dispEnemy, radiusBlind
  , fullAssocs, itemToFull, goesIntoInv, eqpOverfull
  ) where

import Control.Arrow (second)
import Control.Exception.Assert.Sugar
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import Data.List
import Data.Maybe

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemFeature as IF
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.TileKind

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
  fromMaybe (assert `failure` "item body not found"
                    `twith` (iid, s)) $ EM.lookup iid $ sitemD s

bagAssocs :: State -> ItemBag -> [(ItemId, Item)]
bagAssocs s bag =
  let iidItem iid = (iid, getItemBody iid s)
  in map iidItem $ EM.keys bag

bagAssocsK :: State -> ItemBag -> [(ItemId, (Item, Int))]
bagAssocsK s bag =
  let iidItem (iid, k) = (iid, (getItemBody iid s, k))
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
                     ++ bagAssocs s (bbody b) )
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
  let bag = sharedAllOwned body s
      items = map (\(iid, k) -> (getItemBody iid s, k)) $ EM.assocs bag
  in (bag, sum $ map itemPrice items)

sharedInv :: Actor -> State -> ItemBag
sharedInv body s =
  let bs = fidActorNotProjList (bfid body) s
  in EM.unionsWith (+) $ map binv $ if null bs then [body] else bs

sharedEqp :: Actor -> State -> ItemBag
sharedEqp body s =
  let bs = fidActorNotProjList (bfid body) s
  in EM.unionsWith (+) $ map beqp $ if null bs then [body] else bs

sharedAllOwned :: Actor -> State -> ItemBag
sharedAllOwned body s =
  let shaBag = gsha $ sfactionD s EM.! bfid body
  in EM.unionsWith (+) [sharedInv body s, sharedEqp body s, shaBag]

sharedAllOwnedFid :: FactionId -> State -> ItemBag
sharedAllOwnedFid fid s =
  let shaBag = gsha $ sfactionD s EM.! fid
      bs = fidActorNotProjList fid s
  in EM.unionsWith (+) $ map binv bs ++ map beqp bs ++ [shaBag]

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
  fromMaybe (assert `failure` "body not found" `twith` (aid, s))
  $ EM.lookup aid $ sactorD s

getCarriedAssocs :: Actor -> State -> [(ItemId, Item)]
getCarriedAssocs b s =
  bagAssocs s $ EM.unionsWith (const) [binv b, beqp b, bbody b]

getCBag :: Container -> State -> ItemBag
getCBag c s = case c of
  CFloor lid p -> sdungeon s EM.! lid `atI` p
  CActor aid cstore -> getActorBag aid cstore s

getActorBag :: ActorId -> CStore -> State -> ItemBag
getActorBag aid cstore s =
  let b = getActorBody aid s
  in case cstore of
    CGround -> sdungeon s EM.! blid b `atI` bpos b
    CBody -> bbody b
    CEqp -> beqp b
    CInv -> binv b
    CSha -> gsha $ sfactionD s EM.! bfid b

getActorAssocs :: ActorId -> CStore -> State -> [(ItemId, Item)]
getActorAssocs aid cstore s = bagAssocs s $ getActorBag aid cstore s

getActorAssocsK :: ActorId -> CStore -> State -> [(ItemId, (Item, Int))]
getActorAssocsK aid cstore s = bagAssocsK s $ getActorBag aid cstore s

-- | Checks if the actor is present on the current level.
-- The order of argument here and in other functions is set to allow
--
-- > b <- getsState (memActor a)
memActor :: ActorId -> LevelId -> State -> Bool
memActor aid lid s =
  maybe False ((== lid) . blid) $ EM.lookup aid $ sactorD s

calmEnough :: Actor -> ActorKind -> Bool
calmEnough b kind =
  let calmMax = amaxCalm kind
      calmCur = bcalm b
  in 2 * calmMax <= 3 * calmCur

hpEnough :: Actor -> ActorKind -> Bool
hpEnough b kind =
  let hpMax = amaxHP kind
      hpCur = bhp b
  in 2 * hpMax <= 3 * hpCur

-- | Get current time from the dungeon data.
getLocalTime :: LevelId -> State -> Time
getLocalTime lid s = ltime $ sdungeon s EM.! lid

-- | Tell whether the faction can spawn actors.
isSpawnFaction :: FactionId -> State -> Bool
isSpawnFaction fid s = isSpawnFact $ sfactionD s EM.! fid

regenCalmDelta :: Actor -> State -> Int
regenCalmDelta b s =
  let Kind.COps{coactor=Kind.Ops{okind}} = scops s
      ak = okind $ bkind b
      calmIncr = 1 -- normal rate of calm regen
      maxDeltaCalm = amaxCalm ak - bcalm b
      -- Worry actor by enemies felt (even if not seen)
      -- on the level within 3 tiles.
      fact = (EM.! bfid b) . sfactionD $ s
      allFoes = actorRegularList (isAtWar fact) (blid b) $ s
      isHeard body = not (waitedLastTurn body)
                     && chessDist (bpos b) (bpos body) <= 3
      noisyFoes = filter isHeard allFoes
  in if null noisyFoes
     then min calmIncr maxDeltaCalm
     else -1  -- even if all calmness spent, keep informing the client

actorShines :: Actor -> State -> Bool
actorShines b s =
  let eqpAssocs = bagAssocsK s $ beqp b
      bodyAssocs = bagAssocsK s $ bbody b
      floorAssocs = bagAssocsK s $ sdungeon s EM.! blid b `atI` bpos b
  in 0 /= (sumSlotNoFilter IF.EqpSlotLight $ map (second itemNoDisco)
           $ eqpAssocs ++ bodyAssocs ++ floorAssocs)

actorInAmbient :: Actor -> State -> Bool
actorInAmbient b s =
  let Kind.COps{cotile} = scops s
      lvl = (EM.! blid b) . sdungeon $ s
  in Tile.isLit cotile (lvl `at` bpos b)

-- Check whether an actor can displace an enemy. We assume they are adjacent.
dispEnemy :: Actor -> Actor -> State -> Bool
dispEnemy sb tb s =
  let hasSupport b =
        let fact = (EM.! bfid b) . sfactionD $ s
            friendlyFid fid = fid == bfid b || isAllied fact fid
            sup = actorRegularList friendlyFid (blid b) s
        in any (adjacent (bpos b) . bpos) sup
  in bproj tb
     || not (actorDying tb
             || braced tb
             || hasSupport sb && hasSupport tb)  -- solo actors are flexible

-- | Determine if the sight radius is high enough to deem the actor capable
-- of projecting items and similar activities. Otherwise, the actor
-- is assumed to use a combination of peripherial vision, hearing, etc.,
-- and not the actual focused, long-distance sight sense.
radiusBlind :: Int -> Bool
radiusBlind strongestSight = strongestSight < 4

fullAssocs :: Kind.COps -> Discovery -> DiscoAE
           -> ActorId -> [CStore] -> State
           -> [(ItemId, ItemFull)]
fullAssocs cops disco discoAE aid cstores s =
  let allAssocs = concatMap (\cstore -> getActorAssocsK aid cstore s) cstores
      iToFull (iid, (item, k)) =
        (iid, itemToFull cops disco discoAE iid item k)
  in map iToFull allAssocs

itemToFull :: Kind.COps -> Discovery -> DiscoAE -> ItemId -> Item -> Int
           -> ItemFull
itemToFull Kind.COps{coitem=Kind.Ops{okind}}
           disco discoAE iid itemBase itemK =
  let itemDisco = case EM.lookup (jkindIx itemBase) disco of
        Nothing -> Nothing
        Just itemKindId -> Just ItemDisco{ itemKindId
                                         , itemKind = okind itemKindId
                                         , itemAE = EM.lookup iid discoAE }
  in ItemFull {..}

goesIntoInv :: Item -> Bool
goesIntoInv item = isNothing $ strengthEqpSlot item

eqpOverfull :: Actor -> Int -> Bool
eqpOverfull b n = let size = sum $ EM.elems $ beqp b
                  in assert (size <= 10 `blame` (b, n, size))
                     $ size + n > 10
