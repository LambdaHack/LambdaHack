-- | Operations on the 'Actor' type, and related, that need the 'State' type,
-- but not our custom monad types.
module Game.LambdaHack.Common.ActorState
  ( fidActorNotProjGlobalAssocs, actorAssocs, fidActorRegularAssocs
  , fidActorRegularIds, foeRegularAssocs, foeRegularList
  , friendRegularAssocs, friendRegularList, bagAssocs, bagAssocsK
  , posToBig, posToBigAssoc, posToProjs, posToProjAssocs
  , posToAids, posToAidAssocs, calculateTotal, itemPrice, findIid
  , combinedGround, combinedOrgan, combinedEqp
  , combinedItems, combinedFromLore
  , getActorBody, getActorMaxSkills, actorCurrentSkills, canTraverse
  , getCarriedAssocsAndTrunk, getContainerBag
  , getFloorBag, getEmbedBag, getBodyStoreBag, getFactionStashBag
  , mapActorItems_, getActorAssocs, getActorAssocsK
  , memActor, getLocalTime, regenCalmDelta, actorInAmbient, canDeAmbientList
  , dispEnemy, itemToFull, fullAssocs, kitAssocs
  , getItemKindId, getIidKindId, getItemKind, getIidKind
  , getItemKindIdServer, getIidKindIdServer, getItemKindServer, getIidKindServer
  , tileAlterable, lidFromC, posFromC, anyFoeAdj, anyHarmfulFoeAdj
  , adjacentBigAssocs, adjacentProjAssocs, armorHurtBonus, inMelee
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Int (Int64)
import           GHC.Exts (inline)

import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Content.TileKind as TK
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs

fidActorNotProjGlobalAssocs :: FactionId -> State -> [(ActorId, Actor)]
fidActorNotProjGlobalAssocs fid s =
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

fidActorRegularAssocs :: FactionId -> LevelId -> State -> [(ActorId, Actor)]
fidActorRegularAssocs fid = actorRegularAssocs (== fid)

fidActorRegularIds :: FactionId -> LevelId -> State -> [ActorId]
fidActorRegularIds fid lid s =
  map fst $ actorRegularAssocs (== fid) lid s

foeRegularAssocs :: FactionId -> LevelId -> State -> [(ActorId, Actor)]
foeRegularAssocs fid lid s =
  let fact = (EM.! fid) . sfactionD $ s
  in actorRegularAssocs (inline isFoe fid fact) lid s

foeRegularList :: FactionId -> LevelId -> State -> [Actor]
foeRegularList fid lid s =
  let fact = (EM.! fid) . sfactionD $ s
  in map snd $ actorRegularAssocs (inline isFoe fid fact) lid s

friendRegularAssocs :: FactionId -> LevelId -> State -> [(ActorId, Actor)]
friendRegularAssocs fid lid s =
  let fact = (EM.! fid) . sfactionD $ s
  in actorRegularAssocs (inline isFriend fid fact) lid s

friendRegularList :: FactionId -> LevelId -> State -> [Actor]
friendRegularList fid lid s =
  let fact = (EM.! fid) . sfactionD $ s
  in map snd $ actorRegularAssocs (inline isFriend fid fact) lid s

bagAssocs :: State -> ItemBag -> [(ItemId, Item)]
bagAssocs s bag =
  let iidItem iid = (iid, getItemBody iid s)
  in map iidItem $ EM.keys bag

bagAssocsK :: State -> ItemBag -> [(ItemId, (Item, ItemQuant))]
bagAssocsK s bag =
  let iidItem (iid, kit) = (iid, (getItemBody iid s, kit))
  in map iidItem $ EM.assocs bag

posToBig :: Point -> LevelId -> State -> Maybe ActorId
posToBig pos lid s = posToBigLvl pos $ sdungeon s EM.! lid

posToBigAssoc :: Point -> LevelId -> State -> Maybe (ActorId, Actor)
posToBigAssoc pos lid s =
  let maid = posToBigLvl pos $ sdungeon s EM.! lid
  in fmap (\aid -> (aid, getActorBody aid s)) maid

posToProjs :: Point -> LevelId -> State -> [ActorId]
posToProjs pos lid s = posToProjsLvl pos $ sdungeon s EM.! lid

posToProjAssocs :: Point -> LevelId -> State -> [(ActorId, Actor)]
posToProjAssocs pos lid s =
  let l = posToProjsLvl pos $ sdungeon s EM.! lid
  in map (\aid -> (aid, getActorBody aid s)) l

posToAids :: Point -> LevelId -> State -> [ActorId]
posToAids pos lid s = posToAidsLvl pos $ sdungeon s EM.! lid

posToAidAssocs :: Point -> LevelId -> State -> [(ActorId, Actor)]
posToAidAssocs pos lid s =
  let l = posToAidsLvl pos $ sdungeon s EM.! lid
  in map (\aid -> (aid, getActorBody aid s)) l

-- | Calculate loot's worth for a given faction.
calculateTotal :: FactionId -> State -> (ItemBag, Int)
calculateTotal fid s =
  let bag = combinedItems fid s
      items = map (\(iid, (k, _)) -> (getItemBody iid s, k)) $ EM.assocs bag
      price (item, k) = itemPrice k $ getItemKind item s
  in (bag, sum $ map price items)

-- | Price an item, taking count into consideration.
itemPrice :: Int -> IK.ItemKind -> Int
itemPrice jcount itemKind = case lookup IK.VALUABLE $ IK.ifreq itemKind of
  Just k -> jcount * k
  Nothing -> 0

findIid :: ActorId -> FactionId -> ItemId -> State
        -> [(ActorId, (Actor, CStore))]
findIid leader fid iid s =
  let actors = fidActorNotProjGlobalAssocs fid s
      itemsOfActor (aid, b) =
        let itemsOfCStore store =
              let bag = getBodyStoreBag b store s
              in map (\iid2 -> (iid2, (aid, (b, store)))) (EM.keys bag)
            stores = [CEqp, COrgan] ++ [CStash | aid == leader]
        in concatMap itemsOfCStore stores
      items = concatMap itemsOfActor actors
  in map snd $ filter ((== iid) . fst) items

combinedGround :: FactionId -> State -> ItemBag
combinedGround fid s =
  let bs = inline fidActorNotProjGlobalAssocs fid s
  in EM.unionsWith mergeItemQuant
     $ map (\(_, b) -> getFloorBag (blid b) (bpos b) s) bs

-- Trunk not considered (if stolen).
combinedOrgan :: FactionId -> State -> ItemBag
combinedOrgan fid s =
  let bs = inline fidActorNotProjGlobalAssocs fid s
  in EM.unionsWith mergeItemQuant $ map (borgan . snd) bs

combinedEqp :: FactionId -> State -> ItemBag
combinedEqp fid s =
  let bs = inline fidActorNotProjGlobalAssocs fid s
  in EM.unionsWith mergeItemQuant $ map (beqp . snd) bs

-- Trunk not considered (if stolen).
combinedItems :: FactionId -> State -> ItemBag
combinedItems fid s =
  let stashBag = getFactionStashBag fid s
      bs = map snd $ inline fidActorNotProjGlobalAssocs fid s
  in EM.unionsWith mergeItemQuant $ map beqp bs ++ [stashBag]

combinedFromLore :: SLore -> FactionId -> State -> ItemBag
combinedFromLore slore fid s = case slore of
  SItem -> combinedItems fid s
  SOrgan -> combinedOrgan fid s
  STrunk -> combinedOrgan fid s
  SCondition -> combinedOrgan fid s
  SBlast -> EM.empty
  SEmbed -> EM.empty

getActorBody :: ActorId -> State -> Actor
{-# INLINE getActorBody #-}
getActorBody aid s = sactorD s EM.! aid

-- For now, faction and doctrine skill modifiers only change
-- the stats that affect permitted actions (@SkMove..SkApply@),
-- so the expensive @actorCurrentSkills@ operation doesn't need to be used
-- when checking the other skills, e.g., for FOV calculations,
-- and the @getActorMaxSkills@ cheap operation suffices.
-- (@ModeKind@ content is not currently validated in this respect.)
getActorMaxSkills :: ActorId -> State -> Ability.Skills
{-# INLINE getActorMaxSkills #-}
getActorMaxSkills aid s = sactorMaxSkills s EM.! aid

actorCurrentSkills :: Maybe ActorId -> ActorId -> State -> Ability.Skills
actorCurrentSkills mleader aid s =
  let body = getActorBody aid s
      actorMaxSk = getActorMaxSkills aid s
      player = gplayer . (EM.! bfid body) . sfactionD $ s
      skillsFromDoctrine = Ability.doctrineSkills $ fdoctrine player
      factionSkills
        | Just aid == mleader = Ability.zeroSkills
        | otherwise = fskillsOther player `Ability.addSkills` skillsFromDoctrine
  in actorMaxSk `Ability.addSkills` factionSkills

-- Check that the actor can move, also between levels and through doors.
-- Otherwise, it's too awkward for human player to control, e.g.,
-- being stuck in a room with revolving doors closing after one turn
-- and the player needing to micromanage opening such doors with
-- another actor all the time. Completely immovable actors
-- e.g., an impregnable surveillance camera in a crowded corridor,
-- are less of a problem due to micromanagment, but more due to
-- the constant disturbing of other actor's running, etc.
canTraverse :: ActorId -> State -> Bool
canTraverse aid s =
  let actorMaxSk = getActorMaxSkills aid s
  in Ability.getSk Ability.SkMove actorMaxSk > 0
     && Ability.getSk Ability.SkAlter actorMaxSk >= fromEnum TK.talterForStairs

getCarriedAssocsAndTrunk :: Actor -> State -> [(ItemId, Item)]
getCarriedAssocsAndTrunk b s =
  -- The trunk is important for a case of spotting a caught projectile
  -- with a stolen projecting item. This actually does happen.
  let trunk = EM.singleton (btrunk b) (1, [])
  in bagAssocs s $ EM.unionsWith const [beqp b, borgan b, trunk]

getContainerBag :: Container -> State -> ItemBag
getContainerBag c s = case c of
  CFloor lid p -> getFloorBag lid p s
  CEmbed lid p -> getEmbedBag lid p s
  CActor aid cstore -> let b = getActorBody aid s
                       in getBodyStoreBag b cstore s
  CTrunk{} -> EM.empty  -- for dummy/test/analytics cases

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
    CStash -> getFactionStashBag (bfid b) s

getFactionStashBag :: FactionId -> State -> ItemBag
getFactionStashBag fid s = case gstash $ sfactionD s EM.! fid of
  Just (lid, pos) -> getFloorBag lid pos s
  Nothing -> EM.empty

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
      actorMaxSk = getActorMaxSkills aid s
      maxDeltaCalm = xM (Ability.getSk Ability.SkMaxCalm actorMaxSk)
                     - bcalm body
      fact = (EM.! bfid body) . sfactionD $ s
      -- Worry actor by non-projectile enemies felt (even if not seen)
      -- on the level within 3 steps. Even dying, but not hiding in wait.
      isHeardFoe (!p, aid2) =
        let b = getActorBody aid2 s
        in inline chessDist p (bpos body) <= 3
           && not (actorWaitsOrSleeps b)  -- uncommon
           && inline isFoe (bfid body) fact (bfid b)  -- costly
      actorRelaxed = deltaBenign $ bcalmDelta body
      actorWasRelaxed = deltaWasBenign $ bcalmDelta body
  in if | not actorRelaxed -> 0
            -- if no foes around, do not compensate and obscure distress,
            -- otherwise, don't increase delta further and suggest grave harm;
            -- note that in the effect, an actor that first hears distant
            -- action and then hears nearby enemy, won't notice the latter,
            -- which can be justified by distraction and is KISS and tactical
        | any isHeardFoe $ EM.assocs $ lbig $ sdungeon s EM.! blid body ->
          minusM1  -- even if all calmness spent, keep informing the client;
                   -- from above we know delta won't get too large here
        | actorWasRelaxed -> min calmIncr (max 0 maxDeltaCalm)
                                             -- if Calm is over max
        | otherwise -> 0  -- don't regenerate if shortly after stress, to make
                          -- waking up actors via bad stealth easier

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

-- Check whether an actor can displace another. We assume they are adjacent
-- and they are foes.
dispEnemy :: ActorId -> ActorId -> Ability.Skills -> State -> Bool
dispEnemy source target actorMaxSk s =
  let hasBackup b =
        let adjAssocs = adjacentBigAssocs b s
            fact = sfactionD s EM.! bfid b
            friend (_, b2) = isFriend (bfid b) fact (bfid b2) && bhp b2 > 0
        in any friend adjAssocs
      sb = getActorBody source s
      tb = getActorBody target s
  in bproj tb
     || not (actorDying tb
             || actorWaits tb
             || Ability.getSk Ability.SkMove actorMaxSk <= 0
                  -- sometimes this comes from sleep, but it's transient
                  -- and if we made exception for sleep, we would displace
                  -- immobile sleeping actors
             || hasBackup sb && hasBackup tb)  -- solo actors are flexible

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
  IdentityCovered ix _ik -> fromMaybe (error $ show $ jkind item)
                                      (EM.lookup ix $ sdiscoKind s)

getIidKindIdServer :: ItemId -> State -> ContentId IK.ItemKind
getIidKindIdServer iid s = getItemKindIdServer (getItemBody iid s) s

getItemKindServer :: Item -> State -> IK.ItemKind
getItemKindServer item s = okind (coitem $ scops s) $ getItemKindIdServer item s

getIidKindServer :: ItemId -> State -> IK.ItemKind
getIidKindServer iid s = getItemKindServer (getItemBody iid s) s

tileAlterable :: LevelId -> Point -> State -> Bool
tileAlterable lid pos s =
  let COps{coTileSpeedup} = scops s
      embeds = getEmbedBag lid pos s
      lvl = sdungeon s EM.! lid
      t = lvl `at` pos
      triggerable = any (\iid -> not $ null $ IK.ieffects $ getIidKind iid s)
                        (EM.keys embeds)
  in Tile.isModifiable coTileSpeedup t || triggerable

-- | Determine the dungeon level of the container. If the item is in a shared
-- stash, the level depends on which actor asks.
lidFromC :: Container -> State -> LevelId
lidFromC (CFloor lid _) _ = lid
lidFromC (CEmbed lid _) _ = lid
lidFromC (CActor aid _) s = blid $ getActorBody aid s
lidFromC (CTrunk _ lid _) _ = lid

posFromC :: Container -> State -> Point
posFromC (CFloor _ pos) _ = pos
posFromC (CEmbed _ pos) _ = pos
posFromC (CActor aid _) s = bpos $ getActorBody aid s
posFromC c@CTrunk{} _ = error $ "" `showFailure` c

vicinityFoeAdj :: ((ActorId, Actor) -> Bool) -> ActorId -> State -> Bool
{-# INLINE vicinityFoeAdj #-}
vicinityFoeAdj predicate aid s =
  let body = getActorBody aid s
      lvl = (EM.! blid body) . sdungeon $ s
      fact = (EM.! bfid body) . sfactionD $ s
      f !p = case posToBigLvl p lvl of
        Nothing -> False
        Just aid2 -> let b2 = getActorBody aid2 s
                     in isFoe (bfid body) fact (bfid b2)
                        && predicate (aid2, b2)
      h !p = case posToProjsLvl p lvl of
        [] -> False
        aid2 : _ -> isFoe (bfid body) fact . bfid $ getActorBody aid2 s
  in any (\p -> f p || h p) $ vicinityUnsafe $ bpos body

-- | Require that any non-dying foe is adjacent. We include even
-- projectiles that explode when stricken down, because they can be caught
-- and then they don't explode, so it makes sense to focus on handling them.
-- If there are many projectiles in a single adjacent position, we only test
-- the first one, the one that would be hit in melee (this is not optimal
-- if the actor would need to flee instead of meleeing, but fleeing
-- with *many* projectiles adjacent is a possible waste of a move anyway).
anyFoeAdj :: ActorId -> State -> Bool
anyFoeAdj = vicinityFoeAdj (const True)

anyHarmfulFoeAdj :: ActorMaxSkills -> ActorId -> State -> Bool
anyHarmfulFoeAdj actorMaxSkills =
  vicinityFoeAdj (\(aid2, b2) -> actorWorthKilling actorMaxSkills aid2 b2)

adjacentBigAssocs :: Actor -> State -> [(ActorId, Actor)]
{-# INLINE adjacentBigAssocs #-}
adjacentBigAssocs body s =
  let lvl = (EM.! blid body) . sdungeon $ s
      f !p = posToBigLvl p lvl
      g !aid = (aid, getActorBody aid s)
  in map g $ mapMaybe f $ vicinityUnsafe $ bpos body

adjacentProjAssocs :: Actor -> State -> [(ActorId, Actor)]
{-# INLINE adjacentProjAssocs #-}
adjacentProjAssocs body s =
  let lvl = (EM.! blid body) . sdungeon $ s
      f !p = posToProjsLvl p lvl
      g !aid = (aid, getActorBody aid s)
  in map g $ concatMap f $ vicinityUnsafe $ bpos body

armorHurtBonus :: ActorId -> ActorId -> State -> Int
armorHurtBonus source target s =
  let sb = getActorBody source s
      sMaxSk = getActorMaxSkills source s
      tMaxSk = getActorMaxSkills target s
  in armorHurtCalculation (bproj sb) sMaxSk tMaxSk

-- | Check if any non-dying foe is adjacent to any of our normal actors
-- and either is a projectile (can fly into them) or can harm them
-- via melee or can attack from a distance. Otherwise no point meleeing him.
-- This is regardless of whether our actor can melee or just needs to flee,
-- in which case alert is needed so that he is not slowed down by others.
-- However, if our actor can't move nor melee, no real combat is taking place.
-- This is needed only by AI and computed as lazily as possible.
inMelee :: ActorMaxSkills -> FactionId -> LevelId -> State -> Bool
inMelee !actorMaxSkills !fid !lid s =
  let fact = sfactionD s EM.! fid
      f (!aid, !b) =
        blid b == lid
        && inline isFoe fid fact (bfid b)  -- costly
        && actorWorthKilling actorMaxSkills aid b
      allFoes = filter f $ EM.assocs $ sactorD s
      g (!aid, !b) = bfid b == fid
                     && blid b == lid
                     && not (bproj b)
                     && bhp b > 0
                     && let actorMaxSk = actorMaxSkills EM.! aid
                        in Ability.getSk Ability.SkMove actorMaxSk > 0
                           || actorCanMeleeToHarm actorMaxSkills aid b
      allOurs = filter g $ EM.assocs $ sactorD s
      -- We assume foes are less numerous, even though they may come
      -- from multiple factions and they contain projectiles,
      -- because we see all our actors, while many foes may be hidden.
      -- Consequently, we allocate the set of foe positions
      -- and avoid allocating ours, by iterating over our actors.
      -- This in O(mn) instead of O(m+n), but it allocates
      -- less and multiplicative constants are lower.
      -- We inspect adjacent locations of foe positions, not of ours,
      -- thus increasing allocation a bit, but not by much, because
      -- the set should be rather saturated.
      -- If there are no foes in sight, we don't iterate at all.
      setFoeVicinity =
        ES.fromList $ concatMap (vicinityUnsafe . bpos . snd) allFoes
  in not (ES.null setFoeVicinity)  -- shortcut
     && any (\(_, b) -> bpos b `ES.member` setFoeVicinity) allOurs
