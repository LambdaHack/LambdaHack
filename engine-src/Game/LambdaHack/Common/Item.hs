{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, TupleSections #-}
-- | Weapons, treasure and all the other items in the game.
module Game.LambdaHack.Common.Item
  ( Item(..), ItemIdentity(..)
  , ItemKindIx, ItemDisco(..), ItemFull(..), ItemFullKit
  , DiscoveryKind, DiscoveryAspect, ItemIxMap, Benefit(..), DiscoveryBenefit
  , ItemTimer, ItemTimers, ItemQuant, ItemBag, ItemDict
  , toItemKindIx, quantSingle, itemToFull6, aspectRecordFull, strongestSlot
  , itemTimerZero, createItemTimer, shiftItemTimer
  , deltaOfItemTimer, charging, ncharges, hasCharge
  , strongestMelee, unknownMeleeBonus, unknownSpeedBonus
  , conditionMeleeBonus, conditionSpeedBonus, armorHurtCalculation
  , mergeItemQuant, listToolsToConsume, subtractIidfromGrps, sortIids
  , TileAction (..), parseTileAction
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , valueAtEqpSlot, unknownAspect, countIidConsumed
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Hashable (Hashable)
import qualified Data.Ix as Ix
import qualified Data.Ord as Ord
import           GHC.Generics (Generic)

import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import qualified Game.LambdaHack.Content.ItemKind as IK
import qualified Game.LambdaHack.Content.TileKind as TK
import qualified Game.LambdaHack.Core.Dice as Dice
import           Game.LambdaHack.Definition.Ability (EqpSlot (..))
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.Flavour

-- | Game items in actor possesion or strewn around the dungeon.
-- The information contained in this time is available to the player
-- from the moment the item is first seen and is never mutated.
--
-- Some items are not created identified (@IdentityCovered@).
-- Then they are presented as having a template kind that is really
-- not their own, though usually close. Full kind information about
-- item's kind is available through the @ItemKindIx@ index once the item
-- is identified and full information about the value of item's aspect record
-- is available elsewhere (both @IdentityObvious@ and @IdentityCovered@
-- items may or may not need identification of their aspect record).
data Item = Item
  { jkind    :: ItemIdentity     -- ^ the kind of the item, or an indirection
  , jfid     :: Maybe FactionId  -- ^ the faction that created the item, if any
  , jflavour :: Flavour          -- ^ flavour, always the real one,
                                 --   it's not hidden; people may not recognize
                                 --   shape, but they remember colour and old
                                 --   vs fancy look
  }
  deriving (Show, Eq, Generic)

instance Binary Item

-- | Either the explicit obvious kind of the item or the kind it's hidden under,
-- with the details covered under the index indirection.
data ItemIdentity =
    IdentityObvious (ContentId IK.ItemKind)
  | IdentityCovered ItemKindIx (ContentId IK.ItemKind)
  deriving (Show, Eq, Generic)

instance Hashable ItemIdentity

instance Binary ItemIdentity

-- | The map of item ids to item aspect record. The full map is known
-- by the server.
type DiscoveryAspect = EM.EnumMap ItemId IA.AspectRecord

-- | An index of the kind identifier of an item. Clients have partial knowledge
-- how these idexes map to kind ids. They gain knowledge by identifying items.
-- The indexes and kind identifiers are 1-1.
newtype ItemKindIx = ItemKindIx Word16
  deriving (Show, Eq, Ord, Enum, Ix.Ix, Hashable, Binary)

-- | The secret part of the information about an item. If a faction
-- knows the aspect record of the item, this is the complete secret information.
-- Items that don't need second identification (the @kmConst@ flag is set)
-- may be identified or not and both cases are OK (their display flavour
-- will differ and that may be the point).
data ItemDisco =
    ItemDiscoFull IA.AspectRecord
  | ItemDiscoMean IA.KindMean
 deriving (Show, Ord, Eq)

-- No speedup from making fields non-strict.
-- | Full information about an item.
data ItemFull = ItemFull
  { itemBase    :: Item
  , itemKindId  :: ContentId IK.ItemKind
  , itemKind    :: IK.ItemKind
  , itemDisco   :: ItemDisco
  , itemSuspect :: Bool
  }
  deriving Show

type ItemFullKit = (ItemFull, ItemQuant)

-- | The map of item kind indexes to item kind ids.
-- The full map, as known by the server, is 1-1.
-- Because it's sparse and changes, we don't represent it as an (unboxed)
-- vector, until it becomes a bottleneck (if ever, likely on JS, where only
-- vectors are fast).
type DiscoveryKind = EM.EnumMap ItemKindIx (ContentId IK.ItemKind)

-- | The map of item kind indexes to identifiers of items that have that kind.
-- Used to update data about items when their kinds become known, e.g.,
-- AI item use benefit data.
type ItemIxMap = EM.EnumMap ItemKindIx (ES.EnumSet ItemId)

-- | The fields are, in order:
-- 1. whether the item should be kept in equipment (not in stash)
-- 2. the total benefit from picking the item up (to use or to put in equipment)
-- 3. the benefit of applying the item to self
-- 4. the (usually negative, for him) value of hitting a foe in melee with it
-- 5. the (usually negative, for him) value of flinging the item at an opponent
data Benefit = Benefit
  { benInEqp  :: Bool
  , benPickup :: Double
  , benApply  :: Double
  , benMelee  :: Double
  , benFling  :: Double
  }
  deriving (Show, Generic)

instance Binary Benefit

type DiscoveryBenefit = EM.EnumMap ItemId Benefit

-- | The absolute level's local time at which an item's copy becomes
-- operational again. Even if item is not identified and so its timeout
-- unknown, it's enough to compare this to the local level time
-- to learn whether an item is recharged.
--
-- This schema causes timeout jumps for items in stash, but timeout
-- is reset when items move, so this is a minor problem.
-- Global time can't be used even only for items in stash,
-- or exploit would be possible when an actor on a desolate level waits
-- to recharge items for actors on a busy level. It's probably
-- impossible to avoid such exploits or, otherwise, timeout jumps,
-- particularly for faction where many actors move on many levels
-- and so an item in stash is not used by a single actor at a time.
newtype ItemTimer = ItemTimer {itemTimer :: Time}
  deriving (Show, Eq, Binary)

type ItemTimers = [ItemTimer]

-- | Number of items in a bag, together with recharging timer, in case of
-- items that need recharging, exists only temporarily or auto-activate
-- at regular intervals. Data invariant: the length of the timer
-- should be less or equal to the number of items.
type ItemQuant = (Int, ItemTimers)

-- | A bag of items, e.g., one of the stores of an actor or the items
-- on a particular floor position or embedded in a particular map tile.
type ItemBag = EM.EnumMap ItemId ItemQuant

-- | All items in the dungeon (including those carried by actors),
-- indexed by item identifier.
type ItemDict = EM.EnumMap ItemId Item

toItemKindIx :: Word16 -> ItemKindIx
{-# INLINE toItemKindIx #-}
toItemKindIx = ItemKindIx

quantSingle :: ItemQuant
quantSingle = (1, [])

itemToFull6 :: COps -> DiscoveryKind -> DiscoveryAspect -> ItemId -> Item
            -> ItemFull
itemToFull6 COps{coitem, coItemSpeedup} discoKind discoAspect iid itemBase =
  let (itemKindId, itemSuspect) = case jkind itemBase of
        IdentityObvious ik -> (ik, False)
        IdentityCovered ix ik ->
          maybe (ik, True) (, False) $ ix `EM.lookup` discoKind
      itemKind = okind coitem itemKindId
      km = getKindMean itemKindId coItemSpeedup
      -- If the kind is not identified, we know nothing about the real
      -- aspect record, so we at least assume they are variable.
      itemAspectMean | itemSuspect = km {IA.kmConst = False}
                     | otherwise = km
      itemDisco = case EM.lookup iid discoAspect of
        Just itemAspect -> ItemDiscoFull itemAspect
        Nothing -> ItemDiscoMean itemAspectMean
  in ItemFull {..}

aspectRecordFull :: ItemFull -> IA.AspectRecord
aspectRecordFull itemFull =
  case itemDisco itemFull of
    ItemDiscoFull itemAspect -> itemAspect
    ItemDiscoMean itemAspectMean -> IA.kmMean itemAspectMean

-- This ignores items that don't go into equipment, as determined in @benInEqp@.
-- They are removed from equipment elsewhere via @harmful@.
strongestSlot :: DiscoveryBenefit -> Ability.EqpSlot -> [(ItemId, ItemFullKit)]
              -> [(Int, (ItemId, ItemFullKit))]
strongestSlot discoBenefit eqpSlot is =
  let f (iid, (itemFull, kit)) =
        let Benefit{benInEqp, benPickup, benMelee} = discoBenefit EM.! iid
        in if not benInEqp
           then Nothing
           else Just $
             let ben = case eqpSlot of
                   EqpSlotWeaponFast ->
                       -- For equipping/unequipping the main reliable weapon,
                       -- we take into account not only melee damage,
                       -- but also timeout, aspects, etc.
                       ceiling benPickup
                   EqpSlotWeaponBig ->
                       -- For equipping/unequipping the one-shot big hitter
                       -- weapon, we take into account only melee damage
                       -- and we don't even care if it's durable.
                       -- The backup is ready in the slot above, after all.
                       ceiling (- benMelee)
                   _ -> valueAtEqpSlot eqpSlot $ aspectRecordFull itemFull
                 idBonus = if itemSuspect itemFull then 1000 else 0
                 arItem = aspectRecordFull itemFull
                 -- Equip good uniques for flavour and fun from unique effects.
                 uniqueBonus = if IA.checkFlag Ability.Unique arItem && ben > 20
                               then 1000
                               else 0
             in (ben + idBonus + uniqueBonus, (iid, (itemFull, kit)))
  in sortBy (comparing (Ord.Down . fst)) $ mapMaybe f is

valueAtEqpSlot :: EqpSlot -> IA.AspectRecord -> Int
valueAtEqpSlot eqpSlot arItem@IA.AspectRecord{..} =
  case eqpSlot of
    EqpSlotMove -> Ability.getSk Ability.SkMove aSkills
    EqpSlotMelee -> Ability.getSk Ability.SkMelee aSkills
    EqpSlotDisplace -> Ability.getSk Ability.SkDisplace aSkills
    EqpSlotAlter -> Ability.getSk Ability.SkAlter aSkills
    EqpSlotWait -> Ability.getSk Ability.SkWait aSkills
    EqpSlotMoveItem -> Ability.getSk Ability.SkMoveItem aSkills
    EqpSlotProject -> Ability.getSk Ability.SkProject aSkills
    EqpSlotApply -> Ability.getSk Ability.SkApply aSkills
    EqpSlotSwimming -> Ability.getSk Ability.SkSwimming aSkills
    EqpSlotFlying -> Ability.getSk Ability.SkFlying aSkills
    EqpSlotHurtMelee -> Ability.getSk Ability.SkHurtMelee aSkills
    EqpSlotArmorMelee -> Ability.getSk Ability.SkArmorMelee aSkills
    EqpSlotArmorRanged -> Ability.getSk Ability.SkArmorRanged aSkills
    EqpSlotMaxHP -> Ability.getSk Ability.SkMaxHP aSkills
    EqpSlotSpeed -> Ability.getSk Ability.SkSpeed aSkills
    EqpSlotSight -> Ability.getSk Ability.SkSight aSkills
    EqpSlotShine -> Ability.getSk Ability.SkShine aSkills
    EqpSlotMiscBonus ->
      aTimeout  -- usually better items have longer timeout
      + Ability.getSk Ability.SkMaxCalm aSkills
      + Ability.getSk Ability.SkSmell aSkills
      + Ability.getSk Ability.SkNocto aSkills
          -- powerful, but hard to boost over aSight
    EqpSlotWeaponFast -> error $ "" `showFailure` arItem  -- sum of all benefits
    EqpSlotWeaponBig -> error $ "" `showFailure` arItem  -- sum of all benefits

itemTimerZero :: ItemTimer
itemTimerZero = ItemTimer timeZero

createItemTimer :: Time -> Delta Time -> ItemTimer
createItemTimer localTime delta = ItemTimer $ localTime `timeShift` delta

shiftItemTimer :: Delta Time -> ItemTimer -> ItemTimer
shiftItemTimer delta t = ItemTimer $ itemTimer t `timeShift` delta

deltaOfItemTimer :: Time -> ItemTimer -> Delta Time
deltaOfItemTimer localTime t = timeDeltaToFrom (itemTimer t) localTime

charging :: Time -> ItemTimer -> Bool
charging localTime = (> localTime) . itemTimer

ncharges :: Time -> ItemQuant -> Int
ncharges localTime (itemK, itemTimers) =
  itemK - length (filter (charging localTime) itemTimers)

hasCharge :: Time -> ItemQuant -> Bool
hasCharge localTime kit = ncharges localTime kit > 0

strongestMelee :: Bool -> Maybe DiscoveryBenefit -> Time
               -> [(ItemId, ItemFullKit)]
               -> [(Double, Bool, Int, Int, ItemId, ItemFullKit)]
strongestMelee _ _ _ [] = []
strongestMelee ignoreCharges mdiscoBenefit localTime kitAss =
  -- For fighting, as opposed to equipping, we value weapon only for
  -- its raw damage and harming effects and at this very moment only,
  -- not in the future. Hehce, we exclude discharged weapons.
  let f (iid, ifk@(itemFull, kit)) =
        let rawDmg = IK.damageUsefulness $ itemKind itemFull
            unIDedBonus = if itemSuspect itemFull then 1000 else 0
            totalValue = case mdiscoBenefit of
              Just discoBenefit ->
                let Benefit{benMelee} = discoBenefit EM.! iid
                in benMelee - unIDedBonus
              Nothing -> - rawDmg  -- special case: not interested about ID
            arItem = aspectRecordFull itemFull
            timeout = IA.aTimeout arItem
            -- This is crucial for weapons for which AI is too silly
            -- to value the effects at more than 0, even though they are strong
            -- and also to prefer weapons with burn or wound over pure damage,
            -- which is a good rule of thumb before late game at least.
            hasEffect = any IK.forApplyEffect
                            (IK.ieffects $ itemKind itemFull)
            ncha = ncharges localTime kit
        in if ignoreCharges || ncha > 0
           then Just (totalValue, hasEffect, timeout, ncha, iid, ifk)
           else Nothing
  -- We can't filter out weapons that are not harmful to victim
  -- (@benMelee >= 0), because actors use them if nothing else available,
  -- e.g., geysers, bees. This is intended and fun.
  in sortOn (\(value, hasEffect, timeout, _, _, (itemFull, _)) ->
                -- Weapon with higher timeout activated first to increase
                -- the chance of using it again during this fight.
                -- No timeout is ever better, because no wait incurred.
                -- Optimal packing problem: start with the biggest.
                let timN = if timeout == 0 then -99999 else - timeout
                in (value, not hasEffect, timN, itemKindId itemFull))
            (mapMaybe f kitAss)

unknownAspect :: (IK.Aspect -> [Dice.Dice]) -> ItemFull -> Bool
unknownAspect f itemFull@ItemFull{itemKind=IK.ItemKind{iaspects}, ..} =
  case itemDisco of
    ItemDiscoMean IA.KindMean{kmConst} ->
      let arItem = aspectRecordFull itemFull
          unknown x = let (minD, maxD) = Dice.infsupDice x
                      in minD /= maxD
      in itemSuspect && not (IA.checkFlag Ability.MinorAspects arItem)
         || not kmConst && any (or . map unknown . f) iaspects
    ItemDiscoFull{} -> False  -- all known

-- We assume @SkHurtMelee@ never appears inside @Odds@. If it does,
-- not much harm.
unknownMeleeBonus :: [ItemFull] -> Bool
unknownMeleeBonus =
  let p (IK.AddSkill Ability.SkHurtMelee k) = [k]
      p _ = []
      f itemFull b = b || unknownAspect p itemFull
  in foldr f False

-- We assume @SkSpeed@ never appears inside @Odds@. If it does,
-- not much harm.
unknownSpeedBonus :: [ItemFull] -> Bool
unknownSpeedBonus =
  let p (IK.AddSkill Ability.SkSpeed k) = [k]
      p _ = []
      f itemFull b = b || unknownAspect p itemFull
  in foldr f False

conditionMeleeBonus :: [ItemFullKit] -> Int
conditionMeleeBonus kitAss =
  let f (itemFull, (itemK, _)) k =
        let arItem = aspectRecordFull itemFull
        in if IA.checkFlag Ability.Condition arItem
           then k + itemK * IA.getSkill Ability.SkHurtMelee arItem
           else k
  in foldr f 0 kitAss

conditionSpeedBonus :: [ItemFullKit] -> Int
conditionSpeedBonus kitAss =
  let f (itemFull, (itemK, _)) k =
        let arItem = aspectRecordFull itemFull
        in if IA.checkFlag Ability.Condition arItem
           then k + itemK * IA.getSkill Ability.SkSpeed arItem
           else k
  in foldr f 0 kitAss

-- | Damage calculation. The armor and hurt skills are additive.
-- They can't be multiplicative, because then 100% armor would minimize
-- damage regardless of even 200% hurt skill.
-- However, additive skills make the relative effectiveness of weapons
-- dependent on the enemy, so even with -100% hurt skill a kinetic weapon
-- can't be removed from the list, because an enemy may have
-- negative armor skill. This is bad, but also KISS.
armorHurtCalculation :: Bool -> Ability.Skills -> Ability.Skills -> Int
armorHurtCalculation proj sMaxSk tMaxSk =
  let trim200 n = min 200 $ max (-200) n
      itemBonus =
        trim200 (Ability.getSk Ability.SkHurtMelee sMaxSk)
        - if proj
          then trim200 (Ability.getSk Ability.SkArmorRanged tMaxSk)
          else trim200 (Ability.getSk Ability.SkArmorMelee tMaxSk)
  in 100 + max (-95) itemBonus  -- at least 5% of damage gets through

mergeItemQuant :: ItemQuant -> ItemQuant -> ItemQuant
mergeItemQuant (k2, it2) (k1, it1) = (k1 + k2, it1 ++ it2)

listToolsToConsume :: [(ItemId, ItemFullKit)] -> [(ItemId, ItemFullKit)]
                   -> [((CStore, Bool), (ItemId, ItemFullKit))]
listToolsToConsume kitAssG kitAssE =
  let isDurable = IA.checkFlag Ability.Durable
                  . aspectRecordFull . fst . snd
      (kitAssGT, kitAssGF) = partition isDurable kitAssG
      (kitAssET, kitAssEF) = partition isDurable kitAssE
      -- Non-durable tools take precedence, because durable
      -- are applied and, usually being weapons,
      -- may be harmful or may have unintended effects.
      -- CGround takes precedence, too.
  in map ((CGround, False),) kitAssGF
     ++ map ((CEqp, False),) kitAssEF
     ++ map ((CGround, True),) kitAssGT
     ++ map ((CEqp, True),) kitAssET

countIidConsumed :: ItemFullKit
                 -> [(Bool, Int, GroupName IK.ItemKind)]
                 -> (Int, Int, [(Bool, Int, GroupName IK.ItemKind)])
countIidConsumed (ItemFull{itemKind}, (k, _)) grps0 =
  let hasGroup grp =
        maybe False (> 0) $ lookup grp $ IK.ifreq itemKind
      matchGroup (nToApplyIfDurable, nToDestroyAlways, grps)
                 (destroyAlways, n, grp) =
        if hasGroup grp
        then let mkn = min k n  -- even if durable, use each copy only once
                 grps2 = if n - mkn > 0
                         then (destroyAlways, n - mkn, grp) : grps
                         else grps
             in if destroyAlways
                then ( nToApplyIfDurable
                     , max nToDestroyAlways mkn
                     , grps2 )
                else ( max nToApplyIfDurable mkn
                     , nToDestroyAlways
                     , grps2 )
        else ( nToApplyIfDurable
             , nToDestroyAlways
             , (destroyAlways, n, grp) : grps )
  in foldl' matchGroup (0, 0, []) grps0

subtractIidfromGrps :: ( EM.EnumMap CStore ItemBag
                       , [(CStore, (ItemId, ItemFull))]
                       , [(Bool, Int, GroupName IK.ItemKind)] )
                    -> ((CStore, Bool), (ItemId, ItemFullKit))
                    -> ( EM.EnumMap CStore ItemBag
                       , [(CStore, (ItemId, ItemFull))]
                       , [(Bool, Int, GroupName IK.ItemKind)] )
subtractIidfromGrps (bagsToLose1, iidsToApply1, grps1)
                    ((store, durable), (iid, itemFullKit@(itemFull, (_, it)))) =
  let (nToApplyIfDurable, nToDestroyAlways, grps2) =
        countIidConsumed itemFullKit grps1
      (nToApply, nToDestroy) = if durable
                               then (nToApplyIfDurable, nToDestroyAlways)
                               else (0, max nToApplyIfDurable nToDestroyAlways)
  in ( if nToDestroy == 0
       then bagsToLose1  -- avoid vacuus @UpdDestroyItem@
       else let kit2 = (nToDestroy, take nToDestroy it)
                removedBags = EM.singleton store $ EM.singleton iid kit2
            in EM.unionWith (EM.unionWith mergeItemQuant)
                            removedBags bagsToLose1
     , replicate nToApply (store, (iid, itemFull)) ++ iidsToApply1
     , grps2 )

sortIids :: (ItemId -> ItemFull)
         -> [(ItemId, ItemQuant)]
         -> [(ItemId, ItemQuant)]
sortIids itemToF =
  -- If appearance and aspects the same, keep the order from before sort.
  let kindAndAppearance (iid, _) =
        let ItemFull{itemBase=Item{..}, ..} = itemToF iid
        in ( not itemSuspect, itemKindId, itemDisco
           , IK.isymbol itemKind, IK.iname itemKind
           , jflavour, jfid )
  in sortOn kindAndAppearance

data TileAction =
    EmbedAction (ItemId, ItemQuant)
  | ToAction (GroupName TK.TileKind)
  | WithAction [(Int, GroupName IK.ItemKind)] (GroupName TK.TileKind)
  deriving Show

parseTileAction :: Bool -> Bool -> [(IK.ItemKind, (ItemId, ItemQuant))]
                -> TK.Feature
                -> Maybe TileAction
parseTileAction bproj underFeet embedKindList feat = case feat of
  TK.Embed igroup ->
      -- Greater or equal 0 to also cover template UNKNOWN items
      -- not yet identified by the client.
    let f (itemKind, _) =
          fromMaybe (-1) (lookup igroup $ IK.ifreq itemKind) >= 0
    in case find f embedKindList of
      Nothing -> Nothing
      Just (_, iidkit) -> Just $ EmbedAction iidkit
  TK.OpenTo tgroup | not (underFeet || bproj) -> Just $ ToAction tgroup
  TK.CloseTo tgroup | not (underFeet || bproj) -> Just $ ToAction tgroup
  TK.ChangeTo tgroup | not bproj -> Just $ ToAction tgroup
  TK.OpenWith proj grps tgroup | not underFeet ->
    if proj == TK.ProjNo && bproj
    then Nothing
    else Just $ WithAction grps tgroup
  TK.CloseWith proj grps tgroup | not underFeet ->
    -- Not when standing on tile, not to autoclose doors under actor
    -- or close via dropping an item inside.
    if proj == TK.ProjNo && bproj
    then Nothing
    else Just $ WithAction grps tgroup
  TK.ChangeWith proj grps tgroup ->
    if proj == TK.ProjNo && bproj
    then Nothing
    else Just $ WithAction grps tgroup
  _ -> Nothing
