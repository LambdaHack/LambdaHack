{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
-- | The type of kinds of weapons, treasure, organs, shrapnel and actors.
module Game.LambdaHack.Content.ItemKind
  ( ItemKind(..)
  , Effect(..), Aspect(..), ThrowMod(..), Feature(..), EqpSlot(..)
  , toVelocity, toLinger, validateSingleItemKind, validateAllItemKind
  , effectTrav, aspectTrav
  ) where

import qualified Control.Monad.State as St
import Data.Binary
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU

import qualified Game.LambdaHack.Common.Ability as Ability
import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc

-- | Item properties that are fixed for a given kind of items.
data ItemKind = ItemKind
  { isymbol  :: !Char              -- ^ map symbol
  , iname    :: !Text              -- ^ generic name
  , ifreq    :: !(Freqs ItemKind)  -- ^ frequency within groups
  , iflavour :: ![Flavour]         -- ^ possible flavours
  , icount   :: !Dice.Dice         -- ^ created in that quantity
  , irarity  :: !Rarity            -- ^ rarity on given depths
  , iverbHit :: !MU.Part           -- ^ the verb for applying and melee
  , iweight  :: !Int               -- ^ weight in grams
  , iaspects :: ![Aspect Dice.Dice]
                                   -- ^ keep the aspect continuously
  , ieffects :: ![Effect Dice.Dice]
                                   -- ^ cause the effect when triggered
  , ifeature :: ![Feature]  -- ^ public properties
  , idesc    :: !Text              -- ^ description
  , ikit     :: ![(GroupName ItemKind, CStore)]
                                   -- ^ accompanying organs and items
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound

-- TODO: document each constructor
-- TODO: perhaps separate the types of ordinary and exotic effects.
-- | Effects of items. Can be invoked by the item wielder to affect
-- another actor or the wielder himself. Many occurences in the same item
-- are possible. Unchecked assumption: only ordinary effects are inside
-- the exotic effects (at the end of this list).
data Effect a =
    -- Ordinary effects.
    NoEffect !Text
  | Hurt !Dice.Dice
  | Burn !Int
  | Explode !(GroupName ItemKind)
                          -- ^ explode, producing this group of shrapnel
  | RefillHP !Int
  | OverfillHP !Int
  | RefillCalm !Int
  | OverfillCalm !Int
  | Dominate
  | Impress
  | CallFriend !a
  | Summon !(Freqs ItemKind) !a
  | CreateItem !a
  | Ascend !Int
  | Escape !Int           -- ^ the Int says if can be placed on last level, etc.
  | Paralyze !a
  | InsertMove !a
  | Teleport !a
  | PolyItem !CStore
  | Identify !CStore
  | SendFlying !ThrowMod
  | PushActor !ThrowMod
  | PullActor !ThrowMod
  | DropBestWeapon
  | DropEqp !Char !Bool   -- ^ symbol @' '@ means all, @True@ means hit on drop
  | ActivateInv !Char     -- ^ symbol @' '@ means all
  | ApplyPerfume
    -- Exotic effects follow.
  | OneOf ![Effect a]
  | OnSmash !(Effect a)   -- ^ trigger if item smashed (not applied nor meleed)
  | Recharging !(Effect a)  -- ^ this effect inactive until timeout passes
  | CreateOrgan !Dice.Dice !(GroupName ItemKind)
                          -- ^ create a matching item and insert as an organ
                          --   with the given timer; not restricted
                          --   to temporary aspect item kinds
  | DestroyOrgan !(GroupName ItemKind)  -- ^usually a Temporary organ
  | Temporary !Text       -- ^ the item is temporary, vanishes at even void
                          --   Periodic activation, unless Durable
  deriving (Show, Read, Eq, Ord, Generic, Functor)

-- | Aspects of items. Those that are named @Add*@ are additive
-- (starting at 0) for all items wielded by an actor and they affect the actor.
data Aspect a =
    Periodic           -- ^ in equipment, activate as often as @Timeout@ permits
  | Timeout !a         -- ^ some effects will be disabled until item recharges
  | AddHurtMelee !a    -- ^ percentage damage bonus in melee
  | AddArmorMelee !a   -- ^ percentage armor bonus against melee
  | AddHurtRanged !a   -- ^ percentage damage bonus in ranged
  | AddArmorRanged !a  -- ^ percentage armor bonus against ranged
  | AddMaxHP !a        -- ^ maximal hp
  | AddMaxCalm !a      -- ^ maximal calm
  | AddSpeed !a        -- ^ speed in m/10s
  | AddSkills !Ability.Skills  -- ^ skills in particular abilities
  | AddSight !a        -- ^ FOV radius, where 1 means a single tile
  | AddSmell !a        -- ^ smell radius, where 1 means a single tile
  | AddLight !a        -- ^ light radius, where 1 means a single tile
  deriving (Show, Read, Eq, Ord, Generic, Functor)

-- | Parameters modifying a throw. Not additive and don't start at 0.
data ThrowMod = ThrowMod
  { throwVelocity :: !Int  -- ^ fly with this percentage of base throw speed
  , throwLinger   :: !Int  -- ^ fly for this percentage of 2 turns
  }
  deriving (Show, Read, Eq, Ord, Generic)

-- | Features of item. Affect only the item in question, not the actor,
-- and so not additive in any sense.
data Feature =
    ChangeTo !(GroupName ItemKind)
                            -- ^ change to this group when altered
  | Fragile                 -- ^ drop and break at target tile, even if no hit
  | Durable                 -- ^ don't break even when hitting or applying
  | ToThrow !ThrowMod       -- ^ parameters modifying a throw
  | Identified              -- ^ the item starts identified
  | Applicable              -- ^ AI and UI flag: consider applying
  | EqpSlot !EqpSlot !Text  -- ^ AI and UI flag: goes to inventory
  | Precious                -- ^ can't throw or apply if not calm enough;
                            --   AI and UI flag: don't risk identifying by use
  | Tactic !Tactic          -- ^ overrides actor's tactic (TODO)
  deriving (Show, Eq, Ord, Generic)

data EqpSlot =
    EqpSlotPeriodic
  | EqpSlotTimeout
  | EqpSlotAddHurtMelee
  | EqpSlotAddArmorMelee
  | EqpSlotAddHurtRanged
  | EqpSlotAddArmorRanged
  | EqpSlotAddMaxHP
  | EqpSlotAddMaxCalm
  | EqpSlotAddSpeed
  | EqpSlotAddSkills
  | EqpSlotAddSight
  | EqpSlotAddSmell
  | EqpSlotAddLight
  | EqpSlotWeapon
  deriving (Show, Eq, Ord, Generic)

instance Hashable a => Hashable (Effect a)

instance Hashable a => Hashable (Aspect a)

instance Hashable ThrowMod

instance Hashable Feature

instance Hashable EqpSlot

instance Binary a => Binary (Effect a)

instance Binary a => Binary (Aspect a)

instance Binary ThrowMod

instance Binary Feature

instance Binary EqpSlot

-- TODO: Traversable?
-- | Transform an effect using a stateful function.
effectTrav :: Effect a -> (a -> St.State s b) -> St.State s (Effect b)
effectTrav (NoEffect t) _ = return $! NoEffect t
effectTrav (RefillHP p) _ = return $! RefillHP p
effectTrav (OverfillHP p) _ = return $! OverfillHP p
effectTrav (Hurt dice) _ = return $! Hurt dice
effectTrav (RefillCalm p) _ = return $! RefillCalm p
effectTrav (OverfillCalm p) _ = return $! OverfillCalm p
effectTrav Dominate _ = return Dominate
effectTrav Impress _ = return Impress
effectTrav (CallFriend a) f = do
  b <- f a
  return $! CallFriend b
effectTrav (Summon freqs a) f = do
  b <- f a
  return $! Summon freqs b
effectTrav (CreateItem a) f = do
  b <- f a
  return $! CreateItem b
effectTrav ApplyPerfume _ = return ApplyPerfume
effectTrav (Burn p) _ = return $! Burn p
effectTrav (Ascend p) _ = return $! Ascend p
effectTrav (Escape p) _ = return $! Escape p
effectTrav (Paralyze a) f = do
  b <- f a
  return $! Paralyze b
effectTrav (InsertMove a) f = do
  b <- f a
  return $! InsertMove b
effectTrav DropBestWeapon _ = return DropBestWeapon
effectTrav (DropEqp symbol hit) _ = return $! DropEqp symbol hit
effectTrav (SendFlying tmod) _ = return $! SendFlying tmod
effectTrav (PushActor tmod) _ = return $! PushActor tmod
effectTrav (PullActor tmod) _ = return $! PullActor tmod
effectTrav (Teleport a) f = do
  b <- f a
  return $! Teleport b
effectTrav (PolyItem cstore) _ = return $! PolyItem cstore
effectTrav (Identify cstore) _ = return $! Identify cstore
effectTrav (ActivateInv symbol) _ = return $! ActivateInv symbol
effectTrav (OneOf la) f = do
  lb <- mapM (\a -> effectTrav a f) la
  return $! OneOf lb
effectTrav (OnSmash effa) f = do
  effb <- effectTrav effa f
  return $! OnSmash effb
effectTrav (Explode t) _ = return $! Explode t
effectTrav (Recharging effa) f = do
  effb <- effectTrav effa f
  return $! Recharging effb
effectTrav (CreateOrgan k t) _ = return $! CreateOrgan k t
effectTrav (DestroyOrgan t) _ = return $! DestroyOrgan t
effectTrav (Temporary t) _ = return $! Temporary t

-- | Transform an aspect using a stateful function.
aspectTrav :: Aspect a -> (a -> St.State s b) -> St.State s (Aspect b)
aspectTrav Periodic _ = return Periodic
aspectTrav (Timeout a) f = do
  b <- f a
  return $! Timeout b
aspectTrav (AddMaxHP a) f = do
  b <- f a
  return $! AddMaxHP b
aspectTrav (AddMaxCalm a) f = do
  b <- f a
  return $! AddMaxCalm b
aspectTrav (AddSpeed a) f = do
  b <- f a
  return $! AddSpeed b
aspectTrav (AddSkills as) _ = return $! AddSkills as
aspectTrav (AddHurtMelee a) f = do
  b <- f a
  return $! AddHurtMelee b
aspectTrav (AddHurtRanged a) f = do
  b <- f a
  return $! AddHurtRanged b
aspectTrav (AddArmorMelee a) f = do
  b <- f a
  return $! AddArmorMelee b
aspectTrav (AddArmorRanged a) f = do
  b <- f a
  return $! AddArmorRanged b
aspectTrav (AddSight a) f = do
  b <- f a
  return $! AddSight b
aspectTrav (AddSmell a) f = do
  b <- f a
  return $! AddSmell b
aspectTrav (AddLight a) f = do
  b <- f a
  return $! AddLight b

toVelocity :: Int -> Feature
toVelocity n = ToThrow $ ThrowMod n 100

toLinger :: Int -> Feature
toLinger n = ToThrow $ ThrowMod 100 n

-- | Catch invalid item kind definitions.
validateSingleItemKind :: ItemKind -> [Text]
validateSingleItemKind ItemKind{..} =
  [ "iname longer than 23" | T.length iname > 23 ]
  ++ validateRarity irarity
  -- Reject duplicate Timeout and Periodic. Otherwise the behaviour
  -- may not agree with the item's in-game description.
  ++ let periodicAspect :: Aspect a -> Bool
         periodicAspect Periodic = True
         periodicAspect _ = False
         ps = filter periodicAspect iaspects
     in if length ps > 1 then ["more than one Periodic specification"] else []
  ++ let timeoutAspect :: Aspect a -> Bool
         timeoutAspect Timeout{} = True
         timeoutAspect _ = False
         ts = filter timeoutAspect iaspects
     in if length ts > 1 then ["more than one Timeout specification"] else []

-- TODO: if "treasure" stays wired-in, assure there are some treasure items
-- TODO: (spans multiple contents) check that there is at least one item
-- in each ifreq group for each level (thought more precisely we'd need
-- to lookup caves and modes and only check at the levels the caves
-- can appear at).
-- | Validate all item kinds.
validateAllItemKind :: [ItemKind] -> [Text]
validateAllItemKind _ = []
