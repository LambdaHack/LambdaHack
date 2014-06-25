{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
-- | Effects of content on the game state. No operation in this module
-- involves state or monad types.
module Game.LambdaHack.Common.Effect
  ( Effect(..), Aspect(..), ThrowMod(..), Feature(..), EqpSlot(..)
  , effectTrav, aspectTrav
  ) where

import qualified Control.Monad.State as St
import Data.Binary
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Game.LambdaHack.Common.Ability as Ability
import qualified Game.LambdaHack.Common.Dice as Dice

-- TODO: document each constructor
-- | Effects of items, tiles, etc. The type argument represents power.
-- either as a random formula dependent on level, or as a final rolled value.
data Effect a =
    NoEffect
  | Heal !Int
  | Hurt !Dice.Dice !a
  | Calm !Int
  | Dominate
  | Impress
  | CallFriend !Int
  | Summon !a
  | CreateItem !Int
  | ApplyPerfume
  | Burn !Int
  | Blast !Int  -- ^ sound blast
  | Ascend !Int
  | Escape !Int  -- ^ the argument marks if can be placed on last level, etc.
  | Paralyze !a
  | InsertMove !a
  | DropBestWeapon
  | DropEqp !Char !Bool  -- ^ symbol @' '@ means all, @True@ means hit on drop
  | SendFlying !(ThrowMod a)
  | PushActor !(ThrowMod a)
  | PullActor !(ThrowMod a)
  | Teleport !a
  | ActivateEqp !Char  -- ^ symbol @' '@ means all
  | Explode !Text   -- ^ explode, producing this group of shrapnel
  | OnSmash !(Effect a)  -- ^ trigger when item smashed (not applied nor meleed)
  | TimedAspect !Int !(Aspect a)  -- enable the aspect for k clips
  deriving (Show, Read, Eq, Ord, Generic, Functor)

-- | Aspects of items, tiles, etc. The type argument represents power.
-- either as a random formula dependent on level, or as a final rolled value.
data Aspect a =
    Periodic !a     -- ^ is activated this many times in 100
  | AddMaxHP !a     -- ^ maximal hp
  | AddMaxCalm !a   -- ^ maximal calm
  | AddSpeed !a     -- ^ speed in m/10s
  | AddSkills !Ability.Skills  -- ^ skills in particular abilities
  | ArmorMelee !a   -- ^ armor class wrt melee
  | SightRadius !a  -- ^ FOV radius, where 1 means a single tile
  | SmellRadius !a  -- ^ smell radius, where 1 means a single tile
  | AddLight !a     -- ^ light radius, where 1 means a single tile
  deriving (Show, Read, Eq, Ord, Generic, Functor)

-- | Parameters modifying a trow.
data ThrowMod a = ThrowMod
  { throwVelocity :: !a  -- ^ fly with this percentage of base throw speed
  , throwLinger   :: !a  -- ^ fly for this percentage of 2 turns
  }
  deriving (Show, Read, Eq, Ord, Generic, Functor)

-- | All possible item features.
data Feature =
    ChangeTo !Text           -- ^ change to this group when altered
  | Fragile                  -- ^ break even when not hitting an enemy
  | Durable                  -- ^ don't break even hitting or applying
  | ToThrow !(ThrowMod Int)  -- ^ parameters modifying a throw
  | Applicable               -- ^ can't be turned off, is consumed by use
  | EqpSlot !EqpSlot !Text   -- ^ the slot, counts towards the eqp limit
  | Identified               -- ^ any such item starts identified
  | Precious                 -- ^ precious; don't risk identifying by use
  deriving (Show, Eq, Ord, Generic)

data EqpSlot =
    EqpSlotPeriodic
  | EqpSlotAddMaxHP
  | EqpSlotAddMaxCalm
  | EqpSlotAddSpeed
  | EqpSlotAbility
  | EqpSlotArmorMelee
  | EqpSlotSightRadius
  | EqpSlotSmellRadius
  | EqpSlotAddLight
  | EqpSlotWeapon
  deriving (Show, Eq, Ord, Generic)

instance Hashable a => Hashable (Effect a)

instance Hashable a => Hashable (Aspect a)

instance Hashable a => Hashable (ThrowMod a)

instance Hashable Feature

instance Hashable EqpSlot

instance Binary a => Binary (Effect a)

instance Binary a => Binary (Aspect a)

instance Binary a => Binary (ThrowMod a)

instance Binary Feature

instance Binary EqpSlot

-- TODO: Traversable?
-- | Transform an effect using a stateful function.
effectTrav :: Effect a -> (a -> St.State s b) -> St.State s (Effect b)
effectTrav NoEffect _ = return NoEffect
effectTrav (Heal p) _ = return $! Heal p
effectTrav (Hurt dice a) f = do
  b <- f a
  return $! Hurt dice b
effectTrav (Calm p) _ = return $! Calm p
effectTrav Dominate _ = return Dominate
effectTrav Impress _ = return Impress
effectTrav (CallFriend p) _ = return $! CallFriend p
effectTrav (Summon a) f = do
  b <- f a
  return $! Summon b
effectTrav (CreateItem p) _ = return $! CreateItem p
effectTrav ApplyPerfume _ = return ApplyPerfume
effectTrav (Burn p) _ = return $! Burn p
effectTrav (Blast p) _ = return $! Blast p
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
effectTrav (SendFlying tmod) f = do
  tmod2 <- modTrav tmod f
  return $! SendFlying tmod2
effectTrav (PushActor tmod) f = do
  tmod2 <- modTrav tmod f
  return $! PushActor tmod2
effectTrav (PullActor tmod) f = do
  tmod2 <- modTrav tmod f
  return $! PullActor tmod2
effectTrav (Teleport a) f = do
  b <- f a
  return $! Teleport b
effectTrav (ActivateEqp symbol) _ = return $! ActivateEqp symbol
effectTrav (OnSmash effa) f = do
  effb <- effectTrav effa f
  return $! OnSmash effb
effectTrav (Explode t) _ = return $! Explode t
effectTrav (TimedAspect k asp) f = do
  asp2 <- aspectTrav asp f
  return $! TimedAspect k asp2

-- | Transform an aspect using a stateful function.
aspectTrav :: Aspect a -> (a -> St.State s b) -> St.State s (Aspect b)
aspectTrav (Periodic a) f = do
  b <- f a
  return $! Periodic b
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
aspectTrav (ArmorMelee a) f = do
  b <- f a
  return $! ArmorMelee b
aspectTrav (SightRadius a) f = do
  b <- f a
  return $! SightRadius b
aspectTrav (SmellRadius a) f = do
  b <- f a
  return $! SmellRadius b
aspectTrav (AddLight a) f = do
  b <- f a
  return $! AddLight b

-- | Transform a throwing mod using a stateful function.
modTrav :: ThrowMod a -> (a -> St.State s b) -> St.State s (ThrowMod b)
modTrav ThrowMod{..} f = do
  throwVelocityNew <- f throwVelocity
  throwLingerNew <- f throwLinger
  return $! ThrowMod{ throwVelocity = throwVelocityNew
                    , throwLinger = throwLingerNew }
