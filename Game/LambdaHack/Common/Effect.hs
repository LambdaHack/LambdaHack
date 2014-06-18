{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
-- | Effects of content on other content. No operation in this module
-- involves the 'State' or 'Action' type.
module Game.LambdaHack.Common.Effect
  ( Effect(..), Aspect(..), ThrowMod(..)
  , effectTrav, aspectTrav
  , effectToSuffix, aspectToSuffix, kindEffectToSuffix, kindAspectToSuffix
  , affixPower, affixBonus
  ) where

import Control.Exception.Assert.Sugar
import qualified Control.Monad.State as St
import Data.Binary
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Game.LambdaHack.Common.Ability as Ability
import GHC.Generics (Generic)

import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Msg

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
  | TimedAspect !Int !(Aspect a)  -- enable the aspect for k clips
  deriving (Show, Read, Eq, Ord, Generic, Functor)

-- | Aspects of items, tiles, etc. The type argument represents power.
-- either as a random formula dependent on level, or as a final rolled value.
data Aspect a =
    NoAspect
  | Periodic !a
  | AddMaxHP !a
  | AddMaxCalm !a
  | AddSpeed !a
  | AddAbility !Ability.Ability
  | DeleteAbility !Ability.Ability
  | ArmorMelee !a
  | SightRadius !a
  | SmellRadius !a
  | Explode !Text  -- ^ explode, producing this group of shrapnel
  deriving (Show, Read, Eq, Ord, Generic, Functor)

-- | Parameters modifying a trow.
data ThrowMod a = ThrowMod
  { throwVelocity :: !a  -- ^ fly with this percentage of base throw speed
  , throwLinger   :: !a  -- ^ fly for this percentage of 2 turns
  }
  deriving (Show, Read, Eq, Ord, Generic, Functor)

instance Hashable a => Hashable (Effect a)

instance Hashable a => Hashable (Aspect a)

instance Hashable a => Hashable (ThrowMod a)

instance Binary a => Binary (Effect a)

instance Binary a => Binary (Aspect a)

instance Binary a => Binary (ThrowMod a)

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
effectTrav (TimedAspect k asp) f = do
  asp2 <- aspectTrav asp f
  return $! TimedAspect k asp2

-- | Transform an aspect using a stateful function.
aspectTrav :: Aspect a -> (a -> St.State s b) -> St.State s (Aspect b)
aspectTrav NoAspect _ = return NoAspect
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
aspectTrav (AddAbility as) _ = return $! AddAbility as
aspectTrav (DeleteAbility as) _ = return $! DeleteAbility as
aspectTrav (ArmorMelee a) f = do
  b <- f a
  return $! ArmorMelee b
aspectTrav (SightRadius a) f = do
  b <- f a
  return $! SightRadius b
aspectTrav (SmellRadius a) f = do
  b <- f a
  return $! SmellRadius b
aspectTrav (Explode t) _ = return $! Explode t

-- | Transform a throwing mod using a stateful function.
modTrav :: ThrowMod a -> (a -> St.State s b) -> St.State s (ThrowMod b)
modTrav ThrowMod{..} f = do
  throwVelocityNew <- f throwVelocity
  throwLingerNew <- f throwLinger
  return $! ThrowMod{ throwVelocity = throwVelocityNew
                    , throwLinger = throwLingerNew }

-- | Suffix to append to a basic content name if the content causes the effect.
effectToSuff :: (Show a, Ord a, Num a) => Effect a -> (a -> Text) -> Text
effectToSuff effect f =
  case St.evalState (effectTrav effect $ return . f) () of
    NoEffect -> ""
    Heal p | p > 0 -> "of healing" <+> affixBonus p
    Heal 0 -> assert `failure` effect
    Heal p -> "of wounding" <+> affixBonus p
    Hurt dice t -> "(" <> tshow dice <> ")" <+> t
    Calm p | p > 0 -> "of soothing" <+> affixBonus p
    Calm 0 -> assert `failure` effect
    Calm p -> "of alarming" <+> affixBonus p
    Dominate -> "of domination"
    Impress -> "of impression"
    CallFriend p -> "of aid calling" <+> affixPower p
    Summon t -> "of summoning" <+> t
    CreateItem p -> "of item creation" <+> affixPower p
    ApplyPerfume -> "of rose water"
    Burn{} -> ""  -- often accompanies Light, too verbose, too boring
    Blast p -> "of explosion" <+> affixPower p
    Ascend p | p > 0 -> "of ascending" <+> affixPower p
    Ascend 0 -> assert `failure` effect
    Ascend p -> "of descending" <+> affixPower (- p)
    Escape{} -> "of escaping"
    Paralyze t -> "of paralysis" <+> t
    InsertMove t -> "of speed burst" <+> t
    DropBestWeapon -> "of disarming"
    DropEqp ' ' False -> "of equipment dropping"
    DropEqp symbol False ->
      "of equipment '" <> T.singleton symbol <> "' dropping"
    DropEqp ' ' True -> "of equipment smashing"
    DropEqp symbol True ->
      "of equipment '" <> T.singleton symbol <> "' smashing"
    SendFlying ThrowMod{..} ->
      case effect of
        SendFlying tmod -> "of impact" <+> tmodToSuff tmod
        _ -> assert `failure` effect
    PushActor ThrowMod{..} ->
      case effect of
        PushActor tmod -> "of pushing" <+> tmodToSuff tmod
        _ -> assert `failure` effect
    PullActor ThrowMod{..} ->
      case effect of
        PullActor tmod -> "of pulling" <+> tmodToSuff tmod
        _ -> assert `failure` effect
    Teleport t ->
      case effect of
        Teleport p | p > 9 -> "of teleport" <+> t
        Teleport _ -> "of blinking" <+> t
        _ -> assert `failure` effect
    ActivateEqp ' ' -> "of spontaneous activation"
    ActivateEqp symbol ->
      "of spontaneous '" <> T.singleton symbol <> "' activation"
    TimedAspect _ _ ->
      case effect of
        TimedAspect _ aspect -> aspectToSuff aspect f
        _ -> assert `failure` effect

tmodToSuff :: Show a => ThrowMod a -> Text
tmodToSuff ThrowMod{..} = tshow throwVelocity <+> tshow throwLinger

aspectToSuff :: Show a => Aspect a -> (a -> Text) -> Text
aspectToSuff aspect f =
  case St.evalState (aspectTrav aspect $ return . f) () of
    NoAspect -> ""
    Periodic _ ->
      case aspect of
        Periodic n -> "(" <> tshow n <+> "in 100)"
        _ -> assert `failure` aspect
    AddMaxHP t -> "(" <> t <+> "HP)"
    AddMaxCalm t -> "(" <> t <+> "Calm)"
    AddSpeed t -> "of speed" <+> t
    AddAbility t -> "of" <+> tshow t
    DeleteAbility t -> "disabling" <+> tshow t
    ArmorMelee t -> "[" <> t <> "]"
    SightRadius t -> "of sight" <+> t
    SmellRadius t -> "of smell" <+> t
    Explode{} -> ""

effectToSuffix :: Effect Int -> Text
effectToSuffix effect = effectToSuff effect affixBonus

aspectToSuffix :: Aspect Int -> Text
aspectToSuffix aspect = aspectToSuff aspect affixBonus

affixPower :: Int -> Text
affixPower p = case compare p 1 of
  EQ -> ""
  LT -> assert `failure` "power less than 1" `twith` p
  GT -> "(+" <> tshow p <> ")"

affixBonus :: Int -> Text
affixBonus p = case compare p 0 of
  EQ -> ""
  LT -> "(" <> tshow p <> ")"
  GT -> "(+" <> tshow p <> ")"

affixDice :: Dice.Dice -> Text
affixDice d = if Dice.minDice d == Dice.maxDice d
               then affixBonus (Dice.minDice d)
               else "(?)"

kindEffectToSuffix :: Effect Dice.Dice -> Text
kindEffectToSuffix effect = effectToSuff effect affixDice

kindAspectToSuffix :: Aspect Dice.Dice -> Text
kindAspectToSuffix aspect = aspectToSuff aspect affixDice
