{-# LANGUAGE DeriveGeneric, TupleSections #-}
-- | Actors in the game: heroes, monsters, etc.
module Game.LambdaHack.Common.Actor
  ( -- * The@ Acto@r type, its components and operations on them
    Actor(..), ResDelta(..), ActorMaxSkills, Watchfulness(..)
  , deltasSerious, deltasSeriousThisTurn
  , deltasHears, deltaBenign, deltaWasBenign
  , actorCanMelee, actorCanMeleeToHarm, actorWorthChasing, actorWorthKilling
  , gearSpeed, actorTemplate, actorWaits, actorWaitsOrSleeps, actorDying
  , hpTooLow, calmEnough, calmFull, hpFull, canSleep, prefersSleep
  , checkAdjacent, eqpOverfull, eqpFreeN
  , getCarriedIidsAndTrunk, getCarriedIidCStore
    -- * Assorted
  , ActorDict, monsterGenChance, smellTimeout
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import           Data.Int (Int64)
import           GHC.Generics (Generic)

import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Core.Dice as Dice
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs

-- | Actor attributes that are changing throughout the game.
-- If they appear to be dublets of aspects from actor kinds, e.g. HP,
-- they may be results of casting the dice specified in their respective
-- actor kind and/or may be modified temporarily, but return
-- to the original value from their respective kind over time.
--
-- Other properties of an actor, in particular its current aspects,
-- are derived from the actor's trunk, organs and equipment.
-- A class of the aspects, the boolean ones, are called flags.
-- Another class are skills. Stats are a subclass that determines
-- if particular actions are permitted for the actor (or faction).
data Actor = Actor
  { -- The trunk of the actor's body (present also in @borgan@ or @beqp@)
    btrunk      :: ItemId       -- ^ the trunk organ of the actor's body
  , bnumber     :: Maybe Int    -- ^ continued team character identity
                                --   index number in this game

    -- Resources
  , bhp         :: Int64        -- ^ current hit points * 1M
  , bhpDelta    :: ResDelta     -- ^ HP delta this turn * 1M
  , bcalm       :: Int64        -- ^ current calm * 1M
  , bcalmDelta  :: ResDelta     -- ^ calm delta this turn * 1M

    -- Location
  , bpos        :: Point        -- ^ current position
  , boldpos     :: Maybe Point  -- ^ previous position, if any
  , blid        :: LevelId      -- ^ current level
  , bfid        :: FactionId    -- ^ faction the actor currently belongs to
  , btrajectory :: Maybe ([Vector], Speed)
                                -- ^ trajectory the actor must
                                --   travel and his travel speed

    -- Items
  , borgan      :: ItemBag      -- ^ organs
  , beqp        :: ItemBag      -- ^ personal equipment
  , bweapon     :: Int          -- ^ number of weapons among eqp and organs
  , bweapBenign :: Int          -- ^ number of benign items among weapons

    -- Assorted
  , bwatch      :: Watchfulness -- ^ state of the actor's watchfulness
  , bproj       :: Bool         -- ^ is a projectile? affects being able
                                --   to fly through other projectiles, etc.
  }
  deriving (Show, Eq, Generic)

instance Binary Actor

-- | Representation of recent changes to HP of Calm of an actor.
-- This is reset every time the actor perfoms an action, so this is
-- aggregated over actor turn (move), not time turn.
-- The resource changes recorded in the tuple are, respectively,
-- negative and positive.
data ResDelta = ResDelta
  { resCurrentTurn  :: (Int64, Int64)  -- ^ resource change this move
  , resPreviousTurn :: (Int64, Int64)  -- ^ resource change previous move
  }
  deriving (Show, Eq, Generic)

instance Binary ResDelta

type ActorMaxSkills = EM.EnumMap ActorId Ability.Skills

-- | All actors on the level, indexed by actor identifier.
type ActorDict = EM.EnumMap ActorId Actor

data Watchfulness = WWatch | WWait Int | WSleep | WWake
  deriving (Show, Eq, Generic)

instance Binary Watchfulness

deltasSerious :: ResDelta -> Bool
deltasSerious ResDelta{..} = fst resCurrentTurn <= minusM2
                             || fst resPreviousTurn <= minusM2

deltasSeriousThisTurn :: ResDelta -> Bool
deltasSeriousThisTurn ResDelta{..} = fst resCurrentTurn <= minusM2

deltasHears :: ResDelta -> Bool
deltasHears ResDelta{..} = fst resCurrentTurn == minusM1
                           || fst resPreviousTurn == minusM1

deltaBenign :: ResDelta -> Bool
deltaBenign ResDelta{resCurrentTurn} =
  fst resCurrentTurn >= 0  -- only the current one

deltaWasBenign :: ResDelta -> Bool
deltaWasBenign ResDelta{resPreviousTurn} =
  fst resPreviousTurn >= 0  -- only the previous one

actorCanMelee :: ActorMaxSkills -> ActorId -> Actor -> Bool
{-# INLINE actorCanMelee #-}
actorCanMelee actorMaxSkills aid b =
  let actorMaxSk = actorMaxSkills EM.! aid
      condUsableWeapon = bweapon b > 0
      canMelee = Ability.getSk Ability.SkMelee actorMaxSk > 0
  in condUsableWeapon && canMelee

actorCanMeleeToHarm :: ActorMaxSkills -> ActorId -> Actor -> Bool
{-# INLINE actorCanMeleeToHarm #-}
actorCanMeleeToHarm actorMaxSkills aid b =
  let actorMaxSk = actorMaxSkills EM.! aid
      condUsableWeapon = bweapon b - bweapBenign b > 0
      canMelee = Ability.getSk Ability.SkMelee actorMaxSk > 0
  in condUsableWeapon && canMelee

-- Don't target/melee nonmoving actors, including sleeping, because nonmoving
-- can't be lured nor ambushed nor can chase us. However, do target
-- if they have loot or can attack at range or may become very powerful
-- through regeneration if left alone.
actorWorthChasing :: ActorMaxSkills -> ActorId -> Actor -> Bool
actorWorthChasing actorMaxSkills aid b =
  let hasLoot = not (EM.null $ beqp b)
        -- even consider "unreported inventory", for speed and KISS
      actorMaxSk = actorMaxSkills EM.! aid
  in bproj b
     || (Ability.getSk Ability.SkMove actorMaxSk > 0
         || bwatch b == WWake  -- probably will start moving very soon
         || hasLoot
         || Ability.getSk Ability.SkProject actorMaxSk > 0
         || bwatch b == WSleep
            && Ability.getSk Ability.SkMaxHP actorMaxSk > 30)
              -- too dangerous when regenerates through sleep;
              -- heroes usually fall into this category
        && bhp b > 0

-- Whether worth killing if already chased down.
actorWorthKilling :: ActorMaxSkills -> ActorId -> Actor -> Bool
actorWorthKilling actorMaxSkills aid b =
  actorWorthChasing actorMaxSkills aid b
  || actorCanMeleeToHarm actorMaxSkills aid b && bhp b > 0

-- | The speed from organs and gear; being pushed is ignored.
gearSpeed :: Ability.Skills -> Speed
gearSpeed actorMaxSk = toSpeed $
  max minSpeed (Ability.getSk Ability.SkSpeed actorMaxSk)  -- see @minimalSpeed@

actorTemplate :: ItemId -> Maybe Int -> Int64 -> Int64 -> Point -> LevelId
              -> FactionId -> Bool
              -> Actor
actorTemplate btrunk bnumber bhp bcalm bpos blid bfid bproj =
  let btrajectory = Nothing
      boldpos = Nothing
      borgan = EM.empty
      beqp = EM.empty
      bweapon = 0
      bweapBenign = 0
      bwatch = WWatch  -- overriden elsewhere, sometimes
      bhpDelta = ResDelta (0, 0) (0, 0)
      bcalmDelta = ResDelta (0, 0) (0, 0)
  in Actor{..}

actorWaits :: Actor -> Bool
{-# INLINE actorWaits #-}
actorWaits b = case bwatch b of
  WWait{} -> True
  _ -> False

actorWaitsOrSleeps :: Actor -> Bool
{-# INLINE actorWaitsOrSleeps #-}
actorWaitsOrSleeps b = case bwatch b of
  WWait{} -> True
  WSleep -> True
  _ -> False

-- | Projectile that ran out of steam or collided with obstacle, dies.
-- Even if it pierced through an obstacle, but lost its payload
-- while altering the obstacle during piercing, it dies, too.
actorDying :: Actor -> Bool
actorDying b = bhp b <= 0
               || bproj b && (maybe True (null . fst) (btrajectory b)
                              || EM.null (beqp b))

hpTooLow :: Actor -> Ability.Skills -> Bool
hpTooLow b actorMaxSk =
  5 * bhp b < xM (Ability.getSk Ability.SkMaxHP actorMaxSk) && bhp b <= xM 40
  || bhp b <= oneM

-- | Check if actor calm enough to perform some actions.
--
-- If max Calm is zero, always holds, to permit removing disastrous
-- equipped items, which would otherwise be stuck forever.
calmEnough :: Actor -> Ability.Skills -> Bool
calmEnough b actorMaxSk =
  let calmMax = Ability.getSk Ability.SkMaxCalm actorMaxSk
  in 2 * xM calmMax <= 3 * bcalm b

calmFull :: Actor -> Ability.Skills -> Bool
calmFull b actorMaxSk =
  let calmMax = Ability.getSk Ability.SkMaxCalm actorMaxSk
  in xM calmMax <= bcalm b

hpFull :: Actor -> Ability.Skills -> Bool
hpFull b actorMaxSk = xM (Ability.getSk Ability.SkMaxHP actorMaxSk) <= bhp b

-- | Has the skill and can wake up easily, so can sleep safely.
canSleep :: Ability.Skills -> Bool
canSleep actorMaxSk = Ability.getSk Ability.SkWait actorMaxSk >= 3
                      && (Ability.getSk Ability.SkSight actorMaxSk > 0
                          || Ability.getSk Ability.SkHearing actorMaxSk > 0)

-- | Can't loot, not too aggresive, so sometimes prefers to sleep
-- instead of exploring.
prefersSleep :: Ability.Skills -> Bool
prefersSleep actorMaxSk = Ability.getSk Ability.SkMoveItem actorMaxSk <= 0
                          && Ability.getSk Ability.SkAggression actorMaxSk < 2

checkAdjacent :: Actor -> Actor -> Bool
checkAdjacent sb tb = blid sb == blid tb && adjacent (bpos sb) (bpos tb)

eqpOverfull :: Actor -> Int -> Bool
eqpOverfull b n = let size = sum $ map fst $ EM.elems $ beqp b
                  in assert (size <= 10 `blame` (b, n, size))
                     $ size + n > 10

eqpFreeN :: Actor -> Int
eqpFreeN b = let size = sum $ map fst $ EM.elems $ beqp b
             in assert (size <= 10 `blame` (b, size))
                $ 10 - size

getCarriedIidsAndTrunk :: Actor -> [ItemId]
getCarriedIidsAndTrunk b =
  -- The trunk is important for a case of spotting a caught projectile
  -- with a stolen projecting item. This actually does happen.
  let trunk = EM.singleton (btrunk b) quantSingle
  in EM.keys $ EM.unionsWith const [beqp b, borgan b, trunk]

getCarriedIidCStore :: Actor -> [(ItemId, CStore)]
getCarriedIidCStore b =
  let bagCarried (cstore, bag) = map (,cstore) $ EM.keys bag
  in concatMap bagCarried [(CEqp, beqp b), (COrgan, borgan b)]

-- | Chance, in parts per million, that a new monster is generated.
-- Depends on the number of monsters already present, and on the level depth
-- and its cave kind.
--
-- Note that sometimes monsters spawn in groups, increasing danger,
-- but many monsters are generated asleep, decreasing initial danger.
monsterGenChance :: Dice.AbsDepth -> Dice.AbsDepth -> Int -> Int -> Int
monsterGenChance (Dice.AbsDepth ldepth) (Dice.AbsDepth totalDepth)
                 lvlSpawned actorCoeff =
  assert (totalDepth > 0 && ldepth > 0) $  -- ensured by content validation
    -- Heroes have to endure a level-depth-proportional wave of almost
    -- immediate spawners for each level. Then the monsters start
    -- to trickle more and more slowly, at the speed dictated
    -- by @actorCoeff@ specified in cave kind. Finally, spawning flattens out
    -- to ensure that camping is never safe.
    let scaledDepth = ldepth * 10 `div` totalDepth
        maxCoeff = 100 * 30
          -- spawning on a level with benign @actorCoeff@ flattens out
          -- after 30+depth spawns and on a level with fast spawning
          -- flattens out later, but ending at the same level
        coeff = max 1 $ min maxCoeff
                $ actorCoeff * (lvlSpawned - scaledDepth - 2)
        million = 1000000
    in 10 * million `div` coeff

-- | How long until an actor's smell vanishes from a tile.
smellTimeout :: Delta Time
smellTimeout = timeDeltaScale (Delta timeTurn) 200
