{-# LANGUAGE DeriveGeneric #-}
-- | Actors in the game: heroes, monsters, etc.
module Game.LambdaHack.Common.Actor
  ( -- * Actor identifiers
    ActorId
    -- * The@ Acto@r type, its components and operations on them
  , Actor(..), ResDelta(..), ActorMaxSkills, Watchfulness(..)
  , deltaSerious, deltaMild, actorCanMelee
  , gearSpeed, actorTemplate, waitedLastTurn, actorDying
  , hpTooLow, calmEnough, hpEnough, hpFull, canSleep, prefersSleep
  , checkAdjacent, eqpOverfull, eqpFreeN
    -- * Assorted
  , ActorDict, monsterGenChance, smellTimeout
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import           Data.Int (Int64)
import           Data.Ratio
import           GHC.Generics (Generic)

import qualified Game.LambdaHack.Common.Ability as Ability
import qualified Game.LambdaHack.Common.Dice as Dice
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.Random
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Vector

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
  , binv        :: ItemBag      -- ^ personal inventory pack
  , bweapon     :: Int          -- ^ number of weapons among eqp and organs

    -- Assorted
  , bwatch      :: Watchfulness -- ^ state of the actor's watchfulness
  , bproj       :: Bool         -- ^ is a projectile? affects being able
                                --   to fly through other projectiles, etc.
  }
  deriving (Show, Eq, Generic)

instance Binary Actor

-- The resource changes in the tuple are negative and positive, respectively.
data ResDelta = ResDelta
  { resCurrentTurn  :: (Int64, Int64)  -- ^ resource change this player turn
  , resPreviousTurn :: (Int64, Int64)  -- ^ resource change last player turn
  }
  deriving (Show, Eq, Generic)

instance Binary ResDelta

type ActorMaxSkills = EM.EnumMap ActorId Ability.Skills

-- | All actors on the level, indexed by actor identifier.
type ActorDict = EM.EnumMap ActorId Actor

data Watchfulness = WWatch | WWait Int | WSleep | WWake
  deriving (Show, Eq, Generic)

instance Binary Watchfulness

deltaSerious :: ResDelta -> Bool
deltaSerious ResDelta{..} =
  fst resCurrentTurn < 0 && fst resCurrentTurn /= minusM
  || fst resPreviousTurn < 0 && fst resPreviousTurn /= minusM

deltaMild :: ResDelta -> Bool
deltaMild ResDelta{..} = fst resCurrentTurn == minusM
                         || fst resPreviousTurn == minusM

actorCanMelee :: ActorMaxSkills -> ActorId -> Actor -> Bool
actorCanMelee actorMaxSkills aid b =
  let actorMaxSk = actorMaxSkills EM.! aid
      condUsableWeapon = bweapon b > 0
      canMelee = Ability.getSk Ability.SkMelee actorMaxSk > 0
  in condUsableWeapon && canMelee

-- | The speed from organs and gear; being pushed is ignored.
gearSpeed :: Ability.Skills -> Speed
gearSpeed actorMaxSk = toSpeed $
  max minSpeed (Ability.getSk Ability.SkSpeed actorMaxSk)  -- see @minimalSpeed@

actorTemplate :: ItemId -> Int64 -> Int64 -> Point -> LevelId -> FactionId
              -> Bool
              -> Actor
actorTemplate btrunk bhp bcalm bpos blid bfid bproj =
  let btrajectory = Nothing
      boldpos = Nothing
      borgan  = EM.empty
      beqp    = EM.empty
      binv    = EM.empty
      bweapon = 0
      bwatch  = WWatch  -- overriden elsewhere, sometimes
      bhpDelta = ResDelta (0, 0) (0, 0)
      bcalmDelta = ResDelta (0, 0) (0, 0)
  in Actor{..}

waitedLastTurn :: Actor -> Bool
{-# INLINE waitedLastTurn #-}
waitedLastTurn b = case bwatch b of
  WWait{} -> True
  _ -> False

actorDying :: Actor -> Bool
actorDying b = bhp b <= 0
               || bproj b && maybe True (null . fst) (btrajectory b)

hpTooLow :: Actor -> Ability.Skills -> Bool
hpTooLow b actorMaxSk =
  5 * bhp b < xM (Ability.getSk Ability.SkMaxHP actorMaxSk)
  && bhp b <= xM 40 || bhp b <= oneM

calmEnough :: Actor -> Ability.Skills -> Bool
calmEnough b actorMaxSk =
  let calmMax = max 1 $ Ability.getSk Ability.SkMaxCalm actorMaxSk
  in 2 * xM calmMax <= 3 * bcalm b && bcalm b > xM 10

hpEnough :: Actor -> Ability.Skills -> Bool
hpEnough b actorMaxSk =
  xM (Ability.getSk Ability.SkMaxHP actorMaxSk) <= 2 * bhp b && bhp b > oneM

hpFull :: Actor -> Ability.Skills -> Bool
hpFull b actorMaxSk = xM (Ability.getSk Ability.SkMaxHP actorMaxSk) <= bhp b

-- | Has the skill and can wake up easily, so can sleep safely.
canSleep :: Ability.Skills -> Bool
canSleep actorMaxSk = Ability.getSk Ability.SkWait actorMaxSk >= 2
                      && (Ability.getSk Ability.SkSight actorMaxSk > 0
                          || Ability.getSk Ability.SkHearing actorMaxSk > 0)

-- | Can't loot, so sometimes prefers to sleep instead of exploring.
prefersSleep :: Ability.Skills -> Bool
prefersSleep actorMaxSk = Ability.getSk Ability.SkMoveItem actorMaxSk <= 0

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

-- | Chance that a new monster is generated. Depends on the number
-- of monsters already present, and on the level depth and its cave kind.
monsterGenChance :: Dice.AbsDepth -> Dice.AbsDepth -> Int -> Int -> Rnd Bool
monsterGenChance (Dice.AbsDepth ldepth) (Dice.AbsDepth totalDepth)
                 lvlSpawned actorCoeff =
  assert (totalDepth > 0 && ldepth > 0) $
    -- The spawn speed is now doubled compared to the comment below,
    -- due to some monsters generated asleep:
    --
    -- Heroes have to endure a level depth-sized wave of immediate
    -- spawners for each level and only then the monsters start
    -- to trickle more and more slowly, at the speed dictated
    -- by @actorCoeff@ specified in cave kind.
    -- On level 1/10, first 4 monsters spawn immediately, at level 5/10,
    -- 8 spawn immediately. In general at level n, n+3 spawn at once.
    let scaledDepth = ldepth * 10 `div` totalDepth
        -- Never spawn too rarely so that camping is never safe.
        maxCoeff = 100 * 30  -- safe level after 30 spawns flattens out
        coeff = min maxCoeff $ actorCoeff * (lvlSpawned - scaledDepth - 2)
    in chance $ 2%fromIntegral (coeff `max` 1)  -- 2 --- doubled

-- | How long until an actor's smell vanishes from a tile.
smellTimeout :: Delta Time
smellTimeout = timeDeltaScale (Delta timeTurn) 200
