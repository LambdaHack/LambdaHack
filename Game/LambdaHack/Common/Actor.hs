{-# LANGUAGE DeriveGeneric #-}
-- | Actors in the game: heroes, monsters, etc.
module Game.LambdaHack.Common.Actor
  ( -- * Actor identifiers
    ActorId
    -- * The@ Acto@r type, its components and operations on them
  , Actor(..), ResDelta(..), ActorMaxSkills, WaitState(..)
  , deltaSerious, deltaMild, actorCanMelee
  , momentarySpeed, gearSpeed, actorTemplate, waitedLastTurn, actorDying
  , hpTooLow, calmEnough, hpEnough
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

-- | Actor properties that are changing throughout the game.
-- If they appear dublets of properties of actor kinds, e.g. HP,
-- they may be results of casting the dice specified in their respective
-- actor kind and/or may be modified temporarily, but return
-- to the original value from their respective kind over time.
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
  , bwait       :: WaitState    -- ^ length of the actor's waiting streak
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

data WaitState = Watch | Wait Int | Sleep
  deriving (Show, Eq, Generic)

instance Binary WaitState

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

-- | Current physical speed, whether from being pushed or from organs and gear.
momentarySpeed :: Actor -> Ability.Skills -> Speed
momentarySpeed !b actorMaxSk =
  case btrajectory b of
    Nothing -> gearSpeed actorMaxSk
    Just (_, speed) -> speed

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
      bwait   = Watch  -- overriden elsewhere, sometimes
      bhpDelta = ResDelta (0, 0) (0, 0)
      bcalmDelta = ResDelta (0, 0) (0, 0)
  in Actor{..}

waitedLastTurn :: Actor -> Bool
{-# INLINE waitedLastTurn #-}
waitedLastTurn b = case bwait b of
  Wait{} -> True
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
monsterGenChance (Dice.AbsDepth n) (Dice.AbsDepth totalDepth)
                 lvlAlreadySpawned actorCoeff =
  assert (totalDepth > 0 && n > 0) $
    -- Heroes have to endure a level depth-sized wave of immediate
    -- spawners for each level and only then the monsters start
    -- to trickle more and more slowly, at the speed dictated
    -- by @actorCoeff@ specified in cave kind.
    -- On level 1/10, first 4 monsters spawn immediately, at level 5/10,
    -- 8 spawn immediately. In general at level n, n+3 spawn at once.
    let scaledDepth = n * 10 `div` totalDepth
        coeff = actorCoeff * (lvlAlreadySpawned - scaledDepth - 2)
    in chance $ 1%fromIntegral (coeff `max` 1)

-- | How long until an actor's smell vanishes from a tile.
smellTimeout :: Delta Time
smellTimeout = timeDeltaScale (Delta timeTurn) 100
