{-# LANGUAGE DeriveGeneric #-}
-- | Actors in the game: heroes, monsters, etc. No operation in this module
-- involves the 'State' or 'Action' type.
module Game.LambdaHack.Common.Actor
  ( -- * Actor identifiers and related operations
    ActorId, monsterGenChance
    -- * The@ Acto@r type
  , Actor(..), ResDelta(..), ActorAspect
  , deltaSerious, deltaMild
  , bspeed, actorTemplate, braced, waitedLastTurn, actorDying
  , hpTooLow, calmEnough, hpEnough
    -- * Assorted
  , ActorDict, smellTimeout, checkAdjacent
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import Data.Int (Int64)
import Data.Ratio
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector

-- | Actor properties that are changing throughout the game.
-- If they are dublets of properties from @ActorKind@,
-- they are usually modified temporarily, but tend to return
-- to the original value from @ActorKind@ over time. E.g., HP.
data Actor = Actor
  { -- The trunk of the actor's body (present also in @borgan@ or @beqp@)
    btrunk      :: !ItemId

    -- Resources
  , bhp         :: !Int64        -- ^ current hit points * 1M
  , bhpDelta    :: !ResDelta     -- ^ HP delta this turn * 1M
  , bcalm       :: !Int64        -- ^ current calm * 1M
  , bcalmDelta  :: !ResDelta     -- ^ calm delta this turn * 1M

    -- Location
  , bpos        :: !Point        -- ^ current position
  , boldpos     :: !(Maybe Point)
                                 -- ^ previous position, if any
  , blid        :: !LevelId      -- ^ current level
  , boldlid     :: !LevelId      -- ^ previous level
  , bfid        :: !FactionId    -- ^ faction the actor currently belongs to
  , btrajectory :: !(Maybe ([Vector], Speed))
                                 -- ^ trajectory the actor must
                                 --   travel and his travel speed

    -- Items
  , borgan      :: !ItemBag      -- ^ organs
  , beqp        :: !ItemBag      -- ^ personal equipment
  , binv        :: !ItemBag      -- ^ personal inventory pack
  , bweapon     :: !Int          -- ^ number of weapons among eqp and organs

    -- Assorted
  , bwait       :: !Bool         -- ^ is the actor waiting right now?
  , bproj       :: !Bool         -- ^ is a projectile? (shorthand only,
                                 --   this can be deduced from btrunk)
  }
  deriving (Show, Eq, Generic)

instance Binary Actor

-- The resource changes in the tuple are negative and positive, respectively.
data ResDelta = ResDelta
  { resCurrentTurn  :: !(Int64, Int64)  -- ^ resource change this player turn
  , resPreviousTurn :: !(Int64, Int64)  -- ^ resource change last player turn
  }
  deriving (Show, Eq, Generic)

instance Binary ResDelta

type ActorAspect = EM.EnumMap ActorId AspectRecord

deltaSerious :: ResDelta -> Bool
deltaSerious ResDelta{..} =
  fst resCurrentTurn < 0 && fst resCurrentTurn /= minusM
  || fst resPreviousTurn < 0 && fst resCurrentTurn /= minusM

deltaMild :: ResDelta -> Bool
deltaMild ResDelta{..} = fst resCurrentTurn == minusM
                         || fst resPreviousTurn == minusM

-- | Chance that a new monster is generated. Currently depends on the
-- number of monsters already present, and on the level. In the future,
-- the strength of the character and the strength of the monsters present
-- could further influence the chance, and the chance could also affect
-- which monster is generated. How many and which monsters are generated
-- will also depend on the cave kind used to build the level.
monsterGenChance :: AbsDepth -> AbsDepth -> Int -> Int -> Rnd Bool
monsterGenChance _ _ _ 0 = return False
monsterGenChance (AbsDepth n) (AbsDepth totalDepth) lvlSpawned actorCoeff =
  assert (totalDepth > 0 && n > 0)
  -- Mimics @castDice@. On level 5/10, first 6 monsters appear fast.
  $ let scaledDepth = n * 10 `div` totalDepth
        -- Heroes have to endure two lvl-sized waves of spawners for each level.
        numSpawnedCoeff = lvlSpawned `div` 2
    in chance $ 1%(fromIntegral
                     ((actorCoeff * (numSpawnedCoeff - scaledDepth))
                      `max` 1))  -- monsters up to level depth spawned at once

-- Actor operations

-- | A template for a new actor.
actorTemplate :: ItemId -> Int64 -> Int64 -> Point -> LevelId -> FactionId
              -> Actor
actorTemplate btrunk bhp bcalm bpos blid bfid =
  let btrajectory = Nothing
      boldpos = Nothing
      boldlid = blid
      borgan  = EM.empty
      beqp    = EM.empty
      binv    = EM.empty
      bweapon = 0
      bwait   = False
      bhpDelta = ResDelta (0, 0) (0, 0)
      bcalmDelta = ResDelta (0, 0) (0, 0)
      bproj = False
  in Actor{..}

bspeed :: Actor -> AspectRecord -> Speed
bspeed !b AspectRecord{aSpeed} =
  case btrajectory b of
    Nothing -> toSpeed aSpeed
    Just (_, speed) -> speed

-- | Whether an actor is braced for combat this clip.
braced :: Actor -> Bool
braced = bwait

-- | The actor waited last turn.
waitedLastTurn :: Actor -> Bool
waitedLastTurn = bwait

actorDying :: Actor -> Bool
actorDying b = bhp b <= 0
               || bproj b && maybe True (null . fst) (btrajectory b)

hpTooLow :: Actor -> AspectRecord -> Bool
hpTooLow b AspectRecord{aMaxHP} =
  bhp b <= oneM || 5 * bhp b < xM aMaxHP && bhp b <= xM 10

calmEnough :: Actor -> AspectRecord -> Bool
calmEnough b AspectRecord{aMaxCalm} =
  let calmMax = max 1 aMaxCalm
  in 2 * xM calmMax <= 3 * bcalm b && bcalm b > xM 10

hpEnough :: Actor -> AspectRecord -> Bool
hpEnough b AspectRecord{aMaxHP} =
  let hpMax = max 1 aMaxHP
  in xM hpMax <= 3 * bhp b && bhp b >= xM 10

-- | How long until an actor's smell vanishes from a tile.
smellTimeout :: Delta Time
smellTimeout = timeDeltaScale (Delta timeTurn) 100

-- | All actors on the level, indexed by actor identifier.
type ActorDict = EM.EnumMap ActorId Actor

checkAdjacent :: Actor -> Actor -> Bool
checkAdjacent sb tb = blid sb == blid tb && adjacent (bpos sb) (bpos tb)
