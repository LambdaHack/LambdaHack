-- | Actors in the game: monsters and heroes. No operation in this module
-- involves the 'State' or 'Action' type.
module Game.LambdaHack.Actor
  ( -- * Actor identifiers and related operations
    ActorId, findHeroName, monsterGenChance
    -- * The@ Acto@r type
  , Actor(..), template, addHp, timeAddFromSpeed, braced
  , unoccupied, heroKindId, projectileKindId, actorSpeed
    -- * Type of na actor target
  , Target(..)
  ) where

import Control.Monad
import Data.Binary
import Data.Maybe
import Data.Ratio

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector
import Game.LambdaHack.Point
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Random
import qualified Game.LambdaHack.Config as Config
import Game.LambdaHack.Time
import qualified Game.LambdaHack.Color as Color

-- | Actor properties that are changing throughout the game.
-- If they are dublets of properties from @ActorKind@,
-- they are usually modified temporarily, but tend to return
-- to the original value from @ActorKind@ over time. E.g., HP.
data Actor = Actor
  { bkind    :: !(Kind.Id ActorKind)    -- ^ the kind of the actor
  , bsymbol  :: !(Maybe Char)           -- ^ individual map symbol
  , bname    :: !(Maybe String)         -- ^ individual name
  , bcolor   :: !(Maybe Color.Color)    -- ^ individual map color
  , bspeed   :: !(Maybe Speed)          -- ^ individual speed
  , bhp      :: !Int                    -- ^ current hit points
  , bdir     :: !(Maybe (Vector, Int))  -- ^ direction and distance of running
  , btarget  :: Target                  -- ^ target for ranged attacks and AI
  , bloc     :: !Point                  -- ^ current location
  , bletter  :: !Char                   -- ^ next inventory letter
  , btime    :: !Time                   -- ^ absolute time of next action
  , bwait    :: !Time                   -- ^ last bracing expires at this time
  , bfaction :: !(Kind.Id FactionKind)  -- ^ to which faction the actor belongs
  , bproj    :: !Bool                   -- ^ is a projectile? (shorthand only,
                                        -- ^ this can be deduced from bkind)
  }
  deriving Show

instance Binary Actor where
  put Actor{..} = do
    put bkind
    put bsymbol
    put bname
    put bcolor
    put bspeed
    put bhp
    put bdir
    put btarget
    put bloc
    put bletter
    put btime
    put bwait
    put bfaction
    put bproj
  get = do
    bkind   <- get
    bsymbol <- get
    bname   <- get
    bcolor  <- get
    bspeed  <- get
    bhp     <- get
    bdir    <- get
    btarget <- get
    bloc    <- get
    bletter <- get
    btime   <- get
    bwait   <- get
    bfaction <- get
    bproj    <- get
    return Actor{..}

-- ActorId operations

-- | A unique identifier of an actor in a dungeon.
type ActorId = Int

-- | Find a hero name in the config file, or create a stock name.
findHeroName :: Config.CP -> Int -> String
findHeroName config n =
  let heroName = Config.getOption config "heroes" ("HeroName_" ++ show n)
  in fromMaybe ("hero number " ++ show n) heroName

-- | Chance that a new monster is generated. Currently depends on the
-- number of monsters already present, and on the level. In the future,
-- the strength of the character and the strength of the monsters present
-- could further influence the chance, and the chance could also affect
-- which monster is generated. How many and which monsters are generated
-- will also depend on the cave kind used to build the level.
monsterGenChance :: Int -> Int -> Rnd Bool
monsterGenChance depth numMonsters =
  chance $ 1%(fromIntegral (30 * (numMonsters - depth)) `max` 5)

-- Actor operations

-- | A template for a new non-projectile actor. The initial target is invalid
-- to force a reset ASAP.
template :: Kind.Id ActorKind -> Maybe Char -> Maybe String -> Int -> Point
         -> Time -> Kind.Id FactionKind -> Bool -> Actor
template bkind bsymbol bname bhp bloc btime bfaction bproj =
  let bcolor  = Nothing
      bspeed  = Nothing
      btarget = invalidTarget
      bdir    = Nothing
      bletter = 'a'
      bwait   = timeZero
  in Actor{..}

-- | Increment current hit points of an actor.
addHp :: Kind.Ops ActorKind -> Int -> Actor -> Actor
addHp Kind.Ops{okind} extra m =
  assert (extra >= 0 `blame` extra) $
  let maxHP = maxDice (ahp $ okind $ bkind m)
      currentHP = bhp m
  in if currentHP > maxHP
     then m
     else m {bhp = min maxHP (currentHP + extra)}

-- | Access actor speed, individual or, otherwise, stock.
actorSpeed :: Kind.Ops ActorKind -> Actor -> Speed
actorSpeed Kind.Ops{okind} m =
  let stockSpeed = aspeed $ okind $ bkind m
  in fromMaybe stockSpeed $ bspeed m

-- | Add time taken by a single step at the actor's current speed.
timeAddFromSpeed :: Kind.Ops ActorKind -> Actor -> Time -> Time
timeAddFromSpeed coactor m time =
  let speed = actorSpeed coactor m
      delta = ticksPerMeter speed
  in timeAdd time delta

-- | Whether an actor is braced for combat this turn.
braced :: Actor -> Time -> Bool
braced m time = time < bwait m

-- | Checks for the presence of actors in a location.
-- Does not check if the tile is walkable.
unoccupied :: [Actor] -> Point -> Bool
unoccupied actors loc =
  all (\ body -> bloc body /= loc) actors

-- | The unique kind of heroes.
heroKindId :: Kind.Ops ActorKind -> Kind.Id ActorKind
heroKindId Kind.Ops{ouniqGroup} = ouniqGroup "hero"

-- | The unique kind of projectiles.
projectileKindId :: Kind.Ops ActorKind -> Kind.Id ActorKind
projectileKindId Kind.Ops{ouniqGroup} = ouniqGroup "projectile"

-- Target

-- | The type of na actor target.
data Target =
    TEnemy ActorId Point  -- ^ target an actor with its last seen location
  | TLoc Point            -- ^ target a given location
  | TPath [Vector]        -- ^ target the list of locations one after another
  | TCursor               -- ^ target current position of the cursor; default
  deriving (Show, Eq)

-- | An invalid target, with an actor that is not on any level.
invalidTarget :: Target
invalidTarget =
  let invalidActorId = -1
  in TEnemy invalidActorId origin

instance Binary Target where
  put (TEnemy a ll) = putWord8 0 >> put a >> put ll
  put (TLoc loc) = putWord8 1 >> put loc
  put (TPath ls) = putWord8 2 >> put ls
  put TCursor    = putWord8 3
  get = do
    tag <- getWord8
    case tag of
      0 -> liftM2 TEnemy get get
      1 -> liftM TLoc get
      2 -> liftM TPath get
      3 -> return TCursor
      _ -> fail "no parse (Target)"
