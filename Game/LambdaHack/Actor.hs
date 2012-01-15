-- | Actors in the game: monsters and heroes.
module Game.LambdaHack.Actor
  ( -- * Actor identifiers and related operations
    ActorId(..), isAHero, isAMonster, invalidActorId
  , findHeroName, monsterGenChance
    -- * The@ Acto@r type
  , Actor(..), template, addHp, unoccupied, heroKindId
    -- * Type of na actor target
  , Target(..)
  ) where

import Control.Monad
import Data.Binary
import Data.Maybe
import Data.Ratio

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Geometry
import Game.LambdaHack.Dir
import Game.LambdaHack.Loc
import Game.LambdaHack.Content.ActorKind
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Random
import qualified Game.LambdaHack.Config as Config

-- | Monster properties that are changing a lot. If they are dublets
-- of properties from ActorKind, the intention is they may be modified
-- temporarily, but tend to return to the original value over time. E.g., HP.
data Actor = Actor
  { bkind   :: !(Kind.Id ActorKind)  -- ^ the kind of the actor
  , bsymbol :: !(Maybe Char)         -- ^ individual map symbol
  , bname   :: !(Maybe String)       -- ^ individual name
  , bhp     :: !Int                  -- ^ current hit pints
  , bdir    :: !(Maybe (Dir, Int))   -- ^ the direction and distance of running
  , btarget :: Target                -- ^ the target for distance attacks and AI
  , bloc    :: !Loc                  -- ^ current location
  , bletter :: !Char                 -- ^ next inventory letter
  , btime   :: !Time                 -- ^ time of next action
  }
  deriving Show

instance Binary Actor where
  put (Actor ak an as ah ad at al ale ati) = do
    put ak
    put an
    put as
    put ah
    put ad
    put at
    put al
    put ale
    put ati
  get = do
    ak  <- get
    an  <- get
    as  <- get
    ah  <- get
    ad  <- get
    at  <- get
    al  <- get
    ale <- get
    ati <- get
    return (Actor ak an as ah ad at al ale ati)

-- ActorId operations

-- | A unique identifier of an actor in a dungeon.
data ActorId = AHero    !Int  -- ^ hero index (on the lheroes intmap)
             | AMonster !Int  -- ^ monster index (on the lmonsters intmap)
  deriving (Show, Eq, Ord)

instance Binary ActorId where
  put (AHero n)    = putWord8 0 >> put n
  put (AMonster n) = putWord8 1 >> put n
  get = do
    tag <- getWord8
    case tag of
      0 -> liftM AHero get
      1 -> liftM AMonster get
      _ -> fail "no parse (ActorId)"

-- | Checks whether an actor identifier represents a hero.
isAHero :: ActorId -> Bool
isAHero (AHero _) = True
isAHero (AMonster _) = False

-- | Checks whether an actor identifier represents a monster.
isAMonster :: ActorId -> Bool
isAMonster = not . isAHero

-- | An actor that is not on any level.
invalidActorId :: ActorId
invalidActorId = AMonster (-1)

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
monsterGenChance d numMonsters =
  chance $ 1%(fromIntegral (250 + 200 * (numMonsters - d)) `max` 50)

-- Actor operations

-- TODO: Setting the time of new monsters to 0 makes them able to
-- move immediately after generation. This does not seem like
-- a bad idea, but it would certainly be "more correct" to set
-- the time to the creation time instead.
-- | A template for a new actor. The initial target is invalid
-- to force a reset ASAP.
template :: Kind.Id ActorKind -> Maybe Char -> Maybe String -> Int -> Loc
         -> Actor
template mk mc ms hp loc =
  let invalidTarget = TEnemy invalidActorId loc
  in Actor mk mc ms hp Nothing invalidTarget loc 'a' 0

-- | Modify current hit points of an actor.
addHp :: Kind.Ops ActorKind -> Int -> Actor -> Actor
addHp Kind.Ops{okind} extra m =
  assert (extra >= 0 `blame` extra) $
  let maxHP = maxDice (ahp $ okind $ bkind m)
      currentHP = bhp m
  in if currentHP > maxHP
     then m
     else m{bhp = min maxHP (currentHP + extra)}

-- | Checks for the presence of actors in a location.
-- Does not check if the tile is open.
unoccupied :: [Actor] -> Loc -> Bool
unoccupied actors loc =
  all (\ body -> bloc body /= loc) actors

-- | The unique kind of heroes.
heroKindId :: Kind.Ops ActorKind -> Kind.Id ActorKind
heroKindId Kind.Ops{ouniqGroup} = ouniqGroup "hero"

-- Target

-- | The type of na actor target.
data Target =
    TEnemy ActorId Loc  -- ^ target an actor; last seen location
  | TLoc Loc            -- ^ target a given location
  | TCursor             -- ^ target the current position of the cursor; default
  deriving (Show, Eq)

instance Binary Target where
  put (TEnemy a ll) = putWord8 0 >> put a >> put ll
  put (TLoc loc) = putWord8 1 >> put loc
  put TCursor    = putWord8 2
  get = do
    tag <- getWord8
    case tag of
      0 -> liftM2 TEnemy get get
      1 -> liftM TLoc get
      2 -> return TCursor
      _ -> fail "no parse (Target)"
