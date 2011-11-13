module Game.LambdaHack.Actor where

import Data.Binary
import Control.Monad

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Geometry
import Game.LambdaHack.Dir
import Game.LambdaHack.Loc
import Game.LambdaHack.Content.ActorKind
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Random

-- | Monster properties that are changing a lot. If they are dublets
-- of properties form ActorKind, the intention is they may be modified
-- temporarily, but tend to return to the original value over time. E.g., HP.
data Actor = Actor
  { bkind   :: !(Kind.Id ActorKind)  -- ^ the kind of the actor
  , bname   :: !(Maybe String)  -- ^ individual name
  , bsymbol :: !(Maybe Char)    -- ^ individual map symbol
  , bhp     :: !Int             -- ^ current hit pints
  , bdir    :: !(Maybe Dir)     -- ^ the direction of running
  , btarget :: Target           -- ^ the target for distance attacks and AI
  , bloc    :: !Loc             -- ^ current location
  , bletter :: !Char            -- ^ next inventory letter
  , btime   :: !Time            -- ^ time of next action
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

data ActorId = AHero !Int     -- ^ hero index (on the lheroes intmap)
             | AMonster !Int  -- ^ monster index (on the lmonsters intmap)
  deriving (Show, Eq, Ord)

isAHero :: ActorId -> Bool
isAHero (AHero _) = True
isAHero (AMonster _) = False

isAMonster :: ActorId -> Bool
isAMonster = not . isAHero

addHp :: Int -> Actor -> Actor
addHp extra m =
  assert (extra >= 0 `blame` extra) $
  let maxHP = maxDice (ahp $ Kind.getKind $ bkind m)
      currentHP = bhp m
  in if currentHP > maxHP
     then m
     else m{bhp = min maxHP (currentHP + extra)}

-- Checks for the presence of actors. Does *not* check if the tile is open.
unoccupied :: [Actor] -> Loc -> Bool
unoccupied actors loc =
  all (\ body -> bloc body /= loc) actors

heroKindId :: Kind.Id ActorKind
heroKindId = Kind.getId ((== "hero") . aname)

instance Binary ActorId where
  put (AHero n)    = putWord8 0 >> put n
  put (AMonster n) = putWord8 1 >> put n
  get = do
          tag <- getWord8
          case tag of
            0 -> liftM AHero get
            1 -> liftM AMonster get
            _ -> fail "no parse (ActorId)"

data Target =
    TEnemy ActorId Loc  -- ^ fire at the actor; last seen location
  | TLoc Loc            -- ^ fire at a given location
  | TCursor             -- ^ fire at the current position of the cursor; default
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
