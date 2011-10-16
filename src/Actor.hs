module Actor where

import Data.Binary
import Control.Monad
import Control.Exception (assert)

import Geometry
import Item
import Content.ActorKind
import qualified Kind
import Random


-- | Monster properties that are changing a lot. If they are dublets
-- of properties form ActorKind, the intention is they may be modified
-- temporarily, but will return to the original value over time. E.g., HP.
data Actor = Actor
  { akind   :: !(Kind.Id ActorKind)  -- ^ the kind of the actor
  , aname   :: !(Maybe String)  -- ^ individual name
  , asymbol :: !(Maybe Char)    -- ^ individual map symbol
  , ahp     :: !Int             -- ^ current hit pints
  , adir    :: !(Maybe Dir)     -- ^ the direction of running
  , atarget :: Target           -- ^ the target for distance attacks and AI
  , aloc    :: !Loc             -- ^ current location
  , aitems  :: [Item]           -- ^ inventory
  , aletter :: !Char            -- ^ next inventory letter
  , atime   :: !Time            -- ^ time of next action
  }
  deriving Show

instance Binary Actor where
  put (Actor ak an as ah ad at al ai ale ati) = do
    put ak
    put an
    put as
    put ah
    put ad
    put at
    put al
    put ai
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
    ai  <- get
    ale <- get
    ati <- get
    return (Actor ak an as ah ad at al ai ale ati)

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
  assert (extra >= 0) $
  let maxHP = maxDice (bhp $ Kind.getKind $ akind m)
      currentHP = ahp m
  in if currentHP > maxHP
     then m
     else m{ahp = min maxHP (currentHP + extra)}

heroKindId :: Kind.Id ActorKind
heroKindId = Kind.getId ((== "hero") . bname)

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
