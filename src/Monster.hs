module Monster where

import Data.Char
import Data.Binary
import Control.Monad

import Actor
import Geometry
import Display
import Item
import Random
import qualified Config

-- | Hit points of the hero. Experimentally balanced for multiple heroes.
heroHP :: Config.CP -> Int
heroHP config =
  let b = Config.get config "heroes" "baseHp"
      k = Config.get config "heroes" "extraHeroes"
  in  k + b `div` (k + 1)

-- | Initial hero.
defaultHero :: Char -> String -> Loc -> Int -> Movable
defaultHero symbol name ploc hp =
  Movable (Hero symbol name) hp hp Nothing TCursor ploc [] 'a' 10 0

data Movable = Movable
                { mtype   :: !MovableType,
                  mhpmax  :: !Int,
                  mhp     :: !Int,
                  mdir    :: Maybe Dir,  -- for monsters: the dir the monster last moved; TODO: use target for this, instead and use mdir to signal the monster wants to switch position with a hero (if the monster is smart/big enough)
                                         -- for heroes: the dir the hero is running
                  mtarget :: Target,
-- TODO:          mper    :: Maybe Perception  -- see https://github.com/Mikolaj/LambdaHack/issues/issue/31
                  mloc    :: !Loc,
                  mitems  :: [Item],     -- inventory
                  mletter :: !Char,      -- next inventory letter
                  mspeed  :: !Time,      -- speed (i.e., delay before next action)
                  mtime   :: !Time }     -- time of next action
  deriving Show

instance Binary Movable where
  put (Movable mt mhpm mhp md tgt ml minv mletter mspeed mtime) =
    do
      put mt
      put mhpm
      put mhp
      put md
      put tgt
      put ml
      put minv
      put mletter
      put mspeed
      put mtime
  get = do
          mt      <- get
          mhpm    <- get
          mhp     <- get
          md      <- get
          tgt     <- get
          ml      <- get
          minv    <- get
          mletter <- get
          mspeed  <- get
          mtime   <- get
          return (Movable mt mhpm mhp md tgt ml minv mletter mspeed mtime)

data MovableType =
    Hero Char String
  | Eye
  | FastEye
  | Nose
  deriving (Show, Eq)

instance Binary MovableType where
  put (Hero symbol name) = putWord8 0 >> put symbol >> put name
  put Eye                = putWord8 1
  put FastEye            = putWord8 2
  put Nose               = putWord8 3
  get = do
          tag <- getWord8
          case tag of
            0 -> liftM2 Hero get get
            1 -> return Eye
            2 -> return FastEye
            3 -> return Nose
            _ -> fail "no parse (MovableType)"

data Target =
    TEnemy Actor  -- ^ fire at the actor (a monster or a hero)
  | TLoc Loc      -- ^ fire at a given location
  | TCursor       -- ^ fire at the current position of the cursor; the default
  deriving (Show, Eq)

instance Binary Target where
  put (TEnemy a) = putWord8 0 >> put a
  put (TLoc loc) = putWord8 1 >> put loc
  put TCursor    = putWord8 2
  get = do
          tag <- getWord8
          case tag of
            0 -> liftM TEnemy get
            1 -> liftM TLoc get
            2 -> return TCursor
            _ -> fail "no parse (Target)"

-- | Monster frequencies (TODO: should of course vary much more
-- on local circumstances).
monsterFrequency :: Frequency MovableType
monsterFrequency =
  Frequency
  [
    (2, Nose),
    (6, Eye),
    (1, FastEye)
  ]

-- | Generate monster.
newMonster :: Loc -> Frequency MovableType -> Rnd Movable
newMonster loc ftp =
    do
      tp <- frequency ftp
      hp <- hps tp
      let s = speed tp
      return (template tp hp loc s)
  where
    -- setting the time of new monsters to 0 makes them able to
    -- move immediately after generation; this does not seem like
    -- a bad idea, but it would certainly be "more correct" to set
    -- the time to the creation time instead
    template tp hp loc s = Movable tp hp hp Nothing TCursor loc [] 'a' s 0

    hps Eye      = randomR (1,12)  -- falls in 1--4 unarmed rounds
    hps FastEye  = randomR (1,6)   -- 1--2
    hps Nose     = randomR (6,13)  -- 2--5 and in 1 round of the strongest sword

    speed Eye      = 10
    speed FastEye  = 4
    speed Nose     = 11

-- Heroes are white, monsters are colorful.
viewMovable :: MovableType -> (Char, AttrColor)
viewMovable (Hero sym _) = (sym, white)
viewMovable Eye          = ('e', red)
viewMovable FastEye      = ('e', blue)
viewMovable Nose         = ('n', green)
