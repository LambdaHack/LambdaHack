module Terrain
  (Terrain, DL(Light, Dark), rock, opening, floorDark, floorLight, unknown, stairs, door, deDoor, isFloor, isFloorDark, isRock, isOpening, isUnknown, isOpen, isExit, deStairs, fromDL, toDL, isAlight, lookTerrain, viewTerrain) where

import Control.Monad

import Data.Binary
import Data.Maybe

import qualified Color
import Geometry
import WorldLoc

import Color
import Effect
import Random

data TileKind = TileKind
  { usymbol  :: !Char         -- ^ map symbol
  , uname    :: String        -- ^ name
  , ucolor   :: !Color.Color  -- ^ map color
  , ucolor2  :: !Color.Color  -- ^ map color when not in FOV
  , ufreq    :: !Int          -- ^ created that often (within a group?)
  , ufeature :: [Feature]     -- ^ properties
  }
  deriving (Show, Eq, Ord)

data Feature =
    Walkable         -- ^ actors can walk through
  | Clear            -- ^ actors can see through
  | Exit             -- ^ is an exit from a room
  | Lit Int          -- ^ emits light; radius 0 means just the tile is lit
  | Aura Effect      -- ^ sustains the effect continuously
  | Cause Effect     -- ^ causes the effect when triggered
  | Change TileKind  -- ^ transitions when triggered
  | Climbable VDir   -- ^ triggered by climbing
  | Openable         -- ^ triggered by opening
  | Closable         -- ^ triggered by closable
  | Secret RollDice  -- ^ triggered by searching a number of times
  deriving (Show, Eq, Ord)

wall, doorOpen, doorClosed, doorSecret :: TileKind  -- TODO: , opening, floorDark, floorLight, unknown

wall = TileKind
  { usymbol  = '#'
  , uname    = "A wall."
  , ucolor   = Color.BrWhite
  , ucolor2  = Color.defFG
  , ufreq    = 100
  , ufeature = []
  }

doorOpen = TileKind
  { usymbol  = '\''
  , uname    = "An open door."
  , ucolor   = Color.Yellow
  , ucolor2  = Color.BrBlack
  , ufreq    = 100
  , ufeature = [Walkable, Clear, Exit, Lit 0, Change doorClosed, Openable]
  }

doorClosed = TileKind
  { usymbol  = '+'
  , uname    = "A closed door."
  , ucolor   = Color.Yellow
  , ucolor2  = Color.BrBlack
  , ufreq    = 100
  , ufeature = [Exit, Change doorClosed, Closable]
  }

doorSecret = wall
  { ufeature = [Change doorClosed, Secret (7, 2)]
  }

data Terrain =
    Rock
  | Opening
  | Floor DL
  | Unknown
  | Stairs DL VDir (Maybe WorldLoc)
  | Door (Maybe Int)  -- Nothing: open, Just 0: closed, otherwise secret
  deriving Show

instance Binary VDir where
  put = putWord8 . fromIntegral . fromEnum
  get = liftM (toEnum . fromIntegral) getWord8

instance Binary Terrain where
  put Rock            = putWord8 0
  put Opening         = putWord8 1
  put (Floor dl)      = putWord8 2 >> put dl
  put Unknown         = putWord8 3
  put (Stairs dl d n) = putWord8 5 >> put dl >> put d >> put n
  put (Door o)        = putWord8 6 >> put o
  get = do
          tag <- getWord8
          case tag of
            0 -> return Rock
            1 -> return Opening
            2 -> liftM Floor get
            3 -> return Unknown
            5 -> liftM3 Stairs get get get
            6 -> liftM Door get
            _ -> fail "no parse (Terrain)"

instance Eq Terrain where
  Rock == Rock = True
  Opening == Opening = True
  Floor l == Floor l' = l == l'
  Unknown == Unknown = True
  Stairs dl d t == Stairs dl' d' t' = dl == dl' && d == d' && t == t'
  Door o == Door o' = o == o'
  _ == _ = False

data DL = Dark | Light
  deriving (Eq, Show, Enum, Bounded)

instance Binary DL where
  put = putWord8 . fromIntegral . fromEnum
  get = liftM (toEnum . fromIntegral) getWord8

rock, opening, floorDark, floorLight, unknown :: Terrain
rock = Rock
opening = Opening
floorDark = Floor Dark
floorLight = Floor Light
unknown = Unknown

stairs :: DL -> VDir -> Maybe WorldLoc -> Terrain
stairs = Stairs

deStairs :: Terrain -> Maybe (VDir, Maybe WorldLoc)
deStairs (Stairs _ vdir next) = Just (vdir, next)
deStairs _                    = Nothing

deDoor :: Terrain -> Maybe (Maybe Int)
deDoor (Door n) = Just n
deDoor _        = Nothing

door :: Maybe Int -> Terrain
door = Door

isFloor :: Terrain -> Bool
isFloor (Floor _) = True
isFloor _         = False

isFloorDark :: Terrain -> Bool
isFloorDark (Floor Dark) = True
isFloorDark _            = False

isRock :: Terrain -> Bool
isRock Rock = True
isRock _    = False

isOpening :: Terrain -> Bool
isOpening Opening = True
isOpening _       = False

isUnknown :: Terrain -> Bool
isUnknown Unknown = True
isUnknown _       = False

-- | allows moves and vision
isOpen :: Terrain -> Bool
isOpen (Floor {})   = True
isOpen Opening      = True
isOpen (Door o)     = isNothing o
isOpen (Stairs {})  = True
isOpen _            = False

-- | marks an exit from a room
isExit :: Terrain -> Bool
isExit (Stairs  {}) = True
isExit (Opening {}) = True
isExit (Door    {}) = True
isExit _            = False

fromDL :: DL -> Bool
fromDL Dark = False
fromDL Light = True

toDL :: Bool -> DL
toDL False = Dark
toDL True  = Light

-- | is lighted on its own
isAlight :: Terrain -> Bool
isAlight (Floor l)      = fromDL l
isAlight (Stairs l _ _) = fromDL l
isAlight _              = False

-- | Produces a textual description for terrain, used if no objects
-- are present.
lookTerrain :: Terrain -> String
lookTerrain (Floor _)         = "Floor."
lookTerrain Opening           = "An opening."
lookTerrain (Stairs _ Up _)   = "A staircase up."
lookTerrain (Stairs _ Down _) = "A staircase down."
lookTerrain (Door Nothing)    = "An open door."
lookTerrain (Door (Just 0))   = "A closed door."
lookTerrain (Door (Just _))   = "A wall."  -- secret
lookTerrain Rock              = "Rock."
lookTerrain Unknown           = ""

-- | The parameter "n" is the level of evolution:
--
-- 0: final
-- 1: stairs added
-- 2: doors added
-- 3: corridors and openings added
-- 4: only rooms
--
-- The Bool indicates whether the loc is currently visible.
viewTerrain :: Bool -> Terrain -> (Char, Color.Color)
viewTerrain b t =
  let def =     if b then Color.BrWhite else Color.defFG
      defDark = if b then Color.BrYellow else Color.BrBlack
      defDoor = if b then Color.Yellow else Color.BrBlack
  in case t of
       Rock            -> ('#', def)
       Opening         -> ('.', def)
       (Floor d)       -> ('.', if d == Light then def else defDark)
       Unknown         -> (' ', def)
       (Stairs d p _)  -> (if p == Up then '<' else '>',
                           if d == Light then def else defDark)
       (Door (Just 0)) -> ('+', defDoor)
       (Door (Just _)) -> viewTerrain b Rock  -- secret door
       (Door Nothing)  -> ('\'', defDoor)
