module Terrain
  (Terrain, rock, opening, floorDark, floorLight, unknown, stairs, door, deDoor, isFloor, isFloorDark, isRock, isOpening, isUnknown, isOpen, isExit, deStairs, isAlight, lookTerrain, viewTerrain) where

import Control.Monad

import Data.List as L
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
  | Lit Int          -- ^ emits light; radius 0 means just the tile is lit; TODO: (partially) replace ucolor by this feature?
  | Aura Effect      -- ^ sustains the effect continuously
  | Cause Effect     -- ^ causes the effect when triggered
  | Change TileKind  -- ^ transitions when triggered
  | Climbable        -- ^ triggered by climbing
  | Descendable      -- ^ triggered by descending into
  | Openable         -- ^ triggered by opening
  | Closable         -- ^ triggered by closable
  | Secret RollDice  -- ^ triggered when the tile's tsecret becomes (Just 0)
  deriving (Show, Eq, Ord)

wall, doorOpen, doorClosed, opening, floorLight, floorDark, unknown :: TileKind

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
  , ufeature = [Walkable, Clear, Exit, Lit 0, Change doorClosed, Closable]
  }

doorClosed = TileKind
  { usymbol  = '+'
  , uname    = "A closed door."
  , ucolor   = Color.Yellow
  , ucolor2  = Color.BrBlack
  , ufreq    = 100
  , ufeature = [Exit, Change doorOpen, Openable]
  }

-- TODO: probably should not be parameterized
doorSecret = wall
  { ufeature = [Change doorClosed, Secret (7, 2)]
  }

opening = TileKind
  { usymbol  = '.'
  , uname    = "An opening."
  , ucolor   = Color.BrWhite
  , ucolor2  = Color.defFG
  , ufreq    = 100
  , ufeature = [Walkable, Clear, Exit, Lit 0]
  }

floorLight = TileKind
  { usymbol  = '.'
  , uname    = "Floor."
  , ucolor   = Color.BrWhite
  , ucolor2  = Color.defFG
  , ufreq    = 100
  , ufeature = [Walkable, Clear, Lit 0]
  }

floorDark = TileKind
  { usymbol  = '.'
  , uname    = "Floor."
  , ucolor   = Color.BrYellow
  , ucolor2  = Color.BrBlack
  , ufreq    = 100
  , ufeature = [Walkable, Clear]
  }

stairsLightUp = TileKind
  { usymbol  = '<'
  , uname    = "A staircase up."
  , ucolor   = Color.BrWhite
  , ucolor2  = Color.defFG
  , ufreq    = 100
  , ufeature = [Walkable, Clear, Exit, Lit 0,
                Climbable, Cause Teleport]
  }

stairsLightDown = TileKind
  { usymbol  = '>'
  , uname    = "A staircase down."
  , ucolor   = Color.BrWhite
  , ucolor2  = Color.defFG
  , ufreq    = 100
  , ufeature = [Walkable, Clear, Exit, Lit 0,
                Descendable, Cause Teleport]
  }

stairsDarkUp = TileKind
  { usymbol  = '<'
  , uname    = "A staircase up."
  , ucolor   = Color.BrYellow
  , ucolor2  = Color.BrBlack
  , ufreq    = 100
  , ufeature = [Walkable, Clear, Exit,
                Climbable, Cause Teleport]
  }

stairsDarkDown = TileKind
  { usymbol  = '>'
  , uname    = "A staircase down."
  , ucolor   = Color.BrYellow
  , ucolor2  = Color.BrBlack
  , ufreq    = 100
  , ufeature = [Walkable, Clear, Exit,
                Descendable, Cause Teleport]
  }

unknown = TileKind
  { usymbol  = ' '
  , uname    = ""
  , ucolor   = Color.BrWhite
  , ucolor2  = Color.defFG
  , ufreq    = 100
  , ufeature = []
  }

type Terrain = TileKind

instance Binary VDir where
  put = putWord8 . fromIntegral . fromEnum
  get = liftM (toEnum . fromIntegral) getWord8

instance Binary TileKind where
  put _            = putWord8 0
  {-
  put Opening         = putWord8 1
  put (Floor dl)      = putWord8 2 >> put dl
  put Unknown         = putWord8 3
  put (Stairs dl d n) = putWord8 5 >> put dl >> put d >> put n
  put (Door o)        = putWord8 6 >> put o
-}
  get = do
          tag <- getWord8
          case tag of
            0 -> return wall
{-
            1 -> return Opening
            2 -> liftM Floor get
            3 -> return Unknown
            5 -> liftM3 Stairs get get get
            6 -> liftM Door get
-}
            _ -> fail "no parse (Terrain)"

rock = wall

stairs :: Bool -> VDir -> Terrain
stairs True Up    = stairsLightUp
stairs True Down  = stairsLightDown
stairs False Up   = stairsDarkUp
stairs False Down = stairsDarkDown

deStairs :: Terrain -> Maybe VDir
deStairs t =
  let isCD f = case f of Climbable -> True; Descendable -> True; _ -> False
      f = ufeature t
  in case L.filter isCD f of
       [Climbable] -> Just Up
       [Descendable] -> Just Down
       _ -> Nothing

deDoor :: Terrain -> Maybe (Maybe Bool)
deDoor t
  | L.elem Closable (ufeature t) = Just Nothing
  | L.elem Openable (ufeature t) = Just (Just False)
  | let isSecret f = case f of Secret _ -> True; _ -> False
    in L.any isSecret (ufeature t) = Just (Just True) -- TODO
  | otherwise = Nothing

door :: Maybe Int -> Terrain
door Nothing  = doorOpen
door (Just 0) = doorClosed
door (Just n) = doorSecret

isFloor :: Terrain -> Bool
isFloor t = uname t == "Floor."  -- TODO: hack

isFloorDark :: Terrain -> Bool
isFloorDark t = isFloor t && ucolor t == Color.BrYellow  -- TODO: hack

isRock :: Terrain -> Bool
isRock t = uname t == "A wall."  -- TODO: hack

isOpening :: Terrain -> Bool
isOpening t = uname t == "An opening."  -- TODO: hack

isUnknown :: Terrain -> Bool
isUnknown t = uname t == ""  -- TODO: hack

-- | allows moves and vision; TODO: separate
isOpen :: Terrain -> Bool
isOpen t = L.elem Clear (ufeature t)

-- | marks an exit from a room
isExit :: Terrain -> Bool
isExit t = L.elem Exit (ufeature t)

-- | is lighted on its own
isAlight :: Terrain -> Bool
isAlight t =
  let isLit f = case f of Lit _ -> True; _ -> False
  in L.any isLit (ufeature t)

-- | Produces a textual description for terrain, used if no objects
-- are present.
lookTerrain :: Terrain -> String
lookTerrain = uname

-- The Bool indicates whether the loc is currently visible.
viewTerrain :: Bool -> Terrain -> (Char, Color.Color)
viewTerrain b t =
  (usymbol t, if b then ucolor t else ucolor2 t)
