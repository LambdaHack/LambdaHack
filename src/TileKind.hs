module TileKind
  (TileKind(..), Feature(..))
  where

import qualified Color
import Effect
import Random
import qualified Content

data TileKind = TileKind
  { usymbol  :: !Char         -- ^ map symbol
  , uname    :: !String       -- ^ name
  , ucolor   :: !Color.Color  -- ^ map color
  , ucolor2  :: !Color.Color  -- ^ map color when not in FOV
  , ufreq    :: !Int          -- ^ created that often (within a group?)
  , ufeature :: ![Feature]    -- ^ properties
  }
  deriving (Show, Eq, Ord)

data Feature =
    Walkable           -- ^ actors can walk through
  | Clear              -- ^ actors can see through
  | Exit               -- ^ is an exit from a room
  | Lit                -- ^ is lit; TODO: (partially) replace ucolor by this feature?
  | Secret !RollDice   -- ^ tile is generated with this high tsecret field
  | Aura !Effect       -- ^ sustains the effect continuously
  | Cause !Effect      -- ^ causes the effect when triggered
  | Change !Char       -- ^ transitions when triggered
  | Climbable          -- ^ triggered by climbing
  | Descendable        -- ^ triggered by descending into
  | Openable           -- ^ triggered by opening
  | Closable           -- ^ triggered by closable
  | Hidden             -- ^ triggered when the tile's tsecret becomes (Just 0)
  deriving (Show, Eq, Ord)

instance Content.Content TileKind where
  getFreq = ufreq
  content =
    [wall, doorOpen, doorClosed, doorSecret, opening, floorLight, floorDark, stairsLightUp, stairsLightDown, stairsDarkUp, stairsDarkDown, unknown]

wall,      doorOpen, doorClosed, doorSecret, opening, floorLight, floorDark, stairsLightUp, stairsLightDown, stairsDarkUp, stairsDarkDown, unknown :: TileKind

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
  , ufeature = [Walkable, Clear, Exit{-TODO:, Lit-}, Change '+', Closable]
  }

doorClosed = TileKind
  { usymbol  = '+'
  , uname    = "A closed door."
  , ucolor   = Color.Yellow
  , ucolor2  = Color.BrBlack
  , ufreq    = 100
  , ufeature = [Exit, Change '\'', Openable]
  }

doorSecret = wall
  { ufeature = [Hidden, Change '+', Secret (7, 2)]
  }

opening = TileKind
  { usymbol  = '.'
  , uname    = "An opening."
  , ucolor   = Color.BrWhite
  , ucolor2  = Color.defFG
  , ufreq    = 100
  , ufeature = [Walkable, Clear, Exit{-TODO: , Lit-}]
  }

floorLight = TileKind
  { usymbol  = '.'
  , uname    = "Floor."
  , ucolor   = Color.BrWhite
  , ucolor2  = Color.defFG
  , ufreq    = 100
  , ufeature = [Walkable, Clear, Lit]
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
  , ufeature = [Walkable, Clear, Exit, Lit,
                Climbable, Cause Teleport]
  }

stairsLightDown = TileKind
  { usymbol  = '>'
  , uname    = "A staircase down."
  , ucolor   = Color.BrWhite
  , ucolor2  = Color.defFG
  , ufreq    = 100
  , ufeature = [Walkable, Clear, Exit, Lit,
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
