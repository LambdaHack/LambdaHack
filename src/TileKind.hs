module TileKind (TileKind(..)) where

import Color
import qualified Content
import qualified Effect
import Feature

data TileKind = TileKind
  { usymbol  :: !Char       -- ^ map symbol
  , uname    :: !String     -- ^ name
  , ucolor   :: !Color      -- ^ map color
  , ucolor2  :: !Color      -- ^ map color when not in FOV
  , ufreq    :: !Int        -- ^ created that often (within a group?)
  , ufeature :: ![Feature]  -- ^ properties
  }
  deriving (Show, Eq, Ord)

instance Content.Content TileKind where
  getFreq = ufreq
  content =
    [wall, doorOpen, doorClosed, doorSecret, opening, floorLight, floorDark, stairsLightUp, stairsLightDown, stairsDarkUp, stairsDarkDown, unknown]

wall,      doorOpen, doorClosed, doorSecret, opening, floorLight, floorDark, stairsLightUp, stairsLightDown, stairsDarkUp, stairsDarkDown, unknown :: TileKind

wall = TileKind
  { usymbol  = '#'
  , uname    = "A wall."
  , ucolor   = BrWhite
  , ucolor2  = defFG
  , ufreq    = 100
  , ufeature = []
  }

doorOpen = TileKind
  { usymbol  = '\''
  , uname    = "An open door."
  , ucolor   = Yellow
  , ucolor2  = BrBlack
  , ufreq    = 100
  , ufeature = [Walkable, Clear, Exit{-TODO:, Lit-}, Change '+', Closable]
  }

doorClosed = TileKind
  { usymbol  = '+'
  , uname    = "A closed door."
  , ucolor   = Yellow
  , ucolor2  = BrBlack
  , ufreq    = 100
  , ufeature = [Exit, Change '\'', Openable]
  }

doorSecret = wall
  { ufeature = [Hidden, Change '+', Secret (7, 2)]
  }

opening = TileKind
  { usymbol  = '.'
  , uname    = "An opening."
  , ucolor   = BrWhite
  , ucolor2  = defFG
  , ufreq    = 100
  , ufeature = [Walkable, Clear, Exit{-TODO: , Lit-}]
  }

floorLight = TileKind
  { usymbol  = '.'
  , uname    = "Floor."
  , ucolor   = BrWhite
  , ucolor2  = defFG
  , ufreq    = 100
  , ufeature = [Walkable, Clear, Lit]
  }

floorDark = TileKind
  { usymbol  = '.'
  , uname    = "Floor."
  , ucolor   = BrYellow
  , ucolor2  = BrBlack
  , ufreq    = 100
  , ufeature = [Walkable, Clear]
  }

stairsLightUp = TileKind
  { usymbol  = '<'
  , uname    = "A staircase up."
  , ucolor   = BrWhite
  , ucolor2  = defFG
  , ufreq    = 100
  , ufeature = [Walkable, Clear, Exit, Lit,
                Climbable, Cause Effect.Teleport]
  }

stairsLightDown = TileKind
  { usymbol  = '>'
  , uname    = "A staircase down."
  , ucolor   = BrWhite
  , ucolor2  = defFG
  , ufreq    = 100
  , ufeature = [Walkable, Clear, Exit, Lit,
                Descendable, Cause Effect.Teleport]
  }

stairsDarkUp = TileKind
  { usymbol  = '<'
  , uname    = "A staircase up."
  , ucolor   = BrYellow
  , ucolor2  = BrBlack
  , ufreq    = 100
  , ufeature = [Walkable, Clear, Exit,
                Climbable, Cause Effect.Teleport]
  }

stairsDarkDown = TileKind
  { usymbol  = '>'
  , uname    = "A staircase down."
  , ucolor   = BrYellow
  , ucolor2  = BrBlack
  , ufreq    = 100
  , ufeature = [Walkable, Clear, Exit,
                Descendable, Cause Effect.Teleport]
  }

unknown = TileKind
  { usymbol  = ' '
  , uname    = ""
  , ucolor   = BrWhite
  , ucolor2  = defFG
  , ufreq    = 100
  , ufeature = []
  }
