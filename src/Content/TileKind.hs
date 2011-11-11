module Content.TileKind (TileKind(..)) where

import Color
import qualified Content.Content
import qualified Effect
import Feature

data TileKind = TileKind
  { tsymbol  :: !Char       -- ^ map symbol
  , tname    :: !String     -- ^ name
  , tcolor   :: !Color      -- ^ map color
  , tcolor2  :: !Color      -- ^ map color when not in FOV
  , tfreq    :: !Int        -- ^ created that often (within a group?)
  , tfeature :: ![Feature]  -- ^ properties
  }
  deriving (Show, Eq, Ord)

instance Content.Content.Content TileKind where
  getFreq = tfreq
  content =
    [wall, doorOpen, doorClosed, doorSecret, opening, floorLight, floorDark, stairsUp, stairsDown, unknown]

wall,      doorOpen, doorClosed, doorSecret, opening, floorLight, floorDark, stairsUp, stairsDown, unknown :: TileKind

wall = TileKind
  { tsymbol  = '#'
  , tname    = "A wall."
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = []
  }

doorOpen = TileKind
  { tsymbol  = '\''
  , tname    = "An open door."
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfreq    = 100
  , tfeature = [Walkable, Clear, Exit{-TODO:, Lit-}, Change '+', Closable]
  }

doorClosed = TileKind
  { tsymbol  = '+'
  , tname    = "A closed door."
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfreq    = 100
  , tfeature = [Exit, Change '\'', Openable]
  }

doorSecret = wall
  { tfeature = [Hidden, Change '+', Secret (7, 2)]
  }

opening = TileKind
  { tsymbol  = '.'
  , tname    = "An opening."
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = [Walkable, Clear, Exit{-TODO: , Lit-}]
  }

floorLight = TileKind
  { tsymbol  = '.'
  , tname    = "Floor."
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = [Walkable, Clear, Lit]
  }

floorDark = TileKind
  { tsymbol  = '.'
  , tname    = "Floor."
  , tcolor   = BrYellow
  , tcolor2  = BrBlack
  , tfreq    = 100
  , tfeature = [Walkable, Clear]
  }

stairsUp = TileKind
  { tsymbol  = '<'
  , tname    = "A staircase up."
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = [Walkable, Clear, Exit, Lit,
                Climbable, Cause Effect.Teleport]
  }

stairsDown = TileKind
  { tsymbol  = '>'
  , tname    = "A staircase down."
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = [Walkable, Clear, Exit, Lit,
                Descendable, Cause Effect.Teleport]
  }

unknown = TileKind
  { tsymbol  = ' '
  , tname    = ""
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = []
  }
