module TileKind
  (TileKind(..), Feature(..), TileKindId, getKind, wallId, doorSecretId, openingId, floorDarkId, floorLightId, unknownId, stairs, door, deDoor, deStairs) where

import Control.Monad

import qualified Data.List as L
import qualified Data.IntMap as IM
import Data.Binary
import Data.Maybe

import qualified Color
import Geometry

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
    Walkable           -- ^ actors can walk through
  | Clear              -- ^ actors can see through
  | Exit               -- ^ is an exit from a room
  | Lit                -- ^ is lit; TODO: (partially) replace ucolor by this feature?
  | Aura Effect        -- ^ sustains the effect continuously
  | Cause Effect       -- ^ causes the effect when triggered
  | Change TileKindId  -- ^ transitions when triggered
  | Climbable          -- ^ triggered by climbing
  | Descendable        -- ^ triggered by descending into
  | Openable           -- ^ triggered by opening
  | Closable           -- ^ triggered by closable
  | Secret RollDice    -- ^ triggered when the tile's tsecret becomes (Just 0)
  deriving (Show, Eq, Ord)

newtype TileKindId = TileKindId Int
  deriving (Show, Eq, Ord)

instance Binary TileKindId where
  put (TileKindId i) = put i
  get = liftM TileKindId get

kindAssocs :: [(Int, TileKind)]
kindAssocs = L.zip [0..] content

kindMap :: IM.IntMap TileKind
kindMap = IM.fromDistinctAscList kindAssocs

getKind :: TileKindId -> TileKind
getKind (TileKindId i) = kindMap IM.! i

content :: [TileKind]
content =
  [wall, doorOpen, doorClosed, doorSecret, opening, floorLight, floorDark, stairsLightUp, stairsLightDown, stairsDarkUp, stairsDarkDown, unknown]

wall,    doorOpen, doorClosed, doorSecret, opening, floorLight, floorDark, stairsLightUp, stairsLightDown, stairsDarkUp, stairsDarkDown, unknown :: TileKind

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
  , ufeature = [Walkable, Clear, Exit{-TODO:, Lit-}, Change {-TODO: doorClosedId-} wallId, Closable]
  }

doorClosed = TileKind
  { usymbol  = '+'
  , uname    = "A closed door."
  , ucolor   = Color.Yellow
  , ucolor2  = Color.BrBlack
  , ufreq    = 100
  , ufeature = [Exit, Change doorOpenId, Openable]
  }

doorSecret = wall
  { ufeature = [Change doorClosedId, Secret (7, 2)]
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

wallId, openingId, floorDarkId, floorLightId, unknownId, doorOpenId, doorClosedId, doorSecretId :: TileKindId
wallId = TileKindId $ fromJust $ L.elemIndex wall content
openingId = TileKindId $ fromJust $ L.elemIndex opening content
floorDarkId = TileKindId $ fromJust $ L.elemIndex floorDark content
floorLightId = TileKindId $ fromJust $ L.elemIndex floorLight content
unknownId = TileKindId $ fromJust $ L.elemIndex unknown content
doorOpenId = TileKindId $ fromJust $ L.elemIndex doorOpen content
doorClosedId = TileKindId $ fromJust $ L.elemIndex doorClosed content
doorSecretId = TileKindId $ fromJust $ L.elemIndex doorSecret content

stairs :: Bool -> VDir -> TileKindId
stairs True Up    = TileKindId $ fromJust $ L.elemIndex stairsLightUp content
stairs True Down  = TileKindId $ fromJust $ L.elemIndex stairsLightDown content
stairs False Up   = TileKindId $ fromJust $ L.elemIndex stairsDarkUp content
stairs False Down = TileKindId $ fromJust $ L.elemIndex stairsDarkDown content

door :: Maybe Int -> TileKindId
door Nothing  = doorOpenId
door (Just 0) = doorClosedId
door (Just _) = doorSecretId

deStairs :: TileKindId -> Maybe VDir
deStairs t =
  let isCD f = case f of Climbable -> True; Descendable -> True; _ -> False
      fk = ufeature (getKind t)
  in case L.filter isCD fk of
       [Climbable] -> Just Up
       [Descendable] -> Just Down
       _ -> Nothing

deDoor :: TileKindId -> Maybe (Maybe Bool)
deDoor t
  | Closable `elem` ufeature (getKind t) = Just Nothing
  | Openable `elem` ufeature (getKind t) = Just (Just False)
  | let isSecret f = case f of Secret _ -> True; _ -> False
    in L.any isSecret (ufeature (getKind t)) = Just (Just True) -- TODO
  | otherwise = Nothing
