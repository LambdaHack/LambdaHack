module State where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Data.Binary

import Monster
import Geometry
import Level
import Item

-- | The 'State' contains all the game state except the current dungeon
-- level which we usually keep separate.
data State = State
               { splayer      :: Monster,
                 shistory     :: [String],
                 ssensory     :: SensoryMode,
                 sdisplay     :: DisplayMode,
                 stime        :: Time,
                 sassocs      :: Assocs,
                 sdiscoveries :: Discoveries,
                 sdungeon     :: Dungeon       -- ^ all but current dungeon level
               }
  deriving Show

defaultState ploc dng =
  State
    (defaultPlayer ploc)
    []
    Implicit Normal
    0
    M.empty
    S.empty
    dng

updatePlayer :: State -> (Monster -> Monster) -> State
updatePlayer s f = s { splayer = f (splayer s) }

updateHistory :: State -> ([String] -> [String]) -> State
updateHistory s f = s { shistory = f (shistory s) }

toggleVision :: State -> State
toggleVision s = s { ssensory = if ssensory s == Vision then Implicit else Vision }

toggleSmell :: State -> State
toggleSmell s = s { ssensory = if ssensory s == Smell then Implicit else Smell }

toggleOmniscient :: State -> State
toggleOmniscient s = s { sdisplay = if sdisplay s == Omniscient then Normal else Omniscient }

toggleTerrain :: State -> State
toggleTerrain s = s { sdisplay = case sdisplay s of Terrain 1 -> Normal; Terrain n -> Terrain (n-1); _ -> Terrain 4 }

instance Binary State where
  put (State player hst sense disp time assocs discs dng) =
    do
      put player
      put hst
      put sense
      put disp
      put time
      put assocs
      put discs
      put dng
  get =
    do
      player <- get
      hst    <- get
      sense  <- get
      disp   <- get
      time   <- get
      assocs <- get
      discs  <- get
      dng    <- get
      return (State player hst sense disp time assocs discs dng)

data SensoryMode =
    Implicit
  | Vision
  | Smell
  deriving (Show, Eq)

instance Binary SensoryMode where
  put Implicit = putWord8 0
  put Vision   = putWord8 1
  put Smell    = putWord8 2
  get = do
          tag <- getWord8
          case tag of
            0 -> return Implicit
            1 -> return Vision
            2 -> return Smell
            _ -> fail "no parse (SensoryMode)"

data DisplayMode =
    Normal
  | Omniscient
  | Terrain Int
  deriving (Show, Eq)

instance Binary DisplayMode where
  put Normal      = putWord8 0
  put Omniscient  = putWord8 1
  put (Terrain n) = putWord8 2 >> put n
  get = do
          tag <- getWord8
          case tag of
            0 -> return Normal
            1 -> return Omniscient
            2 -> liftM Terrain get
            _ -> fail "no parse (DisplayMode)"

