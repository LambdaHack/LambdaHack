module State where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Data.Binary
import qualified Data.ConfigFile
import qualified Config

import Monster
import Geometry
import Level
import Item
import Message

-- | The 'State' contains all the game state that has to be saved.
-- In practice, we maintain extra state, but that state is state
-- accumulated during a turn or relevant only to the current session.
data State = State
               { splayer      :: Player,
                 shistory     :: [Message],
                 ssensory     :: SensoryMode,
                 sdisplay     :: DisplayMode,
                 stime        :: Time,
                 sassocs      :: Assocs,       -- ^ how does every item appear
                 sdiscoveries :: Discoveries,  -- ^ items (types) have been discovered
                 sdungeon     :: Dungeon,      -- ^ all but current dungeon level
                 slevel       :: Level,
                 config       :: Data.ConfigFile.ConfigParser
               }
  deriving Show

defaultState :: Loc -> Dungeon -> Level -> State
defaultState ploc dng lvl =
  State
    (defaultPlayer ploc)
    []
    Implicit Normal
    0
    M.empty
    S.empty
    dng
    lvl
    Data.ConfigFile.emptyCP

updatePlayer :: (Monster -> Monster) -> State -> State
updatePlayer f s = s { splayer = f (splayer s) }

updateHistory :: ([String] -> [String]) -> State -> State
updateHistory f s = s { shistory = f (shistory s) }

updateDiscoveries :: (Discoveries -> Discoveries) -> State -> State
updateDiscoveries f s = s { sdiscoveries = f (sdiscoveries s) }

updateLevel :: (Level -> Level) -> State -> State
updateLevel f s = s { slevel = f (slevel s) }

toggleVision :: State -> State
toggleVision s = s { ssensory = if ssensory s == Vision then Implicit else Vision }

toggleSmell :: State -> State
toggleSmell s = s { ssensory = if ssensory s == Smell then Implicit else Smell }

toggleOmniscient :: State -> State
toggleOmniscient s = s { sdisplay = if sdisplay s == Omniscient then Normal else Omniscient }

toggleTerrain :: State -> State
toggleTerrain s = s { sdisplay = case sdisplay s of Terrain 1 -> Normal; Terrain n -> Terrain (n-1); _ -> Terrain 4 }

instance Binary State where
  put (State player hst sense disp time assocs discs dng lvl config) =
    do
      put player
      put hst
      put sense
      put disp
      put time
      put assocs
      put discs
      put dng
      put lvl
      put config
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
      lvl    <- get
      config <- get
      return (State player hst sense disp time assocs discs dng lvl config)

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
