module State where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Data.Binary
import qualified Config

import Movable
import Geometry
import Level
import Item
import Message

-- | The 'State' contains all the game state that has to be saved.
-- In practice, we maintain extra state, but that state is state
-- accumulated during a turn or relevant only to the current session.
-- TODO: consider changing slevel to LevelName, removing the lname field
-- and not removing the current level from the dungeon.
data State = State
  { splayer      :: Actor,        -- ^ represents the player-controlled movable
    scursor      :: Cursor,       -- ^ cursor location and level to return to
    shistory     :: [Message],
    ssensory     :: SensoryMode,
    sdisplay     :: DisplayMode,
    stime        :: Time,
    sassocs      :: Assocs,       -- ^ how every item appears
    sdiscoveries :: Discoveries,  -- ^ items (types) that have been discovered
    sdungeon     :: Dungeon,      -- ^ all but the current dungeon level
    slevel       :: Level,
    sconfig      :: Config.CP
  }
  deriving Show

data Cursor = Cursor
  { ctargeting :: Bool,       -- ^ are we in targeting mode?
    clocLn     :: LevelName,  -- ^ cursor level
    clocation  :: Loc,        -- ^ cursor coordinates
    creturnLn  :: LevelName   -- ^ the level current player resides on
  }
  deriving Show

defaultState :: Dungeon -> Level -> State
defaultState dng lvl =
  State
    (AHero 0)
    (Cursor False (LambdaCave (-1)) (-1, -1) (lname lvl))
    []
    Implicit Normal
    0
    M.empty
    S.empty
    dng
    lvl
    (Config.defaultCP)

updateCursor :: (Cursor -> Cursor) -> State -> State
updateCursor f s = s { scursor = f (scursor s) }

updateHistory :: ([String] -> [String]) -> State -> State
updateHistory f s = s { shistory = f (shistory s) }

updateTime :: (Time -> Time) -> State -> State
updateTime f s = s { stime = f (stime s) }

updateDiscoveries :: (Discoveries -> Discoveries) -> State -> State
updateDiscoveries f s = s { sdiscoveries = f (sdiscoveries s) }

updateLevel :: (Level -> Level) -> State -> State
updateLevel f s = s { slevel = f (slevel s) }

updateDungeon :: (Dungeon -> Dungeon) -> State -> State
updateDungeon f s = s {sdungeon = f (sdungeon s)}

toggleVision :: State -> State
toggleVision s = s { ssensory = case ssensory s of Vision 1 -> Implicit
                                                   Vision n -> Vision (n-1)
                                                   _        -> Vision 3 }

toggleSmell :: State -> State
toggleSmell s = s { ssensory = if ssensory s == Smell then Implicit else Smell }

toggleOmniscient :: State -> State
toggleOmniscient s = s { sdisplay = if sdisplay s == Omniscient
                                    then Normal
                                    else Omniscient }

toggleTerrain :: State -> State
toggleTerrain s = s { sdisplay = case sdisplay s of Terrain 1 -> Normal
                                                    Terrain n -> Terrain (n-1)
                                                    _         -> Terrain 4 }

instance Binary State where
  put (State player cursor hst sense disp time assocs discs dng lvl config) =
    do
      put player
      put cursor
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
      cursor <- get
      hst    <- get
      sense  <- get
      disp   <- get
      time   <- get
      assocs <- get
      discs  <- get
      dng    <- get
      lvl    <- get
      config <- get
      return
        (State player cursor hst sense disp time assocs discs dng lvl config)

instance Binary Cursor where
  put (Cursor act cln loc rln) =
    do
      put act
      put cln
      put loc
      put rln
  get =
    do
      act <- get
      cln <- get
      loc <- get
      rln <- get
      return (Cursor act cln loc rln)

data SensoryMode =
    Implicit
  | Vision Int
  | Smell
  deriving (Show, Eq)

instance Binary SensoryMode where
  put Implicit   = putWord8 0
  put (Vision n) = putWord8 1 >> put n
  put Smell      = putWord8 2
  get = do
          tag <- getWord8
          case tag of
            0 -> return Implicit
            1 -> liftM Vision get
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
