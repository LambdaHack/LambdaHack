module State where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import Control.Monad
import Data.Binary
import qualified Config

import Actor
import Geometry
import Level
import Dungeon
import Item
import Message
import qualified ItemKind
import WorldLoc

-- | The 'State' contains all the game state that has to be saved.
-- In practice, we maintain extra state, but that state is state
-- accumulated during a turn or relevant only to the current session.
-- TODO: consider changing slevel to LevelId, removing the lname field
-- and not removing the current level from the dungeon.
data State = State
  { splayer      :: ActorId,      -- ^ represents the player-controlled actor
    scursor      :: Cursor,       -- ^ cursor location and level to return to
    shistory     :: [Message],
    ssensory     :: SensoryMode,
    sdisplay     :: DisplayMode,
    stime        :: Time,
    sassocs      :: Assocs,       -- ^ how every item appears
    sdiscoveries :: Discoveries,  -- ^ items (kinds) that have been discovered
    sdungeon     :: Dungeon,      -- ^ all but the current dungeon level
    slevel       :: Level,
    scounter     :: (Int, Int),   -- ^ stores next hero index and monster index
    sconfig      :: Config.CP
  }
  deriving Show

data Cursor = Cursor
  { ctargeting :: Bool,       -- ^ are we in targeting mode?
    clocLn     :: LevelId,  -- ^ cursor level
    clocation  :: Loc,        -- ^ cursor coordinates
    creturnLn  :: LevelId   -- ^ the level current player resides on
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
    (0, 0)
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
toggleVision s = s { ssensory = case ssensory s of Vision 1 -> Smell
                                                   Vision n -> Vision (n-1)
                                                   Smell    -> Implicit
                                                   Implicit -> Vision 3 }

toggleOmniscient :: State -> State
toggleOmniscient s = s { sdisplay = if sdisplay s == Omniscient
                                    then Normal
                                    else Omniscient }

toggleTerrain :: State -> State
toggleTerrain s = s { sdisplay = case sdisplay s of Terrain 1 -> Normal
                                                    Terrain n -> Terrain (n-1)
                                                    _         -> Terrain 4 }

instance Binary State where
  put (State player cursor hst sense disp time assocs discs dng lvl ct config) =
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
      put ct
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
      ct     <- get
      config <- get
      return
        (State player cursor hst sense disp time assocs discs dng lvl ct config)

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
  | Terrain Int  -- TODO: unused right now
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
