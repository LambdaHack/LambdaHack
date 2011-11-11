module State where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import Data.Binary
import qualified Config
import qualified System.Random as R

import Actor
import Geometry
import Loc
import Level
import qualified Dungeon
import Item
import Message
import WorldLoc

-- | The 'State' contains all the game state that has to be saved.
-- In practice, we maintain extra state, but that state is state
-- accumulated during a turn or relevant only to the current session.
data State = State
  { splayer      :: ActorId      -- ^ represents the player-controlled actor
  , scursor      :: Cursor       -- ^ cursor location and level to return to
  , shistory     :: [Message]
  , ssensory     :: SensoryMode
  , sdisplay     :: DisplayMode
  , stime        :: Time
  , sflavour     :: FlavourMap   -- ^ association of flavour to items
  , sdisco       :: Discoveries  -- ^ items (kinds) that have been discovered
  , sdungeon     :: Dungeon.Dungeon  -- ^ all but the current dungeon level
  , slid         :: LevelId
  , scounter     :: (Int, Int)   -- ^ stores next hero index and monster index
  , sparty       :: IS.IntSet    -- ^ heroes in the party
  , srandom      :: R.StdGen     -- ^ current random generator
  , sconfig      :: Config.CP
  }
  deriving Show

data Cursor = Cursor
  { ctargeting :: Bool       -- ^ are we in targeting mode?
  , clocLn     :: LevelId    -- ^ cursor level
  , clocation  :: Loc        -- ^ cursor coordinates
  , creturnLn  :: LevelId    -- ^ the level current player resides on
  }
  deriving Show

slevel :: State -> Level
slevel State{slid, sdungeon} = sdungeon Dungeon.! slid

defaultState :: Dungeon.Dungeon -> LevelId -> Loc -> R.StdGen -> State
defaultState dng lid ploc g =
  State
    (AHero 0)  -- hack: the hero is not yet alive
    (Cursor False lid ploc lid)
    []
    Implicit Normal
    0
    M.empty
    S.empty
    dng
    lid
    (0, 0)
    IS.empty
    g
    Config.defaultCP

updateCursor :: (Cursor -> Cursor) -> State -> State
updateCursor f s = s { scursor = f (scursor s) }

updateHistory :: ([String] -> [String]) -> State -> State
updateHistory f s = s { shistory = f (shistory s) }

updateTime :: (Time -> Time) -> State -> State
updateTime f s = s { stime = f (stime s) }

updateDiscoveries :: (Discoveries -> Discoveries) -> State -> State
updateDiscoveries f s = s { sdisco = f (sdisco s) }

updateLevel :: (Level -> Level) -> State -> State
updateLevel f s = updateDungeon (Dungeon.adjust f (slid s)) s

updateDungeon :: (Dungeon.Dungeon -> Dungeon.Dungeon) -> State -> State
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
  put (State player cursor hst sense disp time flav disco dng lid ct
       party g config) =
    do
      put player
      put cursor
      put hst
      put sense
      put disp
      put time
      put flav
      put disco
      put dng
      put lid
      put ct
      put party
      put (show g)
      put config
  get =
    do
      player <- get
      cursor <- get
      hst    <- get
      sense  <- get
      disp   <- get
      time   <- get
      flav   <- get
      disco  <- get
      dng    <- get
      lid    <- get
      ct     <- get
      party  <- get
      g      <- get
      config <- get
      return
        (State player cursor hst sense disp time flav disco dng lid ct
         party (read g) config)

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
            1 -> fmap Vision get
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
            2 -> fmap Terrain get
            _ -> fail "no parse (DisplayMode)"
