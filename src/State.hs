module State where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S
import Control.Monad
import Data.Binary
import Data.Maybe
import qualified Config

import Actor
import Monster
import Geometry
import Level
import Item
import Message

-- | The 'State' contains all the game state that has to be saved.
-- In practice, we maintain extra state, but that state is state
-- accumulated during a turn or relevant only to the current session.
data State = State
  { splayer      :: Actor,        -- ^ represents the selected movable
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
  { ctargeting :: Bool,      -- ^ are we in targeting mode?
    clocation   :: Loc,       -- ^ cursor coordinates
    creturnLn   :: LevelName  -- ^ the level current player resides on
  }
  deriving Show

defaultState :: Actor -> Loc -> Dungeon -> Level -> State
defaultState pl ploc dng lvl =
  State
    pl
    (Cursor False ploc (lname lvl))
    []
    Implicit Normal
    0
    M.empty
    S.empty
    dng
    lvl
    (Config.defaultCP)

getActor :: State -> Actor -> Movable
getActor (State { slevel = lvl }) a =
  case a of
    AHero n    -> lheroes lvl IM.! n
    AMonster n -> lmonsters lvl !! n

-- | Finds an actor body on any level.
-- Possible optimization: check current level first.
findAnyActor :: State -> Actor -> Movable
findAnyActor state@(State { slevel   = level,
                             sdungeon = dungeon }) a =
    let Dungeon m = putDungeonLevel level dungeon
        chk lvl =
          case a of
            AHero n    -> IM.lookup n (lheroes lvl)
            AMonster n -> let l = lmonsters lvl
                          in  if L.length l <= n then Nothing else Just $ l !! n
        filtered  = M.mapMaybe chk m
    in  fst $ fromMaybe (error "findAnyActor") $ M.minView $ filtered

getPlayerBody :: State -> Movable
getPlayerBody state = findAnyActor state (splayer state)

-- | The level on which the current player resides.
playerLevel :: State -> LevelName
playerLevel state = creturnLn $ scursor state

levelHeroAssocs :: State -> [(Int, Hero)]
levelHeroAssocs (State { slevel = Level { lheroes = hs } }) = IM.assocs hs

levelHeroList :: State -> [Hero]
levelHeroList s = snd $ L.unzip $ levelHeroAssocs s

findHeroLevel :: Int -> State -> Maybe (LevelName, Hero)
findHeroLevel ni state@(State { slevel   = level,
                                sdungeon = dungeon }) =
    let Dungeon m = putDungeonLevel level dungeon
        chk ln lvl = fmap (\ p -> (ln, p)) (IM.lookup ni (lheroes lvl))
        filtered   = M.mapMaybeWithKey chk m
    in  fmap fst $ M.minView $ filtered

-- | The list of all heroes except the player.
-- Heroes from the current level go first.
allLevelHeroes :: State -> [(Int, LevelName, Hero)]
allLevelHeroes state =
  let Dungeon m = sdungeon state
      one lvl = L.map (\ (i, p) -> (i, lname lvl, p)) (IM.assocs (lheroes lvl))
  in  L.concatMap one (slevel state : M.elems m)

updateAnyHero :: (Hero -> Hero) -> Int -> State -> State
updateAnyHero f ni state =
      case findHeroLevel ni state of
        Just (ln, _hero) ->
          let upd = IM.adjust f ni
          in  updateAnyLevel (updateHeroes upd) ln state
        Nothing -> error $ "updateAnyHero: hero " ++ show ni ++ " not found"

updateAnyLevel :: (Level -> Level) -> LevelName -> State -> State
updateAnyLevel f ln state@(State { slevel = level,
                                   sdungeon = Dungeon dng })
  | ln == lname level = updateLevel f state
  | otherwise = updateDungeon (const $ Dungeon $ M.adjust f ln dng) state

updateCursor :: (Cursor -> Cursor) -> State -> State
updateCursor f s = s { scursor = f (scursor s) }

updateHistory :: ([String] -> [String]) -> State -> State
updateHistory f s = s { shistory = f (shistory s) }

updateDiscoveries :: (Discoveries -> Discoveries) -> State -> State
updateDiscoveries f s = s { sdiscoveries = f (sdiscoveries s) }

updateLevel :: (Level -> Level) -> State -> State
updateLevel f s = s { slevel = f (slevel s) }

updateDungeon :: (Dungeon -> Dungeon) -> State -> State
updateDungeon f s = s {sdungeon = f (sdungeon s)}

updateTime :: (Time -> Time) -> State -> State
updateTime f s = s { stime = f (stime s) }

toggleVision :: State -> State
toggleVision s = s { ssensory = case ssensory s of Vision 1 -> Implicit; Vision n -> Vision (n-1); _ -> Vision 3 }

toggleSmell :: State -> State
toggleSmell s = s { ssensory = if ssensory s == Smell then Implicit else Smell }

toggleOmniscient :: State -> State
toggleOmniscient s = s { sdisplay = if sdisplay s == Omniscient then Normal else Omniscient }

toggleTerrain :: State -> State
toggleTerrain s = s { sdisplay = case sdisplay s of Terrain 1 -> Normal; Terrain n -> Terrain (n-1); _ -> Terrain 4 }

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
  put (Cursor act loc ln) =
    do
      put act
      put loc
      put ln
  get =
    do
      act <- get
      loc <- get
      ln  <- get
      return (Cursor act loc ln)

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
