module State where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S
import Control.Monad
import Data.Binary
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
  { splayer      :: Hero,         -- ^ the selected hero
    slook        :: Maybe Look,   -- ^ cursor, new target, initial level
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

data Look = Look
  { cursorLoc :: Loc,
    newTarget :: Target,
    returnLn  :: LevelName
  }
  deriving Show

defaultState :: Hero -> Dungeon -> Level -> State
defaultState player dng lvl =
  State
    player
    Nothing
    []
    Implicit Normal
    0
    M.empty
    S.empty
    dng
    lvl
    (Config.defaultCP)

updatePlayer :: (Hero -> Hero) -> State -> State
updatePlayer f s = s { splayer = f (splayer s) }

-- | The level on which the current player resides.
playerLevel :: State -> LevelName
playerLevel (State { slevel = level,
                     slook  = look }) =
  maybe (lname level) returnLn look

levelHeroAssocs :: State -> [(Int, Hero)]
levelHeroAssocs (State { splayer = player,
                         slook   = look,
                         slevel  = level@Level { lheroes = hs } }) =
  case look of
    Just (Look { returnLn = ln })
      | ln /= lname level ->
        -- player not on the currently selected level
        IM.assocs hs
    _ -> (heroNumber player, player) : IM.assocs hs

levelHeroList :: State -> [Hero]
levelHeroList s = snd $ L.unzip $ levelHeroAssocs s

findHeroLevel :: Int -> State -> Maybe (LevelName, Hero)
findHeroLevel ni state@(State { splayer  = player,
                                slevel   = level,
                                sdungeon = dungeon }) =
  if ni == heroNumber player
  then Just (playerLevel state, player)
  else
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
updateAnyHero f ni state
  | ni == heroNumber (splayer state) = updatePlayer f state
  | otherwise =
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

updateLook :: (Maybe Look -> Maybe Look) -> State -> State
updateLook f s = s { slook = f (slook s) }

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
  put (State player look hst sense disp time assocs discs dng lvl config) =
    do
      put player
      put look
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
      look   <- get
      hst    <- get
      sense  <- get
      disp   <- get
      time   <- get
      assocs <- get
      discs  <- get
      dng    <- get
      lvl    <- get
      config <- get
      return (State player look hst sense disp time assocs discs dng lvl config)

instance Binary Look where
  put (Look loc tgt ln) =
    do
      put loc
      put tgt
      put ln
  get =
    do
      loc <- get
      tgt <- get
      ln  <- get
      return (Look loc tgt ln)

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
