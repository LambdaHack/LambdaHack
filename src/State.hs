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

defaultState :: Actor -> Dungeon -> Level -> State
defaultState pl dng lvl =
  State
    pl
    (Cursor False (LambdaCave (-1)) (-1, -1) (lname lvl))
    []
    Implicit Normal
    0
    M.empty
    S.empty
    dng
    lvl
    (Config.defaultCP)

-- The operations with "Any", and those that use them, consider all the dungeon.
-- All the other actor and level operations only consider the current level.

-- | Finds an actor body on any level. Error if not found.
findActorAnyLevel :: Actor -> State -> Maybe (LevelName, Movable)
findActorAnyLevel actor state@(State { slevel   = lvl,
                                       sdungeon = Dungeon m }) =
  let chk lvl =
        fmap (\ m -> (lname lvl, m)) $
        case actor of
          AHero n    -> IM.lookup n (lheroes lvl)
          AMonster n -> IM.lookup n (lmonsters lvl)
  in  listToMaybe $ mapMaybe chk (lvl : M.elems m)

getPlayerBody :: State -> Movable
getPlayerBody state = snd $ fromMaybe (error "getPlayerBody") $
                      findActorAnyLevel (splayer state) state

-- | The list of actors and levels for all heroes in the dungeon.
-- Heroes from the current level go first.
allHeroesAnyLevel :: State -> [(Actor, LevelName)]
allHeroesAnyLevel state =
  let Dungeon m = sdungeon state
      one (Level { lname = ln, lheroes = hs }) =
        L.map (\ (i, _) -> (AHero i, ln)) (IM.assocs hs)
  in  L.concatMap one (slevel state : M.elems m)

updateAnyActorBody :: Actor -> (Movable -> Movable) ->  State -> State
updateAnyActorBody actor f state =
  case findActorAnyLevel actor state of
    Just (ln, _) ->
      case actor of
        AHero n    -> updateAnyLevel (updateHeroes   $ IM.adjust f n) ln state
        AMonster n -> updateAnyLevel (updateMonsters $ IM.adjust f n) ln state
    Nothing -> error "updateAnyActorBody"

updateAnyLevel :: (Level -> Level) -> LevelName -> State -> State
updateAnyLevel f ln state@(State { slevel = level,
                                   sdungeon = Dungeon dng })
  | ln == lname level = updateLevel f state
  | otherwise = updateDungeon (const $ Dungeon $ M.adjust f ln dng) state

-- | Gets actor body from the current level. Error if not found.
getActor :: State -> Actor -> Movable
getActor (State { slevel = lvl }) a =
  case a of
    AHero n    -> lheroes   lvl IM.! n
    AMonster n -> lmonsters lvl IM.! n

-- | Removes the actor, if present, from the current level.
deleteActor :: Actor -> State -> State
deleteActor a =
  case a of
    AHero n    -> updateLevel (updateHeroes   (IM.delete n))
    AMonster n -> updateLevel (updateMonsters (IM.delete n))

-- | Add actor to the current level.
insertActor :: Actor -> Movable -> State -> State
insertActor a m =
  case a of
    AHero n    -> updateLevel (updateHeroes   (IM.insert n m))
    AMonster n -> updateLevel (updateMonsters (IM.insert n m))

levelHeroList :: State -> [Hero]
levelHeroList (State { slevel = Level { lheroes = hs } }) = IM.elems hs

levelMonsterList :: State -> [Hero]
levelMonsterList (State { slevel = Level { lmonsters = ms } }) = IM.elems ms

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
