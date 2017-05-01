-- | Server and client game state types and operations.
module Game.LambdaHack.Common.State
  ( -- * Basic game state, local or global
    State
    -- * State components
  , sdungeon, stotalDepth, sactorD, sitemD, sfactionD, stime, scops, shigh, sgameModeId
    -- * State operations
  , defStateGlobal, emptyState, localFromGlobal
  , updateDungeon, updateDepth, updateActorD, updateItemD
  , updateFactionD, updateTime, updateCOps
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.HighScore as HighScore
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.TileKind (TileKind, unknownId)

-- | View on game state. "Remembered" fields carry a subset of the info
-- in the client copies of the state. Clients never directly change
-- their @State@, but apply atomic actions sent by the server to do so.
data State = State
  { _sdungeon    :: !Dungeon      -- ^ remembered dungeon
  , _stotalDepth :: !AbsDepth     -- ^ absolute dungeon depth, for item creation
  , _sactorD     :: !ActorDict    -- ^ remembered actors in the dungeon
  , _sitemD      :: !ItemDict     -- ^ remembered items in the dungeon
  , _sfactionD   :: !FactionDict  -- ^ remembered sides still in game
  , _stime       :: !Time         -- ^ global game time, for UI display only
  , _scops       :: Kind.COps     -- ^ remembered content
  , _shigh       :: !HighScore.ScoreDict  -- ^ high score table
  , _sgameModeId :: !(Kind.Id ModeKind)  -- ^ current game mode
  }
  deriving (Show, Eq)

unknownLevel :: Kind.COps -> AbsDepth -> X -> Y
             -> Text -> ([Point], [Point]) -> Int -> [Point] -> Bool
             -> Level
unknownLevel Kind.COps{cotile=Kind.Ops{ouniqGroup}}
             ldepth lxsize lysize ldesc lstair lclear lescape lnight =
  let outerId = ouniqGroup "basic outer fence"
  in Level { ldepth
           , lfloor = EM.empty
           , lembed = EM.empty
           , lactor = EM.empty
           , ltile = unknownTileMap outerId lxsize lysize
           , lxsize
           , lysize
           , lsmell = EM.empty
           , ldesc
           , lstair
           , lseen = 0
           , lclear
           , ltime = timeZero
           , lactorCoeff = 0
           , lactorFreq = []
           , litemNum = 0
           , litemFreq = []
           , lescape
           , lnight
           }

unknownTileMap :: Kind.Id TileKind -> Int -> Int -> TileMap
unknownTileMap outerId lxsize lysize =
  let unknownMap = PointArray.replicateA lxsize lysize unknownId
      borders = [ Point x y
                | x <- [0, lxsize - 1], y <- [1..lysize - 2] ]
                ++ [ Point x y
                   | x <- [0..lxsize - 1], y <- [0, lysize - 1] ]
      outerUpdate = zip borders $ repeat outerId
  in unknownMap PointArray.// outerUpdate

-- | Initial complete global game state.
defStateGlobal :: Dungeon -> AbsDepth -> FactionDict -> Kind.COps
               -> HighScore.ScoreDict -> Kind.Id ModeKind
               -> State
defStateGlobal _sdungeon _stotalDepth _sfactionD _scops _shigh _sgameModeId =
  State
    { _sactorD = EM.empty
    , _sitemD = EM.empty
    , _stime = timeZero
    , ..
    }

-- | Initial empty state.
emptyState :: Kind.COps -> State
emptyState _scops =
  State
    { _sdungeon = EM.empty
    , _stotalDepth = AbsDepth 0
    , _sactorD = EM.empty
    , _sitemD = EM.empty
    , _sfactionD = EM.empty
    , _stime = timeZero
    , _scops
    , _shigh = HighScore.empty
    , _sgameModeId = minBound  -- the initial value is unused
    }

-- | Local state created by removing secret information from global
-- state components.
localFromGlobal :: State -> State
localFromGlobal State{..} =
  State
    { _sdungeon =
      EM.map (\Level{..} ->
              unknownLevel _scops ldepth lxsize lysize ldesc lstair lclear
                           lescape lnight)
             _sdungeon
    , ..
    }

-- | Update dungeon data within state.
updateDungeon :: (Dungeon -> Dungeon) -> State -> State
updateDungeon f s = s {_sdungeon = f (_sdungeon s)}

-- | Update dungeon depth.
updateDepth :: (AbsDepth -> AbsDepth) -> State -> State
updateDepth f s = s {_stotalDepth = f (_stotalDepth s)}

-- | Update the actor dictionary.
updateActorD :: (ActorDict -> ActorDict) -> State -> State
updateActorD f s = s {_sactorD = f (_sactorD s)}

-- | Update the item dictionary.
updateItemD :: (ItemDict -> ItemDict) -> State -> State
updateItemD f s = s {_sitemD = f (_sitemD s)}

-- | Update faction data within state.
updateFactionD :: (FactionDict -> FactionDict) -> State -> State
updateFactionD f s = s {_sfactionD = f (_sfactionD s)}

-- | Update global time within state.
updateTime :: (Time -> Time) -> State -> State
updateTime f s = s {_stime = f (_stime s)}

-- | Update content data within state.
updateCOps :: (Kind.COps -> Kind.COps) -> State -> State
updateCOps f s = s {_scops = f (_scops s)}

sdungeon :: State -> Dungeon
sdungeon = _sdungeon

stotalDepth :: State -> AbsDepth
stotalDepth = _stotalDepth

sactorD :: State -> ActorDict
sactorD = _sactorD

sitemD :: State -> ItemDict
sitemD = _sitemD

sfactionD :: State -> FactionDict
sfactionD = _sfactionD

stime :: State -> Time
stime = _stime

scops :: State -> Kind.COps
scops = _scops

shigh :: State -> HighScore.ScoreDict
shigh = _shigh

sgameModeId :: State -> Kind.Id ModeKind
sgameModeId = _sgameModeId

instance Binary State where
  put State{..} = do
    put _sdungeon
    put _stotalDepth
    put _sactorD
    put _sitemD
    put _sfactionD
    put _stime
    put _shigh
    put _sgameModeId
  get = do
    _sdungeon <- get
    _stotalDepth <- get
    _sactorD <- get
    _sitemD <- get
    _sfactionD <- get
    _stime <- get
    _shigh <- get
    _sgameModeId <- get
    let _scops = assert `failure` "overwritten by recreated cops" `twith` ()
    return $! State{..}
