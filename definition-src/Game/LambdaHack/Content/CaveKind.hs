-- | The type of cave kinds. Every level in the game is an instantiated
-- cave kind.
module Game.LambdaHack.Content.CaveKind
  ( pattern DEFAULT_RANDOM
  , CaveKind(..), InitSleep(..), makeData
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , validateSingle, validateAll, mandatoryGroups
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Text as T

import           Game.LambdaHack.Content.ItemKind (ItemKind)
import           Game.LambdaHack.Content.PlaceKind (PlaceKind)
import qualified Game.LambdaHack.Content.RuleKind as RK
import           Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Core.Dice as Dice
import           Game.LambdaHack.Core.Random
import           Game.LambdaHack.Definition.ContentData
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.DefsInternal

-- | Parameters for the generation of dungeon levels.
-- Warning: for efficiency, avoid embedded items in any of the common tiles.
data CaveKind = CaveKind
  { cname         :: Text             -- ^ short description
  , cfreq         :: Freqs CaveKind   -- ^ frequency within groups
  , cXminSize     :: X                -- ^ minimal X size of the whole cave
  , cYminSize     :: Y                -- ^ minimal Y size of the whole cave
  , ccellSize     :: Dice.DiceXY      -- ^ size of a map cell holding a place
  , cminPlaceSize :: Dice.DiceXY      -- ^ minimal size of places; for merging
  , cmaxPlaceSize :: Dice.DiceXY      -- ^ maximal size of places; for growing
  , cdarkOdds     :: Dice.Dice        -- ^ the odds a place is dark
                                      --   (level-scaled dice roll > 50)
  , cnightOdds    :: Dice.Dice        -- ^ the odds the cave is dark
                                      --   (level-scaled dice roll > 50)
  , cauxConnects  :: Rational         -- ^ a proportion of extra connections
  , cmaxVoid      :: Rational
      -- ^ at most this proportion of rooms may be void
  , cdoorChance   :: Chance           -- ^ the chance of a door in an opening
  , copenChance   :: Chance           -- ^ if there's a door, is it open?
  , chidden       :: Int              -- ^ if not open, hidden one in n times
  , cactorCoeff   :: Int              -- ^ the lower, the more monsters spawn
  , cactorFreq    :: Freqs ItemKind   -- ^ actor groups to consider
  , citemNum      :: Dice.Dice        -- ^ number of initial items in the cave
  , citemFreq     :: Freqs ItemKind   -- ^ item groups to consider;
      -- note that the groups are flattened; e.g., if an item is moved
      -- to another included group with the same weight, the outcome
      -- doesn't change
  , cplaceFreq    :: Freqs PlaceKind  -- ^ place groups to consider
  , cpassable     :: Bool
      -- ^ are passable default tiles permitted
  , clabyrinth    :: Bool                -- ^ waste of time for AI to explore
  , cdefTile      :: GroupName TileKind  -- ^ the default cave tile
  , cdarkCorTile  :: GroupName TileKind  -- ^ the dark cave corridor tile
  , clitCorTile   :: GroupName TileKind  -- ^ the lit cave corridor tile
  , cwallTile     :: GroupName TileKind  -- ^ the tile used for @FWall@ fence
  , ccornerTile   :: GroupName TileKind  -- ^ tile used for the fence corners
  , cfenceTileN   :: GroupName TileKind  -- ^ the outer fence N wall
  , cfenceTileE   :: GroupName TileKind  -- ^ the outer fence E wall
  , cfenceTileS   :: GroupName TileKind  -- ^ the outer fence S wall
  , cfenceTileW   :: GroupName TileKind  -- ^ the outer fence W wall
  , cfenceApart   :: Bool                -- ^ are places touching fence banned
  , cminStairDist :: Int                 -- ^ minimal distance between stairs
  , cmaxStairsNum :: Dice.Dice           -- ^ maximum number of stairs
  , cescapeFreq   :: Freqs PlaceKind     -- ^ escape groups, if any
  , cstairFreq    :: Freqs PlaceKind     -- ^ place groups for created stairs
  , cstairAllowed :: Freqs PlaceKind     -- ^ extra groups for inherited
  , cskip         :: [Int]  -- ^ which faction starting positions to skip
  , cinitSleep    :: InitSleep           -- ^ whether actors spawn sleeping
  , cdesc         :: Text   -- ^ full cave description
  }
  deriving Show  -- No Eq and Ord to make extending logically sound

data InitSleep = InitSleepAlways | InitSleepPermitted | InitSleepBanned
  deriving (Show, Eq)

-- | Catch caves with not enough space for all the places. Check the size
-- of the cave descriptions to make sure they fit on screen. Etc.
validateSingle :: RK.RuleContent -> CaveKind -> [Text]
validateSingle corule CaveKind{..} =
  let (minCellSizeX, minCellSizeY) = Dice.infDiceXY ccellSize
      (maxCellSizeX, maxCellSizeY) = Dice.supDiceXY ccellSize
      (minMinSizeX, minMinSizeY) = Dice.infDiceXY cminPlaceSize
      (maxMinSizeX, maxMinSizeY) = Dice.supDiceXY cminPlaceSize
      (minMaxSizeX, minMaxSizeY) = Dice.infDiceXY cmaxPlaceSize
  in [ "cname longer than 25" | T.length cname > 25 ]
     ++ [ "cXminSize > RK.rWidthMax" | cXminSize > RK.rWidthMax corule ]
     ++ [ "cYminSize > RK.rHeightMax" | cYminSize > RK.rHeightMax corule ]
     ++ [ "cXminSize < 8" | cXminSize < 8 ]
     ++ [ "cYminSize < 8" | cYminSize < 8 ]  -- see @focusArea@
     ++ [ "cXminSize - 2 < maxCellSizeX" | cXminSize - 2 < maxCellSizeX ]
     ++ [ "cYminSize - 2 < maxCellSizeY" | cYminSize - 2 < maxCellSizeY ]
     ++ [ "minCellSizeX < 2" | minCellSizeX < 2 ]
     ++ [ "minCellSizeY < 2" | minCellSizeY < 2 ]
     ++ [ "minCellSizeX < 4 and stairs"
        | minCellSizeX < 4 && not (null cstairFreq) ]
     ++ [ "minCellSizeY < 4 and stairs"
        | minCellSizeY < 4 && not (null cstairFreq) ]
     -- The following four are heuristics, so not too restrictive:
     ++ [ "minCellSizeX < 6 && non-trivial stairs"
        | minCellSizeX < 6 && not (length cstairFreq <= 1 && null cescapeFreq) ]
     ++ [ "minCellSizeY < 4 && non-trivial stairs"
        | minCellSizeY < 4 && not (length cstairFreq <= 1 && null cescapeFreq) ]
     ++ [ "minMinSizeX < 5 && non-trivial stairs"
        | minMinSizeX < 5 && not (length cstairFreq <= 1 && null cescapeFreq) ]
     ++ [ "minMinSizeY < 3 && non-trivial stairs"
        | minMinSizeY < 3 && not (length cstairFreq <= 1 && null cescapeFreq) ]
     ++ [ "minMinSizeX < 1" | minMinSizeX < 1 ]
     ++ [ "minMinSizeY < 1" | minMinSizeY < 1 ]
     ++ [ "minMaxSizeX < maxMinSizeX" | minMaxSizeX < maxMinSizeX ]
     ++ [ "minMaxSizeY < maxMinSizeY" | minMaxSizeY < maxMinSizeY ]
     ++ [ "chidden < 0" | chidden < 0 ]
     ++ [ "cactorCoeff < 0" | cactorCoeff < 0 ]
     ++ [ "citemNum < 0" | Dice.infDice citemNum < 0 ]
     ++ [ "cmaxStairsNum < 0" | Dice.infDice cmaxStairsNum < 0 ]
     ++ [ "stairs suggested, but not defined"
        | Dice.supDice cmaxStairsNum > 0 && null cstairFreq ]

-- | Validate all cave kinds.
-- Note that names don't have to be unique: we can have several variants
-- of a cave with a given name.
validateAll :: [CaveKind] -> ContentData CaveKind -> [Text]
validateAll _ _ = []  -- so far, always valid

-- * Mandatory item groups

mandatoryGroups :: [GroupName CaveKind]
mandatoryGroups =
       [DEFAULT_RANDOM]

pattern DEFAULT_RANDOM :: GroupName CaveKind

pattern DEFAULT_RANDOM = GroupName "default random"

makeData :: RK.RuleContent
         -> [CaveKind] -> [GroupName CaveKind] -> [GroupName CaveKind]
         -> ContentData CaveKind
makeData corule content groupNamesSingleton groupNames =
  makeContentData "CaveKind" cname cfreq (validateSingle corule) validateAll
                  content
                  groupNamesSingleton
                  (mandatoryGroups ++ groupNames)
