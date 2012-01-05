module Game.LambdaHack.Dungeon
  ( Dungeon, fromList, currentFirst, adjust, (!), lookup, depth
  ) where

import Prelude hiding (lookup)
import Data.Binary
import qualified Data.Map as M
import qualified Data.List as L

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Level
import Game.LambdaHack.WorldLoc

-- | The complete dungeon is a map from level names to levels.
-- We usually store all but the current level in this data structure.
data Dungeon = Dungeon
  { dungeonLevelMap :: M.Map LevelId Level
  , dungeonDepth :: Int  -- can be different than the number of levels
  }
  deriving Show

instance Binary Dungeon where
  put Dungeon{..} = do
    put (M.toAscList dungeonLevelMap)
    put dungeonDepth
  get = do
    lvls <- get
    let dungeonLevelMap = M.fromDistinctAscList lvls
    dungeonDepth <- get
    return Dungeon{..}

-- | Create a dungeon from a list of levels and maximum depth (danger).
fromList :: [(LevelId, Level)] -> Int -> Dungeon
fromList lvls d = assert (d <= L.length lvls `blame` (d, L.length lvls)) $
  Dungeon (M.fromList lvls) d

-- | Association list corresponding to the dungeon.
-- Starts at the supplied level id (usually the current level)
-- to try to speed up the searches and keep the dungeon lazy.
currentFirst :: LevelId -> Dungeon -> [(LevelId, Level)]
currentFirst lid (Dungeon m _) =
  (lid, m M.! lid)
  : L.filter ((/= lid) . fst) (M.assocs m)

adjust :: (Level -> Level) -> LevelId -> Dungeon -> Dungeon
adjust f lid (Dungeon m d) = Dungeon (M.adjust f lid m) d

(!) :: Dungeon -> LevelId -> Level
(!) (Dungeon m _) lid = m M.! lid

lookup :: LevelId -> Dungeon -> Maybe Level
lookup lid (Dungeon m _) = M.lookup lid m

depth :: Dungeon -> Int
depth = dungeonDepth
