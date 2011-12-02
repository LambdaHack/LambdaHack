module Game.LambdaHack.Dungeon
  ( Dungeon, fromList, currentFirst, adjust, (!)
  ) where

import Data.Binary
import qualified Data.Map as M
import qualified Data.List as L

import Game.LambdaHack.Level
import Game.LambdaHack.WorldLoc

-- | The complete dungeon is a map from level names to levels.
-- We usually store all but the current level in this data structure.
newtype Dungeon = Dungeon{dungeonLevelMap :: M.Map LevelId Level}
  deriving Show

instance Binary Dungeon where
  put dng = put (M.assocs (dungeonLevelMap dng))
  get = fmap fromList get

-- | Create a dungeon from a list of levels.
fromList :: [(LevelId, Level)] -> Dungeon
fromList = Dungeon . M.fromList

-- | Association list corresponding to the dungeon.
-- Starts at the supplied level id (usually the current level)
-- to try to speed up the searches and keep the dungeon lazy.
currentFirst :: LevelId -> Dungeon -> [(LevelId, Level)]
currentFirst lid (Dungeon m) =
  (lid, m M.! lid)
  : L.filter ((/= lid) . fst) (M.assocs m)

adjust :: (Level -> Level) -> LevelId -> Dungeon -> Dungeon
adjust f lid (Dungeon m) = Dungeon (M.adjust f lid m)

(!) :: Dungeon -> LevelId -> Level
(!) (Dungeon m) lid = m M.! lid
