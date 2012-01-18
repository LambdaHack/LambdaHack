-- | The type of kinds of terrain tiles.
module Game.LambdaHack.Content.TileKind
  ( TileKind(..), tvalidate
  ) where

import qualified Data.List as L
import qualified Data.Map as M

import Game.LambdaHack.Color
import Game.LambdaHack.Feature
import Game.LambdaHack.Content.Content

-- | The type of kinds of terrain tiles. See @Tile.hs@ about why there is no
-- corresponding type @Tile@, of particular concrete tiles in the dungeon,
-- unlike for the other content.
data TileKind = TileKind
  { tsymbol  :: !Char       -- ^ map symbol
  , tname    :: !String     -- ^ short description
  , tfreq    :: !Freqs      -- ^ frequency within groups
  , tcolor   :: !Color      -- ^ map color
  , tcolor2  :: !Color      -- ^ map color when not in FOV
  , tfeature :: ![Feature]  -- ^ properties
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound, see #53

-- TODO: check that all posible solid place fences have hidden counterparts.
-- | Filter a list of kinds, passing through only the incorrect ones, if any.
--
-- If tiles look the same on the map, the description should be the same, too.
-- Otherwise, the player has to inspect manually all the tiles of that kind
-- to see if any is special. This is a part of a stronger
-- but less precise property that tiles that look the same can't be
-- distinguished by player actions (but may behave differently
-- wrt dungeon generation, AI preferences, etc.).
tvalidate :: [TileKind] -> [TileKind]
tvalidate lt =
  let listFov f = L.map (\ kt -> ((tsymbol kt, f kt), [kt])) lt
      mapFov :: (TileKind -> Color) -> M.Map (Char, Color) [TileKind]
      mapFov f = M.fromListWith (++) $ listFov f
      namesUnequal l = let name = tname (L.head l)
                       in L.any (/= name) (L.map tname l)
      confusions f = L.filter namesUnequal $ M.elems $ mapFov f
  in case confusions tcolor ++ confusions tcolor2 of
    [] -> []
    l : _ -> l
