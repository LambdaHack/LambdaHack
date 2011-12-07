module Game.LambdaHack.Content.TileKind
  ( TileKind(..), tvalidate
  ) where

import qualified Data.List as L
import qualified Data.Map as M

import Game.LambdaHack.Color
import Game.LambdaHack.Feature

data TileKind = TileKind
  { tsymbol  :: !Char       -- ^ map symbol
  , tname    :: !String     -- ^ name
  , tfreq    :: !Int        -- ^ created that often (within a group?)
  , tcolor   :: !Color      -- ^ map color
  , tcolor2  :: !Color      -- ^ map color when not in FOV
  , tfeature :: ![Feature]  -- ^ properties
  }
  deriving Show

tvalidate :: [TileKind] -> [TileKind]
tvalidate lt =
  -- If it looks the same on the map, the description should be the same, too.
  -- Otherwise, the player has to inspect manually all the tiles of that kind
  -- to see if any is special.
  let listFov f = L.map (\ kt -> ((tsymbol kt, f kt), [kt])) lt
      mapFov :: (TileKind -> Color) -> M.Map (Char, Color) [TileKind]
      mapFov f = M.fromListWith (++) $ listFov f
      namesUnequal l = let name = tname (L.head l)
                       in L.any (\ kt -> tname kt /= name) l
      confusions f = L.filter namesUnequal $ M.elems $ mapFov f
  in case confusions tcolor ++ confusions tcolor2 of
    [] -> []
    l : _ -> l
