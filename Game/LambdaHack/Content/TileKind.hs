module Game.LambdaHack.Content.TileKind
  ( TileKind(..), SecretStrength(..), tvalidate
  ) where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Binary
import Game.LambdaHack.Geometry

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

newtype SecretStrength = SecretStrength{secretStrength :: Time}
  deriving (Show, Eq, Ord)
instance Binary SecretStrength where
  put = put . secretStrength
  get = fmap SecretStrength get

-- | If tiles look the same on the map, the description should be the same, too.
-- Otherwise, the player has to inspect manually all the tiles of that kind
-- to see if any is special. This is a part of a stronger
-- but less precise property that tiles that look the same can't be
-- distinguished by player actions (but may behave differently
-- wrt dungeon generation, AI preferences, etc.).
-- TODO: check that all solid room boundaries have hidden counterparts.
tvalidate :: [TileKind] -> [TileKind]
tvalidate lt =
  let listFov f = L.map (\ kt -> ((tsymbol kt, f kt), [kt])) lt
      mapFov :: (TileKind -> Color) -> M.Map (Char, Color) [TileKind]
      mapFov f = M.fromListWith (++) $ listFov f
      namesUnequal l = let name = tname (L.head l)
                       in L.any (\ kt -> tname kt /= name) l
      confusions f = L.filter namesUnequal $ M.elems $ mapFov f
  in case confusions tcolor ++ confusions tcolor2 of
    [] -> []
    l : _ -> l
