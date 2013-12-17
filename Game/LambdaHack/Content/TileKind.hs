-- | The type of kinds of terrain tiles.
module Game.LambdaHack.Content.TileKind
  ( TileKind(..), tvalidate, actionFeatures
  ) where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)

import Control.Exception.Assert.Sugar
import Game.LambdaHack.Common.Color
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Misc

-- | The type of kinds of terrain tiles. See @Tile.hs@ for explanation
-- of the absence of a corresponding type @Tile@ that would hold
-- particular concrete tiles in the dungeon.
data TileKind = TileKind
  { tsymbol  :: !Char         -- ^ map symbol
  , tname    :: !Text         -- ^ short description
  , tfreq    :: !Freqs        -- ^ frequency within groups
  , tcolor   :: !Color        -- ^ map color
  , tcolor2  :: !Color        -- ^ map color when not in FOV
  , tfeature :: ![F.Feature]  -- ^ properties
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound, see #53

-- TODO: Make sure only solid tiles have Suspect.
-- TODO: check that all posible solid place fences have hidden counterparts.
-- TODO: verify that OpenTo, CloseTo and ChangeTo are assigned as specified.
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
  let listFov f = L.map (\kt -> ( ( tsymbol kt
                                  , F.Suspect `elem` tfeature kt
                                  , f kt
                                  )
                                , [kt] )) lt
      mapFov :: (TileKind -> Color) -> M.Map (Char, Bool, Color) [TileKind]
      mapFov f = M.fromListWith (++) $ listFov f
      namesUnequal [] = assert `failure` "no TileKind content" `twith` lt
      namesUnequal (hd : tl) =
        -- Catch if at least one is different.
        L.any (/= tname hd) (L.map tname tl)
        || L.any (/= actionFeatures hd) (L.map actionFeatures tl)
      confusions f = L.filter namesUnequal $ M.elems $ mapFov f
  in case confusions tcolor ++ confusions tcolor2 of
    [] -> []
    l : _ -> l

actionFeatures :: TileKind -> S.Set F.Feature
actionFeatures t =
  let f feat = case feat of
        F.Cause{} -> Just feat
        F.OpenTo{} -> Just $ F.OpenTo ""
        F.CloseTo{} -> Just $ F.CloseTo ""
        F.ChangeTo{} -> Just $ F.ChangeTo ""
        F.Walkable -> Just feat
        F.Clear -> Just feat
        F.Suspect -> Just feat
        F.Aura{} -> Just feat
        F.Impenetrable -> Just feat
        F.Path -> Just feat  -- doesn't affect tile behaviour, but important
        F.HideAs{} -> Nothing
        F.RevealAs{} -> Nothing
        F.Dark -> Nothing  -- not important any longer, after FOV computed
        F.CanItem -> Nothing
        F.CanActor -> Nothing
        F.Exit -> Nothing
  in S.fromList $ mapMaybe f $ tfeature t
