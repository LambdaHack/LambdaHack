-- | The type of kinds of terrain tiles.
module Game.LambdaHack.Content.TileKind
  ( TileKind(..), validateTileKind, actionFeatures
  ) where

import Control.Exception.Assert.Sugar
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)

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
-- If tiles look the same on the map, the description and the substantial
-- features should be the same, too. Otherwise, the player has to inspect
-- manually all the tiles of that kind, or even experiment with them,
-- to see if any is special. This would be tedious. Note that iiles may freely
-- differ wrt dungeon generation, AI preferences, etc.
validateTileKind :: [TileKind] -> [TileKind]
validateTileKind lt =
  let listFov f = map (\kt -> ( ( tsymbol kt
                                  , F.Suspect `elem` tfeature kt
                                  , f kt
                                  )
                                , [kt] )) lt
      mapFov :: (TileKind -> Color) -> M.Map (Char, Bool, Color) [TileKind]
      mapFov f = M.fromListWith (++) $ listFov f
      namesUnequal [] = assert `failure` "no TileKind content" `twith` lt
      namesUnequal (hd : tl) =
        -- Catch if at least one is different.
        any (/= tname hd) (map tname tl)
        || any (/= actionFeatures True hd) (map (actionFeatures True) tl)
      confusions f = filter namesUnequal $ M.elems $ mapFov f
  in case confusions tcolor ++ confusions tcolor2 of
    [] -> []
    l : _ -> l

-- | Features of tiles that differentiate them substantially from one another.
-- By tile contents validation condition, this means the player
-- can tell such tile apart, and only looking at the map, not tile name.
-- So if running uses this function, it won't stop at places that the player
-- can't himself tell from other places, and so running does not confer
-- any advantages, except UI convenience.
actionFeatures :: Bool -> TileKind -> S.Set F.Feature
actionFeatures markSuspect t =
  let f feat = case feat of
        F.Cause{} -> Just feat
        F.OpenTo{} -> Just $ F.OpenTo ""  -- if needed, remove prefix/suffix
        F.CloseTo{} -> Just $ F.CloseTo ""
        F.ChangeTo{} -> Just $ F.ChangeTo ""
        F.Walkable -> Just feat
        F.Clear -> Just feat
        F.Suspect -> if markSuspect then Just feat else Nothing
        F.Aura{} -> Just feat
        F.Impenetrable -> Just feat
        F.Trail -> Just feat  -- doesn't affect tile behaviour, but important
        F.HideAs{} -> Nothing
        F.RevealAs{} -> Nothing
        F.Dark -> Nothing  -- not important any longer, after FOV computed
        F.OftenItem -> Nothing
        F.OftenActor -> Nothing
        F.NoItem -> Nothing
        F.NoActor -> Nothing
  in S.fromList $ mapMaybe f $ tfeature t
