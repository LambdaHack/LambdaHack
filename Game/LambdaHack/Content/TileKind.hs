module Game.LambdaHack.Content.TileKind
  ( TileKind(..), tvalidate
  ) where

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
tvalidate _ = [] -- TODO
