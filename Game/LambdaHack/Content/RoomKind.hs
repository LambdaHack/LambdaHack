module Game.LambdaHack.Content.RoomKind
  ( RoomKind(..)
  ) where

import Game.LambdaHack.Geometry

data RoomKind = RoomKind
  { rsymbol  :: Char
  , rname    :: String
  , rfreq    :: Int
    -- The section of the pattern for tiling the horizontal border.
  , rborderH :: (X, Y)
    -- The section of the pattern for tiling the vertical border.
  , rborderV :: (X, Y)
    -- The top-left corner of the room. After the borders are created
    -- by tiling their respective patterns, The rectangle obtained by
    -- removing the first few rows and columns is tiled to get the interior.
    -- The remaining one or two rectangles are ignored. Note: rooms rotated
    -- by 90 degrees are automatically generated from these descriptions.
  , rtopLeft :: [String]
  }
  deriving Show
