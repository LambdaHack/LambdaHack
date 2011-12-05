module Game.LambdaHack.Content.RoomKind
  ( RoomKind(..)
  ) where

import qualified Data.List as L

data RoomKind = RoomKind
  { rsymbol  :: Char
  , rname    :: String
  , rfreq    :: Int
  , rcover   :: Bool      -- ^ whether to tile the corner along the whole room
                          -- or only the NE and SW tiles along the borders
  , rfence   :: Bool      -- ^ whether to fence the room with solid border
  , rtopLeft :: [String]  -- ^ the top-left corner of the room
  }
  deriving Show
