module Game.LambdaHack.Content.RoomKind
  ( RoomKind(..), validRoom
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

validRoom :: RoomKind -> Bool
validRoom RoomKind{..} =
  let dxcorner = case rtopLeft of [] -> 0 ; l : _ -> L.length l
  in (rcover || dxcorner > 0) && L.all (== dxcorner) (L.map L.length rtopLeft)
