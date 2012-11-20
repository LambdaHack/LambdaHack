-- | Screen frames and animations.
module Game.LambdaHack.Animation
  ( Attr(..), defaultAttr, AttrChar(..)
  , SingleFrame(..), Animation, rederAnim, emptyAnimation
  , twirlSplash, blockHit, blockMiss, swapPlaces
  ) where

import qualified Data.IntMap as IM
import Data.Maybe
import qualified Data.List as L

import Game.LambdaHack.PointXY
import Game.LambdaHack.Point
import Game.LambdaHack.Color

-- | The data sufficent to draw a single game screen frame.
data SingleFrame = SingleFrame
  { sfLevel  :: ![[AttrChar]]  -- ^ content of the screen, line by line
  , sfTop    :: String         -- ^ an extra line to show at the top
  , sfBottom :: String         -- ^ an extra line to show at the bottom
  }
  deriving Eq

-- | Animation is a list of frame modifications to play one by one,
-- where each modification if a map from locations to level map symbols.
newtype Animation = Animation [IM.IntMap AttrChar]

-- | Render animations on top of a screen frame.
rederAnim :: X -> Y -> SingleFrame -> Animation
          -> [Maybe SingleFrame]
rederAnim lxsize lysize basicFrame (Animation anim) =
  let modifyFrame SingleFrame{sfLevel = levelOld, ..} am =
        let fLine y lineOld =
              let f l (x, acOld) =
                    let loc = toPoint lxsize (PointXY (x, y))
                        !ac = fromMaybe acOld $ IM.lookup loc am
                    in ac : l
              in L.foldl' f [] (zip [lxsize-1,lxsize-2..0] (reverse lineOld))
            sfLevel =  -- Fully evaluated.
              let f l (y, lineOld) = let !line = fLine y lineOld in line : l
              in L.foldl' f [] (zip [lysize-1,lysize-2..0] (reverse levelOld))
        in Just SingleFrame{..}
  in map (modifyFrame basicFrame) anim

emptyAnimation :: Animation
emptyAnimation = Animation []

-- | Attack animation. A part of it also reused for self-damage and healing.
twirlSplash :: [Point] -> Color -> Color -> Animation
twirlSplash locs c1 c2 = Animation $ map (IM.fromList . zip locs)
  [ [AttrChar (Attr BrWhite defBG) '*']
  , [ AttrChar (Attr c1 defBG) '/'
    , AttrChar (Attr BrWhite defBG) '^' ]
  , [ AttrChar (Attr c1 defBG) '-'
    , AttrChar (Attr BrWhite defBG) '^' ]
  , [ AttrChar (Attr c1 defBG) '\\'
    , AttrChar (Attr BrWhite defBG) '^' ]
  , [ AttrChar (Attr c1 defBG) '|'
    , AttrChar (Attr BrWhite defBG) '^' ]
  , [AttrChar (Attr c2 defBG) '/']
  , [AttrChar (Attr c2 defBG) '%']
  , [ AttrChar (Attr c2 defBG) '%'
    , AttrChar (Attr BrWhite defBG) '^' ]
  , []
  ]

-- | Attack that hits through a block.
blockHit :: [Point] -> Color -> Color -> Animation
blockHit locs c1 c2 = Animation $ map (IM.fromList . zip locs)
  [ [AttrChar (Attr BrWhite defBG) '*']
  , [ AttrChar (Attr BrBlue defBG) '{'
    , AttrChar (Attr BrWhite defBG) '^' ]
  , [AttrChar (Attr BrBlue defBG) '{']
  , [AttrChar (Attr BrBlue defBG) '}']
  , [ AttrChar (Attr c1 defBG) '}'
    , AttrChar (Attr BrWhite defBG) '^' ]
  , [ AttrChar (Attr c2 defBG) '/'
    , AttrChar (Attr BrWhite defBG) '^' ]
  , [ AttrChar (Attr c2 defBG) '%'
    , AttrChar (Attr BrWhite defBG) '^' ]
  , [ AttrChar (Attr c2 defBG) '%'
    , AttrChar (Attr BrWhite defBG) '^' ]
  , []
  ]

-- | Attack that is blocked.
blockMiss :: [Point] -> Animation
blockMiss locs = Animation $ map (IM.fromList . zip locs)
  [ [AttrChar (Attr BrWhite defBG) '*']
  , [ AttrChar (Attr BrBlue defBG) '{'
    , AttrChar (Attr BrWhite defBG) '^' ]
  , [AttrChar (Attr BrBlue defBG) '}']
  , []
  ]

-- | Swap-places animation, both hostile and friendly.
swapPlaces :: [Point] -> Animation
swapPlaces locs = Animation $ map (IM.fromList . zip locs)
  [ [AttrChar (Attr BrMagenta defBG) '.',
     AttrChar (Attr Magenta defBG) 'o']
  , [AttrChar (Attr BrMagenta defBG) 'd',
     AttrChar (Attr Magenta defBG) 'p']
  , [AttrChar (Attr Magenta defBG) 'p',
     AttrChar (Attr BrMagenta defBG) 'd']
  , [AttrChar (Attr Magenta defBG) 'o']
  , []
  ]
