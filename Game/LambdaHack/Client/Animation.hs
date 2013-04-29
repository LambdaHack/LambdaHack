{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Screen frames and animations.
module Game.LambdaHack.Client.Animation
  ( Attr(..), defAttr, AttrChar(..)
  , SingleFrame(..), Animation, Frames, renderAnim, restrictAnim
  , twirlSplash, blockHit, blockMiss, deathBody, swapPlaces, fadeout
  ) where

import Control.Arrow ((***))
import Data.Binary
import Data.Bits
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.List as L
import Data.Maybe
import Data.Monoid
import Data.Text (Text)

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.PointXY
import Game.LambdaHack.Common.Random

-- | The data sufficent to draw a single game screen frame.
data SingleFrame = SingleFrame
  { sfLevel  :: [[AttrChar]]  -- ^ content of the screen, line by line
  , sfTop    :: Text          -- ^ an extra line to show at the top
  , sfBottom :: Text          -- ^ an extra line to show at the bottom
  }
  deriving (Eq, Show)

instance Binary SingleFrame where
  put SingleFrame{..} = do
    put sfLevel
    put sfTop
    put sfBottom
  get = do
    sfLevel <- get
    sfTop <- get
    sfBottom <- get
    return SingleFrame{..}

-- | Animation is a list of frame modifications to play one by one,
-- where each modification if a map from positions to level map symbols.
newtype Animation = Animation [EM.EnumMap Point AttrChar]
  deriving (Eq, Show, Monoid)

-- | Sequences of screen frames, including delays.
type Frames = [Maybe SingleFrame]

-- | Render animations on top of a screen frame.
renderAnim :: X -> Y -> SingleFrame -> Animation
          -> Frames
renderAnim lxsize lysize basicFrame (Animation anim) =
  let modifyFrame SingleFrame{sfLevel = levelOld, ..} am =
        let fLine y lineOld =
              let f l (x, acOld) =
                    let loc = toPoint lxsize (PointXY (x, y))
                        !ac = fromMaybe acOld $ EM.lookup loc am
                    in ac : l
              in L.foldl' f [] (zip [lxsize-1,lxsize-2..0] (reverse lineOld))
            sfLevel =  -- Fully evaluated.
              let f l (y, lineOld) = let !line = fLine y lineOld in line : l
              in L.foldl' f [] (zip [lysize-1,lysize-2..0] (reverse levelOld))
        in Just SingleFrame{..}
  in map (modifyFrame basicFrame) anim

blank :: Maybe AttrChar
blank = Nothing

coloredSymbol :: Color -> Char -> Maybe AttrChar
coloredSymbol color symbol = Just $ AttrChar (Attr color defBG) symbol

mzipPairs :: (Point, Point) -> (Maybe AttrChar, Maybe AttrChar)
          -> [(Point, AttrChar)]
mzipPairs (p1, p2) (mattr1, mattr2) =
  let mzip (pos, mattr) = fmap (\x -> (pos, x)) mattr
  in if p1 /= p2
     then catMaybes [mzip (p1, mattr1), mzip (p2, mattr2)]
     else -- If actor affects himself, show only the effect, not the action.
          catMaybes [mzip (p1, mattr1)]

restrictAnim :: ES.EnumSet Point -> Animation -> Animation
restrictAnim vis (Animation as) =
  let f imap =
        let common = EM.intersection imap $ EM.fromSet (const ()) vis
          in if EM.null common then Nothing else Just common
  in Animation $ catMaybes $ map f as

-- | Attack animation. A part of it also reused for self-damage and healing.
twirlSplash :: (Point, Point) -> Color -> Color -> Animation
twirlSplash poss c1 c2 = Animation $ map (EM.fromList . mzipPairs poss)
  [ (coloredSymbol BrWhite '*', blank)
  , (coloredSymbol c1      '/', coloredSymbol BrCyan '^')
  , (coloredSymbol c1      '-', blank)
  , (coloredSymbol c1      '\\',blank)
  , (coloredSymbol c1      '|', blank)
  , (coloredSymbol c2      '/', blank)
  , (coloredSymbol c2      '%', coloredSymbol BrCyan '^')
  , (coloredSymbol c2      '%', blank)
  , (blank                    , blank)
  ]

-- | Attack that hits through a block.
blockHit :: (Point, Point) -> Color -> Color -> Animation
blockHit poss c1 c2 = Animation $ map (EM.fromList . mzipPairs poss)
  [ (coloredSymbol BrWhite '*', blank)
  , (coloredSymbol BrBlue  '{', coloredSymbol BrCyan '^')
  , (coloredSymbol BrBlue  '{', blank)
  , (coloredSymbol c1      '}', blank)
  , (coloredSymbol c1      '}', coloredSymbol BrCyan '^')
  , (coloredSymbol c2      '/', blank)
  , (coloredSymbol c2      '%', blank)
  , (coloredSymbol c2      '%', blank)
  , (blank                    , blank)
  ]

-- | Attack that is blocked.
blockMiss :: (Point, Point) -> Animation
blockMiss poss = Animation $ map (EM.fromList . mzipPairs poss)
  [ (coloredSymbol BrWhite '*', blank)
  , (coloredSymbol BrBlue  '{', coloredSymbol BrCyan '^')
  , (coloredSymbol BrBlue  '}', blank)
  , (coloredSymbol BrBlue  '}', blank)
  , (blank                    , blank)
  ]

-- | Death animation for an organic body.
deathBody :: Point -> Animation
deathBody loc = Animation $ map (maybe EM.empty (EM.singleton loc))
  [ coloredSymbol BrRed '\\'
  , coloredSymbol BrRed '\\'
  , coloredSymbol BrRed '|'
  , coloredSymbol BrRed '|'
  , coloredSymbol BrRed '%'
  , coloredSymbol BrRed '%'
  , coloredSymbol Red   '%'
  , coloredSymbol Red   '%'
  , coloredSymbol Red   ';'
  , coloredSymbol Red   ';'
  , coloredSymbol Red   ','
  ]

-- | Swap-places animation, both hostile and friendly.
swapPlaces :: (Point, Point) -> Animation
swapPlaces poss = Animation $ map (EM.fromList . mzipPairs poss)
  [ (coloredSymbol BrMagenta '.', coloredSymbol Magenta   'o')
  , (coloredSymbol BrMagenta 'd', coloredSymbol Magenta   'p')
  , (coloredSymbol Magenta   'p', coloredSymbol BrMagenta 'd')
  , (coloredSymbol Magenta   'o', blank)
  ]

fadeout :: Bool -> Bool -> X -> Y -> Rnd Animation
fadeout out topRight lxsize lysize = do
  let xbound = lxsize - 1
      ybound = lysize - 1
      edge = EM.fromDistinctAscList $ zip [1..] ".%&%;:,."
      fadeChar r n x y =
        let d = x - 2 * y
            ndy = n - d - 2 * ybound
            ndx = n + d - xbound - 1  -- @-1@ for asymmetry
            mnx = if ndy > 0 && ndx > 0
                  then min ndy ndx
                  else max ndy ndx
            v3 = (r `xor` (x * y)) `mod` 3
            k | mnx < 3 || mnx > 10 = mnx
              | (min x (xbound - x - y) + n + v3) `mod` 15 < 11
                && mnx > 6 = mnx - v3
              | (x + 3 * y + v3) `mod` 30 < 19 = mnx + 1
              | otherwise = mnx
        in EM.findWithDefault ' ' k edge
      rollFrame n = do
        r <- random
        let l = [ ( PointXY (if topRight then x else xbound - x, y)
                  , fadeChar r n x y )
                | x <- [0..xbound]
                , y <- [max 0 (ybound - (n - x) `div` 2)..ybound]
                    ++ [0..min ybound ((n - xbound + x) `div` 2)]
                ]
        return $ EM.fromList $ map (toPoint lxsize *** AttrChar defAttr) l
      fs = (if out then id else reverse) [3..3 * lxsize `divUp` 4 + 2]
  as <- mapM rollFrame fs
  return $ Animation as
