{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Screen frames and animations.
module Game.LambdaHack.Client.UI.Animation
  ( Animation, renderAnim
  , pushAndDelay, blinkColorActor, twirlSplash, blockHit, blockMiss
  , deathBody, actorX, swapPlaces, teleport, fadeout
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Bits
import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Client.UI.Frame
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random

-- | Animation is a list of frame modifications to play one by one,
-- where each modification if a map from positions to level map symbols.
newtype Animation = Animation [Overlay]
  deriving (Eq, Show)

-- | Render animations on top of a screen frame.
renderAnim :: FrameForall -> Animation -> Frames
renderAnim basicFrame (Animation anim) =
  let modifyFrame :: Overlay -> FrameForall
      modifyFrame am = overlayFrame am basicFrame
      modifyFrames :: (Overlay, Overlay) -> Maybe FrameForall
      modifyFrames (am, amPrevious) =
        if am == amPrevious then Nothing else Just $ modifyFrame am
  in Just basicFrame : map modifyFrames (zip anim ([] : anim))

blank :: Maybe AttrCharW32
blank = Nothing

cSym :: Color -> Char -> Maybe AttrCharW32
cSym color symbol = Just $ attrChar2ToW32 color symbol

mapPosToOffset :: (Point, AttrCharW32) -> (Int, [AttrCharW32])
mapPosToOffset (Point{..}, attr) =
  let lxsize = fst normalLevelBound + 1  -- TODO
  in ((py + 1) * lxsize + px, [attr])

mzipSingleton :: Point -> Maybe AttrCharW32 -> Overlay
mzipSingleton p1 mattr1 = map mapPosToOffset $
  let mzip (pos, mattr) = fmap (\attr -> (pos, attr)) mattr
  in catMaybes $ [mzip (p1, mattr1)]

mzipPairs :: (Point, Point) -> (Maybe AttrCharW32, Maybe AttrCharW32)
          -> Overlay
mzipPairs (p1, p2) (mattr1, mattr2) = map mapPosToOffset $
  let mzip (pos, mattr) = fmap (\attr -> (pos, attr)) mattr
  in catMaybes $ if p1 /= p2
                 then [mzip (p1, mattr1), mzip (p2, mattr2)]
                 else -- If actor affects himself, show only the effect,
                      -- not the action.
                      [mzip (p1, mattr1)]

pushAndDelay :: Animation
pushAndDelay = Animation [[]]

blinkColorActor :: Point -> Char -> Color -> Color -> Animation
blinkColorActor pos symbol fromCol toCol =
  Animation $ map (mzipSingleton pos)
  [ cSym fromCol symbol
  , cSym toCol symbol
  , cSym fromCol symbol
  , cSym toCol symbol
  , cSym fromCol symbol
  ]

-- | Attack animation. A part of it also reused for self-damage and healing.
twirlSplash :: (Point, Point) -> Color -> Color -> Animation
twirlSplash poss c1 c2 = Animation $ map (mzipPairs poss)
  [ (blank           , cSym BrCyan '\'')
  , (blank           , cSym BrYellow '\'')
  , (blank           , cSym BrYellow '^')
  , (cSym c1      '\\',cSym BrCyan '^')
  , (cSym c1      '|', cSym BrCyan '^')
  , (cSym c1      '%', blank)
  , (cSym c1      '/', blank)
  , (cSym c1      '-', blank)
  , (cSym c1      '\\',blank)
  , (cSym c2      '|', blank)
  , (cSym c2      '%', blank)
  , (cSym c2      '%', blank)
  , (cSym c2      '/', blank)
  ]

-- | Attack that hits through a block.
blockHit :: (Point, Point) -> Color -> Color -> Animation
blockHit poss c1 c2 = Animation $ map (mzipPairs poss)
  [ (blank           , cSym BrCyan '\'')
  , (blank           , cSym BrYellow '\'')
  , (blank           , cSym BrYellow '^')
  , (blank           , cSym BrCyan '^')
  , (cSym BrBlue  '{', cSym BrCyan '\'')
  , (cSym BrBlue  '{', cSym BrYellow '\'')
  , (cSym BrBlue  '{', cSym BrYellow '\'')
  , (cSym BrBlue  '}', blank)
  , (cSym BrBlue  '}', blank)
  , (cSym BrBlue  '}', blank)
  , (cSym c1      '\\',blank)
  , (cSym c1      '|', blank)
  , (cSym c1      '/', blank)
  , (cSym c1      '-', blank)
  , (cSym c2      '\\',blank)
  , (cSym c2      '|', blank)
  , (cSym c2      '/', blank)
  ]

-- | Attack that is blocked.
blockMiss :: (Point, Point) -> Animation
blockMiss poss = Animation $ map (mzipPairs poss)
  [ (blank           , cSym BrCyan '\'')
  , (blank           , cSym BrYellow '^')
  , (cSym BrBlue  '{', cSym BrYellow '\'')
  , (cSym BrBlue  '{', cSym BrCyan '\'')
  , (cSym BrBlue  '{', blank)
  , (cSym BrBlue  '}', blank)
  , (cSym BrBlue  '}', blank)
  , (cSym Blue    '}', blank)
  , (cSym Blue    '}', blank)
  ]

-- | Death animation for an organic body.
deathBody :: Point -> Animation
deathBody pos = Animation $ map (mzipSingleton pos)
  [ cSym BrRed '\\'
  , cSym BrRed '\\'
  , cSym BrRed '|'
  , cSym BrRed '|'
  , cSym BrRed '%'
  , cSym BrRed '%'
  , cSym BrRed '-'
  , cSym BrRed '-'
  , cSym BrRed '\\'
  , cSym BrRed '\\'
  , cSym BrRed '|'
  , cSym BrRed '|'
  , cSym BrRed '%'
  , cSym BrRed '%'
  , cSym BrRed '%'
  , cSym Red   '%'
  , cSym Red   '%'
  , cSym Red   '%'
  , cSym Red   '%'
  , cSym Red   ';'
  , cSym Red   ';'
  , cSym Red   ','
  ]

-- | Mark actor location animation.
actorX :: Point -> Animation
actorX pos = Animation $ map (mzipSingleton pos)
  [ cSym BrRed 'X'
  , cSym BrRed 'X'
  , blank
  , blank
  ]

-- | Actor teleport animation.
teleport :: (Point, Point) -> Animation
teleport poss = Animation $ map (mzipPairs poss)
  [ (cSym BrMagenta 'o', cSym Magenta   '.')
  , (cSym BrMagenta 'O', cSym Magenta   '.')
  , (cSym Magenta   'o', cSym Magenta   'o')
  , (cSym Magenta   '.', cSym BrMagenta 'O')
  , (cSym Magenta   '.', cSym BrMagenta 'o')
  , (cSym Magenta   '.', blank)
  , (blank             , blank)
  ]

-- | Swap-places animation, both hostile and friendly.
swapPlaces :: (Point, Point) -> Animation
swapPlaces poss = Animation $ map (mzipPairs poss)
  [ (cSym BrMagenta 'o', cSym Magenta   'o')
  , (cSym BrMagenta 'd', cSym Magenta   'p')
  , (cSym BrMagenta '.', cSym Magenta   'p')
  , (cSym Magenta   'p', cSym Magenta   '.')
  , (cSym Magenta   'p', cSym BrMagenta 'd')
  , (cSym Magenta   'p', cSym BrMagenta 'd')
  , (cSym Magenta   'o', blank)
  , (blank             , blank)
  ]

fadeout :: Bool -> Int -> X -> Y -> Rnd Animation
fadeout out step lxsize lysize = do
  let xbound = lxsize - 1
      ybound = lysize + 2
      edge = EM.fromDistinctAscList $ zip [1..] ".%&%;:,."
      fadeChar !r !n !x !y =
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
      rollFrame !n = do
        r <- random
        -- TODO: use a simplification of attrChar2ToW32; and elsewhere
        let fadeAttr !y !x = attrChar1ToW32 $ fadeChar r n x y
            fadeLine !y =
              let x1 :: Int
                  {-# INLINE x1 #-}
                  x1 = min xbound ((n - 2 * (ybound - y)))
                  x2 :: Int
                  {-# INLINE x2 #-}
                  x2 = max 0 (xbound - (n - 2 * y))
              in [ (y * lxsize, map (fadeAttr y) [0..x1])
                 , (y * lxsize + x2, map (fadeAttr y) [x2..xbound]) ]
        return $! concatMap fadeLine [0..ybound]
      fs | out = [3, 3 + step .. lxsize - 14]
         | otherwise = [lxsize - 14, lxsize - 14 - step .. 1]
  Animation <$> mapM rollFrame fs
