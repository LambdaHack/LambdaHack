{-# LANGUAGE TupleSections #-}
-- | Screen frames and animations.
module Game.LambdaHack.Client.UI.Animation
  ( Animation, renderAnim
  , pushAndDelay, twirlSplash, blockHit, blockMiss, subtleHit
  , deathBody, shortDeathBody, actorX, teleport, vanish, swapPlaces, fadeout
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , blank, cSym, mapPosToOffset, mzipSingleton, mzipPairs
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Bits
import qualified Data.EnumMap.Strict as EM

import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.Frame
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Core.Random
import           Game.LambdaHack.Definition.Color

-- | Animation is a list of frame modifications to play one by one,
-- where each modification if a map from positions to level map symbols.
newtype Animation = Animation [OverlaySpace]
  deriving (Eq, Show)

-- | Render animations on top of a screen frame.
--
-- Located in this module to keep @Animation@ abstract.
renderAnim :: Int -> PreFrame -> Animation -> PreFrames
renderAnim width basicFrame (Animation anim) =
  let modifyFrame :: OverlaySpace -> PreFrame
      -- Overlay not truncated, because guaranteed within bounds.
      modifyFrame am = overlayFrame width am basicFrame
      modifyFrames :: (OverlaySpace, OverlaySpace) -> Maybe PreFrame
      modifyFrames (am, amPrevious) =
        if am == amPrevious then Nothing else Just $ modifyFrame am
  in Just basicFrame : map modifyFrames (zip anim ([] : anim))

blank :: Maybe AttrCharW32
blank = Nothing

cSym :: Color -> Char -> Maybe AttrCharW32
cSym color symbol = Just $ attrChar2ToW32 color symbol

mapPosToOffset :: (Point, AttrCharW32) -> (K.PointUI, AttrString)
mapPosToOffset (Point{..}, attr) =
  (K.PointUI (px * 2) (py + K.mapStartY), [attr])

mzipSingleton :: Point -> Maybe AttrCharW32 -> OverlaySpace
mzipSingleton p1 mattr1 =
  map mapPosToOffset $
    let mzip (pos, mattr) = fmap (pos,) mattr
    in catMaybes [mzip (p1, mattr1)]

mzipPairs :: (Point, Point) -> (Maybe AttrCharW32, Maybe AttrCharW32)
          -> OverlaySpace
mzipPairs (p1, p2) (mattr1, mattr2) =
  map mapPosToOffset $
    let mzip (pos, mattr) = fmap (pos,) mattr
    in catMaybes $ if p1 /= p2
                   then [mzip (p1, mattr1), mzip (p2, mattr2)]
                   else -- If actor affects himself, show only the effect,
                        -- not the action.
                        [mzip (p1, mattr1)]

pushAndDelay :: Animation
pushAndDelay = Animation [[]]

-- | Attack animation. A part of it also reused for self-damage and healing.
twirlSplash :: (Point, Point) -> Color -> Color -> Animation
twirlSplash poss c1 c2 = Animation $ map (mzipPairs poss)
  [ (blank           , cSym BrCyan '&')
  , (blank           , cSym BrCyan '&')
  , (blank           , blank)
  , (cSym c1      '\\',blank)
  , (cSym c1      '|', cSym BrCyan '&')
  , (cSym c1      '%', cSym BrCyan '&')
  , (cSym c1      '/', blank)
  , (cSym c1      '-', blank)
  , (cSym c1      '\\',blank)
  , (cSym c2      '|', blank)
  , (cSym c2      '%', blank)
  ]

-- | Attack that hits through a block.
blockHit :: (Point, Point) -> Color -> Color -> Animation
blockHit poss c1 c2 = Animation $ map (mzipPairs poss)
  [ (blank           , cSym BrCyan '&')
  , (blank           , cSym BrCyan '&')
  , (blank           , blank)
  , (cSym BrBlue  '{', blank)
  , (cSym BrBlue  '{', cSym BrCyan '&')
  , (cSym BrBlue  '{', cSym BrCyan '&')
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
  [ (blank           , cSym BrCyan '&')
  , (blank           , cSym BrCyan '&')
  , (blank           , blank)
  , (cSym BrBlue  '{', blank)
  , (cSym BrBlue  '{', cSym BrCyan '&')
  , (cSym BrBlue  '{', cSym BrCyan '&')
  , (cSym BrBlue  '{', blank)
  , (cSym BrBlue  '}', blank)
  , (cSym Blue    '}', blank)
  , (cSym Blue    '}', blank)
  , (cSym Blue    '}', blank)
  ]

-- | Attack that is subtle (e.g., damage dice 0).
subtleHit :: (Point, Point) -> Animation
subtleHit poss = Animation $ map (mzipPairs poss)
  [ (blank           , cSym BrCyan '&')
  , (blank           , blank)
  , (blank           , cSym BrYellow '&')
  , (cSym BrBlue  '\\',blank)
  , (blank           , cSym BrYellow '&')
  , (cSym BrBlue  '/', blank)
  , (blank           , blank)
  ]

-- | Death animation for an organic body.
deathBody :: Point -> Animation
deathBody pos = Animation $ map (mzipSingleton pos)
  [ cSym Red '%'
  , cSym Red '-'
  , cSym Red '-'
  , cSym Red '\\'
  , cSym Red '\\'
  , cSym Red '|'
  , cSym Red '|'
  , cSym Red '%'
  , cSym Red '%'
  , cSym Red '%'
  , cSym Red '%'
  , cSym Red ';'
  , cSym Red ';'
  ]

-- | Death animation for an organic body, short version (e.g., for enemies).
shortDeathBody :: Point -> Animation
shortDeathBody pos = Animation $ map (mzipSingleton pos)
  [ cSym Red '%'
  , cSym Red '-'
  , cSym Red '\\'
  , cSym Red '|'
  , cSym Red '%'
  , cSym Red '%'
  , cSym Red '%'
  , cSym Red ';'
  , cSym Red ','
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

-- | Terrain feature vanishing animation.
vanish :: Point -> Animation
vanish pos = Animation $ map (mzipSingleton pos)
  [ cSym BrMagenta 'o'
  , cSym BrMagenta 'O'
  , cSym Magenta   'o'
  , cSym Magenta   '.'
  , cSym Magenta   '.'
  , blank
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

fadeout :: ScreenContent -> Bool -> Int -> Rnd Animation
fadeout ScreenContent{rwidth, rheight} out step = do
  let xbound = rwidth - 1
      ybound = rheight - 1
      margin = (rwidth - 2 * rheight) `div` 2 - 2
      edge = EM.fromDistinctAscList $ zip [1..] ".%&%;:,."
      fadeChar :: Int -> Int -> Int -> Int -> Char
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
        w <- randomWord32
        -- @fromIntegral@ is potentially costly, but arch-independent.
        let fadeAttr !y !x = attrChar1ToW32 $ fadeChar (fromIntegral w) n x y
            fadeLine !y =
              let x1 :: Int
                  {-# INLINE x1 #-}
                  x1 = min xbound (n - 2 * (ybound - y))
                  x2 :: Int
                  {-# INLINE x2 #-}
                  x2 = max 0 (xbound - (n - 2 * y))
              in [ (K.PointUI 0 y, map (fadeAttr y) [0..x1])
                 , (K.PointUI (2 * x2) y, map (fadeAttr y) [x2..xbound]) ]
        return $! concatMap fadeLine [0..ybound]
      fs | out = [3, 3 + step .. rwidth - margin]
         | otherwise = [rwidth - margin, rwidth - margin - step .. 1]
                       ++ [0]  -- no remnants of fadein onscreen, in case of lag
  Animation <$> mapM rollFrame fs
