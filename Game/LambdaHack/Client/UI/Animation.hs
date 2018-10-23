{-# LANGUAGE TupleSections #-}
-- | Screen frames and animations.
module Game.LambdaHack.Client.UI.Animation
  ( Animation, renderAnim
  , pushAndDelay, blinkColorActor, twirlSplash, blockHit, blockMiss, subtleHit
  , deathBody, shortDeathBody, actorX, teleport, swapPlaces, fadeout
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , blank, cSym, mapPosToOffset, mzipSingleton, mzipPairs
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Data.Bits
import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Client.UI.Content.Screen
import Game.LambdaHack.Client.UI.Frame
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random

-- | Animation is a list of frame modifications to play one by one,
-- where each modification if a map from positions to level map symbols.
newtype Animation = Animation [IntOverlay]
  deriving (Eq, Show)

-- | Render animations on top of a screen frame.
--
-- Located in this module to keep @Animation@ abstract.
renderAnim :: Frame -> Animation -> Frames
renderAnim basicFrame (Animation anim) =
  let modifyFrame :: IntOverlay -> Frame
      modifyFrame am = overlayFrame am basicFrame
      modifyFrames :: (IntOverlay, IntOverlay) -> Maybe Frame
      modifyFrames (am, amPrevious) =
        if am == amPrevious then Nothing else Just $ modifyFrame am
  in Just basicFrame : map modifyFrames (zip anim ([] : anim))

blank :: Maybe AttrCharW32
blank = Nothing

cSym :: Color -> Char -> Maybe AttrCharW32
cSym color symbol = Just $ attrChar2ToW32 color symbol

mapPosToOffset :: ScreenContent -> (Point, AttrCharW32) -> (Int, [AttrCharW32])
mapPosToOffset ScreenContent{rwidth} (Point{..}, attr) =
  ((py + 1) * rwidth + px, [attr])

mzipSingleton :: ScreenContent -> Point -> Maybe AttrCharW32 -> IntOverlay
mzipSingleton coscreen p1 mattr1 = map (mapPosToOffset coscreen) $
  let mzip (pos, mattr) = fmap (pos,) mattr
  in catMaybes [mzip (p1, mattr1)]

mzipPairs :: ScreenContent -> (Point, Point) -> (Maybe AttrCharW32, Maybe AttrCharW32)
          -> IntOverlay
mzipPairs coscreen (p1, p2) (mattr1, mattr2) = map (mapPosToOffset coscreen) $
  let mzip (pos, mattr) = fmap (pos,) mattr
  in catMaybes $ if p1 /= p2
                 then [mzip (p1, mattr1), mzip (p2, mattr2)]
                 else -- If actor affects himself, show only the effect,
                      -- not the action.
                      [mzip (p1, mattr1)]

pushAndDelay :: Animation
pushAndDelay = Animation [[]]

blinkColorActor :: ScreenContent -> Point -> Char -> Color -> Color -> Animation
blinkColorActor coscreen pos symbol fromCol toCol =
  Animation $ map (mzipSingleton coscreen pos)
  [ cSym toCol symbol
  , cSym toCol symbol
  , cSym fromCol symbol
  , cSym fromCol symbol
  ]

-- | Attack animation. A part of it also reused for self-damage and healing.
twirlSplash :: ScreenContent -> (Point, Point) -> Color -> Color -> Animation
twirlSplash coscreen poss c1 c2 = Animation $ map (mzipPairs coscreen poss)
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
  ]

-- | Attack that hits through a block.
blockHit :: ScreenContent -> (Point, Point) -> Color -> Color -> Animation
blockHit coscreen poss c1 c2 = Animation $ map (mzipPairs coscreen poss)
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
blockMiss :: ScreenContent -> (Point, Point) -> Animation
blockMiss coscreen poss = Animation $ map (mzipPairs coscreen poss)
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

-- | Attack that is subtle (e.g., damage dice 0).
subtleHit :: ScreenContent -> Point -> Animation
subtleHit coscreen pos = Animation $ map (mzipSingleton coscreen pos)
  [ cSym BrCyan '\''
  , cSym BrYellow '\''
  , cSym BrYellow '^'
  , cSym BrCyan '^'
  , cSym BrCyan '\''
  ]

-- | Death animation for an organic body.
deathBody :: ScreenContent -> Point -> Animation
deathBody coscreen pos = Animation $ map (mzipSingleton coscreen pos)
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
shortDeathBody :: ScreenContent -> Point -> Animation
shortDeathBody coscreen pos = Animation $ map (mzipSingleton coscreen pos)
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
actorX :: ScreenContent -> Point -> Animation
actorX coscreen pos = Animation $ map (mzipSingleton coscreen pos)
  [ cSym BrRed 'X'
  , cSym BrRed 'X'
  , blank
  , blank
  ]

-- | Actor teleport animation.
teleport :: ScreenContent -> (Point, Point) -> Animation
teleport coscreen poss = Animation $ map (mzipPairs coscreen poss)
  [ (cSym BrMagenta 'o', cSym Magenta   '.')
  , (cSym BrMagenta 'O', cSym Magenta   '.')
  , (cSym Magenta   'o', cSym Magenta   'o')
  , (cSym Magenta   '.', cSym BrMagenta 'O')
  , (cSym Magenta   '.', cSym BrMagenta 'o')
  , (cSym Magenta   '.', blank)
  , (blank             , blank)
  ]

-- | Swap-places animation, both hostile and friendly.
swapPlaces :: ScreenContent -> (Point, Point) -> Animation
swapPlaces coscreen poss = Animation $ map (mzipPairs coscreen poss)
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
        let fadeAttr !y !x = attrChar1ToW32 $ fadeChar r n x y
            fadeLine !y =
              let x1 :: Int
                  {-# INLINE x1 #-}
                  x1 = min xbound (n - 2 * (ybound - y))
                  x2 :: Int
                  {-# INLINE x2 #-}
                  x2 = max 0 (xbound - (n - 2 * y))
              in [ (y * rwidth, map (fadeAttr y) [0..x1])
                 , (y * rwidth + x2, map (fadeAttr y) [x2..xbound]) ]
        return $! concatMap fadeLine [0..ybound]
      fs | out = [3, 3 + step .. rwidth - margin]
         | otherwise = [rwidth - margin, rwidth - margin - step .. 1]
                       ++ [0]  -- no remnants of fadein onscreen, in case of lag
  Animation <$> mapM rollFrame fs
