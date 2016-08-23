{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Screen frames and animations.
module Game.LambdaHack.Client.UI.Animation
  ( Animation, renderAnim, restrictAnim
  , pushAndDelay, blinkColorActor, twirlSplash, blockHit, blockMiss
  , deathBody, actorX, swapPlaces, teleport, fadeout
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Bits
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES

import Game.LambdaHack.Client.UI.Frame
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random

type AnimationDiff = EM.EnumMap Point AttrChar

-- | Animation is a list of frame modifications to play one by one,
-- where each modification if a map from positions to level map symbols.
newtype Animation = Animation [AnimationDiff]
  deriving (Eq, Show)

-- | Render animations on top of a screen frame.
renderAnim :: SingleFrame -> Animation -> Frames
renderAnim basicFrame@SingleFrame{singleFrame = levelOld, ..} (Animation anim) =
  let modifyFrame :: AnimationDiff -> SingleFrame
      modifyFrame am =
        let fLine (y, lineOld) =
              let f (x, acOld) = EM.findWithDefault acOld (Point x y) am
              in map f $ zip [0..] lineOld
            singleFrame = map fLine $ zip [0..] levelOld
        in SingleFrame{..}
      modifyFrames :: (AnimationDiff, AnimationDiff) -> Maybe SingleFrame
      modifyFrames (am, amPrevious) =
        if am == amPrevious then Nothing else Just $ modifyFrame am
  in Just basicFrame : map modifyFrames (zip anim (EM.empty : anim))

blank :: Maybe AttrChar
blank = Nothing

cSym :: Color -> Char -> Maybe AttrChar
cSym color symbol = Just $ AttrChar (Attr color defBG) symbol

mapPosToScreenPos :: (Point, AttrChar) -> (Point, AttrChar)
mapPosToScreenPos (Point{..}, attr) = (Point{py = py + 1, ..}, attr)

mzipSingleton :: Point -> Maybe AttrChar -> [(Point, AttrChar)]
mzipSingleton p1 mattr1 = map mapPosToScreenPos $
  let mzip (pos, mattr) = fmap (\attr -> (pos, attr)) mattr
  in catMaybes $ [mzip (p1, mattr1)]

mzipPairs :: (Point, Point) -> (Maybe AttrChar, Maybe AttrChar)
          -> [(Point, AttrChar)]
mzipPairs (p1, p2) (mattr1, mattr2) = map mapPosToScreenPos $
  let mzip (pos, mattr) = fmap (\attr -> (pos, attr)) mattr
  in catMaybes $ if p1 /= p2
                 then [mzip (p1, mattr1), mzip (p2, mattr2)]
                 else -- If actor affects himself, show only the effect,
                      -- not the action.
                      [mzip (p1, mattr1)]

restrictAnim :: ES.EnumSet Point -> Animation -> Animation
restrictAnim vis (Animation as) =
  let f imap =
        let common = EM.intersection imap $ EM.fromSet (const ()) vis
        in if EM.null common then Nothing else Just common
  in Animation $ mapMaybe f as

pushAndDelay :: Animation
pushAndDelay = Animation [EM.empty]

blinkColorActor :: Point -> Char -> Color -> Color -> Animation
blinkColorActor pos symbol fromCol toCol =
  Animation $ map (EM.fromList . mzipSingleton pos)
  [ cSym fromCol symbol
  , cSym toCol symbol
  , cSym fromCol symbol
  , cSym toCol symbol
  , cSym fromCol symbol
  ]

-- | Attack animation. A part of it also reused for self-damage and healing.
twirlSplash :: (Point, Point) -> Color -> Color -> Animation
twirlSplash poss c1 c2 = Animation $ map (EM.fromList . mzipPairs poss)
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
blockHit poss c1 c2 = Animation $ map (EM.fromList . mzipPairs poss)
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
blockMiss poss = Animation $ map (EM.fromList . mzipPairs poss)
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
deathBody pos = Animation $ map (EM.fromList . mzipSingleton pos)
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
actorX pos = Animation $ map (EM.fromList . mzipSingleton pos)
  [ cSym BrRed 'X'
  , cSym BrRed 'X'
  , blank
  , blank
  ]

-- | Actor teleport animation.
teleport :: (Point, Point) -> Animation
teleport poss = Animation $ map (EM.fromList . mzipPairs poss)
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
swapPlaces poss = Animation $ map (EM.fromList . mzipPairs poss)
  [ (cSym BrMagenta 'o', cSym Magenta   'o')
  , (cSym BrMagenta 'd', cSym Magenta   'p')
  , (cSym BrMagenta '.', cSym Magenta   'p')
  , (cSym Magenta   'p', cSym Magenta   '.')
  , (cSym Magenta   'p', cSym BrMagenta 'd')
  , (cSym Magenta   'p', cSym BrMagenta 'd')
  , (cSym Magenta   'o', blank)
  , (blank             , blank)
  ]

fadeout :: Bool -> Bool -> Int -> X -> Y -> Rnd Animation
fadeout out topRight step lxsize lysize = do
  let xbound = lxsize - 1
      ybound = lysize + 2
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
        let l = [ ( Point (if topRight then x else xbound - x) y
                  , AttrChar defAttr $ fadeChar r n x y )
                | x <- [0..xbound]
                , y <- [max 0 (ybound - (n - x) `div` 2)..ybound]
                    ++ [0..min ybound ((n - xbound + x) `div` 2)]
                ]
        return $! EM.fromList l
      fs | out = [3, 3 + step .. lxsize - 14]
         | otherwise = [lxsize - 14, lxsize - 14 - step .. 1]
  Animation <$> mapM rollFrame fs
