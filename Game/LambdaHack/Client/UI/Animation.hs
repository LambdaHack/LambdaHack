{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Screen frames and animations.
module Game.LambdaHack.Client.UI.Animation
  ( overlayFrame
  , Animation, Frames, renderAnim, restrictAnim
  , twirlSplash, blockHit, blockMiss, deathBody, actorX
  , swapPlaces, moveProj, fadeout
  ) where

import Prelude ()
import Prelude.Compat

import Control.Exception.Assert.Sugar
import Data.Bits
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List (foldl')
import Data.Maybe

import Game.LambdaHack.Common.Color
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random

-- | Overlays with a given overlay either the top line and level map area
-- of a screen frame or the whole area of a completely empty screen frame.
overlayFrame :: Overlay -> Maybe SingleFrame -> SingleFrame
overlayFrame sfTop msf =
  let lxsize = fst normalLevelBound + 1  -- TODO
      lysize = snd normalLevelBound + 1
      emptyLine = replicate lxsize (Color.AttrChar Color.defAttr ' ')
      canvasLength = if isNothing msf then lysize + 3 else lysize + 1
      canvas = maybe (replicate canvasLength emptyLine)
                     (\sf -> overlay (sfLevel sf))
                     msf
      topTrunc = overlay sfTop
      topLayer = if length topTrunc <= canvasLength
                 then topTrunc
                 else take (canvasLength - 1) topTrunc
                      ++ overlay (toOverlay ["--a portion of the text trimmed--"])
      f layerLine canvasLine =
        let truncated = truncateMsg lxsize layerLine
        in truncated ++ drop (length truncated) canvasLine
      picture = zipWith f topLayer canvas
      newLevel = picture ++ drop (length picture) canvas
  in SingleFrame $ toOverlayRaw newLevel

-- | Add a space at the message end, for display overlayed over the level map.
-- Also trim (do not wrap!) too long lines.
truncateMsg :: X -> [AttrChar] -> [AttrChar]
truncateMsg w xs =
  case compare w (length xs) of
    LT -> let discarded = drop w xs
          in if all ((== ' ') . acChar) discarded
             then take w xs
             else take (w - 1) xs ++ [AttrChar (Attr BrBlack defBG) '$']
    EQ -> xs
    GT -> if null xs || acChar (last xs) == ' '
          then xs
          else xs ++ [AttrChar Color.defAttr ' ']

-- | Animation is a list of frame modifications to play one by one,
-- where each modification if a map from positions to level map symbols.
newtype Animation = Animation [EM.EnumMap Point AttrChar]
  deriving (Eq, Show, Monoid)

-- | Sequences of screen frames, including delays.
type Frames = [Maybe SingleFrame]

-- | Render animations on top of a screen frame.
renderAnim :: X -> Y -> SingleFrame -> Animation -> Frames
renderAnim lxsize lysize basicFrame (Animation anim) =
  let modifyFrame SingleFrame{sfLevel} _ | overlay sfLevel == [] =
        assert `failure` (lxsize, lysize, basicFrame, anim)
      modifyFrame SingleFrame{sfLevel = levelOld, ..} am =
        let fLine y lineOld =
              let f l (x, acOld) =
                    let pos = Point x y
                        !ac = EM.findWithDefault acOld pos am
                    in ac : l
              in foldl' f [] (zip [lxsize-1,lxsize-2..0] (reverse lineOld))
            sfLevel =  -- fully evaluated inside
              let f l (y, lineOld) = let !line = fLine y lineOld in line : l
              in toOverlayRaw
                 $ foldl' f [] (reverse $ zip [0..] $ overlay levelOld)
        in Just SingleFrame{..}  -- a thunk within Just
  in map (modifyFrame basicFrame) anim

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

mzipTriples :: (Point, Point, Point)
            -> (Maybe AttrChar, Maybe AttrChar, Maybe AttrChar)
            -> [(Point, AttrChar)]
mzipTriples (p1, p2, p3) (mattr1, mattr2, mattr3) = map mapPosToScreenPos $
  let mzip (pos, mattr) = fmap (\attr -> (pos, attr)) mattr
  in catMaybes [mzip (p1, mattr1), mzip (p2, mattr2), mzip (p3, mattr3)]

restrictAnim :: ES.EnumSet Point -> Animation -> Animation
restrictAnim vis (Animation as) =
  let f imap =
        let common = EM.intersection imap $ EM.fromSet (const ()) vis
          in if EM.null common then Nothing else Just common
  in Animation $ mapMaybe f as

-- TODO: in all but moveProj duplicate first and/or last frame, if required,
-- since they are no longer duplicated in renderAnim

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
actorX :: Point -> Char -> Color.Color -> Animation
actorX pos symbol color = Animation $ map (EM.fromList . mzipSingleton pos)
  [ cSym BrRed 'X'
  , cSym BrRed 'X'
  , cSym BrRed symbol
  , cSym color symbol
  , cSym color symbol
  , cSym color symbol
  , cSym color symbol
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
  ]

moveProj :: (Point, Point, Point) -> Char -> Color.Color -> Animation
moveProj poss symbol color = Animation $ map (EM.fromList . mzipTriples poss)
  [ (cSym BrBlack '.', cSym color symbol  , cSym color '.')
--  , (cSym BrBlack '.', cSym BrBlack symbol, cSym color symbol)
  , (cSym BrBlack '.', cSym BrBlack '.'   , cSym color symbol)
  , (blank           , cSym BrBlack '.'   , cSym color symbol)
  ]

fadeout :: Bool -> Bool -> Int -> X -> Y -> Rnd Animation
fadeout out topRight step lxsize lysize = do
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
        let l = [ ( Point (if topRight then x else xbound - x) y
                  , AttrChar defAttr $ fadeChar r n x y )
                | x <- [0..xbound]
                , y <- [max 0 (ybound - (n - x) `div` 2)..ybound]
                    ++ [0..min ybound ((n - xbound + x) `div` 2)]
                ]
        return $! EM.fromList l
      startN = if out then 3 else 1
      fs = [startN, startN + step .. 3 * lxsize `divUp` 4 + 2]
  as <- mapM rollFrame fs
  return $! Animation $ if out then as else reverse (EM.empty : as)
