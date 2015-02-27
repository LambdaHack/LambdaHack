{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Screen frames and animations.
module Game.LambdaHack.Client.UI.Animation
  ( SingleFrame(..), decodeLine, encodeLine
  , overlayOverlay
  , Animation, Frames, renderAnim, restrictAnim
  , twirlSplash, blockHit, blockMiss, deathBody, actorX
  , swapPlaces, moveProj, fadeout
  ) where

import Control.Exception.Assert.Sugar
import Data.Binary
import Data.Bits
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Vector.Generic as G
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Color
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random

decodeLine :: ScreenLine -> [AttrChar]
decodeLine v = map (toEnum . fromIntegral) $ G.toList v

-- | The data sufficent to draw a single game screen frame.
data SingleFrame = SingleFrame
  { sfLevel  :: ![ScreenLine]  -- ^ screen, from top to bottom, line by line
  , sfTop    :: !Overlay       -- ^ some extra lines to show over the top
  , sfBottom :: ![ScreenLine]  -- ^ some extra lines to show at the bottom
  , sfBlank  :: !Bool          -- ^ display only @sfTop@, on blank screen
  }
  deriving (Eq, Show, Generic)

instance Binary SingleFrame

-- | Overlays the @sfTop@ and @sfBottom@ fields onto the @sfLevel@ field.
-- The resulting frame has empty @sfTop@ and @sfBottom@.
-- To be used by simple frontends that don't display overlays
-- in separate windows/panes/scrolled views.
overlayOverlay :: SingleFrame -> SingleFrame
overlayOverlay SingleFrame{..} =
  let lxsize = fst normalLevelBound + 1  -- TODO
      lysize = snd normalLevelBound + 1
      emptyLine = encodeLine
                  $ replicate lxsize (Color.AttrChar Color.defAttr ' ')
      canvasLength = if sfBlank then lysize + 3 else lysize + 1
      canvas | sfBlank = replicate canvasLength emptyLine
             | otherwise = emptyLine : sfLevel
      topTrunc = overlay sfTop
      topLayer = if length topTrunc <= canvasLength
                 then topTrunc
                 else take (canvasLength - 1) topTrunc
                      ++ [toScreenLine "--a portion of the text trimmed--"]
      f layerLine canvasLine =
        layerLine G.++ G.drop (G.length layerLine) canvasLine
      picture = zipWith f topLayer canvas
      bottomLines = if sfBlank then [] else sfBottom
      newLevel = picture ++ drop (length picture) canvas ++ bottomLines
  in SingleFrame { sfLevel = newLevel
                 , sfTop = emptyOverlay
                 , sfBottom = []
                 , sfBlank }

-- | Animation is a list of frame modifications to play one by one,
-- where each modification if a map from positions to level map symbols.
newtype Animation = Animation [EM.EnumMap Point AttrChar]
  deriving (Eq, Show, Monoid)

-- | Sequences of screen frames, including delays.
type Frames = [Maybe SingleFrame]

-- | Render animations on top of a screen frame.
renderAnim :: X -> Y -> SingleFrame -> Animation -> Frames
renderAnim lxsize lysize basicFrame (Animation anim) =
  let modifyFrame SingleFrame{sfLevel = []} _ =
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
              in map encodeLine
                 $ foldl' f [] (zip [lysize-1,lysize-2..0]
                                $ reverse $ map decodeLine levelOld)
        in Just SingleFrame{..}  -- a thunk within Just
  in map (modifyFrame basicFrame) anim

blank :: Maybe AttrChar
blank = Nothing

cSym :: Color -> Char -> Maybe AttrChar
cSym color symbol = Just $ AttrChar (Attr color defBG) symbol

mzipPairs :: (Point, Point) -> (Maybe AttrChar, Maybe AttrChar)
          -> [(Point, AttrChar)]
mzipPairs (p1, p2) (mattr1, mattr2) =
  let mzip (pos, mattr) = fmap (\x -> (pos, x)) mattr
  in catMaybes $ if p1 /= p2
                 then [mzip (p1, mattr1), mzip (p2, mattr2)]
                 else -- If actor affects himself, show only the effect,
                      -- not the action.
                      [mzip (p1, mattr1)]

mzipTriples :: (Point, Point, Point)
            -> (Maybe AttrChar, Maybe AttrChar, Maybe AttrChar)
            -> [(Point, AttrChar)]
mzipTriples (p1, p2, p3) (mattr1, mattr2, mattr3) =
  let mzip (pos, mattr) = fmap (\x -> (pos, x)) mattr
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
deathBody pos = Animation $ map (maybe EM.empty (EM.singleton pos))
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
actorX pos symbol color = Animation $ map (maybe EM.empty (EM.singleton pos))
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
  as <- mapM rollFrame $ if out then fs else reverse fs
  return $! Animation as
