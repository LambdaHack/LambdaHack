{-# LANGUAGE RankNTypes #-}
-- | Screen frames.
--
-- Note that @PointArray.Array@ here represents a screen frame and so
-- screen positions are denoted by @Point@, contrary to the convention
-- that @Point@ refers to game map coordinates, as outlined
-- in description of 'PointSquare' that should normally be used in that role.
module Game.LambdaHack.Client.UI.Frame
  ( ColorMode(..)
  , FrameST, FrameForall(..), FrameBase(..), Frame
  , PreFrame3, PreFrames3, PreFrame, PreFrames
  , SingleFrame(..), OverlaySpace
  , blankSingleFrame, truncateOverlay, overlayFrame
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , truncateAttrLine
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Monad.ST.Strict
import           Data.Function
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as VM
import           Data.Word

import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.PointUI
import qualified Game.LambdaHack.Common.PointArray as PointArray
import qualified Game.LambdaHack.Definition.Color as Color

-- | Color mode for the display.
data ColorMode =
    ColorFull  -- ^ normal, with full colours
  | ColorBW    -- ^ black and white only
  deriving Eq

type FrameST s = G.Mutable U.Vector s Word32 -> ST s ()

-- | Efficiently composable representation of an operation
-- on a frame, that is, on a mutable vector. When the composite operation
-- is eventually performed, the vector is frozen to become a 'SingleFrame'.
newtype FrameForall = FrameForall {unFrameForall :: forall s. FrameST s}

-- | Action that results in a base frame, to be modified further.
newtype FrameBase = FrameBase
  {unFrameBase :: forall s. ST s (G.Mutable U.Vector s Word32)}

-- | A frame, that is, a base frame and all its modifications.
type Frame = ( (FrameBase, FrameForall)
             , (OverlaySpace, OverlaySpace, OverlaySpace) )

-- | Components of a frame, before it's decided if the first can be overwritten
-- in-place or needs to be copied.
type PreFrame3 = (PreFrame, (OverlaySpace, OverlaySpace, OverlaySpace))

-- | Sequence of screen frames, including delays. Potentially based on a single
-- base frame.
type PreFrames3 = [Maybe PreFrame3]

-- | A simpler variant of @PreFrame3@.
type PreFrame = (U.Vector Word32, FrameForall)

-- | A simpler variant of @PreFrames3@.
type PreFrames = [Maybe PreFrame]

-- | Representation of an operation of overwriting a frame with a single line
-- at the given row.
writeLine :: Int -> AttrString -> FrameForall
{-# INLINE writeLine #-}
writeLine offset al = FrameForall $ \v -> do
  let writeAt _ [] = return ()
      writeAt off (ac32 : rest) = do
        VM.write v off (Color.attrCharW32 ac32)
        writeAt (off + 1) rest
  writeAt offset al

-- | A frame that is padded to fill the whole screen with optional
-- overlays to display in proportional, square and monospace fonts.
--
-- Note that we don't provide a list of color-highlighed box positions
-- to be drawn separately, because overlays need to obscure not only map,
-- but the highlights as well, so highlights need to be included earlier.
--
-- See the description of 'PointSquare' for explanation of why screen
-- coordinates in @singleArray@ are @Point@ even though they should be
-- 'PointSquare'.
data SingleFrame = SingleFrame
  { singleArray         :: PointArray.Array Color.AttrCharW32
  , singlePropOverlay   :: OverlaySpace
  , singleSquareOverlay :: OverlaySpace
  , singleMonoOverlay   :: OverlaySpace }
  deriving (Show, Eq)

type OverlaySpace = [(PointUI, AttrString)]

blankSingleFrame :: ScreenContent -> SingleFrame
blankSingleFrame ScreenContent{rwidth, rheight} =
  SingleFrame (PointArray.replicateA rwidth rheight Color.spaceAttrW32)
              []
              []
              []

-- | Truncate the overlay: for each line, if it's too long, it's truncated
-- and if there are too many lines, excess is dropped and warning is appended.
-- The width, in the second argument, is calculated in characters,
-- not in UI (mono font) coordinates, so that taking and dropping characters
-- is performed correctly.
truncateOverlay :: Bool -> Int -> Int -> Bool -> Int -> Bool -> Overlay
                -> OverlaySpace
truncateOverlay halveXstart width rheight wipeAdjacentRaw fillLen onBlank ov =
  let wipeAdjacent = wipeAdjacentRaw && not onBlank
      canvasLength = if onBlank then rheight else rheight - 2
      supHeight = maxYofOverlay ov
      trimmedY = canvasLength - 1
      -- Sadly, this does not trim the other, concurrent, overlays that may
      -- obscure the last line and so contend with the "trimmed" message.
      -- Tough luck; just avoid overrunning overlays in higher level code.
      ovTopFiltered = filter (\(PointUI _ y, _) -> y < trimmedY) ov
      trimmedAlert = ( PointUI 0 trimmedY
                     , stringToAL "--a portion of the text trimmed--" )
      extraLine | supHeight < 3
                  || supHeight >= trimmedY
                  || not wipeAdjacent = []
                | otherwise =
        let supHs = filter (\(PointUI _ y, _) -> y == supHeight) ov
        in if null supHs
           then []
           else let (PointUI xLast yLast, _) =
                      minimumBy (comparing $ \(PointUI x _, _) -> x) supHs
                in [(PointUI xLast (yLast + 1), emptyAttrLine)]
      -- This is crude, because an al at lower x may be longer, but KISS.
      -- This also gives a solid rule which al overwrite others
      -- when merging overlays, independent of the order of merging
      -- (except for duplicate x, for which initial order is retained).
      -- The order functions is cheap, we use @sortBy@, not @sortOn@.
      ovTop = groupBy ((==) `on` \(PointUI _ y, _) -> y)
              $ sortBy (comparing $ \(PointUI x y, _) -> (y, x))
              $ if supHeight >= canvasLength
                then ovTopFiltered ++ [trimmedAlert]
                else ov ++ extraLine
      -- Unlike the trimming above, adding spaces around overlay depends
      -- on there being no gaps and a natural order.
      -- Probably also gives messy results when X offsets are not all the same.
      -- Below we at least mitigate the case of multiple lines per row.
      f _ _ [] = error "empty list of overlay lines at the given row"
      f (yPrev, lenPrev) (yNext, lenNext) (minAl@(PointUI _ yCur, _) : rest) =
        g (if yPrev == yCur - 1 then lenPrev else 0)
          (if yNext == yCur + 1 then lenNext else 0)
          fillLen
          minAl
        : map (g 0 0 0) rest
      g lenPrev lenNext fillL (p@(PointUI xstartRaw _), layerLine) =
        let xstart = if halveXstart then xstartRaw `div` 2 else xstartRaw
            -- TODO: lenPrev and lenNext is from the same kind of font;
            -- if fonts are mixed, too few spaces are added.
            -- We'd need to keep a global store of line lengths
            -- for every position on the screen, filled first going
            -- over all texts and only afterwards texts rendered.
            -- And prop font measure would still make this imprecise.
            -- TODO: rewrite ovBackdrop according to this idea,
            -- but then process square font only mode with the same mechanism.
            maxLen = if wipeAdjacent then max lenPrev lenNext else 0
            fillFromStart = max fillL (1 + maxLen) - xstart
            available = width - xstart
        in (p, truncateAttrLine wipeAdjacent available fillFromStart layerLine)
      rightExtentOfLine (PointUI xstartRaw _, al) =
        let xstart = if halveXstart then xstartRaw `div` 2 else xstartRaw
        in min (width - 1) (xstart + length (attrLine al))
      yAndLen [] = (-99, 0)
      yAndLen als@((PointUI _ y, _) : _) =
        (y, maximum $ map rightExtentOfLine als)
      lens = map yAndLen ovTop
      f2 = map g2
      g2 (p@(PointUI xstartRaw _), layerLine) =
        let xstart = if halveXstart then xstartRaw `div` 2 else xstartRaw
            available = width - xstart
        in (p, truncateAttrLine False available 0 layerLine)
  in concat $ if onBlank
              then map f2 ovTop
              else zipWith3 f ((-9, 0) : lens) (drop 1 lens ++ [(999, 0)]) ovTop

-- | Add a space at the message end, for display overlayed over the level map.
-- Also trim (do not wrap!) too long lines. Also add many spaces when under
-- longer lines.
truncateAttrLine :: Bool -> Int -> Int -> AttrLine -> AttrString
truncateAttrLine addSpaces available fillFromStart aLine =
  let al = attrLine aLine
      len = length al
  in if | null al -> if addSpaces
                     then replicate fillFromStart Color.spaceAttrW32
                     else al
        | len == available - 1 && addSpaces -> al ++ [Color.spaceAttrW32]
        | otherwise -> case compare available len of
            LT -> take (available - 1) al ++ [Color.trimmedLineAttrW32]
            GT | addSpaces ->
              let alSpace = al ++ [Color.spaceAttrW32, Color.spaceAttrW32]
                  whiteN = fillFromStart - len - 2
              in if whiteN <= 0
                 then alSpace  -- speedup (supposedly) for menus
                 else alSpace ++ replicate whiteN Color.spaceAttrW32
            _ -> al

-- | Overlays either the game map only or the whole empty screen frame.
-- We assume the lines of the overlay are not too long nor too many.
overlayFrame :: Int -> OverlaySpace -> PreFrame -> PreFrame
overlayFrame width ov (m, ff) =
  ( m
  , FrameForall $ \v -> do
      unFrameForall ff v
      mapM_ (\(PointUI px py, l) ->
               let offset = py * width + px `div` 2
               in unFrameForall (writeLine offset l) v) ov )
