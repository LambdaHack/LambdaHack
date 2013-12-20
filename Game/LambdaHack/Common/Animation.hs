{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Screen frames and animations.
module Game.LambdaHack.Common.Animation
  ( SingleFrame(..), overlayOverlay, xsizeSingleFrame, ysizeSingleFrame
  , Animation, Frames, renderAnim, restrictAnim
  , twirlSplash, blockHit, blockMiss, deathBody, swapPlaces, fadeout
  , AcFrame(..)
  , DebugModeCli(..), defDebugModeCli
  ) where

import Control.Arrow ((***))
import Control.Exception.Assert.Sugar
import Control.Monad
import Data.Binary
import Data.Bits
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.List as L
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

import Game.LambdaHack.Common.Color
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.PointXY
import Game.LambdaHack.Common.Random

-- | The data sufficent to draw a single game screen frame.
data SingleFrame = SingleFrame
  { sfLevel  :: ![[AttrChar]]  -- ^ screen, from the top, line by line
  , sfTop    :: !Overlay       -- ^ some extra lines to show at the top
  , sfBottom :: !Text          -- ^ an extra line to show at the bottom
                               --   unless the top lines leave no space
  }
  deriving (Eq, Show)

-- | Overlays the sfTop overlay onto the @sfLevel@ and, if needed,
-- the @sfBottom@ fields. The resulting frame has empty @sfTop@.
-- To be used by simple frontends that don't display overlays
-- in separate windows/panes/scrolled views.
overlayOverlay :: SingleFrame -> SingleFrame
overlayOverlay sf@SingleFrame{sfTop=[]} = sf
overlayOverlay sf@SingleFrame{..} =
  let lxsize = xsizeSingleFrame sf
      lysize = ysizeSingleFrame sf
      (over, bottom) = splitAt (lysize + 1)
                       $ map (truncateMsg lxsize) sfTop
      both = zip over sfLevel
      f (t, line) = map (Color.AttrChar Color.defAttr) (T.unpack t)
                    ++ drop (T.length t) line
      newLevel = map f both ++ drop (length both) sfLevel
      newBottom = case bottom of
        [] -> sfBottom
        [s] -> s
        _ -> "--a portion of the text trimmed--"
  in SingleFrame {sfLevel = newLevel, sfTop = [], sfBottom = newBottom}

-- | Animation is a list of frame modifications to play one by one,
-- where each modification if a map from positions to level map symbols.
newtype Animation = Animation [EM.EnumMap Point AttrChar]
  deriving (Eq, Show, Monoid)

-- | Sequences of screen frames, including delays.
type Frames = [Maybe SingleFrame]

xsizeSingleFrame :: SingleFrame -> X
xsizeSingleFrame SingleFrame{sfLevel=[]} = 0
xsizeSingleFrame SingleFrame{sfLevel=line : _} = length line

ysizeSingleFrame :: SingleFrame -> X
ysizeSingleFrame SingleFrame{sfLevel} = length sfLevel - 1

-- | Render animations on top of a screen frame.
renderAnim :: X -> Y -> SingleFrame -> Animation -> Frames
renderAnim lxsize lysize basicFrame (Animation anim) =
  let modifyFrame SingleFrame{sfLevel = []} _ =
        assert `failure` (lxsize, lysize, basicFrame, anim)
      modifyFrame SingleFrame{sfLevel = topLine : levelOld, ..} am =
        let fLine y lineOld =
              let f l (x, acOld) =
                    let pos = toPoint lxsize (PointXY (x, y))
                        !ac = fromMaybe acOld $ EM.lookup pos am
                    in ac : l
              in L.foldl' f [] (zip [lxsize-1,lxsize-2..0] (reverse lineOld))
            sfLevel =  -- fully evaluated inside
              topLine
              : let f l (y, lineOld) = let !line = fLine y lineOld in line : l
                in L.foldl' f [] (zip [lysize-1,lysize-2..0]
                                  $ reverse levelOld)
        in Just SingleFrame{..}  -- a thunk within Just
  in map (modifyFrame basicFrame) anim

blank :: Maybe AttrChar
blank = Nothing

coloredSymbol :: Color -> Char -> Maybe AttrChar
coloredSymbol color symbol = Just $ AttrChar (Attr color defBG) symbol

mzipPairs :: (Point, Point) -> (Maybe AttrChar, Maybe AttrChar)
          -> [(Point, AttrChar)]
mzipPairs (p1, p2) (mattr1, mattr2) =
  let mzip (pos, mattr) = fmap (\x -> (pos, x)) mattr
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

-- | Attack animation. A part of it also reused for self-damage and healing.
twirlSplash :: (Point, Point) -> Color -> Color -> Animation
twirlSplash poss c1 c2 = Animation $ map (EM.fromList . mzipPairs poss)
  [ (coloredSymbol BrWhite '*', blank)
  , (coloredSymbol c1      '/', coloredSymbol BrCyan '^')
  , (coloredSymbol c1      '-', blank)
  , (coloredSymbol c1      '\\',blank)
  , (coloredSymbol c1      '|', blank)
  , (coloredSymbol c2      '%', blank)
  , (coloredSymbol c2      '%', blank)
  , (blank                    , blank)
  ]

-- | Attack that hits through a block.
blockHit :: (Point, Point) -> Color -> Color -> Animation
blockHit poss c1 c2 = Animation $ map (EM.fromList . mzipPairs poss)
  [ (coloredSymbol BrWhite '*', blank)
  , (coloredSymbol BrBlue  '{', coloredSymbol BrCyan '^')
  , (coloredSymbol BrBlue  '{', blank)
  , (coloredSymbol BrBlue  '}', blank)
  , (coloredSymbol c1      '/', blank)
  , (coloredSymbol c2      '%', blank)
  , (coloredSymbol c2      '%', blank)
  , (blank                    , blank)
  ]

-- | Attack that is blocked.
blockMiss :: (Point, Point) -> Animation
blockMiss poss = Animation $ map (EM.fromList . mzipPairs poss)
  [ (coloredSymbol BrWhite '*', blank)
  , (coloredSymbol BrBlue  '{', coloredSymbol BrCyan '\'')
  , (coloredSymbol BrBlue  '}', blank)
  , (blank                    , blank)
  ]

-- | Death animation for an organic body.
deathBody :: Point -> Animation
deathBody pos = Animation $ map (maybe EM.empty (EM.singleton pos))
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
      startN = if out then 3 else 1
      fs = [startN..3 * lxsize `divUp` 4 + 2]
  as <- mapM rollFrame $ if out then fs else reverse fs
  return $ Animation as

data AcFrame =
    AcConfirm !SingleFrame
  | AcRunning !SingleFrame
  | AcNormal !SingleFrame
  | AcDelay
  deriving (Show, Eq)

data DebugModeCli = DebugModeCli
  { sfont          :: !(Maybe String)
      -- ^ Font to use for the main game window.
  , smaxFps        :: !(Maybe Int)
      -- ^ Maximal frames per second.
      -- This is better low and fixed, to avoid jerkiness and delays
      -- that tell the player there are many intelligent enemies on the level.
      -- That's better than scaling AI sofistication down based
      -- on the FPS setting and machine speed.
  , snoDelay       :: !Bool
      -- ^ Don't maintain any requested delays between frames,
      -- e.g., for screensaver.
  , snoMore        :: !Bool
      -- ^ Auto-answer all prompts, e.g., for screensaver.
  , snoAnim        :: !(Maybe Bool)
      -- ^ Don't show any animations.
  , snewGameCli    :: !Bool
      -- ^ Start a new game, overwriting the save file.
  , ssavePrefixCli :: !(Maybe String)
      -- ^ Prefix of the save game file.
  , sfrontendStd   :: !Bool
      -- ^ Whether to use the stdout/stdin frontend.
  , sdbgMsgCli     :: !Bool
      -- ^ Show clients' internal debug messages.
  }
  deriving (Show, Eq)

defDebugModeCli :: DebugModeCli
defDebugModeCli = DebugModeCli
  { sfont = Nothing
  , smaxFps = Nothing
  , snoDelay = False
  , snoMore = False
  , snoAnim = Nothing
  , snewGameCli = False
  , ssavePrefixCli = Nothing
  , sfrontendStd = False
  , sdbgMsgCli = False
  }

instance Binary AcFrame where
  put (AcConfirm fr) = putWord8 0 >> put fr
  put (AcRunning fr) = putWord8 1 >> put fr
  put (AcNormal fr)  = putWord8 2 >> put fr
  put AcDelay        = putWord8 3
  get = do
    tag <- getWord8
    case tag of
      0 -> liftM AcConfirm get
      1 -> liftM AcRunning get
      2 -> liftM AcNormal get
      3 -> return AcDelay
      _ -> fail "no parse (AcFrame)"

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

instance Binary DebugModeCli where
  put DebugModeCli{..} = do
    put sfont
    put smaxFps
    put snoDelay
    put snoMore
    put snoAnim
    put snewGameCli
    put ssavePrefixCli
    put sfrontendStd
    put sdbgMsgCli
  get = do
    sfont <- get
    smaxFps <- get
    snoDelay <- get
    snoMore <- get
    snoAnim <- get
    snewGameCli <- get
    ssavePrefixCli <- get
    sfrontendStd <- get
    sdbgMsgCli <- get
    return DebugModeCli{..}
