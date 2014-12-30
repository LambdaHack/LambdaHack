{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Game messages displayed on top of the screen for the player to read.
module Game.LambdaHack.Common.Msg
  ( makePhrase, makeSentence
  , Msg, (<>), (<+>), tshow, toWidth, moreMsg, endMsg, yesnoMsg, truncateMsg
  , Report, emptyReport, nullReport, singletonReport, addMsg, prependMsg
  , splitReport, renderReport, findInReport, lastMsgOfReport
  , History, emptyHistory, lengthHistory, singletonHistory
  , addReport, renderHistory, takeHistory, lastReportOfHistory
  , Overlay(overlay), emptyOverlay, truncateToOverlay, toOverlay
  , Slideshow(slideshow), splitOverlay, toSlideshow
  , encodeLine, encodeOverlay, ScreenLine, toScreenLine, splitText
  )
  where

import Control.Exception.Assert.Sugar
import Data.Binary
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int32)
import Data.List
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Time

infixr 6 <+>  -- TODO: not needed when we require a very new minimorph
(<+>) :: Text -> Text -> Text
(<+>) = (MU.<+>)

-- Show and pack the result of @show@.
tshow :: Show a => a -> Text
tshow x = T.pack $ show x

toWidth :: Int -> Text -> Text
toWidth n x = T.take n (T.justifyLeft n ' ' x)

-- | Re-exported English phrase creation functions, applied to default
-- irregular word sets.
makePhrase, makeSentence :: [MU.Part] -> Text
makePhrase = MU.makePhrase MU.defIrregular
makeSentence = MU.makeSentence MU.defIrregular

-- | The type of a single message.
type Msg = Text

-- | The \"press something to see more\" mark.
moreMsg :: Msg
moreMsg = "--more--  "

-- | The \"end of screenfuls of text\" mark.
endMsg :: Msg
endMsg = "--end--  "

-- | The confirmation request message.
yesnoMsg :: Msg
yesnoMsg = "[yn]"

-- | Add a space at the message end, for display overlayed over the level map.
-- Also trims (does not wrap!) too long lines. In case of newlines,
-- displays only the first line, but marks the message as partial.
truncateMsg :: X -> Text -> Text
truncateMsg w xsRaw =
  let xs = case T.lines xsRaw of
        [] -> xsRaw
        [line] -> line
        line : _ -> T.justifyLeft (w + 1) ' ' line
      len = T.length xs
  in case compare w len of
       LT -> T.snoc (T.take (w - 1) xs) '$'
       EQ -> xs
       GT -> if T.null xs || T.last xs == ' '
             then xs
             else T.snoc xs ' '

-- | The type of a set of messages to show at the screen at once.
newtype Report = Report [(BS.ByteString, Int)]
  deriving (Show, Binary)

-- | Empty set of messages.
emptyReport :: Report
emptyReport = Report []

-- | Test if the set of messages is empty.
nullReport :: Report -> Bool
nullReport (Report l) = null l

-- | Construct a singleton set of messages.
singletonReport :: Msg -> Report
singletonReport = addMsg emptyReport

-- TODO: Differentiate from msgAdd. Generally, invent more informative names.
-- | Add message to the end of report.
addMsg :: Report -> Msg -> Report
addMsg r m | T.null m = r
addMsg (Report ((x, n) : xns)) y' | x == y =
  Report $ (y, n + 1) : xns
 where y = encodeUtf8 y'
addMsg (Report xns) y = Report $ (encodeUtf8 y, 1) : xns

prependMsg :: Msg -> Report -> Report
prependMsg m r | T.null m = r
prependMsg y (Report xns) = Report $ xns ++ [(encodeUtf8 y, 1)]

-- | Split a messages into chunks that fit in one line.
-- We assume the width of the messages line is the same as of level map.
splitReport :: X -> Report -> Overlay
splitReport w r = toOverlay $ splitReportList w r

splitReportList :: X -> Report -> [Text]
splitReportList w r = splitText w $ renderReport r

-- | Render a report as a (possibly very long) string.
renderReport :: Report  -> Text
renderReport (Report []) = T.empty
renderReport (Report (xn : xs)) =
  renderReport (Report xs) <+> renderRepetition xn

renderRepetition :: (BS.ByteString, Int) -> Text
renderRepetition (s, 1) = decodeUtf8 s
renderRepetition (s, n) = decodeUtf8 s <> "<x" <> tshow n <> ">"

findInReport :: (BS.ByteString -> Bool) -> Report -> Maybe BS.ByteString
findInReport f (Report xns) = find f $ map fst xns

lastMsgOfReport :: Report -> (BS.ByteString, Report)
lastMsgOfReport (Report rep) = case rep of
  [] -> assert `failure` rep
  (lmsg, 1) : repRest -> (lmsg, Report repRest)
  (lmsg, n) : repRest -> (lmsg, Report $ (lmsg, n - 1) : repRest)

-- | Split a string into lines. Avoids ending the line with a character
-- other than whitespace or punctuation. Space characters are removed
-- from the start, but never from the end of lines. Newlines are respected.
splitText :: X -> Text -> [Text]
splitText w xs = concatMap (splitText' w . T.stripStart) $ T.lines xs

splitText' :: X -> Text -> [Text]
splitText' w xs
  | w >= T.length xs = [xs]  -- no problem, everything fits
  | otherwise =
      let (pre, post) = T.splitAt w xs
          (ppre, ppost) = T.break (== ' ') $ T.reverse pre
          testPost = T.stripEnd ppost
      in if T.null testPost
         then pre : splitText w post
         else T.reverse ppost : splitText w (T.reverse ppre <> post)

-- | The history of reports.
newtype History = History [(Time, Report)]
  deriving (Show, Binary)

-- | Empty history of reports.
emptyHistory :: History
emptyHistory = History []

-- | Construct a singleton history of reports.
singletonHistory :: Time -> Report -> History
singletonHistory = addReport emptyHistory

-- | Add a report to history, handling repetitions.
addReport :: History -> Time -> Report -> History
addReport h _ (Report []) = h
addReport (History []) time m = History [(time, m)]
addReport (History oldRs@((oldTime, Report h) : hs)) time (Report m)  =
  case (reverse m, h) of
    ((s1, n1) : rs, (s2, n2) : hhs) | s1 == s2 ->
      let hist = (oldTime, Report ((s2, n1 + n2) : hhs)) : hs
      in History $ if null rs
                   then hist
                   else (time, Report (reverse rs)) : hist
    _ -> History $ (time, Report m) : oldRs

lengthHistory :: History -> Int
lengthHistory (History rs) = length rs

-- | Render history as many lines of text, wrapping if necessary.
renderHistory :: History -> Overlay
renderHistory (History h) =
  let (x, y) = normalLevelBound
      screenLength = y + 2
      reportLines = concatMap (splitReportForHistory (x + 1)) $ reverse h
      padding = screenLength - length reportLines `mod` screenLength
  in toOverlay $ replicate padding "" ++ reportLines

splitReportForHistory :: X -> (Time, Report) -> [Text]
splitReportForHistory w (time, r) =
  -- TODO: display time fractions with granularity enough to differ
  -- from previous and next report, if possible
  let turns = time `timeFitUp` timeTurn
      ts = splitText (w - 1) $ tshow turns <> ":" <+> renderReport r
  in case ts of
    [] -> []
    hd : tl -> hd : map (T.cons ' ') tl

-- | Take the given prefix of reports from a history.
takeHistory :: Int -> History -> History
takeHistory k (History h) = History $ take k h

lastReportOfHistory :: History -> Maybe Report
lastReportOfHistory (History hist) = case hist of
  [] -> Nothing
  (_, rep) : _ -> Just rep

type ScreenLine = U.Vector Int32

toScreenLine :: Text -> ScreenLine
toScreenLine t = let f c = AttrChar defAttr c
                 in encodeLine $ map f $ T.unpack t

encodeLine :: [AttrChar] -> ScreenLine
encodeLine l = G.fromList $ map (fromIntegral . fromEnum) l

encodeOverlay :: [[AttrChar]] -> Overlay
encodeOverlay = Overlay . map encodeLine

-- | A series of screen lines that may or may not fit the width nor height
-- of the screen. An overlay may be transformed by adding the first line
-- and/or by splitting into a slideshow of smaller overlays.
newtype Overlay = Overlay {overlay :: [ScreenLine]}
  deriving (Show, Eq, Binary)

emptyOverlay :: Overlay
emptyOverlay = Overlay []

truncateToOverlay :: Text -> Overlay
truncateToOverlay msg = toOverlay [msg]

toOverlay :: [Text] -> Overlay
toOverlay = let lxsize = fst normalLevelBound + 1  -- TODO
            in Overlay . map toScreenLine . map (truncateMsg lxsize)

-- | Split an overlay into a slideshow in which each overlay,
-- prefixed by @msg@ and postfixed by @moreMsg@ except for the last one,
-- fits on the screen wrt height (but lines may be too wide).
splitOverlay :: Maybe Bool -> Y -> Overlay -> Overlay -> Slideshow
splitOverlay onBlank yspace (Overlay msg) (Overlay ls) =
  let len = length msg
      endB = if onBlank == Just False
             then [toScreenLine
                   $ endMsg <> "[press PGUP to see previous, ESC to cancel]"]
             else []
  in if len >= yspace
     then  -- no space left for @ls@
       Slideshow (onBlank, [Overlay $ take (yspace - 1) msg
                                      ++ [toScreenLine moreMsg]])
     else let splitO over =
                let (pre, post) = splitAt (yspace - 1) $ msg ++ over
                in if null (drop 1 post)  -- (don't call @length@ on @ls@)
                   then [Overlay $ msg ++ over ++ endB]  -- all fits on screen
                   else let rest = splitO post
                        in Overlay (pre ++ [toScreenLine moreMsg]) : rest
          in Slideshow (onBlank, splitO ls)

-- | A few overlays, displayed one by one upon keypress.
-- When displayed, they are trimmed, not wrapped
-- and any lines below the lower screen edge are not visible.
-- If the first pair element is not @Nothing@, the overlay is displayed
-- over a blank screen, including the bottom lines. The boolean flag
-- then indicates whether to start at the topmost screenful or bottommost.
newtype Slideshow = Slideshow {slideshow :: (Maybe Bool, [Overlay])}
  deriving (Show, Eq)

instance Monoid Slideshow where
  mempty = Slideshow (Nothing, [])
  mappend (Slideshow (b1, l1)) (Slideshow (_, l2)) = Slideshow (b1, l1 ++ l2)

-- | Declare the list of raw overlays to be fit for display on the screen.
-- In particular, current @Report@ is eiter empty or unimportant
-- or contained in the overlays and if any vertical or horizontal
-- trimming of the overlays happens, this is intended.
toSlideshow :: Maybe Bool -> [[Text]] -> Slideshow
toSlideshow onBlank l = Slideshow (onBlank, map toOverlay l)
