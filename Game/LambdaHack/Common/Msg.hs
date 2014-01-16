{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Game messages displayed on top of the screen for the player to read.
module Game.LambdaHack.Common.Msg
  ( makePhrase, makeSentence
  , Msg, (<>), (<+>), showT, toWidth, moreMsg, yesnoMsg, truncateMsg
  , Report, emptyReport, nullReport, singletonReport, addMsg
  , splitReport, renderReport, findInReport
  , History, emptyHistory, singletonHistory, mergeHistory
  , addReport, renderHistory, takeHistory
  , Overlay(overlay), emptyOverlay, truncateToOverlay, toOverlay
  , Slideshow(slideshow), splitOverlay, toSlideshow)
  where

import Data.Binary
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Monoid hiding ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import NLP.Miniutter.English ((<+>), (<>))
import qualified NLP.Miniutter.English as MU
import qualified Text.Show.Pretty as Show.Pretty

import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point

-- Pretty print and pack the result of @show@.
showT :: Show a => a -> Text
showT x = T.pack $ Show.Pretty.ppShow x

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

-- | Split a messages into chunks that fit in one line.
-- We assume the width of the messages line is the same as of level map.
splitReport :: X -> X -> Report -> Overlay
splitReport w1 w r = Overlay $ splitReportList w1 w r

splitReportList :: X -> X -> Report -> [Text]
splitReportList w1 w r =
  splitText w1 w $ renderReport r

-- | Render a report as a (possibly very long) string.
renderReport :: Report  -> Text
renderReport (Report []) = T.empty
renderReport (Report (xn : xs)) =
  renderReport (Report xs) <+> renderRepetition xn

renderRepetition :: (BS.ByteString, Int) -> Text
renderRepetition (s, 1) = decodeUtf8 s
renderRepetition (s, n) = decodeUtf8 s <> "<x" <> showT n <> ">"

findInReport :: (BS.ByteString -> Bool) -> Report -> Maybe BS.ByteString
findInReport f (Report xns) = find f $ map fst xns

-- | Split a string into lines. Avoids ending the line with a character
-- other than whitespace or punctuation. Space characters are removed
-- from the start, but never from the end of lines. Newlines are respected.
splitText :: X -> X -> Text -> [Text]
splitText w1 w xs =
  case T.lines xs of
    [] -> []
    t : ts -> splitText' w1 w t
              ++ concatMap (splitText' w w . T.stripStart) ts

splitText' :: X -> X -> Text -> [Text]
splitText' w1 w xs
  | w1 >= T.length xs = [xs]  -- no problem, everything fits
  | otherwise =
      let (pre, post) = T.splitAt w1 xs
          (ppre, ppost) = T.break (== ' ') $ T.reverse pre
          testPost = T.stripEnd ppost
      in if T.null testPost
         then pre : splitText w w post
         else T.reverse ppost : splitText w w (T.reverse ppre <> post)

-- | The history of reports.
newtype History = History [Report]
  deriving (Show, Binary)

-- | Empty history of reports.
emptyHistory :: History
emptyHistory = History []

-- | Construct a singleton history of reports.
singletonHistory :: Report -> History
singletonHistory r = addReport r emptyHistory

mergeHistory :: [(Msg, History)] -> History
mergeHistory l =
  let unhist (History x) = x
      f (msg, h) = singletonReport msg : unhist h
  in History $ concatMap f l

-- | Render history as many lines of text, wrapping if necessary.
renderHistory :: History -> Overlay
renderHistory (History h) =
  let w = fst normalLevelBound + 1
  in Overlay $ concatMap (splitReportList w w) h

-- | Add a report to history, handling repetitions.
addReport :: Report -> History -> History
addReport (Report []) h = h
addReport m (History []) = History [m]
addReport (Report m) (History (Report h : hs)) =
  case (reverse m, h) of
    ((s1, n1) : rs, (s2, n2) : hhs) | s1 == s2 ->
      let hist = Report ((s2, n1 + n2) : hhs) : hs
      in History $ if null rs then hist else Report (reverse rs) : hist
    _ -> History $ Report m : Report h : hs

-- | Take the given prefix of reports from a history.
takeHistory :: Int -> History -> History
takeHistory k (History h) = History $ take k h

-- | A series of screen lines that may or may not fit the width nor height
-- of the screen. An overlay may be transformed by adding the first line
-- and/or by splitting into a slideshow of smaller overlays.
newtype Overlay = Overlay {overlay :: [Text]}
  deriving (Show, Eq, Binary)

emptyOverlay :: Overlay
emptyOverlay = Overlay []

truncateToOverlay :: X -> Text -> Overlay
truncateToOverlay lxsize msg = Overlay [truncateMsg lxsize msg]

toOverlay :: [Text] -> Overlay
toOverlay = Overlay

-- | Split an overlay into a slideshow in which each overlay,
-- prefixed by @msg@ and postfixed by @moreMsg@ except for the last one,
-- fits on the screen wrt height (but lines may still be too wide).
splitOverlay :: Y -> Overlay -> Overlay -> Slideshow
splitOverlay lysize (Overlay msg) (Overlay ls) =
  let over = msg ++ ls
  in if length over <= lysize + 2
     then Slideshow [Overlay over]  -- all fits on one screen
     else let (pre, post) = splitAt (lysize + 1) over
              Slideshow slides =
                splitOverlay lysize (Overlay msg) (Overlay post)
          in Slideshow $ (Overlay $ pre ++ [moreMsg]) : slides

-- | A few overlays, displayed one by one upon keypress.
-- When displayed, they are trimmed, not wrapped
-- and any lines below the lower screen edge are not visible.
newtype Slideshow = Slideshow {slideshow :: [Overlay]}
  deriving (Monoid, Show)

-- | Declare the list of raw overlays to be fit for display on the screen.
-- In particular, current @Report@ is eiter empty or unimportant
-- or contained in the overlays and if any vertical or horizontal
-- trimming of the overlays happens, this is intended.
toSlideshow :: [[Text]] -> Slideshow
toSlideshow l = Slideshow $ map Overlay l
