{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Game messages displayed on top of the screen for the player to read.
module Game.LambdaHack.Msg
  ( makePhrase, makeSentence
  , Msg, (<>), (<+>), showT, moreMsg, yesnoMsg, padMsg
  , Report, emptyReport, nullReport, singletonReport, addMsg
  , splitReport, renderReport
  , History, emptyHistory, singletonHistory, addReport, renderHistory
  , takeHistory
  , Overlay, splitOverlay, stringByLocation
  ) where

import Data.Binary
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import NLP.Miniutter.English (showT, (<+>), (<>))
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Misc
import Game.LambdaHack.PointXY

-- | Re-exported English phrase creation functions, applied to default
-- irregular word sets.
makePhrase, makeSentence :: [MU.Part] -> Text
makePhrase = MU.makePhrase MU.defIrregular
makeSentence = MU.makeSentence MU.defIrregular

-- | The type of a single message.
type Msg = Text

instance Binary Text where
   put = put . encodeUtf8
   get = decodeUtf8 `fmap` get

-- | The \"press something to see more\" mark.
moreMsg :: Msg
moreMsg = "--more--  "

-- | The confirmation request message.
yesnoMsg :: Msg
yesnoMsg = "[yn]"

-- | Add spaces at the message end, for display overlayed over the level map.
-- Also trims (does not wrap!) too long lines.
padMsg :: X -> Text -> Text
padMsg w xs =
  let len = T.length xs
  in case compare w len of
       LT -> T.snoc (T.take (w - 1) xs) '$'
       EQ -> xs
       GT -> if T.null xs || T.last xs == ' '
             then xs
             else T.snoc xs ' '

-- | The type of a set of messages to show at the screen at once.
newtype Report = Report [(BS.ByteString, Int)]
  deriving Show

instance Binary Report where
  put (Report x) = put x
  get = fmap Report get

-- | Empty set of messages.
emptyReport :: Report
emptyReport = Report []

-- | Test if the set of messages is empty.
nullReport :: Report -> Bool
nullReport (Report l) = null l

-- | Construct a singleton set of messages.
singletonReport :: Msg -> Report
singletonReport m = addMsg emptyReport m

-- | Add message to the end of report.
addMsg :: Report -> Msg -> Report
addMsg r m | T.null m = r
addMsg (Report ((x, n) : xns)) y' | x == y =
  Report $ (y, n + 1) : xns
 where y = encodeUtf8 y'
addMsg (Report xns) y = Report $ (encodeUtf8 y, 1) : xns

-- | Split a messages into chunks that fit in one line.
-- We assume the width of the messages line is the same as of level map.
splitReport :: Report -> [Text]
splitReport r =
  let w = fst normalLevelBound + 1
  in splitText w $ renderReport r

-- | Render a report as a (possibly very long) string.
renderReport ::Report  -> Text
renderReport (Report []) = T.empty
renderReport (Report (xn : xs)) =
  renderReport (Report xs) <+> renderRepetition xn

renderRepetition :: (BS.ByteString, Int) -> Text
renderRepetition (s, 1) = decodeUtf8 s
renderRepetition (s, n) = decodeUtf8 s <> "<x" <> showT n <> ">"

-- | Split a string into lines. Avoids ending the line with a character
-- other than whitespace or punctuation. Space characters are removed
-- from hte start, but never from the end of lines.
splitText :: X -> Text -> [Text]
splitText w xs = splitText' w $ T.dropWhile isSpace xs

splitText' :: X -> Text -> [Text]
splitText' w xs
  | w <= 0 = [xs]  -- border case, we cannot make progress
  | w >= T.length xs = [xs]  -- no problem, everything fits
  | otherwise =
      let (pre, post) = T.splitAt w xs
          (ppre, ppost) = T.break (`elem` " .,:;!?") $ T.reverse pre
          testPost = T.dropWhile isSpace ppost
      in if T.null testPost
         then pre : splitText w post
         else T.reverse ppost : splitText w (T.reverse ppre <> post)

-- | The history of reports.
newtype History = History [Report]
  deriving Show

instance Binary History where
  put (History x) = put x
  get = fmap History get

-- | Empty history of reports.
emptyHistory :: History
emptyHistory = History []

-- | Construct a singleton history of reports.
singletonHistory :: Report -> History
singletonHistory r = addReport r emptyHistory

-- | Render history as many lines of text, wrapping if necessary.
renderHistory :: History -> Overlay
renderHistory (History h) = L.concatMap splitReport h

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

-- | A screenful of text lines. When displayed, they are trimmed, not wrapped
-- and any lines below the lower screen edge are not visible.
type Overlay = [Text]

-- | Split an overlay into overlays that fit on the screen.
splitOverlay :: Y -> Overlay -> [Overlay]
splitOverlay _ [] = []  -- nothing to print over the level area
splitOverlay lysize ls | length ls <= lysize = [ls]  -- all fits on one screen
splitOverlay lysize ls = let (pre, post) = splitAt (lysize - 1) ls
                         in pre : splitOverlay lysize post

-- | Returns a function that looks up the characters in the
-- string by location. Takes the width and height of the display plus
-- the string. Returns also the message to print at the top and bottom.
stringByLocation :: X -> Y -> Overlay
                 -> (Text, PointXY -> Maybe Char, Maybe Text)
stringByLocation _ _ [] = (T.empty, const Nothing, Nothing)
stringByLocation lxsize lysize (msgTop : ls) =
  let over = map (padMsg lxsize) $ take lysize ls
      m  = IM.fromDistinctAscList $
             zip [0..] (L.map (IM.fromList . zip [0..] . T.unpack) over)
      msgBottom = case drop lysize ls of
                  [] -> Nothing
                  s : _ -> Just s
  in (msgTop,
      \ (PointXY (x, y)) -> IM.lookup y m >>= \ n -> IM.lookup x n,
      msgBottom)
