{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Game messages displayed on top of the screen for the player to read.
module Game.LambdaHack.Client.UI.Msg
  ( -- * AttrLine and Msg
    tmoreMsg, tendMsg, tyesnoMsg
  , AttrLine, (<+:>), moreMsg, endMsg, yesnoMsg, toAttrLine, splitAttrLine
  , Msg(..), toMsg, toPrompt
    -- * Report
  , Report, emptyReport, nullReport, singletonReport, addMsg, prependMsg
  , renderReport, findInReport, lastMsgOfReport
    -- * History
  , History, emptyHistory, addReport, lengthHistory, linesHistory
  , lastReportOfHistory, renderHistory, splitReportForHistory
  )
  where

import Prelude ()
import Prelude.Compat

import Control.Exception.Assert.Sugar
import Data.Binary
import Data.Binary.Orphans ()
import Data.Char
import Data.List (dropWhileEnd, find)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Game.LambdaHack.Common.Color as Color
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.RingBuffer as RB
import Game.LambdaHack.Common.Time

-- * AttrLine and Msg

-- tmp, until help in colour
tmoreMsg :: Text
tmoreMsg = "--more--  "

-- tmp, until help in colour
tendMsg :: Text
tendMsg = "--end--  "

tyesnoMsg :: Text
tyesnoMsg = "[yn]"

type AttrLine = [Color.AttrChar]

toAttrLine :: Text -> AttrLine
toAttrLine = map (Color.AttrChar Color.defAttr) . T.unpack

-- TODO: make a class, for monoids with neutral elements
infixr 6 <+:>  -- matches Monoid.<>
(<+:>) :: AttrLine -> AttrLine -> AttrLine
(<+:>) [] l2 = l2
(<+:>) l1 [] = l1
(<+:>) l1 l2 = l1 ++ toAttrLine " " ++ l2

-- | The \"press something to see more\" mark.
moreMsg :: AttrLine
moreMsg = toAttrLine "--more--  "

-- | The \"end of screenfuls of text\" mark.
endMsg :: AttrLine
endMsg = toAttrLine"--end--  "

-- | The confirmation request message.
yesnoMsg :: AttrLine
yesnoMsg = toAttrLine "[yn]"

-- | Split a string into lines. Avoids ending the line with a character
-- other than whitespace or punctuation. Space characters are removed
-- from the start, but never from the end of lines. Newlines are respected.
splitAttrLine :: X -> AttrLine -> [AttrLine]
splitAttrLine w l =
  concatMap (splitAttrLine' w . dropWhile (isSpace . Color.acChar))
  $ linesAttr l

linesAttr :: AttrLine -> [AttrLine]
linesAttr l | null l = []
            | otherwise = h : if null t
                              then []
                              else linesAttr (tail t)
 where (h, t) = span ((/= '\n') . Color.acChar) l

splitAttrLine' :: X -> AttrLine -> [AttrLine]
splitAttrLine' w xs
  | w >= length xs = [xs]  -- no problem, everything fits
  | otherwise =
      let (pre, post) = splitAt w xs
          (ppre, ppost) = break ((== ' ') . Color.acChar) $ reverse pre
          testPost = dropWhileEnd (isSpace . Color.acChar) ppost
      in if null testPost
         then pre : splitAttrLine w post
         else reverse ppost : splitAttrLine w (reverse ppre ++ post)

-- | The type of a single message.
data Msg = Msg
  { msgLine :: AttrLine
  , msgHist :: Bool }
  deriving (Show, Eq, Generic)

instance Binary Msg

toMsg :: Text -> Msg
toMsg t = Msg { msgLine = toAttrLine t
              , msgHist = True }

toPrompt :: AttrLine -> Msg
toPrompt l = Msg { msgLine = l
                 , msgHist = False }

-- * Report

-- | The type of a set of messages to show at the screen at once.
newtype Report = Report [(Msg, Int)]
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
addMsg r m | null $ msgLine m = r
addMsg (Report ((x, n) : xns)) y | x == y = Report $ (x, n + 1) : xns
addMsg (Report xns) y = Report $ (y, 1) : xns

prependMsg :: Msg -> Report -> Report
prependMsg m (Report ms) =
  let Report ms2 = addMsg (Report $ reverse ms) m
  in Report $ reverse ms2

-- | Render a report as a (possibly very long) 'AttrLine'.
renderReport :: Report  -> AttrLine
renderReport (Report []) = []
renderReport (Report (xn : xs)) =
  renderReport (Report xs) <+:> renderRepetition xn

renderRepetition :: (Msg, Int) -> AttrLine
renderRepetition (s, 1) = msgLine s
renderRepetition (s, n) = msgLine s ++ toAttrLine ("<x" <> tshow n <> ">")

findInReport :: (AttrLine -> Bool) -> Report -> Maybe Msg
findInReport f (Report xns) = find (f . msgLine) $ map fst xns

lastMsgOfReport :: Report -> (Msg, Report)
lastMsgOfReport (Report rep) = case rep of
  [] -> assert `failure` rep
  (lmsg, 1) : repRest -> (lmsg, Report repRest)
  (lmsg, n) : repRest -> (lmsg, Report $ (lmsg, n - 1) : repRest)

-- * History

-- | The history of reports. This is a ring buffer of the given length
newtype History = History (RB.RingBuffer (Time, Report))
  deriving (Show, Binary)

-- | Empty history of reports of the given maximal length.
emptyHistory :: Int -> History
emptyHistory size = History $ RB.empty size (timeZero, Report [])

-- | Add a report to history, handling repetitions.
addReport :: History -> Time -> Report -> History
addReport !(History rb) !time !rep@(Report m) =
  case RB.uncons rb of
    Nothing -> History $ RB.cons (time, rep) rb
    Just ((oldTime, Report h), hRest) ->
      case (reverse (filter (msgHist . fst) m), h) of
        ([], _) -> History rb
        ((s1, n1) : rs, (s2, n2) : hhs) | s1 == s2 ->
          let hist = RB.cons (oldTime, Report ((s2, n1 + n2) : hhs)) hRest
          in History $ if null rs
                       then hist
                       else RB.cons (time, Report (reverse rs)) hist
        _ -> History $ RB.cons (time, rep) rb

lengthHistory :: History -> Int
lengthHistory (History rs) = RB.rbLength rs

linesHistory :: History -> [(Time, Report)]
linesHistory (History rb) = RB.toList rb

lastReportOfHistory :: History -> Maybe Report
lastReportOfHistory (History rb) = snd . fst <$> RB.uncons rb

splitReportForHistory :: X -> (Time, Report) -> (AttrLine, [AttrLine])
splitReportForHistory w (time, r) =
  -- TODO: display time fractions with granularity enough to differ
  -- from previous and next report, if possible.
  -- or perhaps here display up to 4 decimal points
  let tturns = toAttrLine $ tshow $ time `timeFitUp` timeTurn
      ts = splitAttrLine (w - 1) $ tturns ++ toAttrLine ": " ++ renderReport r
      rep = case ts of
        [] -> []
        hd : tl -> hd : map (toAttrLine " " ++) tl
  in (tturns, rep)

-- | Render history as many lines of text, wrapping if necessary.
renderHistory :: History -> [AttrLine]
renderHistory (History rb) =
  let truncateForHistory (time, r) =
        -- TODO: display time fractions with granularity enough to differ
        -- from previous and next report, if possible
        let turns = time `timeFitUp` timeTurn
        in toAttrLine (tshow turns <> ": ") ++ renderReport r
  in map truncateForHistory $ RB.toList rb
