{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Game messages displayed on top of the screen for the player to read.
module Game.LambdaHack.Client.UI.Msg
  ( -- * AttrLine
    AttrLine, toAttrLine, splitAttrLine
    -- * Msg
  , Msg, toMsg, toPrompt
    -- * Report
  , Report, emptyReport, nullReport, singletonReport, snocReport, consReport
  , renderReport, findInReport, lastMsgOfReport
    -- * History
  , History, emptyHistory, addReport, lengthHistory, linesHistory
  , lastReportOfHistory, renderHistory, splitReportForHistory
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import Data.Binary.Orphans ()
import Data.Char
import qualified Data.Text as T
import GHC.Generics (Generic)

import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.RingBuffer as RB
import Game.LambdaHack.Common.Time

-- * AttrLine

type AttrLine = [Color.AttrChar]

toAttrLine :: Text -> AttrLine
toAttrLine = map (Color.AttrChar Color.defAttr) . T.unpack

-- | Split a string into lines. Avoids ending the line with a character
-- other than whitespace or punctuation. Space characters are removed
-- from the start, but never from the end of lines. Newlines are respected.
splitAttrLine :: X -> AttrLine -> [AttrLine]
splitAttrLine w l =
  concatMap (splitAttrPhrase w . dropWhile (isSpace . Color.acChar))
  $ linesAttr l

linesAttr :: AttrLine -> [AttrLine]
linesAttr l | null l = []
            | otherwise = h : if null t then [] else linesAttr (tail t)
 where (h, t) = span ((/= '\n') . Color.acChar) l

splitAttrPhrase :: X -> AttrLine -> [AttrLine]
splitAttrPhrase w xs
  | w >= length xs = [xs]  -- no problem, everything fits
  | otherwise =
      let (pre, post) = splitAt w xs
          (ppre, ppost) = break ((== ' ') . Color.acChar) $ reverse pre
          testPost = dropWhileEnd (isSpace . Color.acChar) ppost
      in if null testPost
         then pre : splitAttrPhrase w post
         else reverse ppost : splitAttrPhrase w (reverse ppre ++ post)

-- * Msg

-- | The type of a single game message.
data Msg = Msg
  { msgLine :: AttrLine  -- ^ the colours and characters of the message
  , msgHist :: Bool      -- ^ whether the message should be recorded in history
  }
  deriving (Show, Eq, Generic)

instance Binary Msg

toMsg :: AttrLine -> Msg
toMsg l = Msg { msgLine = l
              , msgHist = True }

toPrompt :: AttrLine -> Msg
toPrompt l = Msg { msgLine = l
                 , msgHist = False }

-- * Report

-- | The set of messages, with repetitions, to show at the screen at once.
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
singletonReport = snocReport emptyReport

-- | Add message to the end of report. Deletes old prompt messages.
snocReport :: Report -> Msg -> Report
snocReport (Report r) y =
  let scrubPrompts = filter (msgHist . fst)
  in case r of
    xns | null $ msgLine y -> Report $ scrubPrompts xns
    (x, n) : xns | x == y -> Report $ (x, n + 1) : scrubPrompts xns
    xns -> Report $ (y, 1) : scrubPrompts xns

consReport :: Msg -> Report -> Report
consReport m (Report ms) =
  let Report ms2 = snocReport (Report $ reverse ms) m
  in Report $ reverse ms2

infixr 6 <+:>  -- matches Monoid.<>
(<+:>) :: AttrLine -> AttrLine -> AttrLine
(<+:>) [] l2 = l2
(<+:>) l1 [] = l1
(<+:>) l1 l2 = l1 ++ toAttrLine " " ++ l2

-- | Render a report as a (possibly very long) 'AttrLine'.
renderReport :: Report  -> AttrLine
renderReport (Report []) = []
renderReport (Report (x : xs)) =
  renderReport (Report xs) <+:> renderRepetition x

renderRepetition :: (Msg, Int) -> AttrLine
renderRepetition (s, 1) = msgLine s
renderRepetition (s, n) = msgLine s ++ toAttrLine ("<x" <> tshow n <> ">")

findInReport :: (AttrLine -> Bool) -> Report -> Maybe Msg
findInReport f (Report xns) = find (f . msgLine) $ map fst xns

lastMsgOfReport :: Report -> (AttrLine, Report)
lastMsgOfReport (Report rep) = case rep of
  [] -> ([], Report [])
  (lmsg, 1) : repRest -> (msgLine lmsg, Report repRest)
  (lmsg, n) : repRest -> (msgLine lmsg, Report $ (lmsg, n - 1) : repRest)

-- * History

-- | The history of reports. This is a ring buffer of the given length
newtype History = History (RB.RingBuffer (Time, Report))
  deriving (Show, Binary)

-- | Empty history of reports of the given maximal length.
emptyHistory :: Int -> History
emptyHistory size = History $ RB.empty size (timeZero, Report [])

-- | Add a report to history, handling repetitions.
addReport :: History -> Time -> Report -> History
addReport !(History rb) !time !(Report m') =
  let rep@(Report m) = Report $ filter (msgHist . fst) m'
  in case RB.uncons rb of
    _ | null m -> History rb
    Nothing -> History $ RB.cons (time, rep) rb
    Just ((oldTime, Report h), hRest) ->
      case (reverse m, h) of
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
