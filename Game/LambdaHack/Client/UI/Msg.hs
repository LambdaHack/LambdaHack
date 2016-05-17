{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Game messages displayed on top of the screen for the player to read.
module Game.LambdaHack.Client.UI.Msg
  ( -- * AttrLine and Msg
    Msg, toMsg, toPrompt
    -- * Report
  , Report, emptyReport, nullReport, singletonReport, snocReport, consReport
  , splitReport, truncateReport, findInReport, lastMsgOfReport
    -- * History
  , History, emptyHistory, addReport, lengthHistory, linesHistory
  , lastReportOfHistory, renderHistory, splitReportForHistory
  , splitOverlayOKX
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import Data.Binary.Orphans ()
import Data.Char
import qualified Data.Text as T
import GHC.Generics (Generic)

import Game.LambdaHack.Client.UI.Overlay
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.RingBuffer as RB
import Game.LambdaHack.Common.Time

-- * Msg

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

lastMsgOfReport :: Report -> (AttrLine, Report)
lastMsgOfReport (Report rep) = case rep of
  [] -> ([], Report [])
  (lmsg, 1) : repRest -> (msgLine lmsg, Report repRest)
  (lmsg, n) : repRest -> (msgLine lmsg, Report $ (lmsg, n - 1) : repRest)

-- | Split a messages into chunks that fit in one line.
-- We assume the width of the messages line is the same as of level map.
splitReport :: X -> Report -> Overlay
splitReport w r = toOverlayRaw $ splitAttrLine w $ renderReport r

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

truncateReport :: Report -> Overlay
truncateReport r = toOverlayRaw [renderReport r]

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

-- TODO: assert that ov0 nonempty and perhaps that kxs0 not too short
-- (or should we just keep the rest of the overlay unclickable?)
splitOverlayOKX :: X -> Y -> Report -> OKX -> [OKX]
splitOverlayOKX lxsize yspace report (ov0, kxs0) =
  assert (not $ nullReport report) $
  let msg = overlay $ splitReport lxsize report
      msg0 = if yspace - length msg - 1 <= 0  -- all space taken by @msg@
             then [renderReport report]  -- will display "$" (unless has EOLs)
             else msg
      ls0 = overlay ov0
      len = length msg0
      renumber y (km, (_, x1, x2)) = (km, (y, x1, x2))
      zipRenumber y = zipWith renumber [y..]
      splitO ls kxs =
        let (pre, post) = splitAt (yspace - 1) $ msg0 ++ ls
        in if null post
           then -- all fits on one screen
                let bottomMsgAttr = toAttrLine $
                      T.replicate (length $ last pre) " "
                in [( toOverlayRaw $ pre ++ [bottomMsgAttr]
                    , zipRenumber len kxs )]
           else let (preX, postX) = splitAt (yspace - len - 1) kxs
                    rest = splitO post postX
                in ( toOverlayRaw (pre ++ [moreMsg])
                   , zipRenumber len preX )
                   : rest
      okxs = splitO ls0 kxs0
  in assert (not $ null okxs) okxs
