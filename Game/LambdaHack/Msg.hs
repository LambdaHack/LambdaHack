-- | Game messages displayed on top of the screen for the player to read.
module Game.LambdaHack.Msg
  ( Msg, more, msgEnd, yesno, padMsg
  , Report, emptyReport, nullReport, singletonReport, addMsg, splitReport
  , History, emptyHistory, singletonHistory, addReport, renderHistory
  , takeHistory
  ) where

import qualified Data.List as L
import Data.Char
import Data.Binary

import Game.LambdaHack.Misc

-- | The type of a single message.
type Msg  = String

-- TODO: do not record these in history. Perhaps implement them as
-- a 'sprompt' flag in the game state that requires a keypress to be cleared.
-- | The \"press something to see more\" mark.
more :: Msg
more = " --more--  "

-- | The \"the end of overlays or messages\" mark.
msgEnd :: Msg
msgEnd = " --end--  "

-- | The confirmation request message.
yesno :: Msg
yesno = " [yn]"

-- | Add spaces at the message end, for display overlayed over the level map.
padMsg :: Int -> String -> String
padMsg w xs =
  case L.reverse xs of
    [] -> xs
    ' ' : _ -> xs
    _ | w == length xs -> xs
    reversed -> L.reverse $ ' ' : reversed

-- | The type of a set of messages to show at the screen at once.
newtype Report = Report [(Msg, Int)]
  deriving Show

instance Binary Report where
  put (Report x) = put x
  get = fmap Report get

emptyReport :: Report
emptyReport = Report []

nullReport :: Report -> Bool
nullReport (Report l) = null l

singletonReport :: Msg -> Report
singletonReport m = addMsg emptyReport m

-- | Append two messages.
addMsg :: Report -> Msg -> Report
addMsg r "" = r
addMsg (Report ((x, n) : xns)) y | x == y = Report $ (y, n + 1) : xns
addMsg (Report xns) y = Report $ (y, 1) : xns

-- | Split a messages into chunks that fit in one line.
splitReport :: Int -> Int -> Report -> [String]
splitReport w m (Report xs) = splitString w (renderReport xs) m

renderReport :: [(Msg, Int)] -> String
renderReport [] = ""
renderReport [xn] = renderRepetition xn
renderReport (xn : xs) = renderReport xs ++ " " ++ renderRepetition xn

renderRepetition :: (Msg, Int) -> String
renderRepetition (x, 1) = x
renderRepetition (x, n) = x ++ "<x" ++ show n ++ ">"

splitString :: Int -> String -> Int -> [String]
splitString w xs m
  | w <= m = [xs]   -- border case, we cannot make progress
  | w >= length xs = [xs]   -- no problem, everything fits
  | otherwise =
      let (pre, post) = splitAt (w - m) xs
          (ppre, ppost) = break (`elem` " .,:;!?") $ reverse pre
          rpost = dropWhile isSpace ppost
      in if L.null rpost
         then pre : splitString w post m
         else reverse rpost : splitString w (reverse ppre ++ post) m

-- | The history of reports.
newtype History = History [Report]
  deriving Show

instance Binary History where
  put (History x) = put x
  get = fmap History get

emptyHistory :: History
emptyHistory = History []

singletonHistory :: Report -> History
singletonHistory r = addReport r emptyHistory

renderHistory :: History -> String
renderHistory (History h) =
  let w = fst normalLevelBound + 1
  in unlines $ L.concatMap (L.map (padMsg w) . splitReport w 0) h

addReport :: Report -> History -> History
addReport (Report []) h = h
addReport m (History []) = History [m]
addReport (Report m) (History (Report h : hs)) =
  case (reverse m, h) of
    ((s1, n1) : rs, (s2, n2) : hhs) | s1 == s2 ->
      let hist = Report ((s2, n1 + n2) : hhs) : hs
      in History $ if null rs then hist else Report (reverse rs) : hist
    _ -> History $ Report m : Report h : hs

takeHistory :: Int -> History -> History
takeHistory k (History h) = History $ take k h
