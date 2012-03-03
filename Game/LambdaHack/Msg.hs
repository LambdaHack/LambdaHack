-- | Game messages displayed on top of the screen for the player to read.
module Game.LambdaHack.Msg
  ( Msg, moreMsg, yesnoMsg
  , Report, emptyReport, nullReport, singletonReport, addMsg, splitReport
  , History, emptyHistory, singletonHistory, addReport, renderHistory
  , takeHistory
  , Overlay, stringByLocation
  ) where

import qualified Data.List as L
import Data.Char
import Data.Binary
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap as IM

import Game.LambdaHack.Misc
import Game.LambdaHack.PointXY

-- | The type of a single message.
type Msg  = String

-- | The \"press something to see more\" mark.
moreMsg :: Msg
moreMsg = " --more--  "

-- | The confirmation request message.
yesnoMsg :: Msg
yesnoMsg = " [yn]"

-- | Add spaces at the message end, for display overlayed over the level map.
padMsg :: X -> String -> String
padMsg w xs =
  case L.reverse xs of
    [] -> xs
    ' ' : _ -> xs
    _ | w == length xs -> xs
    reversed -> L.reverse $ ' ' : reversed

-- | The type of a set of messages to show at the screen at once.
newtype Report = Report [(BS.ByteString, Int)]
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

-- | Add message to the end of report.
addMsg :: Report -> Msg -> Report
addMsg r "" = r
addMsg (Report ((x, n) : xns)) y' | x == y =
  Report $ (y, n + 1) : xns
 where y = BS.pack y'
addMsg (Report xns) y = Report $ (BS.pack y, 1) : xns

-- | Split a messages into chunks that fit in one line.
-- We assume the width of the messages line is the same as of level map.
splitReport :: Report -> [String]
splitReport (Report xs) =
  let w = fst normalLevelBound + 1
  in splitString w (renderReport xs)

renderReport :: [(BS.ByteString, Int)] -> String
renderReport [] = ""
renderReport [xn] = renderRepetition xn
renderReport (xn : xs) = renderReport xs ++ " " ++ renderRepetition xn

renderRepetition :: (BS.ByteString, Int) -> String
renderRepetition (s, 1) = BS.unpack s
renderRepetition (s, n) = BS.unpack s ++ "<x" ++ show n ++ ">"

-- | Split a string into lines. Avoids ending the line with a character
-- other than whitespace or punctuation. Space characters are removed
-- from hte start, but never from the end of lines.
splitString :: X -> String -> [String]
splitString w xs = splitString' w $ dropWhile isSpace xs

splitString' :: X -> String -> [String]
splitString' w xs
  | w <= 0 = [xs]  -- border case, we cannot make progress
  | w >= length xs = [xs]  -- no problem, everything fits
  | otherwise =
      let (pre, post) = splitAt w xs
          (ppre, ppost) = break (`elem` " .,:;!?") $ reverse pre
          testPost = dropWhile isSpace ppost
      in if L.null testPost
         then pre : splitString w post
         else reverse ppost : splitString w (reverse ppre ++ post)

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

renderHistory :: History -> Overlay
renderHistory (History h) = L.concatMap splitReport h

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

type Overlay = [String]

splitOverlay :: Y -> Overlay -> [[String]]
splitOverlay _ [] = []  -- nothing to print over the level area
splitOverlay lysize ls | length ls <= lysize = [ls]  -- all fits on one screen
splitOverlay lysize ls = let (pre, post) = splitAt (lysize - 1) ls
                         in (pre ++ [moreMsg]) : splitOverlay lysize post

-- | Returns a function that looks up the characters in the
-- string by location. Takes the height of the display plus
-- the string. Returns also the message to print at the top
-- and number of screens required to display all of the string.
stringByLocation :: X -> Y -> Overlay -> (String, Int, (X, Y) -> Maybe Char)
stringByLocation _ _ [] = ("", 0, const Nothing)
stringByLocation lxsize lysize (msgTop : ls) =
  let over = splitOverlay lysize $ map (padMsg lxsize) ls
      m  = IM.fromDistinctAscList $
             zip [0..] (L.map (IM.fromList . zip [0..]) (concat over))
      ns = length over
  in (msgTop, ns, \ (x, y) -> IM.lookup y m >>= \ n -> IM.lookup x n)
