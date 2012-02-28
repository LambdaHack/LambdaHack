-- | Game messages displayed on top of the screen for the player to read.
module Game.LambdaHack.Msg
  ( Msg, more, msgEnd, yesno
  , Report, singletonMsg, addMsg, splitMsg, padMsg
  , History, renderHistory, addReport
  ) where

import qualified Data.List as L
import Data.Char

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

-- | The type of a set of messages to show at the screen at once.
type Report = [(Msg, Int)]

singletonMsg :: Msg -> Report
singletonMsg "" = []
singletonMsg y = [(y, 1)]

-- | Append two messages.
addMsg :: Report -> Msg -> Report
addMsg xns "" = xns
addMsg ((x, n) : xns) y | x == y = (y, n + 1) : xns
addMsg xns y = (y, 1) : xns

-- | Split a messages into chunks that fit in one line.
splitMsg :: Int -> Int -> Report -> [String]
splitMsg w m xs = splitString w (renderMsg xs) m

renderMsg :: Report -> String
renderMsg [] = ""
renderMsg [xn] = renderRepetition xn
renderMsg (xn : xs) = renderMsg xs ++ " " ++ renderRepetition xn

renderRepetition :: (Msg, Int) -> String
renderRepetition (x, 1) = x
renderRepetition (x, n) = x ++ "<x" ++ show n ++ ">"

splitString :: Int -> Msg -> Int -> [String]
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

-- | Add spaces at the message end, for display overlayed over the level map.
padMsg :: Int -> String -> String
padMsg w xs =
  case L.reverse xs of
    [] -> xs
    ' ' : _ -> xs
    _ | w == length xs -> xs
    reversed -> L.reverse $ ' ' : reversed

-- | The history of reports.
type History = [Report]

renderHistory :: History -> String
renderHistory h =
  let w = fst normalLevelBound + 1
  in unlines $ L.concatMap (L.map (padMsg w) . splitMsg w 0) h

addReport :: Report -> History -> History
addReport m (h : hs) =
  case (reverse m, h) of
    ((s1, n1) : rs, (s2, n2) : hhs) | s1 == s2 ->
      let hist = ((s2, n1 + n2) : hhs) : hs
      in if null rs then hist else reverse rs : hist
    _ -> m : h : hs
addReport m h = m : h
