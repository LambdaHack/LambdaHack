module Game.LambdaHack.Message where

import qualified Data.List as L
import Data.Char

type Message = String

more :: Message
more = " --more--  "

yesno :: Message
yesno = " [yn]"

addMsg :: Message -> Message -> Message
addMsg [] x  = x
addMsg xs [] = xs
addMsg xs x  = xs ++ " " ++ x

splitMsg :: Int -> Message -> [String]
splitMsg w xs
  | w <= m = [xs]   -- border case, we cannot make progress
  | l <= w = [xs]   -- no problem, everything fits
  | otherwise =
      let (pre, post) = splitAt (w - m) xs
          (ppre, ppost) = break (`L.elem` " .,:!;") $ reverse pre
          rpost = dropWhile isSpace ppost
      in  if L.null rpost
          then pre : splitMsg w post
          else reverse rpost : splitMsg w (reverse ppre ++ post)
  where
    m = length more
    l = length xs
