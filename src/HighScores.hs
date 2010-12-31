module HighScores where

import System.Directory
import Control.Exception as E hiding (handle)
import Text.Printf
import System.Time

import Data.Binary
import Data.List as L
import Data.Maybe

import File
import Dungeon
import qualified Config
import qualified Data.ConfigFile

-- | A single score.
-- TODO: add hero's name, exp and level, cause of death, user number/name.
-- Note: I tried using Date.Time, but got all kinds of problems,
-- including build problems and opaque types that make serialization difficult,
-- and I couldn't use Datetime because it needs old base (and is under GPL).
-- TODO: When we finally move to Date.Time, let's take timezone into account.
data ScoreRecord = ScoreRecord
                     { points  :: Int,
                       negTurn :: Int,
                       date    :: ClockTime,
                       current :: Int,
                       killed  :: Bool,
                       victor  :: Bool}
  deriving (Eq, Ord)

instance Binary ClockTime where
  put (TOD s p) =
    do
      put s
      put p
  get =
    do
      s <- get
      p <- get
      return (TOD s p)

instance Binary ScoreRecord where
  put (ScoreRecord points negTurn date current killed victor) =
    do
      put points
      put negTurn
      put date
      put current
      put killed
      put victor
  get =
    do
      points <- get
      negTurn <- get
      date <- get
      current <- get
      killed <- get
      victor <- get
      return (ScoreRecord points negTurn date current killed victor)

-- | Show a single high score.
showScore :: (Int, ScoreRecord) -> String
showScore (pos, score) =
  let won  = if victor score
             then "emerged victorious"
             else "is camping on level " ++ show (current score) ++ ","
      died = if killed score
             then "perished on level " ++ show (current score) ++ ","
             else won
      time = calendarTimeToString . toUTCTime . date $ score
      big  = "                                                 "
      lil  = "              "
  in
   printf "%s\n%4d. %6d  This hero %s after %d steps  \n%son %s.  \n"
     big pos (points score) died (- (negTurn score)) lil time

-- | The list of scores, in decreasing order.
type ScoreTable = [ScoreRecord]

-- | Empty score table
empty :: ScoreTable
empty = []

-- | Name of the high scores file.
file :: Data.ConfigFile.ConfigParser -> IO String
file config = Config.getFile config "LambdaHack.scores" "files" "highscores"

-- | We save a simple serialized version of the high scores table.
-- The 'False' is used only as an EOF marker.
save :: Data.ConfigFile.ConfigParser -> ScoreTable -> IO ()
save config scores =
  do
    f <- file config
    E.catch (removeFile f) (\ e -> case e :: IOException of _ -> return ())
    encodeCompressedFile f (scores, False)

-- | Read the high scores table. Return the empty table if no file.
restore :: Data.ConfigFile.ConfigParser -> IO ScoreTable
restore config =
  E.catch (do
             f <- file config
             (x, z) <- strictDecodeCompressedFile f
             (z :: Bool) `seq` return x)
          (\ e -> case e :: IOException of
                    _ -> return [])

-- | Insert a new score into the table, Return new table and the position.
insertPos :: ScoreRecord -> ScoreTable -> (ScoreTable, Int)
insertPos s h =
  let (prefix, suffix) = L.span (\ x -> x > s) h in
  (prefix ++ [s] ++ suffix, L.length prefix + 1)

-- | Show a screenful of the high scores table.
-- Parameter height is the number of (3-line) scores to be shown.
showTable :: ScoreTable -> Int -> Int -> String
showTable h start height =
  let zipped    = zip [1..] h
      screenful = take height . drop (start - 1) $ zipped
  in
   L.concatMap showScore screenful

-- | Produces a couple of renderings of the high scores table.
slideshow :: Int -> ScoreTable -> Int -> [String]
slideshow pos h height =
  if pos <= height
  then [showTable h 1 height]
  else [showTable h 1 height,
        showTable h (max (height + 1) (pos - height `div` 2)) height]

-- | Take care of a new score, return a list of messages to display.
register :: Data.ConfigFile.ConfigParser -> Bool -> ScoreRecord ->
            IO (String, [String])
register config write s =
  do
    h <- restore config
    let (h', pos) = insertPos s h
        (lines, _) = normalLevelSize
        height = lines `div` 3
        (msgCurrent, msgUnless) =
          if killed s
          then (" short-lived", " (score halved)")
          else if victor s
               then (" glorious",
                     if pos <= height then " among the greatest heroes" else "")
               else (" current", " (unless you are slain)")
        msg = printf "Your%s exploits award you place >> %d <<%s." msgCurrent pos msgUnless
    if write then save config h' else return ()
    return (msg, slideshow pos h' height)
