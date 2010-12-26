module HighScores where

import System.Directory
import Control.Exception as E hiding (handle)
import Text.Printf
import System.Time

import Data.Binary
import Data.List as L

import File
import Dungeon

-- | A single score.
-- TODO: add characte name, character exp and level, level of death,
-- deepest level visited, cause of death, user number/name.
-- Note: I tried using Date.Time, but got all kinds of problems,
-- including build problems and opaque types that make serialization difficult,
-- and I couldn't use Datetime because it needs old base (and is under GPL).
-- TODO: When we finally move to Date.Time, let's take timezone into account.
data ScoreRecord = ScoreRecord
                     { points  :: Int,
                       killed  :: Bool,
                       victor  :: Bool,
                       date    :: ClockTime,
                       turn    :: Int }
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
  put (ScoreRecord points killed victor date turn) =
    do
      put points
      put killed
      put victor
      put date
      put turn
  get =
    do
      points <- get
      killed <- get
      victor <- get
      date <- get
      turn <- get
      return (ScoreRecord points killed victor date turn)

-- | Show a sinngle high score.
showScore :: (Int, ScoreRecord) -> String
showScore (pos, score) =
  let won  = if killed score
             then "was killed"
             else if victor score
                  then "has won"
                  else "took a break"
      time = calendarTimeToString . toUTCTime . date $ score
      big  = "                                                 "
      lil  = "              "
  in
   printf "%s\n%4d. %6d  This hero %s after %d moves  \n%son %s.  \n"
     big pos (points score) won (turn score) lil time

-- | The list of scores, in decreasing order.
type ScoreTable = [ScoreRecord]

-- | Empty score table
empty :: ScoreTable
empty = []

-- | Name of the high scores file. TODO: place in ~/.LambdaHack/ (Windows?)
-- and eventually, optionally, in /var/games.
file = "LambdaHack.scores"

-- | We save a simple serialized version of the high scores table.
-- The 'False' is used only as an EOF marker.
save :: ScoreTable -> IO ()
save scores = encodeCompressedFile file (scores, False)

-- | Read the high scores table. Return the empty table if no file.
-- TODO: fail if the ioe_type of exception is different than NoSuchThing
restore :: IO ScoreTable
restore =
  E.catch (do
             (x, z) <- strictDecodeCompressedFile file
             (z :: Bool) `seq` removeFile file
             return x)
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
        showTable h (max 1 (pos - height `div` 2)) height]

-- | Take care of a new score, return a list of messages to display.
register :: ScoreRecord -> IO (String, [String])
register s =
  do
    h <- restore
    let (h', pos) = insertPos s h
        msg = "Your exploits award you place >> "
              ++ show pos ++ " << on the high scores table."
        (lines, _) = normalLevelSize
    save h'
    return (msg, slideshow pos h' (lines `div` 3))
