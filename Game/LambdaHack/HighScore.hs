{-# LANGUAGE OverloadedStrings #-}
-- | High score table operations.
module Game.LambdaHack.HighScore
  ( ScoreTable, register, slideshow
  ) where

import Control.Monad
import Data.Binary
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU
import System.Directory
import System.Time
import Text.Printf

import Game.LambdaHack.Faction
import Game.LambdaHack.Misc
import Game.LambdaHack.Msg
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.File

-- | A single score record. Records are ordered in the highscore table,
-- from the best to the worst, in lexicographic ordering wrt the fields below.
data ScoreRecord = ScoreRecord
  { points  :: !Int        -- ^ the score
  , negTime :: !Time       -- ^ game time spent (negated, so less better)
  , date    :: !ClockTime  -- ^ date of the last game interruption
  , status  :: !Status     -- ^ reason of the game interruption
  }
  deriving (Eq, Ord)

-- | Show a single high score, from the given ranking in the high score table.
showScore :: (Int, ScoreRecord) -> [Text]
showScore (pos, score) =
  let died = case status score of
        Killed lvl -> "perished on level " ++ show (fromEnum lvl) ++ ","
        Camping -> "is camping somewhere,"
        Victor -> "emerged victorious"
        Restart -> "resigned prematurely"
      curDate = calendarTimeToString . toUTCTime . date $ score
      big, lil :: String
      big = "                                                 "
      lil = "              "
      turns = - (negTime score `timeFit` timeTurn)
     -- TODO: the spaces at the end are hand-crafted. Remove when display
     -- of overlays adds such spaces automatically.
  in map T.pack
       [ big
       , printf "%4d. %6d  This adventuring party %s after %d turns  "
                pos (points score) died turns
       , lil ++ printf "on %s.  " curDate
       ]

-- | The list of scores, in decreasing order.
type ScoreTable = [ScoreRecord]

-- | Empty score table
empty :: ScoreTable
empty = []

-- | Save a serialized version of the high scores table.
save :: FilePath -> ScoreTable -> IO ()
save configScoresFile scores = encodeEOF configScoresFile $ take 100 scores

-- | Read the high scores table. Return the empty table if no file.
restore :: FilePath -> IO ScoreTable
restore configScoresFile = do
  b <- doesFileExist configScoresFile
  if not b
    then return empty
    else strictDecodeEOF configScoresFile

-- | Insert a new score into the table, Return new table and the ranking.
insertPos :: ScoreRecord -> ScoreTable -> (ScoreTable, Int)
insertPos s h =
  let (prefix, suffix) = L.span (> s) h

  in (prefix ++ [s] ++ suffix, L.length prefix + 1)

-- | Generate a new score and possibly save it.
register :: FilePath   -- ^ the config file
         -> Bool       -- ^ whether to write or only render
         -> Int        -- ^ the total score. not halved yet
         -> Time       -- ^ game time spent
         -> Status     -- ^ reason of the game interruption
         -> IO (ScoreTable, Int)
register configScoresFile write total time status = do
  date <- getClockTime
  table <- restore configScoresFile
  let points = case status of
                 Killed _ -> (total + 1) `div` 2
                 _        -> total
      negTime = timeNegate time
      score = ScoreRecord{..}
      (ntable, pos) = insertPos score table
  when write $ save configScoresFile ntable
  return (ntable, pos)

-- | Show a screenful of the high scores table.
-- Parameter height is the number of (3-line) scores to be shown.
showTable :: ScoreTable -> Int -> Int -> Overlay
showTable h start height =
  let zipped    = zip [1..] h
      screenful = take height . drop (start - 1) $ zipped
  in concatMap showScore screenful

-- | Produce a couple of renderings of the high scores table.
showCloseScores :: Int -> ScoreTable -> Int -> [Overlay]
showCloseScores pos h height =
  if pos <= height
  then [showTable h 1 height]
  else [showTable h 1 height,
        showTable h (max (height + 1) (pos - height `div` 2)) height]

-- | Generate a slideshow with the current and previous scores.
slideshow :: ScoreTable -- ^ current score table
          -> Int        -- ^ position of the current high score in the table
          -> Status     -- ^ reason of the game interruption
          -> Slideshow
slideshow table pos status =
  let (_, nlines) = normalLevelBound  -- TODO: query terminal size instead
      height = nlines `div` 3
      (subject, person, msgUnless) =
        case status of
          Killed lvl | fromEnum lvl <= 1 ->
            ("your short-lived struggle", MU.Sg3rd, "(score halved)")
          Killed _ ->
            ("your heroic deeds", MU.PlEtc, "(score halved)")
          Camping ->
            ("your valiant exploits", MU.PlEtc, "(unless you are slain)")
          Victor ->
            ("your glorious victory", MU.Sg3rd,
             if pos <= height
             then "among the greatest heroes"
             else "")
          Restart ->
            ("your abortive attempt", MU.Sg3rd, "(score halved)")
      msg = makeSentence
        [ MU.SubjectVerb person MU.Yes subject "award you"
        , MU.Ordinal pos, "place"
        , msgUnless ]
  in toSlideshow $ map ([msg] ++) $ showCloseScores pos table height

instance Binary ScoreRecord where
  put (ScoreRecord p n (TOD cs cp) s) = do
    put p
    put n
    put cs
    put cp
    put s
  get = do
    p <- get
    n <- get
    cs <- get
    cp <- get
    s <- get
    return (ScoreRecord p n (TOD cs cp) s)
