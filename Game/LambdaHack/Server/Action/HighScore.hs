{-# LANGUAGE OverloadedStrings #-}
-- | High score table operations.
module Game.LambdaHack.Server.Action.HighScore
  ( register
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

import Game.LambdaHack.Server.Config
import Game.LambdaHack.Faction
import Game.LambdaHack.Level
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

-- | Show a single high score, from the given ranking in the high score table.
showScore :: (Int, ScoreRecord) -> [Text]
showScore (pos, score) =
  let died = case status score of
        Killed lvl -> "perished on level " ++ show (levelNumber lvl) ++ ","
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

-- | Save a simple serialized version of the high scores table.
save :: Config -> ScoreTable -> IO ()
save Config{configScoresFile} scores = encodeEOF configScoresFile scores

-- | Read the high scores table. Return the empty table if no file.
restore :: Config -> IO ScoreTable
restore Config{configScoresFile} = do
  b <- doesFileExist configScoresFile
  if not b
    then return empty
    else strictDecodeEOF configScoresFile

-- | Insert a new score into the table, Return new table and the ranking.
insertPos :: ScoreRecord -> ScoreTable -> (ScoreTable, Int)
insertPos s h =
  let (prefix, suffix) = L.span (> s) h
  in (prefix ++ [s] ++ suffix, L.length prefix + 1)

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

-- | Take care of saving a new score to the table
-- and return a list of messages to display.
register :: Config     -- ^ the config file
         -> Bool       -- ^ whether to write or only render
         -> Int        -- ^ the total score. not halved yet
         -> Time       -- ^ game time spent
         -> ClockTime  -- ^ date of the last game interruption
         -> Status     -- ^ reason of the game interruption
         -> IO Slideshow
register config write total time date status = do
  h <- restore config
  let points = case status of
                 Killed _ -> (total + 1) `div` 2
                 _        -> total
      negTime = timeNegate time
      score = ScoreRecord{..}
      (h', pos) = insertPos score h
      (_, nlines) = normalLevelBound  -- TODO: query terminal size instead
      height = nlines `div` 3
      (subject, person, msgUnless) =
        case status of
          Killed lvl | levelNumber lvl <= 1 ->
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
  when write $ save config h'
  return $! toSlideshow $ map ([msg] ++) $ showCloseScores pos h' height
