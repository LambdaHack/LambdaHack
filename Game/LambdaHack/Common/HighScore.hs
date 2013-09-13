{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
-- | High score table operations.
module Game.LambdaHack.Common.HighScore
  ( ScoreTable, empty, register, slideshow
  ) where

import Data.Binary
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU
import System.Time
import Text.Printf

import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Time

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
  let Status{stOutcome, stDepth} = status score
      died = case stOutcome of
        Killed -> "perished on level " ++ show stDepth ++ ","
        Defeated -> "was defeated"
        Camping -> "is camping somewhere,"
        Conquer -> "eliminated all opposition"
        Escape -> "emerged victorious"
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
newtype ScoreTable = ScoreTable [ScoreRecord]
  deriving (Eq, Binary)

instance Show ScoreTable where
  show _ = "a score table"

-- | Empty score table
empty :: ScoreTable
empty = ScoreTable []

-- | Insert a new score into the table, Return new table and the ranking.
-- Make sure the table doesn't grow too large.
insertPos :: ScoreRecord -> ScoreTable -> (ScoreTable, Int)
insertPos s (ScoreTable table) =
  let (prefix, suffix) = L.span (> s) table
      pos = L.length prefix + 1
  in (ScoreTable $ prefix ++ [s] ++ take (100 - pos) suffix, pos)

-- | Register a new score in a score table.
register :: ScoreTable  -- ^ old table
         -> Int         -- ^ the total score. not halved yet
         -> Time        -- ^ game time spent
         -> Status      -- ^ reason of the game interruption
         -> ClockTime   -- ^ current date
         -> (ScoreTable, Int)
register table total time status date =
  let points = case stOutcome status of
                 Killed -> (total + 1) `div` 2
                 _        -> total
      negTime = timeNegate time
      score = ScoreRecord{..}
  in insertPos score table

-- | Show a screenful of the high scores table.
-- Parameter height is the number of (3-line) scores to be shown.
showTable :: ScoreTable -> Int -> Int -> Overlay
showTable (ScoreTable table) start height =
  let zipped    = zip [1..] table
      screenful = take height . drop (start - 1) $ zipped
  in concatMap showScore screenful ++ [moreMsg]

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
        case stOutcome status of
          Killed | stDepth status <= 1 ->
            ("your short-lived struggle", MU.Sg3rd, "(score halved)")
          Killed ->
            ("your heroic deeds", MU.PlEtc, "(score halved)")
          Defeated ->
            ("your futile efforts", MU.PlEtc, "(score halved)")
          Camping ->
            ("your valiant exploits", MU.PlEtc, "(unless you are slain)")
          Conquer ->
            ("your ruthless victory", MU.Sg3rd,
             if pos <= height
             then "among the greatest heroes"
             else "")
          Escape ->
            ("your dashing coup", MU.Sg3rd,
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
