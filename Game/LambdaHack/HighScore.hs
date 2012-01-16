-- | High score table operations.
module Game.LambdaHack.HighScore
  ( Status(..), ScoreRecord(..), ScoreTable, save, restore, register, slideshow
  ) where

import System.Directory
import Control.Monad
import Text.Printf
import System.Time
import Data.Binary
import qualified Data.List as L

import Game.LambdaHack.Utils.File
import qualified Game.LambdaHack.Config as Config
import Game.LambdaHack.Dungeon
import Game.LambdaHack.Geometry

-- TODO: add heroes' names, exp and level, cause of death, user number/name.
-- Note: I tried using Date.Time, but got all kinds of problems,
-- including build problems and opaque types that make serialization difficult,
-- and I couldn't use Datetime because it needs old base (and is under GPL).
-- TODO: When we finally move to Date.Time, let's take timezone into account,
-- at least while displaying.
-- | A single score.
data ScoreRecord = ScoreRecord
  { points  :: !Int        -- ^ point score
  , negTurn :: !Int        -- ^ number of turns (negated for ordering)
  , date    :: !ClockTime  -- ^ date of the last game interruption
  , status  :: !Status     -- ^ reason of the game interruption
  }
  deriving (Eq, Ord)

-- | Status of the player after the last game interruption.
data Status =
    Killed !LevelId   -- ^ the player lost on the given level
  | Camping !LevelId  -- ^ game is supended with the player on the given level
  | Victor            -- ^ the player won
  deriving (Eq, Ord)

instance Binary Status where
  put (Killed ln)  = putWord8 0 >> put ln
  put (Camping ln) = putWord8 1 >> put ln
  put Victor       = putWord8 2
  get = do
    tag <- getWord8
    case tag of
      0 -> liftM Killed  get
      1 -> liftM Camping get
      2 -> return Victor
      _ -> fail "no parse (Status)"

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
showScore :: (Int, ScoreRecord) -> String
showScore (pos, score) =
  let died = case status score of
        Killed lvl -> "perished on level " ++ show (levelNumber lvl) ++ ","
        Camping lvl -> "is camping on level " ++ show (levelNumber lvl) ++ ","
        Victor -> "emerged victorious"
      time  = calendarTimeToString . toUTCTime . date $ score
      big   = "                                                 "
      lil   = "              "
      steps = negTurn score `div` (-10)
  in printf
       "%s\n%4d. %6d  This adventuring party %s after %d steps  \n%son %s.  \n"
       big pos (points score) died steps lil time

-- | The list of scores, in decreasing order.
type ScoreTable = [ScoreRecord]

-- | Empty score table
empty :: ScoreTable
empty = []

-- | Name of the high scores file.
scoresFile :: Config.CP -> IO String
scoresFile config = Config.getFile config "files" "scoresFile"

-- | We save a simple serialized version of the high scores table.
save :: Config.CP -> ScoreTable -> IO ()
save config scores = do
  f <- scoresFile config
  encodeEOF f scores

-- | Read the high scores table. Return the empty table if no file.
restore :: Config.CP -> IO ScoreTable
restore config = do
  f <- scoresFile config
  b <- doesFileExist f
  if not b
    then return empty
    else do
      scores <- strictDecodeEOF f
      return scores

-- | Insert a new score into the table, Return new table and the ranking.
insertPos :: ScoreRecord -> ScoreTable -> (ScoreTable, Int)
insertPos s h =
  let (prefix, suffix) = L.span (> s) h
  in (prefix ++ [s] ++ suffix, L.length prefix + 1)

-- | Show a screenful of the high scores table.
-- Parameter height is the number of (3-line) scores to be shown.
showTable :: ScoreTable -> Int -> Int -> String
showTable h start height =
  let zipped    = zip [1..] h
      screenful = take height . drop (start - 1) $ zipped
  in L.concatMap showScore screenful

-- | Produces a couple of renderings of the high scores table.
slideshow :: Int -> ScoreTable -> Int -> [String]
slideshow pos h height =
  if pos <= height
  then [showTable h 1 height]
  else [showTable h 1 height,
        showTable h (max (height + 1) (pos - height `div` 2)) height]

-- | Take care of saving a new score to the table
-- and return a list of messages to display.
register :: Config.CP -> Bool -> ScoreRecord -> IO (String, [String])
register config write s = do
  h <- restore config
  let (h', pos) = insertPos s h
      (_, nlines) = normalLevelBound  -- TODO: query terminal size instead
      height = nlines `div` 3
      (msgCurrent, msgUnless) =
        case status s of
          Killed _  -> (" short-lived", " (score halved)")
          Camping _ -> (" current", " (unless you are slain)")
          Victor    -> (" glorious",
                        if pos <= height
                        then " among the greatest heroes"
                        else "")
      msg = printf "Your%s exploits award you place >> %d <<%s."
              msgCurrent pos msgUnless
  when write $ save config h'
  return (msg, slideshow pos h' height)
