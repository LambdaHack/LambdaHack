module HighScores where

import System.Directory
import Control.Exception as E hiding (handle)

import Data.List as L

import File

-- | A single score.
-- TODO: date of victory/death, user name, cause of death, etc.
type ScoreRecord = Int

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

-- | Take care of a new score, return a message to display.
register :: ScoreRecord -> IO String
register s =
  do
    h <- restore
    let (h', n) = insertPos s h
        msg = "Your exploits award you place >>"
              ++ show n ++ "<< on the high scores table."
    save h'
    return msg
