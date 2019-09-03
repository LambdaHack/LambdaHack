{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | High score table operations.
module Game.LambdaHack.Common.HighScore
  ( ScoreTable, ScoreDict
  , empty, register, showScore, showAward, getTable, unTable, getRecord
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , ScoreRecord, insertPos
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.Text as T
import           Data.Time.Clock.POSIX
import           Data.Time.LocalTime
import           GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ItemKind (ItemKind)
import Game.LambdaHack.Content.ModeKind (HiCondPoly, HiIndeterminant (..),
                                         ModeKind, Outcome (..))
import Game.LambdaHack.Definition.Defs

-- | A single score record. Records are ordered in the highscore table,
-- from the best to the worst, in lexicographic ordering wrt the fields below.
data ScoreRecord = ScoreRecord
  { points       :: Int        -- ^ the score
  , negTime      :: Time       -- ^ game time spent (negated, so less better)
  , date         :: POSIXTime  -- ^ date of the last game interruption
  , status       :: Status     -- ^ reason of the game interruption
  , challenge    :: Challenge  -- ^ challenge setup of the game
  , gplayerName  :: Text       -- ^ name of the faction's gplayer
  , ourVictims   :: EM.EnumMap (ContentId ItemKind) Int  -- ^ allies lost
  , theirVictims :: EM.EnumMap (ContentId ItemKind) Int  -- ^ foes killed
  }
  deriving (Eq, Ord, Generic)

instance Binary ScoreRecord

-- | The list of scores, in decreasing order.
newtype ScoreTable = ScoreTable {unTable :: [ScoreRecord]}
  deriving (Eq, Binary)

instance Show ScoreTable where
  show _ = "a score table"

-- | A dictionary from game mode IDs to scores tables.
type ScoreDict = EM.EnumMap (ContentId ModeKind) ScoreTable

-- | Empty score table
empty :: ScoreDict
empty = EM.empty

-- | Insert a new score into the table, Return new table and the ranking.
-- Make sure the table doesn't grow too large.
insertPos :: ScoreRecord -> ScoreTable -> (ScoreTable, Int)
insertPos s (ScoreTable table) =
  let (prefix, suffix) = span (> s) table
      pos = length prefix + 1
  in (ScoreTable $ prefix ++ [s] ++ take (100 - pos) suffix, pos)

-- | Register a new score in a score table.
register :: ScoreTable  -- ^ old table
         -> Int         -- ^ the total value of faction items
         -> Int         -- ^ the total value of dungeon items
         -> Time        -- ^ game time spent
         -> Status      -- ^ reason of the game interruption
         -> POSIXTime   -- ^ current date
         -> Challenge   -- ^ challenge setup
         -> Text        -- ^ name of the faction's gplayer
         -> EM.EnumMap (ContentId ItemKind) Int  -- ^ allies lost
         -> EM.EnumMap (ContentId ItemKind) Int  -- ^ foes killed
         -> HiCondPoly
         -> (Bool, (ScoreTable, Int))
register table total dungeonTotal time status@Status{stOutcome}
         date challenge gplayerName ourVictims theirVictims hiCondPoly =
  let turnsSpent = fromIntegral $ timeFitUp time timeTurn
      hiInValue (hi, c) = assert (total <= dungeonTotal) $ case hi of
        HiConst -> c
        HiLoot | dungeonTotal == 0 -> c  -- a fluke; no gold generated
        HiLoot -> c * fromIntegral total / fromIntegral dungeonTotal
        HiSprint -> -- Up to -c turns matter.
                    max 0 (-c - turnsSpent)
        HiBlitz -> -- Up to 1000000/-c turns matter.
                   sqrt $ max 0 (1000000 + c * turnsSpent)
        HiSurvival -> -- Up to 1000000/c turns matter.
                      sqrt $ max 0 (min 1000000 $ c * turnsSpent)
        HiKill -> c * fromIntegral (sum (EM.elems theirVictims))
        HiLoss -> c * fromIntegral (sum (EM.elems ourVictims))
      hiPolynomialValue = sum . map hiInValue
      hiSummandValue (hiPoly, outcomes) =
        if stOutcome `elem` outcomes
        then max 0 (hiPolynomialValue hiPoly)
        else 0
      hiCondValue = sum . map hiSummandValue
      -- Other challenges than HP difficulty are not reflected in score.
      points = (ceiling :: Double -> Int)
               $ hiCondValue hiCondPoly
                 * 1.5 ^^ (- (difficultyCoeff (cdiff challenge)))
      negTime = absoluteTimeNegate time
      score = ScoreRecord{..}
  in (points > 0, insertPos score table)

-- | Show a single high score, from the given ranking in the high score table.
showScore :: TimeZone -> Int -> ScoreRecord -> [Text]
showScore tz pos score =
  let Status{stOutcome, stDepth} = status score
      died = case stOutcome of
        Killed   -> "perished on level" <+> tshow (abs stDepth)
        Defeated -> "got defeated"
        Camping  -> "set camp"
        Conquer  -> "slew all opposition"
        Escape   -> "emerged victorious"
        Restart  -> "resigned prematurely"
      curDate = T.take 19 . tshow . utcToLocalTime tz
                . posixSecondsToUTCTime . date $ score
      turns = absoluteTimeNegate (negTime score) `timeFitUp` timeTurn
      tpos = T.justifyRight 3 ' ' $ tshow pos
      tscore = T.justifyRight 6 ' ' $ tshow $ points score
      victims = let nkilled = sum $ EM.elems $ theirVictims score
                    nlost = sum $ EM.elems $ ourVictims score
                in "killed" <+> tshow nkilled <> ", lost" <+> tshow nlost
      diff = cdiff $ challenge score
      diffText | diff == difficultyDefault = ""
               | otherwise = "difficulty" <+> tshow diff <> ", "
      tturns = makePhrase [MU.CarWs turns "turn"]
  in [ tpos <> "." <+> tscore <+> gplayerName score
       <+> died <> "," <+> victims <> ","
     , "            "
       <> diffText <> "after" <+> tturns <+> "on" <+> curDate <> "."
     ]

getTable :: ContentId ModeKind -> ScoreDict -> ScoreTable
getTable = EM.findWithDefault (ScoreTable [])

getRecord :: Int -> ScoreTable -> ScoreRecord
getRecord pos (ScoreTable table) =
  fromMaybe (error $ "" `showFailure` pos)
  $ listToMaybe $ drop (pred pos) table

showAward :: Int        -- ^ number of (3-line) scores to be shown
          -> ScoreTable -- ^ current score table
          -> Int        -- ^ position of the current score in the table
          -> Text       -- ^ the name of the game mode
          -> Text
showAward height table pos gameModeName =
  let posStatus = status $ getRecord pos table
      (efforts, person, msgUnless) =
        case stOutcome posStatus of
          Killed | stDepth posStatus <= 1 ->
            ("your short-lived struggle", MU.Sg3rd, "(no bonus)")
          Killed ->
            ("your heroic deeds", MU.PlEtc, "(no bonus)")
          Defeated ->
            ("your futile efforts", MU.PlEtc, "(no bonus)")
          Camping ->
            -- This is only according to the limited player knowledge;
            -- the final score can be different, which is fine:
            ("your valiant exploits", MU.PlEtc, "")
          Conquer ->
            ("your ruthless victory", MU.Sg3rd,
             if pos <= height
             then "among the best"  -- "greatest heroes" doesn't fit
             else "(bonus included)")
          Escape ->
            ("your dashing coup", MU.Sg3rd,
             if pos <= height
             then "among the best"
             else "(bonus included)")
          Restart ->
            ("your abortive attempt", MU.Sg3rd, "(no bonus)")
      subject = makePhrase [efforts, "in", MU.Text gameModeName]
  in makeSentence
       [ MU.SubjectVerb person MU.Yes (MU.Text subject) "award you"
       , MU.Ordinal pos, "place", msgUnless ]
