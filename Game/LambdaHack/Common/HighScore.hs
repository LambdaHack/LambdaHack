{-# LANGUAGE CPP, DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | High score table operations.
module Game.LambdaHack.Common.HighScore
  ( ScoreDict, ScoreTable
  , empty, register, showScore, getTable, getRecord, highSlideshow
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , ScoreRecord
#endif
  ) where

import Control.Exception.Assert.Sugar
import Data.Binary
import Data.Binary.Orphans ()
import qualified Data.EnumMap.Strict as EM
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Data.Time.LocalTime
import GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ItemKind (ItemKind)
import Game.LambdaHack.Content.ModeKind (HiCondPoly, HiIndeterminant (..),
                                         ModeKind, Outcome (..))

-- | A single score record. Records are ordered in the highscore table,
-- from the best to the worst, in lexicographic ordering wrt the fields below.
data ScoreRecord = ScoreRecord
  { points       :: !Int        -- ^ the score
  , negTime      :: !Time       -- ^ game time spent (negated, so less better)
  , date         :: !POSIXTime  -- ^ date of the last game interruption
  , status       :: !Status     -- ^ reason of the game interruption
  , difficulty   :: !Int        -- ^ difficulty of the game
  , gplayerName  :: !Text       -- ^ name of the faction's gplayer
  , ourVictims   :: !(EM.EnumMap (Kind.Id ItemKind) Int)  -- ^ allies lost
  , theirVictims :: !(EM.EnumMap (Kind.Id ItemKind) Int)  -- ^ foes killed
  }
  deriving (Eq, Ord, Show, Generic)

instance Binary ScoreRecord

-- | The list of scores, in decreasing order.
newtype ScoreTable = ScoreTable [ScoreRecord]
  deriving (Eq, Binary)

instance Show ScoreTable where
  show _ = "a score table"

-- | A dictionary from game mode IDs to scores tables.
type ScoreDict = EM.EnumMap (Kind.Id ModeKind) ScoreTable

-- | Show a single high score, from the given ranking in the high score table.
showScore :: TimeZone -> (Int, ScoreRecord) -> [Text]
showScore tz (pos, score) =
  let Status{stOutcome, stDepth} = status score
      died = case stOutcome of
        Killed   -> "perished on level" <+> tshow (abs stDepth)
        Defeated -> "was defeated"
        Camping  -> "camps somewhere"
        Conquer  -> "slew all opposition"
        Escape   -> "emerged victorious"
        Restart  -> "resigned prematurely"
      curDate = tshow . utcToLocalTime tz . posixSecondsToUTCTime . date $ score
      turns = absoluteTimeNegate (negTime score) `timeFitUp` timeTurn
      tpos = T.justifyRight 3 ' ' $ tshow pos
      tscore = T.justifyRight 6 ' ' $ tshow $ points score
      victims = let nkilled = sum $ EM.elems $ theirVictims score
                    nlost = sum $ EM.elems $ ourVictims score
                in "killed" <+> tshow nkilled <> ", lost" <+> tshow nlost
      diff = difficulty score
      diffText | diff == difficultyDefault = ""
               | otherwise = "difficulty" <+> tshow diff <> ", "
      tturns = makePhrase [MU.CarWs turns "turn"]
  in [ tpos <> "." <+> tscore <+> gplayerName score
       <+> died <> "," <+> victims <> ","
     , "            "
       <> diffText <> "after" <+> tturns <+> "on" <+> curDate <> "."
     ]

getTable :: Kind.Id ModeKind -> ScoreDict -> ScoreTable
getTable = EM.findWithDefault (ScoreTable [])

getRecord :: Int -> ScoreTable -> ScoreRecord
getRecord pos (ScoreTable table) =
  fromMaybe (assert `failure` (pos, table))
  $ listToMaybe $ drop (pred pos) table

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
         -> Time        -- ^ game time spent
         -> Status      -- ^ reason of the game interruption
         -> POSIXTime   -- ^ current date
         -> Int         -- ^ difficulty level
         -> Text        -- ^ name of the faction's gplayer
         -> EM.EnumMap (Kind.Id ItemKind) Int  -- ^ allies lost
         -> EM.EnumMap (Kind.Id ItemKind) Int  -- ^ foes killed
         -> HiCondPoly
         -> (Bool, (ScoreTable, Int))
register table total time status@Status{stOutcome} date difficulty gplayerName
         ourVictims theirVictims hiCondPoly =
  let turnsSpent = fromIntegral $ timeFitUp time timeTurn
      hiInValue (hi, c) = case hi of
        HiConst -> c
        HiLoot -> c * fromIntegral total
        HiBlitz -> -- Up to 1000000/c turns matter.
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
      points = (ceiling :: Double -> Int)
               $ hiCondValue hiCondPoly
                 * 1.5 ^^ (- (difficultyCoeff difficulty))
      negTime = absoluteTimeNegate time
      score = ScoreRecord{..}
  in (points > 0, insertPos score table)

-- | Show a screenful of the high scores table.
-- Parameter height is the number of (3-line) scores to be shown.
showTable :: TimeZone -> ScoreTable -> Int -> Int -> [Text]
showTable tz (ScoreTable table) start height =
  let zipped    = zip [1..] table
      screenful = take height . drop (start - 1) $ zipped
  in intercalate [""] (map (showScore tz) screenful) ++ [moreMsg]

-- | Produce a couple of renderings of the high scores table.
showNearbyScores :: TimeZone -> Int -> ScoreTable -> Int -> [[Text]]
showNearbyScores tz pos h height =
  if pos <= height
  then [showTable tz h 1 height]
  else [showTable tz h 1 height,
        showTable tz h (max (height + 1) (pos - height `div` 2)) height]

-- | Generate a slideshow with the current and previous scores.
highSlideshow :: ScoreTable -- ^ current score table
              -> Int        -- ^ position of the current score in the table
              -> Text       -- ^ the name of the game mode
              -> TimeZone   -- ^ the timezone where the game is run
              -> Slideshow
highSlideshow table pos gameModeName tz =
  let (_, nlines) = normalLevelBound  -- TODO: query terminal size instead
      height = nlines `div` 3
      posStatus = status $ getRecord pos table
      (efforts, person, msgUnless) =
        case stOutcome posStatus of
          Killed | stDepth posStatus <= 1 ->
            ("your short-lived struggle", MU.Sg3rd, "(no bonus)")
          Killed ->
            ("your heroic deeds", MU.PlEtc, "(no bonus)")
          Defeated ->
            ("your futile efforts", MU.PlEtc, "(no bonus)")
          Camping ->
            -- TODO: this is only according to the limited player knowledge;
            -- the final score can be different; say this somewhere
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
      subject =
        makePhrase [efforts, "in", MU.Capitalize $ MU.Text gameModeName]
      msg = makeSentence
        [ MU.SubjectVerb person MU.Yes (MU.Text subject) "award you"
        , MU.Ordinal pos, "place", msgUnless ]
  in toSlideshow $ map ([msg, ""] ++) $ showNearbyScores tz pos table height
