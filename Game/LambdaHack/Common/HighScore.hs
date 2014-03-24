{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | High score table operations.
module Game.LambdaHack.Common.HighScore
  ( ScoreTable, empty, register, showScore, getRecord, highSlideshow
  ) where

import Control.Exception.Assert.Sugar
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Game.LambdaHack.Common.Kind as Kind
import GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU
import System.Time

import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ActorKind

-- | A single score record. Records are ordered in the highscore table,
-- from the best to the worst, in lexicographic ordering wrt the fields below.
data ScoreRecord = ScoreRecord
  { points       :: !Int        -- ^ the score
  , negTime      :: !Time       -- ^ game time spent (negated, so less better)
  , date         :: !ClockTime  -- ^ date of the last game interruption
  , status       :: !Status     -- ^ reason of the game interruption
  , difficulty   :: !Int        -- ^ difficulty of the game
  , gplayerName  :: !Text       -- ^ name of the faction's gplayer
  , ourVictims   :: !(EM.EnumMap (Kind.Id ActorKind) Int)  -- ^ allies lost
  , theirVictims :: !(EM.EnumMap (Kind.Id ActorKind) Int)  -- ^ foes killed
  }
  deriving (Eq, Ord, Show, Generic)

instance Binary ClockTime where
  put (TOD cs cp) = do
    put cs
    put cp
  get = do
    cs <- get
    cp <- get
    return $! TOD cs cp

instance Binary ScoreRecord

-- | Show a single high score, from the given ranking in the high score table.
showScore :: (Int, ScoreRecord) -> [Text]
showScore (pos, score) =
  let Status{stOutcome, stDepth} = status score
      died = case stOutcome of
        Killed   -> "perished on level" <+> tshow (abs stDepth)
        Defeated -> "was defeated"
        Camping  -> "camps somewhere"
        Conquer  -> "slew all opposition"
        Escape   -> "emerged victorious"
        Restart  -> "resigned prematurely"
      curDate = T.pack $ calendarTimeToString . toUTCTime . date $ score
      turns = absoluteTimeNegate (negTime score) `timeFitUp` timeTurn
      tpos = T.justifyRight 3 ' ' $ tshow pos
      tscore = T.justifyRight 6 ' ' $ tshow $ points score
      victims = let nkilled = sum $ EM.elems $ theirVictims score
                    nlost = sum $ EM.elems $ ourVictims score
                in "killed" <+> tshow nkilled <> ", lost" <+> tshow nlost
      diff = difficulty score
      diffText | diff == difficultyDefault = ""
               | otherwise = "difficulty" <+> tshow diff <> ", "
      tturns = makePhrase $ [MU.CarWs turns "turn"]
  in [ tpos <> "." <+> tscore <+> "The" <+> gplayerName score <+> "team"
       <+> died <> "," <+> victims <> ","
     , "            "
       <> diffText <> "after" <+> tturns <+> "on" <+> curDate <> "."
     ]

getRecord :: Int -> ScoreTable -> ScoreRecord
getRecord pos (ScoreTable table) =
  fromMaybe (assert `failure` (pos, table))
  $ listToMaybe $ drop (pred pos) table

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
  let (prefix, suffix) = span (> s) table
      pos = length prefix + 1
  in (ScoreTable $ prefix ++ [s] ++ take (100 - pos) suffix, pos)

-- | Register a new score in a score table.
register :: ScoreTable  -- ^ old table
         -> Int         -- ^ the total value of faction items
         -> Time        -- ^ game time spent
         -> Status      -- ^ reason of the game interruption
         -> ClockTime   -- ^ current date
         -> Int         -- ^ difficulty level
         -> Text        -- ^ name of the faction's gplayer
         -> EM.EnumMap (Kind.Id ActorKind) Int  -- ^ allies lost
         -> EM.EnumMap (Kind.Id ActorKind) Int  -- ^ foes killed
         -> Bool        -- ^ whether the faction fights against spawners
         -> (Bool, (ScoreTable, Int))
register table total time status@Status{stOutcome} date difficulty gplayerName
         ourVictims theirVictims fightsAgainstSpawners =
  let pBase =
        if fightsAgainstSpawners
        -- Heroes rejoice in loot and mourn their victims.
        then fromIntegral total
        -- Spawners or skirmishers get no bonus from loot and no malus
        -- from loses, but try to kill opponents fast and blodily,
        -- or at least hold up for long and incur heavy losses.
        else let turnsSpent = timeFitUp time timeTurn
                 speedup = max 0 $ 1000000 - 100 * turnsSpent
                 survival = 100 * turnsSpent
             in if stOutcome `elem` [Conquer, Escape]
                -- Up to 1000 points for quick victory, so up to 10000 turns.
                then sqrt $ fromIntegral speedup
                -- Up to 1000 points for surviving long, so up to 10000 turns.
                else min 1000
                     $ sqrt $ fromIntegral survival
      pBonus =
        if fightsAgainstSpawners
        then max 0 (1000 - 100 * sum (EM.elems ourVictims))
        else 1000 + 100 * sum (EM.elems theirVictims)
      pSum :: Double
      pSum = if stOutcome `elem` [Conquer, Escape]
             then pBase + fromIntegral pBonus
             else pBase
      worthMentioning =
        if fightsAgainstSpawners then total > 0 else not (EM.null theirVictims)
      points = (ceiling :: Double -> Int)
               $ pSum * 1.5 ^^ (- (difficultyCoeff difficulty))
      negTime = absoluteTimeNegate time
      score = ScoreRecord{..}
  in (worthMentioning, insertPos score table)

-- | Show a screenful of the high scores table.
-- Parameter height is the number of (3-line) scores to be shown.
tshowable :: ScoreTable -> Int -> Int -> [Text]
tshowable (ScoreTable table) start height =
  let zipped    = zip [1..] table
      screenful = take height . drop (start - 1) $ zipped
  in (intercalate ["\n"] $ map showScore screenful) ++ [moreMsg]

-- | Produce a couple of renderings of the high scores table.
showCloseScores :: Int -> ScoreTable -> Int -> [[Text]]
showCloseScores pos h height =
  if pos <= height
  then [tshowable h 1 height]
  else [tshowable h 1 height,
        tshowable h (max (height + 1) (pos - height `div` 2)) height]

-- | Generate a slideshow with the current and previous scores.
highSlideshow :: ScoreTable -- ^ current score table
              -> Int        -- ^ position of the current score in the table
              -> Slideshow
highSlideshow table pos =
  let (_, nlines) = normalLevelBound  -- TODO: query terminal size instead
      height = nlines `div` 3
      posStatus = status $ getRecord pos table
      (subject, person, msgUnless) =
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
             then "among the greatest heroes"
             else "(bonus included)")
          Escape ->
            ("your dashing coup", MU.Sg3rd,
             if pos <= height
             then "among the greatest heroes"
             else "(bonus included)")
          Restart ->
            ("your abortive attempt", MU.Sg3rd, "(no bonus)")
      msg = makeSentence
        [ MU.SubjectVerb person MU.Yes subject "award you"
        , MU.Ordinal pos, "place"
        , msgUnless ]
  in toSlideshow False $ map ([msg, "\n"] ++) $ showCloseScores pos table height
