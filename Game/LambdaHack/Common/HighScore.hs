{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | High score table operations.
module Game.LambdaHack.Common.HighScore
  ( ScoreTable, empty, register, highSlideshow
  ) where

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Game.LambdaHack.Common.Kind as Kind
import qualified NLP.Miniutter.English as MU
import System.Time
import Text.Printf

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
  , ourVictims   :: !(EM.EnumMap (Kind.Id ActorKind) Int)  -- ^ allies lost
  , theirVictims :: !(EM.EnumMap (Kind.Id ActorKind) Int)  -- ^ foes killed
  }
  deriving (Eq, Ord)

-- TODO: move all to Text
-- | Show a single high score, from the given ranking in the high score table.
showScore :: (Int, ScoreRecord) -> [Text]
showScore (pos, score) =
  let Status{stOutcome, stDepth} = status score
      died = case stOutcome of
        Killed   -> "Perished on level " ++ show (abs stDepth)
        Defeated -> "Was defeated"
        Camping  -> "Camps somewhere"
        Conquer  -> "Slew all opposition"
        Escape   -> "Emerged victorious"
        Restart  -> "Resigned prematurely"
      curDate = calendarTimeToString . toUTCTime . date $ score
      turns = - (negTime score `timeFit` timeTurn)
      diff = 5 - difficulty score
      victims :: String
      victims = printf ", killed %d, lost %d" (EM.size $ theirVictims score)
                                              (EM.size $ ourVictims score)
      diffText :: String
      diffText | diff == 5 = ""
               | otherwise = printf ", difficulty %d" diff
     -- TODO: the spaces at the end are hand-crafted. Remove when display
     -- of overlays adds such spaces automatically.
  in map T.pack
       [ ""
       , printf "%4d. %6d  %s%s%s"
                pos (points score) died victims diffText
       , "              " ++ printf "after %d turns on %s." turns curDate
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
         -> EM.EnumMap (Kind.Id ActorKind) Int  -- ^ allies lost
         -> EM.EnumMap (Kind.Id ActorKind) Int  -- ^ foes killed
         -> Bool        -- ^ whether the faction fights against spawners
         -> Maybe (ScoreTable, Int)
register table total time status@Status{stOutcome} date difficulty
         ourVictims theirVictims fightsAgainstSpawners =
  let pUnscaled =
        if fightsAgainstSpawners
        -- Heroes gather loot and mourn their victims.
        then total - 100 * EM.size ourVictims
        -- Spawners or skirmishers get no bonus from loot and no malus
        -- from loses, but try to kill opponents fast and blodily.
        else let turnsSpent = timeFit time timeTurn
                 speedup = 10000 - 5 * turnsSpent
                 bonus = sqrt $ fromIntegral speedup :: Double
             in 100 * EM.size theirVictims
                + if stOutcome == Conquer
                  then 10 + floor bonus
                  else 0
      pHalved = if stOutcome `elem` [Killed, Defeated, Restart]
                then pUnscaled `divUp` 2
                else pUnscaled
      points = (round :: Double -> Int)
               $ fromIntegral pHalved * 1.5 ^^ difficulty
      negTime = timeNegate time
      score = ScoreRecord{..}
  in if points > 0 then Just $ insertPos score table else Nothing

-- | Show a screenful of the high scores table.
-- Parameter height is the number of (3-line) scores to be shown.
tshowable :: ScoreTable -> Int -> Int -> [Text]
tshowable (ScoreTable table) start height =
  let zipped    = zip [1..] table
      screenful = take height . drop (start - 1) $ zipped
  in concatMap showScore screenful ++ [moreMsg]

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
              -> Status     -- ^ reason of the game interruption
              -> Slideshow
highSlideshow table pos status =
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
            -- TODO: this is only according to the limited player knowledge;
            -- the final score can be different; say this somewhere
            ("your valiant exploits", MU.PlEtc, "(unless you are slain)")
          Conquer ->
            ("your ruthless victory", MU.Sg3rd,
             if pos <= height
             then "among the greatest heroes"
             else "(score based on time)")
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
  in toSlideshow True $ map ([msg] ++) $ showCloseScores pos table height

instance Binary ScoreRecord where
  put (ScoreRecord p n (TOD cs cp) s difficulty ourVictims theirVictims) = do
    put p
    put n
    put cs
    put cp
    put s
    put difficulty
    put ourVictims
    put theirVictims
  get = do
    p <- get
    n <- get
    cs <- get
    cp <- get
    s <- get
    difficulty <- get
    ourVictims <- get
    theirVictims <- get
    return $! ScoreRecord p n (TOD cs cp) s difficulty ourVictims theirVictims
