-- | Factions taking part in the game: e.g., two human players controlling
-- the hero faction battling the monster and the animal factions.
module Game.LambdaHack.Common.Faction
  ( FactionId, FactionDict, Faction(..), Diplomacy(..), Outcome(..), Status(..)
  , isHeroFact, isHorrorFact, isSpawnFact, isSummonFact, isAtWar, isAllied
  , difficultyBound, difficultyDefault, difficultyCoeff
  ) where

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import Data.Text (Text)

import Game.LambdaHack.Common.Actor
import qualified Game.LambdaHack.Common.Color as Color
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ModeKind

-- | All factions in the game, indexed by faction identifier.
type FactionDict = EM.EnumMap FactionId Faction

data Faction = Faction
  { gkind    :: !(Kind.Id FactionKind)  -- ^ the kind of the faction
  , gname    :: !Text                   -- ^ individual name
  , gcolor   :: !Color.Color            -- ^ color of actors or their frames
  , gplayer  :: !Player                 -- ^ the player spec for this faction
  , gdipl    :: !Dipl                   -- ^ diplomatic mode
  , gquit    :: !(Maybe Status)         -- ^ cause of game end/exit
  , gleader  :: !(Maybe ActorId)        -- ^ the leader of the faction, if any
  , gvictims :: !(EM.EnumMap (Kind.Id ActorKind) Int)  -- ^ members killed
  }
  deriving (Show, Eq)

-- | Diplomacy states. Higher overwrite lower in case of assymetric content.
data Diplomacy =
    Unknown
  | Neutral
  | Alliance
  | War
  deriving (Show, Eq, Ord)

type Dipl = EM.EnumMap FactionId Diplomacy

-- | Outcome of a game.
data Outcome =
    Killed    -- ^ the faction was eliminated
  | Defeated  -- ^ the faction lost the game in another way
  | Camping   -- ^ game is supended
  | Conquer   -- ^ the player won by eliminating all rivals
  | Escape    -- ^ the player escaped the dungeon alive
  | Restart   -- ^ game is restarted
  deriving (Show, Eq, Ord)

-- | Current game status.
data Status = Status
  { stOutcome :: !Outcome  -- ^ current game outcome
  , stDepth   :: !Int      -- ^ depth of the final encounter
  , stInfo    :: !Text     -- ^ extra information
  }
  deriving (Show, Eq, Ord)

-- | Tell whether the faction consists of heroes.
isHeroFact :: Kind.COps -> Faction -> Bool
isHeroFact Kind.COps{cofaction=Kind.Ops{okind}} fact =
  let kind = okind (gkind fact)
  in maybe False (> 0) $ lookup "hero" $ ffreq kind

-- | Tell whether the faction consists of summoned horrors only.
isHorrorFact :: Kind.COps -> Faction -> Bool
isHorrorFact Kind.COps{cofaction=Kind.Ops{okind}} fact =
  let kind = okind (gkind fact)
  in maybe False (> 0) $ lookup "horror" $ ffreq kind

-- | Tell whether the faction can spawn actors.
isSpawnFact :: Faction -> Bool
isSpawnFact fact = playerSpawn (gplayer fact) > 0

-- | Tell whether actors of the faction can be summoned by items, etc.
isSummonFact :: Kind.COps -> Faction -> Bool
isSummonFact Kind.COps{cofaction=Kind.Ops{okind}} fact =
  let kind = okind (gkind fact)
  in maybe False (> 0) $ lookup "summon" $ ffreq kind

-- | Check if factions are at war. Assumes symmetry.
isAtWar :: Faction -> FactionId -> Bool
isAtWar fact fid = War == EM.findWithDefault Unknown fid (gdipl fact)

-- | Check if factions are allied. Assumes symmetry.
isAllied :: Faction -> FactionId -> Bool
isAllied fact fid = Alliance == EM.findWithDefault Unknown fid (gdipl fact)

difficultyBound :: Int
difficultyBound = 9

difficultyDefault :: Int
difficultyDefault = 1 + (difficultyBound - 1) `div` 2

-- The function is its own inverse.
difficultyCoeff :: Int -> Int
difficultyCoeff n = difficultyDefault - n

instance Binary Faction where
  put Faction{..} = do
    put gkind
    put gname
    put gcolor
    put gplayer
    put gdipl
    put gquit
    put gleader
    put gvictims
  get = do
    gkind <- get
    gname <- get
    gcolor <- get
    gplayer <- get
    gdipl <- get
    gquit <- get
    gleader <- get
    gvictims <- get
    return $! Faction{..}

instance Binary Diplomacy where
  put Unknown  = putWord8 0
  put Neutral  = putWord8 1
  put Alliance = putWord8 2
  put War      = putWord8 3
  get = do
    tag <- getWord8
    case tag of
      0 -> return Unknown
      1 -> return Neutral
      2 -> return Alliance
      3 -> return War
      _ -> fail "no parse (Diplomacy)"

instance Binary Outcome where
  put Killed = putWord8 0
  put Defeated = putWord8 1
  put Camping = putWord8 2
  put Conquer = putWord8 3
  put Escape = putWord8 4
  put Restart = putWord8 5
  get = do
    tag <- getWord8
    case tag of
      0 -> return Killed
      1 -> return Defeated
      2 -> return Camping
      3 -> return Conquer
      4 -> return Escape
      5 -> return Restart
      _ -> fail "no parse (Outcome)"

instance Binary Status where
  put Status{..} = do
    put stOutcome
    put stDepth
    put stInfo
  get = do
    stOutcome <- get
    stDepth <- get
    stInfo <- get
    return $! Status{..}
