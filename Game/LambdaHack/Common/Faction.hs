-- | Factions taking part in the game: e.g., two human players controlling
-- the hero faction battling the monster and the animal factions.
module Game.LambdaHack.Common.Faction
  ( FactionId, FactionDict, Faction(..), Diplomacy(..), Outcome(..), Status(..)
  , isHeroFact, isCivilianFact, isHorrorFact, isSpawnFact, isSummonFact
  , isAllMoveFact, keepArenaFact, isAtWar, isAllied
  , difficultyBound, difficultyDefault, difficultyCoeff
  ) where

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import Data.Text (Text)

import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Actor
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
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
  , gsha     :: !ItemBag                -- ^ faction's shared inventory
  , gvictims :: !(EM.EnumMap ItemId Int)  -- ^ members killed
  }
  deriving (Show, Eq)

-- | Diplomacy states. Higher overwrite lower in case of assymetric content.
data Diplomacy =
    Unknown
  | Neutral
  | Alliance
  | War
  deriving (Show, Eq, Ord, Enum)

type Dipl = EM.EnumMap FactionId Diplomacy

-- | Outcome of a game.
data Outcome =
    Killed    -- ^ the faction was eliminated
  | Defeated  -- ^ the faction lost the game in another way
  | Camping   -- ^ game is supended
  | Conquer   -- ^ the player won by eliminating all rivals
  | Escape    -- ^ the player escaped the dungeon alive
  | Restart   -- ^ game is restarted
  deriving (Show, Eq, Ord, Enum)

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

-- | Tell whether the faction consists of human civilians.
isCivilianFact :: Kind.COps -> Faction -> Bool
isCivilianFact Kind.COps{cofaction=Kind.Ops{okind}} fact =
  let kind = okind (gkind fact)
  in maybe False (> 0) $ lookup "civilian" $ ffreq kind

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

-- | Tell whether all moving actors of the factions can move at once.
isAllMoveFact :: Kind.COps -> Faction -> Bool
isAllMoveFact Kind.COps{cofaction=Kind.Ops{okind}} fact =
  let kind = okind (gkind fact)
      skillsLeader = fSkillsLeader kind
      skillsOther = fSkillsOther kind
  in EM.findWithDefault 0 Ability.AbMove skillsLeader > 0
     && EM.findWithDefault 0 Ability.AbMove skillsOther > 0

-- | Tell whether a faction that is still in game, keeps arena.
-- Keeping arena means if the faction is still in game,
-- it always has to have a leader in the dungeon somewhere.
-- So, leaderless factions and spawner factions do not keep an arena.
-- Such factions win if they can escape the dungeon.
keepArenaFact :: Faction -> Bool
keepArenaFact fact = playerLeader (gplayer fact) && not (isSpawnFact fact)

-- | Check if factions are at war. Assumes symmetry.
isAtWar :: Faction -> FactionId -> Bool
isAtWar fact fid = War == EM.findWithDefault Unknown fid (gdipl fact)

-- | Check if factions are allied. Assumes symmetry.
isAllied :: Faction -> FactionId -> Bool
isAllied fact fid = Alliance == EM.findWithDefault Unknown fid (gdipl fact)

difficultyBound :: Int
difficultyBound = 9

difficultyDefault :: Int
difficultyDefault = (1 + difficultyBound) `div` 2

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
    put gsha
    put gvictims
  get = do
    gkind <- get
    gname <- get
    gcolor <- get
    gplayer <- get
    gdipl <- get
    gquit <- get
    gleader <- get
    gsha <- get
    gvictims <- get
    return $! Faction{..}

instance Binary Diplomacy where
  put = putWord8 . toEnum . fromEnum
  get = fmap (toEnum . fromEnum) getWord8

instance Binary Outcome where
  put = putWord8 . toEnum . fromEnum
  get = fmap (toEnum . fromEnum) getWord8

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
