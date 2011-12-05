module Game.LambdaHack.Content.ActorKind
  ( ActorKind(..), validActor
  ) where

import Game.LambdaHack.Color
import qualified Game.LambdaHack.Geometry as Geometry
import qualified Game.LambdaHack.Random as Random

-- | Monster properties that are changing rarely and permanently.
data ActorKind = ActorKind
  { asymbol :: !Char             -- ^ map symbol
  , aname   :: !String           -- ^ name
  , afreq   :: !Int              -- ^ dungeon frequency
  , aspeed  :: !Geometry.Time    -- ^ natural speed
  , ahp     :: !Random.RollDice  -- ^ encodes initial and maximal hp
  , acolor  :: !Color            -- ^ map color
  , asight  :: !Bool             -- ^ can it see?
  , asmell  :: !Bool             -- ^ can it smell?
  , aiq     :: !Int              -- ^ intelligence
  , aregen  :: !Int              -- ^ regeneration interval
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound, see #53

validActor :: ActorKind -> Bool
validActor ActorKind{..} = True -- TODO
