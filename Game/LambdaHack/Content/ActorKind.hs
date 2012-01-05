module Game.LambdaHack.Content.ActorKind
  ( ActorKind(..), avalidate
  ) where

import qualified Data.List as L
import qualified Data.Ord as Ord

import Game.LambdaHack.Color
import qualified Game.LambdaHack.Geometry as Geometry
import qualified Game.LambdaHack.Random as Random

-- | Monster properties that are changing rarely and permanently.
data ActorKind = ActorKind
  { asymbol :: !Char             -- ^ map symbol
  , aname   :: !String           -- ^ name
  , afreq   :: !Int              -- ^ created that often
  , aspeed  :: !Geometry.Time    -- ^ natural speed
  , ahp     :: !Random.RollDice  -- ^ encodes initial and maximal hp
  , acolor  :: !Color            -- ^ map color
  , asight  :: !Bool             -- ^ can it see?
  , asmell  :: !Bool             -- ^ can it smell?
  , aiq     :: !Int              -- ^ intelligence
  , aregen  :: !Int              -- ^ regeneration interval
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound, see #53

-- | Make sure actor kinds can be told apart on the map.
avalidate :: [ActorKind] -> [ActorKind]
avalidate l =
  let cmp = Ord.comparing (\ ka -> (asymbol ka, acolor ka))
      eq ka1 ka2 = cmp ka1 ka2 == Ord.EQ
      sorted = L.sortBy cmp l
      nubbed = L.nubBy eq sorted
  in L.deleteFirstsBy eq sorted nubbed
