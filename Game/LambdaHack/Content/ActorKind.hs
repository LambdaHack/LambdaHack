-- | The type of kinds of monsters and heroes.
module Game.LambdaHack.Content.ActorKind
  ( ActorKind(..), avalidate
  ) where

import qualified Data.List as L
import qualified Data.Ord as Ord

import Game.LambdaHack.Color
import qualified Game.LambdaHack.Random as Random
import Game.LambdaHack.Misc

-- TODO: make all but a few fields optional in some way, so that, a.g.,
-- a game content with no regeneration does not ever need to mention aregen.
-- | Actor properties that are fixed for a given kind of actors.
data ActorKind = ActorKind
  { asymbol :: !Char             -- ^ map symbol
  , aname   :: !String           -- ^ short description
  , afreq   :: !Freqs            -- ^ frequency within groups
  , acolor  :: !Color            -- ^ map color
  , aspeed  :: !Int              -- ^ natural speed in m/10s (1 step is 0.5 s)
  , ahp     :: !Random.RollDice  -- ^ encodes initial and maximal hp
  , asight  :: !Bool             -- ^ can it see?
  , asmell  :: !Bool             -- ^ can it smell?
  , aiq     :: !Int              -- ^ intelligence
  , aregen  :: !Time             -- ^ regeneration interval in time ticks
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound, see #53

-- | Filter a list of kinds, passing through only the incorrect ones, if any.
--
-- Make sure actor kinds can be told apart on the level map.
avalidate :: [ActorKind] -> [ActorKind]
avalidate l =
  let cmp = Ord.comparing (\ ka -> (asymbol ka, acolor ka))
      eq ka1 ka2 = cmp ka1 ka2 == Ord.EQ
      sorted = L.sortBy cmp l
      nubbed = L.nubBy eq sorted
  in L.deleteFirstsBy eq sorted nubbed
