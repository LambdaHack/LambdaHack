-- | The type of kinds of monsters and heroes.
module Game.LambdaHack.Content.ActorKind
  ( ActorKind(..), validateActorKind
  ) where

import Data.Function
import Data.List
import qualified Data.Ord as Ord
import Data.Text (Text)
import qualified Data.Text as T

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Time

-- TODO: make all but a few fields optional in some way, so that, a.g.,
-- a game content with no regeneration does not ever need to mention aregen.
-- | Actor properties that are fixed for a given kind of actors.
data ActorKind = ActorKind
  { -- * Initial presentation
    asymbol  :: !Char       -- ^ map symbol
  , aname    :: !Text       -- ^ short description
  , acolor   :: !Color      -- ^ map color
    -- * Resources
  , amaxHP   :: !Int        -- ^ maximal hp
  , amaxCalm :: !Int        -- ^ maximal calm
  , aspeed   :: !Speed      -- ^ natural speed in m/Ms
  , acanDo   :: ![Ability]  -- ^ the set of supported abilities
    -- * Initial items
  , aitems   :: ![(Text, CStore)]  -- ^ initial items
    -- * Initial distribution
  , afreq    :: !Freqs      -- ^ frequency within groups
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound, see #53

-- | Filter a list of kinds, passing through only the incorrect ones, if any.
--
-- Make sure actor kinds can be told apart on the level map, unless they
-- have the same behaviour and differ only by distribution, flavour
-- and initial values that don't play any role afterwards.
validateActorKind :: [ActorKind] -> [ActorKind]
validateActorKind l =
  let behaviour ka = ( aspeed ka, amaxHP ka, amaxCalm ka
                     , sort $ acanDo ka, sort $ aitems ka )
      sortedBehaviour = sortBy (Ord.comparing behaviour) l
      nubbedBehaviour = nubBy ((==) `on` behaviour) sortedBehaviour
      screen ka = (asymbol ka, acolor ka)
      eqScreen = (==) `on` screen
      sortedScreen = sortBy (Ord.comparing screen) nubbedBehaviour
      nubbedScreen = nubBy eqScreen sortedScreen
      tooSimilar = deleteFirstsBy eqScreen sortedScreen nubbedScreen
      tooVerbose = filter (\ak -> T.length (aname ak) > 23) l
  in tooSimilar ++ tooVerbose
