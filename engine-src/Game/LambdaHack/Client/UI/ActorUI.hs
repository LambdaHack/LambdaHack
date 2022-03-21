{-# LANGUAGE DeriveGeneric #-}
-- | UI aspects of actors.
module Game.LambdaHack.Client.UI.ActorUI
  ( ActorUI(..), ActorDictUI
  , keySelected, partActor, partPronoun, tryFindActor, tryFindHeroK
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import           GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Common.Types
import qualified Game.LambdaHack.Definition.Color as Color
import Game.LambdaHack.Definition.DefsInternal
import Game.LambdaHack.Content.ItemKind

data ActorUI = ActorUI
  { bsymbol  :: ContentSymbol ItemKind  -- ^ individual map symbol
  , bname    :: Text           -- ^ individual name
  , bpronoun :: Text           -- ^ individual pronoun
  , bcolor   :: Color.Color    -- ^ individual map color
  }
  deriving (Show, Eq, Generic)

instance Binary ActorUI

type ActorDictUI = EM.EnumMap ActorId ActorUI

keySelected :: (ActorId, Actor, ActorUI)
            -> (Bool, Bool, Bool, ContentSymbol ItemKind, Color.Color, ActorId)
keySelected (aid, Actor{bhp, bwatch}, ActorUI{bsymbol, bcolor}) =
  ( bhp > 0
  , bwatch /= WSleep
  , bsymbol /= toContentSymbol '@'
  , bsymbol
  , bcolor
  , aid)

-- | The part of speech describing the actor.
partActor :: ActorUI -> MU.Part
partActor b = MU.Text $ bname b

-- | The part of speech containing the actor's pronoun.
partPronoun :: ActorUI -> MU.Part
partPronoun b = MU.Text $ bpronoun b

tryFindActor :: State -> (ActorId -> Actor -> Bool) -> Maybe (ActorId, Actor)
tryFindActor s p = find (uncurry p) $ EM.assocs $ sactorD s

tryFindHeroK :: ActorDictUI -> FactionId -> Int -> State
             -> Maybe (ActorId, Actor)
tryFindHeroK d fid k s =
  let c | k == 0          = '@'
        | k > 0 && k < 10 = Char.intToDigit k
        | otherwise       = ' '  -- no hero with such symbol
      theSymbol = toContentSymbol c
  in tryFindActor s (\aid body ->
       maybe False ((== theSymbol) . bsymbol) (EM.lookup aid d)
       && bfid body == fid)
