{-# LANGUAGE DeriveGeneric #-}
-- | UI aspects of actors.
module Game.LambdaHack.Client.UI.ActorUI
  ( ActorUI(..), ActorDictUI
  , keySelected, partActor, partPronoun
  , ppContainer, ppCStore, ppCStoreIn, ppCStoreWownW
  , ppContainerWownW, verbCStore, tryFindHeroK
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Common.Actor
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.State

data ActorUI = ActorUI
  { bsymbol  :: Char         -- ^ individual map symbol
  , bname    :: Text         -- ^ individual name
  , bpronoun :: Text         -- ^ individual pronoun
  , bcolor   :: Color.Color  -- ^ individual map color
  }
  deriving (Show, Eq, Generic)

instance Binary ActorUI

type ActorDictUI = EM.EnumMap ActorId ActorUI

keySelected :: (ActorId, Actor, ActorUI)
            -> (Bool, Bool, Char, Color.Color, ActorId)
keySelected (aid, Actor{bhp}, ActorUI{bsymbol, bcolor}) =
  (bhp > 0, bsymbol /= '@', bsymbol, bcolor, aid)

-- | The part of speech describing the actor.
partActor :: ActorUI -> MU.Part
partActor b = MU.Text $ bname b

-- | The part of speech containing the actor pronoun.
partPronoun :: ActorUI -> MU.Part
partPronoun b = MU.Text $ bpronoun b

ppContainer :: Container -> Text
ppContainer CFloor{} = "nearby"
ppContainer CEmbed{} = "embedded nearby"
ppContainer (CActor _ cstore) = ppCStoreIn cstore
ppContainer c@CTrunk{} = error $ "" `showFailure` c

ppCStore :: CStore -> (Text, Text)
ppCStore CGround = ("on", "the ground")
ppCStore COrgan = ("among", "organs")
ppCStore CEqp = ("in", "equipment")
ppCStore CInv = ("in", "pack")
ppCStore CSha = ("in", "shared stash")

ppCStoreIn :: CStore -> Text
ppCStoreIn c = let (tIn, t) = ppCStore c in tIn <+> t

ppCStoreWownW :: Bool -> CStore -> MU.Part -> [MU.Part]
ppCStoreWownW addPrepositions store owner =
  let (preposition, noun) = ppCStore store
      prep = [MU.Text preposition | addPrepositions]
  in prep ++ case store of
    CGround -> [MU.Text noun, "under", owner]
    CSha -> [MU.Text noun]
    _ -> [MU.WownW owner (MU.Text noun) ]

ppContainerWownW :: (ActorId -> MU.Part) -> Bool -> Container -> [MU.Part]
ppContainerWownW ownerFun addPrepositions c = case c of
  CFloor{} -> ["nearby"]
  CEmbed{} -> ["embedded nearby"]
  CActor aid store -> let owner = ownerFun aid
                      in ppCStoreWownW addPrepositions store owner
  CTrunk{} -> error $ "" `showFailure` c

verbCStore :: CStore -> Text
verbCStore CGround = "drop"
verbCStore COrgan = "implant"
verbCStore CEqp = "equip"
verbCStore CInv = "pack"
verbCStore CSha = "stash"

-- | Tries to finds an actor body satisfying a predicate on any level.
tryFindActor :: State -> (ActorId -> Actor -> Bool) -> Maybe (ActorId, Actor)
tryFindActor s p = find (uncurry p) $ EM.assocs $ sactorD s

tryFindHeroK :: ActorDictUI -> FactionId -> Int -> State
             -> Maybe (ActorId, Actor)
tryFindHeroK d fid k s =
  let c | k == 0          = '@'
        | k > 0 && k < 10 = Char.intToDigit k
        | otherwise       = ' '  -- no hero with such symbol
  in tryFindActor s (\aid body ->
       maybe False ((== c) . bsymbol) (EM.lookup aid d)
       && bfid body == fid)
