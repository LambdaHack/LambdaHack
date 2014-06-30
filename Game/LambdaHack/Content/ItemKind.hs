-- | The type of kinds of weapons, treasure, organs, shrapnel and actors.
module Game.LambdaHack.Content.ItemKind
  ( ItemKind(..), toVelocity, toLinger, validateItemKind
  ) where

import Data.Function
import Data.List
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import qualified Game.LambdaHack.Common.Dice as Dice
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc

-- | Item properties that are fixed for a given kind of items.
data ItemKind = ItemKind
  { isymbol  :: !Char              -- ^ map symbol
  , iname    :: !Text              -- ^ generic name
  , ifreq    :: !Freqs             -- ^ frequency within groups
  , iflavour :: ![Flavour]         -- ^ possible flavours
  , icount   :: !Dice.Dice         -- ^ created in that quantity
  , irarity  :: ![(Int, Int)]      -- ^ rarity on given depths
  , iverbHit :: !MU.Part           -- ^ the verb for applying and melee
  , iweight  :: !Int               -- ^ weight in grams
  , iaspects :: ![Effect.Aspect Dice.Dice]
                                   -- ^ keep the aspect continuously
  , ieffects :: ![Effect.Effect Dice.Dice]
                                   -- ^ cause the effect when triggered
  , ifeature :: ![Effect.Feature]  -- ^ public properties
  , idesc    :: !Text              -- ^ description
  , ikit     :: ![(Text, CStore)]  -- ^ accompanying organs and items
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound, see #53

toVelocity :: Int -> Effect.Feature
toVelocity n = Effect.ToThrow $ Effect.ThrowMod n 100

toLinger :: Int -> Effect.Feature
toLinger n = Effect.ToThrow $ Effect.ThrowMod 100 n

-- | Filter a list of kinds, passing through only the incorrect ones, if any.
validateItemKind :: [ItemKind] -> [ItemKind]
validateItemKind l =
  let bad ik = T.length (iname ik) > 23
               || let sortedRarity = sortBy (comparing fst) (irarity ik)
                  in sortedRarity /= irarity ik
                     || nubBy ((==) `on` fst) sortedRarity /= sortedRarity
                     || case (sortedRarity, reverse sortedRarity) of
                       ((lowest, _) : _, (highest, _) : _) ->
                         lowest < 1 || highest > 10
                       _ -> False
  in filter bad l
