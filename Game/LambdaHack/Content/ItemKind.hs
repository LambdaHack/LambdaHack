-- | The type of kinds of weapons, treasure, organs, shrapnel and actors.
module Game.LambdaHack.Content.ItemKind
  ( ItemKind(..), toVelocity, toLinger
  , validateSingleItemKind, validateAllItemKind
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
  , ikit     :: ![(GroupName, CStore)]  -- ^ accompanying organs and items
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound

toVelocity :: Int -> Effect.Feature
toVelocity n = Effect.ToThrow $ Effect.ThrowMod n 100

toLinger :: Int -> Effect.Feature
toLinger n = Effect.ToThrow $ Effect.ThrowMod 100 n

-- | Catch invalid item kind definitions.
validateSingleItemKind :: ItemKind -> [Text]
validateSingleItemKind ItemKind{..} =
  let sortedRarity = sortBy (comparing fst) irarity
  in [ "iname longer than 23" | T.length iname > 23 ]
     ++ [ "irarity not sorted" | sortedRarity /= irarity ]
     ++ [ "irarity depth thresholds not unique"
        | nubBy ((==) `on` fst) sortedRarity /= sortedRarity ]
     ++ [ "irarity depth not between 1 and 10"
        | case (sortedRarity, reverse sortedRarity) of
            ((lowest, _) : _, (highest, _) : _) ->
              lowest < 1 || highest > 10
            _ -> False ]

-- TODO: if "treasure" stays wired-in, assure there are some treasure items
-- TODO: check that there is at least one item in each ifreq group
-- for each level (thought more precisely we'd need to lookup caves and modes
-- and only check at the levels the caves can appear at).
-- | Validate all item kinds.
validateAllItemKind :: [ItemKind] -> [Text]
validateAllItemKind _ = []
