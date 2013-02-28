{-# LANGUAGE OverloadedStrings #-}
-- | The type of kinds of weapons and treasure.
module Game.LambdaHack.Content.ItemKind
  ( ItemKind(..), ivalidate
  ) where

import Data.Text (Text)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Effect
import Game.LambdaHack.Flavour
import Game.LambdaHack.Misc
import Game.LambdaHack.Random

-- | Item properties that are fixed for a given kind of items.
data ItemKind = ItemKind
  { isymbol      :: !Char         -- ^ map symbol
  , iname        :: !Text         -- ^ generic name
  , ifreq        :: !Freqs        -- ^ frequency within groups
  , iflavour     :: ![Flavour]    -- ^ possible flavours
  , ieffect      :: !Effect       -- ^ the effect when activated
  , icount       :: !RollDeep     -- ^ created in that quantify
  , iverbApply   :: !MU.Part  -- ^ the verb for applying and possibly combat
  , iverbProject :: !MU.Part  -- ^ the verb for projecting
  , iweight      :: !Int          -- ^ weight in grams
  , itoThrow     :: !Int          -- ^ percentage bonus or malus to throw speed
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound, see #53

-- | No specific possible problems for the content of this kind, so far,
-- so the validation function always returns the empty list of offending kinds.
ivalidate :: [ItemKind] -> [ItemKind]
ivalidate _ = []
