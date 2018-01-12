-- | General content types and operations.
module Game.LambdaHack.Common.Kind
  ( COps(..), getStdRuleset
  , ContentData
  , okind, ouniqGroup, opick
  , ofoldrWithKey, ofoldlWithKey', ofoldlGroup', olength
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.ContentData
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.PlaceKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind

-- | Operations for all content types, gathered together.
data COps = COps
  { cocave        :: ContentData CaveKind   -- server only
  , coitem        :: ContentData ItemKind
  , comode        :: ContentData ModeKind   -- server only
  , coplace       :: ContentData PlaceKind  -- server only, so far
  , corule        :: ContentData RuleKind
  , cotile        :: ContentData TileKind
  , coTileSpeedup :: TileSpeedup
  }

instance Show COps where
  show _ = "game content"

instance Eq COps where
  (==) _ _ = True

-- | The standard ruleset used for level operations.
getStdRuleset :: COps -> RuleKind
getStdRuleset COps{corule} = okind corule $ ouniqGroup corule "standard"
