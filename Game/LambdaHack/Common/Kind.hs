-- | General content types and operations.
module Game.LambdaHack.Common.Kind
  ( Ops(..), COps(..), stdRuleset
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
  { cocave        :: Ops CaveKind   -- server only
  , coitem        :: Ops ItemKind
  , comode        :: Ops ModeKind   -- server only
  , coplace       :: Ops PlaceKind  -- server only, so far
  , corule        :: Ops RuleKind
  , cotile        :: Ops TileKind
  , coTileSpeedup :: TileSpeedup
  }

instance Show COps where
  show _ = "game content"

instance Eq COps where
  (==) _ _ = True

-- | The standard ruleset used for level operations.
stdRuleset :: Ops RuleKind -> RuleKind
stdRuleset Ops{ouniqGroup, okind} = okind $ ouniqGroup "standard"
