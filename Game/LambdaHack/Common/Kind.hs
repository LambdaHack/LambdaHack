-- | General content types and operations.
module Game.LambdaHack.Common.Kind
  ( ContentData, COps(..)
  , emptyCOps
  , ItemSpeedup
  , emptyItemSpeedup, getKindMean, speedupItem
  , okind, omemberGroup, oisSingletonGroup, ouniqGroup, opick
  , ofoldlWithKey', ofoldlGroup', omapVector, oimapVector
  , olength, linearInterpolation
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Vector as V

import           Game.LambdaHack.Common.ContentData
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Content.CaveKind
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Content.PlaceKind
import           Game.LambdaHack.Content.RuleKind
import           Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Content.TileKind as TK

-- | Operations for all content types, gathered together.
data COps = COps
  { cocave        :: ContentData CaveKind   -- server only
  , coitem        :: ContentData ItemKind
  , comode        :: ContentData ModeKind   -- server only
  , coplace       :: ContentData PlaceKind  -- server only, so far
  , corule        :: RuleContent
  , cotile        :: ContentData TileKind
  , coItemSpeedup :: ItemSpeedup
  , coTileSpeedup :: TK.TileSpeedup
  }

instance Show COps where
  show _ = "game content"

instance Eq COps where
  (==) _ _ = True

emptyCOps :: COps
emptyCOps = COps
  { cocave  = emptyContentData
  , coitem  = emptyContentData
  , comode  = emptyContentData
  , coplace = emptyContentData
  , corule  = emptyRuleContent
  , cotile  = emptyContentData
  , coItemSpeedup = emptyItemSpeedup
  , coTileSpeedup = TK.emptyTileSpeedup
  }

-- | Map from an item kind identifier to the mean aspect value for the kind.
--
-- Significant portions of this map are unused and so intentially kept
-- unevaluated.
newtype ItemSpeedup = ItemSpeedup (V.Vector IA.KindMean)

emptyItemSpeedup :: ItemSpeedup
emptyItemSpeedup = ItemSpeedup V.empty

getKindMean :: ContentId IK.ItemKind -> ItemSpeedup -> IA.KindMean
getKindMean kindId (ItemSpeedup is) = is V.! contentIdIndex kindId

speedupItem :: ContentData IK.ItemKind -> ItemSpeedup
speedupItem coitem =
  let f !kind =
        let kmMean = IA.meanAspect kind
            kmConst = not $ IA.aspectsRandom (IK.iaspects kind)
        in IA.KindMean{..}
  in ItemSpeedup $ omapVector coitem f
