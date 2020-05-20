-- | General content types and operations.
module Game.LambdaHack.Common.Kind
  ( ContentData, COps(..)
  , emptyCOps
  , ItemSpeedup
  , emptyItemSpeedup, getKindMean, speedupItem
  , TileSpeedup(..), Tab(..)
  , emptyTileSpeedup, emptyTab
  , okind, omemberGroup, oisSingletonGroup, ouniqGroup, opick
  , ofoldlWithKey', ofoldlGroup', omapVector, oimapVector
  , olength, linearInterpolation
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           Data.Word (Word8)

import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Content.CaveKind
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Content.PlaceKind
import           Game.LambdaHack.Content.RuleKind
import           Game.LambdaHack.Content.TileKind (TileKind)
import           Game.LambdaHack.Definition.ContentData
import           Game.LambdaHack.Definition.Defs

-- | Operations for all content types, gathered together.
data COps = COps
  { cocave        :: ContentData CaveKind   -- server only
  , coitem        :: ContentData ItemKind
  , comode        :: ContentData ModeKind   -- server only
  , coplace       :: ContentData PlaceKind  -- server only, so far
  , corule        :: RuleContent
  , cotile        :: ContentData TileKind
  , coItemSpeedup :: ItemSpeedup
  , coTileSpeedup :: TileSpeedup
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
  , coTileSpeedup = emptyTileSpeedup
  }

-- | Map from an item kind identifier to the mean aspect value for the kind.
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

-- | A lot of tabulated maps from tile kind identifier to a property
-- of the tile kind.
data TileSpeedup = TileSpeedup
  { isClearTab          :: Tab Bool
  , isLitTab            :: Tab Bool
  , isHideoutTab        :: Tab Bool
  , isWalkableTab       :: Tab Bool
  , isDoorTab           :: Tab Bool
  , isOpenableTab       :: Tab Bool
  , isClosableTab       :: Tab Bool
  , isChangableTab      :: Tab Bool
  , isModifiableWithTab :: Tab Bool
  , isSuspectTab        :: Tab Bool
  , isHideAsTab         :: Tab Bool
  , consideredByAITab   :: Tab Bool
  , isVeryOftenItemTab  :: Tab Bool
  , isCommonItemTab     :: Tab Bool
  , isOftenActorTab     :: Tab Bool
  , isNoItemTab         :: Tab Bool
  , isNoActorTab        :: Tab Bool
  , isEasyOpenTab       :: Tab Bool
  , isEmbedTab          :: Tab Bool
  , isAquaticTab        :: Tab Bool
  , alterMinSkillTab    :: Tab Word8
  , alterMinWalkTab     :: Tab Word8
  }

-- Vectors of booleans can be slower than arrays, because they are not packed,
-- but with growing cache sizes they may as well turn out faster at some point.
-- The advantage of vectors are exposed internals, in particular unsafe
-- indexing. Also, in JS, bool arrays are obviously not packed.
-- An option: https://github.com/Bodigrim/bitvec
-- | A map morally indexed by @ContentId TileKind@.
newtype Tab a = Tab (U.Vector a)

emptyTileSpeedup :: TileSpeedup
emptyTileSpeedup = TileSpeedup emptyTab emptyTab emptyTab emptyTab emptyTab
                               emptyTab emptyTab emptyTab emptyTab emptyTab
                               emptyTab emptyTab emptyTab emptyTab emptyTab
                               emptyTab emptyTab emptyTab emptyTab emptyTab
                               emptyTab emptyTab

emptyTab :: U.Unbox a => Tab a
emptyTab = Tab $! U.empty
