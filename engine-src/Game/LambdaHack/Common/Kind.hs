{-# LANGUAGE TupleSections #-}
-- | General content types and operations.
module Game.LambdaHack.Common.Kind
  ( ContentData  -- re-exported without some operations
  , COps(..)
  , emptyCOps
  , ItemSpeedup
  , getKindMean, speedupItem
  , okind, omemberGroup, oisSingletonGroup, ouniqGroup, opick
  , ofoldlWithKey', ofoldlGroup', omapVector, oimapVector
  , olength, linearInterpolation, emptyMultiGroupMode, emptyMultiGroupItem
#ifdef EXPOSE_INTERNAL
  , emptyUnknownTile
  , emptyUIFactionGroupName
#endif
    -- * Operations both internal and used in unit tests
  , emptyUIFaction
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Vector as V

import qualified Game.LambdaHack.Common.ItemAspect as IA
import qualified Game.LambdaHack.Common.Tile as Tile
import qualified Game.LambdaHack.Content.CaveKind as CK
import qualified Game.LambdaHack.Content.FactionKind as FK
import qualified Game.LambdaHack.Content.ItemKind as IK
import qualified Game.LambdaHack.Content.ModeKind as MK
import qualified Game.LambdaHack.Content.PlaceKind as PK
import qualified Game.LambdaHack.Content.RuleKind as RK
import qualified Game.LambdaHack.Content.TileKind as TK
import qualified Game.LambdaHack.Definition.Ability as Ability
import qualified Game.LambdaHack.Definition.Color as Color
import           Game.LambdaHack.Definition.ContentData
import           Game.LambdaHack.Definition.Defs
  (ContentId, contentIdIndex, linearInterpolation)
import           Game.LambdaHack.Definition.DefsInternal (toContentSymbol, GroupName (..))
import           Game.LambdaHack.Definition.Flavour (dummyFlavour)

-- | Operations for all content types, gathered together.
--
-- Warning: this type is not abstract, but its values should not be
-- created ad hoc, even for unit tests, but should be constructed
-- with @makeData@ for each particular content kind, which includes validation,
-- and with @speedupItem@, etc., to ensure internal consistency.
--
-- The @emptyCOps@ is one such valid by construction value of this type,
-- except for the @cocave@ field. It's suitable for bootstrapping
-- and for tests not involving dungeon generation from cave templates.
data COps = COps
  { cocave        :: ContentData CK.CaveKind   -- server only
  , cofact        :: ContentData FK.FactionKind
  , coitem        :: ContentData IK.ItemKind
  , comode        :: ContentData MK.ModeKind   -- server only
  , coplace       :: ContentData PK.PlaceKind  -- server only, so far
  , corule        :: RK.RuleContent
  , cotile        :: ContentData TK.TileKind
  , coItemSpeedup :: ItemSpeedup
  , coTileSpeedup :: Tile.TileSpeedup
  }

instance Show COps where
  show _ = "game content"

instance Eq COps where
  (==) _ _ = True

emptyMultiGroupItem :: IK.ItemKind
emptyMultiGroupItem = IK.ItemKind
  { isymbol  = toContentSymbol 'E'
  , iname    = "emptyCOps item"
  , ifreq    = map (, 1) $ IK.mandatoryGroups ++ IK.mandatoryGroupsSingleton
  , iflavour = [dummyFlavour]
  , icount   = 0
  , irarity  = []
  , iverbHit = ""
  , iweight  = 0
  , idamage  = 0
  , iaspects = []
  , ieffects = []
  , idesc    = ""
  , ikit     = []
  }

emptyUnknownTile :: TK.TileKind
emptyUnknownTile = TK.TileKind  -- needs to have index 0 and alter 1
  { tsymbol  = 'E'
  , tname    = "unknown space"  -- name checked in validation
  , tfreq    = map (, 1) $ TK.mandatoryGroups ++ TK.mandatoryGroupsSingleton
  , tcolor   = Color.BrMagenta
  , tcolor2  = Color.BrMagenta
  , talter   = 1
  , tfeature = []
  }

emptyUIFactionGroupName :: GroupName FK.FactionKind
emptyUIFactionGroupName = GroupName "emptyUIFaction"

emptyUIFaction :: FK.FactionKind
emptyUIFaction = FK.FactionKind
  { fname = "emptyUIFaction"
  , ffreq = [(emptyUIFactionGroupName, 1)]
  , fteam = FK.TeamContinuity 999  -- must be > 0
  , fgroups = []
  , fskillsOther = Ability.zeroSkills
  , fcanEscape = False
  , fneverEmpty = True  -- to keep the dungeon alive
  , fhiCondPoly = []
  , fhasGender = False
  , finitDoctrine = Ability.TBlock
  , fspawnsFast = False
  , fhasPointman = False
  , fhasUI = True  -- to own the UI frontend
  , finitUnderAI = False
  , fenemyTeams = []
  , falliedTeams = []
  }

emptyMultiGroupMode :: MK.ModeKind
emptyMultiGroupMode = MK.ModeKind
  { mname   = "emptyMultiGroupMode"
  , mfreq   = map (, 1) MK.mandatoryGroups
  , mtutorial = False
  , mattract = False
  , mroster = [(emptyUIFactionGroupName, [])]
  , mcaves  = []
  , mendMsg = []
  , mrules  = ""
  , mdesc   = ""
  , mreason = ""
  , mhint   = ""
  }

-- | This is as empty, as possible, but still valid content, except for
-- @cocave@ which is empty and not valid (making it valid would require
-- bloating most other contents).
emptyCOps :: COps
emptyCOps =
  let corule = RK.emptyRuleContent
      coitem = IK.makeData (RK.ritemSymbols corule) [emptyMultiGroupItem] [] []
      cotile = TK.makeData [emptyUnknownTile] [] []
      cofact = FK.makeData [emptyUIFaction] [emptyUIFactionGroupName] []
  in COps
    { cocave = emptyContentData  -- not valid! beware when testing!
        -- to make valid cave content, we'd need to define a single cave kind,
        -- which involves creating and validating tile and place kinds, etc.
    , cofact
    , coitem
    , comode = MK.makeData cofact [emptyMultiGroupMode] [] []
    , coplace = PK.makeData cotile [] [] []
    , corule
    , cotile
    , coItemSpeedup = speedupItem coitem
    , coTileSpeedup = Tile.speedupTile False cotile
    }

-- | Map from an item kind identifier to the mean aspect value for the kind.
newtype ItemSpeedup = ItemSpeedup (V.Vector IA.KindMean)

getKindMean :: ContentId IK.ItemKind -> ItemSpeedup -> IA.KindMean
getKindMean kindId (ItemSpeedup is) = is V.! contentIdIndex kindId

speedupItem :: ContentData IK.ItemKind -> ItemSpeedup
speedupItem coitem =
  let f !kind =
        let kmMean = IA.meanAspect kind
            kmConst = not $ IA.aspectsRandom (IK.iaspects kind)
        in IA.KindMean{..}
  in ItemSpeedup $ omapVector coitem f
