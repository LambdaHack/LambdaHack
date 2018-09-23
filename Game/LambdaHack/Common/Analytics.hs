{-# LANGUAGE DeriveGeneric #-}
-- | Per-actor analytics of personal feats.
module Game.LambdaHack.Common.Analytics
  ( ActorAnalytics, KillMap, Analytics(..), KillHow(..)
  , emptyAnalytics
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import           GHC.Generics (Generic)

import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item

-- | Analytics data for each live actor.
type ActorAnalytics = EM.EnumMap ActorId Analytics

type KillMap = EM.EnumMap FactionId (EM.EnumMap ItemId Int)

-- | Statistics of past events concerning an actor.
data Analytics = Analytics
  { akillKineticMelee  :: KillMap
  , akillKineticRanged :: KillMap
  , akillKineticBlast  :: KillMap
  , akillKineticPush   :: KillMap
  , akillOtherMelee    :: KillMap
  , akillOtherRanged   :: KillMap
  , akillOtherBlast    :: KillMap
  , akillOtherPush     :: KillMap
  , akillActorLaunch   :: KillMap
  , akillTileLaunch    :: KillMap
  , akillDropLaunch    :: KillMap
  , akillCatch         :: KillMap
  }
  deriving (Show, Generic)

instance Binary Analytics

data KillHow =
    KillKineticMelee
  | KillKineticRanged
  | KillKineticBlast
  | KillKineticPush
  | KillOtherMelee
  | KillOtherRanged
  | KillOtherBlast
  | KillOtherPush
  | KillActorLaunch
  | KillTileLaunch
  | KillDropLaunch
  | KillCatch

emptyAnalytics :: Analytics
emptyAnalytics = Analytics
  { akillKineticMelee  = EM.empty
  , akillKineticRanged = EM.empty
  , akillKineticBlast  = EM.empty
  , akillKineticPush   = EM.empty
  , akillOtherMelee    = EM.empty
  , akillOtherRanged   = EM.empty
  , akillOtherBlast    = EM.empty
  , akillOtherPush     = EM.empty
  , akillActorLaunch   = EM.empty
  , akillTileLaunch    = EM.empty
  , akillDropLaunch    = EM.empty
  , akillCatch         = EM.empty
  }
