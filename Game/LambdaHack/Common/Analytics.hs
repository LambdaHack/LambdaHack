{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Per-actor analytics of personal feats.
module Game.LambdaHack.Common.Analytics
  ( FactionAnalytics, ActorAnalytics, GenerationAnalytics
  , KillMap, Analytics(..), KillHow(..)
  , emptyAnalytics, addFactionKill, addActorKill
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , addKill
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import           GHC.Generics (Generic)

import Game.LambdaHack.Common.Container
import Game.LambdaHack.Common.Types

-- | Summary analytics data for each faction.
type FactionAnalytics = EM.EnumMap FactionId Analytics

-- | Analytics data for each live actor.
type ActorAnalytics = EM.EnumMap ActorId Analytics

-- | Statistics of possible and actual generation of items for each lore kind.
type GenerationAnalytics = EM.EnumMap SLore (EM.EnumMap ItemId Int)

-- | Labels of individual kill count analytics.
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
  deriving (Show, Eq, Enum, Generic)

instance Binary KillHow

type KillMap = EM.EnumMap FactionId (EM.EnumMap ItemId Int)

-- | Statistics of past events concerning an actor.
newtype Analytics = Analytics
  { akillCounts :: EM.EnumMap KillHow KillMap
  }
  deriving (Show, Eq, Binary)

emptyAnalytics :: Analytics
emptyAnalytics = Analytics
  { akillCounts = EM.empty
  }

addKill :: KillHow -> FactionId -> ItemId -> Maybe Analytics -> Analytics
addKill killHow fid iid =
  let f Nothing = Analytics {akillCounts =
        EM.singleton killHow $ EM.singleton fid $ EM.singleton iid 1}
      f (Just an) = an {akillCounts =
        EM.alter g killHow $ akillCounts an}
      g Nothing = Just $ EM.singleton fid $ EM.singleton iid 1
      g (Just fidMap) = Just $ EM.alter h fid fidMap
      h Nothing = Just $ EM.singleton iid 1
      h (Just iidMap) = Just $ EM.alter i iid iidMap
      i Nothing = Just 1
      i (Just n) = Just $ n + 1
  in f

addFactionKill :: FactionId -> KillHow -> FactionId -> ItemId
               -> FactionAnalytics
               -> FactionAnalytics
addFactionKill fidOfKiller killHow fid iid =
  EM.alter (Just . addKill killHow fid iid) fidOfKiller

addActorKill :: ActorId -> KillHow -> FactionId -> ItemId
             -> ActorAnalytics
             -> ActorAnalytics
addActorKill aid killHow fid iid =
  EM.alter (Just . addKill killHow fid iid) aid
