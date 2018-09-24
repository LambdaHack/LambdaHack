{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Per-actor analytics of personal feats.
module Game.LambdaHack.Common.Analytics
  ( ActorAnalytics, KillMap, Analytics(..), KillHow(..)
  , emptyAnalytics, addKill
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
  deriving (Show, Enum, Generic)

instance Binary KillHow

type KillMap = EM.EnumMap FactionId (EM.EnumMap ItemId Int)

-- | Statistics of past events concerning an actor.
newtype Analytics = Analytics
  { akillCounts :: EM.EnumMap KillHow KillMap
  }
  deriving (Show, Binary)

emptyAnalytics :: Analytics
emptyAnalytics = Analytics
  { akillCounts = EM.empty
  }

addKill :: ActorId -> KillHow -> FactionId -> ItemId
        -> ActorAnalytics
        -> ActorAnalytics
addKill aid killHow fid iid =
  let f Nothing = Just $ Analytics {akillCounts =
        EM.singleton killHow $ EM.singleton fid $ EM.singleton iid 1}
      f (Just an) =
        Just $ an {akillCounts = EM.alter g killHow $ akillCounts an}
      g Nothing = Just $ EM.singleton fid $ EM.singleton iid 1
      g (Just killMap) = Just $ EM.alter h fid killMap
      h Nothing = Just $ EM.singleton iid 1
      h (Just iidMap) = Just $ EM.alter i iid iidMap
      i Nothing = Just 1
      i (Just n) = Just $ n + 1
  in EM.alter f aid
