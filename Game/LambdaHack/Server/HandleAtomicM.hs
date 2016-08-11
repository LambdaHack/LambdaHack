{-# LANGUAGE TupleSections #-}
-- | Handle atomic commands before they are executed to change State
-- and sent to clients.
module Game.LambdaHack.Server.HandleAtomicM
  ( cmdAtomicSemSer
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Perception
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Content.ItemKind (ItemKind)
import Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Content.TileKind as TK
import Game.LambdaHack.Server.CommonM
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.State

-- | Effect of atomic actions on server state is calculated
-- with the global state from before the command is executed.
cmdAtomicSemSer :: MonadServer m => UpdAtomic -> m ()
cmdAtomicSemSer cmd = case cmd of
{-  UpdCreateActor aid body _ -> createActor aid body
  UpdDestroyActor aid b _ -> destroyActor aid b True
  UpdCreateItem _iid _ _ (CActor aid s) -> wipeBfsIfItemAffectsSkills [s] aid
  UpdDestroyItem _iid _ _ (CActor aid s) -> wipeBfsIfItemAffectsSkills [s] aid
  UpdSpotActor aid body _ -> createActor aid body
  UpdLoseActor aid b _ -> destroyActor aid b False
  UpdSpotItem _iid _ _ (CActor aid s) -> wipeBfsIfItemAffectsSkills [s] aid
  UpdLoseItem _iid _ _ (CActor aid s) -> wipeBfsIfItemAffectsSkills [s] aid
  UpdMoveActor aid _ _ -> invalidateBfsAid aid
  UpdDisplaceActor source target -> do
    invalidateBfsAid source
    invalidateBfsAid target
  UpdMoveItem _iid _ aid s1 s2 -> wipeBfsIfItemAffectsSkills [s1, s2] aid
  UpdLeadFaction fid source target -> do
    side <- getsClient sside
    when (side == fid) $ do
      mleader <- getsClient _sleader
      let !_A = assert (mleader == fmap fst source
                          -- somebody changed the leader for us
                        || mleader == fmap fst target
                          -- we changed the leader ourselves
                        `blame` "unexpected leader"
                        `twith` (cmd, mleader)) ()
      modifyClient $ \cli -> cli {_sleader = fmap fst target}
      case target of
        Nothing -> return ()
        Just (aid, mtgt) -> do
          let addNoPath tapTgt = TgtAndPath{tapTgt, tapPath=NoPath}
          modifyClient $ \cli ->
            cli {stargetD = EM.alter (const $ addNoPath <$> mtgt)
                                     aid (stargetD cli)}
  UpdAutoFaction{} -> do
    -- @condBFS@ depends on the setting we change here.
    invalidateBfsAll
    -- Clear all targets except the leader's.
    mleader <- getsClient _sleader
    mtgt <- case mleader of
      Nothing -> return Nothing
      Just leader -> getsClient $ EM.lookup leader . stargetD
    modifyClient $ \cli ->
      cli { stargetD = case (mtgt, mleader) of
              (Just tgt, Just leader) -> EM.singleton leader tgt
              _ -> EM.empty }
-}
  UpdAlterTile lid pos _ toTile -> do
    updateSclear lid pos toTile
    updateSlit lid pos toTile
{-
    cops <- getsState scops
    when (tileChangeAffectsBfs cops fromTile toTile) $
      invalidateBfsLid lid
  UpdDiscover c iid ik seed ldepth -> do
    discoverKind c iid ik
    discoverSeed c iid seed ldepth
  UpdCover c iid ik seed _ldepth -> do
    coverSeed c iid seed
    coverKind c iid ik
  UpdDiscoverKind c iid ik -> discoverKind c iid ik
  UpdCoverKind c iid ik -> coverKind c iid ik
  UpdDiscoverSeed c iid seed  ldepth -> discoverSeed c iid seed ldepth
  UpdCoverSeed c iid seed _ldepth -> coverSeed c iid seed
  UpdPerception lid outPer inPer -> perception lid outPer inPer
  UpdRestart side sdiscoKind sfper s d sdebugCli -> do
    sisAI <- getsClient sisAI
    snxtDiff <- getsClient snxtDiff
    let cli = (emptyStateClient side) {sisAI}
    putClient cli { sdiscoKind
                  , sfper
                  -- , sundo = [UpdAtomic cmd]
                  , scurDiff = d
                  , snxtDiff
                  , sdebugCli }
    createSalter s
    restartClient
  UpdResume _fid sfper -> do
    modifyClient $ \cli -> cli {sfper}
    s <- getState
    createSalter s
  UpdKillExit _fid -> killExit
  UpdWriteSave -> saveClient
-}
  _ -> return ()
