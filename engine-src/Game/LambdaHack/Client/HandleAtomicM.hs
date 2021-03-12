-- | Handle atomic commands received by the client.
module Game.LambdaHack.Client.HandleAtomicM
  ( MonadClientSetup(..)
  , cmdAtomicSemCli
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , invalidateInMelee, invalidateInMeleeDueToItem
  , wipeBfsIfItemAffectsSkills, tileChangeAffectsBfs
  , createActor, destroyActor
  , addItemToDiscoBenefit, perception
  , discoverKind, discoverKindAndAspect, coverKind, coverAspectAndKind
  , discoverAspect, coverAspect
  , killExit
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M

import           Game.LambdaHack.Atomic
import           Game.LambdaHack.Client.Bfs
import           Game.LambdaHack.Client.BfsM
import           Game.LambdaHack.Client.CommonM
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.Preferences
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Perception
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import qualified Game.LambdaHack.Content.CaveKind as CK
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Content.TileKind (TileKind)
import           Game.LambdaHack.Definition.Defs

-- | Client monad for saving a game.
class MonadClient m => MonadClientSetup m where
  saveClient    :: m ()

-- | Effect of atomic actions on client state. It is calculated
-- with the global state from after the command is executed
-- (except where the supplied @oldState@ is used).
cmdAtomicSemCli :: MonadClientSetup m => State -> UpdAtomic -> m ()
{-# INLINE cmdAtomicSemCli #-}
cmdAtomicSemCli oldState cmd = case cmd of
  UpdRegisterItems ais -> mapM_ (addItemToDiscoBenefit . fst) ais
  UpdCreateActor aid b ais -> createActor aid b ais
  UpdDestroyActor aid b _ -> destroyActor aid b True
  UpdCreateItem _ iid _ _ (CActor aid store) -> do
    wipeBfsIfItemAffectsSkills [store] aid
    addItemToDiscoBenefit iid
    invalidateInMeleeDueToItem aid store
  UpdCreateItem _ iid _ _ _ -> addItemToDiscoBenefit iid
  UpdDestroyItem _ _ _ _ (CActor aid store) -> do
    wipeBfsIfItemAffectsSkills [store] aid
    invalidateInMeleeDueToItem aid store
  UpdDestroyItem{} -> return ()
  UpdSpotActor aid b -> do
    ais <- getsState $ getCarriedAssocsAndTrunk b
    createActor aid b ais
  UpdLoseActor aid b -> destroyActor aid b False
  UpdSpotItem _ iid _ (CActor aid store) -> do
    wipeBfsIfItemAffectsSkills [store] aid
    addItemToDiscoBenefit iid
    invalidateInMeleeDueToItem aid store
  UpdSpotItem _ iid _ _ -> addItemToDiscoBenefit iid
  UpdLoseItem _ _ _ (CActor aid store) -> do
    wipeBfsIfItemAffectsSkills [store] aid
    invalidateInMeleeDueToItem aid store
  UpdLoseItem{} -> return ()
  UpdSpotItemBag _ (CActor aid store) bag -> do
    wipeBfsIfItemAffectsSkills [store] aid
    mapM_ addItemToDiscoBenefit $ EM.keys bag
    invalidateInMeleeDueToItem aid store
  UpdSpotItemBag _ _ bag ->
    mapM_ addItemToDiscoBenefit $ EM.keys bag
  UpdLoseItemBag _ (CActor aid store) _ -> do
    wipeBfsIfItemAffectsSkills [store] aid
    invalidateInMeleeDueToItem aid store
  UpdLoseItemBag{} -> return ()
  UpdMoveActor aid _ _ -> do
    invalidateBfsAid aid
    b <- getsState $ getActorBody aid
    -- BFS not invalidated, because distant actors may still move out
    -- of the way and close actors are considered when attempting to move
    -- and then BFS is invalidated, if needed.
    invalidateInMelee (blid b)
  UpdWaitActor aid _fromW toW ->
    -- So that we can later ignore such actors when updating targets
    -- and not risk they being pushed/displaced and targets getting illegal.
    when (toW == WSleep) $
      modifyClient $ updateTarget aid (const Nothing)
  UpdDisplaceActor source target -> do
    invalidateBfsAid source
    invalidateBfsAid target
    b <- getsState $ getActorBody source
    -- BFS not invalidated, because distant actors may still move out
    -- of the way and close actors are considered when attempting to move
    -- and then BFS is invalidated, if needed.
    invalidateInMelee (blid b)
  UpdMoveItem _ _ aid s1 s2 -> do
    wipeBfsIfItemAffectsSkills [s1, s2] aid
    invalidateInMeleeDueToItem aid s1
    invalidateInMeleeDueToItem aid s2
  UpdRefillHP{} -> return ()
  UpdRefillCalm{} -> return ()
  UpdTrajectory{} -> return ()
  UpdQuitFaction fid _ toSt _ -> do
    side <- getsClient sside
    gameModeId <- getsState sgameModeId
    when (side == fid) $ case toSt of
      Just Status{stOutcome=Camping} ->
        modifyClient $ \cli ->
          cli {scampings = ES.insert gameModeId $ scampings cli}
      Just Status{stOutcome=Restart} ->
        modifyClient $ \cli ->
          cli {srestarts = ES.insert gameModeId $ srestarts cli}
      Just Status{stOutcome} | stOutcome `elem` victoryOutcomes -> do
        scurChal <- getsClient scurChal
        let sing = M.singleton scurChal 1
            f = M.unionWith (+)
            g = EM.insertWith f gameModeId sing
        modifyClient $ \cli -> cli {svictories = g $ svictories cli}
      _ -> return ()
  UpdSpotStashFaction{} -> return ()
  UpdLoseStashFaction{} -> return ()
  UpdLeadFaction fid source target -> do
    side <- getsClient sside
    when (side == fid) $ do
      mleader <- getsClient sleader
      let !_A = assert (mleader == source
                          -- somebody changed the leader for us
                        || mleader == target
                          -- we changed the leader ourselves
                        `blame` "unexpected leader"
                        `swith` (cmd, mleader)) ()
      modifyClient $ \cli -> cli {_sleader = target}
  UpdDiplFaction{} -> return ()
  UpdAutoFaction{} ->
    -- @condBFS@ depends on the setting we change here (e.g., smarkSuspect).
    invalidateBfsAll
  UpdRecordKill{} -> return ()
  UpdDoctrineFaction{} -> do
    -- Clear all targets except the leader's.
    mleader <- getsClient sleader
    mtgt <- case mleader of
      Nothing -> return Nothing
      Just leader -> getsClient $ EM.lookup leader . stargetD
    modifyClient $ \cli ->
      let stargetD | Just tgt <- mtgt
                   , Just leader <- mleader
                   = EM.singleton leader tgt
                   | otherwise = EM.empty
      in cli {stargetD}
  UpdAlterTile lid p fromTile toTile -> do
    updateSalter lid [(p, toTile)]
    cops <- getsState scops
    let lvl = (EM.! lid) . sdungeon $ oldState
        t = lvl `at` p
    let !_A = assert (t == fromTile) ()
    when (tileChangeAffectsBfs cops fromTile toTile) $
      invalidateBfsLid lid
  UpdAlterExplorable{} -> return ()
  UpdAlterGold{} -> return ()
  UpdSearchTile aid p toTile -> do
    COps{cotile} <- getsState scops
    b <- getsState $ getActorBody aid
    let lid = blid b
    updateSalter lid [(p, toTile)]
    cops <- getsState scops
    let lvl = (EM.! lid) . sdungeon $ oldState
        t = lvl `at` p
    let !_A = assert (Just t == Tile.hideAs cotile toTile) ()
    -- The following check is needed even if we verity in content
    -- that searching doesn't change clarity and light of tiles,
    -- because it modifies skill needed to alter the tile and even
    -- walkability and changeability.
    when (tileChangeAffectsBfs cops t toTile) $
      invalidateBfsLid lid
  UpdHideTile{} -> return ()
  UpdSpotTile lid ts -> do
    updateSalter lid ts
    cops <- getsState scops
    let lvl = (EM.! lid) . sdungeon $ oldState
        affects (p, toTile) =
          let fromTile = lvl `at` p
          in tileChangeAffectsBfs cops fromTile toTile
        bs = map affects ts
    when (or bs) $ invalidateBfsLid lid
  UpdLoseTile lid ts -> do
    updateSalter lid ts
    invalidateBfsLid lid  -- from known to unknown tiles
  UpdSpotEntry{} -> return ()
  UpdLoseEntry{} -> return ()
  UpdAlterSmell{} -> return ()
  UpdSpotSmell{} -> return ()
  UpdLoseSmell{} -> return ()
  UpdTimeItem{} -> return ()
  UpdAgeGame{} -> return ()
  UpdUnAgeGame{} -> return ()
  UpdDiscover _ iid _ _ -> do
    item <- getsState $ getItemBody iid
    case jkind item of
      IdentityObvious _ik -> discoverAspect iid
      IdentityCovered ix _ik ->
        if ix `EM.notMember` sdiscoKind oldState
        then discoverKindAndAspect ix
        else discoverAspect iid
  UpdCover _ iid _ _ -> do
    item <- getsState $ getItemBody iid
    newState <- getState
    case jkind item of
      IdentityObvious _ik -> coverAspect iid
      IdentityCovered ix _ik ->
        if ix `EM.member` sdiscoKind newState
        then coverAspectAndKind ix
        else coverAspect iid
  UpdDiscoverKind _c ix _ik -> discoverKind ix
  UpdCoverKind _c ix _ik -> coverKind ix
  UpdDiscoverAspect _c iid _arItem -> discoverAspect iid
  UpdCoverAspect _c iid _arItem -> coverAspect iid
  UpdDiscoverServer{} -> error "server command leaked to client"
  UpdCoverServer{} -> error "server command leaked to client"
  UpdPerception lid outPer inPer -> perception lid outPer inPer
  UpdRestart side sfper s scurChal soptions srandom -> do
    COps{cocave} <- getsState scops
    fact <- getsState $ (EM.! side) . sfactionD
    snxtChal <- getsClient snxtChal
    svictories <- getsClient svictories
    scampings <- getsClient scampings
    srestarts <- getsClient srestarts
    stabs <- getsClient stabs
    let h lvl = CK.labyrinth (okind cocave $ lkind lvl)
                && not (fhasGender $ gplayer fact)
          -- Not to burrow through a labyrinth instead of leaving it for
          -- the human player and to prevent AI losing time there instead
          -- of congregating at exits.
        sexplored = EM.keysSet $ EM.filter h $ sdungeon s
        cli = emptyStateClient side
    putClient cli { sexplored
                  -- , sundo = [UpdAtomic cmd]
                  , sfper
                  , srandom
                  , scurChal
                  , snxtChal
                  , scondInMelee = EM.empty
                  , svictories
                  , scampings
                  , srestarts
                  , soptions
                  , stabs }
    salter <- getsState createSalter
    modifyClient $ \cli1 -> cli1 {salter}
  UpdRestartServer{} -> return ()
  UpdResume _side sfperNew -> do
#ifdef WITH_EXPENSIVE_ASSERTIONS
    sfperOld <- getsClient sfper
    let !_A = assert (sfperNew == sfperOld
                      `blame` (_side, sfperNew, sfperOld)) ()
#endif
    modifyClient $ \cli -> cli {sfper = sfperNew}
    salter <- getsState createSalter
    modifyClient $ \cli -> cli {salter}
  UpdResumeServer{} -> return ()
  UpdKillExit _fid -> killExit
  UpdWriteSave -> saveClient
  UpdHearFid{} -> return ()

-- This field is only needed in AI client, but it's on-demand for each level
-- and so fairly cheap.
invalidateInMelee :: MonadClient m => LevelId -> m ()
invalidateInMelee lid =
  modifyClient $ \cli -> cli {scondInMelee = EM.delete lid (scondInMelee cli)}

invalidateInMeleeDueToItem :: MonadClient m => ActorId -> CStore -> m ()
invalidateInMeleeDueToItem aid store =
  when (store `elem` [CEqp, COrgan]) $ do
    b <- getsState $ getActorBody aid
    invalidateInMelee (blid b)

-- For now, only checking the stores.
wipeBfsIfItemAffectsSkills :: MonadClient m => [CStore] -> ActorId -> m ()
wipeBfsIfItemAffectsSkills stores aid =
  unless (null $ intersect stores [CEqp, COrgan]) $ invalidateBfsAid aid

tileChangeAffectsBfs :: COps
                     -> ContentId TileKind -> ContentId TileKind
                     -> Bool
tileChangeAffectsBfs COps{coTileSpeedup} fromTile toTile =
  Tile.alterMinWalk coTileSpeedup fromTile
  /= Tile.alterMinWalk coTileSpeedup toTile

createActor :: MonadClient m => ActorId -> Actor -> [(ItemId, Item)] -> m ()
createActor aid b ais = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  let affect3 tap@TgtAndPath{..} = case tapTgt of
        TPoint (TEnemyPos a) _ _ | a == aid ->
          let tgt | isFoe side fact (bfid b) = TEnemy a  -- still a foe
                  | otherwise = TPoint TKnown (blid b) (bpos b)
          in TgtAndPath tgt Nothing
        _ -> tap
  modifyClient $ \cli -> cli {stargetD = EM.map affect3 (stargetD cli)}
  mapM_ (addItemToDiscoBenefit . fst) ais
  unless (bproj b) $ invalidateBfsPathLid b
  invalidateInMelee (blid b)

destroyActor :: MonadClient m => ActorId -> Actor -> Bool -> m ()
destroyActor aid b destroy = do
  when destroy $  -- if vanishes for a moment only, keep target
    modifyClient $ \cli -> cli {stargetD = EM.delete aid $ stargetD cli} -- gc
  -- Here, among others, (local) flee time of an actor changing level is reset.
  modifyClient $ \cli -> cli { sbfsD = EM.delete aid $ sbfsD cli
                             , sfleeD = EM.delete aid $ sfleeD cli }
  localTime <- getsState $ getLocalTime $ blid b
  fleeD <- getsClient sfleeD
  let recentlyFled aid3 = maybe False (\(_, time) -> timeRecent5 localTime time)
                                (aid3 `EM.lookup` fleeD)
      dummyTarget = TPoint TKnown (blid b) (bpos b)
      affect aid3 tgt = case tgt of
        TEnemy a | a == aid ->
          if destroy || recentlyFled aid3
                          -- if fleeing, don't chase the enemy soon after;
                          -- unfortunately, the enemy also won't be recorded
                          -- in case he gets out of sight, in order to avoid
                          -- him when fleeing again, but all enemies should be
                          -- recorded in such a case, so not a big difference
          then
            -- If *really* nothing more interesting, the actor will
            -- go to last known location to perhaps find other foes.
            dummyTarget
          else
            -- If enemy only hides (or we stepped behind obstacle) find him.
            TPoint (TEnemyPos a) (blid b) (bpos b)
        TNonEnemy a | a == aid -> dummyTarget
        _ -> tgt
      affect3 aid3 TgtAndPath{..} =
        let newMPath = case tapPath of
              Just AndPath{pathGoal} | pathGoal /= bpos b -> Nothing
              _ -> tapPath  -- foe slow enough, so old path good
        in TgtAndPath (affect aid3 tapTgt) newMPath
  modifyClient $ \cli -> cli {stargetD = EM.mapWithKey affect3 (stargetD cli)}
  unless (bproj b) $ invalidateBfsPathLid b
  invalidateInMelee (blid b)

addItemToDiscoBenefit :: MonadClient m => ItemId -> m ()
addItemToDiscoBenefit iid = do
  cops <- getsState scops
  discoBenefit <- getsClient sdiscoBenefit
  case EM.lookup iid discoBenefit of
    Just{} -> return ()
      -- already there, with real or provisional aspect record,
      -- but we haven't learned anything new about the item
    Nothing -> do
      side <- getsClient sside
      factionD <- getsState sfactionD
      itemFull <- getsState $ itemToFull iid
      let benefit = totalUsefulness cops side factionD itemFull
      modifyClient $ \cli ->
        cli {sdiscoBenefit = EM.insert iid benefit (sdiscoBenefit cli)}

perception :: MonadClient m => LevelId -> Perception -> Perception -> m ()
perception lid outPer inPer = do
  -- Clients can't compute FOV on their own, because they don't know
  -- if unknown tiles are clear or not. Server would need to send
  -- info about properties of unknown tiles, which complicates
  -- and makes heavier the most bulky data set in the game: tile maps.
  -- Note we assume, but do not check that @outPer@ is contained
  -- in current perception and @inPer@ has no common part with it.
  -- It would make the already very costly operation even more expensive.
{-
  perOld <- getPerFid lid
  -- Check if new perception is already set in @cmdAtomicFilterCli@
  -- or if we are doing undo/redo, which does not involve filtering.
  -- The data structure is strict, so the cheap check can't be any simpler.
  let interAlready per =
        Just $ totalVisible per `ES.intersection` totalVisible perOld
      unset = maybe False ES.null (interAlready inPer)
              || maybe False (not . ES.null) (interAlready outPer)
  when unset $ do
-}
    let adj Nothing = error $ "no perception to alter" `showFailure` lid
        adj (Just per) = Just $ addPer (diffPer per outPer) inPer
        f = EM.alter adj lid
    modifyClient $ \cli -> cli {sfper = f (sfper cli)}

discoverKind :: MonadClient m => ItemKindIx -> m ()
discoverKind = discoverKindAndAspect

discoverKindAndAspect :: MonadClient m => ItemKindIx -> m ()
discoverKindAndAspect ix = do
  cops <- getsState scops
  -- Wipe out BFS, because the player could potentially learn that his items
  -- affect his actors' skills relevant to BFS.
  invalidateBfsAll
  side <- getsClient sside
  factionD <- getsState sfactionD
  itemToF <- getsState $ flip itemToFull
  let benefit iid = totalUsefulness cops side factionD (itemToF iid)
  itemIxMap <- getsState $ (EM.! ix) . sitemIxMap
  -- Possibly overwrite earlier, provisional benefits.
  forM_ (ES.elems itemIxMap) $ \iid -> modifyClient $ \cli ->
    cli {sdiscoBenefit = EM.insert iid (benefit iid) (sdiscoBenefit cli)}

coverKind :: ItemKindIx -> m ()
coverKind = coverAspectAndKind

coverAspectAndKind :: ItemKindIx -> m ()
coverAspectAndKind _ix = undefined

discoverAspect :: MonadClient m => ItemId -> m ()
discoverAspect iid = do
  cops <- getsState scops
  -- Wipe out BFS, because the player could potentially learn that his items
  -- affect his actors' skills relevant to BFS.
  invalidateBfsAll
  side <- getsClient sside
  factionD <- getsState sfactionD
  itemFull <- getsState $ itemToFull iid
  let benefit = totalUsefulness cops side factionD itemFull
  -- Possibly overwrite earlier, provisional benefits.
  modifyClient $ \cli ->
    cli {sdiscoBenefit = EM.insert iid benefit (sdiscoBenefit cli)}

coverAspect :: ItemId -> m ()
coverAspect _iid = undefined

killExit :: MonadClient m => m ()
killExit = do
  side <- getsClient sside
  debugPossiblyPrint $ "Client" <+> tshow side <+> "quitting."
  modifyClient $ \cli -> cli {squit = True}
  -- Verify that the not saved caches are equal to future reconstructed.
  -- Otherwise, save/restore would change game state.
  sactorMaxSkills2 <- getsState sactorMaxSkills
  salter <- getsClient salter
  sbfsD <- getsClient sbfsD
  alter <- getsState createSalter
  actorMaxSkills <- getsState maxSkillsInDungeon
  let f aid = do
        (canMove, alterSkill) <- condBFS aid
        bfsArr <- createBfs canMove alterSkill aid
        let bfsPath = EM.empty
        return (aid, BfsAndPath bfsArr bfsPath)
  actorD <- getsState sactorD
  lbfsD <- mapM f $ EM.keys actorD
  -- Some freshly generated bfses are not used for comparison, but at least
  -- we check they don't violate internal assertions themselves. Hence the bang.
  let bfsD = EM.fromDistinctAscList lbfsD
      g BfsInvalid !_ = True
      g _ BfsInvalid = False
      g (BfsAndPath bfsArr1 _) (BfsAndPath bfsArr2 _) = bfsArr1 == bfsArr2
      subBfs = EM.isSubmapOfBy g
  let !_A1 = assert (salter == alter
                     `blame` "wrong accumulated salter on side"
                     `swith` (side, salter, alter)) ()
      !_A2 = assert (sactorMaxSkills2 == actorMaxSkills
                     `blame` "wrong accumulated sactorMaxSkills on side"
                     `swith` (side, sactorMaxSkills2, actorMaxSkills)) ()
      !_A3 = assert (sbfsD `subBfs` bfsD
                     `blame` "wrong accumulated sbfsD on side"
                     `swith` (side, sbfsD, bfsD)) ()
  return ()
