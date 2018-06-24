-- | Handle atomic commands received by the client.
module Game.LambdaHack.Client.HandleAtomicM
  ( MonadClientSetup(..)
  , cmdAtomicSemCli
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , wipeBfsIfItemAffectsSkills, tileChangeAffectsBfs, createActor, destroyActor
  , addItemToDiscoBenefit, perception
  , discoverKind, coverKind, discoverAspect, coverAspect
  , killExit
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Lazy as LEM
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M
import           Data.Ord

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
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Perception
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import qualified Game.LambdaHack.Content.CaveKind as CK
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import           Game.LambdaHack.Content.ModeKind (ModeKind, fhasGender)
import           Game.LambdaHack.Content.TileKind (TileKind)

-- | Client monad for saving and restarting games.
class MonadClient m => MonadClientSetup m where
  saveClient    :: m ()
  restartClient :: m ()

-- | Effect of atomic actions on client state. It is calculated
-- with the global state from after the command is executed
-- (except where the supplied @oldState@ is used).
cmdAtomicSemCli :: MonadClientSetup m => State -> UpdAtomic -> m ()
{-# INLINE cmdAtomicSemCli #-}
cmdAtomicSemCli oldState cmd = case cmd of
  UpdCreateActor aid b ais -> createActor aid b ais
  UpdDestroyActor aid b _ -> destroyActor aid b True
  UpdCreateItem iid _ _ (CActor aid store) -> do
    wipeBfsIfItemAffectsSkills [store] aid
    addItemToDiscoBenefit iid
  UpdCreateItem iid _ _ _ -> addItemToDiscoBenefit iid
  UpdDestroyItem _ _ _ (CActor aid store) ->
    wipeBfsIfItemAffectsSkills [store] aid
  UpdSpotActor aid b ais -> createActor aid b ais
  UpdLoseActor aid b _ -> destroyActor aid b False
  UpdSpotItem _ iid _ _ (CActor aid store) -> do
    wipeBfsIfItemAffectsSkills [store] aid
    addItemToDiscoBenefit iid
  UpdSpotItem _ iid _ _ _ -> addItemToDiscoBenefit iid
  UpdLoseItem _ _ _ _ (CActor aid store) ->
    wipeBfsIfItemAffectsSkills [store] aid
  UpdSpotItemBag (CActor aid store) _bag ais -> do
    wipeBfsIfItemAffectsSkills [store] aid
    mapM_ (addItemToDiscoBenefit . fst) ais
  UpdSpotItemBag _ _ ais ->
    mapM_ (addItemToDiscoBenefit . fst) ais
  UpdLoseItemBag (CActor aid store) _bag _ais ->
    wipeBfsIfItemAffectsSkills [store] aid
  UpdMoveActor aid _ _ -> do
    invalidateBfsAid aid
    b <- getsState $ getActorBody aid
    recomputeInMelee (blid b)
  UpdDisplaceActor source target -> do
    invalidateBfsAid source
    invalidateBfsAid target
    b <- getsState $ getActorBody source
    recomputeInMelee (blid b)
  UpdMoveItem _ _ aid s1 s2 -> wipeBfsIfItemAffectsSkills [s1, s2] aid
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
  UpdAutoFaction{} ->
    -- @condBFS@ depends on the setting we change here (e.g., smarkSuspect).
    invalidateBfsAll
  UpdTacticFaction{} -> do
    -- Clear all targets except the leader's.
    mleader <- getsClient sleader
    mtgt <- case mleader of
      Nothing -> return Nothing
      Just leader -> getsClient $ EM.lookup leader . stargetD
    modifyClient $ \cli ->
      cli { stargetD = case (mtgt, mleader) of
              (Just tgt, Just leader) -> EM.singleton leader tgt
              _ -> EM.empty }
  UpdAlterTile lid p fromTile toTile -> do
    updateSalter lid [(p, toTile)]
    cops <- getsState scops
    let lvl = (EM.! lid) . sdungeon $ oldState
        t = lvl `at` p
    let !_A = assert (t == fromTile) ()
    when (tileChangeAffectsBfs cops fromTile toTile) $
      invalidateBfsLid lid
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
  UpdDiscover c iid ik aspectRecord -> do
    item <- getsState $ getItemBody iid
    discoKind <- getsState sdiscoKind
    case jkind item of
      IdentityObvious _ik -> return ()
      IdentityCovered ix _ik | ix `EM.notMember` discoKind ->
        discoverKind c ix ik
      IdentityCovered _ix _ik -> return ()
    discoverAspect c iid aspectRecord
  UpdCover c iid ik aspectRecord -> do
    coverAspect c iid aspectRecord
    item <- getsState $ getItemBody iid
    discoKind <- getsState sdiscoKind
    case jkind item of
      IdentityObvious _ik -> return ()
      IdentityCovered ix _ik | ix `EM.member` discoKind ->
        coverKind c ix ik
      IdentityCovered _ix _ik -> return ()
  UpdDiscoverKind c ix ik -> discoverKind c ix ik
  UpdCoverKind c ix ik -> coverKind c ix ik
  UpdDiscoverAspect c iid aspectRecord -> discoverAspect c iid aspectRecord
  UpdCoverAspect c iid aspectRecord -> coverAspect c iid aspectRecord
  UpdPerception lid outPer inPer -> perception lid outPer inPer
  UpdRestart side sfper s scurChal soptions -> do
    COps{cocave, comode} <- getsState scops
    fact <- getsState $ (EM.! side) . sfactionD
    snxtChal <- getsClient snxtChal
    svictories <- getsClient svictories
    let f acc _p i _a = i : acc
        modes = zip [0..] $ ofoldlGroup' comode "campaign scenario" f []
        g :: (Int, ContentId ModeKind) -> Int
        g (_, mode) = case EM.lookup mode svictories of
          Nothing -> 0
          Just cm -> fromMaybe 0 (M.lookup snxtChal cm)
        (snxtScenario, _) = minimumBy (comparing g) modes
        h lvl = CK.cactorCoeff (okind cocave $ lkind lvl) > 150
                && not (fhasGender $ gplayer fact)
          -- Not to burrow through a labyrinth instead of leaving it for
          -- the human player and to prevent AI losing time there instead
          -- of congregating at exits.
        sexplored = EM.keysSet $ EM.filter h $ sdungeon s
        cli = emptyStateClient side
    putClient cli { sexplored
                  , sfper
                  -- , sundo = [UpdAtomic cmd]
                  , scurChal
                  , snxtChal
                  , snxtScenario
                  , scondInMelee = LEM.fromAscList
                                   $ map (\lid -> (lid, False))
                                   $ EM.keys (sdungeon s)
                  , svictories
                  , soptions }
    salter <- getsState createSalter
    modifyClient $ \cli1 -> cli1 {salter}
    restartClient
  UpdResume _fid sfperNew -> do
#ifdef WITH_EXPENSIVE_ASSERTIONS
    sfperOld <- getsClient sfper
    let !_A = assert (sfperNew == sfperOld `blame` (sfperNew, sfperOld)) ()
#endif
    modifyClient $ \cli -> cli {sfper=sfperNew}
    salter <- getsState createSalter
    modifyClient $ \cli -> cli {salter}
  UpdKillExit _fid -> killExit
  UpdWriteSave -> saveClient
  _ -> return ()

-- This tweak is only needed in AI client, but it's lazy for each level
-- and so fairly cheap.
recomputeInMelee :: MonadClient m => LevelId -> m ()
recomputeInMelee lid = do
  side <- getsClient sside
  s <- getState
  modifyClient $ \cli ->
    cli {scondInMelee = LEM.insert lid (inMelee side lid s) (scondInMelee cli)}

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
  let newPermit = bfid b == side
      affect3 tap@TgtAndPath{..} = case tapTgt of
        TPoint (TEnemyPos a _) _ _ | a == aid ->
          TgtAndPath (TEnemy a newPermit) NoPath
        _ -> tap
  modifyClient $ \cli -> cli {stargetD = EM.map affect3 (stargetD cli)}
  mapM_ (addItemToDiscoBenefit . fst) ais
  recomputeInMelee (blid b)

destroyActor :: MonadClient m => ActorId -> Actor -> Bool -> m ()
destroyActor aid b destroy = do
  when destroy $ modifyClient $ updateTarget aid (const Nothing)  -- gc
  modifyClient $ \cli -> cli {sbfsD = EM.delete aid $ sbfsD cli}  -- gc
  let affect tgt = case tgt of
        TEnemy a permit | a == aid ->
          if destroy then
            -- If *really* nothing more interesting, the actor will
            -- go to last known location to perhaps find other foes.
            TPoint TAny (blid b) (bpos b)
          else
            -- If enemy only hides (or we stepped behind obstacle) find him.
            TPoint (TEnemyPos a permit) (blid b) (bpos b)
        _ -> tgt
      affect3 TgtAndPath{..} =
        let newMPath = case tapPath of
              AndPath{pathGoal} | pathGoal /= bpos b -> NoPath
              _ -> tapPath  -- foe slow enough, so old path good
        in TgtAndPath (affect tapTgt) newMPath
  modifyClient $ \cli -> cli {stargetD = EM.map affect3 (stargetD cli)}
  recomputeInMelee (blid b)

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
      fact <- getsState $ (EM.! side) . sfactionD
      itemFull <- getsState $ itemToFull iid
      let benefit = totalUsefulness cops fact itemFull
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

discoverKind :: MonadClient m
             => Container -> ItemKindIx -> ContentId ItemKind -> m ()
discoverKind _c ix _ik = do
  cops <- getsState scops
  -- Wipe out BFS, because the player could potentially learn that his items
  -- affect his actors' skills relevant to BFS.
  invalidateBfsAll
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  itemToF <- getsState $ flip itemToFull
  let benefit iid =
        let itemFull = itemToF iid
        in totalUsefulness cops fact itemFull
  itemIxMap <- getsState $ (EM.! ix) . sitemIxMap
  -- Possibly overwrite earlier, provisional benefits.
  forM_ (ES.elems itemIxMap) $ \iid -> modifyClient $ \cli ->
    cli {sdiscoBenefit = EM.insert iid (benefit iid) (sdiscoBenefit cli)}

coverKind :: Container -> ItemKindIx -> ContentId ItemKind -> m ()
coverKind _c _ix _ik = undefined

discoverAspect :: MonadClient m
               => Container -> ItemId -> IA.AspectRecord -> m ()
discoverAspect _c iid _aspectRecord = do
  cops <- getsState scops
  -- Wipe out BFS, because the player could potentially learn that his items
  -- affect his actors' skills relevant to BFS.
  invalidateBfsAll
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  itemFull <- getsState $ itemToFull iid
  let benefit = totalUsefulness cops fact itemFull
  -- Possibly overwrite earlier, provisional benefits.
  modifyClient $ \cli ->
    cli {sdiscoBenefit = EM.insert iid benefit (sdiscoBenefit cli)}

coverAspect :: Container -> ItemId -> IA.AspectRecord -> m ()
coverAspect _c _iid _aspectRecord = undefined

killExit :: MonadClient m => m ()
killExit = do
  side <- getsClient sside
  debugPossiblyPrint $ "Client" <+> tshow side <+> "quitting."
  modifyClient $ \cli -> cli {squit = True}
  -- Verify that the not saved caches are equal to future reconstructed.
  -- Otherwise, save/restore would change game state.
  sactorAspect2 <- getsState sactorAspect
  salter <- getsClient salter
  sbfsD <- getsClient sbfsD
  alter <- getsState createSalter
  actorAspect <- getsState actorAspectInDungeon
  let f aid = do
        (canMove, alterSkill) <- condBFS aid
        bfsArr <- createBfs canMove alterSkill aid
        let bfsPath = EM.empty
        return (aid, BfsAndPath{..})
  actorD <- getsState sactorD
  lbfsD <- mapM f $ EM.keys actorD
  -- Some freshly generated bfses are not used for comparison, but at least
  -- we check they don't violate internal assertions themselves. Hence the bang.
  let bfsD = EM.fromDistinctAscList lbfsD
      g BfsInvalid !_ = True
      g _ BfsInvalid = False
      g bap1 bap2 = bfsArr bap1 == bfsArr bap2
      subBfs = EM.isSubmapOfBy g
  let !_A1 = assert (salter == alter
                     `blame` "wrong accumulated salter on side"
                     `swith` (side, salter, alter)) ()
      !_A2 = assert (sactorAspect2 == actorAspect
                     `blame` "wrong accumulated sactorAspect on side"
                     `swith` (side, sactorAspect2, actorAspect)) ()
      !_A3 = assert (sbfsD `subBfs` bfsD
                     `blame` "wrong accumulated sbfsD on side"
                     `swith` (side, sbfsD, bfsD)) ()
  return ()
