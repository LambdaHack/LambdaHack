-- | Handle atomic commands received by the client.
module Game.LambdaHack.Client.HandleAtomicM
  ( cmdAtomicSemCli
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
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
import qualified Game.LambdaHack.Common.Kind as Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Perception
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind (ModeKind)
import           Game.LambdaHack.Content.TileKind (TileKind)

-- | Effect of atomic actions on client state is calculated
-- with the global state from after the command is executed
-- (except where the supplied @oldState@ is used).
cmdAtomicSemCli :: MonadClientSetup m => State -> UpdAtomic -> m ()
{-# INLINE cmdAtomicSemCli #-}
cmdAtomicSemCli oldState cmd = case cmd of
  UpdCreateActor aid b ais -> createActor aid b ais
  UpdDestroyActor aid b _ -> destroyActor aid b True
  UpdCreateItem iid itemBase _ (CActor aid store) -> do
    wipeBfsIfItemAffectsSkills [store] aid
    addItemToDiscoBenefit iid itemBase
  UpdCreateItem iid itemBase _ _ -> addItemToDiscoBenefit iid itemBase
  UpdDestroyItem _ _ _ (CActor aid store) ->
    wipeBfsIfItemAffectsSkills [store] aid
  UpdSpotActor aid b ais -> createActor aid b ais
  UpdLoseActor aid b _ -> destroyActor aid b False
  UpdSpotItem _ iid itemBase _ (CActor aid store) -> do
    wipeBfsIfItemAffectsSkills [store] aid
    addItemToDiscoBenefit iid itemBase
  UpdSpotItem _ iid itemBase _ _ -> addItemToDiscoBenefit iid itemBase
  UpdLoseItem _ _ _ _ (CActor aid store) ->
    wipeBfsIfItemAffectsSkills [store] aid
  UpdSpotItemBag (CActor aid store) _bag ais -> do
    wipeBfsIfItemAffectsSkills [store] aid
    mapM_ (\(iid, item) -> addItemToDiscoBenefit iid item) ais
  UpdSpotItemBag _ _ ais ->
    mapM_ (\(iid, item) -> addItemToDiscoBenefit iid item) ais
  UpdLoseItemBag (CActor aid store) _bag _ais ->
    wipeBfsIfItemAffectsSkills [store] aid
  UpdMoveActor aid _ _ -> invalidateBfsAid aid
  UpdDisplaceActor source target -> do
    invalidateBfsAid source
    invalidateBfsAid target
  UpdMoveItem _ _ aid s1 s2 -> wipeBfsIfItemAffectsSkills [s1, s2] aid
  UpdLeadFaction fid source target -> do
    side <- getsClient sside
    when (side == fid) $ do
      mleader <- getsClient _sleader
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
    mleader <- getsClient _sleader
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
    Kind.COps{cotile} <- getsState scops
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
  UpdAgeGame arenas ->
    -- This tweak is only needed in AI client, but it's fairly cheap.
    modifyClient $ \cli ->
      let g !em !lid = EM.adjust (const Nothing) lid em
      in cli {scondInMelee = foldl' g (scondInMelee cli) arenas}
  UpdDiscover c iid ik seed -> do
    discoverKind c iid ik
    discoverSeed c iid seed
  UpdCover c iid ik seed -> do
    coverSeed c iid seed
    coverKind c iid ik
  UpdDiscoverKind c iid ik -> discoverKind c iid ik
  UpdCoverKind c iid ik -> coverKind c iid ik
  UpdDiscoverSeed c iid seed -> discoverSeed c iid seed
  UpdCoverSeed c iid seed -> coverSeed c iid seed
  UpdPerception lid outPer inPer -> perception lid outPer inPer
  UpdRestart side sfper s scurChal sdebugCli -> do
    Kind.COps{comode=Kind.Ops{ofoldlGroup'}} <- getsState scops
    snxtChal <- getsClient snxtChal
    svictories <- getsClient svictories
    let f acc _p i _a = i : acc
        modes = zip [0..] $ ofoldlGroup' "campaign scenario" f []
        g :: (Int, Kind.Id ModeKind) -> Int
        g (_, mode) = case EM.lookup mode svictories of
          Nothing -> 0
          Just cm -> fromMaybe 0 (M.lookup snxtChal cm)
        (snxtScenario, _) = minimumBy (comparing g) modes
        cli = emptyStateClient side
    putClient cli { sfper
                  -- , sundo = [UpdAtomic cmd]
                  , scurChal
                  , snxtChal
                  , snxtScenario
                  , scondInMelee = EM.map (const Nothing) (sdungeon s)
                  , svictories
                  , sdebugCli }
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

-- For now, only checking the stores.
wipeBfsIfItemAffectsSkills :: MonadClient m => [CStore] -> ActorId -> m ()
wipeBfsIfItemAffectsSkills stores aid =
  unless (null $ intersect stores [CEqp, COrgan]) $ invalidateBfsAid aid

tileChangeAffectsBfs :: Kind.COps
                     -> Kind.Id TileKind -> Kind.Id TileKind
                     -> Bool
tileChangeAffectsBfs Kind.COps{coTileSpeedup} fromTile toTile =
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
  mapM_ (uncurry addItemToDiscoBenefit) ais

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

addItemToDiscoBenefit :: MonadClient m => ItemId -> Item -> m ()
addItemToDiscoBenefit iid item = do
  cops@Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
  discoBenefit <- getsClient sdiscoBenefit
  case EM.lookup iid discoBenefit of
    Just{} -> return ()  -- already there
    Nothing -> do
      discoKind <- getsState sdiscoKind
      case EM.lookup (jkindIx item) discoKind of
        Nothing -> return ()
        Just KindMean{..} -> do  -- possible, if the kind sent in @UpdRestart@
          side <- getsClient sside
          fact <- getsState $ (EM.! side) . sfactionD
          let effects = IK.ieffects $ okind kmKind
              benefit = totalUsefulness cops fact effects kmMean item
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

discoverKind :: MonadClient m => Container -> ItemId -> Kind.Id ItemKind -> m ()
discoverKind _c iid ik = do
  cops@Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
  -- Wipe out BFS, because the player could potentially learn that his items
  -- affect his actors' skills relevant to BFS.
  invalidateBfsAll
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  discoKind <- getsState sdiscoKind
  item <- getsState $ getItemBody iid
  let kind = okind ik
      KindMean{kmMean} = case EM.lookup (jkindIx item) discoKind of
        Just km -> km
        Nothing -> error "item kind not found"
      benefit = totalUsefulness cops fact (IK.ieffects kind) kmMean item
  -- This adds to @sdiscoBenefit@ only @iid@ and not any other items
  -- that share the same @jkindIx@, so this is broken if such items
  -- are not fully IDed from the start.
  modifyClient $ \cli ->
    cli {sdiscoBenefit = EM.insert iid benefit (sdiscoBenefit cli)}

coverKind :: Container -> ItemId -> Kind.Id ItemKind -> m ()
coverKind _c _iid _ik = undefined

discoverSeed :: MonadClient m => Container -> ItemId -> ItemSeed -> m ()
discoverSeed _c iid seed = do
  cops@Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
  -- Wipe out BFS, because the player could potentially learn that his items
  -- affect his actors' skills relevant to BFS.
  invalidateBfsAll
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  discoKind <- getsState sdiscoKind
  item <- getsState $ getItemBody iid
  totalDepth <- getsState stotalDepth
  Level{ldepth} <- getLevel $ jlid item
  let KindMean{..} = case EM.lookup (jkindIx item) discoKind of
        Just km -> km
        Nothing -> error "item kind not found"
      kind = okind kmKind
      aspects = seedToAspect seed kind ldepth totalDepth
      benefit = totalUsefulness cops fact (IK.ieffects kind) aspects item
  modifyClient $ \cli ->
    cli {sdiscoBenefit = EM.insert iid benefit (sdiscoBenefit cli)}

coverSeed :: Container -> ItemId -> ItemSeed -> m ()
coverSeed _c _iid _seed = undefined

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
