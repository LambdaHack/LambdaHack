{-# LANGUAGE TupleSections #-}
-- | Handle atomic commands received by the client.
module Game.LambdaHack.Client.HandleAtomicM
  ( cmdAtomicSemCli, cmdAtomicFilterCli
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M
import Data.Ord
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.Bfs
import Game.LambdaHack.Client.BfsM
import Game.LambdaHack.Client.CommonM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.Preferences
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind (ModeKind)
import Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Content.TileKind as TK

-- * RespUpdAtomicAI

-- | Clients keep a subset of atomic commands sent by the server
-- and add some of their own. The result of this function is the list
-- of commands kept for each command received.
-- This is calculated in the global state from before the command is executed.
cmdAtomicFilterCli :: MonadClient m => UpdAtomic -> m [UpdAtomic]
{-# INLINE cmdAtomicFilterCli #-}
cmdAtomicFilterCli cmd = case cmd of
  UpdAlterTile lid p fromTile toTile -> do
    Kind.COps{cotile=Kind.Ops{okind}} <- getsState scops
    lvl <- getLevel lid
    let t = lvl `at` p
    if t == fromTile
      then return [cmd]
      else do
        -- From @UpdAlterTile@ we know @t == freshClientTile@,
        -- which is uncanny, so we produce a message.
        -- It happens when a client thinks the tile is @t@,
        -- but it's @fromTile@, and @UpdAlterTile@ changes it
        -- to @toTile@. See @updAlterTile@.
        let subject = ""  -- a hack, we we don't handle adverbs well
            verb = "turn into"
            msg = makeSentence [ "the", MU.Text $ TK.tname $ okind t
                               , "at position", MU.Text $ tshow p
                               , "suddenly"  -- adverb
                               , MU.SubjectVerbSg subject verb
                               , MU.AW $ MU.Text $ TK.tname $ okind toTile ]
        return [ cmd  -- reveal the tile
               , UpdMsgAll msg  -- show the message
               ]
  UpdSearchTile aid p toTile -> do
    Kind.COps{cotile} <- getsState scops
    b <- getsState $ getActorBody aid
    lvl <- getLevel $ blid b
    let t = lvl `at` p
        fromTile = Tile.hideAs cotile toTile
    return $!
      if t == fromTile
      then -- Fully ignorant. (No intermediate knowledge possible.)
           [ cmd  -- show the message
           , UpdAlterTile (blid b) p fromTile toTile  -- reveal tile
           ]
      else assert (t == toTile `blame` "LoseTile fails to reset memory"
                               `swith` (aid, p, fromTile, toTile, b, t, cmd))
                  [cmd]  -- Already knows the tile fully, only confirm.
  UpdHideTile{} -> return []  -- will be fleshed out when Undo completed
  UpdSpotTile lid ts -> do
    Kind.COps{cotile} <- getsState scops
    lvl <- getLevel lid
    -- We ignore the server resending us hidden versions of the tiles
    -- (and resending us the same data we already got).
    -- If the tiles are changed to other variants of the hidden tile,
    -- we can still verify by searching.
    let notKnown (p, t) = let tClient = lvl `at` p
                          in Tile.hideAs cotile tClient /= t
        newTs = filter notKnown ts
    return $! if null newTs then [] else [UpdSpotTile lid newTs]
  UpdDiscover c iid _ seed -> do
    itemD <- getsState sitemD
    case EM.lookup iid itemD of
      Nothing -> return []
      Just item -> do
        discoKind <- getsClient sdiscoKind
        if jkindIx item `EM.member` discoKind
          then do
            discoAspect <- getsClient sdiscoAspect
            if iid `EM.member` discoAspect
              then return []
              else return [UpdDiscoverSeed c iid seed]
          else return [cmd]
  UpdCover c iid ik _ -> do
    itemD <- getsState sitemD
    case EM.lookup iid itemD of
      Nothing -> return []
      Just item -> do
        discoKind <- getsClient sdiscoKind
        if jkindIx item `EM.notMember` discoKind
          then return []
          else do
            discoAspect <- getsClient sdiscoAspect
            if iid `EM.notMember` discoAspect
              then return [cmd]
              else return [UpdCoverKind c iid ik]
  UpdDiscoverKind _ iid _ -> do
    itemD <- getsState sitemD
    case EM.lookup iid itemD of
      Nothing -> return []
      Just item -> do
        discoKind <- getsClient sdiscoKind
        if jkindIx item `EM.notMember` discoKind
        then return []
        else return [cmd]
  UpdCoverKind _ iid _ -> do
    itemD <- getsState sitemD
    case EM.lookup iid itemD of
      Nothing -> return []
      Just item -> do
        discoKind <- getsClient sdiscoKind
        if jkindIx item `EM.notMember` discoKind
        then return []
        else return [cmd]
  UpdDiscoverSeed _ iid _ -> do
    itemD <- getsState sitemD
    case EM.lookup iid itemD of
      Nothing -> return []
      Just item -> do
        discoKind <- getsClient sdiscoKind
        if jkindIx item `EM.notMember` discoKind
        then return []
        else do
          discoAspect <- getsClient sdiscoAspect
          if iid `EM.member` discoAspect
            then return []
            else return [cmd]
  UpdCoverSeed _ iid _ -> do
    itemD <- getsState sitemD
    case EM.lookup iid itemD of
      Nothing -> return []
      Just item -> do
        discoKind <- getsClient sdiscoKind
        if jkindIx item `EM.notMember` discoKind
        then return []
        else do
          discoAspect <- getsClient sdiscoAspect
          if iid `EM.notMember` discoAspect
            then return []
            else return [cmd]
  UpdPerception lid outPer inPer -> do
    -- Here we cheat by setting a new perception outright instead of
    -- in @cmdAtomicSemCli@, to avoid computing perception twice.
    perception lid outPer inPer
    s <- getState
    -- Wipe out actors that just became invisible due to changed FOV.
    let outFov = totalVisible outPer
        outPrio = concatMap (\p -> posToAssocs p lid s) $ ES.elems outFov
        fActor (aid, b) = Just $ UpdLoseActor aid b $ getCarriedAssocs b s
          -- this command always succeeds, the actor can be always removed,
          -- because the actor is taken from the state
        outActor = mapMaybe fActor outPrio
    -- Wipe out remembered items on tiles that now came into view.
    let lvl = (EM.! lid) . sdungeon $ s
        inFov = ES.elems $ totalVisible inPer
        inContainer fc itemFloor =
          let inItem = mapMaybe (\p -> (p,) <$> EM.lookup p itemFloor) inFov
              fItem p (iid, kit) =
                UpdLoseItem True iid (getItemBody iid s) kit (fc lid p)
              fBag (p, bag) = map (fItem p) $ EM.assocs bag
          in concatMap fBag inItem
        inFloor = inContainer CFloor (lfloor lvl)
        inEmbed = inContainer CEmbed (lembed lvl)
    -- Remembered map tiles not wiped out, due to optimization in @updSpotTile@.
    -- Wipe out remembered smell on tiles that now came into smell Fov.
    let inSmellFov = totalSmelled inPer
        inSm = mapMaybe (\p -> (p,) <$> EM.lookup p (lsmell lvl))
                        (ES.elems inSmellFov)
        inSmell = if null inSm then [] else [UpdLoseSmell lid inSm]
    -- Note that the items and smells that we forget were previously
    -- invisible, only remembered (because taken from @inPer@),
    -- and the tiles they are on are currently visible (ditto).
    return $! cmd : outActor ++ inFloor ++ inEmbed ++ inSmell
  _ -> return [cmd]

-- | Effect of atomic actions on client state is calculated
-- with the global state from after the command is executed
-- (except where the supplied @oldState@ is used).
cmdAtomicSemCli :: MonadClientSetup m => State -> UpdAtomic -> m ()
{-# INLINE cmdAtomicSemCli #-}
cmdAtomicSemCli oldState cmd = case cmd of
  UpdCreateActor aid b ais -> createActor aid b ais
  UpdDestroyActor aid b _ -> destroyActor aid b True
  UpdCreateItem iid itemBase (k, _) (CActor aid store) -> do
    wipeBfsIfItemAffectsSkills [store] aid
    when (store `elem` [CEqp, COrgan]) $ addItemToActor iid itemBase k aid
    addItemToDiscoBenefit iid itemBase
  UpdCreateItem iid itemBase _ _ -> addItemToDiscoBenefit iid itemBase
  UpdDestroyItem iid itemBase (k, _) (CActor aid store) -> do
    wipeBfsIfItemAffectsSkills [store] aid
    when (store `elem` [CEqp, COrgan]) $ addItemToActor iid itemBase (-k) aid
  UpdSpotActor aid b ais -> createActor aid b ais
  UpdLoseActor aid b _ -> destroyActor aid b False
  UpdSpotItem _ iid itemBase (k, _) (CActor aid store) -> do
    wipeBfsIfItemAffectsSkills [store] aid
    when (store `elem` [CEqp, COrgan]) $ addItemToActor iid itemBase k aid
    addItemToDiscoBenefit iid itemBase
  UpdSpotItem _ iid itemBase _ _ -> addItemToDiscoBenefit iid itemBase
  UpdLoseItem _ iid itemBase (k, _) (CActor aid store) -> do
    wipeBfsIfItemAffectsSkills [store] aid
    when (store `elem` [CEqp, COrgan]) $ addItemToActor iid itemBase (-k) aid
  UpdMoveActor aid _ _ -> invalidateBfsAid aid
  UpdDisplaceActor source target -> do
    invalidateBfsAid source
    invalidateBfsAid target
  UpdMoveItem iid k aid s1 s2 -> do
    wipeBfsIfItemAffectsSkills [s1, s2] aid
    case s1 of
      CEqp -> case s2 of
        COrgan -> return ()
        _ -> do
          itemBase <- getsState $ getItemBody iid
          addItemToActor iid itemBase (-k) aid
      COrgan -> case s2 of
        CEqp -> return ()
        _ -> do
          itemBase <- getsState $ getItemBody iid
          addItemToActor iid itemBase (-k) aid
      _ ->
        when (s2 `elem` [CEqp, COrgan]) $ do
          itemBase <- getsState $ getItemBody iid
          addItemToActor iid itemBase k aid
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
  UpdAlterTile lid pos _fromTile toTile -> do
    updateSalter lid [(pos, toTile)]
    cops <- getsState scops
    let lvl = (EM.! lid) . sdungeon $ oldState
        assumedTile = lvl `at` pos
    when (tileChangeAffectsBfs cops assumedTile toTile) $
      invalidateBfsLid lid
  UpdSpotTile lid ts -> do
    updateSalter lid ts
    cops <- getsState scops
    let lvl = (EM.! lid) . sdungeon $ oldState
        affects (pos, toTile) =
          let fromTile = lvl `at` pos
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
  -- UpdPerception lid outPer inPer -> perception lid outPer inPer
  UpdRestart side sdiscoKind sfper s scurChal sdebugCli -> do
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
    putClient cli { sdiscoKind
                  , sfper
                  -- , sundo = [UpdAtomic cmd]
                  , scurChal
                  , snxtChal
                  , snxtScenario
                  , scondInMelee = EM.map (const Nothing) (sdungeon s)
                  , svictories
                  , sdebugCli }
    modifyClient $ \cli1 -> cli1 {salter = createSalter s}
    -- Currently always void, because no actors yet:
    sactorAspect <- createSactorAspect s
    modifyClient $ \cli1 -> cli1 {sactorAspect}
    restartClient
  UpdResume _fid sfperNew -> do
#ifdef WITH_EXPENSIVE_ASSERTIONS
    sfperOld <- getsClient sfper
    let !_A = assert (sfperNew == sfperOld `blame` (sfperNew, sfperOld)) ()
#endif
    modifyClient $ \cli -> cli {sfper=sfperNew}
    s <- getState
    modifyClient $ \cli -> cli {salter = createSalter s}
    sactorAspect <- createSactorAspect s
    modifyClient $ \cli -> cli {sactorAspect}
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
  aspectRecord <- aspectRecordFromActorClient b ais
  let f = EM.insert aid aspectRecord
  modifyClient $ \cli -> cli {sactorAspect = f $ sactorAspect cli}
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
  let f = EM.delete aid
  modifyClient $ \cli -> cli {sactorAspect = f $ sactorAspect cli}

addItemToActor :: MonadClient m => ItemId -> Item -> Int -> ActorId -> m ()
addItemToActor iid itemBase k aid = do
  arItem <- aspectRecordFromItemClient iid itemBase
  let g arActor = sumAspectRecord [(arActor, 1), (arItem, k)]
      f = EM.adjust g aid
  modifyClient $ \cli -> cli {sactorAspect = f $ sactorAspect cli}

addItemToDiscoBenefit :: MonadClient m => ItemId -> Item -> m ()
addItemToDiscoBenefit iid item = do
  cops@Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
  discoBenefit <- getsClient sdiscoBenefit
  case EM.lookup iid discoBenefit of
    Just{} -> return ()  -- already there
    Nothing -> do
      discoKind <- getsClient sdiscoKind
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
discoverKind c iid kmKind = do
  cops@Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
  -- Wipe out BFS, because the player could potentially learn that his items
  -- affect his actors' skills relevant to BFS.
  invalidateBfsAll
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  item <- getsState $ getItemBody iid
  let kind = okind kmKind
      kmMean = meanAspect kind
      benefit = totalUsefulness cops fact (IK.ieffects kind) kmMean item
      f Nothing = Just KindMean{..}
      f Just{} = error $ "already discovered"
                         `showFailure` (c, iid, kmKind)
  -- This adds to @sdiscoBenefit@ only @iid@ and not any other items
  -- that share the same @jkindIx@, so this is broken if such items
  -- are not fully IDed from the start.
  modifyClient $ \cli ->
    cli { sdiscoKind = EM.alter f (jkindIx item) (sdiscoKind cli)
        , sdiscoBenefit = EM.insert iid benefit (sdiscoBenefit cli) }
  -- Each actor's equipment and organs would need to be inspected,
  -- the iid looked up, e.g., if it wasn't in old discoKind, but is in new,
  -- and then aspect record updated, so it's simpler and not much more
  -- expensive to generate new sactorAspect. Optimize only after profiling.
  s <- getState
  sactorAspect <- createSactorAspect s
  modifyClient $ \cli -> cli {sactorAspect}

coverKind :: MonadClient m => Container -> ItemId -> Kind.Id ItemKind -> m ()
coverKind c iid ik = do
  item <- getsState $ getItemBody iid
  let f Nothing = error $ "already covered" `showFailure` (c, iid, ik)
      f (Just KindMean{kmKind}) =
        assert (ik == kmKind `blame` "unexpected covered item kind"
                             `swith` (ik, kmKind)) Nothing
  -- For now, undoing @sdiscoBenefit@ is too much work.
  modifyClient $ \cli ->
    cli {sdiscoKind = EM.alter f (jkindIx item) (sdiscoKind cli)}
  s <- getState
  sactorAspect <- createSactorAspect s
  modifyClient $ \cli -> cli {sactorAspect}

discoverSeed :: MonadClient m => Container -> ItemId -> ItemSeed -> m ()
discoverSeed c iid seed = do
  cops@Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
  -- Wipe out BFS, because the player could potentially learn that his items
  -- affect his actors' skills relevant to BFS.
  invalidateBfsAll
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  discoKind <- getsClient sdiscoKind
  item <- getsState $ getItemBody iid
  totalDepth <- getsState stotalDepth
  case EM.lookup (jkindIx item) discoKind of
    Nothing -> error $ "kind not known"
                       `showFailure` (c, iid, seed)
    Just KindMean{kmKind} -> do
      Level{ldepth} <- getLevel $ jlid item
      let kind = okind kmKind
          aspects = seedToAspect seed kind ldepth totalDepth
          benefit = totalUsefulness cops fact (IK.ieffects kind) aspects item
          f Nothing = Just aspects
          f Just{} = error $ "already discovered"
                             `showFailure` (c, iid, seed)
  -- This adds to @sdiscoBenefit@ only @iid@ and not any other items
  -- that share the same @jkindIx@, so this is broken if such items
  -- are not fully IDed from the start.
      modifyClient $ \cli ->
        cli { sdiscoAspect = EM.alter f iid (sdiscoAspect cli)
            , sdiscoBenefit = EM.insert iid benefit (sdiscoBenefit cli) }
  s <- getState
  sactorAspect <- createSactorAspect s
  modifyClient $ \cli -> cli {sactorAspect}

coverSeed :: MonadClient m => Container -> ItemId -> ItemSeed -> m ()
coverSeed c iid seed = do
  let f Nothing = error $ "already covered" `showFailure` (c, iid, seed)
      f Just{} = Nothing  -- checking that old and new agree is too much work
  -- For now, undoing @sdiscoBenefit@ is too much work.
  modifyClient $ \cli -> cli {sdiscoAspect = EM.alter f iid (sdiscoAspect cli)}
  s <- getState
  sactorAspect <- createSactorAspect s
  modifyClient $ \cli -> cli {sactorAspect}

killExit :: MonadClient m => m ()
killExit = do
  side <- getsClient sside
  debugPossiblyPrint $ "Client" <+> tshow side <+> "quitting."
  modifyClient $ \cli -> cli {squit = True}
  -- Verify that the not saved caches are equal to future reconstructed.
  -- Otherwise, save/restore would change game state.
  sactorAspect <- getsClient sactorAspect
  salter <- getsClient salter
  sbfsD <- getsClient sbfsD
  s <- getState
  let alter = createSalter s
  actorAspect <- createSactorAspect s
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
      !_A2 = assert (sactorAspect == actorAspect
                     `blame` "wrong accumulated sactorAspect on side"
                     `swith` (side, sactorAspect, actorAspect)) ()
      !_A3 = assert (sbfsD `subBfs` bfsD
                     `blame` "wrong accumulated sbfsD on side"
                     `swith` (side, sbfsD, bfsD)) ()
  return ()
