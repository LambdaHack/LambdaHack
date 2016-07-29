{-# LANGUAGE TupleSections #-}
-- | Handle atomic commands received by the client.
module Game.LambdaHack.Client.HandleAtomicM
  ( cmdAtomicSemCli, cmdAtomicFilterCli
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.Bfs
import Game.LambdaHack.Client.BfsM
import Game.LambdaHack.Client.CommonM
import Game.LambdaHack.Client.MonadClient
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
import Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Content.TileKind as TK

-- * RespUpdAtomicAI

-- | Clients keep a subset of atomic commands sent by the server
-- and add some of their own. The result of this function is the list
-- of commands kept for each command received.
cmdAtomicFilterCli :: MonadClient m => UpdAtomic -> m [UpdAtomic]
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
  UpdSearchTile aid p fromTile toTile -> do
    b <- getsState $ getActorBody aid
    lvl <- getLevel $ blid b
    let t = lvl `at` p
    return $!
      if t == fromTile
      then -- Fully ignorant. (No intermediate knowledge possible.)
           [ cmd  -- show the message
           , UpdAlterTile (blid b) p fromTile toTile  -- reveal tile
           ]
      else assert (t == toTile `blame` "LoseTile fails to reset memory"
                               `twith` (aid, p, fromTile, toTile, b, t, cmd))
                  [cmd]  -- Already knows the tile fully, only confirm.
  UpdLearnSecrets aid fromS _toS -> do
    b <- getsState $ getActorBody aid
    lvl <- getLevel $ blid b
    return $! [cmd | lsecret lvl == fromS]  -- secrets not revealed previously
  UpdSpotTile lid ts -> do
    Kind.COps{cotile} <- getsState scops
    lvl <- getLevel lid
    -- We ignore the server resending us hidden versions of the tiles
    -- (and resending us the same data we already got).
    -- If the tiles are changed to other variants of the hidden tile,
    -- we can still verify by searching, and the UI warns us "obscured".
    let notKnown (p, t) = let tClient = lvl `at` p
                          in t /= tClient
                             && (not (knownLsecret lvl && isSecretPos lvl p)
                                 || t /= Tile.hideAs cotile tClient)
        newTs = filter notKnown ts
    return $! if null newTs then [] else [UpdSpotTile lid newTs]
  UpdDiscover c iid _ seed ldepth -> do
    itemD <- getsState sitemD
    case EM.lookup iid itemD of
      Nothing -> return []
      Just item -> do
        discoKind <- getsClient sdiscoKind
        if jkindIx item `EM.member` discoKind
          then do
            discoEffect <- getsClient sdiscoEffect
            if iid `EM.member` discoEffect
              then return []
              else return [UpdDiscoverSeed c iid seed ldepth]
          else return [cmd]
  UpdCover c iid ik _ _ -> do
    itemD <- getsState sitemD
    case EM.lookup iid itemD of
      Nothing -> return []
      Just item -> do
        discoKind <- getsClient sdiscoKind
        if jkindIx item `EM.notMember` discoKind
          then return []
          else do
            discoEffect <- getsClient sdiscoEffect
            if iid `EM.notMember` discoEffect
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
  UpdDiscoverSeed _ iid _ _ -> do
    itemD <- getsState sitemD
    case EM.lookup iid itemD of
      Nothing -> return []
      Just item -> do
        discoKind <- getsClient sdiscoKind
        if jkindIx item `EM.notMember` discoKind
        then return []
        else do
          discoEffect <- getsClient sdiscoEffect
          if iid `EM.member` discoEffect
            then return []
            else return [cmd]
  UpdCoverSeed _ iid _ _ -> do
    itemD <- getsState sitemD
    case EM.lookup iid itemD of
      Nothing -> return []
      Just item -> do
        discoKind <- getsClient sdiscoKind
        if jkindIx item `EM.notMember` discoKind
        then return []
        else do
          discoEffect <- getsClient sdiscoEffect
          if iid `EM.notMember` discoEffect
            then return []
            else return [cmd]
  UpdPerception lid outPer inPer -> do
    -- Here we cheat by setting a new perception outright instead of
    -- in @cmdAtomicSemCli@, to avoid computing perception twice.
    -- TODO: try to assert similar things as for @atomicRemember@:
    -- that posUpdAtomic of all the Lose* commands was visible in old Per,
    -- but is not visible any more.
    perOld <- getPerFid lid
    perception lid outPer inPer
    perNew <- getPerFid lid
    carriedAssocs <- getsState $ flip getCarriedAssocs
    fid <- getsClient sside
    s <- getState
    -- Wipe out actors that just became invisible due to changed FOV.
    -- Worst case is many actors O(n) in an open room of large diameter O(m).
    -- Then a step reveals many positions. Iterating over them via @posToActors@
    -- takes O(m * n) and so is more cosly than interating over all actors
    -- and for each checking inclusion in a set of positions O(n * log m).
    -- OTOH, m is bounded by sight radius and n is unbounded, so we have
    -- O(n) in both cases, especially with huge levels. To help there,
    -- we'd need to keep a dictionary from positions to actors, which means
    -- @posToActors@ is the right approach for now.
    let seenNew = seenAtomicCli False fid perNew
        seenOld = seenAtomicCli False fid perOld
        outFov = totalVisible perOld ES.\\ totalVisible perNew
        outPrio = concatMap (\p -> posToActors p lid s) $ ES.elems outFov
        fActor (aid, b) =
          let ps = posProjBody b
              -- Verify that we forget only previously seen actors.
              !_A = assert (seenOld ps) ()
          in -- We forget only currently invisible actors.
             if seenNew ps
             then Nothing
             else -- Verify that we forget only previously seen actors.
                  let !_A = assert (seenOld ps) ()
                      ais = carriedAssocs b
                  in Just $ UpdLoseActor aid b ais
        outActor = mapMaybe fActor outPrio
    -- Wipe out remembered items on tiles that now came into view.
    lvl <- getLevel lid
    let inFov = ES.elems $ totalVisible perNew ES.\\ totalVisible perOld
        pMaybe p = maybe Nothing (\x -> Just (p, x))
        inContainer fc itemFloor =
          let inItem = mapMaybe (\p -> pMaybe p $ EM.lookup p itemFloor) inFov
              fItem p (iid, kit) =
                UpdLoseItem iid (getItemBody iid s) kit (fc lid p)
              fBag (p, bag) = map (fItem p) $ EM.assocs bag
          in concatMap fBag inItem
        inFloor = inContainer CFloor (lfloor lvl)
        inEmbed = inContainer CEmbed (lembed lvl)
    -- Remembered map tiles not wiped out, due to optimization in @updSpotTile@.
    -- Wipe out remembered smell on tiles that now came into smell Fov.
    let inSmellFov = totalSmelled perNew ES.\\ totalSmelled perOld
        inSm = mapMaybe (\p -> pMaybe p $ EM.lookup p (lsmell lvl))
                        (ES.elems inSmellFov)
        inSmell = if null inSm then [] else [UpdLoseSmell lid inSm]
    let inTileSmell = inFloor ++ inEmbed ++ inSmell
    psItemSmell <- mapM posUpdAtomic inTileSmell
    -- Verify that we forget only previously invisible items and smell.
    let !_A = assert (allB (not . seenOld) psItemSmell) ()
    -- Verify that we forget only currently seen items and smell.
    let !_A = assert (allB seenNew psItemSmell) ()
    return $! cmd : outActor ++ inTileSmell
  _ -> return [cmd]

-- | Effect of atomic actions on client state is calculated
-- in the global state before the command is executed.
cmdAtomicSemCli :: MonadClientSetup m => UpdAtomic -> m ()
cmdAtomicSemCli cmd = case cmd of
  UpdCreateActor aid body _ -> createActor aid body
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
  UpdAlterTile lid pos _fromTile toTile -> do
    cops <- getsState scops
    lvl <- getLevel lid
    let assumedTile = lvl `at` pos
    when (tileChangeAffectsBfs cops assumedTile toTile) $
      invalidateBfsLid lid
  UpdSpotTile lid ts -> do
    cops <- getsState scops
    lvl <- getLevel lid
    let affects (pos, toTile) =
          let fromTile = lvl `at` pos
          in tileChangeAffectsBfs cops fromTile toTile
        bs = map affects ts
    when (or bs) $ invalidateBfsLid lid
  UpdLoseTile lid _ -> invalidateBfsLid lid  -- from known to unknown tiles
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
  UpdRestart side sdiscoKind sfper _ d sdebugCli -> do
    sisAI <- getsClient sisAI
    snxtDiff <- getsClient snxtDiff
    let cli = (emptyStateClient side) {sisAI}
    putClient cli { sdiscoKind
                  , sfper
                  -- , sundo = [UpdAtomic cmd]
                  , scurDiff = d
                  , snxtDiff
                  , sdebugCli }
    restartClient
  UpdResume _fid sfper -> modifyClient $ \cli -> cli {sfper}
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
tileChangeAffectsBfs Kind.COps{cotile=cotile@Kind.Ops{ouniqGroup}}
                     fromTile toTile =
  let unknownId = ouniqGroup "unknown space"
  in unknownId `elem` [fromTile, toTile]
     || not (Tile.isPassableNoClosed cotile fromTile
             && Tile.isPassableNoClosed cotile toTile
             || not (Tile.isPassable cotile fromTile)
                && not (Tile.isPassable cotile toTile))

createActor :: MonadClient m => ActorId -> Actor -> m ()
createActor aid _b = do
  let affect tgt = case tgt of
        TEnemyPos a _ _ permit | a == aid -> TEnemy a permit
        _ -> tgt
      affect3 tap@TgtAndPath{..} = case tapTgt of
        TEnemyPos a _ _ permit | a == aid -> TgtAndPath (TEnemy a permit) NoPath
        _ -> tap
  modifyClient $ \cli -> cli {stargetD = EM.map affect3 (stargetD cli)}
  modifyClient $ \cli -> cli {sxhair = affect $ sxhair cli}

destroyActor :: MonadClient m => ActorId -> Actor -> Bool -> m ()
destroyActor aid b destroy = do
  when destroy $ modifyClient $ updateTarget aid (const Nothing)  -- gc
  modifyClient $ \cli -> cli {sbfsD = EM.delete aid $ sbfsD cli}  -- gc
  let affect tgt = case tgt of
        TEnemy a _ | a == aid -> TPoint (blid b) (bpos b)
          -- If *really* nothing more interesting, the actor will
          -- go to last known location to perhaps find other foes.
        _ -> tgt
      affect3 TgtAndPath{..} =
        let newMPath = case tapPath of
              AndPath{pathGoal} | pathGoal /= bpos b -> NoPath
              _ -> tapPath  -- foe slow enough, so old path good
        in TgtAndPath (affect tapTgt) newMPath
  modifyClient $ \cli -> cli {stargetD = EM.map affect3 (stargetD cli)}
  modifyClient $ \cli -> cli {sxhair = affect $ sxhair cli}

perception :: MonadClient m => LevelId -> Perception -> Perception -> m ()
perception lid outPer inPer = do
  -- Clients can't compute FOV on their own, because they don't know
  -- if unknown tiles are clear or not. Server would need to send
  -- info about properties of unknown tiles, which complicates
  -- and makes heavier the most bulky data set in the game: tile maps.
  -- Note we assume, but do not check that @outPer@ is contained
  -- in current perception and @inPer@ has no common part with it.
  -- It would make the already very costly operation even more expensive.
  perOld <- getPerFid lid
  -- Check if new perception is already set in @cmdAtomicFilterCli@
  -- or if we are doing undo/redo, which does not involve filtering.
  -- The data structure is strict, so the cheap check can't be any simpler.
  let interAlready per =
        Just $ totalVisible per `ES.intersection` totalVisible perOld
      unset = maybe False ES.null (interAlready inPer)
              || maybe False (not . ES.null) (interAlready outPer)
  when unset $ do
    let adj Nothing = assert `failure` "no perception to alter" `twith` lid
        adj (Just per) = Just $ addPer (diffPer per outPer) inPer
        f = EM.alter adj lid
    modifyClient $ \cli -> cli {sfper = f (sfper cli)}

discoverKind :: MonadClient m
             => Container -> ItemId -> Kind.Id ItemKind -> m ()
discoverKind c iid ik = do
  -- Wipe out BFS, because the player could potentially learn that his items
  -- affect his actors' skills relevant to BFS.
  invalidateBfsAll
  item <- getsState $ getItemBody iid
  let f Nothing = Just ik
      f Just{} = assert `failure` "already discovered"
                        `twith` (c, iid, ik)
  modifyClient $ \cli ->
    cli {sdiscoKind = EM.alter f (jkindIx item) (sdiscoKind cli)}

coverKind :: MonadClient m
          => Container -> ItemId -> Kind.Id ItemKind -> m ()
coverKind c iid ik = do
  item <- getsState $ getItemBody iid
  let f Nothing = assert `failure` "already covered" `twith` (c, iid, ik)
      f (Just ik2) = assert (ik == ik2 `blame` "unexpected covered item kind"
                                       `twith` (ik, ik2)) Nothing
  modifyClient $ \cli ->
    cli {sdiscoKind = EM.alter f (jkindIx item) (sdiscoKind cli)}

discoverSeed :: MonadClient m
             => Container -> ItemId -> ItemSeed -> AbsDepth -> m ()
discoverSeed c iid seed ldepth = do
  -- Wipe out BFS, because the player could potentially learn that his items
  -- affect his actors' skills relevant to BFS.
  invalidateBfsAll
  Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
  discoKind <- getsClient sdiscoKind
  item <- getsState $ getItemBody iid
  totalDepth <- getsState stotalDepth
  case EM.lookup (jkindIx item) discoKind of
    Nothing -> assert `failure` "kind not known"
                      `twith` (c, iid, seed)
    Just ik -> do
      let kind = okind ik
          f Nothing = Just $ seedToAspectsEffects seed kind ldepth totalDepth
          f Just{} = assert `failure` "already discovered"
                            `twith` (c, iid, seed)
      modifyClient $ \cli ->
        cli {sdiscoEffect = EM.alter f iid (sdiscoEffect cli)}

coverSeed :: MonadClient m
          => Container -> ItemId -> ItemSeed -> m ()
coverSeed c iid seed = do
  let f Nothing = assert `failure` "already covered" `twith` (c, iid, seed)
      f Just{} = Nothing  -- checking that old and new agree is too much work
  modifyClient $ \cli -> cli {sdiscoEffect = EM.alter f iid (sdiscoEffect cli)}

killExit :: MonadClient m => m ()
killExit = modifyClient $ \cli -> cli {squit = True}
