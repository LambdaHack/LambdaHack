-- | Semantics of atomic commands shared by client and server.
module Game.LambdaHack.CmdAtomicSem
  ( cmdAtomicSem, resetsFovAtomic, posCmdAtomic, posDescAtomic
  , breakCmdAtomic, loudCmdAtomic
  ) where

import qualified Data.EnumMap.Strict as EM
import Data.List
import Data.Maybe

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.CmdAtomic
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.ItemKind as ItemKind
import Game.LambdaHack.Content.TileKind as TileKind
import Game.LambdaHack.Faction
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Misc
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.State
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

cmdAtomicSem :: MonadAction m => CmdAtomic -> m ()
cmdAtomicSem cmd = case cmd of
  CreateActorA aid body -> createActorA aid body
  DestroyActorA aid body -> destroyActorA aid body
  CreateItemA iid item k c -> createItemA iid item k c
  DestroyItemA iid item k c -> destroyItemA iid item k c
  SpotActorA aid body -> createActorA aid body
  LoseActorA aid body -> destroyActorA aid body
  SpotItemA iid item k c -> createItemA iid item k c
  LoseItemA iid item k c -> destroyItemA iid item k c
  MoveActorA aid fromP toP -> moveActorA aid fromP toP
  WaitActorA aid fromWait toWait -> waitActorA aid fromWait toWait
  DisplaceActorA source target -> displaceActorA source target
  MoveItemA iid k c1 c2 -> moveItemA iid k c1 c2
  AgeActorA aid t -> ageActorA aid t
  HealActorA aid n -> healActorA aid n
  HasteActorA aid delta -> hasteActorA aid delta
  DominateActorA target fromFid toFid -> dominateActorA target fromFid toFid
  PathActorA aid fromPath toPath -> pathActorA aid fromPath toPath
  ColorActorA aid fromCol toCol -> colorActorA aid fromCol toCol
  QuitFactionA fid fromSt toSt -> quitFactionA fid fromSt toSt
  LeadFactionA fid source target -> leadFactionA fid source target
  AlterTileA lid p fromTile toTile -> alterTileA lid p fromTile toTile
  SpotTileA lid diff -> spotTileA lid diff
  AlterSecretA lid diffL -> alterSecretA lid diffL
  AlterSmellA lid diffL -> alterSmellA lid diffL
  DiscoverA lid p iid ik -> discoverA lid p iid ik
  CoverA lid p iid ik -> coverA lid p iid ik
  PerceptionA _ outPA inPA ->
    assert (not (EM.null outPA && EM.null inPA)) $ return ()
  RestartA fid sfper s -> restartA fid sfper s

-- All functions here that take an atomic action are executed
-- in the state just before the action is executed.

resetsFovAtomic :: MonadActionRO m => CmdAtomic -> m (Maybe [FactionId])
resetsFovAtomic cmd = case cmd of
  CreateActorA _ body -> return $ Just [bfaction body]
  DestroyActorA _ _ -> return $ Just []  -- FOV kept for a bit to see aftermath
  CreateItemA _ _ _ _ -> return $ Just []  -- unless shines
  DestroyItemA _ _ _ _ -> return $ Just []  -- ditto
  MoveActorA aid _ _ -> fmap Just $ fidOfAid aid  -- assumption: has no light
-- TODO: MoveActorCarryingLIghtA _ _ _ -> True
  DisplaceActorA source target -> do
    sfid <- fidOfAid source
    tfid <- fidOfAid target
    if source == target
      then return $ Just []
      else return $ Just $ sfid ++ tfid
  DominateActorA _ fromFid toFid -> return $ Just [fromFid, toFid]
  MoveItemA _ _ _ _ -> return $ Just []  -- unless shiny
  AlterTileA _ _ _ _ -> return Nothing  -- even if pos not visible initially
  _ -> return $ Just []

fidOfAid :: MonadActionRO m => ActorId -> m [FactionId]
fidOfAid aid = getsState $ (: []) . bfaction . getActorBody aid

-- | Produces the positions where the action takes place. If a faction
-- is returned, the action is visible only for that faction, if Nothing
-- is returned, it's never visible. Empty list of positions implies
-- the action is visible always.
--
-- The goal of the mechanics: client should not get significantly
-- more information by looking at the atomic commands he is able to see
-- than by looking at the state changes they enact. E.g., @DisplaceActorA@
-- in a black room, with one actor carrying a 0-radius light would not be
-- distinguishable by looking at the state (or the screen) from @MoveActorA@
-- of the illuminated actor, hence such @DisplaceActorA@ should not be
-- observable, but @MoveActorA@ should be (or the former should be perceived
-- as the latter). However, to simplify, we assing as strict visibility
-- requirements to @MoveActorA@ as to @DisplaceActorA@ and fall back
-- to @SpotActorA@ (which provides minimal information that does not
-- contradict state) if the visibility is lower.
posCmdAtomic :: MonadActionRO m
             => CmdAtomic
             -> m (Either (Either Bool FactionId) (LevelId, [Point]))
posCmdAtomic cmd = case cmd of
  CreateActorA _ body -> return $ Right (blid body, [bpos body])
  DestroyActorA _ body -> return $ Right (blid body, [bpos body])
  CreateItemA _ _ _ c -> singleContainer c
  DestroyItemA _ _ _ c -> singleContainer c
  SpotActorA _ body -> return $ Right (blid body, [bpos body])
  LoseActorA _ body -> return $ Right (blid body, [bpos body])
  SpotItemA _ _ _ c -> singleContainer c
  LoseItemA _ _ _ c -> singleContainer c
  MoveActorA aid fromP toP -> do
    (lid, _) <- posOfAid aid
    return $ Right (lid, [fromP, toP])
  WaitActorA aid _ _ -> singleAid aid
  DisplaceActorA source target -> do
    (slid, sp) <- posOfAid source
    (tlid, tp) <- posOfAid target
    return $ assert (slid == tlid) $ Right (slid, [sp, tp])
  MoveItemA _ _ c1 c2 -> do  -- works even if moved between positions
    (lid1, p1) <- posOfContainer c1
    (lid2, p2) <- posOfContainer c2
    return $ assert (lid1 == lid2) $ Right (lid1, [p1, p2])
  AgeActorA aid _ -> singleAid aid
  HealActorA aid _ -> singleAid aid
  HasteActorA aid _ -> singleAid aid
  DominateActorA target _ _ -> singleAid target
  PathActorA aid _ _ -> singleAid aid
  ColorActorA aid _ _ -> singleAid aid
  QuitFactionA _ _ _ -> return $ Left $ Left True  -- all always notice this
  LeadFactionA fid _ _ -> return $ Left $ Right fid  -- a faction's secret
  AlterTileA lid p _ _ -> return $ Right (lid, [p])
  SpotTileA lid diffL -> do
    let ps = map fst diffL
    return $ Right (lid, ps)
  AlterSecretA _ _ -> return $ Left $ Left False  -- none of clients' business
  AlterSmellA _ _ -> return $ Left $ Left True
  DiscoverA lid p _ _ -> return $ Right (lid, [p])
  CoverA lid p _ _ -> return $ Right (lid, [p])
  PerceptionA _ _ _ -> return $ Left $ Left False
  RestartA fid _ _ -> return $ Left $ Right fid

posDescAtomic :: MonadActionRO m
              => DescAtomic
              -> m (Either (Either Bool FactionId) (LevelId, [Point]))
posDescAtomic cmd = case cmd of
  StrikeD source target _ _ -> do
    (slid, sp) <- posOfAid source
    (tlid, tp) <- posOfAid target
    return $ assert (slid == tlid) $ Right (slid, [sp, tp])
  RecoilD source target _ _ -> do
    (slid, sp) <- posOfAid source
    (tlid, tp) <- posOfAid target
    return $ assert (slid == tlid) $ Right (slid, [sp, tp])
  ProjectD aid _ -> singleAid aid
  CatchD aid _ -> singleAid aid
  ActivateD aid _ -> singleAid aid
  CheckD aid _ -> singleAid aid
  TriggerD aid p _ _ -> do
    (lid, pa) <- posOfAid aid
    return $ Right (lid, [pa, p])
  ShunD aid p _ _ -> do
    (lid, pa) <- posOfAid aid
    return $ Right (lid, [pa, p])
  EffectD aid _ -> singleAid aid
  FailureD fid _ -> return $ Left $ Right fid  -- failures are secret
  BroadcastD _ -> return $ Left $ Left True
  DisplayPushD fid -> return $ Left $ Right fid
  DisplayDelayD fid -> return $ Left $ Right fid
  FlushFramesD fid -> return $ Left $ Right fid

posOfAid :: MonadActionRO m => ActorId -> m (LevelId, Point)
posOfAid aid = do
  b <- getsState $ getActorBody aid
  return (blid b, bpos b)

posOfContainer :: MonadActionRO m => Container -> m (LevelId, Point)
posOfContainer (CFloor lid p) = return (lid, p)
posOfContainer (CActor aid _) = posOfAid aid

singleAid :: MonadActionRO m
          => ActorId -> m (Either (Either Bool FactionId) (LevelId, [Point]))
singleAid aid = do
  (lid, p) <- posOfAid aid
  return $ Right (lid, [p])

singleContainer :: MonadActionRO m
                => Container
                -> m (Either (Either Bool FactionId) (LevelId, [Point]))
singleContainer c = do
  (lid, p) <- posOfContainer c
  return $ Right (lid, [p])

-- | Decompose an atomic action. The original action is visible
-- if it's positions are visible both before and after the action
-- (in between the FOV might have changed). The decomposed actions
-- are only tested vs the FOV after the action and they give reduced
-- information that still modifies client's state to match the server state
-- wrt the current FOV and the subset of @posCmdAtomic@ that is visible.
-- The original actions give more information not only due to spanning
-- potentially more positions than those visible. E.g., @MoveActorA@
-- informs about the continued existence of the actor between
-- moves, v.s., popping out of existence and then back in.
breakCmdAtomic :: MonadActionRO m => CmdAtomic -> m [CmdAtomic]
breakCmdAtomic cmd = case cmd of
  MoveActorA aid _ toP -> do
    b <- getsState $ getActorBody aid
    return [LoseActorA aid b, SpotActorA aid b {bpos = toP}]
  DisplaceActorA source target -> do
    sb <- getsState $ getActorBody source
    tb <- getsState $ getActorBody target
    return [ LoseActorA source sb, SpotActorA source sb {bpos = bpos tb}
           , LoseActorA target tb, SpotActorA target tb {bpos = bpos sb}]
  MoveItemA iid k c1 c2 -> do
    item <- getsState $ getItemBody iid
    return [LoseItemA iid item k c1, SpotItemA iid item k c2]
  _ -> return [cmd]

loudCmdAtomic :: MonadActionRO m => CmdAtomic -> m Bool
loudCmdAtomic cmd = case cmd of
  DestroyActorA _ body -> return $ not $ bproj body
  AlterTileA{} -> return True
  _ -> return False

createActorA :: MonadAction m => ActorId -> Actor -> m ()
createActorA aid body = do
  -- Assert that actor's items belong to @sitemD@.
  itemD <- getsState sitemD
  let is = EM.keys $ bbag body
  assert (allB (`EM.member` itemD) is `blame` (aid, body, itemD)) skip
  -- Add actor to @sactorD@.
  let f Nothing = Just body
      f (Just b) = assert `failure` (aid, body, b)
  modifyState $ updateActorD $ EM.alter f aid
  -- Add actor to @sprio@.
  let g Nothing = Just [aid]
      g (Just l) = assert (not (aid `elem` l) `blame` (aid, body, l))
                   $ Just $ aid : l
  updateLevel (blid body) $ updatePrio $ EM.alter g (btime body)

-- | Kills an actor. Note: after this command, usually a new leader
-- for the party should be elected.
destroyActorA :: MonadAction m => ActorId -> Actor -> m ()
destroyActorA aid body = do
  -- Assert that actor's items belong to @sitemD@.
  itemD <- getsState sitemD
  let is = EM.keys $ bbag body
  assert (allB (`EM.member` itemD) is `blame` (aid, body, itemD)) skip
  -- Remove actor from @sactorD@.
  let f Nothing = assert `failure` (aid, body)
      f (Just b) = assert (b == body `blame` (aid, body, b)) Nothing
  modifyState $ updateActorD $ EM.alter f aid
  -- Remove actor from @sprio@.
  let g Nothing = assert `failure` (aid, body)
      g (Just l) = assert (aid `elem` l `blame` (aid, body, l))
                   $ let l2 = delete aid l
                     in if null l2 then Nothing else Just l2
  updateLevel (blid body) $ updatePrio $ EM.alter g (btime body)

-- | Create a few copies of an item that is already registered for the dungeon
-- (in @sitemRev@ field of @StateServer@).
createItemA :: MonadAction m => ItemId -> Item -> Int -> Container -> m ()
createItemA iid item k c = assert (k > 0) $ do
  -- The item may or may not be already present in @sitemD@,
  -- regardless if it's actually present in the dungeon.
  let f item1 item2 = assert (item1 == item2 `blame` (iid, item, k, c)) item1
  modifyState $ updateItemD $ EM.insertWith f iid item
  case c of
    CFloor lid pos -> insertItemFloor lid iid k pos
    CActor aid l -> insertItemActor iid k l aid

insertItemFloor :: MonadAction m
                => LevelId -> ItemId -> Int -> Point -> m ()
insertItemFloor lid iid k pos =
  let bag = EM.singleton iid k
      mergeBag = EM.insertWith (EM.unionWith (+)) pos bag
  in updateLevel lid $ updateFloor mergeBag

insertItemActor :: MonadAction m
                => ItemId -> Int -> InvChar -> ActorId -> m ()
insertItemActor iid k l aid = do
  let bag = EM.singleton iid k
      upd = EM.unionWith (+) bag
  modifyState $ updateActorD $
    EM.adjust (\b -> b {bbag = upd (bbag b)}) aid
  modifyState $ updateActorD $
    EM.adjust (\b -> b {binv = EM.insert l iid (binv b)}) aid
  modifyState $ updateActorBody aid $ \b ->
    b {bletter = max l (bletter b)}

-- | Destroy some copies (possibly not all) of an item.
destroyItemA :: MonadAction m => ItemId -> Item -> Int -> Container -> m ()
destroyItemA iid item k c = assert (k > 0) $ do
  -- Do not remove the item from @sitemD@ nor from @sitemRev@,
  -- It's incredibly costly and not noticeable for the player.
  -- However, assert the item is registered in @sitemD@.
  itemD <- getsState sitemD
  assert (iid `EM.lookup` itemD == Just item `blame` (iid, item, itemD)) skip
  case c of
    CFloor lid pos -> deleteItemFloor lid iid k pos
    CActor aid l -> deleteItemActor iid k l aid

deleteItemFloor :: MonadAction m
                => LevelId -> ItemId -> Int -> Point -> m ()
deleteItemFloor lid iid k pos =
  let rmFromFloor (Just bag) =
        let nbag = rmFromBag k iid bag
        in if EM.null nbag then Nothing else Just nbag
      rmFromFloor Nothing = assert `failure` (lid, iid, k, pos)
  in updateLevel lid $ updateFloor $ EM.alter rmFromFloor pos

deleteItemActor :: MonadAction m
                => ItemId -> Int -> InvChar -> ActorId -> m ()
deleteItemActor iid k l aid = do
  modifyState $ updateActorD $
    EM.adjust (\b -> b {bbag = rmFromBag k iid (bbag b)}) aid
  -- Do not remove from actor's @binv@, but assert it was there.
  b <- getsState $ getActorBody aid
  assert (l `EM.lookup` binv b == Just iid `blame` (iid, l, aid)) skip
  -- Actor's @bletter@ for UI not reset, but checked.
  assert (bletter b >= l`blame` (iid, k, l, aid, bletter b)) skip

moveActorA :: MonadAction m => ActorId -> Point -> Point -> m ()
moveActorA aid fromP toP = assert (fromP /= toP) $ do
  b <- getsState $ getActorBody aid
  assert (fromP == bpos b `blame` (aid, fromP, toP, bpos b)) skip
  modifyState $ updateActorBody aid $ \body -> body {bpos = toP}

waitActorA :: MonadAction m => ActorId -> Time -> Time -> m ()
waitActorA aid fromWait toWait = assert (fromWait /= toWait) $ do
  b <- getsState $ getActorBody aid
  assert (fromWait == bwait b `blame` (aid, fromWait, toWait, bwait b)) skip
  modifyState $ updateActorBody aid $ \body -> body {bwait = toWait}

displaceActorA :: MonadAction m => ActorId -> ActorId -> m ()
displaceActorA source target = assert (source /= target) $ do
  spos <- getsState $ bpos . getActorBody source
  tpos <- getsState $ bpos . getActorBody target
  modifyState $ updateActorBody source $ \ b -> b {bpos = tpos}
  modifyState $ updateActorBody target $ \ b -> b {bpos = spos}

moveItemA :: MonadAction m => ItemId -> Int -> Container -> Container -> m ()
moveItemA iid k c1 c2 = assert (k > 0 && c1 /= c2) $ do
  (lid1, _) <- posOfContainer c1
  (lid2, _) <- posOfContainer c2
  assert (lid1 == lid2 `blame` (iid, k, c1, c2, lid1, lid2)) skip
  case c1 of
    CFloor lid pos -> deleteItemFloor lid iid k pos
    CActor aid l -> deleteItemActor iid k l aid
  case c2 of
    CFloor lid pos -> insertItemFloor lid iid k pos
    CActor aid l -> insertItemActor iid k l aid

ageActorA :: MonadAction m => ActorId -> Time -> m ()
ageActorA aid t = assert (t >= timeZero) $ do
  body <- getsState $ getActorBody aid
  destroyActorA aid body
  createActorA aid body {btime = timeAdd (btime body) t}

healActorA :: MonadAction m => ActorId -> Int -> m ()
healActorA aid n = assert (n /= 0) $
  modifyState $ updateActorBody aid $ \b -> b {bhp = n + bhp b}

hasteActorA :: MonadAction m => ActorId -> Speed -> m ()
hasteActorA aid delta = assert (delta /= speedZero) $ do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  modifyState $ updateActorBody aid $ \ b ->
    let innateSpeed = aspeed $ okind $ bkind b
        curSpeed = fromMaybe innateSpeed (bspeed b)
        newSpeed = speedAdd curSpeed delta
    in assert (newSpeed >= speedZero `blame` (aid, curSpeed, delta)) $
       if curSpeed == innateSpeed
       then b {bspeed = Nothing}
       else b {bspeed = Just newSpeed}

dominateActorA :: MonadAction m => ActorId -> FactionId -> FactionId -> m ()
dominateActorA target fromFid toFid = assert (fromFid /= toFid) $ do
  tb <- getsState $ getActorBody target
  assert (fromFid == bfaction tb `blame` (target, fromFid, toFid, tb)) skip
  modifyState $ updateActorBody target $ \b -> b {bfaction = toFid}

pathActorA :: MonadAction m
           => ActorId -> Maybe [Vector] -> Maybe [Vector] -> m ()
pathActorA aid fromPath toPath = assert (fromPath /= toPath) $ do
  body <- getsState $ getActorBody aid
  assert (fromPath == bpath body `blame` (aid, fromPath, toPath, body)) skip
  modifyState $ updateActorBody aid $ \b -> b {bpath = toPath}

colorActorA :: MonadAction m
            => ActorId -> Maybe Color.Color -> Maybe Color.Color -> m ()
colorActorA aid fromCol toCol = assert (fromCol /= toCol) $ do
  body <- getsState $ getActorBody aid
  assert (fromCol == bcolor body `blame` (aid, fromCol, toCol, body)) skip
  modifyState $ updateActorBody aid $ \b -> b {bcolor = toCol}

quitFactionA :: MonadAction m
             => FactionId -> Maybe (Bool, Status) -> Maybe (Bool, Status)
             -> m ()
quitFactionA fid fromSt toSt = assert (fromSt /= toSt) $ do
  fac <- getsState $ (EM.! fid) . sfaction
  assert (fromSt == gquit fac `blame` (fid, fromSt, toSt, fac)) skip
  let adj fa = fa {gquit = toSt}
  modifyState $ updateFaction $ EM.adjust adj fid

-- The previous leader is assumed to be alive.
leadFactionA :: MonadAction m
             => FactionId -> (Maybe ActorId) -> (Maybe ActorId) -> m ()
leadFactionA fid source target = assert (source /= target) $ do
  fac <- getsState $ (EM.! fid) . sfaction
  assert (source == gleader fac `blame` (fid, source, target, fac)) skip
  let adj fa = fa {gleader = target}
  modifyState $ updateFaction $ EM.adjust adj fid

-- TODO: if to or from unknownId, update lseen (if to/from explorable,
-- update lclear)
alterTileA :: MonadAction m
           => LevelId -> Point -> Kind.Id TileKind -> Kind.Id TileKind
           -> m ()
alterTileA lid p fromTile toTile = assert (fromTile /= toTile) $ do
  let adj ts = assert (ts Kind.! p == fromTile) $ ts Kind.// [(p, toTile)]
  updateLevel lid $ updateTile adj

-- TODO: if to or from unknownId, update lseen (if to/from explorable,
-- update lclear)
spotTileA :: MonadAction m
          => LevelId -> [(Point, (Kind.Id TileKind, Kind.Id TileKind))] -> m ()
spotTileA lid diffL = do
  let f (p, (ov, nv)) = assert (ov /= nv) $ ((p, ov), (p, nv))
      diff2 = unzip $ map f diffL
      matches _ [] = True
      matches ts ((p, ov) : rest) = True ||  -- TODO: let client handle memory
        ts Kind.! p == ov && matches ts rest
      adj ts = assert (matches ts (fst diff2)) $ ts Kind.// snd diff2
  updateLevel lid $ updateTile adj

alterSecretA :: MonadAction m => LevelId -> DiffEM Point Time -> m ()
alterSecretA lid diffL = updateLevel lid $ updateSecret $ applyDiffEM diffL

alterSmellA :: MonadAction m => LevelId -> DiffEM Point Time -> m ()
alterSmellA lid diffL = updateLevel lid $ updateSmell $ applyDiffEM diffL

discoverA :: MonadAction m
          => LevelId -> Point -> ItemId -> (Kind.Id ItemKind) -> m ()
discoverA lid p iid ik = do
  item <- getsState $ getItemBody iid
  let f Nothing = Just ik
      f (Just ik2) = assert `failure` (lid, p, iid, ik, ik2)
  modifyState $ updateDisco $ EM.alter f (jkindIx item)

-- TODO: as a result of undo, this will be wrong on the server,
-- so it has to be filtered out from the server, just as from some clients.
coverA :: MonadAction m
       => LevelId -> Point -> ItemId -> (Kind.Id ItemKind) -> m ()
coverA lid p iid ik = do
  item <- getsState $ getItemBody iid
  let f Nothing = assert `failure` (lid, p, iid, ik)
      f (Just ik2) = assert (ik == ik2 `blame` (ik, ik2)) Nothing
  modifyState $ updateDisco $ EM.alter f (jkindIx item)

-- TODO here, too, it would be disastrous for the server.
-- Perhaps the server should have factionId -1, so that ocmparison
-- with fid below will be False.
restartA :: MonadAction m => FactionId -> FactionPers -> State -> m ()
restartA _ _ s = putState s
