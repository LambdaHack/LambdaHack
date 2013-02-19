-- | Semantics of atomic commands shared by client and server.
module Game.LambdaHack.CmdAtomicSem
  ( cmdAtomicSem, resetsFovAtomic, posCmdAtomic, posDescAtomic, breakCmdAtomic
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
import Game.LambdaHack.Point
import Game.LambdaHack.State
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

cmdAtomicSem :: MonadAction m => CmdAtomic -> m ()
cmdAtomicSem cmd = case cmd of
  CreateActorA aid body -> createActorA aid body
  DestroyActorA aid body -> destroyActorA aid body
  CreateItemA lid iid item k c -> createItemA lid iid item k c
  DestroyItemA lid iid item k c -> destroyItemA lid iid item k c
  SpotActorA aid body -> createActorA aid body
  LoseActorA aid body -> destroyActorA aid body
  SpotItemA lid iid item k c -> createItemA lid iid item k c
  LoseItemA lid iid item k c -> destroyItemA lid iid item k c
  MoveActorA aid fromP toP -> moveActorA aid fromP toP
  WaitActorA aid fromWait toWait -> waitActorA aid fromWait toWait
  DisplaceActorA source target -> displaceActorA source target
  MoveItemA lid iid k c1 c2 -> moveItemA lid iid k c1 c2
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
  PerceptionA _ -> return ()
  SyncA -> return ()

resetsFovAtomic :: MonadActionRO m => FactionId -> CmdAtomic -> m Bool
resetsFovAtomic fid cmd = case cmd of
  CreateActorA _ body -> return $ fid == bfaction body
  DestroyActorA _ _ -> return False  -- FOV left for a bit to see aftermath
  CreateItemA _ _ _ _ _ -> return False  -- unless shines
  DestroyItemA _ _ _ _ _ -> return False  -- ditto
  MoveActorA aid _ _ -> fidEquals fid aid  -- assumption: carries no light
-- TODO: MoveActorCarryingLIghtA _ _ _ -> True
  DisplaceActorA source target -> do
    bs <- fidEquals fid source
    bt <- fidEquals fid target
    return $ source /= target && (bs || bt)
  DominateActorA _ fromFid toFid -> return $ fid `elem` [fromFid, toFid]
  MoveItemA _ _ _ _ _ -> return False  -- unless shiny
  AlterTileA _ _ _ _ -> return True  -- even if pos not visible initially
  SyncA -> return True  -- that's the only meaning of this command
  _ -> return False

fidEquals :: MonadActionRO m => FactionId -> ActorId -> m Bool
fidEquals fid aid = do
  afid <- getsState $ bfaction . getActorBody aid
  return $ fid == afid

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
             => CmdAtomic -> m (Either (Maybe FactionId) [Point])
posCmdAtomic cmd = case cmd of
  CreateActorA _ body -> return $ Right [bpos body]
  DestroyActorA _ body -> return $ Right [bpos body]
  CreateItemA _ _ _ _ c -> singleContainer c
  DestroyItemA _ _ _ _ c -> singleContainer c
  SpotActorA _ body -> return $ Right [bpos body]
  LoseActorA _ body -> return $ Right [bpos body]
  SpotItemA _ _ _ _ c -> singleContainer c
  LoseItemA _ _ _ _ c -> singleContainer c
  MoveActorA _ fromP toP -> return $ Right [fromP, toP]
  WaitActorA aid _ _ -> singleAid aid
  DisplaceActorA source target -> do
    ps <- mapM posOfAid [source, target]
    return $ Right ps
  MoveItemA _ _ _ c1 c2 -> do  -- works even if moved between positions
    p1 <- posOfContainer c1
    p2 <- posOfContainer c2
    return $ Right [p1, p2]
  HealActorA aid _ -> singleAid aid
  HasteActorA aid _ -> singleAid aid
  DominateActorA target _ _ -> singleAid target
  PathActorA aid _ _ -> singleAid aid
  ColorActorA aid _ _ -> singleAid aid
  QuitFactionA _ _ _ -> alwaysKnow
  LeadFactionA fid _ _ -> return $ Left $ Just fid
  AlterTileA _ p _ _ -> return $ Right [p]
  SpotTileA _ diffL -> do
    let ps = map fst diffL
    return $ Right ps
  AlterSecretA _ _ -> return $ Left Nothing  -- none of clients' business
  AlterSmellA _ _ -> alwaysKnow
  DiscoverA _ p _ _ -> return $ Right [p]
  CoverA _ p _ _ -> return $ Right [p]
  PerceptionA _ -> return $ Left Nothing
  SyncA -> return $ Left Nothing

posDescAtomic :: MonadActionRO m
              => DescAtomic -> m (Either (Maybe FactionId) [Point])
posDescAtomic cmd = case cmd of
  StrikeA source target _ _ -> do
    ps <- mapM posOfAid [source, target]
    return $ Right ps
  RecoilA source target _ _ -> do
    ps <- mapM posOfAid [source, target]
    return $ Right ps
  ProjectA aid _ -> singleAid aid
  CatchA aid _ -> singleAid aid
  ActivateA aid _ -> singleAid aid
  CheckA aid _ -> singleAid aid
  TriggerA aid p _ _ -> do
    pa <- posOfAid aid
    return $ Right [pa, p]
  ShunA aid p _ _ -> do
    pa <- posOfAid aid
    return $ Right [pa, p]
  EffectA aid _ -> singleAid aid

alwaysKnow :: Monad m => m (Either (Maybe FactionId) [Point])
alwaysKnow = return $ Right []

posOfAid :: MonadActionRO m => ActorId -> m Point
posOfAid aid = getsState $ bpos . getActorBody aid

posOfContainer :: MonadActionRO m => Container -> m Point
posOfContainer (CFloor pos) = return pos
posOfContainer (CActor aid _) = posOfAid aid

singleAid :: MonadActionRO m => ActorId -> m (Either (Maybe FactionId) [Point])
singleAid aid = do
  p <- posOfAid aid
  return $ Right [p]

singleContainer :: MonadActionRO m
                => Container -> m (Either (Maybe FactionId) [Point])
singleContainer c = do
  p <- posOfContainer c
  return $ Right [p]

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
  MoveItemA lid iid k c1 c2 -> do
    item <- getsState $ getItemBody iid
    return [LoseItemA lid iid item k c1, SpotItemA lid iid item k c2]
  _ -> return [cmd]

-- TODO: assert that the actor's items belong to sitemD.
createActorA :: MonadAction m => ActorId -> Actor -> m ()
createActorA aid body = do
  modifyState $ updateActorD $ EM.insert aid body
  let add Nothing = Just [aid]
      add (Just l) = Just $ aid : l
  updateLevel (blid body) $ updatePrio $ EM.alter add (btime body)

-- TODO: assert that the actor's items belong to sitemD.
-- | Kills an actor. Note: after this command, usually a new leader
-- for the pa rty should be elected.
destroyActorA :: MonadAction m => ActorId -> Actor -> m ()
destroyActorA aid body = do
  let f Nothing = assert `failure` (aid, body)
      f (Just b) = assert (b == body `blame` (aid, b, body)) $ Nothing
  modifyState $ updateActorD $ EM.alter f aid
  let rm Nothing = assert `failure` (aid, body)
      rm (Just l) = assert (aid `elem` l `blame` (aid, body, l)) $
        let l2 = delete aid l
        in if null l2 then Nothing else Just l2
  updateLevel (blid body) $ updatePrio $ EM.alter rm (btime body)

-- | Create a few copies of an item that is already registered for the dungeon
-- (in @sitemRev@ field of @StateServer@).
createItemA :: MonadAction m
            => LevelId -> ItemId -> Item -> Int -> Container -> m ()
createItemA lid iid item k c = assert (k > 0) $ do
  -- The item may or may not be already present in the dungeon.
  let f item1 item2 = assert (item1 == item2) item1
  modifyState $ updateItemD $ EM.insertWith f iid item
  case c of
    CFloor pos -> insertItemFloor lid iid k pos
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
  let upd = EM.unionWith (+) bag
  modifyState $ updateActorD
    $ EM.adjust (\b -> b {bbag = upd (bbag b)}) aid
  modifyState $ updateActorD
    $ EM.adjust (\b -> b {binv = EM.insert l iid (binv b)}) aid
  modifyState $ updateActorBody aid $ \b ->
    b {bletter = max l (bletter b)}

-- | Destroy some copies (possibly not all) of an item.
destroyItemA :: MonadAction m
             => LevelId -> ItemId -> Item -> Int -> Container -> m ()
destroyItemA lid iid _item k c = assert (k > 0) $ do
  -- Do not remove the item from @sitemD@ nor from @sitemRev@,
  -- This is behaviourally equivalent.
  case c of
    CFloor pos -> deleteItemFloor lid iid k pos
    CActor aid _ -> deleteItemActor iid k aid
                    -- Actor's @bletter@ for UI not reset.
                    -- This is OK up to isomorphism.

deleteItemFloor :: MonadAction m
                => LevelId -> ItemId -> Int -> Point -> m ()
deleteItemFloor lid iid k pos =
  let rmFromFloor (Just bag) =
        let nbag = rmFromBag k iid bag
        in if EM.null nbag then Nothing else Just nbag
      rmFromFloor Nothing = assert `failure` (iid, k, pos)
  in updateLevel lid $ updateFloor $ EM.alter rmFromFloor pos

deleteItemActor :: MonadAction m
                => ItemId -> Int -> ActorId -> m ()
deleteItemActor iid k aid =
  modifyState $ updateActorD
  $ EM.adjust (\b -> b {bbag = rmFromBag k iid (bbag b)}) aid

moveActorA :: MonadAction m => ActorId -> Point -> Point -> m ()
moveActorA aid _fromP toP =
  modifyState $ updateActorBody aid $ \b -> b {bpos = toP}

waitActorA :: MonadAction m => ActorId -> Time -> Time -> m ()
waitActorA aid _fromWait toWait =
  modifyState $ updateActorBody aid $ \b -> b {bwait = toWait}

displaceActorA :: MonadAction m => ActorId -> ActorId -> m ()
displaceActorA source target = do
  spos <- getsState $ bpos . getActorBody source
  tpos <- getsState $ bpos . getActorBody target
  modifyState $ updateActorBody source $ \ b -> b {bpos = tpos}
  modifyState $ updateActorBody target $ \ b -> b {bpos = spos}

moveItemA :: MonadAction m
          => LevelId -> ItemId -> Int -> Container -> Container -> m ()
moveItemA lid iid k c1 c2 = assert (k > 0) $ do
  case c1 of
    CFloor pos -> deleteItemFloor lid iid k pos
    CActor aid _ -> deleteItemActor iid k aid
  case c2 of
    CFloor pos -> insertItemFloor lid iid k pos
    CActor aid l -> insertItemActor iid k l aid

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
dominateActorA target fromFid toFid = do
  tb <- getsState $ getActorBody target
  assert (fromFid == bfaction tb `blame` (fromFid, tb, toFid)) $
    modifyState $ updateActorBody target $ \b -> b {bfaction = toFid}

pathActorA :: MonadAction m
           => ActorId -> Maybe [Vector] -> Maybe [Vector] -> m ()
pathActorA aid _fromPath toPath =
  modifyState $ updateActorBody aid $ \b -> b {bpath = toPath}

colorActorA :: MonadAction m
            => ActorId -> Maybe Color.Color -> Maybe Color.Color -> m ()
colorActorA aid _fromCol toCol =
  modifyState $ updateActorBody aid $ \b -> b {bcolor = toCol}

quitFactionA :: MonadAction m
             => FactionId -> Maybe (Bool, Status) -> Maybe (Bool, Status)
             -> m ()
quitFactionA fid _fromSt toSt = do
  let adj fac = fac {gquit = toSt}
  modifyState $ updateFaction $ EM.adjust adj fid

leadFactionA :: MonadAction m
             => FactionId -> (Maybe ActorId) -> (Maybe ActorId) -> m ()
leadFactionA fid source target = assert (source /= target) $ do
  let adj fac = fac {gleader = target}
  modifyState $ updateFaction $ EM.adjust adj fid

-- TODO: if to or from unknownId, update lseen (if to/from explorable,
-- update lclear)
alterTileA :: MonadAction m
           => LevelId -> Point -> Kind.Id TileKind -> Kind.Id TileKind
           -> m ()
alterTileA lid p _fromTile toTile =
  let adj = (Kind.// [(p, toTile)])
  in updateLevel lid $ updateTile adj

-- TODO: if to or from unknownId, update lseen (if to/from explorable,
-- update lclear)
spotTileA :: MonadAction m
          => LevelId -> [(Point, (Kind.Id TileKind, Kind.Id TileKind))] -> m ()
spotTileA lid diffL = do
  let f (p, (_, nv)) = (p, nv)
      dif = map f diffL
      adj = (Kind.// dif)
  updateLevel lid $ updateTile adj

alterSecretA :: MonadAction m => LevelId -> DiffEM Point Time -> m ()
alterSecretA lid diffL = updateLevel lid $ updateSecret $ applyDiffEM diffL

alterSmellA :: MonadAction m => LevelId -> DiffEM Point Time -> m ()
alterSmellA lid diffL = updateLevel lid $ updateSmell $ applyDiffEM diffL

discoverA :: MonadAction m
          => LevelId -> Point -> ItemId -> (Kind.Id ItemKind) -> m ()
discoverA _ _ iid ik = do
  item <- getsState $ getItemBody iid
  modifyState (updateDisco (EM.insert (jkindIx item) ik))

coverA :: MonadAction m
       => LevelId -> Point -> ItemId -> (Kind.Id ItemKind) -> m ()
coverA _ _ iid _ = do
  item <- getsState $ getItemBody iid
  modifyState (updateDisco (EM.delete (jkindIx item)))
