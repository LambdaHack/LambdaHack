-- | Semantics of atomic commands shared by client and server.
module Game.LambdaHack.CmdAtomicSem
  ( cmdAtomicSem, resetsFovAtomic, posCmdAtomic, posDescAtomic
  ) where

import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.List
import Data.Maybe

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.CmdAtomic
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Content.ActorKind
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
  AlterSecretA lid diffL -> alterSecretA lid diffL
  AlterSmellA lid diffL -> alterSmellA lid diffL
  SyncA -> return ()

resetsFovAtomic :: MonadActionRO m => FactionId -> CmdAtomic -> m Bool
resetsFovAtomic fid cmd = case cmd of
  CreateActorA _ body -> return $ fid == bfaction body
  DestroyActorA _ _ -> return False  -- FOV left for 1 turn to see aftermath
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

-- | Produces the positions where the action takes place: a sets where
-- the action starts and a set where the action ends. The boolean indicates
-- that, regardless of the position, the start (or end) is always visible
-- to clients (or always invisible).
posCmdAtomic :: MonadActionRO m
             => CmdAtomic -> m (Either Bool [Point], Either Bool [Point])
posCmdAtomic cmd = case cmd of
  CreateActorA _ body ->
    -- Faction sees spawning of its actor, even if it spawns out of sight.
    return (Left True, Right [bpos body])
  DestroyActorA _ body -> return (Right [bpos body], Left True)
  CreateItemA _ _ _ _ c -> singlePos $ posOfContainer c
  DestroyItemA _ _ _ _ c -> singlePos $ posOfContainer c
  MoveActorA _ fromP toP -> return (Right [fromP], Right [toP])
  WaitActorA aid _ _ -> singlePos $ posOfAid aid
  DisplaceActorA source target -> do
    ps <- mapM posOfAid [source, target]
    return (Right ps, Right ps)
  MoveItemA _ _ _ c1 c2 -> do  -- works even if moved between positions
    p1 <- posOfContainer c1
    p2 <- posOfContainer c2
    return (Right [p1], Right [p2])
  HealActorA aid _ -> singlePos $ posOfAid aid
  HasteActorA aid _ -> singlePos $ posOfAid aid
  DominateActorA target _ _ -> singlePos $ posOfAid target
  PathActorA aid _ _ -> singlePos $ posOfAid aid
  ColorActorA aid _ _ -> singlePos $ posOfAid aid
  QuitFactionA _ _ _ -> return (Left True, Left True)  -- always learn
  LeadFactionA _ source target -> do
    ps <- mapM posOfAid $ catMaybes [source, target]
    return (Right ps, Right ps)
  AlterTileA _ p _ _ -> singlePos $ return p
  AlterSecretA _ _ ->
    return (Left False, Left False)  -- none of clients' business
  AlterSmellA _ _ -> return (Left True, Left True)  -- always register
  SyncA -> return (Left False, Left False)

posDescAtomic :: MonadActionRO m
              => DescAtomic -> m (Either Bool [Point], Either Bool [Point])
posDescAtomic cmd = case cmd of
  StrikeA source target _ _ -> do
    ps <- mapM posOfAid [source, target]
    return (Right ps, Right ps)
  RecoilA source target _ _ -> do
    ps <- mapM posOfAid [source, target]
    return (Right ps, Right ps)
  ProjectA aid _ -> singlePos $ posOfAid aid
  CatchA aid _ -> singlePos $ posOfAid aid
  ActivateA aid _ -> singlePos $ posOfAid aid
  CheckA aid _ -> singlePos $ posOfAid aid
  TriggerA aid p _ _ -> do
    pa <- posOfAid aid
    return (Right [pa], Right [pa, p])
  ShunA aid p _ _ -> do
    pa <- posOfAid aid
    return (Right [pa], Right [pa, p])

singlePos :: MonadActionAbort m
          => m Point -> m (Either Bool [Point], Either Bool [Point])
singlePos m = do
  pos <- m
  return (Right [pos], Right [pos])

posOfAid :: MonadActionRO m => ActorId -> m Point
posOfAid aid = getsState $ bpos . getActorBody aid

posOfContainer :: MonadActionRO m => Container -> m Point
posOfContainer (CFloor pos) = return pos
posOfContainer (CActor aid _) = posOfAid aid

-- TODO: perhaps assert that the inventory of the actor is empty
-- or at least that the items belong to litem.
createActorA :: MonadAction m => ActorId -> Actor -> m ()
createActorA aid body = do
  modifyState $ updateActorD $ EM.insert aid body
  let add Nothing = Just [aid]
      add (Just l) = Just $ aid : l
  updateLevel (blid body) $ updatePrio $ EM.alter add (btime body)
  let fid = bfaction body
      adj fac = fac {gleader = Just aid}
  mleader <- getsState $ gleader . (EM.! fid) . sfaction
  when (mleader == Nothing) $
    modifyState $ updateFaction $ EM.adjust adj fid

-- TODO: perhaps assert that the inventory of the actor is empty.
-- | Kills an actor. Note: after this command, usually a new leader
-- for the party should be elected.
destroyActorA :: MonadAction m => ActorId -> Actor -> m ()
destroyActorA aid body = do
  modifyState $ updateActorD $ EM.delete aid
  let rm Nothing = assert `failure` aid
      rm (Just l) = let l2 = delete aid l
                    in if null l2 then Nothing else Just l2
  updateLevel (blid body) $ updatePrio $ EM.alter rm (btime body)
  let fid = bfaction body
      adj fac = fac {gleader = Nothing}
  mleader <- getsState $ gleader . (EM.! fid) . sfaction
  when (mleader == Just aid) $
    modifyState $ updateFaction $ EM.adjust adj fid

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
  tm <- getsState (getActorBody target)
  assert (fromFid == bfaction tm `blame` (fromFid, tm, toFid)) $
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

alterTileA :: MonadAction m
                => LevelId -> Point -> Kind.Id TileKind -> Kind.Id TileKind
                -> m ()
alterTileA lid p _fromTile toTile =
  let adj = (Kind.// [(p, toTile)])
  in updateLevel lid $ updateTile adj

alterSecretA :: MonadAction m => LevelId -> DiffEM Point Time -> m ()
alterSecretA lid diffL =
  updateLevel lid $ updateSecret $ applyDiffEM diffL

alterSmellA :: MonadAction m => LevelId -> DiffEM Point Time -> m ()
alterSmellA lid diffL =
  updateLevel lid $ updateSmell $ applyDiffEM diffL
