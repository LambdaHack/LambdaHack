-- | Semantics of atomic commands shared by client and server.
module Game.LambdaHack.CmdAtomicSem
  ( cmdAtomicSem, resetsFovAtomic, cmdPosAtomic
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
  HealAtomic n aid -> healAtomic n aid
  HasteAtomic aid delta -> hasteAtomic aid delta
  DominateAtomic fromFid toFid target -> dominateAtomic fromFid toFid target
  SpawnAtomic aid body -> spawnAtomic aid body
  KillAtomic aid body -> killAtomic aid body
  CreateItemAtomic lid iid item k c -> createItemAtomic lid iid item k c
  DestroyItemAtomic lid iid item k c -> destroyItemAtomic lid iid item k c
  MoveItemAtomic lid iid k c1 c2 -> moveItemAtomic lid iid k c1 c2
  WaitAtomic aid fromWait toWait -> waitAtomic aid fromWait toWait
  ChangeTileAtomic p lid fromTile toTile ->
    changeTileAtomic p lid fromTile toTile
  MoveActorAtomic aid fromP toP -> moveActorAtomic aid fromP toP
  DisplaceActorAtomic source target -> displaceActorAtomic source target
  AlterSecretAtomic lid diffL -> alterSecretAtomic lid diffL
  AlterSmellAtomic lid diffL -> alterSmellAtomic lid diffL
  AlterPathAtomic aid fromPath toPath -> alterPathAtomic aid fromPath toPath
  ColorActorAtomic aid fromCol toCol -> colorActorAtomic aid fromCol toCol
  FactionQuitAtomic fid fromSt toSt -> factionQuitAtomic fid fromSt toSt
  LeaderAtomic fid source target -> leaderAtomic fid source target
  SyncAtomic -> return ()

resetsFovAtomic :: MonadActionRO m => FactionId -> CmdAtomic -> m Bool
resetsFovAtomic fid cmd = case cmd of
  DominateAtomic source target _ -> return $ fid `elem` [source, target]
  SpawnAtomic _ body -> return $ fid == bfaction body
  KillAtomic _ _ -> return False  -- FOV left for 1 turn to see aftermath
  CreateItemAtomic _ _ _ _ _ -> return False  -- unless shines
  DestroyItemAtomic _ _ _ _ _ -> return False  -- ditto
  MoveItemAtomic _ _ _ _ _ -> return False  -- assumption: stays on same pos
  ChangeTileAtomic _ _ _ _ -> return True  -- even if pos not visible initially
  MoveActorAtomic aid _ _ -> fidEquals fid aid  -- assumption: carries no light
-- TODO: MoveActorCarryingLIghtAtomic _ _ _ -> True
  DisplaceActorAtomic source target -> do
    bs <- fidEquals fid source
    bt <- fidEquals fid target
    return $ source /= target && (bs || bt)
  SyncAtomic -> return True  -- that's the only meaning of this command
  _ -> return False

fidEquals :: MonadActionRO m => FactionId -> ActorId -> m Bool
fidEquals fid aid = do
  afid <- getsState $ bfaction . getActorBody aid
  return $ fid == afid

-- | Produces the positions where the action takes place: a sets where
-- the action starts and a set where the action ends. The boolean indicates
-- that, regardless of the position, the start (or end) is always visible
-- to clients (or always invisible).
cmdPosAtomic :: MonadActionRO m
             => CmdAtomic -> m (Either Bool [Point], Either Bool [Point])
cmdPosAtomic cmd = case cmd of
  HealAtomic _ aid -> singlePos $ posOfAid aid
  HasteAtomic aid _ -> singlePos $ posOfAid aid
  DominateAtomic _ _ target -> singlePos $ posOfAid target
  SpawnAtomic _ body ->
    -- Faction sees spawning of its actor, even if it spawns out of sight.
    return (Left True, Right [bpos body])
  KillAtomic _ body -> return (Right [bpos body], Left True)
  CreateItemAtomic _ _ _ _ c -> singlePos $ posOfContainer c
  DestroyItemAtomic _ _ _ _ c -> singlePos $ posOfContainer c
  MoveItemAtomic _ _ _ c1 c2 -> do  -- works even if moved between positions
    p1 <- posOfContainer c1
    p2 <- posOfContainer c2
    return (Right [p1], Right [p2])
  WaitAtomic aid _ _ -> singlePos $ posOfAid aid
  ChangeTileAtomic p _ _ _ -> singlePos $ return p
  MoveActorAtomic _ fromP toP -> return (Right [fromP], Right [toP])
  DisplaceActorAtomic source target -> do
    ps <- mapM posOfAid [source, target]
    return (Right ps, Right ps)
  AlterSecretAtomic _ _ ->
    return (Left False, Left False)  -- none of clients' business
  AlterSmellAtomic _ _ -> return (Left True, Left True)  -- always register
  AlterPathAtomic aid _ _ -> singlePos $ posOfAid aid
  ColorActorAtomic aid _ _ -> singlePos $ posOfAid aid
  FactionQuitAtomic _ _ _ -> return (Left True, Left True)  -- always learn
  LeaderAtomic _ source target -> do
    ps <- mapM posOfAid $ catMaybes [source, target]
    return (Right ps, Right ps)
  SyncAtomic -> return (Left False, Left False)

singlePos :: MonadActionAbort m
          => m Point -> m (Either Bool [Point], Either Bool [Point])
singlePos m = do
  pos <- m
  return (Right [pos], Right [pos])

posOfAid :: MonadActionRO m => ActorId -> m Point
posOfAid aid = getsState $ bpos . getActorBody aid

posOfContainer :: MonadActionRO m => Container -> m Point
posOfContainer (CFloor pos) = return pos
posOfContainer (CActor aid) = posOfAid aid

healAtomic :: MonadAction m => Int -> ActorId -> m ()
healAtomic n aid = assert (n /= 0) $
  modifyState $ updateActorBody aid $ \b -> b {bhp = n + bhp b}

hasteAtomic :: MonadAction m => ActorId -> Speed -> m ()
hasteAtomic aid delta = assert (delta /= speedZero) $ do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  modifyState $ updateActorBody aid $ \ b ->
    let innateSpeed = aspeed $ okind $ bkind b
        curSpeed = fromMaybe innateSpeed (bspeed b)
        newSpeed = speedAdd curSpeed delta
    in assert (newSpeed >= speedZero `blame` (aid, curSpeed, delta)) $
       if curSpeed == innateSpeed
       then b {bspeed = Nothing}
       else b {bspeed = Just newSpeed}

dominateAtomic :: MonadAction m => FactionId -> FactionId -> ActorId -> m ()
dominateAtomic fromFid toFid target = do
  tm <- getsState (getActorBody target)
  assert (fromFid == bfaction tm `blame` (fromFid, tm, toFid)) $
    modifyState $ updateActorBody target $ \b -> b {bfaction = toFid}

-- TODO: perhaps assert that the inventory of the actor is empty
-- or at least that the items belong to litem.
spawnAtomic :: MonadAction m => ActorId -> Actor -> m ()
spawnAtomic aid body = do
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
killAtomic :: MonadAction m => ActorId -> Actor -> m ()
killAtomic aid body = do
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
createItemAtomic :: MonadAction m
                 => LevelId -> ItemId -> Item -> Int -> Container -> m ()
createItemAtomic lid iid item k c = assert (k > 0) $ do
  -- The item may or may not be already present in the dungeon.
  let f item1 item2 = assert (item1 == item2) item1
  modifyState $ updateItemD $ EM.insertWith f iid item
  case c of
    CFloor pos -> insertItemFloor lid iid k pos
    CActor aid -> insertItemActor iid k aid

insertItemFloor :: MonadAction m
                => LevelId -> ItemId -> Int -> Point -> m ()
insertItemFloor lid iid k pos =
  let bag = EM.singleton iid k
      mergeBag = EM.insertWith (EM.unionWith (+)) pos bag
  in updateLevel lid $ updateFloor mergeBag

insertItemActor :: MonadAction m
                => ItemId -> Int -> ActorId -> m ()
insertItemActor iid k aid = do
  item <- getsState $ getItemBody iid
  let l = if jsymbol item == '$' then Just $ InvChar '$' else Nothing
      bag = EM.singleton iid k
  body <- getsState $ getActorBody aid
  case assignLetter iid l body of
    Nothing -> insertItemFloor (blid body) iid k (bpos body)
    Just l2 -> do
      let upd = EM.unionWith (+) bag
      modifyState $ updateActorD
        $ EM.adjust (\b -> b {bbag = upd (bbag b)}) aid
      modifyState $ updateActorD
        $ EM.adjust (\b -> b {binv = EM.insert l2 iid (binv b)}) aid
      modifyState $ updateActorBody aid $ \b ->
        b {bletter = max l2 (bletter b)}

-- | Destroy some copies (possibly not all) of an item.
destroyItemAtomic :: MonadAction m
                  => LevelId -> ItemId -> Item -> Int -> Container -> m ()
destroyItemAtomic lid iid _item k c = assert (k > 0) $ do
  -- Do not remove the item from @sitemD@ nor from @sitemRev@,
  -- This is behaviourally equivalent.
  case c of
    CFloor pos -> deleteItemFloor lid iid k pos
    CActor aid -> deleteItemActor iid k aid
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

moveItemAtomic :: MonadAction m
               => LevelId -> ItemId -> Int -> Container -> Container -> m ()
moveItemAtomic lid iid k c1 c2 = assert (k > 0) $ do
  case c1 of
    CFloor pos -> deleteItemFloor lid iid k pos
    CActor aid -> deleteItemActor iid k aid
  case c2 of
    CFloor pos -> insertItemFloor lid iid k pos
    CActor aid -> insertItemActor iid k aid

waitAtomic :: MonadAction m => ActorId -> Time -> Time -> m ()
waitAtomic aid _fromWait toWait =
  modifyState $ updateActorBody aid $ \b -> b {bwait = toWait}

changeTileAtomic :: MonadAction m
                 => Point -> LevelId -> Kind.Id TileKind -> Kind.Id TileKind
                 -> m ()
changeTileAtomic p lid _fromTile toTile =
  let adj = (Kind.// [(p, toTile)])
  in updateLevel lid $ updateTile adj

moveActorAtomic :: MonadAction m => ActorId -> Point -> Point -> m ()
moveActorAtomic aid _fromP toP =
  modifyState $ updateActorBody aid $ \b -> b {bpos = toP}

displaceActorAtomic :: MonadAction m => ActorId -> ActorId -> m ()
displaceActorAtomic source target = do
  spos <- getsState $ bpos . getActorBody source
  tpos <- getsState $ bpos . getActorBody target
  modifyState $ updateActorBody source $ \ b -> b {bpos = tpos}
  modifyState $ updateActorBody target $ \ b -> b {bpos = spos}

alterSecretAtomic :: MonadAction m => LevelId -> DiffEM Point Time -> m ()
alterSecretAtomic lid diffL =
  updateLevel lid $ updateSecret $ applyDiffEM diffL

alterSmellAtomic :: MonadAction m => LevelId -> DiffEM Point Time -> m ()
alterSmellAtomic lid diffL =
  updateLevel lid $ updateSmell $ applyDiffEM diffL

alterPathAtomic :: MonadAction m
                => ActorId -> Maybe [Vector] -> Maybe [Vector] -> m ()
alterPathAtomic aid _fromPath toPath =
  modifyState $ updateActorBody aid $ \b -> b {bpath = toPath}

colorActorAtomic :: MonadAction m
                 => ActorId -> Maybe Color.Color -> Maybe Color.Color -> m ()
colorActorAtomic aid _fromCol toCol =
  modifyState $ updateActorBody aid $ \b -> b {bcolor = toCol}

factionQuitAtomic :: MonadAction m
                  => FactionId -> Maybe (Bool, Status) -> Maybe (Bool, Status)
                  -> m ()
factionQuitAtomic fid _fromSt toSt = do
  let adj fac = fac {gquit = toSt}
  modifyState $ updateFaction $ EM.adjust adj fid

leaderAtomic :: MonadAction m
             => FactionId -> (Maybe ActorId) -> (Maybe ActorId) -> m ()
leaderAtomic fid source target = assert (source /= target) $ do
  let adj fac = fac {gleader = target}
  modifyState $ updateFaction $ EM.adjust adj fid
