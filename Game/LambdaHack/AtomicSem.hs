{-# LANGUAGE OverloadedStrings #-}
-- | Semantics of atomic commands shared by client and server.
-- See https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture.
module Game.LambdaHack.AtomicSem
  ( cmdAtomicSem
  , PosAtomic(..), posCmdAtomic, posSfxAtomic  -- for debug, defined here
  ) where

import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.List
import Data.Maybe

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.AtomicCmd
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.TileKind as TileKind
import Game.LambdaHack.Faction
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Misc
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

cmdAtomicSem :: MonadAction m => CmdAtomic -> m ()
cmdAtomicSem cmd = case cmd of
  CreateActorA aid body ais -> createActorA aid body ais
  DestroyActorA aid body ais -> destroyActorA aid body ais
  CreateItemA iid item k c -> createItemA iid item k c
  DestroyItemA iid item k c -> destroyItemA iid item k c
  SpotActorA aid body ais -> createActorA aid body ais
  LoseActorA aid body ais -> destroyActorA aid body ais
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
  SpotTileA lid ts -> spotTileA lid ts
  LoseTileA lid ts -> loseTileA lid ts
  AlterSecretA lid diffL -> alterSecretA lid diffL
  AlterSmellA lid p fromSm toSm -> alterSmellA lid p fromSm toSm
  SpotSmellA lid sms -> spotSmellA lid sms
  LoseSmellA lid sms -> loseSmellA lid sms
  AgeLevelA lid t -> ageLevelA lid t
  AgeGameA t -> ageGameA t
  DiscoverA{} -> return ()  -- Server keeps all atomic comands so the semantics
  CoverA{} -> return ()     -- of inverses has to be reasonably inverse.
  PerceptionA _ outPA inPA ->
    assert (not (EM.null outPA && EM.null inPA)) $ return ()
  RestartA fid sdisco sfper s -> restartA fid sdisco sfper s
  RestartServerA s -> restartServerA s
  ResumeA{} -> return ()
  ResumeServerA s -> resumeServerA s
  SaveExitA -> return ()
  SaveBkpA -> return ()

-- | Creates an actor. Note: after this command, usually a new leader
-- for the party should be elected (in case this actor is the only one alive).
createActorA :: MonadAction m => ActorId -> Actor -> [(ItemId, Item)] -> m ()
createActorA aid body ais = do
  -- Add actor to @sactorD@.
  let f Nothing = Just body
      f (Just b) = assert `failure` (aid, body, b)
  modifyState $ updateActorD $ EM.alter f aid
  -- Add actor to @sprio@.
  let g Nothing = Just [aid]
      g (Just l) = assert (not (aid `elem` l) `blame` (aid, body, l))
                   $ Just $ aid : l
  updateLevel (blid body) $ updatePrio $ EM.alter g (btime body)
  -- Actor's items may or may not be already present in @sitemD@,
  -- regardless if they are already present otherwise in the dungeon.
  -- We re-add them all to save time determining which really need it.
  forM_ ais $ \(iid, item) -> do
    let h item1 item2 =
          assert (item1 == item2 `blame` (aid, body, iid, item1, item2)) item1
    modifyState $ updateItemD $ EM.insertWith h iid item

-- | Kills an actor. Note: after this command, usually a new leader
-- for the party should be elected.
destroyActorA :: MonadAction m => ActorId -> Actor -> [(ItemId, Item)] -> m ()
destroyActorA aid body ais = do
  -- Assert that actor's items belong to @sitemD@. Do not remove those
  -- that do not appear anywhere else, for simplicity and speed.
  itemD <- getsState sitemD
  let match (iid, item) = itemD EM.! iid == item
  assert (allB match ais `blame` (aid, body, ais, itemD)) skip
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
  assert (fromP == bpos b `blame` (aid, fromP, toP, bpos b, b)) skip
  modifyState $ updateActorBody aid $ \body -> body {bpos = toP}

waitActorA :: MonadAction m => ActorId -> Time -> Time -> m ()
waitActorA aid fromWait toWait = assert (fromWait /= toWait) $ do
  b <- getsState $ getActorBody aid
  assert (fromWait == bwait b `blame` (aid, fromWait, toWait, bwait b, b)) skip
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
ageActorA aid t = assert (t /= timeZero) $ do
  body <- getsState $ getActorBody aid
  ais <- getsState $ getActorItem aid
  destroyActorA aid body ais
  createActorA aid body {btime = timeAdd (btime body) t} ais

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

-- | Alter an attribute (actually, the only, the defining attribute)
-- of a visible tile. This is similar to e.g., @PathActorA@.
alterTileA :: MonadAction m
           => LevelId -> Point -> Kind.Id TileKind -> Kind.Id TileKind
           -> m ()
alterTileA lid p fromTile toTile = assert (fromTile /= toTile) $ do
  Kind.COps{cotile} <- getsState scops
  let adj ts = assert (ts Kind.! p == fromTile) $ ts Kind.// [(p, toTile)]
  updateLevel lid $ updateTile adj
  case (Tile.isExplorable cotile fromTile, Tile.isExplorable cotile toTile) of
    (False, True) -> updateLevel lid $ \lvl -> lvl {lseen = lseen lvl + 1}
    (True, False) -> updateLevel lid $ \lvl -> lvl {lseen = lseen lvl - 1}
    _ -> return ()

-- Notice a previously invisible tiles. This is similar to @SpotActorA@,
-- but done in bulk, because it often involves dozens of tiles pers move.
-- We don't check that the tiles at the positions in question are unknown
-- to save computation, especially for clients that remember tiles
-- at previously seen positions. Similarly, when updating the @lseen@
-- field we don't assume the tiles were unknown previously.
spotTileA :: MonadAction m => LevelId -> [(Point, Kind.Id TileKind)] -> m ()
spotTileA lid ts = assert (not $ null ts) $ do
  Kind.COps{cotile} <- getsState scops
  tileM <- getsLevel lid ltile
  let adj tileMap = tileMap Kind.// ts
  updateLevel lid $ updateTile adj
  let f (p, t2) = do
        let t1 = tileM Kind.! p
        case (Tile.isExplorable cotile t1, Tile.isExplorable cotile t2) of
          (False, True) -> updateLevel lid $ \lvl -> lvl {lseen = lseen lvl+1}
          (True, False) -> updateLevel lid $ \lvl -> lvl {lseen = lseen lvl-1}
          _ -> return ()
  mapM_ f ts

-- Stop noticing a previously invisible tiles. Unlike @spotTileA@, it verifies
-- the state of the tiles before changing them.
loseTileA :: MonadAction m => LevelId -> [(Point, Kind.Id TileKind)] -> m ()
loseTileA lid ts = assert (not $ null ts) $ do
  Kind.COps{cotile=cotile@Kind.Ops{ouniqGroup}} <- getsState scops
  let unknownId = ouniqGroup "unknown space"
      matches _ [] = True
      matches tileMap ((p, ov) : rest) =
        tileMap Kind.! p == ov && matches tileMap rest
      tu = map (\(p, _) -> (p, unknownId)) ts
      adj tileMap = assert (matches tileMap ts) $ tileMap Kind.// tu
  updateLevel lid $ updateTile adj
  let f (_, t1) =
        case Tile.isExplorable cotile t1 of
          True -> updateLevel lid $ \lvl -> lvl {lseen = lseen lvl - 1}
          _ -> return ()
  mapM_ f ts

alterSecretA :: MonadAction m => LevelId -> DiffEM Point Time -> m ()
alterSecretA lid diffL = updateLevel lid $ updateSecret $ applyDiffEM diffL

alterSmellA :: MonadAction m
            => LevelId -> Point -> Maybe Time -> Maybe Time -> m ()
alterSmellA lid p fromSm toSm = do
  let alt sm = assert (sm == fromSm `blame` (lid, p, fromSm, toSm, sm)) toSm
  updateLevel lid $ updateSmell $ EM.alter alt p

spotSmellA :: MonadAction m => LevelId -> [(Point, Time)] -> m ()
spotSmellA lid sms = assert (not $ null sms) $ do
  let alt sm Nothing = Just sm
      alt sm (Just oldSm) = assert `failure` (lid, sms, sm, oldSm)
      f (p, sm) = EM.alter (alt sm) p
      upd m = foldr f m sms
  updateLevel lid $ updateSmell upd

loseSmellA :: MonadAction m => LevelId -> [(Point, Time)] -> m ()
loseSmellA lid sms = assert (not $ null sms) $ do
  let alt sm Nothing = assert `failure` (lid, sms, sm)
      alt sm (Just oldSm) =
        assert (sm == oldSm `blame` (lid, sms, sm, oldSm)) Nothing
      f (p, sm) = EM.alter (alt sm) p
      upd m = foldr f m sms
  updateLevel lid $ updateSmell upd

ageLevelA :: MonadAction m => LevelId -> Time -> m ()
ageLevelA lid delta = assert (delta /= timeZero) $
  updateLevel lid $ \lvl -> lvl {ltime = timeAdd (ltime lvl) delta}

ageGameA :: MonadAction m => Time -> m ()
ageGameA delta = assert (delta /= timeZero) $
  modifyState $ updateTime $ timeAdd delta

restartA :: MonadAction m
         => FactionId -> Discovery -> FactionPers -> State -> m ()
restartA _ _ _ s = putState s

restartServerA :: MonadAction m =>  State -> m ()
restartServerA s = putState s

resumeServerA :: MonadAction m =>  State -> m ()
resumeServerA s = putState s

-- All functions below that take an atomic action are executed
-- in the state just before the action is executed.

data PosAtomic =
    PosLevel LevelId [Point]  -- ^ whomever sees all the positions, notices
  | PosSmell LevelId [Point]  -- ^ whomever smells all the positions, notices
  | PosOnly FactionId         -- ^ only the faction notices
  | PosAndSer FactionId       -- ^ faction and server notices
  | PosServer                 -- ^ only the server notices
  | PosAll                    -- ^ everybody notices
  | PosNone                   -- ^ never broadcasted, but sent manually
  deriving (Show, Eq)

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
posCmdAtomic :: MonadActionRO m => CmdAtomic -> m PosAtomic
posCmdAtomic cmd = case cmd of
  CreateActorA _ body _ -> return $ PosLevel (blid body) [bpos body]
  DestroyActorA _ body _ ->
    -- The faction of the actor sometimes does not see his death
    -- (if none of the other actors is observing it).
    return $ PosLevel (blid body) [bpos body]
  CreateItemA _ _ _ c -> singleContainer c
  DestroyItemA _ _ _ c -> singleContainer c
  SpotActorA _ body _ -> return $ PosLevel (blid body) [bpos body]
  LoseActorA _ body _ -> return $ PosLevel (blid body) [bpos body]
  SpotItemA _ _ _ c -> singleContainer c
  LoseItemA _ _ _ c -> singleContainer c
  MoveActorA aid fromP toP -> do
    (lid, _) <- posOfAid aid
    return $ PosLevel lid [fromP, toP]
  WaitActorA aid _ _ -> singleAid aid
  DisplaceActorA source target -> do
    (slid, sp) <- posOfAid source
    (tlid, tp) <- posOfAid target
    return $ assert (slid == tlid) $ PosLevel slid [sp, tp]
  MoveItemA _ _ c1 c2 -> do  -- works even if moved between positions
    (lid1, p1) <- posOfContainer c1
    (lid2, p2) <- posOfContainer c2
    return $ assert (lid1 == lid2) $ PosLevel lid1 [p1, p2]
  AgeActorA aid _ -> singleAid aid
  HealActorA aid _ -> singleAid aid
  HasteActorA aid _ -> singleAid aid
  DominateActorA target _ _ -> singleAid target
  PathActorA aid _ _ -> singleAid aid
  ColorActorA aid _ _ -> singleAid aid
  QuitFactionA _ _ _ -> return PosAll
  LeadFactionA fid _ _ -> return $ PosAndSer fid
  AlterTileA lid p _ _ -> return $ PosLevel lid [p]
  SpotTileA lid ts -> do
    let ps = map fst ts
    return $ PosLevel lid ps
  LoseTileA lid ts -> do
    let ps = map fst ts
    return $ PosLevel lid ps
  AlterSecretA _ _ -> return PosNone
  AlterSmellA lid p _ _ -> return $ PosSmell lid [p]
  SpotSmellA lid sms -> do
    let ps = map fst sms
    return $ PosSmell lid ps
  LoseSmellA lid sms -> do
    let ps = map fst sms
    return $ PosSmell lid ps
  AgeLevelA lid _ ->  return $ PosLevel lid []
  AgeGameA _ ->  return PosAll
  DiscoverA lid p _ _ -> return $ PosLevel lid [p]
  CoverA lid p _ _ -> return $ PosLevel lid [p]
  PerceptionA _ _ _ -> return PosNone
  RestartA fid _ _ _ -> return $ PosOnly fid
  RestartServerA _ -> return PosServer
  ResumeA fid _ -> return $ PosOnly fid
  ResumeServerA _ -> return PosServer
  SaveExitA -> return $ PosAll
  SaveBkpA -> return $ PosAll

posSfxAtomic :: MonadActionRO m => SfxAtomic -> m PosAtomic
posSfxAtomic cmd = case cmd of
  StrikeD source target _ _ -> do
    (slid, sp) <- posOfAid source
    (tlid, tp) <- posOfAid target
    return $ assert (slid == tlid) $ PosLevel slid [sp, tp]
  RecoilD source target _ _ -> do
    (slid, sp) <- posOfAid source
    (tlid, tp) <- posOfAid target
    return $ assert (slid == tlid) $ PosLevel slid [sp, tp]
  ProjectD aid _ -> singleAid aid
  CatchD aid _ -> singleAid aid
  ActivateD aid _ -> singleAid aid
  CheckD aid _ -> singleAid aid
  TriggerD aid p _ _ -> do
    (lid, pa) <- posOfAid aid
    return $ PosLevel lid [pa, p]
  ShunD aid p _ _ -> do
    (lid, pa) <- posOfAid aid
    return $ PosLevel lid [pa, p]
  EffectD aid _ -> singleAid aid
  FailureD fid _ -> return $ PosOnly fid  -- failures are secret
  BroadcastD _ -> return $ PosAll
  DisplayPushD fid -> return $ PosOnly fid
  DisplayDelayD fid -> return $ PosOnly fid
  FlushFramesD fid -> return $ PosOnly fid
  FadeoutD fid _ -> return $ PosOnly fid
  FadeinD fid _ -> return $ PosOnly fid

posOfAid :: MonadActionRO m => ActorId -> m (LevelId, Point)
posOfAid aid = do
  b <- getsState $ getActorBody aid
  return (blid b, bpos b)

posOfContainer :: MonadActionRO m => Container -> m (LevelId, Point)
posOfContainer (CFloor lid p) = return (lid, p)
posOfContainer (CActor aid _) = posOfAid aid

singleAid :: MonadActionRO m => ActorId -> m PosAtomic
singleAid aid = do
  (lid, p) <- posOfAid aid
  return $ PosLevel lid [p]

singleContainer :: MonadActionRO m => Container -> m PosAtomic
singleContainer c = do
  (lid, p) <- posOfContainer c
  return $ PosLevel lid [p]
