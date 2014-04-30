-- | Semantics of atomic commands shared by client and server.
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Atomic.HandleAtomicWrite
  ( handleCmdAtomic
  ) where

import Control.Arrow (second)
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.List
import Data.Maybe

import Game.LambdaHack.Atomic.CmdAtomic
import Game.LambdaHack.Atomic.MonadStateWrite
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ModeKind as ModeKind
import Game.LambdaHack.Content.TileKind as TileKind

-- | The game-state semantics of atomic game commands.
-- Special effects (@SfxAtomic@) don't modify state.
handleCmdAtomic :: MonadStateWrite m => CmdAtomic -> m ()
handleCmdAtomic cmd = case cmd of
  UpdAtomic upd -> handleUpdAtomic upd
  SfxAtomic _ -> return ()

handleUpdAtomic :: MonadStateWrite m => UpdAtomic -> m ()
handleUpdAtomic cmd = case cmd of
  UpdCreateActor aid body ais -> updCreateActor aid body ais
  UpdDestroyActor aid body ais -> updDestroyActor aid body ais
  UpdCreateItem iid item k c -> updCreateItem iid item k c
  UpdDestroyItem iid item k c -> updDestroyItem iid item k c
  UpdSpotActor aid body ais -> updCreateActor aid body ais
  UpdLoseActor aid body ais -> updDestroyActor aid body ais
  UpdSpotItem iid item k c -> updCreateItem iid item k c
  UpdLoseItem iid item k c -> updDestroyItem iid item k c
  UpdMoveActor aid fromP toP -> updMoveActor aid fromP toP
  UpdWaitActor aid toWait -> updWaitActor aid toWait
  UpdDisplaceActor source target -> updDisplaceActor source target
  UpdMoveItem iid k aid c1 c2 -> updMoveItem iid k aid c1 c2
  UpdAgeActor aid t -> updAgeActor aid t
  UpdHealActor aid n -> updHealActor aid n
  UpdCalmActor aid n -> updCalmActor aid n
  UpdHasteActor aid delta -> updHasteActor aid delta
  UpdOldFidActor aid fromFid toFid -> updOldFidActor aid fromFid toFid
  UpdTrajectoryActor aid fromT toT -> updTrajectoryActor aid fromT toT
  UpdColorActor aid fromCol toCol -> updColorActor aid fromCol toCol
  UpdQuitFaction fid mbody fromSt toSt -> updQuitFaction fid mbody fromSt toSt
  UpdLeadFaction fid source target -> updLeadFaction fid source target
  UpdDiplFaction fid1 fid2 fromDipl toDipl ->
    updDiplFaction fid1 fid2 fromDipl toDipl
  UpdAutoFaction fid st -> updAutoFaction fid st
  UpdRecordKill aid k -> updRecordKill aid k
  UpdAlterTile lid p fromTile toTile -> updAlterTile lid p fromTile toTile
  UpdAlterClear lid delta -> updAlterClear lid delta
  UpdSearchTile _ _ fromTile toTile ->
    assert (fromTile /= toTile) $ return ()  -- only for clients
  UpdSpotTile lid ts -> updSpotTile lid ts
  UpdLoseTile lid ts -> updLoseTile lid ts
  UpdAlterSmell lid p fromSm toSm -> updAlterSmell lid p fromSm toSm
  UpdSpotSmell lid sms -> updSpotSmell lid sms
  UpdLoseSmell lid sms -> updLoseSmell lid sms
  UpdAgeGame t lids -> updAgeGame t lids
  UpdDiscover{} -> return ()      -- Server keeps all atomic comands
  UpdCover{} -> return ()         -- so the semantics of inverses
  UpdDiscoverSeed{} -> return ()  -- has to be reasonably inverse.
  UpdCoverSeed{} -> return ()
  UpdPerception _ outPer inPer ->
    assert (not (nullPer outPer && nullPer inPer)) skip
  UpdRestart fid sdisco sfper s _ _ -> updRestart fid sdisco sfper s
  UpdRestartServer s -> updRestartServer s
  UpdResume{} -> return ()
  UpdResumeServer s -> updResumeServer s
  UpdKillExit{} -> return ()
  UpdSaveBkp -> return ()
  UpdMsgAll{} -> return ()
  UpdRecordHistory{} -> return ()

-- | Creates an actor. Note: after this command, usually a new leader
-- for the party should be elected (in case this actor is the only one alive).
updCreateActor :: MonadStateWrite m
               => ActorId -> Actor -> [(ItemId, Item)] -> m ()
updCreateActor aid body ais = do
  -- Add actor to @sactorD@.
  let f Nothing = Just body
      f (Just b) = assert `failure` "actor already added"
                          `twith` (aid, body, b)
  modifyState $ updateActorD $ EM.alter f aid
  -- Add actor to @sprio@.
  let g Nothing = Just [aid]
      g (Just l) = assert (aid `notElem` l `blame` "actor already added"
                                           `twith` (aid, body, l))
                   $ Just $ aid : l
  updateLevel (blid body) $ updatePrio $ EM.alter g (btime body)
  -- Actor's items may or may not be already present in @sitemD@,
  -- regardless if they are already present otherwise in the dungeon.
  -- We re-add them all to save time determining which really need it.
  forM_ ais $ \(iid, item) -> do
    let h item1 item2 =
          assert (item1 == item2 `blame` "inconsistent created actor items"
                                 `twith` (aid, body, iid, item1, item2)) item1
    modifyState $ updateItemD $ EM.insertWith h iid item

-- | Kills an actor.
updDestroyActor :: MonadStateWrite m
                => ActorId -> Actor -> [(ItemId, Item)] -> m ()
updDestroyActor aid body ais = do
  -- If a leader dies, a new leader should be elected on the server
  -- before this command is executed.
  -- TODO: check this only on the server (e.g., not in LoseActor):
  -- fact <- getsState $ (EM.! bfid body) . sfactionD
  -- assert (Just aid /= gleader fact `blame` (aid, body, fact)) skip
  -- Assert that actor's items belong to @sitemD@. Do not remove those
  -- that do not appear anywhere else, for simplicity and speed.
  itemD <- getsState sitemD
  let match (iid, item) = itemD EM.! iid == item
  assert (allB match ais `blame` "destroyed actor items not found"
                         `twith` (aid, body, ais, itemD)) skip
  -- Remove actor from @sactorD@.
  let f Nothing = assert `failure` "actor already removed" `twith` (aid, body)
      f (Just b) = assert (b == body `blame` "inconsisted destroyed actor body"
                                     `twith` (aid, body, b)) Nothing
  modifyState $ updateActorD $ EM.alter f aid
  -- Remove actor from @sprio@.
  let g Nothing = assert `failure` "actor already removed" `twith` (aid, body)
      g (Just l) = assert (aid `elem` l `blame` "actor already removed"
                                        `twith` (aid, body, l))
                   $ let l2 = delete aid l
                     in if null l2 then Nothing else Just l2
  updateLevel (blid body) $ updatePrio $ EM.alter g (btime body)

-- | Create a few copies of an item that is already registered for the dungeon
-- (in @sitemRev@ field of @StateServer@).
updCreateItem :: MonadStateWrite m => ItemId -> Item -> Int -> Container -> m ()
updCreateItem iid item k c = assert (k > 0) $ do
  -- The item may or may not be already present in @sitemD@,
  -- regardless if it's actually present in the dungeon.
  let f item1 item2 = assert (item1 == item2
                              `blame` "inconsistent created item"
                              `twith` (iid, item, k, c)) item1
  modifyState $ updateItemD $ EM.insertWith f iid item
  insertItemContainer iid k c

-- | Destroy some copies (possibly not all) of an item.
updDestroyItem :: MonadStateWrite m
               => ItemId -> Item -> Int -> Container -> m ()
updDestroyItem iid item k c = assert (k > 0) $ do
  -- Do not remove the item from @sitemD@ nor from @sitemRev@,
  -- It's incredibly costly and not noticeable for the player.
  -- However, assert the item is registered in @sitemD@.
  itemD <- getsState sitemD
  assert (iid `EM.lookup` itemD == Just item `blame` "item already removed"
                                             `twith` (iid, item, itemD)) skip
  deleteItemContainer iid k c

updMoveActor :: MonadStateWrite m => ActorId -> Point -> Point -> m ()
updMoveActor aid fromP toP = assert (fromP /= toP) $ do
  b <- getsState $ getActorBody aid
  assert (fromP == bpos b `blame` "unexpected moved actor position"
                          `twith` (aid, fromP, toP, bpos b, b)) skip
  updateActor aid $ \body -> body {bpos = toP, boldpos = fromP}

updWaitActor :: MonadStateWrite m => ActorId -> Bool -> m ()
updWaitActor aid toWait = do
  b <- getsState $ getActorBody aid
  assert (toWait /= bwait b `blame` "unexpected waited actor time"
                            `twith` (aid, toWait, bwait b, b)) skip
  updateActor aid $ \body -> body {bwait = toWait}

updDisplaceActor :: MonadStateWrite m => ActorId -> ActorId -> m ()
updDisplaceActor source target = assert (source /= target) $ do
  spos <- getsState $ bpos . getActorBody source
  tpos <- getsState $ bpos . getActorBody target
  updateActor source $ \b -> b {bpos = tpos, boldpos = spos}
  updateActor target $ \b -> b {bpos = spos, boldpos = tpos}

updMoveItem :: MonadStateWrite m
            => ItemId -> Int -> ActorId -> CStore -> CStore -> m ()
updMoveItem iid k aid c1 c2 = assert (k > 0 && c1 /= c2) $ do
  deleteItemActor iid k aid c1
  insertItemActor iid k aid c2

-- TODO: optimize (a single call to updatePrio is enough)
updAgeActor :: MonadStateWrite m => ActorId -> Delta Time -> m ()
updAgeActor aid delta = assert (delta /= Delta timeZero) $ do
  body <- getsState $ getActorBody aid
  ais <- getsState $ getCarriedAssocs body
  updDestroyActor aid body ais
  let newBody = body {btime = timeShift (btime body) delta}
  updCreateActor aid newBody ais

updHealActor :: MonadStateWrite m => ActorId -> Int -> m ()
updHealActor aid n = assert (n /= 0) $
  updateActor aid $ \b -> b {bhp = bhp b + n}

updCalmActor :: MonadStateWrite m => ActorId -> Int -> m ()
updCalmActor aid n =
  updateActor aid $ \b ->
    b { bcalm = max 0 $ bcalm b + n
      , bcalmDelta = n }  -- records original delta, for "hears something"

updHasteActor :: MonadStateWrite m => ActorId -> Speed -> m ()
updHasteActor aid delta = assert (delta /= speedZero) $ do
  updateActor aid $ \b ->
    let newSpeed = speedAdd (bspeed b) delta
    in assert (newSpeed >= speedZero
               `blame` "actor slowed below zero"
               `twith` (aid, delta, b, newSpeed))
       $ b {bspeed = newSpeed}

updOldFidActor :: MonadStateWrite m => ActorId -> FactionId -> FactionId -> m ()
updOldFidActor aid fromFid toFid = assert (fromFid /= toFid) $ do
  updateActor aid $ \b ->
    assert (boldfid b == fromFid `blame` (aid, fromFid, toFid, b))
    $ b {boldfid = toFid}

updTrajectoryActor :: MonadStateWrite m
                   => ActorId -> Maybe [Vector] -> Maybe [Vector] -> m ()
updTrajectoryActor aid fromT toT = assert (fromT /= toT) $ do
  body <- getsState $ getActorBody aid
  assert (fromT == btrajectory body `blame` "unexpected actor trajectory"
                                    `twith` (aid, fromT, toT, body)) skip
  updateActor aid $ \b -> b {btrajectory = toT}

updColorActor :: MonadStateWrite m
              => ActorId -> Color.Color -> Color.Color -> m ()
updColorActor aid fromCol toCol = assert (fromCol /= toCol) $ do
  body <- getsState $ getActorBody aid
  assert (fromCol == bcolor body `blame` "unexpected actor color"
                                 `twith` (aid, fromCol, toCol, body)) skip
  updateActor aid $ \b -> b {bcolor = toCol}

updQuitFaction :: MonadStateWrite m
               => FactionId -> Maybe Actor -> Maybe Status -> Maybe Status
               -> m ()
updQuitFaction fid mbody fromSt toSt = do
  assert (fromSt /= toSt `blame` (fid, mbody, fromSt, toSt)) skip
  assert (maybe True ((fid ==) . bfid) mbody) skip
  fact <- getsState $ (EM.! fid) . sfactionD
  assert (fromSt == gquit fact `blame` "unexpected actor quit status"
                               `twith` (fid, fromSt, toSt, fact)) skip
  let adj fa = fa {gquit = toSt}
  updateFaction fid adj

-- The previous leader is assumed to be alive.
updLeadFaction :: MonadStateWrite m
             => FactionId -> Maybe ActorId -> Maybe ActorId -> m ()
updLeadFaction fid source target = assert (source /= target) $ do
  fact <- getsState $ (EM.! fid) . sfactionD
  assert (playerLeader $ gplayer fact) skip  -- @PosNone@ ensure this
  mtb <- getsState $ \s -> fmap (flip getActorBody s) target
  assert (maybe True (not . bproj) mtb
          `blame` (fid, source, target, mtb, fact)) skip
  assert (source == gleader fact
          `blame` "unexpected actor leader"
          `twith` (fid, source, target, mtb, fact)) skip
  let adj fa = fa {gleader = target}
  updateFaction fid adj

updDiplFaction :: MonadStateWrite m
             => FactionId -> FactionId -> Diplomacy -> Diplomacy -> m ()
updDiplFaction fid1 fid2 fromDipl toDipl =
  assert (fid1 /= fid2 && fromDipl /= toDipl) $ do
    fact1 <- getsState $ (EM.! fid1) . sfactionD
    fact2 <- getsState $ (EM.! fid2) . sfactionD
    assert (fromDipl == EM.findWithDefault Unknown fid2 (gdipl fact1)
            && fromDipl == EM.findWithDefault Unknown fid1 (gdipl fact2)
            `blame` "unexpected actor diplomacy status"
            `twith` (fid1, fid2, fromDipl, toDipl, fact1, fact2)) skip
    let adj fid fact = fact {gdipl = EM.insert fid toDipl (gdipl fact)}
    updateFaction fid1 (adj fid2)
    updateFaction fid2 (adj fid1)

updAutoFaction :: MonadStateWrite m => FactionId -> Bool -> m ()
updAutoFaction fid st = do
  let adj fact =
        let player = gplayer fact
        in assert (playerAI player == not st)
           $ fact {gplayer = player {playerAI = st}}
  updateFaction fid adj

-- | Record a given number (usually just 1, or -1 for undo) of actor kills
-- for score calculation.
updRecordKill :: MonadStateWrite m => ActorId -> Int -> m ()
updRecordKill aid k = do
  b <- getsState $ getActorBody aid
  assert (not (bproj b) `blame` (aid, b)) skip
  let alterKind mn = let n = fromMaybe 0 mn + k
                     in if n == 0 then Nothing else Just n
      adjFact fact = fact {gvictims = EM.alter alterKind (bkind b)
                                      $ gvictims fact}
  updateFaction (bfid b) adjFact

-- | Alter an attribute (actually, the only, the defining attribute)
-- of a visible tile. This is similar to e.g., @TrajectoryActorA@.
updAlterTile :: MonadStateWrite m
             => LevelId -> Point -> Kind.Id TileKind -> Kind.Id TileKind
             -> m ()
updAlterTile lid p fromTile toTile = assert (fromTile /= toTile) $ do
  Kind.COps{cotile} <- getsState scops
  lvl <- getLevel lid
  let freshClientTile = hideTile cotile lvl p
  -- The second alternative can happen if, e.g., a client remembers,
  -- but does not see the tile (so does not notice the SearchTile action),
  -- and it suddenly changes into another tile,
  -- which at the same time becomes visible (e.g., an open door).
  -- See 'AtomicSemCli' for how this is reported to the client.
  let adj ts = assert (ts PointArray.! p == fromTile
                       || ts PointArray.! p == freshClientTile
                       `blame` "unexpected altered tile kind"
                       `twith` (lid, p, fromTile, toTile, ts PointArray.! p))
               $ ts PointArray.// [(p, toTile)]
  updateLevel lid $ updateTile adj
  unless (isSecretPos lvl p) $
    case (Tile.isExplorable cotile fromTile, Tile.isExplorable cotile toTile) of
      (False, True) -> updateLevel lid $ \lvl2 -> lvl2 {lseen = lseen lvl + 1}
      (True, False) -> updateLevel lid $ \lvl2 -> lvl2 {lseen = lseen lvl - 1}
      _ -> return ()


updAlterClear :: MonadStateWrite m => LevelId -> Int -> m ()
updAlterClear lid delta = assert (delta /= 0) $
  updateLevel lid $ \lvl -> lvl {lclear = lclear lvl + delta}

-- Notice previously invisible tiles. This is similar to @SpotActorA@,
-- but done in bulk, because it often involves dozens of tiles pers move.
-- We don't check that the tiles at the positions in question are unknown
-- to save computation, especially for clients that remember tiles
-- at previously seen positions. Similarly, when updating the @lseen@
-- field we don't assume the tiles were unknown previously.
updSpotTile :: MonadStateWrite m
            => LevelId -> [(Point, Kind.Id TileKind)] -> m ()
updSpotTile lid ts = assert (not $ null ts) $ do
  Kind.COps{cotile} <- getsState scops
  lvl1@Level{ltile} <- getLevel lid
  let adj tileMap = tileMap PointArray.// ts
  updateLevel lid $ updateTile adj
  let f (p, t2) = unless (isSecretPos lvl1 p) $ do
        let t1 = ltile PointArray.! p
        case (Tile.isExplorable cotile t1, Tile.isExplorable cotile t2) of
          (False, True) -> updateLevel lid $ \lvl -> lvl {lseen = lseen lvl+1}
          (True, False) -> updateLevel lid $ \lvl -> lvl {lseen = lseen lvl-1}
          _ -> return ()
  mapM_ f ts

-- Stop noticing previously visible tiles. Unlike @spotTileA@, it verifies
-- the state of the tiles before changing them.
updLoseTile :: MonadStateWrite m
            => LevelId -> [(Point, Kind.Id TileKind)] -> m ()
updLoseTile lid ts = assert (not $ null ts) $ do
  Kind.COps{cotile=cotile@Kind.Ops{ouniqGroup}} <- getsState scops
  lvl1 <- getLevel lid
  let unknownId = ouniqGroup "unknown space"
      matches _ [] = True
      matches tileMap ((p, ov) : rest) =
        tileMap PointArray.! p == ov && matches tileMap rest
      tu = map (second (const unknownId)) ts
      adj tileMap = assert (matches tileMap ts) $ tileMap PointArray.// tu
  updateLevel lid $ updateTile adj
  let f (p, t1) = unless (isSecretPos lvl1 p) $
        when (Tile.isExplorable cotile t1) $
          updateLevel lid $ \lvl -> lvl {lseen = lseen lvl - 1}
  mapM_ f ts

updAlterSmell :: MonadStateWrite m
            => LevelId -> Point -> Maybe Time -> Maybe Time -> m ()
updAlterSmell lid p fromSm toSm = do
  let alt sm = assert (sm == fromSm `blame` "unexpected tile smell"
                                    `twith` (lid, p, fromSm, toSm, sm)) toSm
  updateLevel lid $ updateSmell $ EM.alter alt p

updSpotSmell :: MonadStateWrite m => LevelId -> [(Point, Time)] -> m ()
updSpotSmell lid sms = assert (not $ null sms) $ do
  let alt sm Nothing = Just sm
      alt sm (Just oldSm) = assert `failure` "smell already added"
                                   `twith` (lid, sms, sm, oldSm)
      f (p, sm) = EM.alter (alt sm) p
      upd m = foldr f m sms
  updateLevel lid $ updateSmell upd

updLoseSmell :: MonadStateWrite m => LevelId -> [(Point, Time)] -> m ()
updLoseSmell lid sms = assert (not $ null sms) $ do
  let alt sm Nothing = assert `failure` "smell already removed"
                              `twith` (lid, sms, sm)
      alt sm (Just oldSm) =
        assert (sm == oldSm `blame` "unexpected lost smell"
                            `twith` (lid, sms, sm, oldSm)) Nothing
      f (p, sm) = EM.alter (alt sm) p
      upd m = foldr f m sms
  updateLevel lid $ updateSmell upd

-- | Age the game.
--
-- TODO: It leaks information that there is activity on various level,
-- even if the faction has no actors there, so show this on UI somewhere,
-- e.g., in the @~@ menu of seen level indicate recent activity.
updAgeGame :: MonadStateWrite m => Delta Time -> [LevelId] -> m ()
updAgeGame delta lids = assert (delta /= Delta timeZero) $ do
  modifyState $ updateTime $ flip timeShift delta
  mapM_ (ageLevel delta) lids

ageLevel :: MonadStateWrite m => Delta Time -> LevelId -> m ()
ageLevel delta lid =
  updateLevel lid $ \lvl -> lvl {ltime = timeShift (ltime lvl) delta}

updRestart :: MonadStateWrite m
           => FactionId -> Discovery -> FactionPers -> State -> m ()
updRestart _ _ _ = putState

updRestartServer :: MonadStateWrite m => State -> m ()
updRestartServer = putState

updResumeServer :: MonadStateWrite m => State -> m ()
updResumeServer = putState
