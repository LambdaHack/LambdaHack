-- | Semantics of atomic commands shared by client and server.
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Atomic.HandleAtomicWrite
  ( handleCmdAtomic
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , updCreateActor, updDestroyActor, updCreateItem, updDestroyItem
  , updMoveActor, updWaitActor, updDisplaceActor, updMoveItem
  , updAgeActor, updRefillHP, updRefillCalm, updFidImpressedActor
  , updTrajectory, updColorActor, updQuitFaction, updLeadFaction
  , updDiplFaction, updTacticFaction, updAutoFaction, updRecordKill
  , updAlterTile, updAlterClear, updLearnSecrets, updSpotTile, updLoseTile
  , updAlterSmell, updSpotSmell, updLoseSmell, updTimeItem, updAgeGame
  , updRestart, updRestartServer, updResumeServer
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Arrow (second)
import qualified Data.EnumMap.Strict as EM
import Data.Int (Int64)

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
import Game.LambdaHack.Content.ItemKind (ItemKind)
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.TileKind (TileKind, unknownId)

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
  UpdCreateItem iid item kit c -> updCreateItem iid item kit c
  UpdDestroyItem iid item kit c -> updDestroyItem iid item kit c
  UpdSpotActor aid body ais -> updCreateActor aid body ais
  UpdLoseActor aid body ais -> updDestroyActor aid body ais
  UpdSpotItem iid item kit c -> updCreateItem iid item kit c
  UpdLoseItem iid item kit c -> updDestroyItem iid item kit c
  UpdMoveActor aid fromP toP -> updMoveActor aid fromP toP
  UpdWaitActor aid toWait -> updWaitActor aid toWait
  UpdDisplaceActor source target -> updDisplaceActor source target
  UpdMoveItem iid k aid c1 c2 -> updMoveItem iid k aid c1 c2
  UpdAgeActor aid t -> updAgeActor aid t
  UpdRefillHP aid n -> updRefillHP aid n
  UpdRefillCalm aid n -> updRefillCalm aid n
  UpdFidImpressedActor aid fromFid toFid ->
    updFidImpressedActor aid fromFid toFid
  UpdTrajectory aid fromT toT -> updTrajectory aid fromT toT
  UpdColorActor aid fromCol toCol -> updColorActor aid fromCol toCol
  UpdQuitFaction fid mbody fromSt toSt -> updQuitFaction fid mbody fromSt toSt
  UpdLeadFaction fid source target -> updLeadFaction fid source target
  UpdDiplFaction fid1 fid2 fromDipl toDipl ->
    updDiplFaction fid1 fid2 fromDipl toDipl
  UpdTacticFaction fid toT fromT -> updTacticFaction fid toT fromT
  UpdAutoFaction fid st -> updAutoFaction fid st
  UpdRecordKill aid ikind k -> updRecordKill aid ikind k
  UpdAlterTile lid p fromTile toTile -> updAlterTile lid p fromTile toTile
  UpdAlterClear lid delta -> updAlterClear lid delta
  UpdSearchTile _ _ fromTile toTile ->
    assert (fromTile /= toTile) $ return ()  -- only for clients
  UpdLearnSecrets aid fromS toS -> updLearnSecrets aid fromS toS
  UpdSpotTile lid ts -> updSpotTile lid ts
  UpdLoseTile lid ts -> updLoseTile lid ts
  UpdAlterSmell lid p fromSm toSm -> updAlterSmell lid p fromSm toSm
  UpdSpotSmell lid sms -> updSpotSmell lid sms
  UpdLoseSmell lid sms -> updLoseSmell lid sms
  UpdTimeItem iid c fromIt toIt -> updTimeItem iid c fromIt toIt
  UpdAgeGame t lids -> updAgeGame t lids
  UpdDiscover{} -> return ()      -- We can't keep dicovered data in State,
  UpdCover{} -> return ()         -- because server saves all atomic commands
  UpdDiscoverKind{} -> return ()  -- to apply their inverses for undo,
  UpdCoverKind{} -> return ()     -- so they would wipe out server knowledge.
  UpdDiscoverSeed{} -> return ()
  UpdCoverSeed{} -> return ()
  UpdPerception _ outPer inPer ->
    assert (not (nullPer outPer && nullPer inPer)) (return ())
  UpdRestart _ _ _ s _ _ -> updRestart s
  UpdRestartServer s -> updRestartServer s
  UpdResume{} -> return ()
  UpdResumeServer s -> updResumeServer s
  UpdKillExit{} -> return ()
  UpdWriteSave -> return ()
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
      g (Just l) =
#ifdef WITH_EXPENSIVE_ASSERTIONS
        assert (aid `notElem` l `blame` "actor already added"
                                `twith` (aid, body, l))
#endif
        (Just $ aid : l)
  updateLevel (blid body) $ updatePrio (EM.alter g (btime body))
                          . updateActorMap (EM.alter g (bpos body))
  -- Actor's items may or may not be already present in @sitemD@,
  -- regardless if they are already present otherwise in the dungeon.
  -- We re-add them all to save time determining which really need it.
  forM_ ais $ \(iid, item) -> do
    let h item1 item2 =
          assert (itemsMatch item1 item2
                  `blame` "inconsistent created actor items"
                  `twith` (aid, body, iid, item1, item2))
                 item2 -- keep the first found level
    modifyState $ updateItemD $ EM.insertWith h iid item

itemsMatch :: Item -> Item -> Bool
itemsMatch item1 item2 =
  jkindIx item1 == jkindIx item2
  -- && aspects and effects are the same, but too much writing;
  -- Note that nothing else needs to be the same, since items are merged
  -- and clients have different views on dungeon items than the server.

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
  let match (iid, item) = itemsMatch (itemD EM.! iid) item
  let !_A = assert (allB match ais `blame` "destroyed actor items not found"
                    `twith` (aid, body, ais, itemD)) ()
  -- Remove actor from @sactorD@.
  let f Nothing = assert `failure` "actor already removed" `twith` (aid, body)
      f (Just b) = assert (b == body `blame` "inconsistent destroyed actor body"
                                     `twith` (aid, body, b)) Nothing
  modifyState $ updateActorD $ EM.alter f aid
  -- Remove actor from @sprio@.
  let g Nothing = assert `failure` "actor already removed" `twith` (aid, body)
      g (Just l) =
#ifdef WITH_EXPENSIVE_ASSERTIONS
        assert (aid `elem` l `blame` "actor already removed"
                             `twith` (aid, body, l))
#endif
        (let l2 = delete aid l
         in if null l2 then Nothing else Just l2)
  updateLevel (blid body) $ updatePrio (EM.alter g (btime body))
                          . updateActorMap (EM.alter g (bpos body))

-- | Create a few copies of an item that is already registered for the dungeon
-- (in @sitemRev@ field of @StateServer@).
updCreateItem :: MonadStateWrite m
              => ItemId -> Item -> ItemQuant -> Container -> m ()
updCreateItem iid item kit@(k, _) c = assert (k > 0) $ do
  -- The item may or may not be already present in @sitemD@,
  -- regardless if it's actually present in the dungeon.
  -- If items equivalent, pick the one found on easier level.
  let f item1 item2 =
        assert (itemsMatch item1 item2)
               item2 -- keep the first found level
  modifyState $ updateItemD $ EM.insertWith f iid item
  insertItemContainer iid kit c

-- | Destroy some copies (possibly not all) of an item.
updDestroyItem :: MonadStateWrite m
               => ItemId -> Item -> ItemQuant -> Container -> m ()
updDestroyItem iid item kit@(k, _) c = assert (k > 0) $ do
  -- Do not remove the item from @sitemD@ nor from @sitemRev@,
  -- It's incredibly costly and not noticeable for the player.
  -- However, assert the item is registered in @sitemD@.
  itemD <- getsState sitemD
  let !_A = assert ((case iid `EM.lookup` itemD of
                        Nothing -> False
                        Just item0 -> itemsMatch item0 item)
                    `blame` "item already removed"
                    `twith` (iid, item, itemD)) ()
  deleteItemContainer iid kit c

updMoveActor :: MonadStateWrite m => ActorId -> Point -> Point -> m ()
updMoveActor aid fromP toP = assert (fromP /= toP) $ do
  body <- getsState $ getActorBody aid
  let !_A = assert (fromP == bpos body
                    `blame` "unexpected moved actor position"
                    `twith` (aid, fromP, toP, bpos body, body)) ()
      newBody = body {bpos = toP, boldpos = Just fromP}
  updateActor aid $ const newBody
  moveActorMap aid body newBody

updWaitActor :: MonadStateWrite m => ActorId -> Bool -> m ()
updWaitActor aid toWait = do
  b <- getsState $ getActorBody aid
  let !_A = assert (toWait /= bwait b
                    `blame` "unexpected waited actor time"
                    `twith` (aid, toWait, bwait b, b)) ()
  updateActor aid $ \body -> body {bwait = toWait}

updDisplaceActor :: MonadStateWrite m => ActorId -> ActorId -> m ()
updDisplaceActor source target = assert (source /= target) $ do
  sbody <- getsState $ getActorBody source
  tbody <- getsState $ getActorBody target
  let spos = bpos sbody
      tpos = bpos tbody
      snewBody = sbody {bpos = tpos, boldpos = Just spos}
      tnewBody = tbody {bpos = spos, boldpos = Just tpos}
  updateActor source $ const snewBody
  updateActor target $ const tnewBody
  moveActorMap source sbody snewBody
  moveActorMap target tbody tnewBody

updMoveItem :: MonadStateWrite m
            => ItemId -> Int -> ActorId -> CStore -> CStore
            -> m ()
updMoveItem iid k aid c1 c2 = assert (k > 0 && c1 /= c2) $ do
  b <- getsState $ getActorBody aid
  bag <- getsState $ getBodyStoreBag b c1
  case iid `EM.lookup` bag of
    Nothing -> assert `failure` (iid, k, aid, c1, c2)
    Just (_, it) -> do
      deleteItemActor iid (k, take k it) aid c1
      insertItemActor iid (k, take k it) aid c2

-- This is equaivalent to (but much cheaper than) updDestroyActor
-- followed by updCreateActor.
updAgeActor :: MonadStateWrite m => ActorId -> Delta Time -> m ()
updAgeActor aid delta = assert (delta /= Delta timeZero) $ do
  body <- getsState $ getActorBody aid
  let newBody = body {btime = timeShift (btime body) delta}
      -- Remove actor from @sprio@ at old time.
      rmPrio Nothing = assert `failure` "actor already removed"
                              `twith` (aid, body)
      rmPrio (Just l) =
#ifdef WITH_EXPENSIVE_ASSERTIONS
        assert (aid `elem` l `blame` "actor already removed"
                             `twith` (aid, body, l))
#endif
        (let l2 = delete aid l
         in if null l2 then Nothing else Just l2)
      -- Add actor to @sprio@ at new time.
      addPrio Nothing = Just [aid]
      addPrio (Just l) =
#ifdef WITH_EXPENSIVE_ASSERTIONS
        assert (aid `notElem` l `blame` "actor already added"
                                `twith` (aid, body, l))
#endif
        (Just $ aid : l)
      updPrio = EM.alter addPrio (btime newBody) . EM.alter rmPrio (btime body)
  updateLevel (blid body) $ updatePrio updPrio
  -- Modify actor body in @sactorD@.
  modifyState $ updateActorD $ EM.insert aid newBody

updRefillHP :: MonadStateWrite m => ActorId -> Int64 -> m ()
updRefillHP aid n =
  updateActor aid $ \b ->
    b { bhp = bhp b + n
      , bhpDelta = let oldD = bhpDelta b
                   in if n == 0
                      then ResDelta { resCurrentTurn = 0
                                    , resPreviousTurn = resCurrentTurn oldD }
                      else oldD {resCurrentTurn = resCurrentTurn oldD + n}
      }

updRefillCalm :: MonadStateWrite m => ActorId -> Int64 -> m ()
updRefillCalm aid n =
  updateActor aid $ \b ->
    b { bcalm = max 0 $ bcalm b + n
      , bcalmDelta = let oldD = bcalmDelta b
                     in if n == 0
                        then ResDelta { resCurrentTurn = 0
                                      , resPreviousTurn = resCurrentTurn oldD }
                        else oldD {resCurrentTurn = resCurrentTurn oldD + n}
      }

updFidImpressedActor :: MonadStateWrite m => ActorId -> FactionId -> FactionId -> m ()
updFidImpressedActor aid fromFid toFid = assert (fromFid /= toFid) $
  updateActor aid $ \b ->
    assert (bfidImpressed b == fromFid `blame` (aid, fromFid, toFid, b))
    $ b {bfidImpressed = toFid}

updTrajectory :: MonadStateWrite m
              => ActorId
              -> Maybe ([Vector], Speed)
              -> Maybe ([Vector], Speed)
              -> m ()
updTrajectory aid fromT toT = assert (fromT /= toT) $ do
  body <- getsState $ getActorBody aid
  let !_A = assert (fromT == btrajectory body
                    `blame` "unexpected actor trajectory"
                    `twith` (aid, fromT, toT, body)) ()
  updateActor aid $ \b -> b {btrajectory = toT}

updColorActor :: MonadStateWrite m
              => ActorId -> Color.Color -> Color.Color -> m ()
updColorActor aid fromCol toCol = assert (fromCol /= toCol) $ do
  body <- getsState $ getActorBody aid
  let !_A = assert (fromCol == bcolor body
                    `blame` "unexpected actor color"
                    `twith` (aid, fromCol, toCol, body)) ()
  updateActor aid $ \b -> b {bcolor = toCol}

updQuitFaction :: MonadStateWrite m
               => FactionId -> Maybe Actor -> Maybe Status -> Maybe Status
               -> m ()
updQuitFaction fid mbody fromSt toSt = do
  let !_A = assert (fromSt /= toSt `blame` (fid, mbody, fromSt, toSt)) ()
  let !_A = assert (maybe True ((fid ==) . bfid) mbody) ()
  fact <- getsState $ (EM.! fid) . sfactionD
  let !_A = assert (fromSt == gquit fact
                    `blame` "unexpected actor quit status"
                    `twith` (fid, fromSt, toSt, fact)) ()
  let adj fa = fa {gquit = toSt}
  updateFaction fid adj

-- The previous leader is assumed to be alive.
updLeadFaction :: MonadStateWrite m
               => FactionId
               -> Maybe ActorId
               -> Maybe ActorId
               -> m ()
updLeadFaction fid source target = assert (source /= target) $ do
  fact <- getsState $ (EM.! fid) . sfactionD
  let !_A = assert (fleaderMode (gplayer fact) /= LeaderNull) ()
    -- @PosNone@ ensures this
  mtb <- getsState $ \s -> flip getActorBody s <$> target
  let !_A = assert (maybe True (not . bproj) mtb
                    `blame` (fid, source, target, mtb, fact)) ()
  let !_A = assert (source == gleader fact
                    `blame` "unexpected actor leader"
                    `twith` (fid, source, target, mtb, fact)) ()
  let adj fa = fa {gleader = target}
  updateFaction fid adj

updDiplFaction :: MonadStateWrite m
               => FactionId -> FactionId -> Diplomacy -> Diplomacy -> m ()
updDiplFaction fid1 fid2 fromDipl toDipl =
  assert (fid1 /= fid2 && fromDipl /= toDipl) $ do
    fact1 <- getsState $ (EM.! fid1) . sfactionD
    fact2 <- getsState $ (EM.! fid2) . sfactionD
    let !_A = assert (fromDipl == EM.findWithDefault Unknown fid2 (gdipl fact1)
                      && fromDipl == EM.findWithDefault Unknown fid1 (gdipl fact2)
                      `blame` "unexpected actor diplomacy status"
                      `twith` (fid1, fid2, fromDipl, toDipl, fact1, fact2)) ()
    let adj fid fact = fact {gdipl = EM.insert fid toDipl (gdipl fact)}
    updateFaction fid1 (adj fid2)
    updateFaction fid2 (adj fid1)

updAutoFaction :: MonadStateWrite m => FactionId -> Bool -> m ()
updAutoFaction fid st =
  updateFaction fid (\fact ->
    assert (isAIFact fact == not st)
    $ fact {gplayer = automatePlayer st (gplayer fact)})

updTacticFaction :: MonadStateWrite m => FactionId -> Tactic -> Tactic -> m ()
updTacticFaction fid toT fromT = do
  let adj fact =
        let player = gplayer fact
        in assert (ftactic player == fromT)
           $ fact {gplayer = player {ftactic = toT}}
  updateFaction fid adj

-- | Record a given number (usually just 1, or -1 for undo) of actor kills
-- for score calculation.
updRecordKill :: MonadStateWrite m => ActorId -> Kind.Id ItemKind -> Int -> m ()
updRecordKill aid ikind k = do
  b <- getsState $ getActorBody aid
  let !_A = assert (not (bproj b) `blame` (aid, b))
  let alterKind mn = let n = fromMaybe 0 mn + k
                     in if n == 0 then Nothing else Just n
      adjFact fact = fact {gvictims = EM.alter alterKind ikind
                                      $ gvictims fact}
  updateFaction (bfidOriginal b) adjFact

-- | Alter an attribute (actually, the only, the defining attribute)
-- of a visible tile. This is similar to e.g., @UpdTrajectory@.
updAlterTile :: MonadStateWrite m
             => LevelId -> Point -> Kind.Id TileKind -> Kind.Id TileKind
             -> m ()
updAlterTile lid p fromTile toTile = assert (fromTile /= toTile) $ do
  Kind.COps{cotile, coTileSpeedup} <- getsState scops
  lvl <- getLevel lid
  -- The second alternative below can happen if, e.g., a client remembers,
  -- but does not see the tile (so does not notice the SearchTile action),
  -- and it suddenly changes into another tile,
  -- which at the same time becomes visible (e.g., an open door).
  let adj ts = assert (ts PointArray.! p == fromTile
                       || ts PointArray.! p == Tile.hideAs cotile fromTile
                       `blame` "unexpected altered tile kind"
                       `twith` (lid, p, fromTile, toTile, ts PointArray.! p))
               $ ts PointArray.// [(p, toTile)]
  updateLevel lid $ updateTile adj
  case (Tile.isExplorable coTileSpeedup fromTile, Tile.isExplorable coTileSpeedup toTile) of
    (False, True) -> updateLevel lid $ \lvl2 -> lvl2 {lseen = lseen lvl + 1}
    (True, False) -> updateLevel lid $ \lvl2 -> lvl2 {lseen = lseen lvl - 1}
    _ -> return ()

updAlterClear :: MonadStateWrite m => LevelId -> Int -> m ()
updAlterClear lid delta = assert (delta /= 0) $
  updateLevel lid $ \lvl -> lvl {lclear = lclear lvl + delta}

-- TODO: use instead of revealing all secret positions initially, at once
-- in Common/State.hs.
updLearnSecrets :: MonadStateWrite m => ActorId -> Int -> Int -> m ()
updLearnSecrets aid fromS toS = assert (fromS /= toS) $ do
  b <- getsState $ getActorBody aid
  updateLevel (blid b) $ \lvl -> assert (lsecret lvl == fromS)
                                 $ lvl {lsecret = toS}

-- Notice previously invisible tiles. This is similar to @UpdSpotActor@,
-- but done in bulk, because it often involves dozens of tiles pers move.
-- We don't check that the tiles at the positions in question are unknown
-- to save computation, especially for clients that remember tiles
-- at previously seen positions. Similarly, when updating the @lseen@
-- field we don't assume the tiles were unknown previously.
updSpotTile :: MonadStateWrite m
            => LevelId -> [(Point, Kind.Id TileKind)] -> m ()
updSpotTile lid ts = assert (not $ null ts) $ do
  Kind.COps{coTileSpeedup} <- getsState scops
  Level{ltile} <- getLevel lid
  let adj tileMap = tileMap PointArray.// ts
  updateLevel lid $ updateTile adj
  let f (p, t2) = do
        let t1 = ltile PointArray.! p
        case (Tile.isExplorable coTileSpeedup t1, Tile.isExplorable coTileSpeedup t2) of
          (False, True) -> updateLevel lid $ \lvl -> lvl {lseen = lseen lvl+1}
          (True, False) -> updateLevel lid $ \lvl -> lvl {lseen = lseen lvl-1}
          _ -> return ()
  mapM_ f ts

-- Stop noticing previously visible tiles. Unlike @updSpotActor@, it verifies
-- the state of the tiles before changing them.
updLoseTile :: MonadStateWrite m
            => LevelId -> [(Point, Kind.Id TileKind)] -> m ()
updLoseTile lid ts = assert (not $ null ts) $ do
  Kind.COps{coTileSpeedup} <- getsState scops
  let matches _ [] = True
      matches tileMap ((p, ov) : rest) =
        tileMap PointArray.! p == ov && matches tileMap rest
      tu = map (second (const unknownId)) ts
      adj tileMap = assert (matches tileMap ts) $ tileMap PointArray.// tu
  updateLevel lid $ updateTile adj
  let f (_, t1) =
        when (Tile.isExplorable coTileSpeedup t1) $
          updateLevel lid $ \lvl -> lvl {lseen = lseen lvl - 1}
  mapM_ f ts

updAlterSmell :: MonadStateWrite m => LevelId -> Point -> Time -> Time -> m ()
updAlterSmell lid p fromSm' toSm' = do
  let fromSm = if fromSm' == timeZero then Nothing else Just fromSm'
      toSm = if toSm' == timeZero then Nothing else Just toSm'
      alt sm = assert (sm == fromSm `blame` "unexpected tile smell"
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

updTimeItem :: MonadStateWrite m
            => ItemId -> Container -> ItemTimer -> ItemTimer
            -> m ()
updTimeItem iid c fromIt toIt = assert (fromIt /= toIt) $ do
  bag <- getsState $ getContainerBag c
  case iid `EM.lookup` bag of
    Just (k, it) -> do
      let !_A = assert (fromIt == it `blame` (k, it, iid, c, fromIt, toIt)) ()
      deleteItemContainer iid (k, fromIt) c
      insertItemContainer iid (k, toIt) c
    Nothing -> assert `failure` (bag, iid, c, fromIt, toIt)

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

updRestart :: MonadStateWrite m => State -> m ()
updRestart = putState

updRestartServer :: MonadStateWrite m => State -> m ()
updRestartServer = putState

updResumeServer :: MonadStateWrite m => State -> m ()
updResumeServer = putState
