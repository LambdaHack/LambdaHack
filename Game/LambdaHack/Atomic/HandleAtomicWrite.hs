-- | Semantics of atomic commands shared by client and server.
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Atomic.HandleAtomicWrite
  ( handleUpdAtomic
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , updCreateActor, updDestroyActor, updCreateItem, updDestroyItem
  , updMoveActor, updWaitActor, updDisplaceActor, updMoveItem
  , updRefillHP, updRefillCalm
  , updTrajectory, updQuitFaction, updLeadFaction
  , updDiplFaction, updTacticFaction, updAutoFaction, updRecordKill
  , updAlterTile, updAlterExplorable, updSpotTile, updLoseTile
  , updAlterSmell, updSpotSmell, updLoseSmell, updTimeItem
  , updAgeGame, updUnAgeGame, updRestart, updRestartServer, updResumeServer
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import Data.Int (Int64)

import Game.LambdaHack.Atomic.CmdAtomic
import Game.LambdaHack.Atomic.MonadStateWrite
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
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
handleUpdAtomic :: MonadStateWrite m => UpdAtomic -> m ()
handleUpdAtomic cmd = case cmd of
  UpdCreateActor aid body ais -> updCreateActor aid body ais
  UpdDestroyActor aid body ais -> updDestroyActor aid body ais
  UpdCreateItem iid item kit c -> updCreateItem iid item kit c
  UpdDestroyItem iid item kit c -> updDestroyItem iid item kit c
  UpdSpotActor aid body ais -> updCreateActor aid body ais
  UpdLoseActor aid body ais -> updDestroyActor aid body ais
  UpdSpotItem _ iid item kit c -> updCreateItem iid item kit c
  UpdLoseItem _ iid item kit c -> updDestroyItem iid item kit c
  UpdMoveActor aid fromP toP -> updMoveActor aid fromP toP
  UpdWaitActor aid toWait -> updWaitActor aid toWait
  UpdDisplaceActor source target -> updDisplaceActor source target
  UpdMoveItem iid k aid c1 c2 -> updMoveItem iid k aid c1 c2
  UpdRefillHP aid n -> updRefillHP aid n
  UpdRefillCalm aid n -> updRefillCalm aid n
  UpdTrajectory aid fromT toT -> updTrajectory aid fromT toT
  UpdQuitFaction fid fromSt toSt -> updQuitFaction fid fromSt toSt
  UpdLeadFaction fid source target -> updLeadFaction fid source target
  UpdDiplFaction fid1 fid2 fromDipl toDipl ->
    updDiplFaction fid1 fid2 fromDipl toDipl
  UpdTacticFaction fid toT fromT -> updTacticFaction fid toT fromT
  UpdAutoFaction fid st -> updAutoFaction fid st
  UpdRecordKill aid ikind k -> updRecordKill aid ikind k
  UpdAlterTile lid p fromTile toTile -> updAlterTile lid p fromTile toTile
  UpdAlterExplorable lid delta -> updAlterExplorable lid delta
  UpdSearchTile aid p toTile -> updSearchTile aid p toTile
  UpdHideTile{} -> undefined
  UpdSpotTile lid ts -> updSpotTile lid ts
  UpdLoseTile lid ts -> updLoseTile lid ts
  UpdAlterSmell lid p fromSm toSm -> updAlterSmell lid p fromSm toSm
  UpdSpotSmell lid sms -> updSpotSmell lid sms
  UpdLoseSmell lid sms -> updLoseSmell lid sms
  UpdTimeItem iid c fromIt toIt -> updTimeItem iid c fromIt toIt
  UpdAgeGame lids -> updAgeGame lids
  UpdUnAgeGame lids -> updUnAgeGame lids
  UpdDiscover c iid ik seed -> updDiscover c iid ik seed
  UpdCover c iid ik seed -> updCover c iid ik seed
  UpdDiscoverKind c iid ik -> updDiscoverKind c iid ik
  UpdCoverKind c iid ik -> updCoverKind c iid ik
  UpdDiscoverSeed c iid seed -> updDiscoverSeed c iid seed
  UpdCoverSeed c iid seed -> updCoverSeed c iid seed
  UpdDiscoverServer iid aspectRecord -> updDiscoverServer iid aspectRecord
  UpdCoverServer iid aspectRecord -> updCoverServer iid aspectRecord
  UpdPerception _ outPer inPer ->
    assert (not (nullPer outPer && nullPer inPer)) (return ())
  UpdRestart _ _ s _ _ -> updRestart s
  UpdRestartServer s -> updRestartServer s
  UpdResume{} -> return ()
  UpdResumeServer s -> updResumeServer s
  UpdKillExit{} -> return ()
  UpdWriteSave -> return ()

-- | Creates an actor. Note: after this command, usually a new leader
-- for the party should be elected (in case this actor is the only one alive).
updCreateActor :: MonadStateWrite m
               => ActorId -> Actor -> [(ItemId, Item)] -> m ()
updCreateActor aid body ais = do
  -- Add actor to @sactorD@.
  -- The exception is possible, e.g., when we teleport and so see our actor
  -- at the new location, but also the location is part of new perception,
  -- so @UpdSpotActor@ is sent.
  let f Nothing = Just body
      f (Just b) = assert (body == b `blame` (aid, body, b)) $
        atomicFail $ "actor already added" `showFailure` (aid, body, b)
  modifyState $ updateActorD $ EM.alter f aid
  -- Add actor to @sprio@.
  let g Nothing = Just [aid]
      g (Just l) =
#ifdef WITH_EXPENSIVE_ASSERTIONS
        -- Not so much expensive, as doubly impossible.
        assert (aid `notElem` l `blame` "actor already added"
                                `swith` (aid, body, l))
#endif
        (Just $ aid : l)
  updateLevel (blid body) $ updateActorMap (EM.alter g (bpos body))
  -- Actor's items may or may not be already present in @sitemD@,
  -- regardless if they are already present otherwise in the dungeon.
  -- We re-add them all to save time determining which really need it.
  forM_ ais $ \(iid, item) -> do
    let h item1 item2 =
          assert (itemsMatch item1 item2
                  `blame` "inconsistent created actor items"
                  `swith` (aid, body, iid, item1, item2))
                 item2 -- keep the first found level
    modifyState $ updateItemD $ EM.insertWith h iid item
  aspectRecord <- getsState $ aspectRecordFromActor body
  modifyState $ updateActorAspect $ EM.insert aid aspectRecord

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
  -- before this command is executed (not checked).
  itemD <- getsState sitemD
  let match (iid, item) = itemsMatch (itemD EM.! iid) item
  -- Assert that actor's items belong to @sitemD@. Do not remove those
  -- that do not appear anywhere else, for simplicity and speed.
  let !_A = assert (allB match ais `blame` "destroyed actor items not found"
                    `swith` (aid, body, ais, itemD)) ()
  -- Remove actor from @sactorD@.
  let f Nothing = error $ "actor already removed" `showFailure` (aid, body)
      f (Just b) = assert (b == body `blame` "inconsistent destroyed actor body"
                                     `swith` (aid, body, b)) Nothing
  modifyState $ updateActorD $ EM.alter f aid
  -- Remove actor from @lactor@.
  let g Nothing = error $ "actor already removed" `showFailure` (aid, body)
      g (Just l) =
#ifdef WITH_EXPENSIVE_ASSERTIONS
        -- Not so much expensive, as doubly impossible.
        assert (aid `elem` l `blame` "actor already removed"
                             `swith` (aid, body, l))
#endif
        (let l2 = delete aid l
         in if null l2 then Nothing else Just l2)
  updateLevel (blid body) $ updateActorMap (EM.alter g (bpos body))
  modifyState $ updateActorAspect $ EM.delete aid

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
  case c of
    CActor aid store ->
      when (store `elem` [CEqp, COrgan]) $ addItemToActor iid item k aid
    _ -> return ()

addItemToActor :: MonadStateWrite m => ItemId -> Item -> Int -> ActorId -> m ()
addItemToActor iid itemBase k aid = do
  arItem <- getsState $ aspectRecordFromItem iid itemBase
  let f arActor = sumAspectRecord [(arActor, 1), (arItem, k)]
  modifyState $ updateActorAspect $ EM.adjust f aid

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
                    `swith` (iid, item, itemD)) ()
  deleteItemContainer iid kit c
  case c of
    CActor aid store ->
      when (store `elem` [CEqp, COrgan]) $ addItemToActor iid item (-k) aid
    _ -> return ()

updMoveActor :: MonadStateWrite m => ActorId -> Point -> Point -> m ()
updMoveActor aid fromP toP = assert (fromP /= toP) $ do
  body <- getsState $ getActorBody aid
  let !_A = assert (fromP == bpos body
                    `blame` "unexpected moved actor position"
                    `swith` (aid, fromP, toP, bpos body, body)) ()
      newBody = body {bpos = toP, boldpos = Just fromP}
  updateActor aid $ const newBody
  moveActorMap aid body newBody

updWaitActor :: MonadStateWrite m => ActorId -> Bool -> m ()
updWaitActor aid toWait = do
  b <- getsState $ getActorBody aid
  let !_A = assert (toWait /= bwait b
                    `blame` "unexpected waited actor time"
                    `swith` (aid, toWait, bwait b, b)) ()
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
updMoveItem iid k aid s1 s2 = assert (k > 0 && s1 /= s2) $ do
  b <- getsState $ getActorBody aid
  bag <- getsState $ getBodyStoreBag b s1
  case iid `EM.lookup` bag of
    Nothing -> error $ "" `showFailure` (iid, k, aid, s1, s2)
    Just (_, it) -> do
      deleteItemActor iid (k, take k it) aid s1
      insertItemActor iid (k, take k it) aid s2
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

updRefillHP :: MonadStateWrite m => ActorId -> Int64 -> m ()
updRefillHP aid n =
  updateActor aid $ \b ->
    b { bhp = bhp b + n
      , bhpDelta = let oldD = bhpDelta b
                   in case compare n 0 of
                     EQ -> ResDelta { resCurrentTurn = (0, 0)
                                    , resPreviousTurn = resCurrentTurn oldD }
                     LT -> oldD {resCurrentTurn =
                                   ( fst (resCurrentTurn oldD) + n
                                   , snd (resCurrentTurn oldD) )}
                     GT -> oldD {resCurrentTurn =
                                   ( fst (resCurrentTurn oldD)
                                   , snd (resCurrentTurn oldD) + n )}
      }

updRefillCalm :: MonadStateWrite m => ActorId -> Int64 -> m ()
updRefillCalm aid n =
  updateActor aid $ \b ->
    b { bcalm = max 0 $ bcalm b + n
      , bcalmDelta = let oldD = bcalmDelta b
                     in case compare n 0 of
                       EQ -> ResDelta { resCurrentTurn = (0, 0)
                                      , resPreviousTurn = resCurrentTurn oldD }
                       LT -> oldD {resCurrentTurn =
                                     ( fst (resCurrentTurn oldD) + n
                                     , snd (resCurrentTurn oldD) )}
                       GT -> oldD {resCurrentTurn =
                                     ( fst (resCurrentTurn oldD)
                                     , snd (resCurrentTurn oldD) + n )}
      }

updTrajectory :: MonadStateWrite m
              => ActorId
              -> Maybe ([Vector], Speed)
              -> Maybe ([Vector], Speed)
              -> m ()
updTrajectory aid fromT toT = assert (fromT /= toT) $ do
  body <- getsState $ getActorBody aid
  let !_A = assert (fromT == btrajectory body
                    `blame` "unexpected actor trajectory"
                    `swith` (aid, fromT, toT, body)) ()
  updateActor aid $ \b -> b {btrajectory = toT}

updQuitFaction :: MonadStateWrite m
               => FactionId -> Maybe Status -> Maybe Status -> m ()
updQuitFaction fid fromSt toSt = do
  let !_A = assert (fromSt /= toSt `blame` (fid, fromSt, toSt)) ()
  fact <- getsState $ (EM.! fid) . sfactionD
  let !_A = assert (fromSt == gquit fact
                    `blame` "unexpected actor quit status"
                    `swith` (fid, fromSt, toSt, fact)) ()
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
  let !_A = assert (source == _gleader fact
                    `blame` "unexpected actor leader"
                    `swith` (fid, source, target, mtb, fact)) ()
  let adj fa = fa {_gleader = target}
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
                      `swith` (fid1, fid2, fromDipl, toDipl, fact1, fact2)) ()
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
  updateFaction (bfid b) adjFact
    -- The death of a dominated actor counts as the dominating faction's loss
    -- for score purposes, so human nor AI can't treat such actor as disposable,
    -- which means domination will not be as cruel, as frustrating,
    -- as it could be and there is a higher chance of getting back alive
    -- the actor, the human player has grown attached to.

-- | Alter an attribute (actually, the only, the defining attribute)
-- of a visible tile. This is similar to e.g., @UpdTrajectory@.
--
-- For now, we don't remove embedded items when altering a tile
-- and neither do we create fresh ones. It works fine, e.g., for tiles on fire
-- that change into burnt out tile and then the fire item can no longer
-- be triggered due to @alterMinSkillKind@ excluding items without @Embed@,
-- even if the burnt tile has low enough @talter@.
updAlterTile :: MonadStateWrite m
             => LevelId -> Point -> Kind.Id TileKind -> Kind.Id TileKind
             -> m ()
updAlterTile lid p fromTile toTile = assert (fromTile /= toTile) $ do
  Kind.COps{coTileSpeedup} <- getsState scops
  lvl <- getLevel lid
  let t = lvl `at` p
  if t /= fromTile
  then atomicFail "tile to alter is different than assumed"
  else do
    let adj ts = ts PointArray.// [(p, toTile)]
    updateLevel lid $ updateTile adj
    case ( Tile.isExplorable coTileSpeedup fromTile
         , Tile.isExplorable coTileSpeedup toTile ) of
      (False, True) -> updateLevel lid $ \lvl2 -> lvl2 {lseen = lseen lvl + 1}
      (True, False) -> updateLevel lid $ \lvl2 -> lvl2 {lseen = lseen lvl - 1}
      _ -> return ()

updAlterExplorable :: MonadStateWrite m => LevelId -> Int -> m ()
updAlterExplorable lid delta = assert (delta /= 0) $
  updateLevel lid $ \lvl -> lvl {lexplorable = lexplorable lvl + delta}

updSearchTile :: MonadStateWrite m
              => ActorId -> Point -> Kind.Id TileKind -> m ()
updSearchTile aid p toTile = do
  Kind.COps{cotile} <- getsState scops
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  let t = lvl `at` p
  if t == toTile
  then atomicFail "tile already searched"
  else assert (t == Tile.hideAs cotile toTile)
       $ updSpotTile (blid b) [(p, toTile)]

-- Notice previously invisible tiles. This is similar to @UpdSpotActor@,
-- but done in bulk, because it often involves dozens of tiles per move.
-- We don't check that the tiles at the positions in question are unknown,
-- because clients remember tiles at previously seen positions
-- instead of marking them unknown as soon as they get out of view.
-- Similarly, when updating the @lseen@ field we don't assume
-- the tiles were unknown previously.
updSpotTile :: MonadStateWrite m
            => LevelId -> [(Point, Kind.Id TileKind)] -> m ()
updSpotTile lid ts = assert (not $ null ts) $ do
  Kind.COps{coTileSpeedup} <- getsState scops
  lvlInitial <- getLevel lid
  let f (p, t2) = do
        let t1 = lvlInitial `at` p
        case ( Tile.isExplorable coTileSpeedup t1
             , Tile.isExplorable coTileSpeedup t2 ) of
          (False, True) -> updateLevel lid $ \lvl -> lvl {lseen = lseen lvl+1}
          (True, False) -> updateLevel lid $ \lvl -> lvl {lseen = lseen lvl-1}
          _ -> return ()
        return $ t1 == t2
  bs <- mapM f ts
  when (and bs) $ atomicFail "all tiles already spotted"
  let adj tileMap = tileMap PointArray.// ts
  updateLevel lid $ updateTile adj

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
                                    `swith` (lid, p, fromSm, toSm, sm)) toSm
  updateLevel lid $ updateSmell $ EM.alter alt p

updSpotSmell :: MonadStateWrite m => LevelId -> [(Point, Time)] -> m ()
updSpotSmell lid sms = assert (not $ null sms) $ do
  let alt sm Nothing = Just sm
      alt sm (Just oldSm) = error $ "smell already added"
                                    `showFailure` (lid, sms, sm, oldSm)
      f (p, sm) = EM.alter (alt sm) p
      upd m = foldr f m sms
  updateLevel lid $ updateSmell upd

updLoseSmell :: MonadStateWrite m => LevelId -> [(Point, Time)] -> m ()
updLoseSmell lid sms = assert (not $ null sms) $ do
  let alt sm Nothing = error $ "smell already removed"
                               `showFailure` (lid, sms, sm)
      alt sm (Just oldSm) =
        assert (sm == oldSm `blame` "unexpected lost smell"
                            `swith` (lid, sms, sm, oldSm)) Nothing
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
    Nothing -> error $ "" `showFailure` (bag, iid, c, fromIt, toIt)

-- | Age the game.
updAgeGame :: MonadStateWrite m => [LevelId] -> m ()
updAgeGame lids = do
  modifyState $ updateTime $ flip timeShift (Delta timeClip)
  mapM_ (ageLevel (Delta timeClip)) lids

updUnAgeGame :: MonadStateWrite m => [LevelId] -> m ()
updUnAgeGame lids = do
  modifyState $ updateTime $ flip timeShift (timeDeltaReverse $ Delta timeClip)
  mapM_ (ageLevel (timeDeltaReverse $ Delta timeClip)) lids

ageLevel :: MonadStateWrite m => Delta Time -> LevelId -> m ()
ageLevel delta lid =
  updateLevel lid $ \lvl -> lvl {ltime = timeShift (ltime lvl) delta}

updDiscover :: MonadStateWrite m
            => Container -> ItemId -> Kind.Id ItemKind -> ItemSeed -> m ()
updDiscover _c iid ik seed = do
  itemD <- getsState sitemD
  case EM.lookup iid itemD of
    Nothing -> atomicFail "discovered item unknown"
    Just item -> do
      discoKind <- getsState sdiscoKind
      if jkindIx item `EM.member` discoKind
      then do
        discoAspect <- getsState sdiscoAspect
        if iid `EM.member` discoAspect
          then atomicFail "item already fully discovered"
          else do
            discoverSeed iid seed
            resetActorAspect
      else do
        discoverKind iid ik
        discoverSeed iid seed
        resetActorAspect

resetActorAspect :: MonadStateWrite m => m ()
resetActorAspect = do
  -- Each actor's equipment and organs would need to be inspected,
  -- the iid looked up, e.g., if it wasn't in old discoKind, but is in new,
  -- and then aspect record updated, so it's simpler and not much more
  -- expensive to generate new sactorAspect. Optimize only after profiling.
  -- Also note this doesn't get invoked on the server, because it bails out
  -- earlier, upon noticing the item is already fully known.
  actorAspect <- getsState actorAspectInDungeon
  modifyState $ updateActorAspect $ const actorAspect

updCover :: Container -> ItemId -> Kind.Id ItemKind -> ItemSeed -> m ()
updCover _c _iid _ik _seed = undefined

updDiscoverKind :: MonadStateWrite m
                => Container -> ItemId -> Kind.Id ItemKind -> m ()
updDiscoverKind _c iid kmKind = do
  itemD <- getsState sitemD
  case EM.lookup iid itemD of
    Nothing -> atomicFail "discovered item unknown"
    Just item -> do
      discoKind <- getsState sdiscoKind
      if jkindIx item `EM.member` discoKind
      then atomicFail "item kind already discovered"
      else do
        discoverKind iid kmKind
        resetActorAspect

discoverKind :: MonadStateWrite m => ItemId -> Kind.Id ItemKind -> m ()
discoverKind iid kmKind = do
  Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
  item <- getsState $ getItemBody iid
  let kind = okind kmKind
      kmMean = meanAspect kind
      f Nothing = Just KindMean{..}
      f Just{} = error $ "already discovered" `showFailure` (iid, kmKind)
  modifyState $ updateDiscoKind $ \discoKind1 ->
    EM.alter f (jkindIx item) discoKind1

updCoverKind :: Container -> ItemId -> Kind.Id ItemKind -> m ()
updCoverKind _c _iid _ik = undefined

updDiscoverSeed :: MonadStateWrite m
                => Container -> ItemId -> ItemSeed -> m ()
updDiscoverSeed _c iid seed = do
  itemD <- getsState sitemD
  case EM.lookup iid itemD of
    Nothing -> atomicFail "discovered item unknown"
    Just item -> do
      discoKind <- getsState sdiscoKind
      if jkindIx item `EM.notMember` discoKind
      then error "discovered item kind unknown"
      else do
        discoAspect <- getsState sdiscoAspect
        if iid `EM.member` discoAspect
        then atomicFail "item seed already discovered"
        else do
          discoverSeed iid seed
          resetActorAspect

discoverSeed :: MonadStateWrite m => ItemId -> ItemSeed -> m ()
discoverSeed iid seed = do
  Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
  item <- getsState $ getItemBody iid
  totalDepth <- getsState stotalDepth
  Level{ldepth} <- getLevel $ jlid item
  discoKind <- getsState sdiscoKind
  let KindMean{..} = case EM.lookup (jkindIx item) discoKind of
        Just km -> km
        Nothing -> error "discovered item kind unknown"
      kind = okind kmKind
      aspects = seedToAspect seed kind ldepth totalDepth
      f Nothing = Just aspects
      f Just{} = error $ "already discovered" `showFailure` (iid, seed)
  modifyState $ updateDiscoAspect $ \discoAspect1 ->
    EM.alter f iid discoAspect1

updCoverSeed :: Container -> ItemId -> ItemSeed -> m ()
updCoverSeed _c _iid _seed = undefined

updDiscoverServer :: MonadStateWrite m => ItemId -> AspectRecord -> m ()
updDiscoverServer iid aspectRecord =
  modifyState $ updateDiscoAspect $ \discoAspect1 ->
    EM.insert iid aspectRecord discoAspect1

updCoverServer :: MonadStateWrite m => ItemId -> AspectRecord -> m ()
updCoverServer iid aspectRecord =
  modifyState $ updateDiscoAspect $ \discoAspect1 ->
    assert (discoAspect1 EM.! iid == aspectRecord)
    $ EM.delete iid discoAspect1

updRestart :: MonadStateWrite m => State -> m ()
updRestart = putState

updRestartServer :: MonadStateWrite m => State -> m ()
updRestartServer = putState

updResumeServer :: MonadStateWrite m => State -> m ()
updResumeServer = putState
