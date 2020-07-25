{-# LANGUAGE FlexibleContexts #-}
-- | Semantics of atomic commands shared by client and server.
--
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Atomic.HandleAtomicWrite
  ( handleUpdAtomic
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , updRegisterItems, updCreateActor, updDestroyActor
  , updCreateItem, updDestroyItem, updSpotItemBag, updLoseItemBag
  , updMoveActor, updWaitActor, updDisplaceActor, updMoveItem
  , updRefillHP, updRefillCalm
  , updTrajectory, updQuitFaction, updSpotStashFaction, updLoseStashFaction
  , updLeadFaction, updDiplFaction, updDoctrineFaction, updAutoFaction
  , updRecordKill, updAlterTile, updAlterExplorable, updSearchTile
  , updSpotTile, updLoseTile, updAlterSmell, updSpotSmell, updLoseSmell
  , updTimeItem, updAgeGame, updUnAgeGame, ageLevel, updDiscover, updCover
  , updDiscoverKind, discoverKind, updCoverKind
  , updDiscoverAspect, discoverAspect, updCoverAspect
  , updDiscoverServer, updCoverServer
  , updRestart, updRestartServer, updResumeServer
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Int (Int64)

import           Game.LambdaHack.Atomic.CmdAtomic
import           Game.LambdaHack.Atomic.MonadStateWrite
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Perception
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Vector
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import           Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Content.PlaceKind as PK
import           Game.LambdaHack.Content.TileKind (TileKind, unknownId)
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs

-- | The game-state semantics of atomic game commands.
-- There is no corresponding definition for special effects (@SfxAtomic@),
-- because they don't modify 'State'.
--
-- For each of the commands, we are guaranteed that the client,
-- the command is addressed to, perceives all the positions the command
-- affects (as computed by 'Game.LambdaHack.Atomic.PosAtomicRead.posUpdAtomic').
-- In the code for each semantic function we additonally verify
-- the client is aware of any relevant items and/or actors and we throw
-- the @AtomicFail@ exception if it's not.
-- The server keeps copies of all clients' states and, before sending a command
-- to a client, applies it to the client's state copy.
-- If @AtomicFail@ is signalled, the command is ignored for that client.
-- This enables simpler server code that addresses commands to all clients
-- that can see it, even though not all are able to process it.
handleUpdAtomic :: MonadStateWrite m => UpdAtomic -> m ()
handleUpdAtomic cmd = case cmd of
  UpdRegisterItems ais -> updRegisterItems ais
  UpdCreateActor aid body ais -> updCreateActor aid body ais
  UpdDestroyActor aid body ais -> updDestroyActor aid body ais
  UpdCreateItem _ iid item kit c -> updCreateItem iid item kit c
  UpdDestroyItem _ iid item kit c -> updDestroyItem iid item kit c
  UpdSpotActor aid body -> updSpotActor aid body
  UpdLoseActor aid body -> updLoseActor aid body
  UpdSpotItem _ iid kit c -> updSpotItem iid kit c
  UpdLoseItem _ iid kit c -> updLoseItem iid kit c
  UpdSpotItemBag _ c bag -> updSpotItemBag c bag
  UpdLoseItemBag _ c bag -> updLoseItemBag c bag
  UpdMoveActor aid fromP toP -> updMoveActor aid fromP toP
  UpdWaitActor aid fromWS toWS -> updWaitActor aid fromWS toWS
  UpdDisplaceActor source target -> updDisplaceActor source target
  UpdMoveItem iid k aid c1 c2 -> updMoveItem iid k aid c1 c2
  UpdRefillHP aid n -> updRefillHP aid n
  UpdRefillCalm aid n -> updRefillCalm aid n
  UpdTrajectory aid fromT toT -> updTrajectory aid fromT toT
  UpdQuitFaction fid fromSt toSt _ -> updQuitFaction fid fromSt toSt
  UpdSpotStashFaction _ fid lid pos -> updSpotStashFaction fid lid pos
  UpdLoseStashFaction _ fid lid pos -> updLoseStashFaction fid lid pos
  UpdLeadFaction fid source target -> updLeadFaction fid source target
  UpdDiplFaction fid1 fid2 fromDipl toDipl ->
    updDiplFaction fid1 fid2 fromDipl toDipl
  UpdDoctrineFaction fid toT fromT -> updDoctrineFaction fid toT fromT
  UpdAutoFaction fid st -> updAutoFaction fid st
  UpdRecordKill aid ikind k -> updRecordKill aid ikind k
  UpdAlterTile lid p fromTile toTile -> updAlterTile lid p fromTile toTile
  UpdAlterExplorable lid delta -> updAlterExplorable lid delta
  UpdAlterGold delta -> updAlterGold delta
  UpdSearchTile aid p toTile -> updSearchTile aid p toTile
  UpdHideTile{} -> undefined
  UpdSpotTile lid ts -> updSpotTile lid ts
  UpdLoseTile lid ts -> updLoseTile lid ts
  UpdSpotEntry lid ts -> updSpotEntry lid ts
  UpdLoseEntry lid ts -> updLoseEntry lid ts
  UpdAlterSmell lid p fromSm toSm -> updAlterSmell lid p fromSm toSm
  UpdSpotSmell lid sms -> updSpotSmell lid sms
  UpdLoseSmell lid sms -> updLoseSmell lid sms
  UpdTimeItem iid c fromIt toIt -> updTimeItem iid c fromIt toIt
  UpdAgeGame lids -> updAgeGame lids
  UpdUnAgeGame lids -> updUnAgeGame lids
  UpdDiscover c iid ik arItem -> updDiscover c iid ik arItem
  UpdCover c iid ik arItem -> updCover c iid ik arItem
  UpdDiscoverKind c ix ik -> updDiscoverKind c ix ik
  UpdCoverKind c ix ik -> updCoverKind c ix ik
  UpdDiscoverAspect c iid arItem -> updDiscoverAspect c iid arItem
  UpdCoverAspect c iid arItem -> updCoverAspect c iid arItem
  UpdDiscoverServer iid arItem -> updDiscoverServer iid arItem
  UpdCoverServer iid arItem -> updCoverServer iid arItem
  UpdPerception _ outPer inPer ->
    assert (not (nullPer outPer && nullPer inPer)) (return ())
  UpdRestart _ _ s _ _ _ -> updRestart s
  UpdRestartServer s -> updRestartServer s
  UpdResume{} -> return ()
  UpdResumeServer s -> updResumeServer s
  UpdKillExit{} -> return ()
  UpdWriteSave -> return ()
  UpdHearFid{} -> return ()

-- Actor's items may or may not be already present in @sitemD@,
-- regardless if they are already present otherwise in the dungeon.
-- We re-add them all to save time determining which really need it.
-- If collision occurs, pick the item found on easier level.
updRegisterItems :: MonadStateWrite m => [(ItemId, Item)] -> m ()
updRegisterItems ais = do
  let h item1 item2 =
        assert (itemsMatch item1 item2
                `blame` "inconsistent added items"
                `swith` (item1, item2, ais))
               item2 -- keep the first found level
  forM_ ais $ \(iid, item) -> do
    let f = case jkind item of
          IdentityObvious _ -> id
          IdentityCovered ix _ ->
            updateItemIxMap $ EM.insertWith ES.union ix (ES.singleton iid)
    modifyState $ f . updateItemD (EM.insertWith h iid item)

-- Note: after this command, usually a new leader
-- for the party should be elected (in case this actor is the only one alive).
updCreateActor :: MonadStateWrite m
               => ActorId -> Actor -> [(ItemId, Item)] -> m ()
updCreateActor aid body ais = do
  updRegisterItems ais
  updSpotActor aid body

-- If a leader dies, a new leader should be elected on the server
-- before this command is executed (not checked).
updDestroyActor :: MonadStateWrite m
                => ActorId -> Actor -> [(ItemId, Item)] -> m ()
updDestroyActor aid body ais = do
  -- Assert that actor's items belong to @sitemD@. Do not remove those
  -- that do not appear anywhere else, for simplicity and speed.
  itemD <- getsState sitemD
  let match (iid, item) = itemsMatch (itemD EM.! iid) item
  let !_A = assert (allB match ais `blame` "destroyed actor items not found"
                    `swith` (aid, body, ais, itemD)) ()
  updLoseActor aid body

-- Create a few copies of an item that is already registered for the dungeon
-- (in @sitemRev@ field of @StateServer@).
--
-- Number of copies may be zero, when the item is only created as a sample
-- to let the player know what can potentially be genereated in the dungeon.
updCreateItem :: MonadStateWrite m
              => ItemId -> Item -> ItemQuant -> Container -> m ()
updCreateItem iid item kit c = do
  updRegisterItems [(iid, item)]
  updSpotItem iid kit c

-- Destroy some copies (possibly not all) of an item.
updDestroyItem :: MonadStateWrite m
               => ItemId -> Item -> ItemQuant -> Container -> m ()
updDestroyItem iid item kit@(k, _) c = assert (k > 0) $ do
  -- Do not remove the item from @sitemD@ nor from @sitemRev@
  -- nor from @DiscoveryAspect@, @ItemIxMap@, etc.
  -- It's incredibly costly and not particularly noticeable for the player.
  -- Moreover, copies of the item may reappear in the future
  -- and then we save computation and the player remembers past discovery.
  -- However, assert the item is registered in @sitemD@.
  itemD <- getsState sitemD
  let !_A = assert ((case iid `EM.lookup` itemD of
                        Nothing -> False
                        Just item0 -> itemsMatch item0 item)
                    `blame` "item already removed"
                    `swith` (iid, item, itemD)) ()
  updLoseItem iid kit c

updSpotActor :: MonadStateWrite m => ActorId -> Actor -> m ()
updSpotActor aid body = do
  -- The exception is possible, e.g., when we teleport and so see our actor
  -- at the new location, but also the location is part of new perception,
  -- so @UpdSpotActor@ is sent.
  let f Nothing = Just body
      f (Just b) = assert (body == b `blame` (aid, body, b)) $
        atomicFail $ "actor already added" `showFailure` (aid, body, b)
  modifyState $ updateActorD $ EM.alter f aid
  let g Nothing = Just [aid]
      g (Just l) =
#ifdef WITH_EXPENSIVE_ASSERTIONS
        -- Not so much expensive, as doubly impossible.
        assert (aid `notElem` l `blame` "actor already added"
                                `swith` (aid, body, l))
#endif
        (Just $ aid : l)
  let h Nothing = Just aid
      h (Just aid2) = error $ "an actor already present there"
                              `showFailure` (aid, body, aid2)
  updateLevel (blid body) $ if bproj body
                            then updateProjMap (EM.alter g (bpos body))
                            else updateBigMap (EM.alter h (bpos body))
  actorMaxSk <- getsState $ maxSkillsFromActor body
  modifyState $ updateActorMaxSkills $ EM.insert aid actorMaxSk

updLoseActor :: MonadStateWrite m => ActorId -> Actor ->  m ()
updLoseActor aid body = do
  -- Remove actor from @sactorD@.
  let f Nothing = error $ "actor already removed" `showFailure` (aid, body)
      f (Just b) = assert (b == body `blame` "inconsistent destroyed actor body"
                                     `swith` (aid, body, b)) Nothing
  modifyState $ updateActorD $ EM.alter f aid
  let g Nothing = error $ "actor already removed" `showFailure` (aid, body)
      g (Just l) =
#ifdef WITH_EXPENSIVE_ASSERTIONS
        -- Not so much expensive, as doubly impossible.
        assert (aid `elem` l `blame` "actor already removed"
                             `swith` (aid, body, l))
#endif
        (let l2 = delete aid l
         in if null l2 then Nothing else Just l2)
  let h Nothing = error $ "actor already removed" `showFailure` (aid, body)
      h (Just _aid2) =
#ifdef WITH_EXPENSIVE_ASSERTIONS
        -- Not so much expensive, as doubly impossible.
        assert (aid == _aid2 `blame` "actor already removed"
                             `swith` (aid, body, _aid2))
#endif
        Nothing
  updateLevel (blid body) $ if bproj body
                            then updateProjMap (EM.alter g (bpos body))
                            else updateBigMap (EM.alter h (bpos body))
  modifyState $ updateActorMaxSkills $ EM.delete aid

updSpotItem :: MonadStateWrite m => ItemId -> ItemQuant -> Container -> m ()
updSpotItem iid kit@(k, _) c = do
  item <- getsState $ getItemBody iid
  when (k > 0) $ do
    insertItemContainer iid kit c
    case c of
      CActor aid store -> when (store `elem` [CEqp, COrgan])
                          $ addItemToActorMaxSkills iid item k aid
      _ -> return ()

updLoseItem :: MonadStateWrite m => ItemId -> ItemQuant -> Container -> m ()
updLoseItem iid kit@(k, _) c = assert (k > 0) $ do
  item <- getsState $ getItemBody iid
  deleteItemContainer iid kit c
  case c of
    CActor aid store -> when (store `elem` [CEqp, COrgan])
                        $ addItemToActorMaxSkills iid item (-k) aid
    _ -> return ()

updSpotItemBag :: MonadStateWrite m => Container -> ItemBag -> m ()
updSpotItemBag c bag =
  -- The case of empty bag is for a hack to help identifying sample items.
  when (not $ EM.null bag) $ do
    insertBagContainer bag c
    case c of
      CActor aid store ->
        when (store `elem` [CEqp, COrgan]) $ do
          itemD <- getsState sitemD
          let ais = map (\iid -> (iid, itemD EM.! iid)) $ EM.keys bag
          forM_ ais $ \(iid, item) ->
            addItemToActorMaxSkills iid item (fst $ bag EM.! iid) aid
      _ -> return ()

updLoseItemBag :: MonadStateWrite m => Container -> ItemBag -> m ()
updLoseItemBag c bag = assert (EM.size bag > 0) $ do
  deleteBagContainer bag c
  -- Do not remove the items from @sitemD@ nor from @sitemRev@,
  -- It's incredibly costly and not noticeable for the player.
  -- However, assert the items are registered in @sitemD@.
  case c of
    CActor aid store ->
      when (store `elem` [CEqp, COrgan]) $ do
        itemD <- getsState sitemD
        let ais = map (\iid -> (iid, itemD EM.! iid)) $ EM.keys bag
        forM_ ais $ \(iid, item) ->
          addItemToActorMaxSkills iid item (- (fst $ bag EM.! iid)) aid
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

updWaitActor :: MonadStateWrite m
             => ActorId -> Watchfulness -> Watchfulness -> m ()
updWaitActor aid fromWS toWS = assert (fromWS /= toWS) $ do
  body <- getsState $ getActorBody aid
  let !_A = assert (fromWS == bwatch body
                    `blame` "unexpected actor wait state"
                    `swith` (aid, fromWS, bwatch body, body)) ()
  updateActor aid $ \b -> b {bwatch = toWS}

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
  swapActorMap source sbody target tbody

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
        addItemToActorMaxSkills iid itemBase (-k) aid
    COrgan -> case s2 of
      CEqp -> return ()
      _ -> do
        itemBase <- getsState $ getItemBody iid
        addItemToActorMaxSkills iid itemBase (-k) aid
    _ ->
      when (s2 `elem` [CEqp, COrgan]) $ do
        itemBase <- getsState $ getItemBody iid
        addItemToActorMaxSkills iid itemBase k aid

updRefillHP :: MonadStateWrite m => ActorId -> Int64 -> m ()
updRefillHP aid nRaw =
  updateActor aid $ \b ->
    -- Make rescue easier by not going into negative HP the first time.
    let newRawHP = bhp b + nRaw
        newHP = if bhp b <= 0 then newRawHP else max 0 newRawHP
        n = newHP - bhp b
    in b { bhp = newHP
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
               => FactionId -> Maybe Status -> Maybe Status
               -> m ()
updQuitFaction fid fromSt toSt = do
  let !_A = assert (fromSt /= toSt `blame` (fid, fromSt, toSt)) ()
  fact <- getsState $ (EM.! fid) . sfactionD
  let !_A = assert (fromSt == gquit fact
                    `blame` "unexpected actor quit status"
                    `swith` (fid, fromSt, toSt, fact)) ()
  let adj fa = fa {gquit = toSt}
  updateFaction fid adj

updSpotStashFaction :: MonadStateWrite m
                    => FactionId -> LevelId -> Point -> m ()
updSpotStashFaction fid lid pos = do
  let adj fa = fa {gstash = Just (lid, pos)}
    -- the stash may be outdated, but not empty and it's correct,
    -- because we know stash may be only one, so here it's added,
    -- the old one is removed, despite us not seeing its location;
    -- warning: in this form, this is not reversible, no undo,
    -- so we'd need to add the required @UpdLoseStashFaction@
    -- elsehwere, similarly as @LoseTile@ is added when FOV
    -- reveals that tile is different than expected
  updateFaction fid adj

updLoseStashFaction :: MonadStateWrite m
                    => FactionId -> LevelId -> Point -> m ()
updLoseStashFaction fid lid pos = do
  let adj fa = assert (gstash fa == Just (lid, pos)
                       `blame` "unexpected lack of gstash"
                       `swith` (fid, lid, pos, fa))
               $ fa {gstash = Nothing}
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
                    `swith` (fid, source, target, mtb, fact)) ()
  let adj fa = fa {_gleader = target}
  updateFaction fid adj

updDiplFaction :: MonadStateWrite m
               => FactionId -> FactionId -> Diplomacy -> Diplomacy -> m ()
updDiplFaction fid1 fid2 fromDipl toDipl =
  assert (fid1 /= fid2 && fromDipl /= toDipl) $ do
    fact1 <- getsState $ (EM.! fid1) . sfactionD
    fact2 <- getsState $ (EM.! fid2) . sfactionD
    let !_A =
          assert (fromDipl == EM.findWithDefault Unknown fid2 (gdipl fact1)
                  && fromDipl == EM.findWithDefault Unknown fid1 (gdipl fact2)
                  `blame` "unexpected actor diplomacy status"
                  `swith` (fid1, fid2, fromDipl, toDipl, fact1, fact2)) ()
    let adj fid fact = fact {gdipl = EM.insert fid toDipl (gdipl fact)}
    updateFaction fid1 (adj fid2)
    updateFaction fid2 (adj fid1)

updDoctrineFaction :: MonadStateWrite m
                   => FactionId -> Ability.Doctrine -> Ability.Doctrine -> m ()
updDoctrineFaction fid toT fromT = do
  let adj fact =
        let player = gplayer fact
        in assert (fdoctrine player == fromT)
           $ fact {gplayer = player {fdoctrine = toT}}
  updateFaction fid adj

updAutoFaction :: MonadStateWrite m => FactionId -> Bool -> m ()
updAutoFaction fid st =
  updateFaction fid (\fact ->
    assert (isAIFact fact == not st)
    $ fact {gplayer = automatePlayer st (gplayer fact)})

-- Record a given number (usually just 1, or -1 for undo) of actor kills
-- for score calculation.
updRecordKill :: MonadStateWrite m
              => ActorId -> ContentId ItemKind -> Int -> m ()
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

-- Alter an attribute (actually, the only, the defining attribute)
-- of a visible tile. This is similar to e.g., @UpdTrajectory@.
--
-- Removing and creating embedded items when altering a tile
-- is done separately via @UpdCreateItem@ and @UpdDestroyItem@.
updAlterTile :: MonadStateWrite m
             => LevelId -> Point -> ContentId TileKind -> ContentId TileKind
             -> m ()
updAlterTile lid p fromTile toTile = assert (fromTile /= toTile) $ do
  COps{coTileSpeedup} <- getsState scops
  lvl <- getLevel lid
  let t = lvl `at` p
  if t /= fromTile
  then atomicFail "terrain to modify is different than assumed"
  else do
    let adj ts = ts PointArray.// [(p, toTile)]
    updateLevel lid $ updateTile adj
    case ( Tile.isExplorable coTileSpeedup fromTile
         , Tile.isExplorable coTileSpeedup toTile ) of
      (False, True) -> updateLevel lid $ \lvl2 -> lvl2 {lseen = lseen lvl2 + 1}
      (True, False) -> updateLevel lid $ \lvl2 -> lvl2 {lseen = lseen lvl2 - 1}
      _ -> return ()

updAlterExplorable :: MonadStateWrite m => LevelId -> Int -> m ()
updAlterExplorable lid delta = assert (delta /= 0) $
  updateLevel lid $ \lvl -> lvl {lexpl = lexpl lvl + delta}

updAlterGold :: MonadStateWrite m => Int -> m ()
updAlterGold delta = assert (delta /= 0) $
  modifyState $ updateGold (+ delta)

-- Showing to the client the embedded items, if any, is done elsewhere.
updSearchTile :: MonadStateWrite m
              => ActorId -> Point -> ContentId TileKind -> m ()
updSearchTile aid p toTile = do
  COps{cotile} <- getsState scops
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  let t = lvl `at` p
  if t == toTile
  then atomicFail "tile already searched"
  else assert (Just t == Tile.hideAs cotile toTile) $ do
    updLoseTile (blid b) [(p, t)]
    updSpotTile (blid b) [(p, toTile)]  -- not the hidden version this one time

-- Notice previously invisible tiles. This is done in bulk,
-- because it often involves dozens of tiles per move.
-- We verify that the old tiles at the positions in question
-- are indeed unknown.
updSpotTile :: MonadStateWrite m
            => LevelId -> [(Point, ContentId TileKind)] -> m ()
updSpotTile lid ts = assert (not $ null ts) $ do
  COps{coTileSpeedup} <- getsState scops
  let unk tileMap (p, _) = tileMap PointArray.! p == unknownId
      adj tileMap = assert (all (unk tileMap) ts) $ tileMap PointArray.// ts
  updateLevel lid $ updateTile adj
  let f (_, t1) = when (Tile.isExplorable coTileSpeedup t1) $
        updateLevel lid $ \lvl -> lvl {lseen = lseen lvl + 1}
  mapM_ f ts

-- Stop noticing previously visible tiles. It verifies
-- the state of the tiles before wiping them out.
updLoseTile :: MonadStateWrite m
            => LevelId -> [(Point, ContentId TileKind)] -> m ()
updLoseTile lid ts = assert (not $ null ts) $ do
  COps{coTileSpeedup} <- getsState scops
  let matches tileMap (p, ov) = tileMap PointArray.! p == ov
      tu = map (second (const unknownId)) ts
      adj tileMap = assert (all (matches tileMap) ts) $ tileMap PointArray.// tu
  updateLevel lid $ updateTile adj
  let f (_, t1) = when (Tile.isExplorable coTileSpeedup t1) $
        updateLevel lid $ \lvl -> lvl {lseen = lseen lvl - 1}
  mapM_ f ts

updSpotEntry :: MonadStateWrite m => LevelId -> [(Point, PK.PlaceEntry)] -> m ()
updSpotEntry lid ts = assert (not $ null ts) $ do
  let alt en Nothing = Just en
      alt en (Just oldEn) = atomicFail $ "entry already added"
                                         `showFailure` (lid, ts, en, oldEn)
      f (p, en) = EM.alter (alt en) p
      upd m = foldr f m ts
  updateLevel lid $ updateEntry upd

updLoseEntry :: MonadStateWrite m => LevelId -> [(Point, PK.PlaceEntry)] -> m ()
updLoseEntry lid ts = assert (not $ null ts) $ do
  let alt en Nothing = error $ "entry already removed"
                               `showFailure` (lid, ts, en)
      alt en (Just oldEn) =
        assert (en == oldEn `blame` "unexpected lost entry"
                            `swith` (lid, ts, en, oldEn)) Nothing
      f (p, en) = EM.alter (alt en) p
      upd m = foldr f m ts
  updateLevel lid $ updateEntry upd

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
            => ItemId -> Container -> ItemTimers -> ItemTimers
            -> m ()
updTimeItem iid c fromIt toIt = assert (fromIt /= toIt) $ do
  bag <- getsState $ getContainerBag c
  case iid `EM.lookup` bag of
    Just (k, it) -> do
      let !_A1 = assert (fromIt == it `blame` (k, it, iid, c, fromIt, toIt)) ()
          !_A2 = assert (length toIt <= k `blame` (k, toIt, iid, c, fromIt)) ()
      deleteItemContainer iid (k, fromIt) c
      insertItemContainer iid (k, toIt) c
    Nothing -> error $ "" `showFailure` (bag, iid, c, fromIt, toIt)

updAgeGame :: MonadStateWrite m => ES.EnumSet LevelId -> m ()
updAgeGame lids = do
  modifyState $ updateTime $ flip timeShift (Delta timeClip)
  mapM_ (ageLevel (Delta timeClip)) $ ES.toList lids

updUnAgeGame :: MonadStateWrite m => ES.EnumSet LevelId -> m ()
updUnAgeGame lids = do
  modifyState $ updateTime $ flip timeShift (timeDeltaReverse $ Delta timeClip)
  mapM_ (ageLevel (timeDeltaReverse $ Delta timeClip)) $ ES.toList lids

ageLevel :: MonadStateWrite m => Delta Time -> LevelId -> m ()
ageLevel delta lid =
  updateLevel lid $ \lvl -> lvl {ltime = timeShift (ltime lvl) delta}

updDiscover :: MonadStateWrite m
            => Container -> ItemId -> ContentId ItemKind -> IA.AspectRecord
            -> m ()
updDiscover _c iid ik arItem = do
  itemD <- getsState sitemD
  COps{coItemSpeedup} <- getsState scops
  let kmIsConst = IA.kmConst $ getKindMean ik coItemSpeedup
  discoKind <- getsState sdiscoKind
  let discoverAtMostAspect = do
        discoAspect <- getsState sdiscoAspect
        if kmIsConst || iid `EM.member` discoAspect
        then atomicFail "item already fully discovered"
        else discoverAspect iid arItem
  case EM.lookup iid itemD of
    Nothing -> atomicFail "discovered item unheard of"
    Just item -> case jkind item of
      IdentityObvious _ -> discoverAtMostAspect
      IdentityCovered ix _ik -> case EM.lookup ix discoKind of
        Just{} -> discoverAtMostAspect
        Nothing -> do
          discoverKind ix ik
          unless kmIsConst $ discoverAspect iid arItem
  resetActorMaxSkills

updCover :: Container -> ItemId -> ContentId ItemKind -> IA.AspectRecord -> m ()
updCover _c _iid _ik _arItem = undefined

updDiscoverKind :: MonadStateWrite m
                => Container -> ItemKindIx -> ContentId ItemKind -> m ()
updDiscoverKind _c ix kmKind = do
  discoKind <- getsState sdiscoKind
  if ix `EM.member` discoKind
  then atomicFail "item kind already discovered"
  else do
    discoverKind ix kmKind
    resetActorMaxSkills

discoverKind :: MonadStateWrite m => ItemKindIx -> ContentId ItemKind -> m ()
discoverKind ix kindId = do
  let f Nothing = Just kindId
      f Just{} = error $ "already discovered" `showFailure` (ix, kindId)
  modifyState $ updateDiscoKind $ \discoKind1 ->
    EM.alter f ix discoKind1

updCoverKind :: Container -> ItemKindIx -> ContentId ItemKind -> m ()
updCoverKind _c _ix _ik = undefined

updDiscoverAspect :: MonadStateWrite m
                  => Container -> ItemId -> IA.AspectRecord -> m ()
updDiscoverAspect _c iid arItem = do
  COps{coItemSpeedup} <- getsState scops
  itemD <- getsState sitemD
  case EM.lookup iid itemD of
    Nothing -> atomicFail "discovered item unheard of"
    Just item -> do
      -- Here the kind information is exact, hence @getItemKindIdServer@.
      kindId <- getsState $ getItemKindIdServer item
      discoAspect <- getsState sdiscoAspect
      let kmIsConst = IA.kmConst $ getKindMean kindId coItemSpeedup
      if kmIsConst || iid `EM.member` discoAspect
      then atomicFail "item arItem already discovered"
      else do
        discoverAspect iid arItem
        resetActorMaxSkills

discoverAspect :: MonadStateWrite m => ItemId -> IA.AspectRecord -> m ()
discoverAspect iid arItem = do
  let f Nothing = Just arItem
      f Just{} = error $ "already discovered" `showFailure` (iid, arItem)
  -- At this point we know the item is not @kmConst@.
  modifyState $ updateDiscoAspect $ \discoAspect1 ->
    EM.alter f iid discoAspect1

updCoverAspect :: Container -> ItemId -> IA.AspectRecord -> m ()
updCoverAspect _c _iid _arItem = undefined

updDiscoverServer :: MonadStateWrite m => ItemId -> IA.AspectRecord -> m ()
updDiscoverServer iid arItem =
  modifyState $ updateDiscoAspect $ \discoAspect1 ->
    EM.insert iid arItem discoAspect1

updCoverServer :: MonadStateWrite m => ItemId -> IA.AspectRecord -> m ()
updCoverServer iid arItem =
  modifyState $ updateDiscoAspect $ \discoAspect1 ->
    assert (discoAspect1 EM.! iid == arItem)
    $ EM.delete iid discoAspect1

updRestart :: MonadStateWrite m => State -> m ()
updRestart = putState

updRestartServer :: MonadStateWrite m => State -> m ()
updRestartServer = putState

updResumeServer :: MonadStateWrite m => State -> m ()
updResumeServer = putState
