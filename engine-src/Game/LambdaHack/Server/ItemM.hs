-- | Server operations for items.
module Game.LambdaHack.Server.ItemM
  ( registerItem, moveStashIfNeeded, randomResetTimeout, embedItemOnPos
  , prepareItemKind, rollItemAspect, rollAndRegisterItem
  , placeItemsInDungeon, embedItemsInDungeon, mapActorCStore_
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , onlyRegisterItem, computeRndTimeout, createCaveItem, createEmbedItem
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.HashMap.Strict as HM

import           Game.LambdaHack.Atomic
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Content.CaveKind (citemFreq, citemNum)
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Core.Dice as Dice
import           Game.LambdaHack.Core.Frequency
import           Game.LambdaHack.Core.Random
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Server.ItemRev
import           Game.LambdaHack.Server.MonadServer
import           Game.LambdaHack.Server.ServerOptions
import           Game.LambdaHack.Server.State

onlyRegisterItem :: MonadServerAtomic m => ItemKnown -> m ItemId
onlyRegisterItem itemKnown@(ItemKnown _ arItem _) = do
  itemRev <- getsServer sitemRev
  case HM.lookup itemKnown itemRev of
    Just iid -> return iid
    Nothing -> do
      icounter <- getsServer sicounter
      executedOnServer <-
        execUpdAtomicSer $ UpdDiscoverServer icounter arItem
      let !_A = assert executedOnServer ()
      modifyServer $ \ser ->
        ser { sitemRev = HM.insert itemKnown icounter (sitemRev ser)
            , sicounter = succ icounter }
      return $! icounter

registerItem :: MonadServerAtomic m
             => Bool -> ItemFullKit -> ItemKnown -> Container -> m ItemId
registerItem verbose (itemFull@ItemFull{itemBase, itemKindId, itemKind}, kit)
             itemKnown@(ItemKnown _ arItem _) containerRaw = do
  container <- case containerRaw of
    CActor aid CEqp -> do
      b <- getsState $ getActorBody aid
      return $! if eqpFreeN b >= fst kit
                then containerRaw
                else CActor aid CStash
    _ -> return containerRaw
  iid <- onlyRegisterItem itemKnown
  let slore = IA.loreFromContainer arItem container
  modifyServer $ \ser ->
    ser {sgenerationAn = EM.adjust (EM.insertWith (+) iid (fst kit)) slore
                                   (sgenerationAn ser)}
  moveStash <- moveStashIfNeeded container
  mapM_ execUpdAtomic moveStash
  execUpdAtomic $ UpdCreateItem verbose iid itemBase kit container
  let worth = itemPrice (fst kit) itemKind
  case container of
    _ | worth == 0 -> return ()
    CActor _ COrgan -> return ()  -- destroyed on drop
    CTrunk{} -> return ()  -- we assume any valuables in CEmbed can be dug out
    _ -> execUpdAtomic $ UpdAlterGold worth
  knowItems <- getsServer $ sknowItems . soptions
  when knowItems $ case container of
    CTrunk{} -> return ()
    _ -> execUpdAtomic $ UpdDiscover container iid itemKindId arItem
  -- The first recharging period after creation is random,
  -- between 1 and 2 standard timeouts of the item.
  -- In this way we avoid many rattlesnakes rattling in unison.
  case container of
    CActor _ cstore | cstore `elem` [CEqp, COrgan] ->
      randomResetTimeout (fst kit) iid itemFull [] container
    _ -> return ()
  return iid

moveStashIfNeeded :: MonadStateRead m => Container -> m [UpdAtomic]
moveStashIfNeeded c = case c of
  CActor aid CStash -> do
    b <- getsState $ getActorBody aid
    mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid b
    case mstash of
      Just (lid, pos) -> do
        bagStash <- getsState $ getFloorBag lid pos
        return $! if EM.null bagStash
                  then [ UpdLoseStashFaction False (bfid b) lid pos
                       , UpdSpotStashFaction True (bfid b) (blid b) (bpos b) ]
                  else []
      Nothing -> return [UpdSpotStashFaction True (bfid b) (blid b) (bpos b)]
  _ -> return []

randomResetTimeout :: MonadServerAtomic m
                   => Int -> ItemId -> ItemFull -> [ItemTimer] -> Container
                   -> m ()
randomResetTimeout k iid itemFull beforeIt toC = do
  lid <- getsState $ lidFromC toC
  localTime <- getsState $ getLocalTime lid
  mrndTimeout <- rndToAction $ computeRndTimeout localTime itemFull
  -- The created or moved item set (not the items previously at destination)
  -- has its timeouts reset to a random value between timeout and twice timeout.
  -- This prevents micromanagement via swapping items in and out of eqp
  -- and via exact prediction of first timeout after equip.
  case mrndTimeout of
    Just rndT -> do
      bagAfter <- getsState $ getContainerBag toC
      let afterIt = case iid `EM.lookup` bagAfter of
            Nothing -> error $ "" `showFailure` (iid, bagAfter, toC)
            Just (_, it2) -> it2
          resetIt = beforeIt ++ replicate k rndT
      when (afterIt /= resetIt) $
        execUpdAtomic $ UpdTimeItem iid toC afterIt resetIt
    Nothing -> return ()  -- no @Timeout@ aspect; don't touch

computeRndTimeout :: Time -> ItemFull -> Rnd (Maybe ItemTimer)
computeRndTimeout localTime ItemFull{itemDisco=ItemDiscoFull itemAspect} = do
  let t = IA.aTimeout itemAspect
  if t > 0 then do
    rndT <- randomR0 t
    let rndTurns = timeDeltaScale (Delta timeTurn) (t + rndT)
    return $ Just $ createItemTimer localTime rndTurns
  else return Nothing
computeRndTimeout _ _ = error "computeRndTimeout: server ignorant about an item"

createCaveItem :: MonadServerAtomic m => Point -> LevelId -> m ()
createCaveItem pos lid = do
  COps{cocave} <- getsState scops
  Level{lkind, ldepth} <- getLevel lid
  let container = CFloor lid pos
      litemFreq = citemFreq $ okind cocave lkind
  -- Power depth of new items unaffected by number of spawned actors.
  freq <- prepareItemKind 0 ldepth litemFreq
  mIidEtc <- rollAndRegisterItem True ldepth freq container Nothing
  createKitItems lid pos mIidEtc

createEmbedItem :: MonadServerAtomic m
                => LevelId -> Point -> GroupName ItemKind -> m ()
createEmbedItem lid pos grp = do
  Level{ldepth} <- getLevel lid
  let container = CEmbed lid pos
  -- Power depth of new items unaffected by number of spawned actors.
  freq <- prepareItemKind 0 ldepth [(grp, 1)]
  mIidEtc <- rollAndRegisterItem True ldepth freq container Nothing
  createKitItems lid pos mIidEtc

-- Create, register and insert all initial kit items.
createKitItems :: MonadServerAtomic m
               => LevelId -> Point -> Maybe (ItemId, ItemFullKit) -> m ()
createKitItems lid pos mIidEtc = case mIidEtc of
  Nothing -> error $ "" `showFailure` (lid, pos, mIidEtc)
  Just (_, (itemFull, _)) -> do
    cops <- getsState scops
    lvl@Level{ldepth} <- getLevel lid
    let ikit = IK.ikit $ itemKind itemFull
        nearbyPassable = take (20 + length ikit)
                         $ nearbyPassablePoints cops lvl pos
        walkable p = Tile.isWalkable (coTileSpeedup cops) (lvl `at` p)
        good p = walkable p && p `EM.notMember` lfloor lvl
        kitPos = zip ikit $ filter good nearbyPassable
                            ++ filter walkable nearbyPassable
                            ++ repeat pos
    forM_ kitPos $ \((ikGrp, cstore), p) -> do
      let container = if cstore == CGround
                      then CFloor lid p
                      else CEmbed lid pos
          itemFreq = [(ikGrp, 1)]
      -- Power depth of new items unaffected by number of spawned actors.
      freq <- prepareItemKind 0 ldepth itemFreq
      mresult <- rollAndRegisterItem False ldepth freq container Nothing
      assert (isJust mresult) $ return ()

-- Tiles already placed, so it's possible to scatter companion items
-- over walkable tiles.
embedItemOnPos :: MonadServerAtomic m
               => LevelId -> Point -> ContentId TileKind -> m ()
embedItemOnPos lid pos tk = do
  COps{cotile} <- getsState scops
  let embedGroups = Tile.embeddedItems cotile tk
  mapM_ (createEmbedItem lid pos) embedGroups

prepareItemKind :: MonadServerAtomic m
                => Int -> Dice.AbsDepth -> Freqs ItemKind
                -> m (Frequency
                        (GroupName ItemKind, ContentId IK.ItemKind, ItemKind))
prepareItemKind lvlSpawned ldepth itemFreq = do
  cops <- getsState scops
  uniqueSet <- getsServer suniqueSet
  totalDepth <- getsState stotalDepth
  return $! newItemKind cops uniqueSet itemFreq ldepth totalDepth lvlSpawned

rollItemAspect :: MonadServerAtomic m
               => Frequency
                    (GroupName ItemKind, ContentId IK.ItemKind, ItemKind)
               -> Dice.AbsDepth
               -> m NewItem
rollItemAspect freq ldepth = do
  cops <- getsState scops
  flavour <- getsServer sflavour
  discoRev <- getsServer sdiscoKindRev
  totalDepth <- getsState stotalDepth
  m2 <- rndToAction $ newItem cops freq flavour discoRev ldepth totalDepth
  case m2 of
    NewItem _ (ItemKnown _ arItem _) ItemFull{itemKindId} _ -> do
      when (IA.checkFlag Ability.Unique arItem) $
        modifyServer $ \ser ->
          ser {suniqueSet = ES.insert itemKindId (suniqueSet ser)}
    NoNewItem -> return ()
  return m2

rollAndRegisterItem :: MonadServerAtomic m
                    => Bool
                    -> Dice.AbsDepth
                    -> Frequency
                         (GroupName ItemKind, ContentId IK.ItemKind, ItemKind)
                    -> Container
                    -> Maybe Int
                    -> m (Maybe (ItemId, ItemFullKit))
rollAndRegisterItem verbose ldepth freq container mk = do
  m2 <- rollItemAspect freq ldepth
  case m2 of
    NoNewItem -> return Nothing
    NewItem _ itemKnown itemFull kit -> do
      let f k = if k == 1 && null (snd kit)
                then quantSingle
                else (k, snd kit)
          !kit2 = maybe kit f mk
      iid <- registerItem verbose (itemFull, kit2) itemKnown container
      return $ Just (iid, (itemFull, kit2))

-- Tiles already placed, so it's possible to scatter over walkable tiles.
placeItemsInDungeon :: forall m. MonadServerAtomic m
                    => EM.EnumMap LevelId (EM.EnumMap FactionId Point) -> m ()
placeItemsInDungeon factionPositions = do
  COps{cocave, coTileSpeedup} <- getsState scops
  totalDepth <- getsState stotalDepth
  let initialItems (lid, lvl@Level{lkind, ldepth}) = do
        litemNum <- rndToAction $ castDice ldepth totalDepth
                                  (citemNum $ okind cocave lkind)
        let alPos = EM.elems $ EM.findWithDefault EM.empty lid factionPositions
            placeItems :: Int -> m ()
            placeItems n | n == litemNum = return ()
            placeItems !n = do
              Level{lfloor} <- getLevel lid
              -- Don't generate items around initial actors or in bunches.
              let distAndNotFloor !p _ =
                    let f !k b = chessDist p k > 4 && b
                    in p `EM.notMember` lfloor && foldr f True alPos
              mpos <- rndToAction $ findPosTry2 20 lvl
                (\_ !t -> Tile.isWalkable coTileSpeedup t
                          && not (Tile.isNoItem coTileSpeedup t))
                [ \_ !t -> Tile.isVeryOftenItem coTileSpeedup t
                , \_ !t -> Tile.isCommonItem coTileSpeedup t ]
                distAndNotFloor
                [distAndNotFloor, distAndNotFloor]
              case mpos of
                Just pos -> do
                  createCaveItem pos lid
                  placeItems (n + 1)
                Nothing -> debugPossiblyPrint
                  "Server: placeItemsInDungeon: failed to find positions"
        placeItems 0
  dungeon <- getsState sdungeon
  -- Make sure items on easy levels are generated first, to avoid all
  -- artifacts on deep levels.
  let fromEasyToHard = sortBy (comparing (ldepth . snd)) $ EM.assocs dungeon
  mapM_ initialItems fromEasyToHard

-- Tiles already placed, so it's possible to scatter companion items
-- over walkable tiles.
embedItemsInDungeon :: MonadServerAtomic m => m ()
embedItemsInDungeon = do
  let embedItemsOnLevel (lid, Level{ltile}) =
        PointArray.imapMA_ (embedItemOnPos lid) ltile
  dungeon <- getsState sdungeon
  -- Make sure items on easy levels are generated first, to avoid all
  -- artifacts on deep levels.
  let fromEasyToHard = sortBy (comparing (ldepth . snd)) $ EM.assocs dungeon
  mapM_ embedItemsOnLevel fromEasyToHard

-- | Mapping over actor's items from a give store.
mapActorCStore_ :: MonadServer m
                => CStore -> (ItemId -> ItemQuant -> m ()) -> Actor -> m ()
mapActorCStore_ cstore f b = do
  bag <- getsState $ getBodyStoreBag b cstore
  mapM_ (uncurry f) $ EM.assocs bag
