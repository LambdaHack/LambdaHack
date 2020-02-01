-- | Server operations for items.
module Game.LambdaHack.Server.ItemM
  ( registerItem, moveStashIfNeeded, randomResetTimeout, embedItem
  , prepareItemKind, rollItemAspect, rollAndRegisterItem
  , placeItemsInDungeon, embedItemsInDungeon, mapActorCStore_
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , onlyRegisterItem, computeRndTimeout, createLevelItem
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Function
import qualified Data.HashMap.Strict as HM
import           Data.Ord

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
      return $! if eqpFreeN b >= fst kit then containerRaw else CActor aid CStash
    _ -> return containerRaw
  iid <- onlyRegisterItem itemKnown
  let slore = IA.loreFromContainer arItem container
  modifyServer $ \ser ->
    ser {sgenerationAn = EM.adjust (EM.insertWith (+) iid (fst kit)) slore
                                   (sgenerationAn ser)}
  moveStash <- moveStashIfNeeded container
  mapM_  execUpdAtomic moveStash
  execUpdAtomic $ UpdCreateItem verbose iid itemBase kit container
  let worth = itemPrice (fst kit) itemKind
  unless (worth == 0) $ execUpdAtomic $ UpdAlterGold worth
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
                   => Int -> ItemId -> ItemFull -> [Time] -> Container
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

computeRndTimeout :: Time -> ItemFull -> Rnd (Maybe Time)
computeRndTimeout localTime ItemFull{itemDisco=ItemDiscoFull itemAspect} = do
  let t = IA.aTimeout itemAspect
  if t /= 0 then do
    rndT <- randomR0 t
    let rndTurns = timeDeltaScale (Delta timeTurn) (t + rndT)
    return $ Just $ timeShift localTime rndTurns
  else return Nothing
computeRndTimeout _ _ = error "computeRndTimeout: server ignorant about an item"

createLevelItem :: MonadServerAtomic m => Point -> LevelId -> m ()
createLevelItem pos lid = do
  COps{cocave} <- getsState scops
  Level{lkind} <- getLevel lid
  let container = CFloor lid pos
      litemFreq = citemFreq $ okind cocave lkind
  void $ rollAndRegisterItem True lid litemFreq container Nothing

embedItem :: MonadServerAtomic m
          => LevelId -> Point -> ContentId TileKind -> m ()
embedItem lid pos tk = do
  COps{cotile} <- getsState scops
  let embeds = Tile.embeddedItems cotile tk
      container = CEmbed lid pos
      f grp = rollAndRegisterItem True lid [(grp, 1)] container Nothing
  mapM_ f embeds

prepareItemKind :: MonadServerAtomic m
                => Int -> LevelId -> Freqs ItemKind
                -> m (Frequency (ContentId IK.ItemKind, ItemKind))
prepareItemKind lvlSpawned lid itemFreq = do
  cops <- getsState scops
  uniqueSet <- getsServer suniqueSet
  totalDepth <- getsState stotalDepth
  Level{ldepth} <- getLevel lid
  return $! newItemKind cops uniqueSet itemFreq ldepth totalDepth lvlSpawned

rollItemAspect :: MonadServerAtomic m
               => Frequency (ContentId IK.ItemKind, ItemKind) -> LevelId
               -> m (Maybe (ItemKnown, ItemFullKit))
rollItemAspect freq lid = do
  cops <- getsState scops
  flavour <- getsServer sflavour
  discoRev <- getsServer sdiscoKindRev
  totalDepth <- getsState stotalDepth
  Level{ldepth} <- getLevel lid
  m2 <- rndToAction $ newItem cops freq flavour discoRev ldepth totalDepth
  case m2 of
    Just (itemKnown, ifk@(itemFull@ItemFull{itemKindId}, _)) -> do
      let arItem = aspectRecordFull itemFull
      when (IA.checkFlag Ability.Unique arItem) $
        modifyServer $ \ser ->
          ser {suniqueSet = ES.insert itemKindId (suniqueSet ser)}
      return $ Just (itemKnown, ifk)
    Nothing -> return Nothing

rollAndRegisterItem :: MonadServerAtomic m
                    => Bool -> LevelId -> Freqs ItemKind -> Container
                    -> Maybe Int
                    -> m (Maybe (ItemId, ItemFullKit))
rollAndRegisterItem verbose lid itemFreq container mk = do
  -- Power depth of new items unaffected by number of spawned actors.
  freq <- prepareItemKind 0 lid itemFreq
  m2 <- rollItemAspect freq lid
  case m2 of
    Nothing -> return Nothing
    Just (itemKnown, (itemFull, kit)) -> do
      let kit2 = (fromMaybe (fst kit) mk, snd kit)
      iid <- registerItem verbose (itemFull, kit2) itemKnown container
      return $ Just (iid, (itemFull, kit2))

placeItemsInDungeon :: forall m. MonadServerAtomic m
                    => EM.EnumMap LevelId [Point] -> m ()
placeItemsInDungeon alliancePositions = do
  COps{cocave, coTileSpeedup} <- getsState scops
  totalDepth <- getsState stotalDepth
  let initialItems (lid, lvl@Level{lkind, ldepth}) = do
        litemNum <- rndToAction $ castDice ldepth totalDepth
                                  (citemNum $ okind cocave lkind)
        let alPos = EM.findWithDefault [] lid alliancePositions
            placeItems :: Int -> m ()
            placeItems n | n == litemNum = return ()
            placeItems !n = do
              Level{lfloor} <- getLevel lid
              -- Don't generate items around initial actors or in bunches.
              let distAllianceAndNotFloor !p _ =
                    let f !k b = chessDist p k > 4 && b
                    in p `EM.notMember` lfloor && foldr f True alPos
              mpos <- rndToAction $ findPosTry2 20 lvl
                (\_ !t -> Tile.isWalkable coTileSpeedup t
                          && not (Tile.isNoItem coTileSpeedup t))
                [ \_ !t -> Tile.isVeryOftenItem coTileSpeedup t
                , \_ !t -> Tile.isCommonItem coTileSpeedup t ]
                distAllianceAndNotFloor
                [ distAllianceAndNotFloor
                , distAllianceAndNotFloor ]
              case mpos of
                Just pos -> do
                  createLevelItem pos lid
                  placeItems (n + 1)
                Nothing -> debugPossiblyPrint
                  "Server: placeItemsInDungeon: failed to find positions"
        placeItems 0
  dungeon <- getsState sdungeon
  -- Make sure items on easy levels are generated first, to avoid all
  -- artifacts on deep levels.
  let absLid = abs . fromEnum
      fromEasyToHard = sortBy (comparing absLid `on` fst) $ EM.assocs dungeon
  mapM_ initialItems fromEasyToHard

embedItemsInDungeon :: MonadServerAtomic m => m ()
embedItemsInDungeon = do
  let embedItems (lid, Level{ltile}) = PointArray.imapMA_ (embedItem lid) ltile
  dungeon <- getsState sdungeon
  -- Make sure items on easy levels are generated first, to avoid all
  -- artifacts on deep levels.
  let absLid = abs . fromEnum
      fromEasyToHard = sortBy (comparing absLid `on` fst) $ EM.assocs dungeon
  mapM_ embedItems fromEasyToHard

-- | Mapping over actor's items from a give store.
mapActorCStore_ :: MonadServer m
                => CStore -> (ItemId -> ItemQuant -> m a) -> Actor -> m ()
mapActorCStore_ cstore f b = do
  bag <- getsState $ getBodyStoreBag b cstore
  mapM_ (uncurry f) $ EM.assocs bag
