-- | Server operations for items.
module Game.LambdaHack.Server.ItemM
  ( registerItem, embedItem, rollItem, rollAndRegisterItem
  , placeItemsInDungeon, embedItemsInDungeon, mapActorCStore_
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , onlyRegisterItem, createLevelItem
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Function
import qualified Data.HashMap.Strict as HM
import           Data.Ord

import           Game.LambdaHack.Atomic
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.ContentData
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.Random
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Content.CaveKind (citemFreq, citemNum)
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.TileKind (TileKind)
import           Game.LambdaHack.Server.ItemRev
import           Game.LambdaHack.Server.MonadServer
import           Game.LambdaHack.Server.ServerOptions
import           Game.LambdaHack.Server.State

onlyRegisterItem :: MonadServerAtomic m => ItemKnown -> m ItemId
onlyRegisterItem itemKnown@(_, aspectRecord, _) = do
  itemRev <- getsServer sitemRev
  case HM.lookup itemKnown itemRev of
    Just iid -> return iid
    Nothing -> do
      icounter <- getsServer sicounter
      executedOnServer <-
        execUpdAtomicSer $ UpdDiscoverServer icounter aspectRecord
      let !_A = assert executedOnServer ()
      modifyServer $ \ser ->
        ser { sitemRev = HM.insert itemKnown icounter (sitemRev ser)
            , sicounter = succ icounter }
      return $! icounter

registerItem :: MonadServerAtomic m
             => ItemFullKit -> ItemKnown -> Container -> Bool
             -> m ItemId
registerItem (ItemFull{itemBase, itemKindId, itemKind}, kit)
             itemKnown@(_, aspectRecord, _) container verbose = do
  iid <- onlyRegisterItem itemKnown
  let cmd = if verbose then UpdCreateItem else UpdSpotItem False
  execUpdAtomic $ cmd iid itemBase kit container
  let worth = itemPrice (fst kit) itemKind
  unless (worth == 0) $ execUpdAtomic $ UpdAlterGold worth
  knowItems <- getsServer $ sknowItems . soptions
  when knowItems $ case container of
    CTrunk{} -> return ()
    _ -> execUpdAtomic $ UpdDiscover container iid itemKindId aspectRecord
  return iid

createLevelItem :: MonadServerAtomic m => Point -> LevelId -> m ()
createLevelItem pos lid = do
  COps{cocave} <- getsState scops
  Level{lkind} <- getLevel lid
  let container = CFloor lid pos
      litemFreq = citemFreq $ okind cocave lkind
  void $ rollAndRegisterItem lid litemFreq container True Nothing

embedItem :: MonadServerAtomic m
          => LevelId -> Point -> ContentId TileKind -> m ()
embedItem lid pos tk = do
  COps{cotile} <- getsState scops
  let embeds = Tile.embeddedItems cotile tk
      container = CEmbed lid pos
      f grp = rollAndRegisterItem lid [(grp, 1)] container False Nothing
  mapM_ f embeds

rollItem :: MonadServerAtomic m
         => Int -> LevelId -> Freqs ItemKind
         -> m (Maybe (ItemKnown, ItemFullKit, GroupName ItemKind))
rollItem lvlSpawned lid itemFreq = do
  cops <- getsState scops
  flavour <- getsServer sflavour
  discoRev <- getsServer sdiscoKindRev
  uniqueSet <- getsServer suniqueSet
  totalDepth <- getsState stotalDepth
  Level{ldepth} <- getLevel lid
  m3 <- rndToAction $ newItem cops flavour discoRev uniqueSet
                              itemFreq lvlSpawned lid ldepth totalDepth
  case m3 of
    Just (_, (ItemFull{itemKindId, itemKind}, _), _) ->
      when (IK.Unique `elem` IK.ifeature itemKind) $
        modifyServer $ \ser ->
          ser {suniqueSet = ES.insert itemKindId (suniqueSet ser)}
    _ -> return ()
  return m3

rollAndRegisterItem :: MonadServerAtomic m
                    => LevelId -> Freqs ItemKind -> Container -> Bool
                    -> Maybe Int
                    -> m (Maybe (ItemId, (ItemFullKit, GroupName ItemKind)))
rollAndRegisterItem lid itemFreq container verbose mk = do
  -- Power depth of new items unaffected by number of spawned actors.
  m3 <- rollItem 0 lid itemFreq
  case m3 of
    Nothing -> return Nothing
    Just (itemKnown, (itemFull, kit), itemGroup) -> do
      let kit2 = (fromMaybe (fst kit) mk, snd kit)
      iid <- registerItem (itemFull, kit2) itemKnown container verbose
      return $ Just (iid, ((itemFull, kit2), itemGroup))

placeItemsInDungeon :: forall m. MonadServerAtomic m
                    => EM.EnumMap LevelId [Point] -> m ()
placeItemsInDungeon alliancePositions = do
  COps{cocave, coTileSpeedup} <- getsState scops
  totalDepth <- getsState stotalDepth
  let initialItems (lid, Level{lkind, ldepth, lxsize, lysize, ltile}) = do
        litemNum <- rndToAction $ castDice ldepth totalDepth
                                  (citemNum $ okind cocave lkind)
        let placeItems :: Int -> m ()
            placeItems n | n == litemNum = return ()
            placeItems !n = do
              Level{lfloor} <- getLevel lid
              -- We ensure that there are no big regions without items at all.
              let distAndOften !p !t =
                    let f !k _ b = chessDist p k > 6 && b
                    in Tile.isOftenItem coTileSpeedup t
                       && EM.foldrWithKey f True lfloor
                  alPos = EM.findWithDefault [] lid alliancePositions
                  -- Don't generate items around initial actors or in tiles.
                  distAllianceAndNotFloor !p _ =
                    let f !k b = chessDist p k > 4 && b
                    in p `EM.notMember` lfloor && foldr f True alPos
              pos <- rndToAction $ findPosTry2 200 ltile
                (\_ !t -> Tile.isWalkable coTileSpeedup t
                          && not (Tile.isNoItem coTileSpeedup t))
                -- If there are very many items, some regions may be very rich,
                -- but let's try to spread at least the initial items evenly.
                ([distAndOften | n * 100 < lxsize * lysize]
                 ++ [\_ !t -> Tile.isOftenItem coTileSpeedup t])
                distAllianceAndNotFloor
                [distAllianceAndNotFloor]
              createLevelItem pos lid
              placeItems (n + 1)
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
