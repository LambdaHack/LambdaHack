-- | Server operations for items.
module Game.LambdaHack.Server.ItemM
  ( rollItem, rollAndRegisterItem, onlyRegisterItem, registerItem
  , placeItemsInDungeon, embedItemsInDungeon, fullAssocsServer
  , itemToFullServer, mapActorCStore_
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Function
import qualified Data.HashMap.Strict as HM
import Data.Ord
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.TileKind (TileKind)
import Game.LambdaHack.Server.ItemRev
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.State

onlyRegisterItem :: (MonadAtomic m, MonadServer m)
                 => ItemFull -> ItemKnown -> ItemSeed -> m ItemId
onlyRegisterItem ItemFull{..} itemKnown@(_, _, _, aspectRecord) seed = do
  itemRev <- getsServer sitemRev
  case HM.lookup itemKnown itemRev of
    Just iid ->
      -- TODO: try to avoid this case for createItems,
      -- to make items more interesting
      return iid
    Nothing -> do
      icounter <- getsServer sicounter
      modifyServer $ \ser ->
        ser { sdiscoAspect = EM.insert icounter aspectRecord (sdiscoAspect ser)
            , sitemSeedD = EM.insert icounter seed (sitemSeedD ser)
            , sitemRev = HM.insert itemKnown icounter (sitemRev ser)
            , sicounter = succ icounter }
      return $! icounter

registerItem :: (MonadAtomic m, MonadServer m)
             => ItemFull -> ItemKnown -> ItemSeed -> Container -> Bool
             -> m ItemId
registerItem itemFull@ItemFull{..} itemKnown seed container verbose = do
  iid <- onlyRegisterItem itemFull itemKnown seed
  let cmd = if verbose then UpdCreateItem else UpdSpotItem False
  execUpdAtomic $ cmd iid itemBase (itemK, itemTimer) container
  return iid

createLevelItem :: (MonadAtomic m, MonadServer m) => Point -> LevelId -> m ()
createLevelItem pos lid = do
  Level{litemFreq} <- getLevel lid
  let container = CFloor lid pos
  void $ rollAndRegisterItem lid litemFreq container True Nothing

-- TODO: this is becoming costly; redo after Embed rethought
-- perhaps keep such tiles in a map to avoid imap over all dungeon tiles
embedItem :: (MonadAtomic m, MonadServer m)
          => LevelId -> Point -> Kind.Id TileKind -> m ()
embedItem lid pos tk = do
  Kind.COps{cotile, coTileSpeedup} <- getsState scops
  let embeds = Tile.embedItems cotile tk
      -- TODO: unhack this, e.g., by turning each Cause into Embed
      itemFreq = zip embeds (repeat 1)
                 ++ -- Hack: the bag, not item, is relevant.
                    [("hero", 1) | Tile.hasCauses coTileSpeedup tk
                                   && null embeds]
      container = CEmbed lid pos
  void $ rollAndRegisterItem lid itemFreq container False Nothing

rollItem :: (MonadAtomic m, MonadServer m)
         => Int -> LevelId -> Freqs ItemKind
         -> m (Maybe ( ItemKnown, ItemFull, ItemDisco
                     , ItemSeed, GroupName ItemKind ))
rollItem lvlSpawned lid itemFreq = do
  cops <- getsState scops
  flavour <- getsServer sflavour
  disco <- getsServer sdiscoKind
  discoRev <- getsServer sdiscoKindRev
  uniqueSet <- getsServer suniqueSet
  totalDepth <- getsState stotalDepth
  Level{ldepth} <- getLevel lid
  m5 <- rndToAction $ newItem cops flavour disco discoRev uniqueSet
                              itemFreq lvlSpawned lid ldepth totalDepth
  case m5 of
    Just (_, _, ItemDisco{itemKindId, itemKind}, _, _) ->
      when (IK.Unique `elem` IK.ieffects itemKind) $
        modifyServer $ \ser ->
          ser {suniqueSet = ES.insert itemKindId (suniqueSet ser)}
    _ -> return ()
  return m5

rollAndRegisterItem :: (MonadAtomic m, MonadServer m)
                    => LevelId -> Freqs ItemKind -> Container -> Bool
                    -> Maybe Int
                    -> m (Maybe (ItemId, (ItemFull, GroupName ItemKind)))
rollAndRegisterItem lid itemFreq container verbose mk = do
  -- Power depth of new items unaffected by number of spawned actors.
  m5 <- rollItem 0 lid itemFreq
  case m5 of
    Nothing -> return Nothing
    Just (itemKnown, itemFullRaw, itemDisco, seed, itemGroup) -> do
      let item = itemBase itemFullRaw
          trunkName = makePhrase [MU.WownW (MU.Text $ jname item) "trunk"]
          itemTrunk = if null $ IK.ikit $ itemKind itemDisco
                      then item
                      else item {jname = trunkName}
          itemFull = itemFullRaw { itemK = fromMaybe (itemK itemFullRaw) mk
                                 , itemBase = itemTrunk }
      iid <- registerItem itemFull itemKnown seed container verbose
      return $ Just (iid, (itemFull, itemGroup))

placeItemsInDungeon :: forall m. (MonadAtomic m, MonadServer m) => m ()
placeItemsInDungeon = do
  Kind.COps{coTileSpeedup} <- getsState scops
  let initialItems (lid, Level{ltile, litemNum, lxsize, lysize}) = do
        let placeItems :: Int -> m ()
            placeItems 0 = return ()
            placeItems !n = do
              Level{lfloor} <- getLevel lid
              -- We ensure that there are no big regions without items at all.
              let dist !p _ =
                    let f !k _ b = chessDist p k > 8 && b
                    in EM.foldrWithKey f True lfloor
                  notM !p _ = p `EM.notMember` lfloor
              pos <- rndToAction $ findPosTry2 100 ltile
                (\_ !t -> Tile.isWalkable coTileSpeedup t
                          && not (Tile.isNoItem coTileSpeedup t))
                -- If there are very many items, some regions may be very rich.
                ([dist | n * 100 < lxsize * lysize] ++ [notM])
                (\_ !t -> Tile.isOftenItem coTileSpeedup t)
                [notM]
              createLevelItem pos lid
              placeItems (n - 1)
        placeItems litemNum
  dungeon <- getsState sdungeon
  -- Make sure items on easy levels are generated first, to avoid all
  -- artifacts on deep levels.
  let absLid = abs . fromEnum
      fromEasyToHard = sortBy (comparing absLid `on` fst) $ EM.assocs dungeon
  mapM_ initialItems fromEasyToHard

embedItemsInDungeon :: (MonadAtomic m, MonadServer m) => m ()
embedItemsInDungeon = do
  let embedItems (lid, Level{ltile}) = PointArray.imapMA_ (embedItem lid) ltile
  dungeon <- getsState sdungeon
  -- Make sure items on easy levels are generated first, to avoid all
  -- artifacts on deep levels.
  let absLid = abs . fromEnum
      fromEasyToHard = sortBy (comparing absLid `on` fst) $ EM.assocs dungeon
  mapM_ embedItems fromEasyToHard

fullAssocsServer :: MonadServer m
                 => ActorId -> [CStore] -> m [(ItemId, ItemFull)]
fullAssocsServer aid cstores = do
  cops <- getsState scops
  discoKind <- getsServer sdiscoKind
  discoAspect <- getsServer sdiscoAspect
  getsState $ fullAssocs cops discoKind discoAspect aid cstores

itemToFullServer :: MonadServer m => m (ItemId -> ItemQuant -> ItemFull)
itemToFullServer = do
  cops <- getsState scops
  discoKind <- getsServer sdiscoKind
  discoAspect <- getsServer sdiscoAspect
  s <- getState
  let itemToF iid =
        itemToFull cops discoKind discoAspect iid (getItemBody iid s)
  return itemToF

-- | Mapping over actor's items from a give store.
mapActorCStore_ :: MonadServer m
                => CStore -> (ItemId -> ItemQuant -> m a) -> Actor ->  m ()
mapActorCStore_ cstore f b = do
  bag <- getsState $ getBodyStoreBag b cstore
  mapM_ (uncurry f) $ EM.assocs bag
