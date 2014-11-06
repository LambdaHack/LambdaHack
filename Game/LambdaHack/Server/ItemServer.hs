-- | Server operations for items.
module Game.LambdaHack.Server.ItemServer
  ( rollItem, rollAndRegisterItem, registerItem, createItems
  , placeItemsInDungeon, embedItemsInDungeon, fullAssocsServer
  , activeItemsServer, itemToFullServer, mapActorCStore_
  ) where

import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.HashMap.Strict as HM
import Data.Key (mapWithKeyM_)
import Data.Maybe

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
import Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Content.TileKind as TK
import Game.LambdaHack.Server.ItemRev
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.State

registerItem :: (MonadAtomic m, MonadServer m)
             => Item -> ItemKnown -> ItemSeed -> Int -> Container -> Bool
             -> m ItemId
registerItem item itemKnown@(_, iae) seed k container verbose = do
  itemRev <- getsServer sitemRev
  let cmd = if verbose then UpdCreateItem else UpdSpotItem
  case HM.lookup itemKnown itemRev of
    Just iid -> do
      -- TODO: try to avoid this case for createItems,
      -- to make items more interesting
      execUpdAtomic $ cmd iid item (k, []) container
      return iid
    Nothing -> do
      icounter <- getsServer sicounter
      modifyServer $ \ser ->
        ser { sicounter = succ icounter
            , sitemRev = HM.insert itemKnown icounter (sitemRev ser)
            , sitemSeedD = EM.insert icounter seed (sitemSeedD ser)
            , sdiscoEffect = EM.insert icounter iae (sdiscoEffect ser)}
      execUpdAtomic $ cmd icounter item (k, []) container
      return $! icounter

createItems :: (MonadAtomic m, MonadServer m)
            => Int -> Point -> LevelId -> m ()
createItems n pos lid = do
  Level{litemFreq} <- getLevel lid
  let container = CFloor lid pos
  replicateM_ n $ void $
    rollAndRegisterItem lid litemFreq container True Nothing

embedItem :: (MonadAtomic m, MonadServer m)
          => LevelId -> Point -> Kind.Id TileKind -> m ()
embedItem lid pos tk = do
  Kind.COps{cotile} <- getsState scops
  let embeds = Tile.embedItems cotile tk
      causes = Tile.causeEffects cotile tk
      -- TODO: unack this, e.g., by turning each Cause into Embed
      itemFreq = zip embeds (repeat 1)
                 ++ if not (null causes) && null embeds
                    then [("hero", 1)]  -- hack: the bag, not item, is relevant
                    else []
      container = CEmbed lid pos
  void $ rollAndRegisterItem lid itemFreq container False Nothing

rollItem :: (MonadAtomic m, MonadServer m)
         => LevelId -> Freqs ItemKind
         -> m (Maybe (ItemKnown, ItemFull, ItemSeed, GroupName ItemKind))
rollItem lid itemFreq = do
  cops <- getsState scops
  flavour <- getsServer sflavour
  discoRev <- getsServer sdiscoKindRev
  totalDepth <- getsState stotalDepth
  Level{ldepth} <- getLevel lid
  rndToAction $ newItem cops flavour discoRev itemFreq lid ldepth totalDepth

rollAndRegisterItem :: (MonadAtomic m, MonadServer m)
                    => LevelId -> Freqs ItemKind -> Container -> Bool
                    -> Maybe Int
                    -> m (Maybe (ItemId, (ItemFull, GroupName ItemKind)))
rollAndRegisterItem lid itemFreq container verbose mk = do
  m4 <- rollItem lid itemFreq
  case m4 of
    Nothing -> return Nothing
    Just (itemKnown, itemFullRaw, seed, itemGroup) -> do
      let itemFull = itemFullRaw {itemK = fromMaybe (itemK itemFullRaw) mk}
      iid <- registerItem (itemBase itemFull) itemKnown seed
                          (itemK itemFull) container verbose
      return $ Just (iid, (itemFull, itemGroup))

placeItemsInDungeon :: (MonadAtomic m, MonadServer m) => m ()
placeItemsInDungeon = do
  Kind.COps{cotile} <- getsState scops
  let initialItems lid (Level{lfloor, ltile, litemNum, lxsize, lysize}) = do
        let factionDist = max lxsize lysize - 5
        replicateM litemNum $ do
          let dist p = minimum $ maxBound : map (chessDist p) (EM.keys lfloor)
          pos <- rndToAction $ findPosTry 100 ltile
                   (\_ t -> Tile.isWalkable cotile t
                            && (not $ Tile.hasFeature cotile TK.NoItem t))
                   [ \p t -> Tile.hasFeature cotile TK.OftenItem t
                             && dist p > factionDist `div` 5
                   , \p t -> Tile.hasFeature cotile TK.OftenItem t
                             && dist p > factionDist `div` 7
                   , \p t -> Tile.hasFeature cotile TK.OftenItem t
                             && dist p > factionDist `div` 9
                   , \p t -> Tile.hasFeature cotile TK.OftenItem t
                             && dist p > factionDist `div` 12
                   , \p _ -> dist p > factionDist `div` 5
                   , \p t -> Tile.hasFeature cotile TK.OftenItem t
                             || dist p > factionDist `div` 7
                   , \p t -> Tile.hasFeature cotile TK.OftenItem t
                             || dist p > factionDist `div` 9
                   , \p t -> Tile.hasFeature cotile TK.OftenItem t
                             || dist p > factionDist `div` 12
                   , \p _ -> dist p > 1
                   , \p _ -> EM.notMember p lfloor
                   ]
          createItems 1 pos lid
  dungeon <- getsState sdungeon
  mapWithKeyM_ initialItems dungeon

embedItemsInDungeon :: (MonadAtomic m, MonadServer m) => m ()
embedItemsInDungeon = do
  let embedItems lid (Level{ltile}) =
        PointArray.mapWithKeyM_A (embedItem lid) ltile
  dungeon <- getsState sdungeon
  mapWithKeyM_ embedItems dungeon

fullAssocsServer :: MonadServer m
                 => ActorId -> [CStore] -> m [(ItemId, ItemFull)]
fullAssocsServer aid cstores = do
  cops <- getsState scops
  discoKind <- getsServer sdiscoKind
  discoEffect <- getsServer sdiscoEffect
  getsState $ fullAssocs cops discoKind discoEffect aid cstores

activeItemsServer :: MonadServer m => ActorId -> m [ItemFull]
activeItemsServer aid = do
  activeAssocs <- fullAssocsServer aid [CEqp, COrgan]
  return $! map snd activeAssocs

itemToFullServer :: MonadServer m => m (ItemId -> ItemQuant -> ItemFull)
itemToFullServer = do
  cops <- getsState scops
  discoKind <- getsServer sdiscoKind
  discoEffect <- getsServer sdiscoEffect
  s <- getState
  let itemToF iid = itemToFull cops discoKind discoEffect iid (getItemBody iid s)
  return itemToF

-- | Mapping over actor's items from a give store.
mapActorCStore_ :: MonadServer m
                => CStore -> (ItemId -> ItemQuant -> m a) -> Actor ->  m ()
mapActorCStore_ cstore f b = do
  bag <- getsState $ getBodyActorBag b cstore
  mapM_ (uncurry f) $ EM.assocs bag
