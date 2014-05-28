-- | Server operations for items.
module Game.LambdaHack.Server.ItemServer
  ( registerItem, createItems, placeItemsInDungeon
  , fullAssocsServer, itemToFullServer
  ) where

import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.HashMap.Strict as HM
import Data.Key (mapWithKeyM_)

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Server.ItemRev
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.State

registerItem :: (MonadAtomic m, MonadServer m)
             => ItemKnown -> ItemSeed -> KisOn -> Container -> Bool -> m ItemId
registerItem itemKnown@(item, iae) seed kIsOn container verbose = do
  itemRev <- getsServer sitemRev
  let cmd = if verbose then UpdCreateItem else UpdSpotItem
  case HM.lookup itemKnown itemRev of
    Just iid -> do
      -- TODO: try to avoid this case for createItems,
      -- to make items more interesting
      execUpdAtomic $ cmd iid item kIsOn container
      return iid
    Nothing -> do
      icounter <- getsServer sicounter
      modifyServer $ \ser ->
        ser { sicounter = succ icounter
            , sitemRev = HM.insert itemKnown icounter (sitemRev ser)
            , sitemSeedD = EM.insert icounter seed (sitemSeedD ser)
            , sdiscoAE = EM.insert icounter iae (sdiscoAE ser)}
      execUpdAtomic $ cmd icounter item kIsOn container
      return $! icounter

createItems :: (MonadAtomic m, MonadServer m)
            => Int -> Point -> LevelId -> m ()
createItems n pos lid = do
  Kind.COps{coitem} <- getsState scops
  flavour <- getsServer sflavour
  discoRev <- getsServer sdiscoRev
  Level{ldepth, litemFreq} <- getLevel lid
  depth <- getsState sdepth
  let container = CFloor lid pos
  replicateM_ n $ do
    (itemKnown, seed, k) <-
      rndToAction $ newItem coitem flavour discoRev litemFreq lid ldepth depth
    void $ registerItem itemKnown seed (k, True) container True

placeItemsInDungeon :: (MonadAtomic m, MonadServer m) => m ()
placeItemsInDungeon = do
  Kind.COps{cotile} <- getsState scops
  let initialItems lid (Level{ltile, litemNum, lxsize, lysize}) = do
        let factionDist = max lxsize lysize - 5
        replicateM litemNum $ do
          Level{lfloor} <- getLevel lid
          let dist p = minimum $ maxBound : map (chessDist p) (EM.keys lfloor)
          pos <- rndToAction $ findPosTry 100 ltile
                   (\_ t -> Tile.isWalkable cotile t
                            && (not $ Tile.hasFeature cotile F.NoItem t))
                   [ \p t -> Tile.hasFeature cotile F.OftenItem t
                             && dist p > factionDist `div` 5
                   , \p t -> Tile.hasFeature cotile F.OftenItem t
                             && dist p > factionDist `div` 7
                   , \p t -> Tile.hasFeature cotile F.OftenItem t
                             && dist p > factionDist `div` 9
                   , \p t -> Tile.hasFeature cotile F.OftenItem t
                             && dist p > factionDist `div` 12
                   , \p _ -> dist p > factionDist `div` 5
                   , \p t -> Tile.hasFeature cotile F.OftenItem t
                             || dist p > factionDist `div` 7
                   , \p t -> Tile.hasFeature cotile F.OftenItem t
                             || dist p > factionDist `div` 9
                   , \p t -> Tile.hasFeature cotile F.OftenItem t
                             || dist p > factionDist `div` 12
                   , \p _ -> dist p > 1
                   , \p _ -> EM.notMember p lfloor
                   ]
          createItems 1 pos lid
  dungeon <- getsState sdungeon
  mapWithKeyM_ initialItems dungeon

fullAssocsServer :: MonadServer m
                 => ActorId -> [CStore] -> m [(ItemId, ItemFull)]
fullAssocsServer aid cstores = do
  cops <- getsState scops
  disco <- getsServer sdisco
  discoAE <- getsServer sdiscoAE
  getsState $ fullAssocs cops disco discoAE aid cstores

itemToFullServer :: MonadServer m => m (ItemId -> KisOn -> ItemFull)
itemToFullServer = do
  cops <- getsState scops
  disco <- getsServer sdisco
  discoAE <- getsServer sdiscoAE
  s <- getState
  let itemToF iid = itemToFull cops disco discoAE iid (getItemBody iid s)
  return itemToF
