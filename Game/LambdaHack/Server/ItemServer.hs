-- | Server operations for items.
module Game.LambdaHack.Server.ItemServer
  ( rollItem, rollAndRegisterItem, registerItem
  , placeItemsInDungeon, embedItemsInDungeon, fullAssocsServer
  , activeItemsServer, itemToFullServer, mapActorCStore_
  ) where

import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Function
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Maybe
import Data.Ord
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Content.TileKind as TK
import Game.LambdaHack.Server.ItemRev
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.State

registerItem :: (MonadAtomic m, MonadServer m)
             => ItemFull -> ItemKnown -> ItemSeed -> Int -> Container -> Bool
             -> m ItemId
registerItem itemFull itemKnown@(_, iae) seed k container verbose = do
  itemRev <- getsServer sitemRev
  let cmd = if verbose then UpdCreateItem else UpdSpotItem
  case HM.lookup itemKnown itemRev of
    Just iid -> do
      -- TODO: try to avoid this case for createItems,
      -- to make items more interesting
      execUpdAtomic $ cmd iid (itemBase itemFull) (k, []) container
      return iid
    Nothing -> do
      let fovSight = fromMaybe 0
                     $ strengthFromEqpSlot IK.EqpSlotAddSight itemFull
          fovSmell = fromMaybe 0
                     $ strengthFromEqpSlot IK.EqpSlotAddSmell itemFull
          fovLight = fromMaybe 0
                     $ strengthFromEqpSlot IK.EqpSlotAddLight itemFull
          ssl = FovCache3{..}
      icounter <- getsServer sicounter
      modifyServer $ \ser ->
        ser { sdiscoEffect = EM.insert icounter iae (sdiscoEffect ser)
            , sitemSeedD = EM.insert icounter seed (sitemSeedD ser)
            , sitemRev = HM.insert itemKnown icounter (sitemRev ser)
            , sItemFovCache = if ssl == emptyFovCache3 then sItemFovCache ser
                              else EM.insert icounter ssl (sItemFovCache ser)
            , sicounter = succ icounter }
      execUpdAtomic $ cmd icounter (itemBase itemFull) (k, []) container
      return $! icounter

createLevelItem :: (MonadAtomic m, MonadServer m)
                => Point -> LevelId -> m ()
createLevelItem pos lid = do
  Level{litemFreq} <- getLevel lid
  let container = CFloor lid pos
  void $ rollAndRegisterItem lid litemFreq container True Nothing

embedItem :: (MonadAtomic m, MonadServer m)
          => LevelId -> Point -> Kind.Id TileKind -> m ()
embedItem lid pos tk = do
  Kind.COps{cotile} <- getsState scops
  let embeds = Tile.embedItems cotile tk
      causes = Tile.causeEffects cotile tk
      -- TODO: unhack this, e.g., by turning each Cause into Embed
      itemFreq = zip embeds (repeat 1)
                 ++ -- Hack: the bag, not item, is relevant.
                    [("hero", 1) |  not (null causes) && null embeds]
      container = CEmbed lid pos
  void $ rollAndRegisterItem lid itemFreq container False Nothing

rollItem :: (MonadAtomic m, MonadServer m)
         => LevelId -> Freqs ItemKind
         -> m (Maybe ( ItemKnown, ItemFull, ItemDisco
                     , ItemSeed, GroupName ItemKind ))
rollItem lid itemFreq = do
  cops <- getsState scops
  flavour <- getsServer sflavour
  discoRev <- getsServer sdiscoKindRev
  uniqueSet <- getsServer suniqueSet
  totalDepth <- getsState stotalDepth
  Level{ldepth} <- getLevel lid
  m5 <- rndToAction $ newItem cops flavour discoRev uniqueSet
                              itemFreq lid ldepth totalDepth
  case m5 of
    Just (_, _, ItemDisco{ itemKindId
                         , itemAE=Just ItemAspectEffect{jaspects}}, _, _) ->
      when (IK.Unique `elem` jaspects) $
        modifyServer $ \ser ->
          ser {suniqueSet = ES.insert itemKindId (suniqueSet ser)}
    _ -> return ()
  return m5

rollAndRegisterItem :: (MonadAtomic m, MonadServer m)
                    => LevelId -> Freqs ItemKind -> Container -> Bool
                    -> Maybe Int
                    -> m (Maybe (ItemId, (ItemFull, GroupName ItemKind)))
rollAndRegisterItem lid itemFreq container verbose mk = do
  m5 <- rollItem lid itemFreq
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
      iid <- registerItem itemFull itemKnown seed
                          (itemK itemFull) container verbose
      return $ Just (iid, (itemFull, itemGroup))

placeItemsInDungeon :: forall m. (MonadAtomic m, MonadServer m) => m ()
placeItemsInDungeon = do
  Kind.COps{cotile} <- getsState scops
  let initialItems (lid, Level{lfloor, ltile, litemNum, lxsize, lysize}) = do
        let factionDist = max lxsize lysize - 5
            placeItems :: [Point] -> Int -> m ()
            placeItems _ 0 = return ()
            placeItems lfloorKeys n = do
              let dist p = minimum $ maxBound : map (chessDist p) lfloorKeys
              pos <- rndToAction $ findPosTry 100 ltile
                   (\_ t -> Tile.isWalkable cotile t
                            && not (Tile.hasFeature cotile TK.NoItem t))
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
                   , \p _ -> dist p > 0
                   ]
              createLevelItem pos lid
              placeItems (pos : lfloorKeys) (n - 1)
        placeItems (EM.keys lfloor) litemNum
  dungeon <- getsState sdungeon
  -- Make sure items on easy levels are generated first, to avoid all
  -- artifacts on deep levels.
  let absLid = abs . fromEnum
      fromEasyToHard = sortBy (comparing absLid `on` fst) $ EM.assocs dungeon
  mapM_ initialItems fromEasyToHard

embedItemsInDungeon :: (MonadAtomic m, MonadServer m) => m ()
embedItemsInDungeon = do
  let embedItems (lid, Level{ltile}) =
        PointArray.mapWithKeyMA (embedItem lid) ltile
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
  let itemToF iid =
        itemToFull cops discoKind discoEffect iid (getItemBody iid s)
  return itemToF

-- | Mapping over actor's items from a give store.
mapActorCStore_ :: MonadServer m
                => CStore -> (ItemId -> ItemQuant -> m a) -> Actor ->  m ()
mapActorCStore_ cstore f b = do
  bag <- getsState $ getBodyActorBag b cstore
  mapM_ (uncurry f) $ EM.assocs bag
