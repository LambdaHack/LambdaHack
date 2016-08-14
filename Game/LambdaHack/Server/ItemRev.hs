{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Server types and operations for items that don't involve server state
-- nor our custom monads.
module Game.LambdaHack.Server.ItemRev
  ( ItemKnown, ItemRev, buildItem, newItem, UniqueSet
    -- * Item discovery types
  , DiscoveryKindRev, serverDiscos, ItemSeedDict
    -- * The @FlavourMap@ type
  , FlavourMap, emptyFlavourMap, dungeonFlavourMap
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.HashMap.Strict as HM
import qualified Data.Ix as Ix
import qualified Data.Set as S

import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK

-- | The reverse map to @DiscoveryKind@, needed for item creation.
type DiscoveryKindRev = EM.EnumMap (Kind.Id ItemKind) ItemKindIx

-- | The map of item ids to item seeds, needed for item creation.
type ItemSeedDict = EM.EnumMap ItemId ItemSeed

type UniqueSet = ES.EnumSet (Kind.Id ItemKind)

serverDiscos :: Kind.COps -> Rnd (DiscoveryKind, DiscoveryKindRev)
serverDiscos Kind.COps{coitem=Kind.Ops{obounds, ofoldrWithKey, okind}} = do
  let ixs = map toEnum $ take (Ix.rangeSize obounds) [0..]
      shuffle :: Eq a => [a] -> Rnd [a]
      shuffle [] = return []
      shuffle l = do
        x <- oneOf l
        (x :) <$> shuffle (delete x l)
  shuffled <- shuffle ixs
  let f kmKind _ (ikMap, ikRev, ix : rest) =
        let kmMean = meanAspect $ okind kmKind
        in (EM.insert ix KindMean{..} ikMap, EM.insert kmKind ix ikRev, rest)
      f ik  _ (ikMap, _, []) =
        assert `failure` "too short ixs" `twith` (ik, ikMap)
      (discoS, discoRev, _) =
        ofoldrWithKey f (EM.empty, EM.empty, shuffled)
  return (discoS, discoRev)

-- | Build an item with the given stats.
buildItem :: FlavourMap -> DiscoveryKindRev -> Kind.Id ItemKind -> ItemKind
          -> LevelId
          -> Item
buildItem (FlavourMap flavour) discoRev ikChosen kind jlid =
  let jkindIx  = discoRev EM.! ikChosen
      jsymbol  = IK.isymbol kind
      jname    = IK.iname kind
      jflavour =
        case IK.iflavour kind of
          [fl] -> fl
          _ -> flavour EM.! ikChosen
      jfeature = IK.ifeature kind
      jweight = IK.iweight kind
  in Item{..}

-- | Generate an item based on level.
newItem :: Kind.COps -> FlavourMap -> DiscoveryKind -> DiscoveryKindRev -> UniqueSet
        -> Freqs ItemKind -> Int -> LevelId -> AbsDepth -> AbsDepth
        -> Rnd (Maybe ( ItemKnown, ItemFull, ItemDisco
                      , ItemSeed, GroupName ItemKind ))
newItem Kind.COps{coitem=Kind.Ops{ofoldrGroup}}
        flavour disco discoRev uniqueSet itemFreq lvlSpawned jlid
        ldepth@(AbsDepth ldAbs) totalDepth@(AbsDepth depth) = do
  -- Effective generation depth of actors (not items) increases with spawns.
  let scaledDepth = ldAbs * 10 `div` depth
      numSpawnedCoeff = lvlSpawned `div` 2
      ldSpawned = max ldAbs  -- the first fast spawns are of the nominal level
                  $ min depth
                  $ ldAbs + numSpawnedCoeff - scaledDepth
      findInterval _ x1y1 [] = (x1y1, (11, 0))
      findInterval ld x1y1 ((x, y) : rest) =
        if fromIntegral ld * 10 <= x * fromIntegral depth
        then (x1y1, (x, y))
        else findInterval ld (x, y) rest
      linearInterpolation ld dataset =
        -- We assume @dataset@ is sorted and between 0 and 10.
        let ((x1, y1), (x2, y2)) = findInterval ld (0, 0) dataset
        in ceiling
           $ fromIntegral y1
             + fromIntegral (y2 - y1)
               * (fromIntegral ld * 10 - x1 * fromIntegral depth)
               / ((x2 - x1) * fromIntegral depth)
      f _ _ _ ik _ acc | ik `ES.member` uniqueSet = acc
      f itemGroup q p ik kind acc =
        -- Don't consider lvlSpawned for uniques.
        let ld = if IK.Unique `elem` IK.iaspects kind then ldAbs else ldSpawned
            rarity = linearInterpolation ld (IK.irarity kind)
        in (q * p * rarity, ((ik, kind), itemGroup)) : acc
      g (itemGroup, q) = ofoldrGroup itemGroup (f itemGroup q) []
      freqDepth = concatMap g itemFreq
      freq = toFreq ("newItem ('" <> tshow ldSpawned <> ")") freqDepth
  if nullFreq freq then return Nothing
  else do
    ((itemKindId, itemKind), itemGroup) <- frequency freq
    -- Number of new items/actors unaffected by number of spawned actors.
    itemN <- castDice ldepth totalDepth (IK.icount itemKind)
    seed <- fmap toEnum random
    let itemBase = buildItem flavour discoRev itemKindId itemKind jlid
        kindIx = jkindIx itemBase
        itemK = max 1 itemN
        itemTimer = []
        itemAspectMean = kmMean $ EM.findWithDefault (assert `failure` kindIx) kindIx disco
        itemDiscoData = ItemDisco { itemKindId, itemKind, itemAspectMean
                                  , itemAspect = Just aspectRecord }
        itemDisco = Just itemDiscoData
        -- Bonuses on items/actors unaffected by number of spawned actors.
        aspectRecord = seedToAspect seed itemKind ldepth totalDepth
        itemFull = ItemFull {..}
    return $ Just ( (kindIx, aspectRecord)
                  , itemFull
                  , itemDiscoData
                  , seed
                  , itemGroup )

-- | Flavours assigned by the server to item kinds, in this particular game.
newtype FlavourMap = FlavourMap (EM.EnumMap (Kind.Id ItemKind) Flavour)
  deriving (Show, Binary)

emptyFlavourMap :: FlavourMap
emptyFlavourMap = FlavourMap EM.empty

-- | Assigns flavours to item kinds. Assures no flavor is repeated for the same
-- symbol, except for items with only one permitted flavour.
rollFlavourMap :: S.Set Flavour -> Kind.Id ItemKind -> ItemKind
               -> Rnd ( EM.EnumMap (Kind.Id ItemKind) Flavour
                      , EM.EnumMap Char (S.Set Flavour) )
               -> Rnd ( EM.EnumMap (Kind.Id ItemKind) Flavour
                      , EM.EnumMap Char (S.Set Flavour) )
rollFlavourMap fullFlavSet key ik rnd =
  let flavours = IK.iflavour ik
  in if length flavours == 1
     then rnd
     else do
       (assocs, availableMap) <- rnd
       let available =
             EM.findWithDefault fullFlavSet (IK.isymbol ik) availableMap
           proper = S.fromList flavours `S.intersection` available
       assert (not (S.null proper)
               `blame` "not enough flavours for items"
               `twith` (flavours, available, ik, availableMap)) $ do
         flavour <- oneOf (S.toList proper)
         let availableReduced = S.delete flavour available
         return ( EM.insert key flavour assocs
                , EM.insert (IK.isymbol ik) availableReduced availableMap)

-- | Randomly chooses flavour for all item kinds for this game.
dungeonFlavourMap :: Kind.COps -> Rnd FlavourMap
dungeonFlavourMap Kind.COps{coitem=Kind.Ops{ofoldrWithKey}} =
  liftM (FlavourMap . fst) $
    ofoldrWithKey (rollFlavourMap (S.fromList stdFlav))
                  (return (EM.empty, EM.empty))

-- | Reverse item map, for item creation, to keep items and item identifiers
-- in bijection.
type ItemRev = HM.HashMap ItemKnown ItemId

-- | The essential item properties, used for the @ItemRev@ hash table
-- from items to their ids, needed to assign ids to newly generated items.
-- All the other meaningul properties can be derived from the two.
-- Note that @jlid@ is not meaningful; it gets forgotten if items from
-- different levels roll the same random properties and so are merged.
type ItemKnown = (ItemKindIx, AspectRecord)
