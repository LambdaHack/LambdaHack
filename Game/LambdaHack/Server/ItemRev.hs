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
import qualified Data.Set as S

import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK

-- | The reverse map to @DiscoveryKind@, needed for item creation.
type DiscoveryKindRev = EM.EnumMap (Kind.Id ItemKind) ItemKindIx

-- | The map of item ids to item seeds, needed for item creation.
type ItemSeedDict = EM.EnumMap ItemId ItemSeed

type UniqueSet = ES.EnumSet (Kind.Id ItemKind)

serverDiscos :: Kind.COps -> Rnd (DiscoveryKind, DiscoveryKindRev)
serverDiscos Kind.COps{coitem=Kind.Ops{olength, ofoldlWithKey', okind}} = do
  let ixs = [toEnum 0..toEnum (olength-1)]
      shuffle :: Eq a => [a] -> Rnd [a]
      shuffle [] = return []
      shuffle l = do
        x <- oneOf l
        (x :) <$> shuffle (delete x l)
  shuffled <- shuffle ixs
  let f (!ikMap, !ikRev, ix : rest) kmKind _ =
        let kmMean = meanAspect $ okind kmKind
        in (EM.insert ix KindMean{..} ikMap, EM.insert kmKind ix ikRev, rest)
      f (ikMap, _, []) ik  _ =
        assert `failure` "too short ixs" `twith` (ik, ikMap)
      (discoS, discoRev, _) =
        ofoldlWithKey' f (EM.empty, EM.empty, shuffled)
  return (discoS, discoRev)

-- | Build an item with the given stats.
buildItem :: FlavourMap -> DiscoveryKindRev -> Kind.Id ItemKind -> ItemKind
          -> LevelId -> Dice.Dice
          -> Item
buildItem (FlavourMap flavour) discoRev ikChosen kind jlid jdamage =
  let jkindIx  = discoRev EM.! ikChosen
      jfid     = Nothing  -- the default
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
newItem :: Kind.COps -> FlavourMap
        -> DiscoveryKind -> DiscoveryKindRev -> UniqueSet
        -> Freqs ItemKind -> Int -> LevelId -> AbsDepth -> AbsDepth
        -> Rnd (Maybe ( ItemKnown, ItemFull, ItemDisco
                      , ItemSeed, GroupName ItemKind ))
newItem Kind.COps{coitem=Kind.Ops{ofoldlGroup'}}
        flavour disco discoRev uniqueSet itemFreq lvlSpawned lid
        ldepth@(AbsDepth ldAbs) totalDepth@(AbsDepth depth) = do
  -- Effective generation depth of actors (not items) increases with spawns.
  let scaledDepth = ldAbs * 10 `div` depth
      numSpawnedCoeff = lvlSpawned `div` 2
      ldSpawned = max ldAbs  -- the first fast spawns are of the nominal level
                  $ min depth
                  $ ldAbs + numSpawnedCoeff - scaledDepth
      findInterval _ x1y1 [] = (x1y1, (11, 0))
      findInterval !ld !x1y1 ((!x, !y) : rest) =
        if fromIntegral ld * 10 <= x * fromIntegral depth
        then (x1y1, (x, y))
        else findInterval ld (x, y) rest
      linearInterpolation !ld !dataset =
        -- We assume @dataset@ is sorted and between 0 and 10.
        let ((x1, y1), (x2, y2)) = findInterval ld (0, 0) dataset
        in ceiling
           $ fromIntegral y1
             + fromIntegral (y2 - y1)
               * (fromIntegral ld * 10 - x1 * fromIntegral depth)
               / ((x2 - x1) * fromIntegral depth)
      f _ _ acc _ ik _ | ik `ES.member` uniqueSet = acc
      f !itemGroup !q !acc !p !ik !kind =
        -- Don't consider lvlSpawned for uniques.
        let ld = if IK.Unique `elem` IK.ieffects kind then ldAbs else ldSpawned
            rarity = linearInterpolation ld (IK.irarity kind)
        in (q * p * rarity, ((ik, kind), itemGroup)) : acc
      g (itemGroup, q) = ofoldlGroup' itemGroup (f itemGroup q) []
      freqDepth = concatMap g itemFreq
      freq = toFreq ("newItem ('" <> tshow ldSpawned <> ")") freqDepth
  if nullFreq freq then return Nothing
  else do
    ((itemKindId, itemKind), itemGroup) <- frequency freq
    -- Number of new items/actors unaffected by number of spawned actors.
    itemN <- castDice ldepth totalDepth (IK.icount itemKind)
    seed <- toEnum <$> random
    jdamage <- frequency $ toFreq "jdamage" $ IK.idamage itemKind
    let itemBase = buildItem flavour discoRev itemKindId itemKind lid jdamage
        kindIx = jkindIx itemBase
        itemK = max 1 itemN
        itemTimer = [timeZero | IK.Periodic `elem` IK.ieffects itemKind]
                      -- delay first discharge of single organs
        itemAspectMean = kmMean $ EM.findWithDefault (assert `failure` kindIx)
                                                     kindIx disco
        itemDiscoData = ItemDisco { itemKindId, itemKind, itemAspectMean
                                  , itemAspect = Just aspectRecord }
        itemDisco = Just itemDiscoData
        -- Bonuses on items/actors unaffected by number of spawned actors.
        aspectRecord = seedToAspect seed itemKind ldepth totalDepth
        itemFull = ItemFull {..}
    return $ Just ( (kindIx, aspectRecord, jdamage, jfid itemBase)
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
rollFlavourMap :: S.Set Flavour
               -> Rnd ( EM.EnumMap (Kind.Id ItemKind) Flavour
                      , EM.EnumMap Char (S.Set Flavour) )
               -> Kind.Id ItemKind -> ItemKind
               -> Rnd ( EM.EnumMap (Kind.Id ItemKind) Flavour
                      , EM.EnumMap Char (S.Set Flavour) )
rollFlavourMap fullFlavSet rnd key ik =
  let flavours = IK.iflavour ik
  in if length flavours == 1
     then rnd
     else do
       (!assocs, !availableMap) <- rnd
       let available =
             EM.findWithDefault fullFlavSet (IK.isymbol ik) availableMap
           proper = S.fromList flavours `S.intersection` available
       assert (not (S.null proper)
               `blame` "not enough flavours for items"
               `twith` (flavours, available, ik, availableMap)) $ do
         flavour <- oneOf $ S.toList proper
         let availableReduced = S.delete flavour available
         return ( EM.insert key flavour assocs
                , EM.insert (IK.isymbol ik) availableReduced availableMap)

-- | Randomly chooses flavour for all item kinds for this game.
dungeonFlavourMap :: Kind.COps -> Rnd FlavourMap
dungeonFlavourMap Kind.COps{coitem=Kind.Ops{ofoldlWithKey'}} =
  liftM (FlavourMap . fst) $
    ofoldlWithKey' (rollFlavourMap (S.fromList stdFlav))
                   (return (EM.empty, EM.empty))

-- | Reverse item map, for item creation, to keep items and item identifiers
-- in bijection.
type ItemRev = HM.HashMap ItemKnown ItemId

-- | The essential item properties, used for the @ItemRev@ hash table
-- from items to their ids, needed to assign ids to newly generated items.
-- All the other meaningul properties can be derived from them.
-- Note 1: @jlid@ is not meaningful; it gets forgotten if items from
-- different levels roll the same random properties and so are merged.
-- However, the first item generated by the server wins, which is most
-- of the time the lower @jlid@ item, which makes sense for the client.
-- Note 2: @ItemSeed@ instead of @AspectRecord@ is not enough,
-- becaused different seeds may result in the same @AspectRecord@
-- and we don't want such items to be distinct in UI and elsewhere.
type ItemKnown = (ItemKindIx, AspectRecord, Dice.Dice, Maybe FactionId)
