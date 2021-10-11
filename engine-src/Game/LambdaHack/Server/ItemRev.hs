{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Creation of items on the server. Types and operations that don't involve
-- server state nor our custom monads.
module Game.LambdaHack.Server.ItemRev
  ( ItemKnown(..), NewItem(..), ItemRev, UniqueSet
  , buildItem, newItemKind, newItem
    -- * Item discovery types
  , DiscoveryKindRev, emptyDiscoveryKindRev, serverDiscos
    -- * The @FlavourMap@ type
  , FlavourMap, emptyFlavourMap, dungeonFlavourMap, rollFlavourMap, invalidFlavourCode
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.HashMap.Strict as HM
import           Data.Hashable (Hashable)
import           Data.Vector.Binary ()
import qualified Data.Vector.Unboxed as U
import           GHC.Generics (Generic)

import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Content.ItemKind (ItemKind)
import qualified Game.LambdaHack.Content.ItemKind as IK
import qualified Game.LambdaHack.Core.Dice as Dice
import           Game.LambdaHack.Core.Frequency
import           Game.LambdaHack.Core.Random
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.Flavour

-- | The essential item properties, used for the @ItemRev@ hash table
-- from items to their ids, needed to assign ids to newly generated items.
-- All the other meaningful properties can be derived from them.
-- Note: item seed instead of @AspectRecord@ is not enough,
-- becaused different seeds may result in the same @AspectRecord@
-- and we don't want such items to be distinct in UI and elsewhere.
data ItemKnown = ItemKnown ItemIdentity IA.AspectRecord (Maybe FactionId)
  deriving (Show, Eq, Generic)

instance Binary ItemKnown

instance Hashable ItemKnown

data NewItem =
    NewItem ItemKnown ItemFull ItemQuant
  | NoNewItem

-- | Reverse item map, for item creation, to keep items and item identifiers
-- in bijection.
type ItemRev = HM.HashMap ItemKnown ItemId

type UniqueSet = ES.EnumSet (ContentId ItemKind)

-- | Build an item with the given kind and aspects.
buildItem :: COps -> IA.AspectRecord -> FlavourMap
          -> DiscoveryKindRev -> ContentId ItemKind
          -> Item
buildItem COps{coitem} arItem (FlavourMap flavourMap)
          (DiscoveryKindRev discoRev) ikChosen =
  let jkind = case IA.aPresentAs arItem of
        Just grp ->
          let kindHidden = ouniqGroup coitem grp
          in IdentityCovered
               (toItemKindIx $ discoRev U.! contentIdIndex ikChosen)
               kindHidden
        Nothing -> IdentityObvious ikChosen
      jfid     = Nothing  -- the default
      jflavour = toEnum $ fromEnum $ flavourMap U.! contentIdIndex ikChosen
  in Item{..}

-- | Roll an item kind based on given @Freqs@ and kind rarities
newItemKind :: COps -> UniqueSet -> Freqs ItemKind
            -> Dice.AbsDepth -> Dice.AbsDepth -> Int
            -> Frequency (ContentId IK.ItemKind, ItemKind)
newItemKind COps{coitem, coItemSpeedup} uniqueSet itemFreq
            (Dice.AbsDepth ldepth) (Dice.AbsDepth totalDepth) lvlSpawned =
  assert (any (\(_, n) -> n > 0) itemFreq) $
  -- Effective generation depth of actors (not items) increases with spawns.
  -- Up to 10 spawns, no effect. With 20 spawns, depth + 5, and then
  -- each 10 spawns adds 5 depth.
  let numSpawnedCoeff = max 0 $ lvlSpawned `div` 2 - 5
      ldSpawned = ldepth + numSpawnedCoeff
      f _ acc _ ik _ | ik `ES.member` uniqueSet = acc
      f !q !acc !p !ik !kind =
        -- Don't consider lvlSpawned for uniques, except those that have
        -- @Unique@ under @Odds@.
        let ld = if IA.checkFlag Ability.Unique
                    $ IA.kmMean $ getKindMean ik coItemSpeedup
                 then ldepth
                 else ldSpawned
            rarity = linearInterpolation ld totalDepth (IK.irarity kind)
            !fr = q * p * rarity
        in (fr, (ik, kind)) : acc
      g (!itemGroup, !q) = ofoldlGroup' coitem itemGroup (f q) []
      freqDepth = concatMap g itemFreq
  in toFreq "newItemKind" freqDepth

-- | Given item kind frequency, roll item kind, generate item aspects
-- based on level and put together the full item data set.
newItem :: COps -> Frequency (ContentId IK.ItemKind, ItemKind)
        -> FlavourMap -> DiscoveryKindRev
        -> Dice.AbsDepth -> Dice.AbsDepth
        -> Rnd NewItem
newItem cops freq flavourMap discoRev levelDepth totalDepth =
  if nullFreq freq
  then return NoNewItem  -- e.g., rare tile has a unique embed, only first time
  else do
    (itemKindId, itemKind) <- frequency freq
    -- Number of new items/actors unaffected by number of spawned actors.
    itemN <- castDice levelDepth totalDepth (IK.icount itemKind)
    arItem <- IA.rollAspectRecord (IK.iaspects itemKind) levelDepth totalDepth
    let itemBase = buildItem cops arItem flavourMap discoRev itemKindId
        itemIdentity = jkind itemBase
        !itemK = max 1 itemN
        !itemTimer = [itemTimerZero | IA.checkFlag Ability.Periodic arItem]
          -- enable optimization in @applyPeriodicLevel@
        itemSuspect = False
        -- Bonuses on items/actors unaffected by number of spawned actors.
        itemDisco = ItemDiscoFull arItem
        itemFull = ItemFull {..}
        itemKnown = ItemKnown itemIdentity arItem (jfid itemBase)
        itemQuant = if itemK == 1 && null itemTimer
                    then quantSingle
                    else (itemK, itemTimer)
    return $! NewItem itemKnown itemFull itemQuant

-- | The reverse map to @DiscoveryKind@, needed for item creation.
-- This is total and never changes, hence implemented as vector.
-- Morally, it's indexed by @ContentId ItemKind@ and elements are @ItemKindIx@.
newtype DiscoveryKindRev = DiscoveryKindRev (U.Vector Word16)
  deriving (Show, Binary)

emptyDiscoveryKindRev :: DiscoveryKindRev
emptyDiscoveryKindRev = DiscoveryKindRev U.empty

serverDiscos :: COps -> DiscoveryKindRev
             -> Rnd (DiscoveryKind, DiscoveryKindRev)
serverDiscos COps{coitem} (DiscoveryKindRev discoRev0) = do
  let ixs = [0..toEnum (olength coitem - 1)]
      inMetaGame kindId =
        IK.SetFlag Ability.MetaGame `elem` IK.iaspects (okind coitem kindId)
      keepMeta i ix = if inMetaGame (toEnum i) then ix else maxBound
  shuffled <-
    if U.null discoRev0
    then shuffle ixs
    else shuffleExcept (U.imap keepMeta discoRev0) (olength coitem) ixs
  let f (!ikMap, (!ix) : rest) !kmKind _ =
        (EM.insert (toItemKindIx ix) kmKind ikMap, rest)
      f (ikMap, []) ik _ =
        error $ "too short ixs" `showFailure` (ik, ikMap)
      (discoS, _) = ofoldlWithKey' coitem f (EM.empty, shuffled)
      udiscoRev = U.fromListN (olength coitem) shuffled
  return (discoS, DiscoveryKindRev udiscoRev)

-- | Flavours assigned by the server to item kinds, in this particular game.
-- This is total and never changes, hence implemented as vector.
-- Morally, it's indexed by @ContentId ItemKind@ and elements are @Flavour@.
newtype FlavourMap = FlavourMap (U.Vector Word16)
  deriving (Show, Binary)

emptyFlavourMap :: FlavourMap
emptyFlavourMap = FlavourMap U.empty

stdFlav :: ES.EnumSet Flavour
stdFlav = ES.fromList stdFlavList

invalidFlavourCode :: Word16
invalidFlavourCode = maxBound 


-- | Assigns flavours to item kinds. Assures no flavor is repeated for the same
-- symbol, except for items with only one permitted flavour.
rollFlavourMap :: U.Vector Word16
               -> Rnd ( EM.EnumMap (ContentId ItemKind) Flavour
                      , EM.EnumMap (ContentSymbol ItemKind) (ES.EnumSet Flavour) )
               -> ContentId ItemKind -> ItemKind
               -> Rnd ( EM.EnumMap (ContentId ItemKind) Flavour
                      , EM.EnumMap (ContentSymbol ItemKind) (ES.EnumSet Flavour) )
rollFlavourMap uFlavMeta !rnd !key !ik = case IK.iflavour ik of
  [] -> error "empty iflavour"
  [flavour] -> do
    (!assocs, !availableMap) <- rnd
    return ( EM.insert key flavour assocs
           , availableMap )
  flvs -> do
    (!assocs, !availableMap) <- rnd
    let a0 = uFlavMeta U.! toEnum (fromEnum key)
    if a0 == invalidFlavourCode then do
      if length flvs < 6 then do  -- too few to even attempt unique assignment
        flavour <- oneOf flvs
        return ( EM.insert key flavour assocs
               , availableMap )
      else do
        let available = availableMap EM.! IK.isymbol ik
            proper = ES.fromList flvs `ES.intersection` available
        assert (not (ES.null proper)
                `blame` "not enough flavours for items"
                `swith` (flvs, available, ik, availableMap)) $ do
          flavour <- oneOf $ ES.elems proper
          let availableReduced = ES.delete flavour available
          return ( EM.insert key flavour assocs
                 , EM.insert (IK.isymbol ik) availableReduced availableMap )
    else return ( EM.insert key (toEnum $ fromEnum a0) assocs
                , availableMap )

-- | Randomly chooses flavour for all item kinds for this game.
dungeonFlavourMap :: COps -> FlavourMap -> Rnd FlavourMap
dungeonFlavourMap COps{coitem} (FlavourMap flavourMapFromPreviousGame) = do
  let inMetaGame kindId =
        IK.SetFlag Ability.MetaGame `elem` IK.iaspects (okind coitem kindId)
      keepMeta i fl = if inMetaGame (toEnum i) then fl else invalidFlavourCode
      uFlavMeta = if U.null flavourMapFromPreviousGame
                  then U.replicate (olength coitem) invalidFlavourCode
                  else U.imap keepMeta flavourMapFromPreviousGame
      flavToAvailable :: EM.EnumMap Char (ES.EnumSet Flavour) -> Int -> Word16
                      -> EM.EnumMap Char (ES.EnumSet Flavour)
      flavToAvailable em i fl =
        let ik = okind coitem (toEnum i)
            setBase = EM.findWithDefault stdFlav (IK.isymbol ik) em
            setMeta = if fl == invalidFlavourCode
                      then setBase
                      else ES.delete (toEnum $ fromEnum fl) setBase
        in EM.insert (IK.isymbol ik) setMeta em
      availableMap = U.ifoldl' flavToAvailable EM.empty uFlavMeta
  (assocsFlav, _) <- ofoldlWithKey' coitem (rollFlavourMap uFlavMeta)
                                    (return (EM.empty, availableMap))
  let uFlav = U.fromListN (olength coitem)
              $ map (toEnum . fromEnum) $ EM.elems assocsFlav
  return $! FlavourMap uFlav
