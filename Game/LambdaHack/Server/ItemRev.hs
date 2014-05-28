{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Server types and operations for items that don't involve server state
-- nor our custom monads.
module Game.LambdaHack.Server.ItemRev
  ( ItemRev, buildItem, newItem
    -- * Item discovery types
  , DiscoRev, serverDiscos, ItemSeedDict
    -- * The @FlavourMap@ type
  , FlavourMap, emptyFlavourMap, dungeonFlavourMap
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.HashMap.Strict as HM
import qualified Data.Ix as Ix
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)

import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Content.ItemKind

-- | The reverse map to @Discovery@, needed for item creation.
type DiscoRev = EM.EnumMap (Kind.Id ItemKind) ItemKindIx

-- | The map of item ids to item seeds.
-- The full map is known by the server.
type ItemSeedDict = EM.EnumMap ItemId ItemSeed

serverDiscos :: Kind.Ops ItemKind -> Rnd (Discovery, DiscoRev)
serverDiscos Kind.Ops{obounds, ofoldrWithKey} = do
  let ixs = map toEnum $ take (Ix.rangeSize obounds) [0..]
      shuffle :: Eq a => [a] -> Rnd [a]
      shuffle [] = return []
      shuffle l = do
        x <- oneOf l
        fmap (x :) $ shuffle (delete x l)
  shuffled <- shuffle ixs
  let f ik _ (ikMap, ikRev, ix : rest) =
        (EM.insert ix ik ikMap, EM.insert ik ix ikRev, rest)
      f ik  _ (ikMap, _, []) =
        assert `failure` "too short ixs" `twith` (ik, ikMap)
      (discoS, discoRev, _) =
        ofoldrWithKey f (EM.empty, EM.empty, shuffled)
  return (discoS, discoRev)

-- | Build an item with the given stats.
buildItem :: FlavourMap -> DiscoRev -> Kind.Id ItemKind -> ItemKind -> LevelId
          -> Item
buildItem (FlavourMap flavour) discoRev ikChosen kind jlid =
  let jkindIx  = discoRev EM.! ikChosen
      jsymbol  = isymbol kind
      jname    = iname kind
      jflavour =
        case iflavour kind of
          [fl] -> fl
          _ -> flavour EM.! ikChosen
      jfeature = ifeature kind
      jweight = iweight kind
  in Item{..}

-- | Generate an item based on level.
newItem :: Kind.Ops ItemKind -> FlavourMap -> DiscoRev
        -> Frequency Text -> LevelId -> Int -> Int
        -> Rnd (ItemKnown, ItemSeed, Int)
newItem coitem@Kind.Ops{opick, okind}
        flavour discoRev itemFreq jlid ln depth = do
  itemGroup <- frequency itemFreq
  let castItem :: Int -> Rnd (ItemKnown, ItemSeed, Int)
      castItem 0 = do
        let zeroedFreq = setFreq itemFreq itemGroup 0
            newFreq = if nullFreq zeroedFreq
                      then toFreq "fallback item" [(1, "fallback item")]
                      else zeroedFreq
        newItem coitem flavour discoRev newFreq jlid ln depth
      castItem count = do
        ikChosen <- fmap (fromMaybe $ assert `failure` itemGroup)
                    $ opick itemGroup (const True)
        let kind = okind ikChosen
        jcount <- castDice ln depth (icount kind)
        if jcount == 0 then
          castItem $ count - 1
        else do
          seed <- fmap toEnum random
          return ( ( buildItem flavour discoRev ikChosen kind jlid
                   , seedToAspectsEffects seed kind ln depth )
                 , seed
                 , jcount )
  castItem 10

-- | Flavours assigned by the server to item kinds, in this particular game.
newtype FlavourMap = FlavourMap (EM.EnumMap (Kind.Id ItemKind) Flavour)
  deriving (Show, Binary)

emptyFlavourMap :: FlavourMap
emptyFlavourMap = FlavourMap EM.empty

-- | Assigns flavours to item kinds. Assures no flavor is repeated,
-- except for items with only one permitted flavour.
rollFlavourMap :: S.Set Flavour -> Kind.Id ItemKind -> ItemKind
               -> Rnd ( EM.EnumMap (Kind.Id ItemKind) Flavour
                      , EM.EnumMap Char (S.Set Flavour) )
               -> Rnd ( EM.EnumMap (Kind.Id ItemKind) Flavour
                      , EM.EnumMap Char (S.Set Flavour) )
rollFlavourMap fullFlavSet key ik rnd =
  let flavours = iflavour ik
  in if length flavours == 1
     then rnd
     else do
       (assocs, availableMap) <- rnd
       let available = EM.findWithDefault fullFlavSet (isymbol ik) availableMap
           proper = S.fromList flavours `S.intersection` available
       assert (not (S.null proper)
               `blame` "not enough flavours for items"
               `twith` (flavours, available, ik, availableMap)) $ do
         flavour <- oneOf (S.toList proper)
         let availableReduced = S.delete flavour available
         return ( EM.insert key flavour assocs
                , EM.insert (isymbol ik) availableReduced availableMap)

-- | Randomly chooses flavour for all item kinds for this game.
dungeonFlavourMap :: Kind.Ops ItemKind -> Rnd FlavourMap
dungeonFlavourMap Kind.Ops{ofoldrWithKey} =
  liftM (FlavourMap . fst) $
    ofoldrWithKey (rollFlavourMap (S.fromList stdFlav))
                  (return (EM.empty, EM.empty))

-- | Reverse item map, for item creation, to keep items and item identifiers
-- in bijection.
type ItemRev = HM.HashMap ItemKnown ItemId
