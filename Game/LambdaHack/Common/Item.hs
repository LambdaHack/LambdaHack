{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | Weapons, treasure and all the other items in the game.
-- No operation in this module
-- involves the 'State' or 'Action' type.
-- TODO: Document after it's rethought and rewritten wrt separating
-- inventory manangement and items proper.
module Game.LambdaHack.Common.Item
  ( -- * Teh @Item@ type
    ItemId, Item(..), jkind, buildItem, newItem, viewItem
    -- * Inventory search
  , strongestSearch, strongestSword, strongestRegen
   -- * The item discovery types
  , ItemKindIx, Discovery, DiscoRev, serverDiscos
    -- * The @FlavourMap@ type
  , FlavourMap, emptyFlavourMap, dungeonFlavourMap
    -- * Textual description
  , partItem, partItemWs, partItemAW
  ) where

import Control.Monad
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.Hashable as Hashable
import qualified Data.Ix as Ix
import Data.List
import qualified Data.Set as S
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU

import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Flavour
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Utils.Assert

-- | A unique identifier of an item in the dungeon.
newtype ItemId = ItemId Int
  deriving (Show, Eq, Ord, Enum, Binary)

-- | An index of the kind id of an item. Clients have partial knowledge
-- how these idexes map to kind ids. They gain knowledge by identifying items.
newtype ItemKindIx = ItemKindIx Int
  deriving (Show, Eq, Ord, Enum, Ix.Ix, Hashable.Hashable, Binary)

-- | The map of item kind indexes to item kind ids.
-- The full map, as known by the server, is a bijection.
type Discovery = EM.EnumMap ItemKindIx (Kind.Id ItemKind)

-- | The reverse map to @Discovery@, needed for item creation.
type DiscoRev = EM.EnumMap (Kind.Id ItemKind) ItemKindIx

-- TODO: somehow hide from clients jeffect of unidentified items.
-- | Game items in inventories or strewn around the dungeon.
-- The fields @jsymbol@, @jname@ and @jflavour@ make it possible to refer to
-- and draw an unidentified item. Full information about item is available
-- through the @jkindIx@ index as soon as the item is identified.
data Item = Item
  { jkindIx  :: !ItemKindIx  -- ^ index pointing to the kind of the item
  , jsymbol  :: !Char        -- ^ individual map symbol
  , jname    :: !Text        -- ^ individual generic name
  , jflavour :: !Flavour     -- ^ individual flavour
  , jeffect  :: !(Effect Int)  -- ^ the effect when activated
  }
  deriving (Show, Eq, Ord, Generic)

instance Hashable.Hashable Item

instance Binary Item

-- | Recover a kind id of an item, if identified.
jkind :: Discovery -> Item -> Maybe (Kind.Id ItemKind)
jkind disco i = EM.lookup (jkindIx i) disco

serverDiscos :: Kind.Ops ItemKind -> Rnd (Discovery, DiscoRev)
serverDiscos Kind.Ops{obounds, ofoldrWithKey} = do
  let ixs = map ItemKindIx $ take (Ix.rangeSize obounds) [0..]
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
buildItem :: FlavourMap -> DiscoRev
          -> Kind.Id ItemKind -> ItemKind -> Effect Int -> Item
buildItem (FlavourMap flavour) discoRev ikChosen kind jeffect =
  let jkindIx  = discoRev EM.! ikChosen
      jsymbol  = isymbol kind
      jname    = iname kind
      jflavour =
        case iflavour kind of
          [fl] -> fl
          _ -> flavour EM.! ikChosen
  in Item{..}

-- | Generate an item based on level.
newItem :: Kind.Ops ItemKind -> FlavourMap -> DiscoRev -> Int -> Int
        -> Rnd (Item, Int, ItemKind)
newItem cops@Kind.Ops{opick, okind} flavour discoRev lvl depth = do
  ikChosen <- opick "dng" (const True)
  let kind = okind ikChosen
  jcount <- castDeep lvl depth (icount kind)
  if jcount == 0
    then -- Rare item; beware of inifite loops.
         newItem cops flavour discoRev lvl depth
    else do
      effect <- effectTrav (ieffect kind) (castDeep lvl depth)
      return ( buildItem flavour discoRev ikChosen kind effect
             , jcount
             , kind )

-- | Represent an item on the map.
viewItem :: Item -> (Char, Color.Color)
viewItem i = (jsymbol i, flavourToColor $ jflavour i)

-- | Flavours assigned by the server to item kinds, in this particular game.
newtype FlavourMap = FlavourMap (EM.EnumMap (Kind.Id ItemKind) Flavour)
  deriving (Show, Binary)

emptyFlavourMap :: FlavourMap
emptyFlavourMap = FlavourMap EM.empty

-- | Assigns flavours to item kinds. Assures no flavor is repeated,
-- except for items with only one permitted flavour.
rollFlavourMap :: Kind.Id ItemKind -> ItemKind
               -> Rnd (EM.EnumMap (Kind.Id ItemKind) Flavour, S.Set Flavour)
               -> Rnd (EM.EnumMap (Kind.Id ItemKind) Flavour, S.Set Flavour)
rollFlavourMap key ik rnd =
  let flavours = iflavour ik
  in if length flavours == 1
     then rnd
     else do
       (assocs, available) <- rnd
       let proper = S.fromList flavours `S.intersection` available
       flavour <- oneOf (S.toList proper)
       return (EM.insert key flavour assocs, S.delete flavour available)

-- | Randomly chooses flavour for all item kinds for this game.
dungeonFlavourMap :: Kind.Ops ItemKind -> Rnd FlavourMap
dungeonFlavourMap Kind.Ops{ofoldrWithKey} =
  liftM (FlavourMap . fst) $
    ofoldrWithKey rollFlavourMap (return (EM.empty, S.fromList stdFlav))

strongestItem :: [(ItemId, Item)] -> (Item -> Maybe Int)
              -> Maybe (Int, (ItemId, Item))
strongestItem is p =
  let ks = map (p . snd) is
  in case zip ks is of
    [] -> Nothing
    kis -> case maximum kis of
      (Nothing, _) -> Nothing
      (Just k, iki) -> Just (k, iki)

strongestSearch :: [(ItemId, Item)] -> Maybe (Int, (ItemId, Item))
strongestSearch is =
  strongestItem is $ \ i ->
    case jeffect i of Searching k -> Just k; _ -> Nothing

strongestSword :: Kind.COps -> [(ItemId, Item)] -> Maybe (Int, (ItemId, Item))
strongestSword Kind.COps{corule} is =
  strongestItem is $ \ i ->
    case jeffect i of
      Hurt d k | jsymbol i `elem` ritemMelee (Kind.stdRuleset corule)
        -> Just $ floor (meanDice d) + k
      _ -> Nothing

strongestRegen :: [(ItemId, Item)] -> Maybe (Int, (ItemId, Item))
strongestRegen is =
  strongestItem is $ \ i ->
    case jeffect i of Regeneration k -> Just k; _ -> Nothing

-- | The part of speech describing the item.
partItem :: Kind.Ops ItemKind -> Discovery -> Item -> (MU.Part, MU.Part)
partItem _cops disco i =
  let genericName = jname i
      flav = flavourToName $ jflavour i
  in case jkind disco i of
    Nothing ->
      -- TODO: really hide jeffect from a client that has not discovered
      -- that individual item's properties (nor item kind, if there's only
      -- one effect possible for the kind (plus effect deduction))
      (MU.Text $ flav <+> genericName, "")
    Just _ ->
      let eff = effectToSuffix $ jeffect i
      in (MU.Text genericName, MU.Text eff)

partItemWs :: Kind.Ops ItemKind -> Discovery -> Int -> Item -> MU.Part
partItemWs coitem disco jcount i =
  let (name, stats) = partItem coitem disco i
  in MU.Phrase [MU.CarWs jcount name, stats]

partItemAW :: Kind.Ops ItemKind -> Discovery -> Item -> MU.Part
partItemAW coitem disco i =
  let (name, stats) = partItem coitem disco i
  in MU.AW $ MU.Phrase [name, stats]
