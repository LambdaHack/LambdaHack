{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings
             #-}
-- | Weapons, treasure and all the other items in the game.
-- No operation in this module
-- involves the 'State' or 'Action' type.
-- TODO: Document after it's rethought and rewritten wrt separating
-- inventory manangement and items proper.
module Game.LambdaHack.Item
  ( -- * Teh @Item@ type
    ItemId, ItemBag, Item(..), jkind, buildItem, newItem, viewItem
    -- * Inventory search
  , strongestSearch, strongestSword, strongestRegen
    -- * Inventory management
  , joinItem, assignLetter
    -- * Inventory symbol operations
  , letterLabel, cmpLetterMaybe, maxLetter, letterRange
    -- * The item discovery types
  , ItemKindIx, Discoveries, DiscoRev, serverDiscos
    -- * The @FlavourMap@ type
  , FlavourMap, dungeonFlavourMap
    -- * Textual description
  , partItem, partItemNWs, partItemAW
  ) where

import Control.Monad
import Data.Binary
import Data.Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Ix as Ix
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import qualified NLP.Miniutter.English as MU

import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Effect
import Game.LambdaHack.Flavour
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Msg
import Game.LambdaHack.Random
import Game.LambdaHack.Utils.Assert

-- | A unique identifier of an item in the dungeon.
newtype ItemId = ItemId Int
  deriving (Show, Eq, Ord, Enum, Typeable)

instance Binary ItemId where
  put (ItemId n) = put n
  get = fmap ItemId get

type ItemBag = EM.EnumMap ItemId (Int, Maybe Char)

-- | An index of the kind id of an item. Clients have partial knowledge
-- how these idexes map to kind ids. They gain knowledge by identifying items.
newtype ItemKindIx = ItemKindIx Int
  deriving (Show, Eq, Ord, Enum, Ix.Ix)

instance Binary ItemKindIx where
  put (ItemKindIx i) = put i
  get = fmap ItemKindIx get

-- | The map of item kind indexes to item kind ids.
-- The full map, as known by the server, is a bijection.
type Discoveries = EM.EnumMap ItemKindIx (Kind.Id ItemKind)

-- | The reverse map to @Discoveries@, needed for item creation.
type DiscoRev    = EM.EnumMap (Kind.Id ItemKind) ItemKindIx

-- TODO: see the TODO about ipower in ItemKind.
-- TODO: define type InvSymbol = Char and move all ops to another file.
-- TODO: the list resulting from joinItem can contain items
-- with the same letter.
-- TODO: name [Item] Inventory and have some invariants, e.g. no equal letters.
-- | Game items in inventories or strewn around the dungeon.
-- The fields @jsymbol@, @jname@ and @jflavour@ make it possible to refer to
-- and draw an unidentified item. Full information about item is available
-- through the @jkindIx@ index as soon as the item is identified.
data Item = Item
  { jkindIx  :: !ItemKindIx    -- ^ index pointing to the kind of the item
  , jsymbol  :: !Char          -- ^ individual map symbol
  , jname    :: !Text          -- ^ individual generic name
  , jflavour :: !Flavour       -- ^ individual flavour
  , jpower   :: !Int           -- ^ power of the item
  }
  deriving (Show, Eq)

instance Binary Item where
  put (Item{..} ) = do
    put jkindIx
    put jsymbol
    put jname
    put jflavour
    put jpower
  get = do
    jkindIx <- get
    jsymbol <- get
    jname <- get
    jflavour <- get
    jpower <- get
    return Item{..}

-- | Recover a kind id of an item, if identified.
jkind :: Discoveries -> Item -> Maybe (Kind.Id ItemKind)
jkind disco i = EM.lookup (jkindIx i) disco

serverDiscos :: Kind.Ops ItemKind -> Rnd (Discoveries, DiscoRev)
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
        assert `failure` (ik, ikMap, "too short ixs" :: Text)
      (discoS, discoRev, _) =
        ofoldrWithKey f (EM.empty, EM.empty, shuffled)
  return (discoS, discoRev)

-- | Build an item with the given stats.
buildItem :: FlavourMap -> DiscoRev
          -> Kind.Id ItemKind -> ItemKind -> Int -> Item
buildItem (FlavourMap flavour) discoRev ikChosen kind jpower =
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
  ikChosen <- opick (T.pack "dng") (const True)
  let kind = okind ikChosen
  jcount <- rollDeep lvl depth (icount kind)
  if jcount == 0
    then  -- Rare item; beware of inifite loops.
      newItem cops flavour discoRev lvl depth
    else do
      jpower <- rollDeep lvl depth (ipower kind)
      return ( buildItem flavour discoRev ikChosen kind jpower
             , jcount
             , kind )

-- | Represent an item on the map.
viewItem :: Item -> (Char, Color.Color)
viewItem i = (jsymbol i, flavourToColor $ jflavour i)

-- | Flavours assigned by the server to item kinds, in this particular game.
newtype FlavourMap = FlavourMap (EM.EnumMap (Kind.Id ItemKind) Flavour)
  deriving Show

instance Binary FlavourMap where
  put (FlavourMap m) = put m
  get = fmap FlavourMap get

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

-- | Assigns a letter to an item, for inclusion
-- in the inventory of a hero. Takes a remembered
-- letter and a starting letter.
assignLetter :: Maybe Char -> Char -> [Char] -> Maybe Char
assignLetter r c cs =
  case r of
    Just l | l `elem` allowed -> Just l
    _ -> listToMaybe free
 where
  current    = ES.fromList cs
  allLetters = ['a'..'z'] ++ ['A'..'Z']
  candidates = take (length allLetters) $
                 drop (fromJust (findIndex (== c) allLetters)) $
                   cycle allLetters
  free       = filter (\x -> not (x `ES.member` current)) candidates
  allowed    = '$' : free

cmpLetter :: Char -> Char -> Ordering
cmpLetter x y = compare (isUpper x, toLower x) (isUpper y, toLower y)

cmpLetterMaybe :: Maybe Char -> Maybe Char -> Ordering
cmpLetterMaybe Nothing  Nothing   = EQ
cmpLetterMaybe Nothing  (Just _)  = GT
cmpLetterMaybe (Just _) Nothing   = LT
cmpLetterMaybe (Just l) (Just l') = cmpLetter l l'

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy cmp x y = case cmp x y of
                  LT  ->  y
                  _   ->  x

maxLetter :: Char -> Char -> Char
maxLetter = maxBy cmpLetter

mergeLetter :: Maybe Char -> Maybe Char -> Maybe Char
mergeLetter = mplus

letterRange :: [Char] -> Text
letterRange ls =
  sectionBy (sortBy cmpLetter ls) Nothing
 where
  succLetter c d = ord d - ord c == 1

  sectionBy []     Nothing      = T.empty
  sectionBy []     (Just (c,d)) = finish (c,d)
  sectionBy (x:xs) Nothing      = sectionBy xs (Just (x,x))
  sectionBy (x:xs) (Just (c,d))
    | succLetter d x            = sectionBy xs (Just (c,x))
    | otherwise                 = finish (c,d) <> sectionBy xs (Just (x,x))

  finish (c,d) | c == d         = T.pack [c]
               | succLetter c d = T.pack $ [c, d]
               | otherwise      = T.pack $ [c, '-', d]

letterLabel :: Maybe Char -> MU.Part
letterLabel Nothing  = MU.Text $ T.pack "   "
letterLabel (Just c) = MU.Text $ T.pack $ c : " -"

joinItem :: (Int, Maybe Char) -> (Int, Maybe Char) -> (Int, Maybe Char)
joinItem (i, a) (j, b) = (i + j, mergeLetter a b)

strongestItem :: [Item] -> (Item -> Bool) -> Maybe Item
strongestItem is p =
  let cmp = comparing jpower
      igs = filter p is
  in case igs of
    [] -> Nothing
    _  -> Just $ maximumBy cmp igs

strongestSearch :: Kind.Ops ItemKind -> Discoveries -> [Item] -> Maybe Item
strongestSearch Kind.Ops{okind} disco bitems =
  strongestItem bitems $ \ i ->
    case jkind disco i of
      Nothing -> False
      Just ik -> ieffect (okind ik) == Searching

-- TODO: generalise, in particular take base damage into account
strongestSword :: Kind.COps -> [Item] -> Maybe Item
strongestSword Kind.COps{corule} bitems =
  strongestItem bitems $ \ i ->
    jsymbol i `elem` (ritemMelee $ Kind.stdRuleset corule)

strongestRegen :: Kind.Ops ItemKind -> Discoveries -> [Item] -> Maybe Item
strongestRegen Kind.Ops{okind} disco bitems =
  strongestItem bitems $ \ i ->
    case jkind disco i of
      Nothing -> False
      Just ik -> ieffect (okind ik) == Regeneration

-- | The part of speech describing the item.
partItem :: Kind.Ops ItemKind -> Discoveries -> Item -> (MU.Part, MU.Part)
partItem Kind.Ops{okind} disco i =
  let genericName = jname i
      flav = flavourToName $ jflavour i
  in case jkind disco i of
    Nothing -> (MU.Text $ flav <+> genericName, "")
    Just ik ->
      let kind = okind ik
          eff = effectToSuffix (ieffect kind)
          pwr = if jpower i == 0
                then ""
                else "(+" <> showT (jpower i) <> ")"
      in (MU.Text genericName, MU.Text $ eff <+> pwr)

partItemNWs :: Kind.Ops ItemKind -> Discoveries -> Int -> Item -> MU.Part
partItemNWs coitem disco jcount i =
  let (name, stats) = partItem coitem disco i
  in MU.Phrase [MU.NWs jcount name, stats]

partItemAW :: Kind.Ops ItemKind -> Discoveries -> Item -> MU.Part
partItemAW coitem disco i =
  let (name, stats) = partItem coitem disco i
  in MU.AW $ MU.Phrase [name, stats]
