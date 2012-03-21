-- | Weapons, treasure and all the other items in the game.
-- No operation in this module
-- involves the 'State' or 'Action' type.
-- TODO: Document after it's rethought and rewritten wrt separating
-- inventory manangement and items proper.
module Game.LambdaHack.Item
  ( -- * Teh @Item@ type
    Item(..), newItem, viewItem, itemPrice
    -- * Inventory search
  , strongestSearch, strongestSword, strongestRegen
    -- * Inventory management
  , joinItem, removeItemByLetter, equalItemIdentity, removeItemByIdentity
  , assignLetter
    -- * Inventory symbol operations
  , letterLabel, cmpLetterMaybe, maxLetter, letterRange
    -- * The @FlavourMap@ type
  , FlavourMap, getFlavour, dungeonFlavourMap
    -- * The @Discoveries@ type
  , Discoveries
  ) where

import Data.Binary
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Char
import Data.Function
import Data.Ord
import Control.Monad

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Random
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Flavour
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Effect

-- TODO: see the TODO about ipower in ItemKind.
-- TODO: define type InvSymbol = Char and move all ops to another file.
-- TODO: perhaps remove jletter and jcount? Should inventory semantics
-- be separate from item semantics?
-- TODO: the list resulting from joinItem can contain items
-- with the same letter.
-- TODO: name [Item] Inventory and have some invariants, e.g. no equal letters.
-- | Game items in inventories or strewn around the dungeon.
data Item = Item
  { jkind   :: !(Kind.Id ItemKind)  -- ^ kind of the item
  , jpower  :: !Int                 -- ^ power of the item
  , jletter :: Maybe Char           -- ^ inventory symbol
  , jcount  :: !Int                 -- ^ inventory count
  }
  deriving Show

instance Binary Item where
  put (Item ik ip il ic ) =
    put ik >> put ip >> put il >> put ic
  get = liftM4 Item get get get get

-- | Generate an item.
newItem :: Kind.Ops ItemKind -> Int -> Int -> Rnd Item
newItem cops@Kind.Ops{opick, okind} lvl depth = do
  ikChosen <- opick "dng" (const True)
  let kind = okind ikChosen
  count <- rollDeep lvl depth (icount kind)
  if count == 0
    then newItem cops lvl depth  -- Rare item; beware of inifite loops.
    else do
      power <- rollDeep lvl depth (ipower kind)
      return $ Item ikChosen power (itemLetter kind) count

-- | Represent an item on the map.
viewItem :: Kind.Ops ItemKind -> Kind.Id ItemKind -> FlavourMap
         -> (Char, Color.Color)
viewItem cops@Kind.Ops{osymbol} ik assocs =
  (osymbol ik, flavourToColor $ getFlavour cops assocs ik)

-- | Price an item, taking count into consideration.
itemPrice :: Kind.Ops ItemKind -> Item -> Int
itemPrice Kind.Ops{osymbol} i =
  case osymbol (jkind i) of
    '$' -> jcount i
    '*' -> jcount i * 100
    _   -> 0

-- | The type of already discovered items.
type Discoveries = S.Set (Kind.Id ItemKind)

-- Could be optimized to IntMap and IntSet, but won't ever be a bottleneck,
-- unless we have thousands of item kinds.
-- TODO: rewrite and move elsewhere
-- | Flavours assigned to items in this game.
type FlavourMap = M.Map (Kind.Id ItemKind) Flavour

-- | Assigns flavours to item kinds. Assures no flavor is repeated,
-- except for items with only one permitted flavour.
rollFlavourMap :: Kind.Id ItemKind -> ItemKind
               -> Rnd (FlavourMap, S.Set Flavour)
               -> Rnd (FlavourMap, S.Set Flavour)
rollFlavourMap key ik rnd =
  let flavours = iflavour ik
  in if L.length flavours == 1
     then rnd
     else do
       (assocs, available) <- rnd
       let proper = S.fromList flavours `S.intersection` available
       flavour <- oneOf (S.toList proper)
       return (M.insert key flavour assocs, S.delete flavour available)

-- | Randomly chooses flavour for all item kinds for this game.
dungeonFlavourMap :: Kind.Ops ItemKind -> Rnd FlavourMap
dungeonFlavourMap Kind.Ops{ofoldrWithKey} =
  liftM fst $
    ofoldrWithKey rollFlavourMap (return (M.empty, S.fromList stdFlav))

getFlavour :: Kind.Ops ItemKind -> FlavourMap -> Kind.Id ItemKind -> Flavour
getFlavour Kind.Ops{okind} assocs ik =
  let kind = okind ik
  in case iflavour kind of
    []  -> assert `failure` (assocs, ik, kind)
    [f] -> f
    _:_ -> assocs M.! ik

itemLetter :: ItemKind -> Maybe Char
itemLetter ik = if isymbol ik == '$' then Just '$' else Nothing

-- | Assigns a letter to an item, for inclusion
-- in the inventory of a hero. Takes a remembered
-- letter and a starting letter.
assignLetter :: Maybe Char -> Char -> [Item] -> Maybe Char
assignLetter r c is =
  case r of
    Just l | l `elem` allowed -> Just l
    _ -> listToMaybe free
 where
  current    = S.fromList (mapMaybe jletter is)
  allLetters = ['a'..'z'] ++ ['A'..'Z']
  candidates = take (length allLetters) $
                 drop (fromJust (L.findIndex (== c) allLetters)) $
                   cycle allLetters
  free       = L.filter (\x -> not (x `S.member` current)) candidates
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

letterRange :: [Char] -> String
letterRange ls =
  sectionBy (L.sortBy cmpLetter ls) Nothing
 where
  succLetter c d = ord d - ord c == 1

  sectionBy []     Nothing      = ""
  sectionBy []     (Just (c,d)) = finish (c,d)
  sectionBy (x:xs) Nothing      = sectionBy xs (Just (x,x))
  sectionBy (x:xs) (Just (c,d))
    | succLetter d x            = sectionBy xs (Just (c,x))
    | otherwise                 = finish (c,d) ++ sectionBy xs (Just (x,x))

  finish (c,d) | c == d         = [c]
               | succLetter c d = [c,d]
               | otherwise      = [c,'-',d]

letterLabel :: Maybe Char -> String
letterLabel Nothing  = "    "
letterLabel (Just c) = c : " - "

-- | Adds an item to a list of items, joining equal items.
-- Also returns the joined item.
joinItem :: Item -> [Item] -> (Item, [Item])
joinItem i is =
  case findItem (equalItemIdentity i) is of
    Nothing     -> (i, i : is)
    Just (j,js) -> let n = i { jcount = jcount i + jcount j,
                               jletter = mergeLetter (jletter j) (jletter i) }
                   in (n, n : js)

-- | Removes an item from a list of items.
-- Takes an equality function (i.e., by letter or ny kind) as an argument.
removeItemBy :: (Item -> Item -> Bool) -> Item -> [Item] -> [Item]
removeItemBy eq i = concatMap $ \ x ->
  if eq i x
  then let remaining = jcount x - jcount i
       in if remaining > 0
          then [x { jcount = remaining }]
          else []
  else [x]

equalItemIdentity :: Item -> Item -> Bool
equalItemIdentity i1 i2 = jpower i1 == jpower i2 && jkind i1 == jkind i2

removeItemByIdentity :: Item -> [Item] -> [Item]
removeItemByIdentity = removeItemBy equalItemIdentity

equalItemLetter :: Item -> Item -> Bool
equalItemLetter = (==) `on` jletter

removeItemByLetter :: Item -> [Item] -> [Item]
removeItemByLetter = removeItemBy equalItemLetter

-- | Finds an item in a list of items.
findItem :: (Item -> Bool) -> [Item] -> Maybe (Item, [Item])
findItem p =
  findItem' []
 where
  findItem' _   []     = Nothing
  findItem' acc (i:is)
    | p i              = Just (i, reverse acc ++ is)
    | otherwise        = findItem' (i:acc) is

strongestItem :: [Item] -> (Item -> Bool) -> Maybe Item
strongestItem is p =
  let cmp = comparing jpower
      igs = L.filter p is
  in case igs of
    [] -> Nothing
    _  -> Just $ L.maximumBy cmp igs

strongestSearch :: Kind.Ops ItemKind -> [Item] -> Maybe Item
strongestSearch Kind.Ops{okind} bitems =
  strongestItem bitems $ \ i -> (ieffect $ okind $ jkind i) == Searching

-- TODO: generalise, in particular take base damage into account
strongestSword :: Kind.COps -> [Item] -> Maybe Item
strongestSword Kind.COps{coitem=Kind.Ops{osymbol}, corule} bitems =
  strongestItem bitems $ \ i -> (osymbol $ jkind i)
                                `elem` (ritemMelee $ Kind.stdRuleset corule)

strongestRegen :: Kind.Ops ItemKind -> [Item] -> Maybe Item
strongestRegen Kind.Ops{okind} bitems =
  strongestItem bitems $ \ i -> (ieffect $ okind $ jkind i) == Regeneration
