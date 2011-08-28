module Item where

import Data.Binary
import Data.Set as S
import Data.List as L
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Maybe
import Data.Char
import Data.Function
import Control.Monad

import Random
import ItemKind
import qualified Color
import Flavour

data Item = Item
  { ikind    :: !ItemId,
    ipower   :: !Int,  -- TODO: see the TODO about jpower
    iletter  :: Maybe Char,  -- ^ inventory identifier
    icount   :: !Int }
  deriving Show

instance Binary Item where
  put (Item ikind ipower iletter icount ) =
    put ikind >> put ipower >> put iletter >> put icount
  get = liftM4 Item get get get get

type Assocs = M.Map ItemId Flavour  -- TODO: rewrite and move elsewhere

type Discoveries = S.Set ItemId

-- | Assinges flavours to item kinds. Assures no flavor is repeated,
-- except for items with only one permitted flavour.
rollAssocs :: ItemId -> [Flavour] ->
              Rnd (Assocs, S.Set Flavour) ->
              Rnd (Assocs, S.Set Flavour)
rollAssocs key flavours rnd =
  if L.length flavours == 1
  then rnd
  else do
    (assocs, available) <- rnd
    let proper = S.fromList flavours `S.intersection` available
    flavour <- oneOf (S.toList proper)
    return (M.insert key flavour assocs, S.delete flavour available)

-- | Randomly chooses flavour for all item kinds for this game.
dungeonAssocs :: Rnd Assocs
dungeonAssocs =
  liftM fst $
  M.foldWithKey rollAssocs (return (M.empty, S.fromList stdFlav)) itemFlavours

getFlavour :: Assocs -> ItemId -> Flavour
getFlavour assocs ik =
  let kind = ItemKind.getKind ik
  in  case jflavour kind of
        []  -> error "getFlavour"
        [f] -> f
        _:_ -> assocs M.! ik

viewItem :: ItemId -> Assocs -> (Char, Color.Color)
viewItem ik assocs = (jsymbol (getKind ik), flavourToColor $ getFlavour assocs ik)

itemLetter :: ItemKind -> Maybe Char
itemLetter ik = if jsymbol ik == '$' then Just '$' else Nothing

-- | Generate an item.
newItem :: Int -> Rnd Item
newItem lvl = do
  ikChosen <- frequency itemFrequency
  let kind = getKind ikChosen
  count <- rollQuad lvl (jcount kind)
  if count == 0
    then newItem lvl  -- Rare item; beware of inifite loops.
    else do
      power <- rollQuad lvl (jpower kind)
      return $ Item ikChosen power (itemLetter kind) count

-- | Assigns a letter to an item, for inclusion
-- in the inventory of a hero. Takes a remembered
-- letter and a starting letter.
assignLetter :: Maybe Char -> Char -> [Item] -> Maybe Char
assignLetter r c is =
    case r of
      Just l | l `L.elem` allowed -> Just l
      _ -> listToMaybe free
  where
    current    = S.fromList (concatMap (maybeToList . iletter) is)
    allLetters = ['a'..'z'] ++ ['A'..'Z']
    candidates = take (length allLetters) $
                   drop (fromJust (L.findIndex (==c) allLetters)) $
                     cycle allLetters
    free       = L.filter (\x -> not (x `S.member` current)) candidates
    allowed    = '$' : free

cmpLetter :: Char -> Char -> Ordering
cmpLetter x y = compare (isUpper x, toLower x) (isUpper y, toLower y)

cmpLetter' :: Maybe Char -> Maybe Char -> Ordering
cmpLetter' Nothing  Nothing   = EQ
cmpLetter' Nothing  (Just _)  = GT
cmpLetter' (Just _) Nothing   = LT
cmpLetter' (Just l) (Just l') = cmpLetter l l'

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy cmp x y = case cmp x y of
                  LT  ->  y
                  _   ->  x

maxLetter = maxBy cmpLetter

mergeLetter :: Maybe Char -> Maybe Char -> Maybe Char
mergeLetter = mplus

letterRange :: [Char] -> String
letterRange xs = sectionBy (sortBy cmpLetter xs) Nothing
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
-- TODO: the resulting list can contain items with the same letter.
-- TODO: name [Item] Inventory and have some invariants, e.g. no equal letters.
joinItem :: Item -> [Item] -> (Item, [Item])
joinItem i is =
  case findItem (equalItemIdentity i) is of
    Nothing     -> (i, i : is)
    Just (j,js) -> let n = i { icount = icount i + icount j,
                               iletter = mergeLetter (iletter j) (iletter i) }
                   in (n, n : js)

-- | Removes an item from a list of items.
-- Takes an equality function (i.e., by letter or ny kind) as an argument.
removeItemBy :: (Item -> Item -> Bool) -> Item -> [Item] -> [Item]
removeItemBy eq i = concatMap $ \ x ->
                    if eq i x
                      then let remaining = icount x - icount i
                           in  if remaining > 0
                                 then [x { icount = remaining }]
                                 else []
                      else [x]

equalItemIdentity :: Item -> Item -> Bool
equalItemIdentity i1 i2 = ipower i1 == ipower i2 && ikind i1 == ikind i2

removeItemByIdentity = removeItemBy equalItemIdentity

equalItemLetter :: Item -> Item -> Bool
equalItemLetter = (==) `on` iletter

removeItemByLetter = removeItemBy equalItemLetter

-- | Finds an item in a list of items.
findItem :: (Item -> Bool) -> [Item] -> Maybe (Item, [Item])
findItem p is = findItem' [] is
  where
    findItem' acc []     = Nothing
    findItem' acc (i:is)
      | p i              = Just (i, reverse acc ++ is)
      | otherwise        = findItem' (i:acc) is

strongestItem :: [Item] -> String -> Maybe Item
strongestItem is groupName =
  let cmp = compare `on` ipower
      igs = L.filter (\ i -> jname (getKind (ikind i)) == groupName) is
  in  case igs of
        [] -> Nothing
        _  -> Just $ L.maximumBy cmp igs

itemPrice :: Item -> Int
itemPrice i =
  case jname (getKind (ikind i)) of
    "gold piece" -> icount i
    "gem" -> 100
    _ -> 0
