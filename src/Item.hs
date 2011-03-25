module Item where

import Data.Binary
import Data.Set as S
import Data.List as L
import Data.Maybe
import Data.Char
import Data.Function
import Control.Monad

import Random
import ItemKind

data Item = Item
             { icount  :: Int,
               ikind   :: ItemKind,
               iletter :: Maybe Char }  -- inventory identifier
  deriving Show

type Discoveries = S.Set ItemKind

equalItemKind :: Item -> Item -> Bool
equalItemKind = (==) `on` ikind

equalItemLetter :: Item -> Item -> Bool
equalItemLetter = (==) `on` iletter

instance Binary Item where
  put (Item icount ikind iletter) = put icount >> put ikind >> put iletter
  get = liftM3 Item get get get

-- | Generate an item.
newItem :: Int -> Frequency ItemKind -> Rnd Item
newItem n ftp =
  do
    tp <- frequency ftp
    item <- itemStrength n tp
    nr <- itemQuantity n tp
    return (Item nr item (itemLetter tp))

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
    candidates = take (length allLetters) (drop (fromJust (L.findIndex (==c) allLetters)) (cycle allLetters))
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

    sectionBy []     Nothing                  = ""
    sectionBy []     (Just (c,d))             = finish (c,d)
    sectionBy (x:xs) Nothing                  = sectionBy xs (Just (x,x))
    sectionBy (x:xs) (Just (c,d)) | succLetter d x
                                              = sectionBy xs (Just (c,x))
                                  | otherwise
                                              = finish (c,d) ++ sectionBy xs (Just (x,x))

    finish (c,d) | c == d         = [c]
                 | succLetter c d = [c,d]
                 | otherwise      = [c,'-',d]

letterLabel :: Maybe Char -> String
letterLabel Nothing  = "    "
letterLabel (Just c) = c : " - "

-- | Adds an item to a list of items, joining equal items.
-- Also returns the joined item.
joinItem :: Item -> [Item] -> (Item,[Item])
joinItem i is = case findItem (equalItemKind i) is of
                  Nothing     -> (i, i : is)
                  Just (j,js) -> let n = i { icount = icount i + icount j,
                                             iletter = mergeLetter (iletter j) (iletter i) }
                                 in (n, n : js)

-- | Removes an item from a list of items. Takes an equality function (i.e., by letter or
-- ny kind) as an argument.
removeItemBy :: (Item -> Item -> Bool) -> Item -> [Item] -> [Item]
removeItemBy eq i = concatMap $ \ x ->
                    if eq i x
                      then let remaining = icount x - icount i
                           in  if remaining > 0
                                 then [x { icount = remaining }]
                                 else []
                      else [x]

removeItemByLetter = removeItemBy equalItemLetter
removeItemByKind   = removeItemBy equalItemKind

-- | Finds an item in a list of items.
findItem :: (Item -> Bool) -> [Item] -> Maybe (Item, [Item])
findItem p is = findItem' [] is
  where
    findItem' acc []     = Nothing
    findItem' acc (i:is)
      | p i              = Just (i, reverse acc ++ is)
      | otherwise        = findItem' (i:acc) is

strongestSword :: [Item] -> Int
strongestSword l =
  let aux acc (Item { ikind = Sword i }) = max acc i
      aux acc _ = acc
  in  foldl' aux 0 l
