module Item where

import Data.Binary
import Data.Map as M
import Data.Set as S
import Data.List as L
import Data.Maybe
import Data.Char
import Data.Function
import Control.Monad

import Display
import Geometry
import Random

data Item = Item
             { icount  :: Int,
               itype   :: ItemType,
               iletter :: Maybe Char }  -- inventory identifier
  deriving Show

data ItemType =
   Ring
 | Scroll
 | Potion PotionType
 | Wand
 | Amulet
 | Gem
 | Gold
 | Sword Int
 deriving (Eq, Ord, Show)

data PotionType =
   PotionWater
 | PotionHealing
 deriving (Eq, Ord, Show)

data Appearance =
   Clear
 | White
 deriving (Eq, Show)

type Assocs = M.Map ItemType Appearance
type Discoveries = S.Set ItemType

equalItemType :: Item -> Item -> Bool
equalItemType = (==) `on` itype

equalItemLetter :: Item -> Item -> Bool
equalItemLetter = (==) `on` iletter

potionType :: PotionType -> String -> String
potionType PotionWater   s = s ++ " of water"
potionType PotionHealing s = s ++ " of healing"

appearance :: Appearance -> String -> String
appearance Clear s = "clear " ++ s
appearance White s = "white " ++ s

instance Binary Item where
  put (Item icount itype iletter) = put icount >> put itype >> put iletter
  get = liftM3 Item get get get

instance Binary ItemType where
  put Ring       = putWord8 0
  put Scroll     = putWord8 1
  put (Potion t) = putWord8 2 >> put t
  put Wand       = putWord8 3
  put Amulet     = putWord8 4
  put Gem        = putWord8 5
  put Gold       = putWord8 6
  put (Sword i)  = putWord8 7 >> put i
  get = do
          tag <- getWord8
          case tag of
            0 -> return Ring
            1 -> return Scroll
            2 -> liftM Potion get
            3 -> return Wand
            4 -> return Amulet
            5 -> return Gem
            6 -> return Gold
            7 -> liftM Sword get

instance Binary PotionType where
  put PotionWater   = putWord8 0
  put PotionHealing = putWord8 1
  get = do
          tag <- getWord8
          case tag of
            0 -> return PotionWater
            1 -> return PotionHealing

instance Binary Appearance where
  put Clear = putWord8 0
  put White = putWord8 1
  get = do
          tag <- getWord8
          case tag of
            0 -> return Clear
            1 -> return White

itemFrequency :: Frequency ItemType
itemFrequency =
  Frequency
  [
    (100, Gold),
    (70,  Sword (-1)),
    (30,  Gem),
    (20,  Ring),
    (30,  Scroll),
    (10,  Wand),
    (10,  Amulet),
    (30,  Potion PotionWater),
    (20,  Potion PotionHealing)
  ]

itemQuantity :: Int -> ItemType -> Rnd Int
itemQuantity n Gold = (2 * n) *~ d 8
itemQuantity _ _    = return 1

itemStrength :: Int -> ItemType -> Rnd ItemType
itemStrength n (Sword _) =
  do
    r <- d (2 + n `div` 2)
    return $ Sword $ (n + 1) `div` 3 + r
itemStrength _ tp        = return tp

itemLetter :: ItemType -> Maybe Char
itemLetter Gold = Just '$'
itemLetter _    = Nothing

-- | Generate an item.
newItem :: Int -> Frequency ItemType -> Rnd Item
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

viewItem :: ItemType -> Assocs -> (Char, Attr -> Attr)
viewItem i a = viewItem' i (M.lookup i a)
  where
    viewItem' (Sword {})  _            = (')', id)
    viewItem' Ring        _            = ('=', id)
    viewItem' Scroll      _            = ('?', id)
    viewItem' (Potion {}) (Just Clear) = ('!', setBold . setFG blue)
    viewItem' (Potion {}) (Just White) = ('!', setBold . setFG white)
    viewItem' (Potion {}) _            = ('!', id)
    viewItem' Wand        _            = ('/', id)
    viewItem' Gold        _            = ('$', setBold . setFG yellow)
    viewItem' Gem         _            = ('*', setFG red)
    viewItem' Amulet      _            = ('"', id)
    viewItem' _           _            = ('~', id)

-- | Adds an item to a list of items, joining equal items.
-- Also returns the joined item.
joinItem :: Item -> [Item] -> (Item,[Item])
joinItem i is = case findItem (equalItemType i) is of
                  Nothing     -> (i, i : is)
                  Just (j,js) -> let n = i { icount = icount i + icount j,
                                             iletter = mergeLetter (iletter j) (iletter i) }
                                 in (n, n : js)

-- | Removes an item from a list of items. Takes an equality function (i.e., by letter or
-- by type) as an argument.
removeItemBy :: (Item -> Item -> Bool) -> Item -> [Item] -> [Item]
removeItemBy eq i = concatMap $ \ x ->
                    if eq i x
                      then let remaining = icount x - icount i
                           in  if remaining > 0
                                 then [x { icount = remaining }]
                                 else []
                      else [x]

removeItemByLetter = removeItemBy equalItemLetter
removeItemByType   = removeItemBy equalItemType

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
  let aux acc (Item { itype = Sword i }) = max acc i
      aux acc _ = acc
  in  foldl aux 0 l

makeObject :: Int -> (String -> String) -> String -> String
makeObject 1 adj obj = let b = adj obj
                       in  case b of
                             (c:_) | c `elem` "aeio" -> "an " ++ b
                             _                       -> "a " ++ b
makeObject n adj obj = show n ++ " " ++ adj (obj ++ "s")
