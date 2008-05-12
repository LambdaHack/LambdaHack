module Item where

import Data.Binary
import Data.Map as M
import Data.Set as S
import Data.List as L
import Data.Maybe
import Data.Char
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
    (30,  Gem),
    (20,  Ring),
    (40,  Scroll),
    (20,  Wand),
    (10,  Amulet),
    (30,  Potion PotionWater),
    (10,  Potion PotionHealing)
  ]

itemQuantity :: Int -> ItemType -> Rnd Int
itemQuantity n Gold = (2 * n) *~ d 8
itemQuantity _ _    = return 1

itemLetter :: ItemType -> Maybe Char
itemLetter Gold = Just '$'
itemLetter _    = Nothing

-- | Generate an item.
newItem :: Int -> Frequency ItemType -> Rnd Item
newItem n ftp =
  do
    tp <- frequency ftp
    nr <- itemQuantity n tp
    return (Item nr tp (itemLetter tp))

-- | Assigns a letter to an item, for inclusion
-- in the inventory of the player. Takes a remembered
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

viewItem :: ItemType -> (Char, Attr -> Attr)
viewItem Ring                   = ('=', id)
viewItem Scroll                 = ('?', id)
viewItem (Potion PotionWater)   = ('!', setBold . setFG blue)
viewItem (Potion PotionHealing) = ('!', setBold . setFG white)
viewItem Wand                   = ('/', id)
viewItem Gold                   = ('$', setBold . setFG yellow)
viewItem Gem                    = ('*', setFG red)
viewItem _                      = ('~', id)

-- | Adds an item to a list of items, joining equal items.
-- Also returns the joined item.
joinItem :: Item -> [Item] -> (Item,[Item])
joinItem i is = case findItem (\ j -> itype i == itype j) is of
                  Nothing     -> (i, i : is)
                  Just (j,js) -> let n = i { icount = icount i + icount j,
                                             iletter = mergeLetter (iletter j) (iletter i) }
                                 in (n, n : js)

-- | Finds an item in a list of items.
findItem :: (Item -> Bool) -> [Item] -> Maybe (Item, [Item])
findItem p is = findItem' [] is
  where
    findItem' acc []     = Nothing
    findItem' acc (i:is)
      | p i              = Just (i, reverse acc ++ is)
      | otherwise        = findItem' (i:acc) is

makeObject :: Int -> (String -> String) -> String -> String
makeObject 1 adj obj = let b = adj obj
                       in  case b of
                             (c:_) | c `elem` "aeio" -> "an " ++ b
                             _                       -> "a " ++ b
makeObject n adj obj = show n ++ " " ++ adj (obj ++ "s")

objectItem :: Assocs -> Discoveries -> Int -> ItemType -> String
objectItem _ _ n Ring       = makeObject n id "ring"
objectItem _ _ n Scroll     = makeObject n id "scroll"
objectItem a d n (Potion t) = makeObject n (identified a d (Potion t)) "potion"
objectItem _ _ n Wand       = makeObject n id "wand"
objectItem _ _ n Amulet     = makeObject n id "amulet"
objectItem _ _ n Gem        = makeObject n id "gem"
objectItem _ _ n Gold       = makeObject n id "gold piece"

identified :: Assocs -> Discoveries -> ItemType -> String -> String
identified a d i
  | i `S.member` d = case i of
                       Potion t -> potionType t
                       _        -> ("really strange " ++)
  | otherwise      = case M.lookup i a of
                       Just ap  -> appearance ap
                       _        -> ("really strange " ++)
