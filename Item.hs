module Item where

import Data.Binary
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
 | Potion
 | Wand
 | Amulet
 | Gem
 | Gold
 deriving (Eq, Show)

instance Binary Item where
  put (Item icount itype iletter) = put icount >> put itype >> put iletter
  get = liftM3 Item get get get

instance Binary ItemType where
  put Ring   = putWord8 0
  put Scroll = putWord8 1
  put Potion = putWord8 2
  put Wand   = putWord8 3
  put Amulet = putWord8 4
  put Gem    = putWord8 5
  put Gold   = putWord8 6
  get = do
          tag <- getWord8
          case tag of
            0 -> return Ring
            1 -> return Scroll
            2 -> return Potion
            3 -> return Wand
            4 -> return Amulet
            5 -> return Gem
            6 -> return Gold

itemFrequency :: Frequency ItemType
itemFrequency =
  Frequency
  [
    (10, Gold),
    (3, Gem),
    (2, Ring),
    (4, Scroll),
    (2, Wand),
    (1, Amulet),
    (4, Potion)
  ]

-- | Generate an item.
newItem :: Frequency ItemType -> Rnd Item
newItem ftp =
  do
    tp <- frequency ftp
    return (Item 1 tp Nothing)

-- | Assigns a letter to an item, for inclusion
-- in the inventory of the player. Takes a remembered
-- letter and a starting letter.
assignLetter :: Maybe Char -> Char -> [Item] -> Maybe Char
assignLetter r c is =
    case r of
      Just l | l `L.elem` free -> Just l
      _ -> listToMaybe free
             
  where
    current    = S.fromList (concatMap (maybeToList . iletter) is)
    allLetters = ['a'..'z'] ++ ['A'..'Z']
    candidates = take (length allLetters) (drop (fromJust (findIndex (==c) allLetters)) (cycle allLetters))
    free       = L.filter (\x -> not (x `member` current)) candidates

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

viewItem :: ItemType -> (Char, Attr -> Attr)
viewItem Ring   = ('=', id)
viewItem Scroll = ('?', id)
viewItem Potion = ('!', id)
viewItem Wand   = ('/', id)
viewItem Gold   = ('$', setBold . setFG yellow)
viewItem Gem    = ('*', setFG red)
viewItem _      = ('~', id)

objectItem :: Int -> ItemType -> String
objectItem 1 Ring   = "a ring"
objectItem n Ring   = show n ++ " rings"
objectItem 1 Scroll = "a scroll"
objectItem n Scroll = show n ++ " scrolls"
objectItem 1 Potion = "a potion"
objectItem n Potion = show n ++ " potions"
objectItem 1 Wand   = "a wand"
objectItem n Wand   = show n ++ " wands"
objectItem 1 Amulet = "an amulet"
objectItem n Amulet = show n ++ " amulets"
objectItem 1 Gem    = "a gem"
objectItem n Gem    = show n ++ " gems"
objectItem 1 Gold   = "a gold piece"
objectItem n Gold   = show n ++ " gold pieces"
