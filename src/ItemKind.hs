module ItemKind where

import Data.Binary
import qualified Data.Binary.Put as Put
import qualified Data.Binary.Get as Get
import Data.Map as M
import Control.Monad

import qualified Attr
import Random

data ItemKind =
   Ring
 | Scroll
 | Potion PotionKind
 | Wand
 | Amulet
 | Gem
 | Gold
 | Sword Int
 | Dart
 deriving (Eq, Ord, Show)

data PotionKind =
    PotionWater
  | PotionHealing
  deriving (Show, Eq, Ord, Enum, Bounded)

data Appearance =
    Clear
  | White
  deriving (Show, Eq, Ord, Enum, Bounded)

type Assocs = M.Map ItemKind Appearance

instance Binary ItemKind where
  put Ring       = Put.putWord16le 0
  put Scroll     = Put.putWord16le 1
  put (Potion t) = Put.putWord16le 2 >> put t
  put Wand       = Put.putWord16le 3
  put Amulet     = Put.putWord16le 4
  put Gem        = Put.putWord16le 5
  put Gold       = Put.putWord16le 6
  put (Sword i)  = Put.putWord16le 7 >> put i
  put Dart       = Put.putWord16le 8
  get = do
          tag <- Get.getWord16le
          case tag of
            0 -> return Ring
            1 -> return Scroll
            2 -> liftM Potion get
            3 -> return Wand
            4 -> return Amulet
            5 -> return Gem
            6 -> return Gold
            7 -> liftM Sword get
            8 -> return Dart

instance Binary PotionKind where
  put = putWord8 . fromIntegral . fromEnum
  get = liftM (toEnum . fromIntegral) getWord8

instance Binary Appearance where
  put = putWord8 . fromIntegral . fromEnum
  get = liftM (toEnum . fromIntegral) getWord8

potionKind :: PotionKind -> String -> String
potionKind PotionWater   s = s ++ " of water"
potionKind PotionHealing s = s ++ " of healing"

appearance :: Appearance -> String -> String
appearance Clear s = "clear " ++ s
appearance White s = "white " ++ s

itemFrequency :: Frequency ItemKind
itemFrequency =
  Frequency
  [
    (80, Gold),
    (70,  Sword (-1)),
    (40,  Dart),
    (20,  Gem),
    (10,  Ring),
    (10,  Scroll),
    (30,  Wand),
    (10,  Amulet),
    (30,  Potion PotionWater),
    (20,  Potion PotionHealing)
  ]

itemQuantity :: Int -> ItemKind -> Rnd Int
itemQuantity n Gold = (2 * n) *~ d 8
itemQuantity _ Dart = 3 *~ d 3
itemQuantity _ _    = return 1

itemStrength :: Int -> ItemKind -> Rnd ItemKind
itemStrength n (Sword _) =
  do
    r <- d (2 + n `div` 2)
    return $ Sword $ (n + 1) `div` 3 + r
itemStrength _ tp        = return tp

itemLetter :: ItemKind -> Maybe Char
itemLetter Gold = Just '$'
itemLetter _    = Nothing

viewItem :: ItemKind -> Assocs -> (Char, Attr.Color)
viewItem i a = viewItem' i (M.lookup i a)
  where
    def = Attr.defFG
    viewItem' (Sword {})  _            = (')', def)
    viewItem' Dart        _            = (')', def)
    viewItem' Ring        _            = ('=', def)
    viewItem' Scroll      _            = ('?', def)
    viewItem' (Potion {}) (Just Clear) = ('!', Attr.BrBlue)
    viewItem' (Potion {}) (Just White) = ('!', Attr.BrCyan)
    viewItem' (Potion {}) _            = ('!', def)
    viewItem' Wand        _            = ('/', def)
    viewItem' Gold        _            = ('$', Attr.BrYellow)
    viewItem' Gem         _            = ('*', Attr.BrMagenta)
    viewItem' Amulet      _            = ('"', def)
    viewItem' _           _            = ('~', def)
