module Kind
  (Id, Kind.getKind, getId, frequency, foldrWithKey)
  where

import Data.Binary
import qualified Data.List as L
import qualified Data.IntMap as IM
import qualified Data.Word as Word

import Utils.Assert
import Content.Content
import Frequency

newtype Id a = Id Word.Word8
  deriving (Show, Eq, Ord)

instance Binary (Id a) where
  put (Id i) = putWord8 i
  get = fmap Id getWord8

getKind :: Content a => Id a -> a
getKind (Id i) = kindMap IM.! (fromIntegral i)

getId :: Content a => (a -> Bool) -> Id a
getId f = case [Id i | (i, k) <- kindAssocs, f k] of
            [i] -> i
            l -> assert `failure` l

frequency :: Content a => Frequency (Id a, a)
frequency = Frequency [(getFreq k, (Id i, k)) | (i, k) <- kindAssocs]

foldrWithKey :: Content a => (Id a -> a -> b -> b) -> b -> b
foldrWithKey f z = L.foldr (\ (i, a) -> f (Id i) a) z kindAssocs
