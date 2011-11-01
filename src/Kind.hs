module Kind
  (Id, Kind.getKind, getId, frequency, foldrWithKey,
   Array, (!), (//), listArray)
  where

import Data.Binary
import qualified Data.List as L
import qualified Data.IntMap as IM
import qualified Data.Word as Word
import qualified Data.Array.Unboxed as A
import qualified Data.Ix as Ix

import Utils.Assert
import Content.Content
import Frequency

newtype Id a = Id{kindId :: Word8} deriving (Show, Eq, Ord)

instance Binary (Id a) where
  put (Id i) = put i
  get = fmap Id get

getKind :: Content a => Id a -> a
getKind (Id i) = kindMap IM.! (fromEnum i)

getId :: Content a => (a -> Bool) -> Id a
getId f = case [Id i | (i, k) <- kindAssocs, f k] of
            [i] -> i
            l -> assert `failure` l

frequency :: Content a => Frequency (Id a, a)
frequency = Frequency [(getFreq k, (Id i, k)) | (i, k) <- kindAssocs]

foldrWithKey :: Content a => (Id a -> a -> b -> b) -> b -> b
foldrWithKey f z = L.foldr (\ (i, a) -> f (Id i) a) z kindAssocs

newtype Array i c = Array{_kindArray :: A.UArray i Word.Word8} deriving Show

-- TODO: save/restore is still too slow, but we are already past
-- the point of diminishing returns. A dramatic change would be
-- low-level conversion to ByteString and serializing that.
instance (Ix.Ix i, Binary i) => Binary (Array i c) where
  put (Array a) = put a
  get = fmap Array get

(!) :: Ix.Ix i => Array i c -> i -> Id c
(!) (Array a) i = Id $ a A.! i

(//) :: Ix.Ix i => Array i c -> [(i, Id c)] -> Array i c
(//) (Array a) l = Array $ a A.// [(i, kindId e) | (i, e) <- l]

listArray :: Ix.Ix i => (i, i) -> [Id c] -> Array i c
listArray bounds l = Array $ A.listArray bounds [(kindId e) | e <- l]
