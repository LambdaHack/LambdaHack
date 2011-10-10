module Kind
  (Id, getKind, getId, frequency, foldWithKey)
  where

import Data.Binary
import qualified Data.List as L
import qualified Data.IntMap as IM
import Control.Monad
import Data.Maybe

import Content
import Frequency

newtype Id a = Id Int
  deriving (Show, Eq, Ord)

instance Binary (Id a) where
  put (Id i) = put i
  get = liftM Id get

getKind :: Content a => Id a -> a
getKind (Id i) = kindMap IM.! i

getId :: (Content a, Eq a) => a -> Id a
getId a = Id $ fromJust $ L.elemIndex a content

frequency :: Content a => Frequency (Id a, a)
frequency = Frequency [(getFreq k, (Id i, k)) | (i, k) <- kindAssocs]

foldWithKey :: Content a => (Id a -> a -> b -> b) -> b -> b
foldWithKey f z = L.foldr (\ (k, a) -> f (Id k) a) z kindAssocs
