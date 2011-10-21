module Kind
  (Id, Kind.getKind, getId, frequency, foldrWithKey)
  where

import Data.Binary
import qualified Data.List as L
import qualified Data.IntMap as IM
import Control.Monad

import Assert
import Content.Content
import Frequency

newtype Id a = Id Int
  deriving (Show, Eq, Ord)

instance Binary (Id a) where
  put (Id i) = put i
  get = liftM Id get

getKind :: Content a => Id a -> a
getKind (Id i) = kindMap IM.! i

getId :: Content a => (a -> Bool) -> Id a
getId f = case [Id i | (i, k) <- kindAssocs, f k] of
            [i] -> i
            l -> assert `failure` l

frequency :: Content a => Frequency (Id a, a)
frequency = Frequency [(getFreq k, (Id i, k)) | (i, k) <- kindAssocs]

foldrWithKey :: Content a => (Id a -> a -> b -> b) -> b -> b
foldrWithKey f z = L.foldr (\ (k, a) -> f (Id k) a) z kindAssocs
