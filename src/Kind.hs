module Kind
  (Id, Ops(getKind, getId, frequency), buildOps)
  where

import Data.Binary
import qualified Data.List as L
import qualified Data.IntMap as IM
import Control.Monad
import Data.Maybe

import Frequency

newtype Id a = Id Int
  deriving (Show, Eq, Ord)

instance Binary (Id a) where
  put (Id i) = put i
  get = liftM Id get

data Ops a = Ops
  { getKind   :: Id a -> a
  , getId     :: a -> Id a
  , frequency :: Frequency (Id a)
  }

buildOps :: Eq a => [a] -> (a -> Int) -> Ops a
buildOps content getFreq =
  let kindAssocs = L.zip [0..] content
      kindMap = IM.fromDistinctAscList kindAssocs
      getKind (Id i) = kindMap IM.! i
      getId a = Id $ fromJust $ L.elemIndex a content
      frequency = Frequency [(getFreq k, Id i) | (i, k) <- kindAssocs]
  in Ops{getKind, getId, frequency}
