module Kind
  (Id, Content(getFreq, content), getKind, getId, frequency)
  where  -- TODO: make content write-only somehow

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

class Content a where
  getFreq :: a -> Int
  content :: [a]
  kindAssocs :: [(Int, a)]
  kindAssocs = L.zip [0..] content
  kindMap :: IM.IntMap a
  kindMap = IM.fromDistinctAscList kindAssocs

getKind :: Content a => Id a -> a
getKind (Id i) = kindMap IM.! i

getId :: (Content a, Eq a) => a -> Id a
getId a = Id $ fromJust $ L.elemIndex a content

frequency :: Content a => Frequency (Id a, a)
frequency = Frequency [(getFreq k, (Id i, k)) | (i, k) <- kindAssocs]
