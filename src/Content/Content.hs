module Content.Content (Content(..)) where

import qualified Data.List as L
import qualified Data.IntMap as IM

class Content a where
  getFreq :: a -> Int

  content :: [a]

  kindAssocs :: [(Int, a)]
  kindAssocs = L.zip [0..] content

  kindMap :: IM.IntMap a
  kindMap = IM.fromDistinctAscList kindAssocs
