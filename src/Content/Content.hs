module Content.Content (Content(..)) where

import qualified Data.List as L
import qualified Data.IntMap as IM
import qualified Data.Word as Word

class Content a where
  getFreq :: a -> Int

  content :: [a]

  kindAssocs :: [(Word.Word8, a)]
  kindAssocs = L.zip [0..] content

  kindMap :: IM.IntMap a
  kindMap = IM.fromDistinctAscList $ L.zip [0..] content
