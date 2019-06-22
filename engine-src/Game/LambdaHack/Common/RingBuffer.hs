{-# LANGUAGE DeriveGeneric #-}
-- | Ring buffers.
module Game.LambdaHack.Common.RingBuffer
  ( RingBuffer
  , empty, cons, toList, length
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude hiding (length, uncons)

import           Data.Binary
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)

-- | Ring buffers of a size determined at initialization.
data RingBuffer a = RingBuffer
  { rbCarrier :: Seq.Seq a
  , rbMaxSize :: Int
  , rbNext    :: Int
  , rbLength  :: Int
  }
  deriving (Show, Generic)

instance Binary a => Binary (RingBuffer a)

-- Only takes O(log n)).
empty :: Int -> a -> RingBuffer a
empty size dummy =
  let rbMaxSize = max 1 size
  in RingBuffer (Seq.replicate rbMaxSize dummy) rbMaxSize 0 0

cons :: a -> RingBuffer a -> RingBuffer a
cons a RingBuffer{..} =
  let incNext = (rbNext + 1) `mod` rbMaxSize
      incLength = min rbMaxSize $ rbLength + 1
  in RingBuffer (Seq.update rbNext a rbCarrier) rbMaxSize incNext incLength

toList :: RingBuffer a -> [a]
toList RingBuffer{..} =
  let l = Foldable.toList rbCarrier
      start = (rbNext + rbMaxSize - rbLength) `mod` rbMaxSize
  in reverse $ take rbLength $ drop start $ l ++ l

length :: RingBuffer a -> Int
length RingBuffer{rbLength} = rbLength
