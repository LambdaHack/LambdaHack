{-# LANGUAGE DeriveGeneric #-}
-- | Ring buffers.
module Game.LambdaHack.Common.RingBuffer
  ( RingBuffer
  , empty, cons, uncons, toList, length
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude hiding (length, uncons)

import Data.Binary
import Data.Binary.Orphans ()
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)

data RingBuffer a = RingBuffer
  { rbCarrier :: !(Seq.Seq a)
  , rbMaxSize :: !Int
  , rbNext    :: !Int
  , rbLength  :: !Int
  }
  deriving (Show, Generic)

instance Binary a => Binary (RingBuffer a)

-- Only takes O(log n)).
empty :: Int -> a -> RingBuffer a
empty rbMaxSize dummy = RingBuffer (Seq.replicate rbMaxSize dummy) rbMaxSize 0 0

-- | Add element to the front of the buffer.
cons :: a -> RingBuffer a -> RingBuffer a
cons a RingBuffer{..} =
  let incNext = (rbNext + 1) `mod` rbMaxSize
      incLength = min rbMaxSize $ rbLength + 1
  in RingBuffer (Seq.update rbNext a rbCarrier) rbMaxSize incNext incLength

uncons :: RingBuffer a -> Maybe (a, RingBuffer a)
uncons RingBuffer{..} =
  let decNext = (rbNext - 1) `mod` rbMaxSize
  in if rbLength == 0
     then Nothing
     else Just ( Seq.index rbCarrier decNext
               , RingBuffer rbCarrier rbMaxSize decNext (rbLength - 1) )

toList :: RingBuffer a -> [a]
toList RingBuffer{..} =
  let l = Foldable.toList rbCarrier
      start = (rbNext + rbMaxSize - rbLength) `mod` rbMaxSize
  in take rbLength $ drop start $ l ++ l

length :: RingBuffer a -> Int
length RingBuffer{rbLength} = rbLength
