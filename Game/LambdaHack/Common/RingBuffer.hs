{-# LANGUAGE DeriveGeneric #-}
-- | Ring buffers.
module Game.LambdaHack.Common.RingBuffer
  ( RingBuffer(rbLength)
  , empty, cons, uncons, toList
  ) where

import Data.Binary
import qualified Data.Vector as V
import Data.Vector.Binary ()
import GHC.Generics (Generic)

data RingBuffer a = RingBuffer
  { rbCarrier :: !(V.Vector a)
  , rbNext    :: !Int
  , rbLength  :: !Int
  }
  deriving (Show, Generic)

instance Binary a => Binary (RingBuffer a)

empty :: Int -> a -> RingBuffer a
empty size dummy = RingBuffer (V.replicate size dummy) 0 0

cons :: a -> RingBuffer a -> RingBuffer a
cons a RingBuffer{..} =
  let size = V.length rbCarrier
      incNext = (rbNext + 1) `mod` size
      incLength = min size $ rbLength + 1
  in RingBuffer (rbCarrier V.// [(rbNext, a)]) incNext incLength

uncons :: RingBuffer a -> Maybe (a, RingBuffer a)
uncons RingBuffer{..} =
  let size = V.length rbCarrier
      decNext = (rbNext - 1) `mod` size
  in if rbLength == 0
     then Nothing
     else Just ( rbCarrier V.! decNext
               , RingBuffer rbCarrier decNext (rbLength - 1) )

toList :: RingBuffer a -> [a]
toList RingBuffer{..} =
  let l = V.toList rbCarrier
      size = V.length rbCarrier
      start = (rbNext + size - rbLength) `mod` size
  in take rbLength $ drop start $ l ++ l
