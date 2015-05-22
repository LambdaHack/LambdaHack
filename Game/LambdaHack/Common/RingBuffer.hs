{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Ring buffers.
module Game.LambdaHack.Common.RingBuffer
  ( RingBuffer
  , empty, insert, uncons, toList, rbLength
  ) where

import Data.Binary

newtype RingBuffer a = RingBuffer [a]
  deriving (Show, Binary)

empty :: Int -> RingBuffer a
empty _size = RingBuffer []

insert :: a -> RingBuffer a -> RingBuffer a
insert a (RingBuffer l) = RingBuffer $ a : l

uncons :: RingBuffer a -> Maybe (a, RingBuffer a)
uncons (RingBuffer l) = case l of
  [] -> Nothing
  h : t -> Just (h, RingBuffer t)

toList :: RingBuffer a -> [a]
toList (RingBuffer l) = l

rbLength :: RingBuffer a -> Int
rbLength (RingBuffer l) = length l
