module Frequency where

import Control.Monad

newtype Frequency a = Frequency { runFrequency :: [(Int, a)] }
  deriving Show

instance Monad Frequency where
  return x  =  Frequency [(1, x)]
  m >>= f   =  Frequency
               [(p * q, y) | (p, x) <- runFrequency m,
                             (q, y) <- runFrequency (f x) ]
  fail _    =  Frequency []

instance MonadPlus Frequency where
  mplus (Frequency xs) (Frequency ys) = Frequency (xs ++ ys)
  mzero = Frequency []

instance Functor Frequency where
  fmap f (Frequency xs) = Frequency (map (\ (p, x) -> (p, f x)) xs)

-- only try the second possibility if the first fails
melse :: Frequency a -> Frequency a -> Frequency a
melse (Frequency []) y = y
melse x             _y = x

scale :: Int -> Frequency a -> Frequency a
scale n (Frequency xs) = Frequency (map (\ (p, x) -> (n * p, x)) xs)

uniform :: [a] -> Frequency a
uniform = Frequency . map (\ x -> (1, x))
