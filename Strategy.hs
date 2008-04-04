module Strategy where

import Control.Monad

import Frequency

-- Monster strategies

-- | A strategy is a frequency table.
type Strategy a = Frequency a

infixr 2 .|

(.|) :: Strategy a -> Strategy a -> Strategy a
(.|) = melse

infix 3 .=>

(.=>) :: Bool -> Strategy a -> Strategy a
p .=> m | p          =  m
        | otherwise  =  mzero

only :: (a -> Bool) -> Strategy a -> Strategy a
only p s =
  do
    x <- s
    p x .=> return x
