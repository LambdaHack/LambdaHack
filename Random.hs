module Random where

import Data.Ratio

import qualified System.Random as R
import Control.Monad.State

type Rnd a = State R.StdGen a

randomR :: (R.Random a) => (a, a) -> Rnd a
randomR r = State (R.randomR r)

binaryChoice :: a -> a -> Rnd a
binaryChoice p0 p1 =
  do
    b <- randomR (False,True)
    return (if b then p0 else p1)

chance :: Rational -> Rnd Bool
chance r =
  do
    let n = numerator r
        d = denominator r
    k <- randomR (1,d)
    return (k <= n)

oneOf :: [a] -> Rnd a
oneOf xs =
  do
    r <- randomR (0, length xs - 1)
    return (xs !! r)

type Frequency a = [(Int, a)]

frequency :: Frequency a -> Rnd a
frequency xs =
  do
    r <- randomR (1, sum (map fst xs))
    return (frequency' r xs)
 where
  frequency' :: Int -> [(Int, a)] -> a
  frequency' _ [(_, x)] = x
  frequency' m ((n, x) : xs)
    | m <= n            = x
    | otherwise         = frequency' (m - n) xs

rndToIO :: Rnd a -> IO a
rndToIO r =
  do
    g <- R.getStdGen
    let (x,g') = runState r g
    R.setStdGen g'
    return x
