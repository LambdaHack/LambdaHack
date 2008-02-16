module Random (Rnd, randomR, rndToIO, binaryChoice, chance) where

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

rndToIO :: Rnd a -> IO a
rndToIO r =
  do
    g <- R.getStdGen
    let (x,g') = runState r g
    R.setStdGen g'
    return x
