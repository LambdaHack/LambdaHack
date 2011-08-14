module Random (module Frequency, module Random) where

import qualified Data.Binary as Binary
import Data.Ratio
import Data.List as L
import qualified System.Random as R
import Control.Monad.State

import Frequency

type Rnd a = State R.StdGen a

-- Written in a "portable" way because the implementation of
-- State changes between mtl versions 1 and 2.
randomR :: (R.Random a) => (a, a) -> Rnd a
randomR rng =
  do
    g <- get
    let (x, ng) = R.randomR rng g
    put ng
    return x

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

-- | d for die/dice
d :: Int -> Rnd Int
d x = if x <= 0 then return 0 else randomR (1,x)

oneOf :: [a] -> Rnd a
oneOf xs =
  do
    r <- randomR (0, length xs - 1)
    return (xs !! r)

frequency :: Frequency a -> Rnd a
frequency (Frequency xs) =
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

-- ** Arithmetic operations on Rnd.

infixl 7 *~
infixl 6 ~+~

(~+~) :: Num a => Rnd a -> Rnd a -> Rnd a
(~+~) = liftM2 (+)

(*~) :: Num a => Int -> Rnd a -> Rnd a
x *~ r = liftM sum (replicateM x r)

-- RollDice: 1d7, 3d3, etc. (a, b) represent (a *~ d b).
type RollDice = (Binary.Word8, Binary.Word8)

rollDice :: RollDice -> Rnd Int
rollDice (a', b') =
  let (a, b) = (fromEnum a', fromEnum b')
  in  a *~ d b

-- rollQuad (a, b, x, y) = a *~ d b + (lvl * (x *~ d y)) / 10
type RollQuad = (Binary.Word8, Binary.Word8, Binary.Word8, Binary.Word8)

rollQuad :: Int -> RollQuad -> Rnd Int
rollQuad lvl (a, b, x, y) = do
  aDb <- rollDice (a, b)
  xDy <- rollDice (x, y)
  return $ aDb + (lvl * xDy) `div` 10

intToQuad :: Int -> RollQuad
intToQuad 0 = (0, 0, 0, 0)
intToQuad n = let n' = fromIntegral n
              in  if n' > maxBound || n' < minBound
                  then error "intToQuad"
                  else (n', 1, 0, 0)
