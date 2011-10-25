module Random where

import qualified Data.Binary as Binary
import Data.Ratio
import qualified System.Random as R
import Control.Monad.State

import Utils.Assert
import Frequency

-- TODO: if the file grows much larger, split it and move a part to Utils/

type Rnd a = State R.StdGen a

-- TODO: rewrite; was written in a "portable" way because the implementation of
-- State changes between mtl versions 1 and 2. Now we are using only mtl 2.
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
    k <- randomR (1, d)
    return (k <= n)

-- | roll a single die
roll :: Int -> Rnd Int
roll x = if x <= 0 then return 0 else randomR (1,x)

oneOf :: [a] -> Rnd a
oneOf xs =
  do
    r <- randomR (0, length xs - 1)
    return (xs !! r)

frequency :: Frequency a -> Rnd a
frequency (Frequency fs) =
  do
    r <- randomR (1, sum (map fst fs))
    return (frequency' r fs)
 where
  frequency' :: Int -> [(Int, a)] -> a
  frequency' m []       = assert `failure` (map fst fs, m)
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

-- RollDice: 1d7, 3d3, 2d0, etc. (a, b) represent (a *~ roll b).
type RollDice = (Binary.Word8, Binary.Word8)

rollDice :: RollDice -> Rnd Int
rollDice (a', b') =
  let (a, b) = (fromEnum a', fromEnum b')
  in a *~ roll b

maxDice :: RollDice -> Int
maxDice (a', b') =
  let (a, b) = (fromEnum a', fromEnum b')
  in a * b

minDice :: RollDice -> Int
minDice (a', b') =
  let (a, b) = (fromEnum a', fromEnum b')
  in if b == 0 then 0 else a

meanDice :: RollDice -> Rational
meanDice (a', b') =
  let (a, b) = (fromIntegral a', fromIntegral b')
  in if b == 0 then 0 else a * (b + 1) % 2

-- rollQuad (a, b, x, y) = a *~ roll b + (lvl * (x *~ roll y)) / 10
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
                  then assert `failure` n
                  else (n', 1, 0, 0)
