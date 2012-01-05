module Game.LambdaHack.Random
  ( Rnd, randomR, binaryChoice
  , roll, oneOf, frequency, (*~), (~+~)
  , RollDice(..), rollDice, maxDice, minDice, meanDice
  , RollDiceXY, rollDiceXY
  , RollQuad, rollQuad, chanceQuad, intToQuad
  , Chance, chance
  ) where

import qualified Data.Binary as Binary
import Data.Ratio
import qualified System.Random as R
import Control.Monad.State
import qualified Data.List as L

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Frequency
import Game.LambdaHack.WorldLoc

-- TODO: if the file grows much larger, split it and move a part to Utils/

type Rnd a = State R.StdGen a

-- TODO: rewrite; was written in a "portable" way because the implementation of
-- State changes between mtl versions 1 and 2. Now we are using only mtl 2.
randomR :: (R.Random a) => (a, a) -> Rnd a
randomR rng = do
  g <- get
  let (x, ng) = R.randomR rng g
  put ng
  return x

binaryChoice :: a -> a -> Rnd a
binaryChoice p0 p1 = do
  b <- randomR (False, True)
  return (if b then p0 else p1)

-- | roll a single die
roll :: Int -> Rnd Int
roll x = if x <= 0 then return 0 else randomR (1, x)

oneOf :: [a] -> Rnd a
oneOf xs = do
  r <- randomR (0, length xs - 1)
  return (xs !! r)

frequency :: Frequency a -> Rnd a
frequency (Frequency [(n, x)]) | n > 0 = return x  -- speedup
frequency (Frequency fs) =
  assert (sumf > 0 `blame` map fst fs) $ do
  r <- randomR (1, sumf)
  return (frequency' r fs)
 where
  sumf = sum (map fst fs)
  frequency' :: Int -> [(Int, a)] -> a
  frequency' m []       = assert `failure` (map fst fs, m)
  frequency' m ((n, x) : xs)
    | m <= n            = x
    | otherwise         = frequency' (m - n) xs

-- ** Arithmetic operations on Rnd.

infixl 7 *~
infixl 6 ~+~

(~+~) :: Num a => Rnd a -> Rnd a -> Rnd a
(~+~) = liftM2 (+)

(*~) :: Num a => Int -> Rnd a -> Rnd a
x *~ r = liftM sum (replicateM x r)

-- | 1d7, 3d3, 2d0, etc. 'RollDice a b' represents 'a *~ roll b)'.
data RollDice = RollDice Binary.Word8 Binary.Word8
  deriving (Eq, Ord)

instance Show RollDice where
  show (RollDice a b) = show a ++ "d" ++ show b

instance Read RollDice where
  readsPrec d s =
    let (a, db) = L.break (== 'd') s
        av = read a
    in case db of
      'd' : b -> [ (RollDice av bv, rest) | (bv, rest) <- readsPrec d b ]
      _ -> []

rollDice :: RollDice -> Rnd Int
rollDice (RollDice a' 1)  = return $ fromEnum a'  -- optimization
rollDice (RollDice a' b') =
  let (a, b) = (fromEnum a', fromEnum b')
  in a *~ roll b

maxDice :: RollDice -> Int
maxDice (RollDice a' b') =
  let (a, b) = (fromEnum a', fromEnum b')
  in a * b

minDice :: RollDice -> Int
minDice (RollDice a' b') =
  let (a, b) = (fromEnum a', fromEnum b')
  in if b == 0 then 0 else a

meanDice :: RollDice -> Rational
meanDice (RollDice a' b') =
  let (a, b) = (fromIntegral a', fromIntegral b')
  in if b' == 0 then 0 else a * (b + 1) % 2

type RollDiceXY = (RollDice, RollDice)

rollDiceXY :: RollDiceXY -> Rnd (Int, Int)
rollDiceXY (RollDice xa' xb', RollDice ya' yb') = do
  let (xa, xb) = (fromEnum xa', fromEnum xb')
      (ya, yb) = (fromEnum ya', fromEnum yb')
  x <- xa *~ roll xb
  y <- ya *~ roll yb
  return (x, y)

-- | 'rollQuad (aDb, xDy) = rollDice aDb + lvl * rollDice xDy / depth'
type RollQuad = (RollDice, RollDice)

rollQuad :: LevelId -> Int -> RollQuad -> Rnd Int
rollQuad lvl depth (RollDice a b, RollDice x y) =
  assert (n > 0 && n <= depth `blame` (lvl, depth)) $ do
  aDb <- rollDice (RollDice a b)
  xDy <- rollDice (RollDice x y)
  return $ aDb + ((n - 1) * xDy) `div` (depth - 1)
 where
  n = levelNumber lvl

chanceQuad :: LevelId -> Int -> RollQuad -> Rnd Bool
chanceQuad lvl depth quad = do
  c <- rollQuad lvl depth quad
  return $ c > 50

intToQuad :: Int -> RollQuad
intToQuad 0  = (RollDice 0 0, RollDice 0 0)
intToQuad n' = let n = toEnum n'
               in if n > maxBound || n < minBound
                  then assert `failure` n'
                  else (RollDice n 1, RollDice 0 0)

type Chance = Rational

chance :: Chance -> Rnd Bool
chance r = do
  let n = numerator r
      d = denominator r
  k <- randomR (1, d)
  return (k <= n)
