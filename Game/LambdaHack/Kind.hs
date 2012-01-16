{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Game.LambdaHack.Kind
  ( Id, Ops(..), COps(..), createOps
  , Array, (!), (//), listArray, array, bounds
  ) where

import Data.Binary
import qualified Data.List as L
import qualified Data.IntMap as IM
import qualified Data.Word as Word
import qualified Data.Array.Unboxed as A
import qualified Data.Ix as Ix
import Data.Maybe

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Utils.Frequency
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.PlaceKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Content.Content
import Game.LambdaHack.Random

newtype Id a = Id Word8 deriving (Show, Eq, Ord, Ix.Ix)

instance Binary (Id a) where
  put (Id i) = put i
  get = fmap Id get

data Ops a = Ops
  { osymbol :: Id a -> Char
  , oname :: Id a -> String
  , okind :: Id a -> a
  , ouniqGroup :: String -> Id a
  , opick :: String -> (a -> Bool) -> Rnd (Id a)
  , ofoldrWithKey :: forall b. (Id a -> a -> b -> b) -> b -> b
  , obounds :: (Id a, Id a)
  , ospeedup :: [Id a -> Bool]  -- TODO: switch list to tuple via a type family?
  }

createOps :: forall a. Show a => CDefs a -> Ops a
createOps CDefs{getSymbol, getName, getFreq, content, validate} =
  let kindAssocs :: [(Word.Word8, a)]
      kindAssocs = L.zip [0..] content
      kindMap :: IM.IntMap a
      kindMap = IM.fromDistinctAscList $ L.zip [0..] content
      groupFreq group k = fromMaybe 0 (L.lookup group $ getFreq k)
      kindFreq :: String -> Frequency (Id a, a)
      kindFreq group =
        toFreq [ (n, (Id i, k))
               | (i, k) <- kindAssocs, let n = groupFreq group k, n > 0 ]
      okind = \ (Id i) -> kindMap IM.! (fromEnum i)
      correct a = not (L.null (getName a)) && L.all ((> 0) . snd) (getFreq a)
      offenders = validate content
  in assert (allB correct content) $
     assert (L.null offenders `blame` offenders) $
     Ops
       { osymbol = getSymbol . okind
       , oname = getName . okind
       , okind = okind
       , ouniqGroup = \ group ->
           case [Id i | (i, k) <- kindAssocs, groupFreq group k > 0] of
             [i] -> i
             l -> assert `failure` l
       , opick = \ group p ->
           fmap fst $ frequency $ filterFreq (p . snd) $ kindFreq group
       , ofoldrWithKey = \ f z -> L.foldr (\ (i, a) -> f (Id i) a) z kindAssocs
       , obounds =
         let limits = let (i1, a1) = IM.findMin kindMap
                          (i2, a2) = IM.findMax kindMap
                      in ((Id (toEnum i1), a1), (Id (toEnum i2), a2))
         in (Id 0, (fst . snd) limits)
       , ospeedup = []  -- the default, override elsewhere
       }

data COps = COps
  { coactor :: Ops ActorKind
  , cocave  :: Ops CaveKind
  , coitem  :: Ops ItemKind
  , coplace :: Ops PlaceKind
  , corule  :: Ops RuleKind
  , cotile  :: Ops TileKind
  }

instance Show COps where
  show _ = "Game content."

newtype Array i c = Array (A.UArray i Word.Word8) deriving Show

-- TODO: save/restore is still too slow, but we are already past
-- the point of diminishing returns. A dramatic change would be
-- low-level conversion to ByteString and serializing that.
instance (Ix.Ix i, Binary i) => Binary (Array i c) where
  put (Array a) = put a
  get = fmap Array get

(!) :: Ix.Ix i => Array i c -> i -> Id c
(!) (Array a) i = Id $ a A.! i

(//) :: Ix.Ix i => Array i c -> [(i, Id c)] -> Array i c
(//) (Array a) l = Array $ a A.// [(i, e) | (i, Id e) <- l]

listArray :: Ix.Ix i => (i, i) -> [Id c] -> Array i c
listArray bds l = Array $ A.listArray bds [e | Id e <- l]

array :: Ix.Ix i => (i, i) -> [(i, Id c)] -> Array i c
array bds l = Array $ A.array bds [(i, e) | (i, Id e) <- l]

bounds :: Ix.Ix i => Array i c -> (i, i)
bounds (Array a) = A.bounds a
