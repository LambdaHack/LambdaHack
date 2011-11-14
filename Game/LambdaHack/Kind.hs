{-# LANGUAGE RankNTypes #-}
module Game.LambdaHack.Kind
  ( Id, Ops(..), COps(..), contentOps
  , getId
  , Array, (!), (//), listArray, bounds
  ) where

import Data.Binary
import qualified Data.List as L
import qualified Data.IntMap as IM
import qualified Data.Word as Word
import qualified Data.Array.Unboxed as A
import qualified Data.Ix as Ix

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Content.Content
import Game.LambdaHack.Frequency
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.TileKind

newtype Id a = Id Word8 deriving (Show, Eq, Ord, Ix.Ix)

instance Binary (Id a) where
  put (Id i) = put i
  get = fmap Id get

data Ops a = Ops
  { osymbol :: a -> Char
  , oname :: a -> String
  , ofreq :: a -> Int
  , ofindSymbol :: Id a -> Char
  , ofindName :: Id a -> String
  , ofindFreq :: Id a -> Int
  , ofindKind :: Id a -> a
  , ofilter :: (a -> Bool) -> [(Id a, a)]
  , ofrequency :: Frequency (Id a, a)
  , ofoldrWithKey :: forall b . (Id a -> a -> b -> b) -> b -> b
  , olimits :: ((Id a, a), (Id a, a))
  , obounds :: (Id a, Id a)
  }

createOps :: Content a => Ops a
createOps =
  let -- kindAssocs :: [(Word.Word8, a)]
      kindAssocs = L.zip [0..] content
      -- kindMap :: IM.IntMap a
      kindMap = IM.fromDistinctAscList $ L.zip [0..] content
      ofindKind = \ (Id i) -> kindMap IM.! (fromEnum i)
  in Ops
  { osymbol = getSymbol
  , oname = getName
  , ofreq = getFreq
  , ofindSymbol = getSymbol . ofindKind
  , ofindName = getName . ofindKind
  , ofindFreq = getFreq . ofindKind
  , ofindKind = ofindKind
  , ofilter = \ f -> [(Id i, k) | (i, k) <- kindAssocs, f k]
  , ofrequency = Frequency [(getFreq k, (Id i, k)) | (i, k) <- kindAssocs]
  , ofoldrWithKey = \ f z -> L.foldr (\ (i, a) -> f (Id i) a) z kindAssocs
  , olimits = let (i1, a1) = IM.findMin kindMap
                  (i2, a2) = IM.findMax kindMap
              in ((Id (toEnum i1), a1), (Id (toEnum i2), a2))
  , obounds = (Id 0, (fst . snd) limitsId)
  }

data COps = COps
  { coactor :: Ops ActorKind
  , cocave  :: Ops CaveKind
  , coitem  :: Ops ItemKind
  , cotile  :: Ops TileKind
  }

instance Show COps where
  show _ = "Game content."

contentOps :: COps
contentOps = COps
  { coactor = createOps
  , cocave  = createOps
  , coitem  = createOps
  , cotile  = createOps
  }

getId :: Content a => (a -> Bool) -> Id a
getId f = case [Id i | (i, k) <- kindAssocs, f k] of
            [i] -> i
            l -> assert `failure` l

limitsId :: Content a => ((Id a, a), (Id a, a))
limitsId = let (i1, a1) = IM.findMin kindMap
               (i2, a2) = IM.findMax kindMap
           in ((Id (toEnum i1), a1), (Id (toEnum i2), a2))

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

bounds :: Ix.Ix i => Array i c -> (i, i)
bounds (Array a) = A.bounds a
