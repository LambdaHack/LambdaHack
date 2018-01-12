{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-expose-all-unfoldings #-}
-- | General content types and operations.
module Game.LambdaHack.Common.Kind
  ( Ops(..), COps(..), stdRuleset, createOps
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.PlaceKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind

-- | Content operations for the content of type @a@.
data Ops a = Ops
  { okind          :: ContentId a -> a  -- ^ content element at given id
  , ouniqGroup     :: GroupName a -> ContentId a
                                 -- ^ the id of the unique member of
                                 --   a singleton content group
  , opick          :: GroupName a -> (a -> Bool) -> Rnd (Maybe (ContentId a))
                                 -- ^ pick a random id belonging to a group
                                 --   and satisfying a predicate
  , ofoldrWithKey  :: forall b. (ContentId a -> a -> b -> b) -> b -> b
                                 -- ^ fold over all content elements of @a@
  , ofoldlWithKey' :: forall b. (b -> ContentId a -> a -> b) -> b -> b
                                 -- ^ fold strictly over all content @a@
  , ofoldlGroup'   :: forall b.
                      GroupName a
                      -> (b -> Int -> ContentId a -> a -> b)
                      -> b
                      -> b
                                 -- ^ fold over the given group only
  , olength        :: Int        -- ^ size of content @a@
  }

-- | Operations for all content types, gathered together.
data COps = COps
  { cocave        :: Ops CaveKind   -- server only
  , coitem        :: Ops ItemKind
  , comode        :: Ops ModeKind   -- server only
  , coplace       :: Ops PlaceKind  -- server only, so far
  , corule        :: Ops RuleKind
  , cotile        :: Ops TileKind
  , coTileSpeedup :: TileSpeedup
  }

instance Show COps where
  show _ = "game content"

instance Eq COps where
  (==) _ _ = True

-- | The standard ruleset used for level operations.
stdRuleset :: Ops RuleKind -> RuleKind
stdRuleset Ops{ouniqGroup, okind} = okind $ ouniqGroup "standard"

-- Not specialized, because no speedup, but big JS code bloat
-- (-fno-expose-all-unfoldings and NOINLINE used to ensure that,
-- in the absence of NOSPECIALIZABLE pragma).
-- | Create content operations for type @a@ from definition of content
-- of type @a@.
createOps :: forall a. Show a => ContentDef a -> Ops a
{-# NOINLINE createOps #-}
createOps ContentDef{contentVector, kindFreq} =
  Ops  { okind = \ !i -> contentVector V.! fromEnum i
       , ouniqGroup = \ !cgroup ->
           let freq = let assFail = error $ "no unique group"
                                            `showFailure` (cgroup, kindFreq)
                      in M.findWithDefault assFail cgroup kindFreq
           in case freq of
             [(n, (i, _))] | n > 0 -> i
             l -> error $ "not unique" `showFailure` (l, cgroup, kindFreq)
       , opick = \ !cgroup !p ->
           case M.lookup cgroup kindFreq of
             Just freqRaw ->
               let freq = toFreq ("opick ('" <> tshow cgroup <> "')")
                          $ filter (p . snd . snd) freqRaw
               in if nullFreq freq
                  then return Nothing
                  else fmap (Just . fst) $ frequency freq
                    {- with monadic notation; may produce empty freq:
                    (i, k) <- freq
                    breturn (p k) i
                    -}
                    {- with MonadComprehensions:
                    frequency [ i | (i, k) <- kindFreq M.! cgroup, p k ]
                    -}
             _ -> return Nothing
       , ofoldrWithKey = \f z ->
          V.ifoldr (\i c a -> f (toEnum i) c a) z contentVector
       , ofoldlWithKey' = \f z ->
          V.ifoldl' (\a i c -> f a (toEnum i) c) z contentVector
       , ofoldlGroup' = \cgroup f z ->
           case M.lookup cgroup kindFreq of
             Just freq -> foldl' (\acc (p, (i, a)) -> f acc p i a) z freq
             _ -> error $ "no group '" ++ show cgroup
                                       ++ "' among content that has groups "
                                       ++ show (M.keys kindFreq)
                          `showFailure` ()
       , olength = V.length contentVector
       }
