{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, TypeFamilies #-}
-- | General content types and operations.
module Game.LambdaHack.Common.Kind
  ( Id, Speedup(..), Ops(..), COps(..), createOps, stdRuleset
  , Tab, createTab, accessTab
  ) where

import Control.Exception.Assert.Sugar
import qualified Data.Array.Unboxed as A
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.Ix as Ix
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.PlaceKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Common.Frequency

-- | Content identifiers for the content type @c@.
newtype Id c = Id Word8
  deriving (Show, Eq, Ord, Ix.Ix, Enum, Bounded, Binary)

-- | Type family for auxiliary data structures for speeding up
-- content operations.
data family Speedup a

data instance Speedup TileKind = TileSpeedup
  { isClearTab :: !Tab
  , isLitTab   :: !Tab
  , isWalkableTab :: !Tab
  , isPassableTab :: !Tab
  , isDoorTab :: !Tab
  , isSuspectTab :: !Tab
  }

newtype Tab = Tab (A.UArray (Id TileKind) Bool)

createTab :: Ops TileKind -> (TileKind -> Bool) -> Tab
createTab Ops{ofoldrWithKey, obounds} p =
  let f _ k acc = p k : acc
      clearAssocs = ofoldrWithKey f []
  in Tab $ A.listArray obounds clearAssocs

accessTab :: Tab -> Id TileKind -> Bool
{-# INLINE accessTab #-}
accessTab (Tab tab) ki = tab A.! ki

-- | Content operations for the content of type @a@.
data Ops a = Ops
  { okind         :: Id a -> a      -- ^ the content element at given id
  , ouniqGroup    :: Text -> Id a   -- ^ the id of the unique member of
                                    --   a singleton content group
  , opick         :: Text -> (a -> Bool) -> Rnd (Maybe (Id a))
                                    -- ^ pick a random id belonging to a group
                                    --   and satisfying a predicate
  , ofoldrWithKey :: forall b. (Id a -> a -> b -> b) -> b -> b
                                    -- ^ fold over all content elements of @a@
  , obounds       :: !(Id a, Id a)  -- ^ bounds of identifiers of content @a@
  , ospeedup      :: !(Maybe (Speedup a))  -- ^ auxiliary speedup components
  }

-- | Create content operations for type @a@ from definition of content
-- of type @a@.
createOps :: forall a. Show a => ContentDef a -> Ops a
createOps ContentDef{getName, getFreq, content, validate} =
  assert (length content <= fromEnum (maxBound :: Id a)) $
  let kindMap :: EM.EnumMap (Id a) a
      !kindMap = EM.fromDistinctAscList $ zip [Id 0..] content
      kindFreq :: M.Map Text (Frequency (Id a, a))
      kindFreq =
        let tuples = [ (cgroup, (n, (i, k)))
                     | (i, k) <- EM.assocs kindMap
                     , (cgroup, n) <- getFreq k, n > 0 ]
            f m (cgroup, nik) = M.insertWith (++) cgroup [nik] m
            lists = foldl' f M.empty tuples
            nameFreq cgroup = toFreq $ "opick ('" <> cgroup <> "')"
        in M.mapWithKey nameFreq lists
      okind i = fromMaybe (assert `failure` "no kind" `twith` (i, kindMap))
                $ EM.lookup i kindMap
      correct a = not (T.null (getName a)) && all ((> 0) . snd) (getFreq a)
      offenders = validate content
  in assert (allB correct content) $
     assert (null offenders `blame` "content not valid" `twith` offenders)
     -- By this point 'content' can be GCd.
     Ops
       { okind
       , ouniqGroup = \cgroup ->
           let freq = fromMaybe (assert `failure` "no unique group"
                                        `twith` (cgroup, kindFreq))
                      $ M.lookup cgroup kindFreq
           in case runFrequency freq of
             [(n, (i, _))] | n > 0 -> i
             l -> assert `failure` "not unique" `twith` (l, cgroup, kindFreq)
       , opick = \cgroup p ->
           case M.lookup cgroup kindFreq of
             Just freq | not $ nullFreq freq -> fmap Just $ frequency $ do
               (i, k) <- freq
               breturn (p k) i
               {- with MonadComprehensions:
               frequency [ i | (i, k) <- kindFreq M.! cgroup, p k ]
               -}
             _ -> return Nothing
       , ofoldrWithKey = \f z -> foldr (\(i, a) -> f i a) z
                                 $ EM.assocs kindMap
       , obounds = ( fst $ EM.findMin kindMap
                   , fst $ EM.findMax kindMap )
       , ospeedup = Nothing  -- define elsewhere
       }

-- | Operations for all content types, gathered together.
data COps = COps
  { coactor   :: !(Ops ActorKind)
  , cocave    :: !(Ops CaveKind)     -- server only
  , cofaction :: !(Ops FactionKind)
  , coitem    :: !(Ops ItemKind)
  , comode    :: !(Ops ModeKind)     -- server only
  , coplace   :: !(Ops PlaceKind)    -- server only, so far
  , corule    :: !(Ops RuleKind)
  , cotile    :: !(Ops TileKind)
  }

-- | The standard ruleset used for level operations.
stdRuleset :: Ops RuleKind -> RuleKind
stdRuleset Ops{ouniqGroup, okind} = okind $ ouniqGroup "standard"

instance Show COps where
  show _ = "game content"

instance Eq COps where
  (==) _ _ = True
