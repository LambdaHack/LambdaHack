-- | General content types and operations.
module Game.LambdaHack.Common.Kind
  ( Id, Ops(..), COps(..), createOps, stdRuleset
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.KindOps
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.PlaceKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind

-- | Create content operations for type @a@ from definition of content
-- of type @a@.
createOps :: forall a. Show a => ContentDef a -> Ops a
createOps ContentDef{getName, getFreq, content, validateSingle, validateAll} =
  assert (EM.size content <= fromEnum (maxBound :: Id a)) $
  let kindFreq :: M.Map (GroupName a) [(Int, (Id a, a))]
      kindFreq =
        let tuples = [ (cgroup, (n, (i, k)))
                     | (i, k) <- EM.assocs content
                     , (cgroup, n) <- getFreq k
                     , n > 0 ]
            f m (cgroup, nik) = M.insertWith (++) cgroup [nik] m
        in foldl' f M.empty tuples
      correct a = not (T.null (getName a)) && all ((> 0) . snd) (getFreq a)
      singleOffenders = [ (offences, a)
                        | a <- EM.elems content
                        , let offences = validateSingle a
                        , not (null offences) ]
      allOffences = validateAll $ EM.elems content
  in assert (allB correct $ EM.elems content) $
     assert (null singleOffenders `blame` "some content items not valid"
                                  `twith` singleOffenders) $
     assert (null allOffences `blame` "the content set not valid"
                              `twith` allOffences)
     -- By this point 'content' can be GCd.
     Ops
       { okind = \i ->
          let assFail = assert `failure` "no kind" `twith` (i, content)
          in EM.findWithDefault assFail i content
       , ouniqGroup = \cgroup ->
           let freq = let assFail = assert `failure` "no unique group"
                                           `twith` (cgroup, kindFreq)
                      in M.findWithDefault assFail cgroup kindFreq
           in case freq of
             [(n, (i, _))] | n > 0 -> i
             l -> assert `failure` "not unique" `twith` (l, cgroup, kindFreq)
       , opick = \cgroup p ->
           case M.lookup cgroup kindFreq of
             Just freqRaw ->
               let freq = toFreq ("opick ('" <> tshow cgroup <> "')") freqRaw
               in if nullFreq freq
                  then return Nothing
                  else fmap Just $ frequency $ do
                    (i, k) <- freq
                    breturn (p k) i
                    {- with MonadComprehensions:
                    frequency [ i | (i, k) <- kindFreq M.! cgroup, p k ]
                    -}
             _ -> return Nothing
       , ofoldrWithKey = \f z -> foldr (uncurry f) z $ EM.assocs content
       , ofoldrGroup = \cgroup f z ->
           case M.lookup cgroup kindFreq of
             Just freq -> foldr (\(p, (i, a)) -> f p i a) z freq
             _ -> assert `failure` "no group '" <> tshow cgroup
                                   <> "' among content that has groups"
                                   <+> tshow (M.keys kindFreq)
       , obounds = ( fst $ EM.findMin content
                   , fst $ EM.findMax content )
       }

-- | Operations for all content types, gathered together.
data COps = COps
  { cocave        :: !(Ops CaveKind)     -- server only
  , coitem        :: !(Ops ItemKind)
  , comode        :: !(Ops ModeKind)     -- server only
  , coplace       :: !(Ops PlaceKind)    -- server only, so far
  , corule        :: !(Ops RuleKind)
  , cotile        :: !(Ops TileKind)
  , coClear       :: !Bool
  , coTileSpeedup :: !TileSpeedup
  }

instance Show COps where
  show _ = "game content"

instance Eq COps where
  (==) _ _ = True

-- | The standard ruleset used for level operations.
stdRuleset :: Ops RuleKind -> RuleKind
stdRuleset Ops{ouniqGroup, okind} = okind $ ouniqGroup "standard"
