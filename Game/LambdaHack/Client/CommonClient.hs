-- | Common client monad operations.
module Game.LambdaHack.Client.CommonClient
  ( getPerFid, aidTgtToPos, aidTgtAims, makeLine
  , partAidLeader, partActorLeader, actorAbilities, updateItemSlot
  ) where

import Control.Exception.Assert.Sugar
import qualified Data.EnumMap.Strict as EM
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Tuple
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.ItemSlot
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Ability (Ability)
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind

-- | Get the current perception of a client.
getPerFid :: MonadClient m => LevelId -> m Perception
getPerFid lid = do
  fper <- getsClient sfper
  return $! fromMaybe (assert `failure` "no perception at given level"
                              `twith` (lid, fper))
                      $ EM.lookup lid fper

-- | The part of speech describing the actor or a special name if a leader
-- of the observer's faction. The actor may not be present in the dungeon.
partActorLeader :: MonadClient m => ActorId -> Actor -> m MU.Part
partActorLeader aid b = do
  mleader <- getsClient _sleader
  return $! case mleader of
    Just leader | aid == leader -> "you"
    _ -> partActor b

-- | The part of speech describing the actor (designated by actor id
-- and present in the dungeon) or a special name if a leader
-- of the observer's faction.
partAidLeader :: MonadClient m => ActorId -> m MU.Part
partAidLeader aid = do
  b <- getsState $ getActorBody aid
  partActorLeader aid b

-- | Calculate the position of an actor's target.
aidTgtToPos :: MonadClient m
            => ActorId -> LevelId -> Maybe Target -> m (Maybe Point)
aidTgtToPos aid lidV tgt =
  case tgt of
    Just (TEnemy a _) -> do
      body <- getsState $ getActorBody a
      return $! if blid body == lidV
                then Just (bpos body)
                else Nothing
    Just (TEnemyPos _ lid p _) ->
      return $! if lid == lidV then Just p else Nothing
    Just (TPoint lid p) ->
      return $! if lid == lidV then Just p else Nothing
    Just (TVector v) -> do
      b <- getsState $ getActorBody aid
      Level{lxsize, lysize} <- getLevel lidV
      let shifted = shiftBounded lxsize lysize (bpos b) v
      return $! if shifted == bpos b && v /= Vector 0 0
                then Nothing
                else Just shifted
    Nothing -> do
      scursor <- getsClient scursor
      aidTgtToPos aid lidV $ Just scursor

-- | Check whether one is permitted to aim at a target
-- (this is only checked for actors; positions let player
-- shoot at obstacles, e.g., to destroy them).
-- This assumes @aidTgtToPos@ does not return @Nothing@.
--
-- Note: Perception is not enough for the check,
-- because the target actor can be obscured by a glass wall
-- or be out of sight range, but in weapon range.
aidTgtAims :: MonadClient m
           => ActorId -> LevelId -> Maybe Target -> m (Maybe Text)
aidTgtAims aid lidV tgt = do
  case tgt of
    Just (TEnemy a _) -> do
      body <- getsState $ getActorBody a
      let pos = bpos body
      b <- getsState $ getActorBody aid
      if blid b == lidV then do
        seps <- getsClient seps
        (steps, _eps) <- makeLine b pos seps
        if steps == chessDist (bpos b) pos
          then return Nothing
          else return $ Just "aiming line to the opponent blocked"
      else return $ Just "target opponent not on this level"
    Just TEnemyPos{} -> return $ Just "target opponent not visible"
    Just TPoint{} -> return Nothing
    Just TVector{} -> return Nothing
    Nothing -> do
      scursor <- getsClient scursor
      aidTgtAims aid lidV $ Just scursor

-- | Counts the number of steps until the projectile would hit
-- an actor or obstacle. Prefers the given eps.
-- TODO: but modifies eps, if needed.
makeLine :: MonadClient m => Actor -> Point -> Int -> m (Int, Int)
makeLine body fpos eps = do
  cops <- getsState scops
  lvl@Level{lxsize, lysize} <- getLevel (blid body)
  bs <- getsState $ actorNotProjList (const True) (blid body)
  let mbl = bla lxsize lysize eps (bpos body) fpos
  case mbl of
    Just bl@(pos1:_) -> do
      let noActor p = any ((== p) . bpos) bs || p == fpos
      case break noActor bl of
        (flies, hits : _) -> do
          let blRest = flies ++ [hits]
              blZip = zip (bpos body : blRest) blRest
              blAccess = takeWhile (uncurry $ accessible cops lvl) blZip
          mab <- getsState $ posToActor pos1 (blid body)
          if maybe True (bproj . snd . fst) mab then
            return $ (length blAccess, eps)
          else return (0, eps)  -- ProjectBlockActor
        _ -> assert `failure` (body, fpos, bl)
    Just [] -> assert `failure` (body, fpos)
    Nothing -> return (0, eps)  -- ProjectAimOnself

actorAbilities :: MonadClient m => ActorId -> Maybe ActorId -> m [Ability]
actorAbilities aid mleader = do
  Kind.COps{ coactor=Kind.Ops{okind}
           , cofaction=Kind.Ops{okind=fokind} } <- getsState scops
  body <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid body) . sfactionD
  let factionAbilities
        | Just aid == mleader = fAbilityLeader $ fokind $ gkind fact
        | otherwise = fAbilityOther $ fokind $ gkind fact
  return $! acanDo (okind $ bkind body) `intersect` factionAbilities

updateItemSlot :: MonadClient m => ActorId -> ItemId -> m Bool
updateItemSlot aid iid = do
  b <- getsState $ getActorBody aid
  slots <- getsClient sslots
  case lookup iid $ map swap $ EM.assocs slots of
    Just _ -> return True  -- slot already assigned
    Nothing -> do
      item <- getsState $ getItemBody iid
      lastSlot <- getsClient slastSlot
      mc <- getsState $ assignSlot item b slots lastSlot
      case mc of
        Just l2 -> do
          modifyClient $ \cli ->
            cli { sslots = EM.insert l2 iid (sslots cli)
                , slastSlot = max l2 (slastSlot cli) }
          return True
        Nothing -> return False  -- overfull
