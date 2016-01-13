{-# LANGUAGE DataKinds #-}
-- | Common client monad operations.
module Game.LambdaHack.Client.CommonClient
  ( getPerFid, aidTgtToPos, aidTgtAims, makeLine
  , partAidLeader, partActorLeader, partPronounLeader
  , actorSkillsClient, updateItemSlot, fullAssocsClient, activeItemsClient
  , itemToFullClient, pickWeaponClient, sumOrganEqpClient
  ) where

import Prelude ()
import Prelude.Compat

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import Data.Text (Text)
import Data.Tuple
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.ItemSlot
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK

-- | Get the current perception of a client.
getPerFid :: MonadClient m => LevelId -> m Perception
getPerFid lid = do
  fper <- getsClient sfper
  let assFail = assert `failure` "no perception at given level"
                       `twith` (lid, fper)
  return $! EM.findWithDefault assFail lid fper

-- | The part of speech describing the actor or "you" if a leader
-- of the client's faction. The actor may be not present in the dungeon.
partActorLeader :: MonadClient m => ActorId -> Actor -> m MU.Part
partActorLeader aid b = do
  mleader <- getsClient _sleader
  return $! case mleader of
    Just leader | aid == leader -> "you"
    _ -> partActor b

-- | The part of speech with the actor's pronoun or "you" if a leader
-- of the client's faction. The actor may be not present in the dungeon.
partPronounLeader :: MonadClient m => ActorId -> Actor -> m MU.Part
partPronounLeader aid b = do
  mleader <- getsClient _sleader
  return $! case mleader of
    Just leader | aid == leader -> "you"
    _ -> partPronoun b

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
-- Returns a different @seps@, if needed to reach the target actor.
--
-- Note: Perception is not enough for the check,
-- because the target actor can be obscured by a glass wall
-- or be out of sight range, but in weapon range.
aidTgtAims :: MonadClient m
           => ActorId -> LevelId -> Maybe Target -> m (Either Text Int)
aidTgtAims aid lidV tgt = do
  let findNewEps onlyFirst pos = do
        oldEps <- getsClient seps
        b <- getsState $ getActorBody aid
        mnewEps <- makeLine onlyFirst b pos oldEps
        case mnewEps of
          Just newEps -> return $ Right newEps
          Nothing ->
            return $ Left
                   $ if onlyFirst then "aiming blocked at the first step"
                     else "aiming line to the opponent blocked somewhere"
  case tgt of
    Just (TEnemy a _) -> do
      body <- getsState $ getActorBody a
      let pos = bpos body
      if blid body == lidV
      then findNewEps False pos
      else return $ Left "selected opponent not on this level"
    Just TEnemyPos{} -> return $ Left "selected opponent not visible"
    Just (TPoint lid pos) ->
      if lid == lidV
      then findNewEps True pos
      else return $ Left "selected position not on this level"
    Just (TVector v) -> do
      b <- getsState $ getActorBody aid
      Level{lxsize, lysize} <- getLevel lidV
      let shifted = shiftBounded lxsize lysize (bpos b) v
      if shifted == bpos b && v /= Vector 0 0
      then return $ Left "selected translation is void"
      else findNewEps True shifted
    Nothing -> do
      scursor <- getsClient scursor
      aidTgtAims aid lidV $ Just scursor

-- | Counts the number of steps until the projectile would hit
-- an actor or obstacle. Starts searching with the given eps and returns
-- the first found eps for which the number reaches the distance between
-- actor and target position, or Nothing if none can be found.
makeLine :: MonadClient m => Bool -> Actor -> Point -> Int -> m (Maybe Int)
makeLine onlyFirst body fpos epsOld = do
  cops@Kind.COps{cotile=Kind.Ops{ouniqGroup}} <- getsState scops
  lvl@Level{lxsize, lysize} <- getLevel (blid body)
  bs <- getsState $ filter (not . bproj)
                    . actorList (const True) (blid body)
  let unknownId = ouniqGroup "unknown space"
      dist = chessDist (bpos body) fpos
      calcScore eps = case bla lxsize lysize eps (bpos body) fpos of
        Just bl ->
          let blDist = take dist bl
              blZip = zip (bpos body : blDist) blDist
              noActor p = all ((/= p) . bpos) bs || p == fpos
              accessU = all noActor blDist
                        && all (uncurry $ accessibleUnknown cops lvl) blZip
              accessFirst | not onlyFirst = False
                          | otherwise =
                all noActor (take 1 blDist)
                && all (uncurry $ accessibleUnknown cops lvl) (take 1 blZip)
              nUnknown = length $ filter ((== unknownId) . (lvl `at`)) blDist
          in if | accessU -> - nUnknown
                | accessFirst -> -10000
                | otherwise -> minBound
        Nothing -> assert `failure` (body, fpos, epsOld)
      tryLines curEps (acc, _) | curEps == epsOld + dist = acc
      tryLines curEps (acc, bestScore) =
        let curScore = calcScore curEps
            newAcc = if curScore > bestScore
                     then (Just curEps, curScore)
                     else (acc, bestScore)
        in tryLines (curEps + 1) newAcc
  return $! if | dist <= 0 -> Nothing  -- ProjectAimOnself
               | calcScore epsOld > minBound -> Just epsOld  -- keep old
               | otherwise ->
                 tryLines (epsOld + 1) (Nothing, minBound)  -- generate best

actorSkillsClient :: MonadClient m => ActorId -> m Ability.Skills
actorSkillsClient aid = do
  activeItems <- activeItemsClient aid
  body <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid body) . sfactionD
  side <- getsClient sside
  -- Newest Leader in _sleader, not yet in sfactionD.
  mleader1 <- if side == bfid body then getsClient _sleader else return Nothing
  let mleader2 = fst <$> gleader fact
      mleader = mleader1 `mplus` mleader2
  getsState $ actorSkills mleader aid activeItems

updateItemSlot :: MonadClient m
               => CStore -> Maybe ActorId -> ItemId -> m SlotChar
updateItemSlot store maid iid = do
  slots@(itemSlots, organSlots) <- getsClient sslots
  let onlyOrgans = store == COrgan
      lSlots = if onlyOrgans then organSlots else itemSlots
      incrementPrefix m l iid2 = EM.insert l iid2 $
        case EM.lookup l m of
          Nothing -> m
          Just iidOld ->
            let lNew = SlotChar (slotPrefix l + 1) (slotChar l)
            in incrementPrefix m lNew iidOld
  case lookup iid $ map swap $ EM.assocs lSlots of
    Nothing -> do
      side <- getsClient sside
      item <- getsState $ getItemBody iid
      lastSlot <- getsClient slastSlot
      mb <- maybe (return Nothing) (fmap Just . getsState . getActorBody) maid
      l <- getsState $ assignSlot store item side mb slots lastSlot
      let newSlots | onlyOrgans = ( itemSlots
                                  , incrementPrefix organSlots l iid )
                   | otherwise =  ( incrementPrefix itemSlots l iid
                                  , organSlots )
      modifyClient $ \cli -> cli {sslots = newSlots}
      return l
    Just l -> return l  -- slot already assigned; a letter or a number

fullAssocsClient :: MonadClient m
                 => ActorId -> [CStore] -> m [(ItemId, ItemFull)]
fullAssocsClient aid cstores = do
  cops <- getsState scops
  discoKind <- getsClient sdiscoKind
  discoEffect <- getsClient sdiscoEffect
  getsState $ fullAssocs cops discoKind discoEffect aid cstores

activeItemsClient :: MonadClient m => ActorId -> m [ItemFull]
activeItemsClient aid = do
  activeAssocs <- fullAssocsClient aid [CEqp, COrgan]
  return $! map snd activeAssocs

itemToFullClient :: MonadClient m => m (ItemId -> ItemQuant -> ItemFull)
itemToFullClient = do
  cops <- getsState scops
  discoKind <- getsClient sdiscoKind
  discoEffect <- getsClient sdiscoEffect
  s <- getState
  let itemToF iid = itemToFull cops discoKind discoEffect iid
                               (getItemBody iid s)
  return itemToF

-- Client has to choose the weapon based on its partial knowledge,
-- because if server chose it, it would leak item discovery information.
pickWeaponClient :: MonadClient m
                 => ActorId -> ActorId
                 -> m (Maybe (RequestTimed 'Ability.AbMelee))
pickWeaponClient source target = do
  eqpAssocs <- fullAssocsClient source [CEqp]
  bodyAssocs <- fullAssocsClient source [COrgan]
  actorSk <- actorSkillsClient source
  sb <- getsState $ getActorBody source
  localTime <- getsState $ getLocalTime (blid sb)
  let allAssocs = eqpAssocs ++ bodyAssocs
      calm10 = calmEnough10 sb $ map snd allAssocs
      forced = assert (not $ bproj sb) False
      permitted = permittedPrecious calm10 forced
      preferredPrecious = either (const False) id . permitted
      strongest = strongestMelee True localTime allAssocs
      strongestPreferred = filter (preferredPrecious . snd . snd) strongest
  case strongestPreferred of
    _ | EM.findWithDefault 0 Ability.AbMelee actorSk <= 0 -> return Nothing
    [] -> return Nothing
    iis@((maxS, _) : _) -> do
      let maxIis = map snd $ takeWhile ((== maxS) . fst) iis
      (iid, _) <- rndToAction $ oneOf maxIis
      -- Prefer COrgan, to hint to the player to trash the equivalent CEqp item.
      let cstore = if isJust (lookup iid bodyAssocs) then COrgan else CEqp
      return $ Just $ ReqMelee target iid cstore

sumOrganEqpClient :: MonadClient m
                  => IK.EqpSlot -> ActorId -> m Int
sumOrganEqpClient eqpSlot aid = do
  activeItems <- activeItemsClient aid
  return $! sumSlotNoFilter eqpSlot activeItems
