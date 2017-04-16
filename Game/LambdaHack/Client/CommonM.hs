{-# LANGUAGE DataKinds #-}
-- | Common client monad operations.
module Game.LambdaHack.Client.CommonM
  ( getPerFid, aidTgtToPos, makeLine
  , maxActorSkillsClient, currentSkillsClient, fullAssocsClient
  , itemToFullClient, pickWeaponClient, updateSalter, createSalter
  , aspectRecordFromItemClient, aspectRecordFromActorClient, createSactorAspect
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.TileKind (TileKind, isUknownSpace)

-- | Get the current perception of a client.
getPerFid :: MonadClient m => LevelId -> m Perception
getPerFid lid = do
  fper <- getsClient sfper
  let assFail = assert `failure` "no perception at given level"
                       `twith` (lid, fper)
  return $! EM.findWithDefault assFail lid fper

-- | Calculate the position of an actor's target.
aidTgtToPos :: MonadClient m => ActorId -> LevelId -> Target -> m (Maybe Point)
aidTgtToPos aid lidV tgt =
  case tgt of
    TEnemy a _ -> do
      body <- getsState $ getActorBody a
      return $! if blid body == lidV
                then Just (bpos body)
                else Nothing
    TPoint _ lid p ->
      return $! if lid == lidV then Just p else Nothing
    TVector v -> do
      b <- getsState $ getActorBody aid
      Level{lxsize, lysize} <- getLevel lidV
      let shifted = shiftBounded lxsize lysize (bpos b) v
      return $! if shifted == bpos b && v /= Vector 0 0
                then Nothing
                else Just shifted

-- | Counts the number of steps until the projectile would hit
-- an actor or obstacle. Starts searching with the given eps and returns
-- the first found eps for which the number reaches the distance between
-- actor and target position, or Nothing if none can be found.
makeLine :: MonadClient m => Bool -> Actor -> Point -> Int -> m (Maybe Int)
makeLine onlyFirst body fpos epsOld = do
  Kind.COps{coTileSpeedup} <- getsState scops
  lvl@Level{lxsize, lysize} <- getLevel (blid body)
  posA <- getsState $ \s p -> posToAssocs p (blid body) s
  let dist = chessDist (bpos body) fpos
      calcScore eps = case bla lxsize lysize eps (bpos body) fpos of
        Just bl ->
          let blDist = take dist bl
              noActor p = all (bproj . snd) (posA p) || p == fpos
              accessibleUnknown tpos =
                let tt = lvl `at` tpos
                in Tile.isWalkable coTileSpeedup tt || isUknownSpace tt
              accessU = all noActor blDist
                        && all accessibleUnknown blDist
              accessFirst | not onlyFirst = False
                          | otherwise =
                all noActor (take 1 blDist)
                && all accessibleUnknown (take 1 blDist)
              nUnknown = length $ filter (isUknownSpace . (lvl `at`)) blDist
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

maxActorSkillsClient :: MonadClient m => ActorId -> m Ability.Skills
maxActorSkillsClient aid = do
  actorAspect <- getsClient sactorAspect
  case EM.lookup aid actorAspect of
    Just aspectRecord -> return $ aSkills aspectRecord  -- keep it lazy
    Nothing -> assert `failure` aid

currentSkillsClient :: MonadClient m => ActorId -> m Ability.Skills
currentSkillsClient aid = do
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup aid actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` aid
  body <- getsState $ getActorBody aid
  side <- getsClient sside
  -- Newest Leader in _sleader, not yet in sfactionD.
  mleader <- if side == bfid body
             then getsClient _sleader
             else do
               fact <- getsState $ (EM.! bfid body) . sfactionD
               return $! _gleader fact
  getsState $ actorSkills mleader aid ar  -- keep it lazy

fullAssocsClient :: MonadClient m
                 => ActorId -> [CStore] -> m [(ItemId, ItemFull)]
fullAssocsClient aid cstores = do
  cops <- getsState scops
  discoKind <- getsClient sdiscoKind
  discoAspect <- getsClient sdiscoAspect
  getsState $ fullAssocs cops discoKind discoAspect aid cstores

itemToFullClient :: MonadClient m => m (ItemId -> ItemQuant -> ItemFull)
itemToFullClient = do
  cops <- getsState scops
  discoKind <- getsClient sdiscoKind
  discoAspect <- getsClient sdiscoAspect
  s <- getState
  let itemToF iid = itemToFull cops discoKind discoAspect iid
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
  actorSk <- currentSkillsClient source
  actorAspect <- getsClient sactorAspect
  let allAssocsRaw = eqpAssocs ++ bodyAssocs
      allAssocs = filter (isMelee . itemBase . snd) allAssocsRaw
  discoBenefit <- getsClient sdiscoBenefit
  strongest <- pickWeaponM (Just discoBenefit)
                           allAssocs actorSk actorAspect source
  case strongest of
    [] -> return Nothing
    iis@((maxS, _) : _) -> do
      let maxIis = map snd $ takeWhile ((== maxS) . fst) iis
      (iid, _) <- rndToAction $ oneOf maxIis
      -- Prefer COrgan, to hint to the player to trash the equivalent CEqp item.
      let cstore = if isJust (lookup iid bodyAssocs) then COrgan else CEqp
      return $ Just $ ReqMelee target iid cstore

updateSalter :: MonadClient m => LevelId -> [(Point, Kind.Id TileKind)] -> m ()
updateSalter lid pts = do
  Kind.COps{coTileSpeedup} <- getsState scops
  let pas = map (second $ toEnum . Tile.alterMinWalk coTileSpeedup) pts
      f = (PointArray.// pas)
  modifyClient $ \cli -> cli {salter = EM.adjust f lid $ salter cli}

createSalter :: State -> AlterLid
createSalter s =
  let Kind.COps{coTileSpeedup} = scops s
      f Level{ltile} =
        PointArray.mapA (toEnum . Tile.alterMinWalk coTileSpeedup) ltile
  in EM.map f $ sdungeon s

aspectRecordFromItem :: DiscoveryKind -> DiscoveryAspect -> ItemId -> Item
                     -> AspectRecord
aspectRecordFromItem disco discoAspect iid itemBase =
  case EM.lookup iid discoAspect of
    Just ar -> ar
    Nothing -> case EM.lookup (jkindIx itemBase) disco of
        Just KindMean{kmMean} -> kmMean
        Nothing -> emptyAspectRecord

aspectRecordFromItemClient :: MonadClient m => ItemId -> Item -> m AspectRecord
aspectRecordFromItemClient iid itemBase = do
  disco <- getsClient sdiscoKind
  discoAspect <- getsClient sdiscoAspect
  return $! aspectRecordFromItem disco discoAspect iid itemBase

aspectRecordFromActorState :: DiscoveryKind -> DiscoveryAspect -> Actor -> State
                           -> AspectRecord
aspectRecordFromActorState disco discoAspect b s =
  let processIid (iid, (k, _)) =
        let itemBase = getItemBody iid s
            ar = aspectRecordFromItem disco discoAspect iid itemBase
        in (ar, k)
      processBag ass = sumAspectRecord $ map processIid ass
  in processBag $ EM.assocs (borgan b) ++ EM.assocs (beqp b)

aspectRecordFromActorClient :: MonadClient m
                            => Actor -> [(ItemId, Item)] -> m AspectRecord
aspectRecordFromActorClient b ais = do
  disco <- getsClient sdiscoKind
  discoAspect <- getsClient sdiscoAspect
  s <- getState
  let f (iid, itemBase) itemD = EM.insert iid itemBase itemD
      sAis = updateItemD (\itemD -> foldr f itemD ais) s
  return $! aspectRecordFromActorState disco discoAspect b sAis

createSactorAspect :: MonadClient m => State -> m ActorAspect
createSactorAspect s = do
  disco <- getsClient sdiscoKind
  discoAspect <- getsClient sdiscoAspect
  let f b = aspectRecordFromActorState disco discoAspect b s
  return $! EM.map f $ sactorD s
