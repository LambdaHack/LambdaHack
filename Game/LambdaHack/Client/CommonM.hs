{-# LANGUAGE DataKinds #-}
-- | Common client monad operations.
module Game.LambdaHack.Client.CommonM
  ( getPerFid, aidTgtToPos, makeLine
  , maxActorSkillsClient, currentSkillsClient, pickWeaponClient
  , updateSalter, createSalter
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
  let assFail = error $ "no perception at given level"
                        `showFailure` (lid, fper)
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
        Nothing -> error $ "" `showFailure` (body, fpos, epsOld)
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
    Nothing -> error $ "" `showFailure` aid

currentSkillsClient :: MonadClient m => ActorId -> m Ability.Skills
currentSkillsClient aid = do
  actorAspect <- getsClient sactorAspect
  let ar = fromMaybe (error $ "" `showFailure` aid) (EM.lookup aid actorAspect)
  body <- getsState $ getActorBody aid
  side <- getsClient sside
  -- Newest Leader in _sleader, not yet in sfactionD.
  mleader <- if side == bfid body
             then getsClient _sleader
             else do
               fact <- getsState $ (EM.! bfid body) . sfactionD
               return $! _gleader fact
  getsState $ actorSkills mleader aid ar  -- keep it lazy

-- Client has to choose the weapon based on its partial knowledge,
-- because if server chose it, it would leak item discovery information.
pickWeaponClient :: MonadClient m
                 => ActorId -> ActorId
                 -> m (Maybe (RequestTimed 'Ability.AbMelee))
pickWeaponClient source target = do
  eqpAssocs <- getsState $ fullAssocs source [CEqp]
  bodyAssocs <- getsState $ fullAssocs source [COrgan]
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
