-- | Common client monad operations.
module Game.LambdaHack.Client.CommonM
  ( getPerFid, aidTgtToPos, makeLine
  , currentSkillsClient, pickWeaponClient
  , updateSalter, createSalter
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM

import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.Request
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Perception
import           Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Vector
import           Game.LambdaHack.Content.RuleKind
import           Game.LambdaHack.Content.TileKind (TileKind, isUknownSpace)
import           Game.LambdaHack.Core.Random
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs

-- | Get the current perception of a client.
getPerFid :: MonadClientRead m => LevelId -> m Perception
getPerFid lid = do
  fper <- getsClient sfper
  let assFail = error $ "no perception at given level"
                        `showFailure` (lid, fper)
  return $! EM.findWithDefault assFail lid fper

-- | Calculate the position of an actor's target.
aidTgtToPos :: ActorId -> LevelId -> Maybe Target -> State -> Maybe Point
aidTgtToPos _ _ Nothing _ = Nothing
aidTgtToPos aid lidV (Just tgt) s = case tgt of
  TEnemy a ->
    let body = getActorBody a s
    in if blid body == lidV then Just (bpos body) else Nothing
  TNonEnemy a ->
    let body = getActorBody a s
    in if blid body == lidV then Just (bpos body) else Nothing
  TPoint _ lid p ->
    if lid == lidV then Just p else Nothing
  TVector v ->
    let COps{corule=RuleContent{rXmax, rYmax}} = scops s
        b = getActorBody aid s
        shifted = shiftBounded rXmax rYmax (bpos b) v
    in if shifted == bpos b && v /= Vector 0 0 then Nothing else Just shifted

-- | Counts the number of steps until the projectile would hit a non-projectile
-- actor or obstacle. Starts searching with the given eps and returns
-- the first found eps for which the number reaches the distance between
-- actor and target position, or Nothing if none can be found.
makeLine :: MonadStateRead m => Bool -> Actor -> Point -> Int -> m (Maybe Int)
makeLine onlyFirst body fpos epsOld = do
  COps{corule=RuleContent{rXmax, rYmax}, coTileSpeedup} <- getsState scops
  lvl <- getLevel (blid body)
  let dist = chessDist (bpos body) fpos
      calcScore eps = case bla rXmax rYmax eps (bpos body) fpos of
        Just bl ->
          let blDist = take (dist - 1) bl  -- goal not checked; actor well aware
              noActor p = p == fpos || not (occupiedBigLvl p lvl)
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

-- @MonadStateRead@ would be enough, but the logic is sound only on client.
currentSkillsClient :: MonadClientRead m => ActorId -> m Ability.Skills
currentSkillsClient aid = do
  body <- getsState $ getActorBody aid
  side <- getsClient sside
  -- Newest Leader in sleader, not yet in sfactionD.
  mleader <- if bfid body == side
             then getsClient sleader
             else do
               fact <- getsState $ (EM.! bfid body) . sfactionD
               return $! gleader fact
  getsState $ actorCurrentSkills mleader aid  -- keep it lazy

-- Client has to choose the weapon based on its partial knowledge,
-- because if server chose it, it would leak item discovery information.
--
-- Note that currently the aspects of the target actor are not considered,
-- because all weapons share the sum of all source actor aspects and only differ
-- in damage (equally important for all targets) and effects (really hard
-- to tell which is better for which target or even which is better
-- for the same target, so it's random). If only individual weapon's +toHit
-- was applied to the target, situation would be much more complex,
-- which is precisely why we keep it as is and let the player make choices
-- by equipping and unequipping weapons instead. Content should ensure
-- that the rule of thumb (which AI uses) that more weapons is better
-- should give good results almost always, at least at the start of the game,
-- to limit micromanagement and to spare newbies.
--
-- Note that situation is completely different with choosing projectiles
-- against a particular foe, even before (potential) splash damage
-- that hits multiple tagets comes into the equation. AI has to be very
-- primitive and random here as well.
pickWeaponClient :: MonadClient m
                 => ActorId -> ActorId
                 -> m (Maybe RequestTimed)
pickWeaponClient source target = do
  eqpAssocs <- getsState $ kitAssocs source [CEqp]
  bodyAssocs <- getsState $ kitAssocs source [COrgan]
  actorSk <- currentSkillsClient source
  let kitAssRaw = eqpAssocs ++ bodyAssocs
      kitAss = filter (IA.checkFlag Ability.Meleeable
                       . aspectRecordFull . fst . snd) kitAssRaw
  discoBenefit <- getsClient sdiscoBenefit
  strongest <- pickWeaponM False (Just discoBenefit) kitAss actorSk source
  case strongest of
    [] -> return Nothing
    iis@((maxS, _, _, _, _) : _) -> do
      let maxIis = takeWhile (\(value, _, _, _, _) -> value == maxS) iis
      (_, _, _, iid, _) <- rndToAction $ oneOf maxIis
      -- Prefer COrgan, to hint to the player to trash the equivalent CEqp item.
      let cstore = if isJust (lookup iid bodyAssocs) then COrgan else CEqp
      return $ Just $ ReqMelee target iid cstore

updateSalter :: MonadClient m
             => LevelId -> [(Point, ContentId TileKind)] -> m ()
updateSalter lid pts = do
  COps{coTileSpeedup} <- getsState scops
  let pas = map (second $ toEnum . Tile.alterMinWalk coTileSpeedup) pts
      f = (PointArray.// pas)
  modifyClient $ \cli -> cli {salter = EM.adjust f lid $ salter cli}

createSalter :: State -> AlterLid
createSalter s =
  let COps{coTileSpeedup} = scops s
      f Level{ltile} =
        PointArray.mapA (toEnum . Tile.alterMinWalk coTileSpeedup) ltile
  in EM.map f $ sdungeon s
