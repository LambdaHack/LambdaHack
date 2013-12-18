-- | Running and disturbance.
--
-- The general rule is: whatever is behind you (and so ignored previously),
-- determines what you ignore moving forward. This is calcaulated
-- separately for the tiles to the left, to the right and in the middle
-- along the running direction. So, if you want to ignore something
-- start running when you stand on it (or to the right or left, respectively)
-- or by entering it (or passing to the right or left, respectively).
--
-- Some things are never ignored, such as: enemies seen, imporant messages
-- heard, solid tiles and actors in the way.
module Game.LambdaHack.Client.RunAction
  ( continueRunDir
  ) where

import Control.Exception.Assert.Sugar
import qualified Data.ByteString.Char8 as BS
import qualified Data.EnumMap.Strict as EM
import qualified Data.List as L
import Data.Maybe
import Data.Text (Text)

import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.TileKind

verbose :: Bool
verbose = True

abrt :: MonadClientAbort m => Text -> m a
abrt t = abortIfWith verbose $ "Run stopped:" <+> t

-- | This function implements the actual logic of running. It checks if we
-- have to stop running because something interesting cropped up,
-- it ajusts the direction given by the vector if we reached
-- a corridor's corner (we never change direction except in corridors)
-- and it increments the counter of traversed tiles.
continueRunDir :: MonadClientAbort m
               => ActorId -> (Bool, Int)
               -> m (Vector, Int)
continueRunDir aid (permit, distLast) = do
  let maxDistance = 20
  cops <- getsState scops
  body <- getsState $ getActorBody aid
  sreport <- getsClient sreport -- TODO: check the message before it goes into history
  fact <- getsState $ (EM.! bfid body) . sfactionD
  let lid = blid body
  ms <- getsState $ actorList (isAtWar fact) lid
  lvl <- getLevel lid
  let posHere = bpos body
      posLast = boldpos body
      dirLast = displacement posLast posHere
      boringMsgs = map BS.pack [ "You hear some noises." ]
      -- TODO: use a regexp from the UI config instead
      msgShown  = isJust $ findInReport (`notElem` boringMsgs) sreport
      enemySeen = not $ null ms
      check
        | msgShown = abort  -- the message should still be visible
        | enemySeen = abrt "enemy seen"
        | distLast >= maxDistance = abort  -- can come from 1000 below
            -- TODO: abrt $ "reached max distance" <+> show maxDistance
        | accessibleDir cops lvl posHere dirLast =
            checkAndRun aid dirLast distLast
        | not permit = abrt "blocked"  -- don't open doors inside a run
        | otherwise =
            -- Perhaps turn, since the obstacle was visible when the run
            -- started, and so turning is implicitly permitted.
            tryTurning aid distLast
  check

tryTurning :: MonadClientAbort m
           => ActorId -> Int
           -> m (Vector, Int)
tryTurning aid distLast = do
  cops@Kind.COps{cotile} <- getsState scops
  body <- getsState $ getActorBody aid
  let lid = blid body
  lvl@Level{lxsize} <- getLevel lid
  let posHere = bpos body
      posLast = boldpos body
      dirLast = displacement posLast posHere
  let openableDir pos dir = Tile.openable cotile (lvl `at` (pos `shift` dir))
      dirEnterable pos d = accessibleDir cops lvl pos d || openableDir pos d
      dirNearby dir1 dir2 = euclidDistSq lxsize dir1 dir2 `elem` [1, 2]
      dirsEnterable = L.filter (dirEnterable posHere) (moves lxsize)
  case dirsEnterable of
    [] -> assert `failure` "actor is stuck" `twith` (posHere, dirLast)
    [negdir] -> assert (negdir == neg dirLast) $ abrt "dead end"
    _ -> case filter (dirNearby dirLast) dirsEnterable of
      [] -> abrt "blocked and no similar direction available"
      [d] -> if accessibleDir cops lvl posHere d
             then checkAndRun aid d distLast
             else abrt "blocked and the alternative dir is a closed door"
      _ -> abrt "blocked and no unique similar direction found"

checkAndRun :: MonadClientAbort m
            => ActorId -> Vector -> Int
            -> m (Vector, Int)
checkAndRun aid dir distLast = do
  Kind.COps{cotile=cotile@Kind.Ops{okind}} <- getsState scops
  body <- getsState $ getActorBody aid
  smarkSuspect <- getsClient smarkSuspect
  fact <- getsState $ (EM.! bfid body) . sfactionD
  let lid = blid body
  lvl@Level{lxsize} <- getLevel lid
  hs <- getsState $ actorList (not . isAtWar fact) lid
  let posHere = bpos body
      posHasItems pos = not $ EM.null $ lvl `atI` pos
      posThere = posHere `shift` dir
      posLast = boldpos body
      dirLast = displacement posLast posHere
      actorThere = posThere `elem` L.map bpos hs
      -- This is supposed to work on unit vectors --- diagonal, as well as,
      -- vertical and horizontal.
      anglePos :: Point -> Vector -> RadianAngle -> Point
      anglePos pos d angle = shift pos (rotate lxsize angle d)
      -- We assume the tiles have not changes since last running step.
      -- If they did, we don't care --- running should be stopped
      -- because of the change of nearby tiles then (TODO).
      -- We don't take into account the two tiles at the rear of last
      -- surroundings, because the actor may have come from there
      -- (via a diagonal move) and if so, he may be interested in such tiles.
      -- If he arrived directly from the right or left, he is responsible
      -- for starting the run further away, if he does not want to ignore
      -- such tiles as the ones he came from.
      tileLast = lvl `at` posLast
      tileHere = lvl `at` posHere
      tileThere = lvl `at` posThere
      leftPsLast = map (anglePos posHere dirLast) [pi/2, 3*pi/4]
                   ++ map (anglePos posHere dir) [pi/2, 3*pi/4]
      rightPsLast = map (anglePos posHere dirLast) [-pi/2, -3*pi/4]
                    ++ map (anglePos posHere dir) [-pi/2, -3*pi/4]
      leftForwardPosHere = anglePos posHere dir (pi/4)
      rightForwardPosHere = anglePos posHere dir (-pi/4)
      leftTilesLast = map (lvl `at`) leftPsLast
      rightTilesLast = map (lvl `at`) rightPsLast
      leftForwardTileHere = lvl `at` leftForwardPosHere
      rightForwardTileHere = lvl `at` rightForwardPosHere
      featAt = actionFeatures smarkSuspect . okind
      terrainChangeMiddle = featAt tileThere
                            `notElem` map featAt [tileLast, tileHere]
      terrainChangeLeft = featAt leftForwardTileHere
                          `notElem` map featAt leftTilesLast
      terrainChangeRight = featAt rightForwardTileHere
                           `notElem` map featAt rightTilesLast
      itemChangeMiddle = posHasItems posThere
                         `notElem` map posHasItems [posLast, posHere]
      itemChangeLeft = posHasItems leftForwardPosHere
                       `notElem` map posHasItems leftPsLast
      itemChangeRight = posHasItems rightForwardPosHere
                        `notElem` map posHasItems rightPsLast
      check
        | actorThere = abrt "actor in the way"
        | terrainChangeLeft = abrt "terrain change on the left"
        | terrainChangeRight = abrt "terrain change on the right"
        | itemChangeLeft = abrt "item change on the left"
        | itemChangeRight = abrt "item change on the right"
        | terrainChangeMiddle =
            -- The Exit feature marks tiles that need to be entered before
            -- the run is finished. They don't do anything unless triggered,
            -- so the player has a choice no sooner than when he enters them.
            if Tile.hasFeature cotile F.Exit tileThere
            then return (dir, 1000)
            else abrt "terrain change in the middle and no exit"
        | itemChangeMiddle = return (dir, 1000)
                                -- enter the item tile, then stop
        | otherwise = return (dir, distLast + 1)
  check
