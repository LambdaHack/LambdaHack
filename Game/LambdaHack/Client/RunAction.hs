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

import qualified Data.ByteString.Char8 as BS
import qualified Data.EnumMap.Strict as EM
import qualified Data.List as L
import Data.Maybe

import Control.Exception.Assert.Sugar
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

-- TODO: perhaps abortWith report_about_disturbance?
-- | This function implements the actual logic of running. It checks if we
-- have to stop running because something interesting cropped up,
-- it ajusts the direction given by the vector if we reached
-- a corridor's corner (we never change direction except in corridors)
-- and it increments the counter of traversed tiles.
continueRunDir :: MonadClientAbort m
               => ActorId -> (Vector, Int)
               -> m (Vector, Int)
continueRunDir aid (dirLast, distLast) = do
  cops@Kind.COps{cotile=cotile@Kind.Ops{okind}} <- getsState scops
  body <- getsState $ getActorBody aid
  let lid = blid body
  sreport <- getsClient sreport -- TODO: check the message before it goes into history
  smarkSuspect <- getsClient smarkSuspect
  fact <- getsState $ (EM.! bfid body) . sfactionD
  ms <- getsState $ actorList (isAtWar fact) lid
  hs <- getsState $ actorList (not . isAtWar fact) lid
  lvl@Level{lxsize} <- getLevel $ blid body
  let posHere = bpos body
      posHasItems pos = not $ EM.null $ lvl `atI` pos
      posThere  = posHere `shift` dirLast
      posLast = boldpos body
  assert (posHere == shift posLast dirLast) skip  -- TODO: deduce dirLast
  let boringMsgs = map BS.pack [ "You hear some noises." ]
      -- TODO: use a regexp from the UI config instead
      msgShown  = isJust $ findInReport (`notElem` boringMsgs) sreport
      enemySeen = not $ null ms
      actorThere = posThere `elem` L.map bpos hs
      -- This is supposed to work on unit vectors --- diagonal, as well as,
      -- vertical and horizontal.
      anglePos :: Point -> Vector -> RadianAngle -> Point
      anglePos pos dir angle = shift pos (rotate lxsize angle dir)
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
      rightPsLast = map (anglePos posHere dirLast) [-pi/2, -3*pi/4]
      leftForwardPosHere = anglePos posHere dirLast (pi/4)
      rightForwardPosHere = anglePos posHere dirLast (-pi/4)
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
  if not (accessibleDir cops lvl posHere dirLast)  -- don't open doors via run
     || msgShown || enemySeen || actorThere || distLast >= 20 then abort
  else if terrainChangeLeft || terrainChangeRight
          || itemChangeLeft || itemChangeRight then abort
  else if itemChangeMiddle then return (dirLast, 1000)
                                -- enter the item tile, then stop
  else if terrainChangeMiddle then
    -- The Exit feature marks tiles that need to be entered before
    -- the run is finished. They don't do anything unless triggered,
    -- so the player has a choice no sooner than when he enters them.
    if Tile.hasFeature cotile F.Exit tileThere
    then return (dirLast, 1000)
    else abort
  else return (dirLast, distLast + 1)
