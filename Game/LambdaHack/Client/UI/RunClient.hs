{-# LANGUAGE RankNTypes #-}
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
module Game.LambdaHack.Client.UI.RunClient
  ( continueRun, moveRunAid
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.EnumMap.Strict as EM
import Data.Function
import Data.List
import Data.Maybe

import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.TileKind

-- | Continue running in the given direction.
continueRun :: MonadClient m
            => RunParams -> m (Either Msg (RunParams, RequestTimed))
continueRun paramOld =
  case paramOld of
    RunParams{ runMembers = []
             , runStopMsg = Just stopMsg } -> return $ Left stopMsg
    RunParams{ runLeader
             , runMembers = r : rs
             , runDist = 0
             , runStopMsg
             , runInitDir = Just dir } ->
      if r == runLeader then do
        -- Start a many-actor run with distance 1, to prevent changing
        -- direction on first turn, if the original direction is blocked.
        -- We want our runners to keep formation.
        let runDistNew = if null rs then 0 else 1
        continueRun paramOld{runDist = runDistNew, runInitDir = Nothing}
      else do
        runOutcome <- continueRunDir r 0 (Just dir)
        case runOutcome of
          Left "" -> do  -- hack; means that zeroth step OK
            runStopOrCmd <- moveRunAid r dir
            let runMembersNew = if isJust runStopMsg then rs else rs ++ [r]
                paramNew = paramOld {runMembers = runMembersNew}
            return $! case runStopOrCmd of
              Left stopMsg -> assert `failure` (paramOld, stopMsg)
              Right runCmd -> Right (paramNew, runCmd)
          Left runStopMsgCurrent -> do
            let runStopMsgNew = fromMaybe runStopMsgCurrent runStopMsg
                paramNew = paramOld { runMembers = rs
                                    , runStopMsg = Just runStopMsgNew }
            continueRun paramNew
          _ -> assert `failure` (paramOld, runOutcome)
    RunParams{ runLeader
             , runMembers = r : rs
             , runDist
             , runStopMsg
             , runInitDir = Nothing } -> do
      let runDistNew = if r == runLeader then runDist + 1 else runDist
      mdirOrRunStopMsgCurrent <- continueRunDir r runDistNew Nothing
      let runStopMsgCurrent =
            either Just (const Nothing) mdirOrRunStopMsgCurrent
          runStopMsgNew = runStopMsg `mplus` runStopMsgCurrent
          -- We check @runStopMsgNew@, because even if the current actor
          -- runs OK, we want to stop soon if some others had to stop.
          runMembersNew = if isJust runStopMsgNew then rs else rs ++ [r]
          paramNew = paramOld { runMembers = runMembersNew
                              , runDist = runDistNew
                              , runStopMsg = runStopMsgNew }
      case mdirOrRunStopMsgCurrent of
        Left _ -> continueRun paramNew  -- run all undisturbed; only one time
        Right dir -> return $ Right (paramNew, ReqMove r dir)
      -- The potential invisible actor is hit. War is started without asking.
    _ -> assert `failure` paramOld

-- | Actor moves or searches or alters. No visible actor at the position.
moveRunAid :: MonadClient m
           => ActorId -> Vector -> m (Either Msg RequestTimed)
moveRunAid source dir = do
  cops@Kind.COps{cotile} <- getsState scops
  sb <- getsState $ getActorBody source
  let lid = blid sb
  lvl <- getLevel lid
  let spos = bpos sb           -- source position
      tpos = spos `shift` dir  -- target position
      t = lvl `at` tpos
      runStopOrCmd =
        -- Movement requires full access.
        if accessible cops lvl spos tpos then
          -- The potential invisible actor is hit. War started without asking.
          Right $ ReqMove source dir
        -- No access, so search and/or alter the tile. Non-walkability is
        -- not implied by the lack of access.
        else if not (Tile.isWalkable cotile t)
                && (isSecretPos lvl tpos  -- possible secrets here
                    && (Tile.isSuspect cotile t  -- not yet searched
                        || hideTile cotile lvl tpos /= t)  -- searching again
                    || Tile.isOpenable cotile t
                    || Tile.isClosable cotile t
                    || Tile.isChangeable cotile t) then
          if not $ EM.null $ lvl `atI` tpos then
            Left $ showReqFailure AlterBlockItem
          else
            Right $ ReqAlter source tpos Nothing
            -- We don't use MoveSer, because we don't hit invisible actors.
            -- The potential invisible actor, e.g., in a wall or in
            -- an inaccessible doorway, is made known, taking a turn.
            -- If server performed an attack for free
            -- on the invisible actor anyway, the player (or AI)
            -- would be tempted to repeatedly hit random walls
            -- in hopes of killing a monster lurking within.
            -- If the action had a cost, misclicks would incur the cost, too.
            -- Right now the player may repeatedly alter tiles trying to learn
            -- about invisible pass-wall actors, but it costs a turn
            -- and does not harm the invisible actors, so it's not tempting.
       -- Ignore a known boring, not accessible tile.
       else Left "never mind"
  return $! runStopOrCmd

-- | This function implements the actual logic of running. It checks if we
-- have to stop running because something interesting cropped up,
-- it ajusts the direction given by the vector if we reached
-- a corridor's corner (we never change direction except in corridors)
-- and it increments the counter of traversed tiles.
continueRunDir :: MonadClient m
               => ActorId -> Int -> Maybe Vector -> m (Either Msg Vector)
continueRunDir aid distLast mdir = do
  sreport <- getsClient sreport -- TODO: check the message before it goes into history
  let boringMsgs = map BS.pack [ "You hear some noises."
                               , "reveals that the" ]
      boring repLine = any (`BS.isInfixOf` repLine) boringMsgs
      -- TODO: use a regexp from the UI config instead
      msgShown  = isJust $ findInReport (not . boring) sreport
  if msgShown then return $ Left "message shown"
  else do
    let maxDistance = 20
    cops@Kind.COps{cotile} <- getsState scops
    body <- getsState $ getActorBody aid
    let lid = blid body
    lvl <- getLevel lid
    let posHere = bpos body
        posLast = boldpos body
        dirLast = displacement posLast posHere
        dir = fromMaybe dirLast mdir
        posThere = posHere `shift` dir
    actorsThere <- getsState $ posToActors posThere lid
    let openableLast = Tile.isOpenable cotile (lvl `at` (posHere `shift` dir))
        check
          | not $ null actorsThere = return $ Left "actor in the way"
                         -- don't displace actors, except with leader in step 1
          | distLast >= maxDistance =
              return $ Left $ "reached max run distance" <+> tshow maxDistance
          | accessibleDir cops lvl posHere dir =
              if distLast == 0
              then return $ Left ""  -- hack; means that zeroth step OK
              else checkAndRun aid dir
          | distLast /= 1 = return $ Left "blocked"
                            -- don't change direction, except in step 1
          | openableLast = return $ Left "blocked by a closed door"
                           -- the player may prefer to open the door
          | otherwise =
              -- Assume turning is permitted, because this is the start
              -- of the run, so the situation is mostly known to the player
              tryTurning aid
    check

tryTurning :: MonadClient m
           => ActorId -> m (Either Msg Vector)
tryTurning aid = do
  cops@Kind.COps{cotile} <- getsState scops
  body <- getsState $ getActorBody aid
  let lid = blid body
  lvl <- getLevel lid
  let posHere = bpos body
      posLast = boldpos body
      dirLast = displacement posLast posHere
  let openableDir dir = Tile.isOpenable cotile (lvl `at` (posHere `shift` dir))
      dirEnterable dir = accessibleDir cops lvl posHere dir || openableDir dir
      dirNearby dir1 dir2 = euclidDistSqVector dir1 dir2 `elem` [1, 2]
      dirSimilar dir = dirNearby dirLast dir && dirEnterable dir
      dirsSimilar = filter dirSimilar moves
  case dirsSimilar of
    [] -> return $ Left "dead end"
    d1 : ds | all (dirNearby d1) ds ->  -- only one or two directions possible
      case sortBy (compare `on` euclidDistSqVector dirLast)
           $ filter (accessibleDir cops lvl posHere) $ d1 : ds of
        [] ->
          return $ Left "blocked and all similar directions are closed doors"
        d : _ -> checkAndRun aid d
    _ -> return $ Left "blocked and many distant similar directions found"

-- The direction is different than the original, if called from @tryTurning@
-- and the same if from @continueRunDir@.
checkAndRun :: MonadClient m
            => ActorId -> Vector -> m (Either Msg Vector)
checkAndRun aid dir = do
  Kind.COps{cotile=cotile@Kind.Ops{okind}} <- getsState scops
  body <- getsState $ getActorBody aid
  smarkSuspect <- getsClient smarkSuspect
  let lid = blid body
  lvl <- getLevel lid
  let posHere = bpos body
      posHasItems pos = not $ EM.null $ lvl `atI` pos
      posThere = posHere `shift` dir
  actorsThere <- getsState $ posToActors posThere lid
  let posLast = boldpos body
      dirLast = displacement posLast posHere
      -- This is supposed to work on unit vectors --- diagonal, as well as,
      -- vertical and horizontal.
      anglePos :: Point -> Vector -> RadianAngle -> Point
      anglePos pos d angle = shift pos (rotate angle d)
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
      terrainChangeMiddle = null (Tile.causeEffects cotile tileThere)
                              -- step into; will stop next turn due to message
                            && featAt tileThere
                               `notElem` map featAt [tileLast, tileHere]
      terrainChangeLeft = featAt leftForwardTileHere
                          `notElem` map featAt leftTilesLast
      terrainChangeRight = featAt rightForwardTileHere
                           `notElem` map featAt rightTilesLast
      itemChangeLeft = posHasItems leftForwardPosHere
                       `notElem` map posHasItems leftPsLast
      itemChangeRight = posHasItems rightForwardPosHere
                        `notElem` map posHasItems rightPsLast
      check
        | not $ null actorsThere = return $ Left "actor in the way"
                       -- Actor in possibly another direction tnat original.
        | terrainChangeLeft = return $ Left "terrain change on the left"
        | terrainChangeRight = return $ Left "terrain change on the right"
        | itemChangeLeft = return $ Left "item change on the left"
        | itemChangeRight = return $ Left "item change on the right"
        | terrainChangeMiddle = return $ Left "terrain change in the middle"
        | otherwise = return $ Right dir
  check
