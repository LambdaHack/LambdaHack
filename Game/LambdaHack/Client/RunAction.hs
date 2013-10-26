-- | Running and disturbance.
module Game.LambdaHack.Client.RunAction
  ( continueRunDir
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.List as L
import Data.Maybe

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
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.PointXY
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Utils.Assert

-- | Start running in the given direction and with the given number
-- of tiles already traversed (usually 0). The first turn of running
-- succeeds much more often than subsequent turns, because most
-- of the disturbances are ignored, since the player is aware of them
-- and still explicitly requests a run.
canRun :: MonadClient m => ActorId -> (Vector, Int) -> m Bool
canRun leader (dir, dist) = do
  cops <- getsState scops
  b <- getsState $ getActorBody leader
  lvl <- getsLevel (blid b) id
  stgtMode <- getsClient stgtMode
  assert (isNothing stgtMode `blame` (dir, dist, stgtMode)) skip
  return $ accessibleDir cops lvl (bpos b) dir

runDir :: MonadClient m => ActorId -> (Vector, Int) -> m (Vector, Int)
runDir leader (dir, dist) = do
  canR <- canRun leader (dir, dist)
  let -- Do not count distance if we just open a door.
      distNew = if canR then dist + 1 else dist
  return (dir, distNew)

-- | Human running mode, determined from the nearby cave layout.
data RunMode =
    RunOpen                     -- ^ open space, in particular the T crossing
  | RunHub                      -- ^ a hub of separate corridors
  | RunCorridor (Vector, Bool)  -- ^ a single corridor, turning here or not
  | RunDeadEnd                  -- ^ dead end

-- | Determine the running mode. For corridors, pick the running direction
-- trying to explore all corners, by prefering cardinal to diagonal moves.
runMode :: Point -> Vector -> (Point -> Vector -> Bool) -> X -> RunMode
runMode pos dir dirEnterable lxsize =
  let dirNearby dir1 dir2 = euclidDistSq lxsize dir1 dir2 == 1
      dirBackward d = euclidDistSq lxsize (neg dir) d <= 1
      dirAhead d = euclidDistSq lxsize dir d <= 2
      findOpen =
        let f dirC open = open ++
              case L.filter (dirNearby dirC) dirsEnterable of
                l | dirBackward dirC -> dirC : l  -- points backwards
                []  -> []  -- a narrow corridor, just one tile wide
                [_] -> []  -- a turning corridor, two tiles wide
                l   -> dirC : l  -- too wide
        in L.foldr f []
      dirsEnterable = L.filter (dirEnterable pos) (moves lxsize)
  in case dirsEnterable of
    [] -> assert `failure` (pos, dir)
    [negdir] -> assert (negdir == neg dir) RunDeadEnd
    _ ->
      let dirsOpen = findOpen dirsEnterable
          dirsCorridor = dirsEnterable L.\\ dirsOpen
      in case dirsCorridor of
        [] -> RunOpen  -- no corridors
        _ | L.any dirAhead dirsOpen -> RunOpen  -- open space ahead
        [d] -> RunCorridor (d, False)  -- corridor with no turn
        [d1, d2] | dirNearby d1 d2 ->  -- corridor with a turn
          -- Prefer cardinal to diagonal dirs, for hero safety,
          -- even if that means changing direction.
          RunCorridor (if diagonal lxsize d1 then d2 else d1, True)
        _ -> RunHub  -- a hub of many separate corridors

-- TODO: express as MonadActionRO
-- | Check for disturbances to running such as newly visible items, monsters.
runDisturbance :: Point -> Int -> Report
               -> [Actor] -> [Actor] -> Perception -> Bool -> Point
               -> (F.Feature -> Point -> Bool) -> (Point -> Bool) -> X -> Y
               -> (Vector, Int) -> Maybe (Vector, Int)
runDisturbance posLast distLast report hs ms per markSuspect posHere
               posHasFeature posHasItems lxsize lysize (dirNew, distNew) =
  let boringMsgs = map BS.pack [ "Saving backup."
                               , "You hear some noises." ]
      -- TODO: use a regexp from the UI config instead
      msgShown  = isJust $ findInReport (`notElem` boringMsgs) report
      msposs    = ES.delete posHere $ ES.fromList (L.map bpos ms)
      enemySeen =
        not (ES.null (msposs `ES.intersection` totalVisible per))
      surrLast  = posLast : vicinity lxsize lysize posLast
      surrHere  = posHere : vicinity lxsize lysize posHere
      posThere  = posHere `shift` dirNew
      heroThere = posThere `elem` L.map bpos hs
      -- Stop if you touch any individual tile with these propereties
      -- first time, unless you enter it next move, in which case stop then.
      touchList = [ posHasFeature F.Exit
                  , posHasItems
                  ]
      -- Here additionally ignore a tile property if you stand on such tile.
      standList = [ posHasFeature F.Path
                  , not . posHasFeature F.Lit
                  ]
      -- Here stop only if you touch any such tile for the first time.
      -- TODO: stop when running along a path and it ends (or turns).
      -- TODO: perhaps in open areas change direction to follow lit and paths.
      firstList = [ posHasFeature F.Lit
                  , not . posHasFeature F.Path
                  , \t -> markSuspect && posHasFeature F.Suspect t
                    -- TODO: refine for suspect floors (e.g., traps)
                  ]
      -- TODO: stop when walls vanish from cardinal directions or when any
      -- walls re-appear again. Actually stop one tile before that happens.
      -- Then remove some other, subsumed conditions.
      -- This will help with corridors starting in dark rooms.
      touchNew fun =
        let touchLast = L.filter fun surrLast
            touchHere = L.filter fun surrHere
        in touchHere L.\\ touchLast
      touchExplore fun = touchNew fun == [posThere]
      touchStop fun = touchNew fun /= []
      standNew fun = L.filter (\pos -> posHasFeature F.Walkable pos ||
                                       posHasFeature F.Openable pos)
                       (touchNew fun)
      standExplore fun = not (fun posHere) && standNew fun == [posThere]
      standStop fun = not (fun posHere) && standNew fun /= []
      firstNew fun = L.all (not . fun) surrLast &&
                     L.any fun surrHere
      firstExplore fun = firstNew fun && fun posThere
      firstStop = firstNew
      tryRunMaybe
        | msgShown || enemySeen
          || heroThere || distLast >= 40  = Nothing
        | L.any touchExplore touchList    = Just (dirNew, 1000)
        | L.any standExplore standList    = Just (dirNew, 1000)
        | L.any firstExplore firstList    = Just (dirNew, 1000)
        | L.any touchStop touchList       = Nothing
        | L.any standStop standList       = Nothing
        | L.any firstStop firstList       = Nothing
        | otherwise                       = Just (dirNew, distNew)
  in tryRunMaybe

-- | This function implements the actual logic of running. It checks if we
-- have to stop running because something interesting cropped up,
-- it ajusts the direction given by the vector if we reached
-- a corridor's corner (we never change direction except in corridors)
-- and it increments the counter of traversed tiles.
continueRunDir :: MonadClientAbort m
               => ActorId -> (Vector, Int)
               -> m (Vector, Int)
continueRunDir leader (dirLast, distLast) = do
  cops@Kind.COps{cotile} <- getsState scops
  body <- getsState $ getActorBody leader
  let lid = blid body
  per <- getPerFid lid
  sreport <- getsClient sreport -- TODO: check the message before it goes into history
  smarkSuspect <- getsClient smarkSuspect
  fact <- getsState $ (EM.! bfid body) . sfactionD
  ms <- getsState $ actorList (isAtWar fact) lid
  hs <- getsState $ actorList (not . isAtWar fact) lid
  lvl@Level{lxsize, lysize} <- getsLevel (blid body) id
  let posHere = bpos body
      posHasFeature f pos = Tile.hasFeature cotile f (lvl `at` pos)
      posHasItems pos = not $ EM.null $ lvl `atI` pos
      posLast = if distLast == 0 then posHere else posHere `shift` neg dirLast
      tryRunDist (dir, distNew)
        | accessibleDir cops lvl posHere dir =
          -- TODO: perhaps @abortWith report2?
          maybe abort (runDir leader) $
            runDisturbance
              posLast distLast sreport hs ms per smarkSuspect posHere
              posHasFeature posHasItems lxsize lysize (dir, distNew)
        | otherwise = abort  -- do not open doors in the middle of a run
      tryRun dir = tryRunDist (dir, distLast)
      _tryRunAndStop dir = tryRunDist (dir, 1000)
      openableDir pos dir = Tile.hasFeature cotile F.Openable
                              (lvl `at` (pos `shift` dir))
      dirEnterable pos d = accessibleDir cops lvl pos d || openableDir pos d
  case runMode posHere dirLast dirEnterable lxsize of
    RunDeadEnd -> abort                   -- we don't run backwards
    RunOpen    -> tryRun dirLast          -- run forward into the open space
    RunHub     -> abort                   -- stop and decide where to go
    RunCorridor (dirNext, _turn) ->       -- look ahead
      tryRun dirNext
      -- TODO: instead of a lookahead (does not work, since clients have
      -- limited knowledge), pass _turn similarly as in (dir, 1000)
      -- and decide next turn.
      -- TODO: perhaps boldpos can be handy here
      -- case runMode (posHere `shift` dirNext) dirNext dirEnterable lxsize of
      --   RunDeadEnd     -> tryRun dirNext  -- explore the dead end
      --   RunCorridor _  -> tryRun dirNext  -- follow the corridor
      --   RunOpen | turn -> abort           -- stop and decide when to turn
      --   RunHub  | turn -> abort           -- stop and decide when to turn
      --   RunOpen -> tryRunAndStop dirNext  -- no turn, get closer and stop
      --   RunHub  -> tryRunAndStop dirNext  -- no turn, get closer and stop
