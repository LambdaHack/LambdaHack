-- | Running and disturbance.
module Game.LambdaHack.Running
  ( run, continueRun
  ) where

import qualified Data.IntSet as IS
import qualified Data.List as L

import Game.LambdaHack.Action
import Game.LambdaHack.Actions
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.EffectAction
import qualified Game.LambdaHack.Feature as F
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.PointXY
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

-- | Start running in the given direction and with the given number
-- of tiles already traversed (usually 0). The first turn of running
-- succeeds much more often than subsequent turns, because most
-- of the disturbances are ignored, since the player is aware of them
-- and still explicitly requests a run.
run :: MonadAction m => (Vector, Int) -> m ()
run (dir, dist) = do
  cops <- askCOps
  pl <- getsServer splayer
  locHere <- getsServer (bloc . getPlayerBody)
  lvl <- getsServer slevel
  targeting <- getsServer (ctargeting . scursor)
  assert (targeting == TgtOff `blame` (dir, dist, targeting, "/= TgtOff")) $ do
    let accessibleDir loc d = accessible cops lvl loc (loc `shift` d)
        -- Do not count distance if we just open a door.
        distNew = if accessibleDir locHere dir then dist + 1 else dist
    updatePlayerBody (\ p -> p { bdir = Just (dir, distNew) })
    -- Attacks and opening doors disallowed when continuing to run.
    moveOrAttack False pl dir

-- | Player running mode, determined from the nearby cave layout.
data RunMode =
    RunOpen                     -- ^ open space, in particular the T crossing
  | RunHub                      -- ^ a hub of separate corridors
  | RunCorridor (Vector, Bool)  -- ^ a single corridor, turning here or not
  | RunDeadEnd                  -- ^ dead end

-- | Determine the running mode. For corridors, pick the running direction
-- trying to explore all corners, by prefering cardinal to diagonal moves.
runMode :: Point -> Vector -> (Point -> Vector -> Bool) -> X -> RunMode
runMode loc dir dirEnterable lxsize =
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
      dirsEnterable = L.filter (dirEnterable loc) (moves lxsize)
  in case dirsEnterable of
    [] -> assert `failure` (loc, dir)
    [negdir] -> assert (negdir == neg dir) $ RunDeadEnd
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

-- | Check for disturbances to running such as newly visible items, monsters.
runDisturbance :: Point -> Int -> Report
               -> [Actor] -> [Actor] -> Perception -> Point
               -> (F.Feature -> Point -> Bool) -> (Point -> Bool) -> X -> Y
               -> (Vector, Int) -> Maybe (Vector, Int)
runDisturbance locLast distLast msg hs ms per locHere
               locHasFeature locHasItems lxsize lysize (dirNew, distNew) =
  let msgShown  = not $ nullReport msg
      mslocs    = IS.delete locHere $ IS.fromList (L.map bloc ms)
      enemySeen = not (IS.null (mslocs `IS.intersection` totalVisible per))
      surrLast  = locLast : vicinity lxsize lysize locLast
      surrHere  = locHere : vicinity lxsize lysize locHere
      locThere  = locHere `shift` dirNew
      heroThere = locThere `elem` L.map bloc hs
      -- Stop if you touch any individual tile with these propereties
      -- first time, unless you enter it next move, in which case stop then.
      touchList = [ locHasFeature F.Exit
                  , locHasItems
                  ]
      -- Here additionally ignore a tile property if you stand on such tile.
      standList = [ locHasFeature F.Path
                  , not . locHasFeature F.Lit
                  ]
      -- Here stop only if you touch any such tile for the first time.
      -- TODO: stop when running along a path and it ends (or turns).
      -- TODO: perhaps in open areas change direction to follow lit and paths.
      firstList = [ locHasFeature F.Lit
                  , not . locHasFeature F.Path
                  ]
      -- TODO: stop when walls vanish from cardinal directions or when any
      -- walls re-appear again. Actually stop one tile before that happens.
      -- Then remove some other, subsumed conditions.
      -- This will help with corridors starting in dark rooms.
      touchNew fun =
        let touchLast = L.filter fun surrLast
            touchHere = L.filter fun surrHere
        in touchHere L.\\ touchLast
      touchExplore fun = touchNew fun == [locThere]
      touchStop fun = touchNew fun /= []
      standNew fun = L.filter (\ loc -> locHasFeature F.Walkable loc ||
                                        locHasFeature F.Openable loc)
                       (touchNew fun)
      standExplore fun = not (fun locHere) && standNew fun == [locThere]
      standStop fun = not (fun locHere) && standNew fun /= []
      firstNew fun = L.all (not . fun) surrLast &&
                     L.any fun surrHere
      firstExplore fun = firstNew fun && fun locThere
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
continueRun :: MonadAction m => (Vector, Int) -> m ()
continueRun (dirLast, distLast) = do
  cops@Kind.COps{cotile} <- askCOps
  locHere <- getsServer (bloc . getPlayerBody)
  per <- askPerception
  Diary{sreport} <- getDiary
  ms  <- getsServer dangerousList
  sfaction <- getsServer sfaction
  hs <- getsServer (factionList [sfaction])
  lvl@Level{lxsize, lysize} <- getsServer slevel
  let locHasFeature f loc = Tile.hasFeature cotile f (lvl `at` loc)
      locHasItems loc = not $ L.null $ lvl `atI` loc
      locLast = if distLast == 0 then locHere else locHere `shift` neg dirLast
      tryRunDist (dir, distNew)
        | accessibleDir locHere dir =
          -- TODO: perhaps @abortWith report2?
          maybe abort run $
            runDisturbance locLast distLast sreport hs ms per locHere
              locHasFeature locHasItems lxsize lysize (dir, distNew)
        | otherwise = abort  -- do not open doors in the middle of a run
      tryRun dir = tryRunDist (dir, distLast)
      tryRunAndStop dir = tryRunDist (dir, 1000)
      accessibleDir loc dir = accessible cops lvl loc (loc `shift` dir)
      openableDir loc dir   = Tile.hasFeature cotile F.Openable
                                (lvl `at` (loc `shift` dir))
      dirEnterable loc d = accessibleDir loc d || openableDir loc d
  case runMode locHere dirLast dirEnterable lxsize of
    RunDeadEnd -> abort                   -- we don't run backwards
    RunOpen    -> tryRun dirLast          -- run forward into the open space
    RunHub     -> abort                   -- stop and decide where to go
    RunCorridor (dirNext, turn) ->        -- look ahead
      case runMode (locHere `shift` dirNext) dirNext dirEnterable lxsize of
        RunDeadEnd     -> tryRun dirNext  -- explore the dead end
        RunCorridor _  -> tryRun dirNext  -- follow the corridor
        RunOpen | turn -> abort           -- stop and decide when to turn
        RunHub  | turn -> abort           -- stop and decide when to turn
        RunOpen -> tryRunAndStop dirNext  -- no turn, get closer and stop
        RunHub  -> tryRunAndStop dirNext  -- no turn, get closer and stop
