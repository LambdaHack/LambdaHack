module Game.LambdaHack.Actions where

import Control.Monad
import Control.Monad.State hiding (State, state)
import qualified Data.List as L
import qualified Data.IntMap as IM
import Data.Maybe
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Action
import Game.LambdaHack.Loc
import Game.LambdaHack.Dir
import Game.LambdaHack.Grammar
import Game.LambdaHack.Geometry
import qualified Game.LambdaHack.HighScores as H
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Keys as K
import Game.LambdaHack.Level
import Game.LambdaHack.LevelState
import Game.LambdaHack.Msg
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.ActorAdd
import Game.LambdaHack.Perception
import Game.LambdaHack.State
import qualified Game.LambdaHack.Config as Config
import qualified Game.LambdaHack.Save as Save
import qualified Game.LambdaHack.Effect as Effect
import Game.LambdaHack.EffectAction
import Game.LambdaHack.WorldLoc
import qualified Game.LambdaHack.Tile as Tile
import qualified Game.LambdaHack.Kind as Kind
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.DungeonState
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.TileKind as TileKind
import Game.LambdaHack.Keybindings

-- The Action stuff that is independent from ItemAction.hs.
-- (Both depend on EffectAction.hs).

displayHistory :: Action ()
displayHistory = do
  hst <- gets shistory
  msgOverlayConfirm "" (unlines hst)
  abort

dumpConfig :: Action ()
dumpConfig = do
  config <- gets sconfig
  let fn = "config.dump"
  liftIO $ Config.dump fn config
  abortWith $ "Current configuration dumped to file " ++ fn ++ "."

saveGame :: Action ()
saveGame = do
  b <- msgYesNo "Really save?"
  if b
    then do
      -- Save the game state
      cops <- contentf Kind.coitem
      state <- get
      liftIO $ Save.saveGame state
      ln <- gets slid
      let total = calculateTotal cops state
          status = H.Camping ln
      go <- handleScores False status total
      when go $ msgMore "See you soon, stronger and braver!"
      end
    else abortWith "Game resumed."

quitGame :: Action ()
quitGame = do
  b <- msgYesNo "Really quit?"
  if b
    then end -- no highscore display for quitters
    else abortWith "Game resumed."

moveCursor :: Dir -> Int -> Action ()
moveCursor dir n = do
  lxsize <- gets (lxsize . slevel)
  lysize <- gets (lysize . slevel)
  let upd cursor =
        let boundedShift loc =
              let (sx, sy) = fromLoc lxsize (loc `shift` dir)
                  (bx, by) = (max 1 $ min sx (lxsize - 2),
                              max 1 $ min sy (lysize - 2))
              in toLoc lxsize (bx, by)
            cloc = iterate boundedShift (clocation cursor) !! n
        in cursor{ clocation = cloc }
  modify (updateCursor upd)
  doLook

-- TODO: Think about doing the mode dispatch elsewhere, especially if over
-- time more and more commands need to do the dispatch inside their code
-- (currently only a couple do).
move :: Dir -> Action ()
move dir = do
  pl <- gets splayer
  targeting <- gets (ctargeting . scursor)
  if targeting /= TgtOff then moveCursor dir 1 else moveOrAttack True pl dir

run :: (Dir, Int) -> Action ()
run (dir, dist) = do
  cops <- contentOps
  pl <- gets splayer
  locHere <- gets (bloc . getPlayerBody)
  lvl <- gets slevel
  targeting <- gets (ctargeting . scursor)
  if targeting /= TgtOff
    then moveCursor dir 10
    else do
      let accessibleDir loc d = accessible cops lvl loc (loc `shift` d)
          -- Do not count distance if we just open a door.
          distNew = if accessibleDir locHere dir then dist + 1 else dist
      updatePlayerBody (\ p -> p { bdir = Just (dir, distNew) })
      -- attacks and opening doors disallowed while running
      moveOrAttack False pl dir

-- | Player running mode, determined from the nearby cave layout.
data RunMode =
    RunOpen                  -- ^ open space, in particular the T crossing
  | RunHub                   -- ^ a hub of separate corridors
  | RunCorridor (Dir, Bool)  -- ^ a single corridor, turning here or not
  | RunDeadEnd               -- ^ dead end

-- | Determine the running mode. For corridors, pick the running direction
-- trying to explore all corners, by prefering cardinal to diagonal moves.
runMode :: Loc -> Dir -> (Loc -> Dir -> Bool) -> X -> RunMode
runMode loc dir dirEnterable lxsize =
  let dirNearby dir1 dir2 = dirDistSq lxsize dir1 dir2 == 1
      dirBackward d = dirDistSq lxsize (neg dir) d <= 1
      dirAhead d = dirDistSq lxsize dir d <= 2
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

-- | Check for disturbances to running such newly visible items, monsters, etc.
runDisturbance :: Loc -> Int -> Msg -> Party -> Party -> Perceptions -> Loc
               -> (F.Feature -> Loc -> Bool) -> (Loc -> Bool) -> X -> Y
               -> (Dir, Int) -> Maybe (Dir, Int)
runDisturbance locLast distLast msg hs ms per locHere
               locHasFeature locHasItems lxsize lysize (dirNew, distNew) =
  let msgShown  = not (L.null msg)
      mslocs    = IS.delete locHere $ IS.fromList (L.map bloc (IM.elems ms))
      enemySeen = not (IS.null (mslocs `IS.intersection` totalVisible per))
      surrLast  = locLast : surroundings lxsize lysize locLast
      surrHere  = locHere : surroundings lxsize lysize locHere
      locThere  = locHere `shift` dirNew
      heroThere = locThere `elem` L.map bloc (IM.elems hs)
      -- Stop if you touch any individual tile with these propereties
      -- first time, unless you enter it next move, in which case stop then.
      touchList = [ locHasFeature F.Exit
                  , locHasItems
                  ]
      -- Here additionally ignore a tile property if you stand on such tile.
      standList = [ locHasFeature F.Special
                  , not . locHasFeature F.Lit
                  ]
      -- Here stop only if you touch any such tile for the first time.
      -- TODO: perhaps in open areas change direction to follow lit and special.
      firstList = [ locHasFeature F.Lit
                  , not . locHasFeature F.Special
                  ]
      -- TODO: stop when walls vanish from cardinal directions or when any
      -- walls re-appear again. Actually stop one tile before that happens.
      -- Then remove some other, subsumed conditions.
      -- This will help with corridors starting in dark rooms.
      touchNew fun =
        let touchLast = L.filter (\ loc -> fun loc) surrLast
            touchHere = L.filter (\ loc -> fun loc) surrHere
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
      firstStop fun = firstNew fun
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

-- | This function implements the actual "logic" of running. It checks if we
-- have to stop running because something interesting cropped up
-- and it ajusts the direction if we reached a corridor's corner
-- (we never change direction except in corridors).
continueRun :: (Dir, Int) -> Action ()
continueRun (dirLast, distLast) = do
  cops@Kind.COps{cotile} <- contentOps
  locHere <- gets (bloc . getPlayerBody)
  per <- currentPerception
  msg <- currentMsg
  ms  <- gets (lmonsters . slevel)
  hs  <- gets (lheroes . slevel)
  lvl@Level{lxsize, lysize} <- gets slevel
  let locHasFeature f loc = Tile.hasFeature cotile f (lvl `at` loc)
      locHasItems loc = not $ L.null $ lvl `atI` loc
      locLast = if distLast == 0 then locHere else locHere `shift` (neg dirLast)
      tryRunDist (dir, distNew)
        | accessibleDir locHere dir =
          maybe abort run $
            runDisturbance locLast distLast msg hs ms per locHere
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

ifRunning :: ((Dir, Int) -> Action a) -> Action a -> Action a
ifRunning t e = do
  ad <- gets (bdir . getPlayerBody)
  maybe e t ad

-- | Update player memory.
remember :: Action ()
remember = do
  per <- currentPerception
  lvl <- gets slevel
  let vis = IS.toList (totalVisible per)
  let rememberTile = [(loc, lvl `at` loc) | loc <- vis]
  modify (updateLevel (updateLRMap (Kind.// rememberTile)))
  let alt Nothing      = Nothing
      alt (Just ([], _)) = Nothing
      alt (Just (t, _))  = Just (t, t)
      rememberItem = IM.alter alt
  modify (updateLevel (updateIMap (\ m -> L.foldr rememberItem m vis)))

-- | Guess and report why the bump command failed.
guessBump :: Kind.Ops TileKind -> F.Feature -> Kind.Id TileKind -> Action ()
guessBump cotile F.Openable t | Tile.hasFeature cotile F.Closable t =
  abortWith "already open"
guessBump cotile F.Closable t | Tile.hasFeature cotile F.Openable t =
  abortWith "already closed"
guessBump cotile F.Ascendable t | Tile.hasFeature cotile F.Descendable t =
  abortWith "the way goes down, not up"
guessBump cotile F.Descendable t | Tile.hasFeature cotile F.Ascendable t =
  abortWith "the way goes up, not down"
guessBump _ _ _ = neverMind True

-- | Try to trigger a tile using a feature.
-- TODO: use in more places.
bumpTile :: Loc -> F.Feature -> Action ()
bumpTile dloc feat = do
  cotile <- contentf Kind.cotile
  state <- get
  lvl   <- gets slevel
  pl    <- gets splayer
  let hms = levelHeroList state ++ levelMonsterList state
      t = lvl `at` dloc
  if Tile.hasFeature cotile feat t
    then case lvl `atI` dloc of
      [] -> if unoccupied hms dloc
            then triggerTile cotile lvl dloc
            else abortWith "blocked"  -- by monsters or heroes
      _ : _ -> abortWith "jammed"  -- by items
    else guessBump cotile feat t
  advanceTime pl

-- | Perform the action specified for the tile in case it's triggered.
triggerTile :: Kind.Ops TileKind -> Level -> Loc -> Action ()
triggerTile cotile@Kind.Ops{okind} lvl dloc =
  let f F.Aura{} = return ()  -- TODO
      f F.Cause{} = return ()  -- TODO
      f (F.ChangeTo name) = do
        newTileId <- rndToAction $ Tile.changeTo cotile name
        let adj = (Kind.// [(dloc, newTileId)])
        modify (updateLevel (updateLMap adj))
      f _ = return ()
  in mapM_ f $ TileKind.tfeature $ okind $ lvl `at` dloc

-- | Ask for a direction and alter a tile, if possible.
playerTriggerTile :: F.Feature -> Action ()
playerTriggerTile feat = do
  msgReset "direction?"
  displayAll
  e <- session nextCommand
  lxsize <- gets (lxsize . slevel)
  K.handleDirection lxsize e (playerBumpTile feat) (neverMind True)

-- | Player closes a door. AI never does.
playerBumpTile :: F.Feature -> Dir -> Action ()
playerBumpTile feat dir = do
  pl    <- gets splayer
  body  <- gets (getActor pl)
  let dloc = bloc body `shift` dir
  bumpTile dloc feat

-- | An actor opens a door. Player (hero or monster) or enemy.
actorOpenDoor :: ActorId -> Dir -> Action ()
actorOpenDoor actor dir = do
  Kind.COps{ cotile
           , coitem
           , coactor=Kind.Ops{okind}
           } <- contentOps
  lvl  <- gets slevel
  pl   <- gets splayer
  body <- gets (getActor actor)
  bitems <- gets (getActorItem actor)
  let dloc = shift (bloc body) dir  -- the location we act upon
      t = lvl `at` dloc
      isPlayer = actor == pl
      isVerbose = isPlayer  -- don't report monster failures, if it's not player
      iq = aiq $ okind $ bkind body
      openPower = TileKind.SecretStrength $
        if isPlayer
        then 1  -- player can't open secret doors
        else case strongestSearch coitem bitems of
               Just i  -> iq + jpower i
               Nothing -> iq
  unless (openable cotile lvl openPower dloc) $ neverMind isVerbose
  if Tile.hasFeature cotile F.Closable t
    then abortIfWith isVerbose "already open"
    else if not (Tile.hasFeature cotile F.Closable t ||
                 Tile.hasFeature cotile F.Openable t ||
                 Tile.hasFeature cotile F.Hidden t)
         then neverMind isVerbose  -- not doors at all
         else triggerTile cotile lvl dloc
  advanceTime actor

-- | Attempt a level switch to k levels shallower.
-- TODO: perhaps set up some level name arithmetics in Level.hs
-- and hide there the fact levels are now essentially Ints.
lvlAscend :: Int -> Action ()
lvlAscend k = do
  slid   <- gets slid
  config <- gets sconfig
  let n = levelNumber slid
      nln = n - k
      depth = Config.get config "dungeon" "depth"
  when (nln < 1 || nln > depth) $
    abortWith "no more levels in this direction"
  modify (\ state -> state {slid = (LambdaCave nln)})

-- | Attempt a level change via up level and down level keys.
-- Will quit the game if the player leaves the dungeon.
lvlGoUp :: Bool -> Action ()
lvlGoUp isUp = do
  cotile    <- contentf Kind.cotile
  cursor    <- gets scursor
  targeting <- gets (ctargeting . scursor)
  pbody     <- gets getPlayerBody
  slid      <- gets slid
  lvl       <- gets slevel
  st        <- get
  let loc = if targeting /= TgtOff then clocation cursor else bloc pbody
      tile = lvl `at` loc
      k = if isUp then 1 else -1
      sdir | Tile.hasFeature cotile (F.Cause Effect.Ascend) tile = Just 1
           | Tile.hasFeature cotile (F.Cause Effect.Descend) tile = Just (-1)
           | otherwise = Nothing
  case sdir of
    Just vdir | k == vdir -> -- stairs are in the right direction
      if targeting /= TgtOff
      then case whereTo st k of
        Nothing ->  -- we are at the "end" of the dungeon
          abortWith "cannot escape dungeon in targeting mode"
        Just (nln, nloc) ->
          assert (nln /= slid `blame` (nln, "stairs looped")) $ do
            modify (\ state -> state {slid = nln})
            -- do not freely reveal the other end of the stairs
            lvl2 <- gets slevel
            let upd cur =
                  let clocation =
                        if (lvl2 `rememberAt` nloc) == Tile.unknownId cotile
                        then loc
                        else nloc
                  in cur { clocation, clocLn = nln }
            modify (updateCursor upd)
            doLook
      else do
        effLvlvGoUp k
        playerAdvanceTime
    _ -> -- no stairs in the right direction
      if targeting /= TgtOff
      then do
        lvlAscend k
        let upd cur = cur {clocLn = slid}
        modify (updateCursor upd)
        doLook
      else
        let txt = if isUp then "up" else "down"
        in abortWith ("no stairs " ++ txt)

-- | Switches current hero to the next hero on the level, if any, wrapping.
cycleHero :: Action ()
cycleHero = do
  pl <- gets splayer
  hs <- gets (lheroes . slevel)
  let i        = case pl of AHero n -> n ; _ -> -1
      (lt, gt) = IM.split i hs
  case IM.keys gt ++ IM.keys lt of
    [] -> abortWith "Cannot select another hero on this level."
    ni : _ -> selectPlayer (AHero ni)
              >>= assert `trueM` (pl, ni, "hero duplicated")

-- | Search for secret doors
search :: Action ()
search = do
  Kind.COps{coitem, cotile} <- contentOps
  lvl    <- gets slevel
  le     <- gets (lsecret . slevel)
  lxsize <- gets (lxsize . slevel)
  ploc   <- gets (bloc . getPlayerBody)
  pitems <- gets getPlayerItem
  let delta = case strongestSearch coitem pitems of
                Just i  -> 1 + jpower i
                Nothing -> 1
      searchTile sle mv =
        let loc = shift ploc mv
            t = lvl `at` loc
            k = TileKind.secretStrength (le IM.! loc) - delta
        in if Tile.hasFeature cotile F.Hidden t
           then if k > 0
                then IM.insert loc (TileKind.SecretStrength k) sle
                else IM.delete loc sle
           else sle
      leNew = L.foldl' searchTile le (moves lxsize)
  modify (updateLevel (\ l -> l {lsecret = leNew}))
  lvlNew <- gets slevel
  let triggerHidden mv =
        let dloc = shift ploc mv
            t = lvlNew `at` dloc
        in if Tile.hasFeature cotile F.Hidden t && IM.notMember dloc leNew
           then triggerTile cotile lvlNew dloc
           else return ()
  mapM_ triggerHidden (moves lxsize)
  playerAdvanceTime

-- | This function performs a move (or attack) by any actor,
-- i.e., it can handle monsters, heroes and both.
moveOrAttack :: Bool       -- ^ allow attacks?
             -> ActorId    -- ^ who's moving?
             -> Dir        -- ^ in which direction?
             -> Action ()
moveOrAttack allowAttacks actor dir = do
  -- We start by looking at the target position.
  cops@Kind.COps{cotile = cotile@Kind.Ops{okind}} <- contentOps
  state  <- get
  pl     <- gets splayer
  lvl    <- gets slevel
  sm     <- gets (getActor actor)
  let sloc = bloc sm           -- source location
      tloc = sloc `shift` dir  -- target location
  tgt <- gets (locToActor tloc)
  case tgt of
    Just target
      | allowAttacks ->
          -- Attacking does not require full access, adjacency is enough.
          actorAttackActor actor target
      | accessible cops lvl sloc tloc -> do
          -- Switching positions requires full access.
          actorRunActor actor target
          when (actor == pl) $
            msgAdd $ lookAt cops False True state lvl tloc ""
      | otherwise -> abortWith ""
    Nothing
      | accessible cops lvl sloc tloc -> do
          -- perform the move
          updateAnyActor actor $ \ body -> body {bloc = tloc}
          when (actor == pl) $
            msgAdd $ lookAt cops False True state lvl tloc ""
          advanceTime actor
      | allowAttacks && actor == pl
        && Tile.canBeHidden cotile (okind $ lvl `rememberAt` tloc) -> do
          msgAdd "You search your surroundings."  -- TODO: proper msg
          search
      | otherwise -> actorOpenDoor actor dir  -- try to open a door

-- | Resolves the result of an actor moving into another. Usually this
-- involves melee attack, but with two heroes it just changes focus.
-- Actors on blocked locations can be attacked without any restrictions.
-- For instance, an actor capable of moving through walls
-- can be attacked from an adjacent position.
-- This function is analogous to projectGroupItem, but for melee
-- and not using up the weapon.
actorAttackActor :: ActorId -> ActorId -> Action ()
actorAttackActor source@(AHero _) target@(AHero _) =
  -- Select adjacent hero by bumping into him. Takes no time.
  selectPlayer target
  >>= assert `trueM` (source, target, "player bumps into himself")
actorAttackActor source target = do
  Kind.COps{coactor, coitem} <- contentOps
  state <- get
  sm    <- gets (getActor source)
  tm    <- gets (getActor target)
  per   <- currentPerception
  bitems <- gets (getActorItem source)
  let verb = attackToVerb "sword"  -- TODO
      sloc = bloc sm
      -- The hand-to-hand "weapon", equivalent to +0 sword.
      h2h = Item (fistKindId coitem) 0 Nothing 1
      str = strongestSword coitem bitems
      stack  = fromMaybe h2h str
      single = stack { jcount = 1 }
      -- The msg describes the source part of the action.
      -- TODO: right now it also describes the victim and weapon;
      -- perhaps, when a weapon is equipped, just say "you hit" or "you miss"
      -- and then "nose dies" or "nose yells in pain".
      msg = subjectVerbMObject coactor sm verb tm $
              if isJust str
              then " with " ++ objectItem coitem state single
              else ""
  when (sloc `IS.member` totalVisible per) $ msgAdd msg
  -- Msgs inside itemEffectAction describe the target part.
  itemEffectAction 0 source target single
  advanceTime source

attackToVerb :: String -> String
attackToVerb "sword" = "hit"  -- TODO: "slash"? "pierce"? "swing"?
attackToVerb "mace" = "bludgeon"
attackToVerb _ = "hit"

-- | Resolves the result of an actor running into another.
-- This involves switching positions of the two actors.
actorRunActor :: ActorId -> ActorId -> Action ()
actorRunActor source target = do
  pl   <- gets splayer
  sloc <- gets (bloc . getActor source)  -- source location
  tloc <- gets (bloc . getActor target)  -- target location
  updateAnyActor source $ \ m -> m { bloc = tloc }
  updateAnyActor target $ \ m -> m { bloc = sloc }
  if source == pl
    then stopRunning  -- do not switch positions repeatedly
    else when (isAMonster source) $ focusIfAHero target
  advanceTime source

-- | Generate a monster, possibly.
generateMonster :: Action ()
generateMonster = do
  cops <- contentOps
  state  <- get
  nstate <- rndToAction $ rollMonster cops state
  srandom <- gets srandom
  put $ nstate {srandom}

-- | Possibly regenerate HP for all actors on the current level.
regenerateLevelHP :: Action ()
regenerateLevelHP = do
  Kind.COps{ coitem
           , coactor=coactor@Kind.Ops{okind}
           } <- contentOps
  time <- gets stime
  let upd itemIM a m =
        let ak = okind $ bkind m
            bitems = fromMaybe [] $ IM.lookup a itemIM
            regen = aregen ak `div`
                    case strongestRegen coitem bitems of
                      Just i  -> jpower i
                      Nothing -> 1
        in if time `mod` regen /= 0
           then m
           else addHp coactor 1 m
  -- We really want hero selection to be a purely UI distinction,
  -- so all heroes need to regenerate, not just the player.
  -- Only the heroes on the current level regenerate (others are frozen
  -- in time together with their level). This prevents cheating
  -- via sending one hero to a safe level and waiting there.
  hi  <- gets (lheroItem . slevel)
  modify (updateLevel (updateHeroes   (IM.mapWithKey (upd hi))))
  mi  <- gets (lmonItem . slevel)
  modify (updateLevel (updateMonsters (IM.mapWithKey (upd mi))))

-- | Display command help.
displayHelp :: Action ()
displayHelp = do
  let disp (_, _, keyb@Keybindings{kmacro}) =
        let coImage k =
              let domain = M.keysSet kmacro
              in if k `S.member` domain
                 then []
                 else k : [ from | (from, to) <- M.assocs kmacro, to == k ]
            help = keyHelp coImage keyb
        in msgOverlayConfirm "Basic keys:" help
  session disp
  abort
