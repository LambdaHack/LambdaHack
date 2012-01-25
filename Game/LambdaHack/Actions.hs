-- | The game action stuff that is independent from ItemAction.hs.
-- (Both depend on EffectAction.hs).
-- TODO: Add an export list and document after it's rewritten according to #50.
module Game.LambdaHack.Actions where

import Control.Monad
import Control.Monad.State hiding (State, state)
import qualified Data.List as L
import qualified Data.IntMap as IM
import Data.Maybe
import qualified Data.IntSet as IS

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Action
import Game.LambdaHack.Point
import Game.LambdaHack.Vector
import Game.LambdaHack.Grammar
import qualified Game.LambdaHack.Dungeon as Dungeon
import qualified Game.LambdaHack.HighScore as H
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Key as K
import Game.LambdaHack.Level
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Perception
import Game.LambdaHack.State
import qualified Game.LambdaHack.Config as Config
import qualified Game.LambdaHack.Save as Save
import qualified Game.LambdaHack.Effect as Effect
import Game.LambdaHack.EffectAction
import qualified Game.LambdaHack.Tile as Tile
import qualified Game.LambdaHack.Kind as Kind
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.DungeonState
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.TileKind as TileKind
import Game.LambdaHack.Content.ItemKind

displayHistory :: Action ()
displayHistory = do
  diary <- currentDiary
  msgOverlaysConfirm "" [unlines $ shistory diary]
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
      diary <- currentDiary
      liftIO $ Save.saveGame state diary
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

moveCursor :: Vector -> Int -> Action ()
moveCursor dir n = do
  lxsize <- gets (lxsize . slevel)
  lysize <- gets (lysize . slevel)
  let upd cursor =
        let shiftB loc =
              shiftBounded lxsize (1, 1, lxsize - 2, lysize - 2) loc dir
            cloc = iterate shiftB (clocation cursor) !! n
        in cursor { clocation = cloc }
  modify (updateCursor upd)
  doLook

-- TODO: Think about doing the mode dispatch elsewhere, especially if over
-- time more and more commands need to do the dispatch inside their code
-- (currently only a couple do).
move :: Vector -> Action ()
move dir = do
  pl <- gets splayer
  targeting <- gets (ctargeting . scursor)
  if targeting /= TgtOff then moveCursor dir 1 else moveOrAttack True pl dir

ifRunning :: ((Vector, Int) -> Action a) -> Action a -> Action a
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
guessBump _ F.Openable _ =
  abortWith "not a door"
guessBump cotile F.Closable t | Tile.hasFeature cotile F.Openable t =
  abortWith "already closed"
guessBump _ F.Closable _ =
  abortWith "not a door"
guessBump cotile F.Ascendable t | Tile.hasFeature cotile F.Descendable t =
  abortWith "the way goes down, not up"
guessBump _ F.Ascendable _ =
  abortWith "no stairs up"
guessBump cotile F.Descendable t | Tile.hasFeature cotile F.Ascendable t =
  abortWith "the way goes up, not down"
guessBump _ F.Descendable _ =
  abortWith "no stairs down"
guessBump _ _ _ = neverMind True

-- | Player tries to trigger a tile using a feature.
bumpTile :: Point -> F.Feature -> Action ()
bumpTile dloc feat = do
  cotile <- contentf Kind.cotile
  lvl    <- gets slevel
  let t = lvl `at` dloc
  if Tile.hasFeature cotile feat t
    then triggerTile dloc
    else guessBump cotile feat t
  playerAdvanceTime

-- | Perform the action specified for the tile in case it's triggered.
triggerTile :: Point -> Action ()
triggerTile dloc = do
  Kind.Ops{okind, opick} <- contentf Kind.cotile
  lvl <- gets slevel
  let f (F.Cause effect) = do
        pl <- gets splayer
        (_b, _msg) <- effectToAction effect 0 pl pl 0
        return ()
      f (F.ChangeTo group) = do
        state <- get
        let hms = levelHeroList state ++ levelMonsterList state
        case lvl `atI` dloc of
          [] -> if unoccupied hms dloc
                then do
                  newTileId <- rndToAction $ opick group (const True)
                  let adj = (Kind.// [(dloc, newTileId)])
                  modify (updateLevel (updateLMap adj))
                else abortWith "blocked"  -- by monsters or heroes
          _ : _ -> abortWith "jammed"  -- by items
      f _ = return ()
  mapM_ f $ TileKind.tfeature $ okind $ lvl `at` dloc

-- | Ask for a direction and trigger a tile, if possible.
playerTriggerDir :: F.Feature -> Action ()
playerTriggerDir feat = do
  msgReset "direction?"
  displayAll
  e <- session nextCommand
  lxsize <- gets (lxsize . slevel)
  K.handleDir lxsize e (playerBumpDir feat) (neverMind True)

-- | Player tries to trigger a tile in a given direction.
playerBumpDir :: F.Feature -> Vector -> Action ()
playerBumpDir feat dir = do
  pl    <- gets splayer
  body  <- gets (getActor pl)
  let dloc = bloc body `shift` dir
  bumpTile dloc feat

-- | Player tries to trigger the tile he's standing on.
playerTriggerTile :: F.Feature -> Action ()
playerTriggerTile feat = do
  ploc <- gets (bloc . getPlayerBody)
  bumpTile ploc feat

-- | An actor opens a door. Player (hero or monster) or enemy.
actorOpenDoor :: ActorId -> Vector -> Action ()
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
      openPower = Tile.SecretStrength $
        if isPlayer
        then 1  -- player can't open hidden doors
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
         else triggerTile dloc
  advanceTime actor

-- | Change the displayed level in targeting mode to (at most)
-- k levels shallower. Enters targeting mode, if no already in one.
tgtAscend :: Int -> Action ()
tgtAscend k = do
  cotile    <- contentf Kind.cotile
  cursor    <- gets scursor
  targeting <- gets (ctargeting . scursor)
  slid      <- gets slid
  lvl       <- gets slevel
  st        <- get
  dungeon   <- gets sdungeon
  let loc = clocation cursor
      tile = lvl `at` loc
      rightStairs =
        k ==  1 && Tile.hasFeature cotile (F.Cause Effect.Ascend)  tile ||
        k == -1 && Tile.hasFeature cotile (F.Cause Effect.Descend) tile
  if rightStairs  -- stairs, in the right direction
    then case whereTo st k of
      Nothing ->  -- we are at the "end" of the dungeon
        abortWith "no more levels in this direction"
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
    else do  -- no stairs in the right direction
      let n = Dungeon.levelNumber slid
          depth = Dungeon.depth dungeon
          nln = Dungeon.levelDefault $ min depth $ max 1 $ n - k
      when (nln == slid) $ abortWith "no more levels in this direction"
      modify (\ state -> state {slid = nln})
      let upd cur = cur {clocLn = nln}
      modify (updateCursor upd)
  when (targeting == TgtOff) $ do
    let upd cur = cur {ctargeting = TgtPlayer}
    modify (updateCursor upd)
  doLook

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

-- | Search for hidden doors
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
            k = Tile.secretStrength (le IM.! loc) - delta
        in if Tile.hasFeature cotile F.Hidden t
           then if k > 0
                then IM.insert loc (Tile.SecretStrength k) sle
                else IM.delete loc sle
           else sle
      leNew = L.foldl' searchTile le (moves lxsize)
  modify (updateLevel (\ l -> l {lsecret = leNew}))
  lvlNew <- gets slevel
  let triggerHidden mv =
        let dloc = shift ploc mv
            t = lvlNew `at` dloc
        in if Tile.hasFeature cotile F.Hidden t && IM.notMember dloc leNew
           then triggerTile dloc
           else return ()
  mapM_ triggerHidden (moves lxsize)
  playerAdvanceTime

-- | This function performs a move (or attack) by any actor,
-- i.e., it can handle monsters, heroes and both.
moveOrAttack :: Bool       -- ^ allow attacks?
             -> ActorId    -- ^ who's moving?
             -> Vector     -- ^ in which direction?
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
  Kind.COps{coactor, coitem=coitem@Kind.Ops{opick, okind}} <- contentOps
  state <- get
  sm    <- gets (getActor source)
  tm    <- gets (getActor target)
  per   <- currentPerception
  bitems <- gets (getActorItem source)
  let h2hGroup = if isAHero source then "unarmed" else "monstrous"
  h2hKind <- rndToAction $ opick h2hGroup (const True)
  let sloc = bloc sm
      -- The picked bodily "weapon".
      h2h = Item h2hKind 0 Nothing 1
      str = strongestSword coitem bitems
      stack  = fromMaybe h2h str
      single = stack { jcount = 1 }
      verb = iverbApply $ okind $ jkind single
      -- The msg describes the source part of the action.
      -- TODO: right now it also describes the victim and weapon;
      -- perhaps, when a weapon is equipped, just say "you hit" or "you miss"
      -- and then "nose dies" or "nose yells in pain".
      msg = actorVerbActorExtra coactor sm verb tm $
              if isJust str
              then " with " ++ objectItem coitem state single
              else ""
  when (sloc `IS.member` totalVisible per) $ msgAdd msg
  -- Msgs inside itemEffectAction describe the target part.
  itemEffectAction 0 source target single
  advanceTime source

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
