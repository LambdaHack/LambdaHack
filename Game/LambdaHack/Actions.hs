{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | The game action stuff that is independent from ItemAction.hs
-- (both depend on EffectAction.hs).
-- TODO: Add an export list and document after it's rewritten according to #17.
module Game.LambdaHack.Actions where

-- Cabal
import qualified Paths_LambdaHack as Self (version)

import Control.Monad
import Control.Monad.Writer.Strict (WriterT, tell)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Animation (bposkMiss, swapPlaces)
import Game.LambdaHack.Binding
import qualified Game.LambdaHack.Command as Command
import Game.LambdaHack.Config
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind as TileKind
import Game.LambdaHack.Draw
import Game.LambdaHack.DungeonState
import qualified Game.LambdaHack.Effect as Effect
import Game.LambdaHack.EffectAction
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Key as K
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Misc
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.Random
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Utils.Frequency
import Game.LambdaHack.Vector

default (Text)

gameSave :: MonadAction m => m ()
gameSave = do
  saveGameBkp
  msgAdd "Game progress saved to a backup file."

gameExit :: MonadAction m => m ()
gameExit = do
  b <- displayYesNo "Really save and exit?"
  if b
    then modifyServer (\ s -> s {squit = Just (True, Camping)})
    else abortWith "Game resumed."

gameRestart :: MonadAction m => m ()
gameRestart = do
  b1 <- displayMore ColorFull "You just requested a new game."
  when (not b1) $ neverMind True
  b2 <- displayYesNo "Current progress will be lost! Really restart the game?"
  when (not b2) $ abortWith "Yea, so much still to do."
  modifyServer (\ s -> s {squit = Just (False, Restart)})

moveCursor :: MonadClient m => Vector -> Int -> WriterT Slideshow m ()
moveCursor dir n = do
  lxsize <- getsLocal (lxsize . getArena)
  lysize <- getsLocal (lysize . getArena)
  let upd cursor =
        let shiftB loc =
              shiftBounded lxsize (1, 1, lxsize - 2, lysize - 2) loc dir
            cpos = iterate shiftB (cposition cursor) !! n
        in cursor { cposition = cpos }
  modifyClient (updateCursor upd)
  doLook

ifRunning :: MonadClientRO m => ((Vector, Int) -> m a) -> m a -> m a
ifRunning t e = do
  srunning <- getsClient srunning
  maybe e t srunning

-- | Guess and report why the bump command failed.
guessBump :: MonadActionRoot m => Kind.Ops TileKind -> F.Feature -> Kind.Id TileKind -> m ()
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
bumpTile :: MonadAction m => Point -> F.Feature -> m ()
bumpTile dpos feat = do
  Kind.COps{cotile} <- getsLocal scops
  lvl    <- getsLocal getArena
  let t = lvl `at` dpos
  if Tile.hasFeature cotile feat t
    then triggerTile dpos
    else guessBump cotile feat t

-- | Perform the action specified for the tile in case it's triggered.
triggerTile :: MonadAction m => Point -> m ()
triggerTile dpos = do
  Kind.COps{cotile=Kind.Ops{okind, opick}} <- getsGlobal scops
  lvl <- getsGlobal getArena
  let f (F.Cause effect) = do
        pl <- getsGlobal splayer
        void $ effectToAction effect 0 pl pl 0 False  -- no bposk against tile
        return ()
      f (F.ChangeTo tgroup) = do
        Level{lactor} <- getsGlobal getArena
        case lvl `atI` dpos of
          [] -> if unoccupied (IM.elems lactor) dpos
                then do
                  newTileId <- rndToAction $ opick tgroup (const True)
                  let adj = (Kind.// [(dpos, newTileId)])
                  modifyGlobal (updateArena (updateLMap adj))
-- TODO: take care of AI using this function (aborts, etc.).
                else abortWith "bposked"  -- by monsters or heroes
          _ : _ -> abortWith "jammed"  -- by items
      f _ = return ()
  mapM_ f $ TileKind.tfeature $ okind $ lvl `at` dpos

-- | Ask for a direction and trigger a tile, if possible.
playerTriggerDir :: MonadAction m => F.Feature -> MU.Part -> m ()
playerTriggerDir feat verb = do
  let keys = zip K.dirAllMoveKey $ repeat K.NoModifier
      prompt = makePhrase ["What to", verb MU.:> "? [movement key"]
  e <- displayChoiceUI prompt [] keys
  lxsize <- getsLocal (lxsize . getArena)
  K.handleDir lxsize e (playerBumpDir feat) (neverMind True)

-- | Player tries to trigger a tile in a given direction.
playerBumpDir :: MonadAction m => F.Feature -> Vector -> m ()
playerBumpDir feat dir = do
  pl    <- getsLocal splayer
  body  <- getsLocal (getActor pl)
  let dpos = bpos body `shift` dir
  bumpTile dpos feat

-- | Player tries to trigger the tile he's standing on.
playerTriggerTile :: MonadAction m => F.Feature -> m ()
playerTriggerTile feat = do
  ppos <- getsLocal (bpos . getPlayerBody)
  bumpTile ppos feat

-- | An actor opens a door.
actorOpenDoor :: MonadAction m => ActorId -> Vector -> m ()
actorOpenDoor actor dir = do
  Kind.COps{cotile} <- getsGlobal scops
  lvl  <- getsGlobal getArena
  pl   <- getsGlobal splayer
  body <- getsGlobal (getActor actor)
  let dpos = shift (bpos body) dir  -- the position we act upon
      t = lvl `at` dpos
      isPlayer = actor == pl
      isVerbose = isPlayer  -- don't report, unless it's player-controlled
  unless (openable cotile lvl dpos) $ neverMind isVerbose
  if Tile.hasFeature cotile F.Closable t
    then abortIfWith isVerbose "already open"
    else if not (Tile.hasFeature cotile F.Closable t ||
                 Tile.hasFeature cotile F.Openable t ||
                 Tile.hasFeature cotile F.Hidden t)
         then neverMind isVerbose  -- not doors at all
         else triggerTile dpos

-- | Change the displayed level in targeting mode to (at most)
-- k levels shallower. Enters targeting mode, if not already in one.
tgtAscend :: MonadAction m => Int -> WriterT Slideshow m ()
tgtAscend k = do
  Kind.COps{cotile} <- getsLocal scops
  cursor <- getsClient scursor
  targeting <- getsClient (ctargeting . scursor)
  sarena <- getsGlobal sarena
  lvl       <- getsGlobal getArena
  st        <- getGlobal
  depth     <- getsGlobal sdepth
  let loc = cposition cursor
      tile = lvl `at` loc
      rightStairs =
        k ==  1 && Tile.hasFeature cotile (F.Cause Effect.Ascend)  tile ||
        k == -1 && Tile.hasFeature cotile (F.Cause Effect.Descend) tile
  if rightStairs  -- stairs, in the right direction
    then case whereTo st k of
      Nothing ->  -- we are at the "end" of the dungeon
        abortWith "no more levels in this direction"
      Just (nln, npos) ->
        assert (nln /= sarena `blame` (nln, "stairs looped")) $ do
          -- We only look at the level, but we have to keep current
          -- time somewhere, e.g., for when we change the player
          -- to a hero on this level and then end targeting.
          -- If that's too slow, we could keep current time in the @Cursor@.
          switchLevel nln
          -- do not freely reveal the other end of the stairs
          clvl <- getsLocal getArena
          let upd cur =
                let cposition =
                      if Tile.hasFeature cotile F.Exit (clvl `at` npos)
                      then npos  -- already know as an exit, focus on it
                      else loc   -- unknow, do not reveal the position
                in cur { cposition, cposLn = nln }
          modifyClient (updateCursor upd)
    else do  -- no stairs in the right direction
      let n = levelNumber sarena
          nln = levelDefault $ min depth $ max 1 $ n - k
      when (nln == sarena) $ abortWith "no more levels in this direction"
      switchLevel nln  -- see comment above
      let upd cur = cur {cposLn = nln}
      modifyClient (updateCursor upd)
  when (targeting == TgtOff) $ do
    let upd cur = cur {ctargeting = TgtExplicit}
    modifyClient (updateCursor upd)
  doLook

heroesAfterPl :: MonadClient m => m [ActorId]
heroesAfterPl = do
  pl <- getsLocal splayer
  s  <- getLocal
  let hs = map (tryFindHeroK s) [0..9]
      i = fromMaybe (-1) $ findIndex (== Just pl) hs
      (lt, gt) = (take i hs, drop (i + 1) hs)
  return $ catMaybes gt ++ catMaybes lt

-- | Switches current hero to the next hero on the level, if any, wrapping.
-- We cycle through at most 10 heroes (\@, 1--9).
cycleHero :: MonadAction m => m ()
cycleHero = do
  pl <- getsLocal splayer
  s  <- getLocal
  hs <- heroesAfterPl
  case filter (flip memActor s) hs of
    [] -> abortWith "Cannot select any other hero on this level."
    ni : _ -> selectPlayer ni
                >>= assert `trueM` (pl, ni, "hero duplicated")

-- | Switches current hero to the previous hero in the whole dungeon,
-- if any, wrapping. We cycle through at most 10 heroes (\@, 1--9).
backCycleHero :: MonadAction m => m ()
backCycleHero = do
  pl <- getsLocal splayer
  hs <- heroesAfterPl
  case reverse hs of
    [] -> abortWith "No other hero in the party."
    ni : _ -> selectPlayer ni
                >>= assert `trueM` (pl, ni, "hero duplicated")

-- | Search for hidden doors.
search :: MonadAction m => m ()
search = do
  Kind.COps{coitem, cotile} <- getsGlobal scops
  lvl    <- getsGlobal getArena
  lsecret <- getsGlobal (lsecret . getArena)
  lxsize <- getsGlobal (lxsize . getArena)
  ppos   <- getsGlobal (bpos . getPlayerBody)
  pitems <- getsGlobal getPlayerItem
  discoS <- getsGlobal sdisco
  let delta = timeScale timeTurn $
                case strongestSearch coitem discoS pitems of
                  Just i  -> 1 + jpower i
                  Nothing -> 1
      searchTile sle mv =
        let loc = shift ppos mv
            t = lvl `at` loc
            k = case IM.lookup loc lsecret of
              Nothing -> assert `failure` (loc, lsecret)
              Just st -> timeAdd st $ timeNegate delta
        in if Tile.hasFeature cotile F.Hidden t
           then if k > timeZero
                then IM.insert loc k sle
                else IM.delete loc sle
           else sle
      leNew = foldl' searchTile lsecret (moves lxsize)
  modifyGlobal (updateArena (\ l -> l {lsecret = leNew}))
  lvlNew <- getsGlobal getArena
  let triggerHidden mv = do
        let dpos = shift ppos mv
            t = lvlNew `at` dpos
        when (Tile.hasFeature cotile F.Hidden t && IM.notMember dpos leNew) $
          triggerTile dpos
  mapM_ triggerHidden (moves lxsize)

-- | This function performs a move (or attack) by any actor,
-- i.e., it can handle monsters, heroes and both.
moveOrAttack :: MonadAction m
             => Bool       -- ^ allow attacks?
             -> ActorId    -- ^ who's moving?
             -> Vector     -- ^ in which direction?
             -> m ()
moveOrAttack allowAttacks actor dir = do
  -- We start by looking at the target position.
  cops@Kind.COps{cotile = cotile@Kind.Ops{okind}} <- getsGlobal scops
  loc <- getLocal
  pl     <- getsGlobal splayer
  lvl    <- getsGlobal getArena
  clvl   <- getsLocal getArena
  sm     <- getsGlobal (getActor actor)
  let spos = bpos sm           -- source position
      tpos = spos `shift` dir  -- target position
  tgt <- getsGlobal (posToActor tpos)
  case tgt of
    Just target
      | allowAttacks ->
          -- Attacking does not require full access, adjacency is enough.
          actorAttackActor actor target
      | accessible cops lvl spos tpos -> do
          -- Switching positions requires full access.
          when (actor == pl) $
            msgAdd $ lookAt cops False True loc tpos ""
          actorRunActor actor target
      | otherwise -> abortWith "bposked"
    Nothing
      | accessible cops lvl spos tpos -> do
          -- Perform the move.
          updateAnyActor actor $ \ body -> body {bpos = tpos}
          when (actor == pl) $
            msgAdd $ lookAt cops False True loc tpos ""
      | allowAttacks && actor == pl
        && Tile.canBeHidden cotile (okind $ clvl `at` tpos) -> do
          msgAdd "You search all adjacent walls for half a second."
          search
      | otherwise ->
          actorOpenDoor actor dir  -- try to open a door, TODO: bumpTile tpos F.Openable

-- | Resolves the result of an actor moving into another. Usually this
-- involves melee attack, but with two heroes it just changes focus.
-- Actors on bposked positions can be attacked without any restrictions.
-- For instance, an actor embedded in a wall
-- can be attacked from an adjacent position.
-- This function is analogous to projectGroupItem, but for melee
-- and not using up the weapon.
actorAttackActor :: MonadAction m => ActorId -> ActorId -> m ()
actorAttackActor source target = do
  smRaw <- getsGlobal (getActor source)
  tmRaw <- getsGlobal (getActor target)
  per   <- askPerception
  time  <- getsGlobal getTime
  s <- getGlobal
  let spos = bpos smRaw
      tpos = bpos tmRaw
      svisible = spos `IS.member` totalVisible per
      tvisible = tpos `IS.member` totalVisible per
      sm | svisible  = smRaw
         | otherwise = smRaw {bname = Just "somebody"}
      tm | tvisible  = tmRaw
         | otherwise = tmRaw {bname = Just "somebody"}
  if bfaction sm == bfaction tm && isControlledFaction s (bfaction sm)
     && not (bproj sm) && not (bproj tm)
    then assert `failure` (source, target, "player AI bumps into friendlies")
    else do
      cops@Kind.COps{coactor, coitem=coitem@Kind.Ops{opick, okind}} <- getsGlobal scops
      state <- getGlobal
      bitems <- getsGlobal (getActorItem source)
      let h2hGroup = if isAHero state source then "unarmed" else "monstrous"
      h2hKind <- rndToAction $ opick h2hGroup (const True)
      flavour <- getsServer sflavour
      discoRev <- getsServer sdiscoRev
      disco <- getsGlobal sdisco
      let h2hItem = buildItem flavour discoRev h2hKind (okind h2hKind) 1 0
          (stack, say, verbosity, verb) =
            if isProjectile state source
            then case bitems of
              [bitem] -> (bitem, False, 10, "hit")     -- projectile
              _ -> assert `failure` bitems
            else case strongestSword cops bitems of
              Nothing -> (h2hItem, False, 0,
                          iverbApply $ okind h2hKind)  -- hand to hand combat
              Just w  ->
                let verbApply = case jkind disco w of
                      Nothing -> "hit"
                      Just ik -> iverbApply $ okind ik
                in (w, True, 0, verbApply)
          -- The msg describes the source part of the action.
          -- TODO: right now it also describes the victim and weapon;
          -- perhaps, when a weapon is equipped, just say "you hit"
          -- or "you miss" and then "nose dies" or "nose yells in pain".
          msg = makeSentence $
            [ MU.SubjectVerbSg (partActor coactor sm) verb
            , partActor coactor tm ]
            ++ if say
               then ["with", partItemAW coitem disco stack]
               else []
          msgMiss = makeSentence
            [ MU.SubjectVerbSg (partActor coactor sm) "try to"
            , verb MU.:> ", but"
            , MU.SubjectVerbSg (partActor coactor tm) "bposk"
            ]
      let performHit bposk = do
            when (svisible || tvisible) $ msgAdd msg
            -- Msgs inside itemEffectAction describe the target part.
            itemEffectAction verbosity source target stack bposk
      -- Projectiles can't be bposked, can be sidestepped.
      if braced tm time && not (bproj sm)
        then do
          bposked <- rndToAction $ chance $ 1%2
          if bposked
            then do
              when (svisible || tvisible) $ msgAdd msgMiss
              cli <- getClient
              loc <- getLocal
              let poss = (breturn tvisible tpos,
                          breturn svisible spos)
                  anim = bposkMiss poss
                  animFrs = animate cli loc cops per anim
              displayFramesPush $ Nothing : animFrs
            else performHit True
        else performHit False

-- | Resolves the result of an actor running (not walking) into another.
-- This involves switching positions of the two actors.
actorRunActor :: MonadAction m => ActorId -> ActorId -> m ()
actorRunActor source target = do
  pl <- getsGlobal splayer
  sm <- getsGlobal (getActor source)
  tm <- getsGlobal (getActor target)
  let spos = bpos sm
      tpos = bpos tm
  updateAnyActor source $ \ m -> m { bpos = tpos }
  updateAnyActor target $ \ m -> m { bpos = spos }
  cops@Kind.COps{coactor} <- getsGlobal scops
  per <- askPerception
  let visible = spos `IS.member` totalVisible per ||
                tpos `IS.member` totalVisible per
      msg = makeSentence
        [ MU.SubjectVerbSg (partActor coactor sm) "displace"
        , partActor coactor tm ]
  when visible $ msgAdd msg
  cli <- getClient  -- here cli possibly contains the new msg
  loc <- getLocal
  let poss = (Just tpos, Just spos)
      animFrs = animate cli loc cops per $ swapPlaces poss
  when visible $ displayFramesPush $ Nothing : animFrs
  if source == pl
   then stopRunning  -- do not switch positions repeatedly
   else void $ focusIfOurs target

-- | Create a new monster in the level, at a random position.
rollMonster :: Kind.COps -> Perception -> State -> StateServer
            -> Rnd (State, StateServer)
rollMonster Kind.COps{ cotile
                     , coactor=Kind.Ops{opick, okind}
                     , cofact=Kind.Ops{okind=fokind}
                     } per state ser = do
  let lvl@Level{lactor} = getArena state
      ms = hostileList state
      hs = heroList state
      isLit = Tile.isLit cotile
  rc <- monsterGenChance (levelNumber $ sarena state) (length ms)
  if not rc
    then return (state, ser)
    else do
      let distantAtLeast d =
            \ l _ -> all (\ h -> chessDist (lxsize lvl) (bpos h) l > d) hs
      loc <-
        findPosTry 20 (ltile lvl)  -- 20 only, for unpredictability
          [ \ _ t -> not (isLit t)
          , distantAtLeast 15
          , \ l t -> not (isLit t) || distantAtLeast 15 l t
          , distantAtLeast 10
          , \ l _ -> not $ l `IS.member` totalVisible per
          , distantAtLeast 5
          , \ l t -> Tile.hasFeature cotile F.Walkable t
                     && unoccupied (IM.elems lactor) l
          ]
      let f (fid, fa) =
            let kind = fokind (gkind fa)
            in if fspawn kind <= 0
               then Nothing
               else Just (fspawn kind, (kind, fid))
      case catMaybes $ map f $ IM.toList $ sfaction state of
        [] -> return (state, ser)
        spawnList -> do
          let freq = toFreq "spawn" spawnList
          (spawnKind, bfaction) <- frequency freq
          mk <- opick (fname spawnKind) (const True)
          hp <- rollDice $ ahp $ okind mk
          return $ addMonster cotile mk hp loc bfaction False state ser

-- | Generate a monster, possibly.
generateMonster :: MonadServer m => m ()
generateMonster = do
  cops <- getsGlobal scops
  state <- getGlobal
  ser <- getServer
  per <- askPerceptionSer
  (nstate, nser) <- rndToAction $ rollMonster cops per state ser
  putGlobal nstate
  srandom <- getsServer srandom
  putServer $! nser {srandom}

-- | Possibly regenerate HP for all actors on the current level.
regenerateLevelHP :: MonadServer m => m ()
regenerateLevelHP = do
  Kind.COps{ coitem
           , coactor=coactor@Kind.Ops{okind}
           } <- getsGlobal scops
  time <- getsGlobal getTime
  discoS <- getsGlobal sdisco
  let upd itemIM a m =
        let ak = okind $ bkind m
            bitems = fromMaybe [] $ IM.lookup a itemIM
            regen = max 1 $
                      aregen ak `div`
                      case strongestRegen coitem discoS bitems of
                        Just i  -> 5 * jpower i
                        Nothing -> 1
        in if (time `timeFit` timeTurn) `mod` regen /= 0
           then m
           else addHp coactor 1 m
  -- We really want hero selection to be a purely UI distinction,
  -- so all heroes need to regenerate, not just the player.
  -- Only the heroes on the current level regenerate (others are frozen
  -- in time together with their level). This prevents cheating
  -- via sending one hero to a safe level and waiting there.
  hi <- getsGlobal (linv . getArena)
  modifyGlobal (updateArena (updateActor (IM.mapWithKey (upd hi))))

-- | Display command help.
displayHelp :: MonadClient m => WriterT Slideshow m ()
displayHelp = do
  keyb <- askBinding
  tell $ keyHelp keyb

-- | Display the main menu.
displayMainMenu :: MonadClient m => WriterT Slideshow m ()
displayMainMenu = do
  Kind.COps{corule} <- getsLocal scops
  Binding{krevMap} <- askBinding
  let stripFrame t = case T.uncons t of
        Just ('\n', art) -> map (T.tail . T.init) $ tail . init $ T.lines art
        _ -> assert `failure` "displayMainMenu:" <+> t
      pasteVersion art =
        let pathsVersion = rpathsVersion $ Kind.stdRuleset corule
            version = " Version " ++ showVersion pathsVersion
                      ++ " (frontend: " ++ frontendName
                      ++ ", engine: LambdaHack " ++ showVersion Self.version
                      ++ ") "
            versionLen = length version
        in init art ++ [take (80 - versionLen) (last art) ++ version]
      kds =  -- key-description pairs
        let showKD cmd key = (showT key, Command.cmdDescription cmd)
            revLookup cmd =
              maybe ("", "") (showKD cmd . fst) $ M.lookup cmd krevMap
            cmds = [Command.GameSave, Command.GameExit,
                   Command.GameRestart, Command.Help]
        in map revLookup cmds ++ [(fst (revLookup Command.Clear), "continue")]
      bindings =  -- key bindings to display
        let bindingLen = 25
            fmt (k, d) =
              let gapLen = (8 - T.length k) `max` 1
                  padLen = bindingLen - T.length k - gapLen - T.length d
              in k <> T.replicate gapLen " " <> d <> T.replicate padLen " "
        in map fmt kds
      overwrite =  -- overwrite the art with key bindings
        let over [] line = ([], T.pack line)
            over bs@(binding : bsRest) line =
              let (prefix, lineRest) = break (=='{') line
                  (braces, suffix)   = span  (=='{') lineRest
              in if length braces == 25
                 then (bsRest, T.pack prefix <> binding <> T.pack suffix)
                 else (bs, T.pack line)
        in snd . mapAccumL over bindings
      mainMenuArt = rmainMenuArt $ Kind.stdRuleset corule
      menuOverlay =  -- TODO: switch to Text and use T.justifyLeft
        overwrite $ pasteVersion $ map T.unpack $ stripFrame $ mainMenuArt
  case menuOverlay of
    [] -> assert `failure` "empty Main Menu overlay"
    hd : tl -> do
      slides <- overlayToSlideshow hd tl
      tell slides

displayHistory :: MonadClient m => WriterT Slideshow m ()
displayHistory = do
  StateClient{shistory} <- getClient
  time <- getsLocal getTime
  let turn = time `timeFit` timeTurn
      msg = makeSentence [ "You survived for"
                       , MU.NWs turn "half-second turn" ]
            <+> "Past messages:"
  slides <- overlayToSlideshow msg $ renderHistory shistory
  tell slides

dumpConfig :: MonadServer m => m ()
dumpConfig = do
  Config{configRulesCfgFile} <- getsServer sconfig
  let fn = configRulesCfgFile ++ ".dump"
      msg = "Current game rules configuration dumped to file"
            <+> T.pack fn <> "."
  dumpCfg fn
  abortWith msg

-- | Add new smell traces to the level. Only humans leave a strong scent.
addSmell :: MonadServer m => m ()
addSmell = do
  s  <- getGlobal
  pl <- getsGlobal splayer
  let time = getTime s
      ppos = bpos (getPlayerBody s)
      upd = IM.insert ppos $ timeAdd time smellTimeout
  when (isAHero s pl) $
    modifyGlobal $ updateArena $ updateSmell upd

-- | Update the wait/bposk count.
setWaitBlock :: MonadServer m => ActorId -> m ()
setWaitBlock actor = do
  Kind.COps{coactor} <- getsGlobal scops
  time <- getsGlobal getTime
  updateAnyActor actor $ \ m -> m {bwait = timeAddFromSpeed coactor m time}

-- | Player waits a turn (and bposks, etc.).
waitBlock :: MonadServer m => m ()
waitBlock = do
  pl <- getsGlobal splayer
  setWaitBlock pl
