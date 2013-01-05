{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Semantics of 'Command.CmdSer' server commands.
-- A couple of them do not take time, the rest does.
-- TODO: document
module Game.LambdaHack.ServerAction where

import Control.Monad
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List
import Data.Maybe
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Action
-- import Game.LambdaHack.Action hiding (MonadAction, MonadActionRO, MonadClient,
--                               MonadClientRO)
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Animation (blockMiss, swapPlaces)
import Game.LambdaHack.ClientAction
import Game.LambdaHack.Config
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.TileKind as TileKind
import Game.LambdaHack.Draw
import Game.LambdaHack.EffectAction
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Key as K
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
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

-- + Semantics of server commands

-- ** GameSaveSer

gameSaveSer :: MonadServer m => m ()
gameSaveSer = saveGameBkp

-- ** CfgDumpSer

cfgDumpSer :: MonadServer m => m ()
cfgDumpSer = do
  Config{configRulesCfgFile} <- getsServer sconfig
  let fn = configRulesCfgFile ++ ".dump"
      msg = "Current game rules configuration dumped to file"
            <+> T.pack fn <> "."
  dumpCfg fn
  -- Wait with confirmation until saved; tell where the file is.
  -- TODO: show abort message to the current client, not all clients
  abortWith msg

-- ** ApplySer

-- TODO: plit into ApplyInvSer and ApplyFloorSer
applySer :: MonadAction m   -- MonadServer m
         => ActorId  -- ^ actor applying the item (is on current level)
         -> Item     -- ^ the item to be applied
         -> Point    -- ^ position of the actor in case applying from the floor
         -> m ()
applySer actor item pos = do
  -- Only one item consumed, even if several in inventory.
  let consumed = item { jcount = 1 }
  removeFromInventory actor consumed pos
  -- TODO: local state is not updated here and itemEffectAction uses it
  itemEffectAction 5 actor actor consumed False

-- TODO: this is subtly wrong: if identical items are on the floor and in
-- inventory, the floor one will be chosen, regardless of player intention.
-- TODO: right now it natily hacks (with the ppos) around removing items
-- of actors that die when applying items. The subtle incorrectness
-- helps here a lot, because items of dead actors land on the floor,
-- so we use them up in inventory, but remove them after use from the floor.
-- TODO: How can this happen considering we remove them before use?
-- | Remove given item from an actor's inventory or floor.
removeFromInventory :: MonadServer m => ActorId -> Item -> Point -> m ()
removeFromInventory actor i pos = do
  b <- removeFromPos i pos
  unless b $
    modifyGlobal (updateAnyActorItem actor (removeItemByLetter i))

-- | Remove given item from the given position. Tell if successful.
removeFromPos :: MonadServer m => Item -> Point -> m Bool
removeFromPos i pos = do
  lvl <- getsGlobal getArena
  if not $ any (equalItemIdentity i) (lvl `atI` pos)
    then return False
    else do
      modifyGlobal (updateArena (updateIMap adj))
      return True
     where
      rib Nothing = assert `failure` (i, pos)
      rib (Just is) =
        case removeItemByIdentity i is of
          [] -> Nothing
          x  -> Just x
      adj = IM.alter rib pos

-- ** DropSer

dropSer :: MonadServer m => ActorId -> Item -> m ()
dropSer aid item = do
  pos  <- getsGlobal (bpos . getActor aid)
  modifyGlobal (updateAnyActorItem aid (removeItemByLetter item))
  modifyGlobal (updateArena (dropItemsAt [item] pos))

-- ** MoveSer

-- | Actor moves or swaps position with others or opens doors.
-- Note that client can't determine which of these actions is chosen,
-- because foes can be invisible, etc.
moveSer :: MonadAction m  -- MonadServer m
        => ActorId    -- ^ who's moving?
        -> Vector     -- ^ in which direction?
        -> m ()
moveSer actor dir = do
  -- We start by looking at the target position.
  cops <- getsGlobal scops
  lvl <- getsGlobal getArena
  sm <- getsGlobal (getActor actor)
  let spos = bpos sm           -- source position
      tpos = spos `shift` dir  -- target position
  tgt <- getsGlobal (posToActor tpos)
  case tgt of
    Just target
      | accessible cops lvl spos tpos ->
          -- Switching positions requires full access.
          actorRunActor actor target
      | otherwise -> abortWith "blocked"
    Nothing
      | accessible cops lvl spos tpos ->
          -- Perform the actual move.
          updateAnyActor actor $ \ body -> body {bpos = tpos}
      | otherwise ->
          actorOpenDoor actor dir  -- try to open a door, TODO: bumpTile tpos F.Openable

















projectGroupItem :: MonadAction m => ActorId  -- ^ actor projecting the item (is on current lvl)
                 -> Point    -- ^ target position of the projectile
                 -> MU.Part  -- ^ how the projecting is called
                 -> Item     -- ^ the item to be projected
                 -> m ()
projectGroupItem source tpos _verb item = do
  cops@Kind.COps{coactor, coitem} <- getsLocal scops
  sm    <- getsLocal (getActor source)
  per   <- askPerceptionSer
  pl    <- getsLocal splayer
  Actor{btime}  <- getsLocal getPlayerBody
  lvl   <- getsLocal getArena
  ceps  <- getsClient (ceps . scursor)
  lxsize <- getsLocal (lxsize . getArena)
  lysize <- getsLocal (lysize . getArena)
  sside <- getsLocal sside
  disco <- getsLocal sdisco
  let consumed = item { jcount = 1 }
      spos = bpos sm
      svisible = spos `IS.member` totalVisible per
      subject =
        if svisible
        then sm
        else sm {bname = Just "somebody"}
      -- When projecting, the first turn is spent aiming.
      -- The projectile is seen one tile from the actor, giving a hint
      -- about the aim and letting the target evade.
      msg = makeSentence
        [ MU.SubjectVerbSg (partActor coactor subject) "aim"
        , partItemNWs coitem disco consumed ]
      -- TODO: AI should choose the best eps.
      eps = if source == pl then ceps else 0
      -- Setting monster's projectiles time to player time ensures
      -- the projectile covers the whole normal distance already the first
      -- turn that the player observes it moving. This removes
      -- the possibility of micromanagement by, e.g.,  waiting until
      -- the first distance is short.
      -- When the monster faction has its selected player, hero player's
      -- projectiles should be set to the time of the opposite party as well.
      -- Both parties would see their own projectiles move part of the way
      -- and the opposite party's projectiles waiting one turn.
      btimeDelta = timeAddFromSpeed coactor sm btime
      time =
        if bfaction sm == sside || source == pl
        then btimeDelta `timeAdd` timeNegate timeClip
        else btime
      bl = bla lxsize lysize eps spos tpos
  case bl of
    Nothing -> abortWith "cannot zap oneself"
    Just [] -> assert `failure` (spos, tpos, "project from the edge of level")
    Just path@(pos:_) -> do
      let projVis = pos `IS.member` totalVisible per
      removeFromInventory source consumed spos
      inhabitants <- getsGlobal (posToActor pos)
      if accessible cops lvl spos pos && isNothing inhabitants
        then do
          glo <- getGlobal
          ser <- getServer
          let (nglo, nser) =
                addProjectile cops consumed pos (bfaction sm) path time glo ser
          putGlobal nglo
          putServer nser
        else
          abortWith "blocked"
      when (svisible || projVis) $ msgAdd msg

playerProjectGroupItem :: MonadAction m => MU.Part -> MU.Part -> [Char] -> m ()
playerProjectGroupItem verb object syms = do
  ms     <- getsLocal hostileList
  lxsize <- getsLocal (lxsize . getArena)
  lysize <- getsLocal (lysize . getArena)
  ppos   <- getsLocal (bpos . getPlayerBody)
  if foesAdjacent lxsize lysize ppos ms
    then abortWith "You can't aim in melee."
    else playerProjectGI verb object syms

playerProjectGI :: MonadAction m => MU.Part -> MU.Part -> [Char] -> m ()
playerProjectGI verb object syms = do
  cli <- getClient
  pos <- getLocal
  pl    <- getsLocal splayer
  case targetToPos cli pos of
    Just p -> do
      Kind.COps{coitem=Kind.Ops{okind}} <- getsLocal scops
      is   <- getsLocal getPlayerItem
      item <- getGroupItem is object syms
                (makePhrase ["What to", verb MU.:> "?"]) "in inventory"
      targeting <- getsClient (ctargeting . scursor)
      when (targeting == TgtAuto) $ endTargeting True
      disco <- getsLocal sdisco
      let verbProject = case jkind disco item of
            Nothing -> verb
            Just ik -> iverbProject $ okind ik
      projectGroupItem pl p verbProject item
    Nothing -> assert `failure` (pos, pl, "target unexpectedly invalid")

actorPickupItem :: MonadAction m => ActorId -> m ()
actorPickupItem actor = do
  Kind.COps{coactor, coitem} <- getsGlobal scops
  pl <- getsGlobal splayer
  per <- askPerception
  lvl <- getsGlobal getArena
  body <- getsGlobal (getActor actor)
  bitems <- getsGlobal (getActorItem actor)
  disco <- getsLocal sdisco
  let p       = bpos body
      perceived = p `IS.member` totalVisible per
      isPlayer  = actor == pl
  -- check if something is here to pick up
  case lvl `atI` p of
    []   -> abortWith "nothing here"
    i:is -> -- pick up first item; TODO: let pl select item; not for monsters
      case assignLetter (jletter i) (bletter body) bitems of
        Just l -> do
          let (ni, nitems) = joinItem (i { jletter = Just l }) bitems
          -- msg depends on who picks up and if a hero can perceive it
          if isPlayer
            then msgAdd $ makePhrase [ letterLabel (jletter ni)
                                     , partItemNWs coitem disco ni ]
            else when perceived $
                   msgAdd $ makeSentence
                     [ MU.SubjectVerbSg (partActor coactor body) "pick up"
                     , partItemNWs coitem disco i ]
          removeFromPos i p
            >>= assert `trueM` (i, is, p, "item is stuck")
          -- add item to actor's inventory:
          updateAnyActor actor $ \ m ->
            m { bletter = maxLetter l (bletter body) }
          modifyGlobal (updateAnyActorItem actor (const nitems))
        Nothing -> abortWith "cannot carry any more"

pickupItem :: MonadAction m => m ()
pickupItem = do
  pl <- getsLocal splayer
  actorPickupItem pl

gameExit :: MonadAction m => m ()
gameExit = do
  b <- displayYesNo "Really save and exit?"
  if b
    then modifyServer (\ s -> s {squit = Just True})
    else abortWith "Game resumed."

gameRestart :: MonadAction m => m ()
gameRestart = do
  b1 <- displayMore ColorFull "You just requested a new game."
  when (not b1) $ neverMind True
  b2 <- displayYesNo "Current progress will be lost! Really restart the game?"
  when (not b2) $ abortWith "Yea, would be a pity to leave them to die."
  let upd f = f {gquit = Just (False, Restart)}
  modifyGlobal $ updateSide upd

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
        void $ effectToAction effect 0 pl pl 0 False  -- no block against tile
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
                else abortWith "blocked"  -- by monsters or heroes
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

-- | Actor moves or attacks or searches or opens doors.
actorAttack :: MonadAction m
            => ActorId    -- ^ who's moving?
            -> Vector     -- ^ in which direction?
            -> m ()
actorAttack actor dir = do
  -- We start by looking at the target position.
  cops@Kind.COps{cotile = cotile@Kind.Ops{okind}} <- getsGlobal scops
  lvl    <- getsGlobal getArena
  clvl   <- getsLocal getArena
  sm     <- getsGlobal (getActor actor)
  let spos = bpos sm           -- source position
      tpos = spos `shift` dir  -- target position
  tgt <- getsGlobal (posToActor tpos)
  case tgt of
    Just target ->
      -- Attacking does not require full access, adjacency is enough.
      actorAttackActor actor target
    Nothing
      | accessible cops lvl spos tpos -> do
          -- Perform the actual move.
          updateAnyActor actor $ \ body -> body {bpos = tpos}
      | Tile.canBeHidden cotile (okind $ clvl `at` tpos) -> do
          msgAdd "You search all adjacent walls for half a second."
          search
      | otherwise ->
          actorOpenDoor actor dir  -- try to open a door, TODO: bumpTile tpos F.Openable

-- | Resolves the result of an actor moving into another. Usually this
-- involves melee attack, but with two heroes it just changes focus.
-- Actors on blocked positions can be attacked without any restrictions.
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
            , MU.SubjectVerbSg (partActor coactor tm) "block"
            ]
      let performHit block = do
            when (svisible || tvisible) $ msgAdd msg
            -- Msgs inside itemEffectAction describe the target part.
            itemEffectAction verbosity source target stack block
      -- Projectiles can't be blocked, can be sidestepped.
      if braced tm time && not (bproj sm)
        then do
          blocked <- rndToAction $ chance $ 1%2
          if blocked
            then do
              when (svisible || tvisible) $ msgAdd msgMiss
              cli <- getClient
              loc <- getLocal
              let poss = (tpos, spos)
                  anim = blockMiss poss
                  animFrs = animate cli loc per anim
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
  Kind.COps{coactor} <- getsGlobal scops
  per <- askPerception
  let visible = spos `IS.member` totalVisible per ||
                tpos `IS.member` totalVisible per
      msg = makeSentence
        [ MU.SubjectVerbSg (partActor coactor sm) "displace"
        , partActor coactor tm ]
  when visible $ msgAdd msg
  cli <- getClient  -- here cli possibly contains the new msg
  loc <- getLocal
  let poss = (tpos, spos)
      animFrs = animate cli loc per $ swapPlaces poss
  when visible $ displayFramesPush $ Nothing : animFrs
  if source == pl
-- TODO: The actor will stop running due to the message as soon as running
-- is fixed to check the message before it goes into history.
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

-- | Update the wait/block count.
setWaitBlock :: MonadServer m => ActorId -> m ()
setWaitBlock actor = do
  Kind.COps{coactor} <- getsGlobal scops
  time <- getsGlobal getTime
  updateAnyActor actor $ \ m -> m {bwait = timeAddFromSpeed coactor m time}

-- | Player waits a turn (and blocks, etc.).
waitBlock :: MonadServer m => m ()
waitBlock = do
  pl <- getsGlobal splayer
  setWaitBlock pl
