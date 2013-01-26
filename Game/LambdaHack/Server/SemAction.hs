{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Semantics of 'CmdSer' server commands.
-- A couple of them do not take time, the rest does.
-- TODO: document
module Game.LambdaHack.Server.SemAction where

import Control.Monad
import Control.Monad.Reader.Class
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.CmdCli
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.TileKind as TileKind
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.Random
import Game.LambdaHack.Server.Action
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.EffectAction
import Game.LambdaHack.Server.State
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Utils.Frequency
import Game.LambdaHack.Vector

default (Text)

-- + Semantics of server commands

-- ** GameSaveSer

gameSaveSer :: MonadServerChan m => m ()
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

-- TODO: split into ApplyInvSer and ApplyFloorSer
applySer :: MonadServerChan m   -- MonadServer m
         => ActorId  -- ^ actor applying the item (is on current level)
         -> MU.Part  -- ^ how the applying is called
         -> Item     -- ^ the item to be applied
         -> m ()
applySer actor verb item = do
  body <- getsState (getActorBody actor)
  let pos = bpos body
      -- Only one item consumed, even if several in inventory.
      consumed = item { jcount = 1 }
  broadcastPosCli [pos] $ ApplyCli actor verb consumed
  removeFromInventory actor consumed pos
  itemEffectAction 5 actor actor consumed False

-- TODO: this is subtly wrong: if identical items are on the floor and in
-- inventory, the floor one will be chosen, regardless of player intention.
-- TODO: right now it nastily hacks (with the ppos) around removing items
-- of actors that die when applying items. The subtle incorrectness
-- helps here a lot, because items of dead actors land on the floor,
-- so we use them up in inventory, but remove them after use from the floor.
-- TODO: How can this happen considering we remove them before use?
-- | Remove given item from an actor's inventory or floor.
removeFromInventory :: MonadServer m => ActorId -> Item -> Point -> m ()
removeFromInventory actor i pos = do
  b <- removeFromPos i pos
  unless b $
    modifyState (updateActorItem actor (removeItemByLetter i))

-- | Remove given item from the given position. Tell if successful.
removeFromPos :: MonadServer m => Item -> Point -> m Bool
removeFromPos i pos = do
  lvl <- getsState getArena
  if not $ any (equalItemIdentity i) (lvl `atI` pos)
    then return False
    else do
      modifyState (updateArena (updateIMap adj))
      return True
     where
      rib Nothing = assert `failure` (i, pos)
      rib (Just is) =
        case removeItemByIdentity i is of
          [] -> Nothing
          x  -> Just x
      adj = IM.alter rib pos

-- ** ProjectSer

projectSer :: MonadServerChan m
           => ActorId  -- ^ actor projecting the item (is on current lvl)
           -> Point    -- ^ target position of the projectile
           -> Int      -- ^ digital line parameter
           -> MU.Part  -- ^ how the projecting is called
           -> Item     -- ^ the item to be projected
           -> m ()
projectSer source tpos eps _verb item = do
  cops@Kind.COps{coactor} <- getsState scops
  sm    <- getsState (getActorBody source)
  Actor{btime} <- getsState $ getActorBody source
  lvl   <- getsState getArena
  lxsize <- getsState (lxsize . getArena)
  lysize <- getsState (lysize . getArena)
  side <- getsState sside
  let consumed = item { jcount = 1 }
      spos = bpos sm
      -- When projecting, the first turn is spent aiming.
      -- The projectile is seen one tile from the actor, giving a hint
      -- about the aim and letting the target evade.
      -- TODO: AI should choose the best eps.
      -- Setting monster's projectiles time to player time ensures
      -- the projectile covers the whole normal distance already the first
      -- turn that the player observes it moving. This removes
      -- the possibility of micromanagement by, e.g.,  waiting until
      -- the first distance is short.
      -- When the monster faction has its leader, player's
      -- projectiles should be set to the time of the opposite party as well.
      -- Both parties would see their own projectiles move part of the way
      -- and the opposite party's projectiles waiting one turn.
      btimeDelta = timeAddFromSpeed coactor sm btime
      time =
        if bfaction sm == side
        then btimeDelta `timeAdd` timeNegate timeClip
        else btime
      bl = bla lxsize lysize eps spos tpos
  case bl of
    Nothing -> abortWith "cannot zap oneself"
    Just [] -> assert `failure` (spos, tpos, "project from the edge of level")
    Just path@(pos:_) -> do
      removeFromInventory source consumed spos
      inhabitants <- getsState (posToActor pos)
      if accessible cops lvl spos pos && isNothing inhabitants
        then do
          glo <- getState
          ser <- getServer
          let (nglo, nser) =
                addProjectile cops consumed pos (bfaction sm) path time glo ser
          putState nglo
          putServer nser
          broadcastPosCli [spos, pos] $ ProjectCli spos source consumed
        else
          abortWith "blocked"

-- | Create a projectile actor containing the given missile.
addProjectile :: Kind.COps -> Item -> Point -> FactionId
              -> [Point] -> Time -> State -> StateServer
              -> (State, StateServer)
addProjectile Kind.COps{coactor, coitem=coitem@Kind.Ops{okind}}
              item loc bfaction path btime
              s ser@StateServer{scounter} =
  let ik = okind (fromJust $ jkind (sdisco s) item)
      speed = speedFromWeight (iweight ik) (itoThrow ik)
      range = rangeFromSpeed speed
      adj | range < 5 = "falling"
          | otherwise = "flying"
      -- Not much details about a fast flying object.
      (object1, object2) = partItem coitem M.empty item
      name = makePhrase [MU.AW $ MU.Text adj, object1, object2]
      dirPath = take range $ displacePath path
      m = Actor
        { bkind   = projectileKindId coactor
        , bsymbol = Nothing
        , bname   = Just name
        , bcolor  = Nothing
        , bspeed  = Just speed
        , bhp     = 0
        , bdirAI  = Nothing
        , bpath   = Just dirPath
        , bpos    = loc
        , bletter = 'a'
        , btime
        , bwait   = timeZero
        , bfaction
        , bproj   = True
        }
      upd = updateActor (IM.insert scounter m)
            . updateInv (IM.insert scounter [item])
  in ( updateArena upd s
     , ser {scounter = scounter + 1} )

-- ** TriggerSer

-- | Perform the action specified for the tile in case it's triggered.
triggerSer :: MonadServerChan m => ActorId -> Point -> m ()
triggerSer aid dpos = do
  Kind.COps{cotile=Kind.Ops{okind, opick}} <- getsState scops
  lvl <- getsState getArena
  let f (F.Cause effect) = do
        -- No block against tile, hence @False@.
        void $ effectToAction effect 0 aid aid 0 False
        return ()
      f (F.ChangeTo tgroup) = do
        Level{lactor} <- getsState getArena
        case lvl `atI` dpos of
          [] -> if unoccupied (IM.elems lactor) dpos
                then do
                  newTileId <- rndToAction $ opick tgroup (const True)
                  let adj = (Kind.// [(dpos, newTileId)])
                  modifyState (updateArena (updateLMap adj))
-- TODO: take care of AI using this function (aborts, etc.).
                else abortWith "blocked"  -- by monsters or heroes
          _ : _ -> abortWith "jammed"  -- by items
      f _ = return ()
  mapM_ f $ TileKind.tfeature $ okind $ lvl `at` dpos

-- * PickupSer

pickupSer :: MonadServerChan m => ActorId -> Item -> Char -> m ()
pickupSer aid i l = do
  side <- getsState sside
  body <- getsState (getActorBody aid)
  -- Nobody can be forced to pick up an item.
  assert (bfaction body == side `blame` (body, side)) $ do
    let p = bpos body
    bitems <- getsState (getActorItem aid)
    removeFromPos i p
      >>= assert `trueM` (aid, i, p, "item is stuck")
    let (ni, nitems) = joinItem (i {jletter = Just l}) bitems
    modifyState $ updateActorBody aid $ \m ->
      m {bletter = maxLetter (fromJust $ jletter ni) (bletter m)}
    modifyState (updateActorItem aid (const nitems))
    void $ broadcastPosCli [p] (PickupCli aid i ni)

-- ** DropSer

dropSer :: MonadServer m => ActorId -> Item -> m ()
dropSer aid item = do
  pos <- getsState (bpos . getActorBody aid)
  modifyState (updateActorItem aid (removeItemByLetter item))
  modifyState (updateArena (dropItemsAt [item] pos))

-- * WaitSer

-- | Update the wait/block count.
waitSer :: MonadServer m => ActorId -> m ()
waitSer actor = do
  Kind.COps{coactor} <- getsState scops
  time <- getsState getTime
  modifyState $ updateActorBody actor $ \ m ->
    m {bwait = timeAddFromSpeed coactor m time}

-- ** MoveSer

-- | Actor moves or attacks or searches or opens doors.
-- Note that client can't determine which of these actions is chosen,
-- because foes can be invisible, doors hidden, clients can move
-- simultaneously during the same turn, etc. Also, only the server
-- is authorized to check if a move is legal and it needs full context
-- for that, e.g., the initial actor position to check if melee attack
-- does not try to reach to a distant tile.
moveSer :: MonadServerChan m => ActorId -> Vector -> m ()
moveSer actor dir = do
  cops@Kind.COps{cotile = cotile@Kind.Ops{okind}} <- getsState scops
  lvl <- getsState getArena
  sm <- getsState (getActorBody actor)
  let spos = bpos sm           -- source position
      tpos = spos `shift` dir  -- target position
  -- We start by looking at the target position.
  tgt <- getsState (posToActor tpos)
  side <- getsState sside
  case tgt of
    Just target ->
      -- Attacking does not require full access, adjacency is enough.
      actorAttackActor actor target
    Nothing
      | accessible cops lvl spos tpos ->
          -- Perform the actual move.
          modifyState $ updateActorBody actor $ \ body -> body {bpos = tpos}
      | Tile.canBeHidden cotile (okind $ lvl `at` tpos) -> do
          sendUpdateCli side
            $ ShowMsgCli "You search all adjacent walls for half a second."
          search actor
      | otherwise ->
          actorOpenDoor actor dir

-- | Resolves the result of an actor moving into another.
-- Actors on blocked positions can be attacked without any restrictions.
-- For instance, an actor embedded in a wall can be attacked from
-- an adjacent position. This function is analogous to projectGroupItem,
-- but for melee and not using up the weapon.
actorAttackActor :: MonadServerChan m => ActorId -> ActorId -> m ()
actorAttackActor source target = do
  sm <- getsState (getActorBody source)
  tm <- getsState (getActorBody target)
  time <- getsState getTime
  discoS <- getsState sdisco
  s <- getState
  let spos = bpos sm
      tpos = bpos tm
  if bfaction sm == bfaction tm && isHumanFaction s (bfaction sm)
     && not (bproj sm) && not (bproj tm)
    then assert `failure` (source, target, "human AI bumps into friendlies")
    else do
      cops@Kind.COps{coitem=Kind.Ops{opick, okind}} <- getsState scops
      state <- getState
      bitems <- getsState (getActorItem source)
      let h2hGroup | isSpawningFaction state (bfaction sm) = "monstrous"
                   | otherwise = "unarmed"
      h2hKind <- rndToAction $ opick h2hGroup (const True)
      flavour <- getsServer sflavour
      discoRev <- getsServer sdiscoRev
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
                let verbApply = case jkind discoS w of  -- TODO: use disco
                      Nothing -> "hit"
                      Just ik -> iverbApply $ okind ik
                in (w, True, 0, verbApply)
      let performHit block = do
            broadcastPosCli [spos, tpos] $ ShowAttackCli source target verb stack say
            -- Msgs inside itemEffectAction describe the target part.
            itemEffectAction verbosity source target stack block
      -- Projectiles can't be blocked, can be sidestepped.
      if braced tm time && not (bproj sm)
        then do
          blocked <- rndToAction $ chance $ 1%2
          if blocked
            then broadcastPosUI [spos, tpos] $ AnimateBlockCli source target verb
            else performHit True
        else performHit False

-- | Search for hidden doors.
search :: MonadServerChan m => ActorId -> m ()
search aid = do
  Kind.COps{coitem, cotile} <- getsState scops
  lvl <- getsState getArena
  lsecret <- getsState (lsecret . getArena)
  lxsize <- getsState (lxsize . getArena)
  ppos <- getsState (bpos . getActorBody aid)
  pitems <- getsState (getActorItem aid)
  discoS <- getsState sdisco
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
  modifyState (updateArena (\ l -> l {lsecret = leNew}))
  lvlNew <- getsState getArena
  let triggerHidden mv = do
        let dpos = shift ppos mv
            t = lvlNew `at` dpos
        when (Tile.hasFeature cotile F.Hidden t && IM.notMember dpos leNew) $
          triggerSer aid dpos
  mapM_ triggerHidden (moves lxsize)

-- TODO: bumpTile tpos F.Openable
-- | An actor opens a door.
actorOpenDoor :: MonadServerChan m => ActorId -> Vector -> m ()
actorOpenDoor actor dir = do
  Kind.COps{cotile} <- getsState scops
  lvl<- getsState getArena
  body <- getsState (getActorBody actor)
  glo <- getState
  let dpos = shift (bpos body) dir  -- the position we act upon
      t = lvl `at` dpos
      isVerbose = isHumanFaction glo (bfaction body)
  unless (openable cotile lvl dpos) $ neverMind isVerbose
  if Tile.hasFeature cotile F.Closable t
    then abortIfWith isVerbose "already open"
    else if not (Tile.hasFeature cotile F.Closable t ||
                 Tile.hasFeature cotile F.Openable t ||
                 Tile.hasFeature cotile F.Hidden t)
         then neverMind isVerbose  -- not doors at all
         else triggerSer actor dpos

-- ** RunSer

-- | Actor moves or swaps position with others or opens doors.
runSer :: MonadServerChan m => ActorId -> Vector -> m ()
runSer actor dir = do
  cops <- getsState scops
  lvl <- getsState getArena
  sm <- getsState (getActorBody actor)
  let spos = bpos sm           -- source position
      tpos = spos `shift` dir  -- target position
  -- We start by looking at the target position.
  tgt <- getsState (posToActor tpos)
  case tgt of
    Just target
      | accessible cops lvl spos tpos ->
          -- Switching positions requires full access.
          displaceActor actor target
      | otherwise -> abortWith "blocked"
    Nothing
      | accessible cops lvl spos tpos ->
          -- Perform the actual move.
          modifyState $ updateActorBody actor $ \ body -> body {bpos = tpos}
      | otherwise ->
          actorOpenDoor actor dir

-- | When an actor runs (not walks) into another, they switch positions.
displaceActor :: MonadServerChan m => ActorId -> ActorId -> m ()
displaceActor source target = do
  sm <- getsState (getActorBody source)
  tm <- getsState (getActorBody target)
  let spos = bpos sm
      tpos = bpos tm
  modifyState $ updateActorBody source $ \ m -> m { bpos = tpos }
  modifyState $ updateActorBody target $ \ m -> m { bpos = spos }
  broadcastPosUI [spos, tpos] $ DisplaceCli source target
--  leader <- getsClient getLeader
--  if Just source == leader
-- TODO: The actor will stop running due to the message as soon as running
-- is fixed to check the message before it goes into history.
--   then stopRunning  -- do not switch positions repeatedly
--   else void $ focusIfOurs target

-- ** GameExit

gameExitSer :: MonadServer m => m ()
gameExitSer = modifyState $ updateQuit $ const $ Just True

-- ** GameRestart

gameRestartSer :: MonadServer m => m ()
gameRestartSer = do
  let upd f = f {gquit = Just (False, Restart)}
  modifyState $ updateSide upd

-- * Assorted helper server functions.

-- | Create a new monster on the level, at a random position.
rollMonster :: Kind.COps -> IS.IntSet -> State -> StateServer
            -> Rnd (Maybe (FactionId, (State, StateServer)))
rollMonster Kind.COps{ cotile
                     , coactor=Kind.Ops{opick, okind}
                     , cofact=Kind.Ops{okind=fokind}
                     } visible state ser = do
  let f (fid, fa) =
        let kind = fokind (gkind fa)
        in if fspawn kind <= 0
           then Nothing
           else Just (fspawn kind, (kind, fid))
  case catMaybes $ map f $ IM.toList $ sfaction state of
    [] -> return Nothing
    spawnList -> do
      let freq = toFreq "spawn" spawnList
      (spawnKind, bfaction) <- frequency freq
      let lvl@Level{lactor} = getArena state
          ours = actorNotProjList (== bfaction) lvl
          others = actorNotProjList (/= bfaction) lvl
          isLit = Tile.isLit cotile
      rc <- monsterGenChance (levelNumber $ sarena state) (length ours)
      if not rc
        then return Nothing
        else do
          let distantAtLeast d =
                \ l _ -> all (\ h ->
                               chessDist (lxsize lvl) (bpos h) l > d) others
          loc <-
            findPosTry 40 (ltile lvl)
              [ \ _ t -> not (isLit t)
              , distantAtLeast 15
              , \ l t -> not (isLit t) || distantAtLeast 15 l t
              , distantAtLeast 10
              , \ l _ -> not $ l `IS.member` visible
              , distantAtLeast 5
              , \ l t -> Tile.hasFeature cotile F.Walkable t
                         && unoccupied (IM.elems lactor) l
              ]
          mk <- opick (fname spawnKind) (const True)
          hp <- rollDice $ ahp $ okind mk
          return $ Just $
            (bfaction, addMonster cotile mk hp loc bfaction False state ser)

-- | Generate a monster, possibly.
generateMonster :: MonadServer m => m (Maybe FactionId)
generateMonster = do
  cops <- getsState scops
  state <- getState
  ser <- getServer
  pers <- ask
  arena <- getsState sarena
  let allPers = IS.unions $ map (totalVisible . (M.! arena)) $ IM.elems pers
  nst <- rndToAction $ rollMonster cops allPers state ser
  case nst of
    Nothing -> return Nothing
    Just (fid, (nstate, nser)) -> do
      random <- getsState srandom
      putState $! updateRandom (const random) nstate
      putServer nser
      return $ Just fid

-- | Possibly regenerate HP for all actors on the current level.
regenerateLevelHP :: MonadServer m => m ()
regenerateLevelHP = do
  Kind.COps{ coitem
           , coactor=coactor@Kind.Ops{okind}
           } <- getsState scops
  time <- getsState getTime
  discoS <- getsState sdisco
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
  -- so all heroes need to regenerate, not just the leader.
  -- Only the heroes on the current level regenerate (others are frozen
  -- in time together with their level). This prevents cheating
  -- via sending one hero to a safe level and waiting there.
  hi <- getsState (linv . getArena)
  modifyState (updateArena (updateActor (IM.mapWithKey (upd hi))))

-- | Add new smell traces to the level. Only non-spawning factions
-- leave a scent that is easy to tell from common dungeon smells.
addSmell :: MonadServer m => ActorId -> m ()
addSmell aid = do
  time <- getsState getTime
  ppos <- getsState $ bpos . getActorBody aid
  let upd = IM.insert ppos $ timeAdd time smellTimeout
  modifyState $ updateArena $ updateSmell upd
