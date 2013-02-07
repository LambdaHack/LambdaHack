{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Semantics of 'CmdSer' server commands.
-- A couple of them do not take time, the rest does.
-- TODO: document
module Game.LambdaHack.Server.CmdSerSem where

import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
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
import Game.LambdaHack.Server.CmdAtomic
import Game.LambdaHack.Server.CmdAtomicSem
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.EffectSem
import Game.LambdaHack.Server.State
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

default (Text)

-- + Semantics of server commands

-- ** ApplySer

applySer :: MonadServerChan m   -- MonadActionAbort m
         => ActorId    -- ^ actor applying the item (is on current level)
         -> MU.Part    -- ^ how the applying is called
         -> ItemId     -- ^ the item to be applied
         -> Container  -- ^ the location of the item
         -> WriterT [CmdAtomic] m ()
applySer actor verb iid container = do
  item <- getsState $ getItemBody iid
  body <- getsState (getActorBody actor)
  let pos = bpos body
  broadcastPosCli [pos] $ ApplyCli actor verb item
  itemEffect 5 actor actor item False
  tell [DestroyItemAtomic iid item 1 container]

-- ** ProjectSer

projectSer :: MonadServerChan m
           => ActorId    -- ^ actor projecting the item (is on current lvl)
           -> Point      -- ^ target position of the projectile
           -> Int        -- ^ digital line parameter
           -> MU.Part    -- ^ how the projecting is called
           -> ItemId     -- ^ the item to be projected
           -> Container  -- ^ whether the items comes from floor or inventory
           -> WriterT [CmdAtomic] m ()
projectSer source tpos eps _verb iid container = do
  cops@Kind.COps{coactor} <- getsState scops
  sm <- getsState (getActorBody source)
  Actor{btime} <- getsState $ getActorBody source
  lvl <- getsState getArena
  lxsize <- getsState (lxsize . getArena)
  lysize <- getsState (lysize . getArena)
  side <- getsState sside
  let spos = bpos sm
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
      time = if bfaction sm == side
             then btimeDelta `timeAdd` timeNegate timeClip
             else btime
      bl = bla lxsize lysize eps spos tpos
  case bl of
    Nothing -> abortWith "cannot zap oneself"
    Just [] -> assert `failure` (spos, tpos, "project from the edge of level")
    Just path@(pos:_) -> do
      inhabitants <- getsState (posToActor pos)
      if accessible cops lvl spos pos && isNothing inhabitants
        then do
          projId <- addProjectile iid pos (bfaction sm) path time
          tell [MoveItemAtomic iid 1 container (CActor projId)]
          item <- getsState $ getItemBody iid
          broadcastPosCli [spos, pos] $ ProjectCli spos source item
        else
          abortWith "blocked"

-- | Create a projectile actor containing the given missile.
addProjectile :: MonadServer m
              => ItemId -> Point -> FactionId -> [Point] -> Time
              -> WriterT [CmdAtomic] m ActorId
addProjectile iid loc bfaction path btime = do
  Kind.COps{coactor, coitem=coitem@Kind.Ops{okind}} <- getsState scops
  disco <- getsState sdisco
  item <- getsState $ getItemBody iid
  let ik = okind (fromJust $ jkind disco item)
      speed = speedFromWeight (iweight ik) (itoThrow ik)
      range = rangeFromSpeed speed
      adj | range < 5 = "falling"
          | otherwise = "flying"
      -- Not much details about a fast flying object.
      (object1, object2) = partItem coitem EM.empty item
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
        , bbag    = EM.empty
        , binv    = EM.empty
        , bletter = InvChar 'a'
        , btime
        , bwait   = timeZero
        , bfaction
        , bproj   = True
        }
  acounter <- getsServer sacounter
  modifyServer $ \ser -> ser {sacounter = succ acounter}
  tell [SpawnAtomic acounter m]
  return acounter

-- ** TriggerSer

-- | Perform the action specified for the tile in case it's triggered.
triggerSer :: MonadServerChan m => ActorId -> Point -> WriterT [CmdAtomic] m ()
triggerSer aid dpos = do
  Kind.COps{cotile=Kind.Ops{okind, opick}} <- getsState scops
  lvl <- getsState getArena
  let f (F.Cause ef) = do
        -- No block against tile, hence @False@.
        void $ effectSem ef 0 aid aid 0 False
        return ()
      f (F.ChangeTo tgroup) = do
        Level{lactor} <- getsState getArena
        if EM.null $ lvl `atI` dpos
          then if unoccupied (EM.elems lactor) dpos
               then do
                 fromTile <- getsState $ (`at` dpos) . getArena
                 toTile <- rndToAction $ opick tgroup (const True)
                 tell [ChangeTileAtomic dpos fromTile toTile]
-- TODO: take care of AI using this function (aborts, etc.).
               else abortWith "blocked"  -- by monsters or heroes
          else abortWith "jammed"  -- by items
      f _ = return ()
  mapM_ f $ TileKind.tfeature $ okind $ lvl `at` dpos

-- * PickupSer

pickupSer :: MonadServerChan m
          => ActorId -> ItemId -> Int -> InvChar -> WriterT [CmdAtomic] m ()
pickupSer aid iid k l = assert (k > 0 `blame` (aid, iid, k, l)) $ do
  side <- getsState sside
  body <- getsState (getActorBody aid)
  -- Nobody can be forced to pick up an item.
  -- TODO: check elsewhere and for all commands.
  assert (bfaction body == side `blame` (body, side)) $ do
    let p = bpos body
    tell [MoveItemAtomic iid k (CFloor p) (CActor aid)]
    void $ broadcastPosCli [p] (PickupCli aid iid k l)

-- ** DropSer

dropSer :: MonadActionRO m => ActorId -> ItemId -> WriterT [CmdAtomic] m ()
dropSer aid iid = do
  p <- getsState (bpos . getActorBody aid)
  let k = 1
  tell [MoveItemAtomic iid k (CActor aid) (CFloor p)]

-- * WaitSer

-- | Update the wait/block count.
waitSer :: MonadActionRO m => ActorId -> WriterT [CmdAtomic] m ()
waitSer aid = do
  Kind.COps{coactor} <- getsState scops
  time <- getsState getTime
  body <- getsState $ getActorBody aid
  let fromWait = bwait body
      toWait = timeAddFromSpeed coactor body time
  tell [WaitAtomic aid fromWait toWait]

-- ** MoveSer

-- | Actor moves or attacks or searches or opens doors.
-- Note that client can't determine which of these actions is chosen,
-- because foes can be invisible, doors hidden, clients can move
-- simultaneously during the same turn, etc. Also, only the server
-- is authorized to check if a move is legal and it needs full context
-- for that, e.g., the initial actor position to check if melee attack
-- does not try to reach to a distant tile.
moveSer :: MonadServerChan m => ActorId -> Vector -> WriterT [CmdAtomic] m ()
moveSer aid dir = do
  cops@Kind.COps{cotile = cotile@Kind.Ops{okind}} <- getsState scops
  lvl <- getsState getArena
  sm <- getsState (getActorBody aid)
  let spos = bpos sm           -- source position
      tpos = spos `shift` dir  -- target position
  -- We start by looking at the target position.
  tgt <- getsState (posToActor tpos)
  side <- getsState sside
  case tgt of
    Just target ->
      -- Attacking does not require full access, adjacency is enough.
      actorAttackActor aid target
    Nothing
      | accessible cops lvl spos tpos ->
          tell [MoveActorAtomic aid spos tpos]
      | Tile.canBeHidden cotile (okind $ lvl `at` tpos) -> do
          sendUpdateCli side
            $ ShowMsgCli "You search all adjacent walls for half a second."
          search aid
      | otherwise ->
          actorOpenDoor aid dir

-- | Resolves the result of an actor moving into another.
-- Actors on blocked positions can be attacked without any restrictions.
-- For instance, an actor embedded in a wall can be attacked from
-- an adjacent position. This function is analogous to projectGroupItem,
-- but for melee and not using up the weapon.
actorAttackActor :: MonadServerChan m
                 => ActorId -> ActorId -> WriterT [CmdAtomic] m ()
actorAttackActor source target = do
  sm <- getsState (getActorBody source)
  tm <- getsState (getActorBody target)
  time <- getsState getTime
  discoS <- getsState sdisco
  s <- getState
  let spos = bpos sm
      tpos = bpos tm
  when (bfaction sm == bfaction tm && isHumanFaction s (bfaction sm)
        && not (bproj sm) && not (bproj tm))
    $ assert `failure` (source, target, "human AI bumps into friendlies")
  cops@Kind.COps{coitem=Kind.Ops{opick, okind}} <- getsState scops
  state <- getState
  bitems <- getsState (getActorItem source)
  let h2hGroup | isSpawningFaction state (bfaction sm) = "monstrous"
               | otherwise = "unarmed"
  h2hKind <- rndToAction $ opick h2hGroup (const True)
  flavour <- getsServer sflavour
  discoRev <- getsServer sdiscoRev
  let h2hItem = buildItem flavour discoRev h2hKind (okind h2hKind) 0
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
        broadcastPosCli [spos, tpos]
          $ ShowAttackCli source target verb stack say
        -- Msgs inside itemEffectSem describe the target part.
        itemEffect verbosity source target stack block
  -- Projectiles can't be blocked, can be sidestepped.
  if braced tm time && not (bproj sm)
    then do
      blocked <- rndToAction $ chance $ 1%2
      if blocked
        then broadcastPosUI [spos, tpos] $ AnimateBlockCli source target verb
        else performHit True
    else performHit False

-- | Search for hidden doors.
search :: MonadServerChan m => ActorId -> WriterT [CmdAtomic] m ()
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
      searchTile diffL mv =
        let loc = shift ppos mv
            t = lvl `at` loc
            (ok, k) = case EM.lookup loc lsecret of
              Nothing -> assert `failure` (loc, lsecret)
              Just ti -> (ti, timeAdd ti $ timeNegate delta)
        in if Tile.hasFeature cotile F.Hidden t
           then if k > timeZero
                then (loc, (Just ok, Just k)) : diffL
                else (loc, (Just ok, Nothing)) : diffL
           else diffL
  let diffL = foldl' searchTile [] (moves lxsize)
  tell [AlterSecretAtomic diffL]
  let triggerHidden (_, (_, Just _)) = return ()
      triggerHidden (dpos, (_, Nothing)) = triggerSer aid dpos
  mapM_ triggerHidden diffL

-- TODO: bumpTile tpos F.Openable
-- | An actor opens a door.
actorOpenDoor :: MonadServerChan m
              => ActorId -> Vector -> WriterT [CmdAtomic] m ()
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
runSer :: MonadServerChan m => ActorId -> Vector -> WriterT [CmdAtomic] m ()
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
          tell [MoveActorAtomic actor spos tpos]
      | otherwise ->
          actorOpenDoor actor dir

-- | When an actor runs (not walks) into another, they switch positions.
displaceActor :: MonadServerChan m
              => ActorId -> ActorId -> WriterT [CmdAtomic] m ()
displaceActor source target = do
  tell [DisplaceActorAtomic source target]
  spos <- getsState $ bpos . getActorBody source
  tpos <- getsState $ bpos . getActorBody target
  broadcastPosUI [spos, tpos] $ DisplaceCli source target
--  leader <- getsClient getLeader
--  if Just source == leader
-- TODO: The actor will stop running due to the message as soon as running
-- is fixed to check the message before it goes into history.
--   then stopRunning  -- do not switch positions repeatedly
--   else void $ focusIfOurs target

-- ** GameExit

gameExitSer :: MonadAction m => m ()
gameExitSer = modifyState $ updateQuit $ const $ Just True

-- ** GameRestart

gameRestartSer :: MonadAction m => m ()
gameRestartSer = do
  let upd f = f {gquit = Just (False, Restart)}
  modifyState $ updateSide upd

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

-- * Assorted helper functions

-- | Create a new monster on the level, at a random position.
rollSpawnPos :: Kind.COps -> ES.EnumSet Point -> Level -> Rnd Point
rollSpawnPos Kind.COps{cotile} visible lvl@Level{lactor} = do
  let inhabitants = actorNotProjList (const True) lvl
      isLit = Tile.isLit cotile
      distantAtLeast d =
        \ l _ -> all (\ h -> chessDist (lxsize lvl) (bpos h) l > d) inhabitants
  findPosTry 40 (ltile lvl)
    [ \ _ t -> not (isLit t)
    , distantAtLeast 15
    , \ l t -> not (isLit t) || distantAtLeast 15 l t
    , distantAtLeast 10
    , \ l _ -> not $ l `ES.member` visible
    , distantAtLeast 5
    , \ l t -> Tile.hasFeature cotile F.Walkable t
               && unoccupied (EM.elems lactor) l
    ]

-- | Generate a monster, possibly.
generateMonster :: (MonadAction m, MonadServer m) => m (Maybe FactionId)
generateMonster = do
  cops@Kind.COps{cofact=Kind.Ops{okind}} <- getsState scops
  pers <- ask
  arena <- getsState sarena
  lvl@Level{ldepth} <- getsState getArena
  faction <- getsState sfaction
  let f fid = fspawn (okind (gkind (faction EM.! fid))) > 0
      spawns = actorNotProjList f lvl
  rc <- rndToAction $ monsterGenChance ldepth (length spawns)
  if not rc
    then return Nothing
    else do
      let allPers =
            ES.unions $ map (totalVisible . (EM.! arena)) $ EM.elems pers
      pos <- rndToAction $ rollSpawnPos cops allPers lvl
      (mf, cmds) <- runWriterT $ spawnMonsters 1 pos
      mapM_ cmdAtomicSem cmds
      return mf

-- | Possibly regenerate HP for all actors on the current level.
--
-- We really want hero selection to be a purely UI distinction,
-- so all heroes need to regenerate, not just the leader.
-- Only the heroes on the current level regenerate (others are frozen
-- in time together with their level). This prevents cheating
-- via sending one hero to a safe level and waiting there.
regenerateLevelHP :: MonadAction m => m ()
regenerateLevelHP = do
  Kind.COps{ coitem
           , coactor=Kind.Ops{okind}
           } <- getsState scops
  time <- getsState getTime
  discoS <- getsState sdisco
  s <- getState
  let pick (a, m) =
        let ak = okind $ bkind m
            items = getActorItem a s
            regen = max 1 $
                      aregen ak `div`
                      case strongestRegen coitem discoS items of
                        Just i  -> 5 * jpower i
                        Nothing -> 1
            bhpMax = maxDice (ahp ak)
            deltaHP = min 1 (bhpMax - bhp m)
        in if (time `timeFit` timeTurn) `mod` regen /= 0 || deltaHP <= 0
           then Nothing
           else Just a
  toRegen <- getsState $ catMaybes . map pick . EM.assocs . lactor . getArena
  mapM_ (\aid -> cmdAtomicSem $ HealAtomic 1 aid) toRegen

-- TODO: let only some actors/items leave smell, e.g., a Smelly Hide Armour.
-- | Add a smell trace for the actor to the level.
addSmell :: MonadActionRO m => ActorId -> WriterT [CmdAtomic] m ()
addSmell aid = do
  time <- getsState getTime
  pos <- getsState $ bpos . getActorBody aid
  oldS <- getsState $ (EM.lookup pos) . lsmell . getArena
  tell [AlterSmellAtomic [(pos, (oldS, Just $ timeAdd time smellTimeout))]]
