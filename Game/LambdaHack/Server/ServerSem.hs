{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Semantics of 'CmdSer' server commands.
-- A couple of them do not take time, the rest does.
-- Note that since the results are atomic commands, which are executed
-- only later (on the server and some of the clients), all condition
-- are checkd by the semantic functions in the context of the state
-- before the server command. Even if one or more atomic actions
-- are already issued by the point an expression is evaluated, they do not
-- influence the outcome of the evaluation.
-- TODO: document
module Game.LambdaHack.Server.ServerSem where

import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.AtomicCmd
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.TileKind as TileKind
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Point
import Game.LambdaHack.Random
import Game.LambdaHack.Server.Action hiding (sendQueryAI, sendQueryUI,
                                      sendUpdateAI, sendUpdateUI)
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.EffectSem
import Game.LambdaHack.Server.State
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

default (Text)

execFailure :: MonadAtomic m => FactionId -> Msg -> m Bool
execFailure fid msg = do
  execSfxAtomic $ FailureD fid msg
  return False

broadcastCmdAtomic :: MonadAtomic m
                   => (FactionId -> CmdAtomic) -> m ()
broadcastCmdAtomic fcmd = do
  faction <- getsState sfaction
  mapM_ (execCmdAtomic . fcmd) $ EM.keys faction

broadcastSfxAtomic :: MonadAtomic m
                   => (FactionId -> SfxAtomic) -> m ()
broadcastSfxAtomic fcmd = do
  faction <- getsState sfaction
  mapM_ (execSfxAtomic . fcmd) $ EM.keys faction

-- * MoveSer

-- | Actor moves or attacks or searches or opens doors.
-- Note that client can't determine which of these actions is chosen,
-- because foes can be invisible, doors hidden, clients can move
-- simultaneously during the same turn, etc. Also, only the server
-- is authorized to check if a move is legal and it needs full context
-- for that, e.g., the initial actor position to check if melee attack
-- does not try to reach to a distant tile.
moveSer :: (MonadAtomic m, MonadServer m) => ActorId -> Vector -> m Bool
moveSer aid dir = do
  cops <- getsState scops
  sm <- getsState $ getActorBody aid
  lvl <- getsLevel (blid sm) id
  let spos = bpos sm           -- source position
      tpos = spos `shift` dir  -- target position
  -- We start by looking at the target position.
  let arena = blid sm
  tgt <- getsState (posToActor tpos arena)
  case tgt of
    Just target -> do
      -- Attacking does not require full access, adjacency is enough.
      actorAttackActor aid target
      return True
    Nothing
      | accessible cops lvl spos tpos -> do
          execCmdAtomic $ MoveActorA aid spos tpos
          addSmell aid
          return True
      | otherwise ->  -- try to open a door, even if not visible
          actorOpenDoor aid dir

-- TODO: let only some actors/items leave smell, e.g., a Smelly Hide Armour.
-- | Add a smell trace for the actor to the level. For, all and only
-- actors from non-spawnig factions leave smell.
addSmell :: MonadAtomic m => ActorId -> m ()
addSmell aid = do
  b <- getsState $ getActorBody aid
  spawning <- getsState $ flip isSpawningFaction (bfaction b)
  when (not spawning) $ do
    time <- getsState $ getLocalTime $ blid b
    oldS <- getsLevel (blid b) $ (EM.lookup $ bpos b) . lsmell
    let newTime = timeAdd time smellTimeout
    execCmdAtomic $ AlterSmellA (blid b) (bpos b) oldS (Just newTime)

-- | Resolves the result of an actor moving into another.
-- Actors on blocked positions can be attacked without any restrictions.
-- For instance, an actor embedded in a wall can be attacked from
-- an adjacent position. This function is analogous to projectGroupItem,
-- but for melee and not using up the weapon.
actorAttackActor :: (MonadAtomic m, MonadServer m)
                 => ActorId -> ActorId -> m ()
actorAttackActor source target = do
  cops@Kind.COps{coitem=Kind.Ops{opick, okind}} <- getsState scops
  sm <- getsState (getActorBody source)
  tm <- getsState (getActorBody target)
  let sfid = bfaction sm
      tfid = bfaction tm
  time <- getsState $ getLocalTime (blid tm)
  s <- getState
  itemAssocs <- getsState $ getActorItem source
  (miid, item) <-
    if bproj sm
    then case itemAssocs of
      [(iid, item)] -> return (Just iid, item)  -- projectile
      _ -> assert `failure` itemAssocs
    else case strongestSword cops itemAssocs of
      Just (_, (iid, w)) -> return (Just iid, w)
      Nothing -> do  -- hand to hand combat
        let h2hGroup | isSpawningFaction s sfid = "monstrous"
                     | otherwise = "unarmed"
        h2hKind <- rndToAction $ opick h2hGroup (const True)
        flavour <- getsServer sflavour
        discoRev <- getsServer sdiscoRev
        let kind = okind h2hKind
            effect = fmap (maxDice . fst) (ieffect kind)
        return $ ( Nothing
                 , buildItem flavour discoRev h2hKind kind effect )
  let performHit block = do
        let hitA = if block then HitBlockD else HitD
        execSfxAtomic $ StrikeD source target item hitA
        -- Deduct a hitpoint for a pierce of a projectile.
        when (bproj sm) $ execCmdAtomic $ HealActorA source (-1)
        -- Msgs inside itemEffectSem describe the target part.
        itemEffect source target miid item
  -- Projectiles can't be blocked (though can be sidestepped).
  if braced tm time && not (bproj sm)
    then do
      blocked <- rndToAction $ chance $ 1%2
      if blocked
        then execSfxAtomic $ StrikeD source target item MissBlockD
        else performHit True
    else performHit False
  sfact <- getsState $ (EM.! sfid) . sfaction
  let fromDipl = EM.findWithDefault Unknown tfid (gdipl sfact)
  unless (isAtWar sfact tfid) $
    execCmdAtomic $ DiplFactionA sfid tfid fromDipl War

-- TODO: bumpTile tpos F.Openable
-- | An actor opens a door.
actorOpenDoor :: (MonadAtomic m, MonadServer m) => ActorId -> Vector -> m Bool
actorOpenDoor actor dir = do
  Kind.COps{cotile} <- getsState scops
  body <- getsState $ getActorBody actor
  lvl <- getsLevel (blid body) id
  let dpos = shift (bpos body) dir  -- the position we act upon
      t = lvl `at` dpos
  if not (openable cotile lvl dpos) then
    execFailure (bfaction body) "never mind"
  else do
    if Tile.hasFeature cotile F.Closable t
      then execFailure (bfaction body) "already open"
      else if not (Tile.hasFeature cotile F.Closable t ||
                   Tile.hasFeature cotile F.Openable t)
           then execFailure (bfaction body) "never mind"  -- not doors at all
           else triggerSer actor dpos

-- * RunSer

-- | Actor moves or swaps position with others or opens doors.
runSer :: (MonadAtomic m, MonadServer m) => ActorId -> Vector -> m Bool
runSer aid dir = do
  cops <- getsState scops
  sm <- getsState $ getActorBody aid
  lvl <- getsLevel (blid sm) id
  let spos = bpos sm           -- source position
      tpos = spos `shift` dir  -- target position
  -- We start by looking at the target position.
  let arena = blid sm
  tgt <- getsState (posToActor tpos arena)
  case tgt of
    Just target
      | accessible cops lvl spos tpos -> do
          -- Switching positions requires full access.
          displaceActor aid target
          return True
      | otherwise ->
          execFailure (bfaction sm) "blocked"
    Nothing
      | accessible cops lvl spos tpos -> do
          execCmdAtomic $ MoveActorA aid spos tpos
          addSmell aid
          return True
      | otherwise ->
          actorOpenDoor aid dir

-- | When an actor runs (not walks) into another, they switch positions.
displaceActor :: MonadAtomic m
              => ActorId -> ActorId -> m ()
displaceActor source target = do
  execCmdAtomic $ DisplaceActorA source target
  addSmell source
--  leader <- getsClient getLeader
--  if Just source == leader
-- TODO: The actor will stop running due to the message as soon as running
-- is fixed to check the message before it goes into history.
--   then stopRunning  -- do not switch positions repeatedly
--   else void $ focusIfOurs target

-- * WaitSer

-- | Update the wait/block count. Uses local, per-level time,
-- to remain correct even if the level is frozen for some global time turns.
waitSer :: MonadAtomic m => ActorId -> m ()
waitSer aid = do
  Kind.COps{coactor} <- getsState scops
  body <- getsState $ getActorBody aid
  time <- getsState $ getLocalTime $ blid body
  let fromWait = bwait body
      toWait = timeAddFromSpeed coactor body time
  execCmdAtomic $ WaitActorA aid fromWait toWait

-- * PickupSer

pickupSer :: MonadAtomic m
          => ActorId -> ItemId -> Int -> InvChar -> m ()
pickupSer aid iid k l = assert (k > 0 `blame` (aid, iid, k, l)) $ do
  b <- getsState $ getActorBody aid
  execCmdAtomic $ MoveItemA iid k (CFloor (blid b) (bpos b)) (CActor aid l)

-- * DropSer

dropSer :: MonadAtomic m => ActorId -> ItemId -> m ()
dropSer aid iid = do
  b <- getsState $ getActorBody aid
  let k = 1
  execCmdAtomic $ MoveItemA iid k (actorContainer aid (binv b) iid)
                                           (CFloor (blid b) (bpos b))

-- * ProjectSer

projectSer :: (MonadAtomic m, MonadServer m)
           => ActorId    -- ^ actor projecting the item (is on current lvl)
           -> Point      -- ^ target position of the projectile
           -> Int        -- ^ digital line parameter
           -> ItemId     -- ^ the item to be projected
           -> Container  -- ^ whether the items comes from floor or inventory
           -> m Bool
projectSer source tpos eps iid container = do
  cops@Kind.COps{coactor} <- getsState scops
  sm <- getsState (getActorBody source)
  Actor{btime} <- getsState $ getActorBody source
  lvl <- getsLevel (blid sm) id
  lxsize <- getsLevel (blid sm) lxsize
  lysize <- getsLevel (blid sm) lysize
  let spos = bpos sm
      arena = blid sm
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
      time = btimeDelta `timeAdd` timeNegate timeClip
      bl = bla lxsize lysize eps spos tpos
  case bl of
    Nothing -> execFailure (bfaction sm) "cannot zap oneself"
    Just [] -> assert `failure` (spos, tpos, "project from the edge of level")
    Just path@(pos:_) -> do
      inhabitants <- getsState (posToActor pos arena)
      if accessible cops lvl spos pos && isNothing inhabitants
        then do
          execSfxAtomic $ ProjectD source iid
          projId <- addProjectile iid pos (blid sm) (bfaction sm) path time
          execCmdAtomic
            $ MoveItemA iid 1 container (CActor projId (InvChar 'a'))
          return True
        else
          execFailure (bfaction sm) "blocked"

-- | Create a projectile actor containing the given missile.
addProjectile :: (MonadAtomic m, MonadServer m)
              => ItemId -> Point -> LevelId -> FactionId -> [Point] -> Time
              -> m ActorId
addProjectile iid loc blid bfaction path btime = do
  Kind.COps{coactor, coitem=coitem@Kind.Ops{okind}} <- getsState scops
  disco <- getsServer sdisco
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
        , bpath   = Just dirPath
        , bpos    = loc
        , blid
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
  execCmdAtomic $ CreateActorA acounter m [(iid, item)]
  return acounter

-- * ApplySer

applySer :: (MonadAtomic m, MonadServer m)
         => ActorId    -- ^ actor applying the item (is on current level)
         -> ItemId     -- ^ the item to be applied
         -> Container  -- ^ the location of the item
         -> m ()
applySer actor iid container = do
  item <- getsState $ getItemBody iid
  execSfxAtomic $ ActivateD actor iid
  itemEffect actor actor (Just iid) item
  execCmdAtomic $ DestroyItemA iid item 1 container

-- * TriggerSer

-- | Perform the action specified for the tile in case it's triggered.
triggerSer :: (MonadAtomic m, MonadServer m)
           => ActorId -> Point -> m Bool
triggerSer aid dpos = do
  Kind.COps{cotile=Kind.Ops{okind, opick}} <- getsState scops
  b <- getsState $ getActorBody aid
  let arena = blid b
  lvl <- getsLevel arena id
  let f feat = do
        case feat of
          F.Cause ef -> do
            execSfxAtomic $ TriggerD aid dpos feat {-TODO-}True
            -- No block against tile, hence @False@.
            void $ effectSem ef aid aid
            return True
          F.ChangeTo tgroup -> do
            execSfxAtomic $ TriggerD aid dpos feat {-TODO-}True
            as <- getsState $ actorList (const True) arena
            if EM.null $ lvl `atI` dpos
              then if unoccupied as dpos
                 then do
                   fromTile <- getsLevel (blid b) (`at` dpos)
                   toTile <- rndToAction $ opick tgroup (const True)
                   execCmdAtomic $ AlterTileA (blid b) dpos fromTile toTile
                   return True
-- TODO: take care of AI using this function (aborts on some of the features, succes on others, etc.).o
                 else execFailure (bfaction b) "blocked"  -- by actors
            else execFailure (bfaction b) "jammed"  -- by items
          _ -> return True
  bs <- mapM f $ TileKind.tfeature $ okind $ lvl `at` dpos
  return $! or bs  -- TODO: stop after first failure, probably

-- * SetPathSer

setPathSer :: (MonadAtomic m, MonadServer m)
           => ActorId -> [Vector] -> m ()
setPathSer aid path = do
  when (length path <= 2) $ do
    fromColor <- getsState $ bcolor . getActorBody aid
    let toColor = Just Color.BrBlack
    when (fromColor /= toColor) $
      execCmdAtomic $ ColorActorA aid fromColor toColor
  fromPath <- getsState $ bpath . getActorBody aid
  case path of
    [] -> execCmdAtomic $ PathActorA aid fromPath (Just [])
    d : lv -> do
      void $ moveSer aid d
      execCmdAtomic $ PathActorA aid fromPath (Just lv)

-- * GameRestart

gameRestartSer :: (MonadAtomic m, MonadServer m) => ActorId -> m ()
gameRestartSer aid = do
  b <- getsState $ getActorBody aid
  let fid = bfaction b
  oldSt <- getsState $ gquit . (EM.! fid) . sfaction
  modifyServer $ \ser -> ser {squit = True}  -- do this at once
  execCmdAtomic $ QuitFactionA fid oldSt $ Just (False, Restart)

-- * GameExit

gameExitSer :: (MonadAtomic m, MonadServer m) => ActorId -> m ()
gameExitSer aid = do
  b <- getsState $ getActorBody aid
  let fid = bfaction b
  oldSt <- getsState $ gquit . (EM.! fid) . sfaction
  modifyServer $ \ser -> ser {squit = True}  -- do this at once
  execCmdAtomic $ QuitFactionA fid oldSt $ Just (True, Camping)

-- * GameSaveSer

gameSaveSer :: MonadServer m => m ()
gameSaveSer = modifyServer $ \ser -> ser {sbkpSave = True}  -- don't rush it

-- * CfgDumpSer

cfgDumpSer :: (MonadAtomic m, MonadServer m) => ActorId -> m ()
cfgDumpSer aid = do
  b <- getsState $ getActorBody aid
  let fid = bfaction b
  Config{configRulesCfgFile} <- getsServer sconfig
  let fn = configRulesCfgFile ++ ".dump"
      msg = "Server dumped current game rules configuration to file"
            <+> T.pack fn <> "."
  dumpCfg fn
  -- Wait with confirmation until saved; tell where the file is.
  execSfxAtomic $ FailureD fid msg -- TODO: it's not a failure
