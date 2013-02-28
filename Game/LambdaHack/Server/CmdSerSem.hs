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
module Game.LambdaHack.Server.CmdSerSem where

import Control.Monad
import Control.Monad.Writer.Strict (WriterT)
import qualified Data.EnumMap.Strict as EM
import Data.List
import Data.Maybe
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Action hiding (abortWith, neverMind)
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.CmdAtomic
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
import Game.LambdaHack.Server.Action
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.EffectSem
import Game.LambdaHack.Server.State
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

default (Text)

abortWith :: Monad m => FactionId -> Msg -> WriterT [Atomic] m ()
abortWith fid msg = tellDescAtomic $ FailureD fid msg

neverMind :: Monad m => FactionId -> WriterT [Atomic] m ()
neverMind fid = abortWith fid "never mind"

-- * MoveSer

-- | Actor moves or attacks or searches or opens doors.
-- Note that client can't determine which of these actions is chosen,
-- because foes can be invisible, doors hidden, clients can move
-- simultaneously during the same turn, etc. Also, only the server
-- is authorized to check if a move is legal and it needs full context
-- for that, e.g., the initial actor position to check if melee attack
-- does not try to reach to a distant tile.
moveSer :: MonadServer m => ActorId -> Vector -> WriterT [Atomic] m ()
moveSer aid dir = do
  cops@Kind.COps{cotile = cotile@Kind.Ops{okind}} <- getsState scops
  sm <- getsState $ getActorBody aid
  lvl <- getsLevel (blid sm) id
  let spos = bpos sm           -- source position
      tpos = spos `shift` dir  -- target position
  -- We start by looking at the target position.
  let arena = blid sm
  tgt <- getsState (posToActor tpos arena)
  case tgt of
    Just target ->
      -- Attacking does not require full access, adjacency is enough.
      actorAttackActor aid target
    Nothing
      | accessible cops lvl spos tpos ->
          tellCmdAtomic $ MoveActorA aid spos tpos
      | Tile.canBeHidden cotile (okind $ lvl `at` tpos) ->
          search aid
      | otherwise ->
          actorOpenDoor aid dir

-- | Resolves the result of an actor moving into another.
-- Actors on blocked positions can be attacked without any restrictions.
-- For instance, an actor embedded in a wall can be attacked from
-- an adjacent position. This function is analogous to projectGroupItem,
-- but for melee and not using up the weapon.
actorAttackActor :: MonadServer m
                 => ActorId -> ActorId -> WriterT [Atomic] m ()
actorAttackActor source target = do
  cops@Kind.COps{coitem=Kind.Ops{opick, okind}} <- getsState scops
  sm <- getsState (getActorBody source)
  tm <- getsState (getActorBody target)
  time <- getsState $ getTime (blid tm)
  s <- getState
  when (bfaction sm == bfaction tm && isHumanFaction s (bfaction sm)
        && not (bproj sm) && not (bproj tm))
    $ assert `failure` (source, target, "human AI bumps into friendlies")
  itemAssocs <- getsState $ getActorItem source
  (miid, item) <-
    if bproj sm
    then case itemAssocs of
      [(iid, item)] -> return (Just iid, item)  -- projectile
      _ -> assert `failure` itemAssocs
    else case strongestSword cops itemAssocs of
      Just (iid, w) -> return (Just iid, w)
      Nothing -> do  -- hand to hand combat
        let h2hGroup | isSpawningFaction s (bfaction sm) = "monstrous"
                     | otherwise = "unarmed"
        h2hKind <- rndToAction $ opick h2hGroup (const True)
        flavour <- getsServer sflavour
        discoRev <- getsServer sdiscoRev
        return $ ( Nothing
                 , buildItem flavour discoRev h2hKind (okind h2hKind) 0 )
  let performHit block = do
        let hitA = if block then HitBlockD else HitD
        -- Msgs inside itemEffectSem describe the target part.
        itemEffect source target miid item
        -- Order reversed to anticipate death in the strike message.
        -- Note: this means actors should not be destroyed in @itemEffect@.
        tellDescAtomic $ StrikeD source target item hitA
        -- Deduct a hitpoint for a pierce of a projectile.
        when (bproj sm) $ tellCmdAtomic $ HealActorA source (-1)
  -- Projectiles can't be blocked (though can be sidestepped).
  if braced tm time && not (bproj sm)
    then do
      blocked <- rndToAction $ chance $ 1%2
      if blocked
        then tellDescAtomic $ StrikeD source target item MissBlockD
        else performHit True
    else performHit False

-- | Search for hidden doors.
search :: MonadServer m => ActorId -> WriterT [Atomic] m ()
search aid = do
  Kind.COps{coitem, cotile} <- getsState scops
  b <- getsState $ getActorBody aid
  lvl <- getsLevel (blid b) id
  lsecret <- getsLevel (blid b) lsecret
  lxsize <- getsLevel (blid b) lxsize
  itemAssocs <- getsState (getActorItem aid)
  discoS <- getsServer sdisco
  let delta = timeScale timeTurn $
                case strongestSearch coitem discoS itemAssocs of
                  Just (_, i)  -> 1 + jpower i
                  Nothing -> 1
      searchTile diffL mv =
        let loc = shift (bpos b) mv
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
  tellCmdAtomic $ AlterSecretA (blid b) diffL
  let triggerHidden (_, (_, Just _)) = return ()
      triggerHidden (dpos, (_, Nothing)) = triggerSer aid dpos
  mapM_ triggerHidden diffL

-- TODO: bumpTile tpos F.Openable
-- | An actor opens a door.
actorOpenDoor :: MonadServer m => ActorId -> Vector -> WriterT [Atomic] m ()
actorOpenDoor actor dir = do
  Kind.COps{cotile} <- getsState scops
  body <- getsState $ getActorBody actor
  lvl <- getsLevel (blid body) id
  let dpos = shift (bpos body) dir  -- the position we act upon
      t = lvl `at` dpos
  if not (openable cotile lvl dpos) then neverMind (bfaction body)
  else do
    if Tile.hasFeature cotile F.Closable t
      then abortWith (bfaction body) "already open"
      else if not (Tile.hasFeature cotile F.Closable t ||
                   Tile.hasFeature cotile F.Openable t ||
                   Tile.hasFeature cotile F.Hidden t)
           then neverMind (bfaction body)  -- not doors at all
           else triggerSer actor dpos

-- * RunSer

-- | Actor moves or swaps position with others or opens doors.
runSer :: MonadServer m => ActorId -> Vector -> WriterT [Atomic] m ()
runSer actor dir = do
  cops <- getsState scops
  sm <- getsState $ getActorBody actor
  lvl <- getsLevel (blid sm) id
  let spos = bpos sm           -- source position
      tpos = spos `shift` dir  -- target position
  -- We start by looking at the target position.
  let arena = blid sm
  tgt <- getsState (posToActor tpos arena)
  case tgt of
    Just target
      | accessible cops lvl spos tpos ->
          -- Switching positions requires full access.
          displaceActor actor target
      | otherwise -> abortWith (bfaction sm) "blocked"
    Nothing
      | accessible cops lvl spos tpos ->
          tellCmdAtomic $ MoveActorA actor spos tpos
      | otherwise ->
          actorOpenDoor actor dir

-- | When an actor runs (not walks) into another, they switch positions.
displaceActor :: MonadActionRO m
              => ActorId -> ActorId -> WriterT [Atomic] m ()
displaceActor source target = tellCmdAtomic $ DisplaceActorA source target
--  leader <- getsClient getLeader
--  if Just source == leader
-- TODO: The actor will stop running due to the message as soon as running
-- is fixed to check the message before it goes into history.
--   then stopRunning  -- do not switch positions repeatedly
--   else void $ focusIfOurs target

-- * WaitSer

-- | Update the wait/block count.
waitSer :: MonadActionRO m => ActorId -> WriterT [Atomic] m ()
waitSer aid = do
  Kind.COps{coactor} <- getsState scops
  body <- getsState $ getActorBody aid
  time <- getsState $ getTime $ blid body
  let fromWait = bwait body
      toWait = timeAddFromSpeed coactor body time
  tellCmdAtomic $ WaitActorA aid fromWait toWait

-- * PickupSer

pickupSer :: MonadActionRO m
          => ActorId -> ItemId -> Int -> InvChar -> WriterT [Atomic] m ()
pickupSer aid iid k l = assert (k > 0 `blame` (aid, iid, k, l)) $ do
  b <- getsState $ getActorBody aid
  tellCmdAtomic $ MoveItemA iid k (CFloor (blid b) (bpos b)) (CActor aid l)

-- * DropSer

dropSer :: MonadActionRO m => ActorId -> ItemId -> WriterT [Atomic] m ()
dropSer aid iid = do
  b <- getsState $ getActorBody aid
  let k = 1
  tellCmdAtomic $ MoveItemA iid k (actorContainer aid (binv b) iid)
                                           (CFloor (blid b) (bpos b))

-- * ProjectSer

projectSer :: MonadServer m
           => ActorId    -- ^ actor projecting the item (is on current lvl)
           -> Point      -- ^ target position of the projectile
           -> Int        -- ^ digital line parameter
           -> ItemId     -- ^ the item to be projected
           -> Container  -- ^ whether the items comes from floor or inventory
           -> WriterT [Atomic] m ()
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
    Nothing -> abortWith (bfaction sm) "cannot zap oneself"
    Just [] -> assert `failure` (spos, tpos, "project from the edge of level")
    Just path@(pos:_) -> do
      inhabitants <- getsState (posToActor pos arena)
      if accessible cops lvl spos pos && isNothing inhabitants
        then do
          tellDescAtomic $ ProjectD source iid
          projId <- addProjectile iid pos (blid sm) (bfaction sm) path time
          tellCmdAtomic
            $ MoveItemA iid 1 container (CActor projId (InvChar 'a'))
        else
          abortWith (bfaction sm) "blocked"

-- | Create a projectile actor containing the given missile.
addProjectile :: MonadServer m
              => ItemId -> Point -> LevelId -> FactionId -> [Point] -> Time
              -> WriterT [Atomic] m ActorId
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
  tellCmdAtomic $ CreateActorA acounter m
  return acounter

-- * ApplySer

applySer :: MonadServer m
         => ActorId    -- ^ actor applying the item (is on current level)
         -> ItemId     -- ^ the item to be applied
         -> Container  -- ^ the location of the item
         -> WriterT [Atomic] m ()
applySer actor iid container = do
  item <- getsState $ getItemBody iid
  tellDescAtomic $ ActivateD actor iid
  itemEffect actor actor (Just iid) item
  tellCmdAtomic $ DestroyItemA iid item 1 container

-- * TriggerSer

-- | Perform the action specified for the tile in case it's triggered.
triggerSer :: MonadServer m
           => ActorId -> Point -> WriterT [Atomic] m ()
triggerSer aid dpos = do
  Kind.COps{cotile=Kind.Ops{okind, opick}} <- getsState scops
  b <- getsState $ getActorBody aid
  let arena = blid b
  lvl <- getsLevel arena id
  let f feat = do
        case feat of
          F.Cause ef -> do
            tellDescAtomic $ TriggerD aid dpos feat {-TODO-}True
            -- No block against tile, hence @False@.
            void $ effectSem ef aid aid 0
          F.ChangeTo tgroup -> do
            tellDescAtomic $ TriggerD aid dpos feat {-TODO-}True
            as <- getsState $ actorList (const True) arena
            if EM.null $ lvl `atI` dpos
              then if unoccupied as dpos
                 then do
                   fromTile <- getsLevel (blid b) (`at` dpos)
                   toTile <- rndToAction $ opick tgroup (const True)
                   tellCmdAtomic $ AlterTileA (blid b) dpos fromTile toTile
-- TODO: take care of AI using this function (aborts, etc.).
                 else abortWith (bfaction b) "blocked"  -- by actors
            else abortWith (bfaction b) "jammed"  -- by items
          _ -> return ()
  mapM_ f $ TileKind.tfeature $ okind $ lvl `at` dpos

-- * SetPathSer

setPathSer :: MonadServer m
           => ActorId -> [Vector] -> WriterT [Atomic] m ()
setPathSer aid path = do
  when (length path <= 2) $ do
    fromColor <- getsState $ bcolor . getActorBody aid
    let toColor = Just Color.BrBlack
    when (fromColor /= toColor) $
      tellCmdAtomic $ ColorActorA aid fromColor toColor
  fromPath <- getsState $ bpath . getActorBody aid
  case path of
    [] -> tellCmdAtomic $ PathActorA aid fromPath (Just [])
    d : lv -> do
      moveSer aid d
      tellCmdAtomic $ PathActorA aid fromPath (Just lv)

-- * GameRestart

gameRestartSer :: MonadActionRO m => ActorId -> WriterT [Atomic] m ()
gameRestartSer aid = do
  b <- getsState $ getActorBody aid
  let fid = bfaction b
  oldSt <- getsState $ gquit . (EM.! fid) . sfaction
  tellCmdAtomic $ QuitFactionA fid oldSt $ Just (False, Restart)

-- * GameExit

gameExitSer :: MonadServer m => m ()
gameExitSer = modifyServer $ \ser -> ser {squit = Just True}

-- * GameSaveSer

gameSaveSer :: MonadServer m => m ()
gameSaveSer = modifyServer $ \ser -> ser {squit = Just False}

-- * CfgDumpSer

cfgDumpSer :: MonadServer m => ActorId -> WriterT [Atomic] m ()
cfgDumpSer aid = do
  b <- getsState $ getActorBody aid
  let fid = bfaction b
  Config{configRulesCfgFile} <- getsServer sconfig
  let fn = configRulesCfgFile ++ ".dump"
      msg = "Server dumped current game rules configuration to file"
            <+> T.pack fn <> "."
  dumpCfg fn
  -- Wait with confirmation until saved; tell where the file is.
  -- TODO: show abort message to the current client, not all clients
  abortWith fid msg
