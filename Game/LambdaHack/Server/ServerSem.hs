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
import Data.Key (mapWithKeyM_)
import Data.Maybe
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Control.Exception.Assert.Sugar
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.AtomicCmd
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.ServerCmd
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.TileKind as TileKind
import Game.LambdaHack.Server.Action hiding (sendQueryAI, sendQueryUI,
                                      sendUpdateAI, sendUpdateUI)
import Game.LambdaHack.Server.EffectSem
import Game.LambdaHack.Server.State

execFailure :: (MonadAtomic m, MonadServer m)
            => Actor -> FailureSer -> m ()
execFailure body failureSer = do
  -- Clients should rarely do that (only in case of invisible actors)
  -- so we report it, send a --more-- meeesage (if not AI), but do not crash
  -- (server should work OK with stupid clients, too).
  let fid = bfid body
      msg = showFailureSer failureSer
  debugPrint $ "execFailure:" <+> showT fid <+> ":" <+> msg
  execSfxAtomic $ MsgFidD fid msg  -- TODO: --more--

broadcastCmdAtomic :: MonadAtomic m
                   => (FactionId -> CmdAtomic) -> m ()
broadcastCmdAtomic fcmd = do
  factionD <- getsState sfactionD
  mapWithKeyM_ (\fid _ -> execCmdAtomic $ fcmd fid) factionD

broadcastSfxAtomic :: MonadAtomic m
                   => (FactionId -> SfxAtomic) -> m ()
broadcastSfxAtomic fcmd = do
  factionD <- getsState sfactionD
  mapWithKeyM_ (\fid _ -> execSfxAtomic $ fcmd fid) factionD

-- * MoveSer

checkAdjacent :: MonadActionRO m => Actor -> Actor -> m Bool
checkAdjacent sb tb = do
  Level{lxsize} <- getLevel $ blid sb
  return $ blid sb == blid tb && adjacent lxsize (bpos sb) (bpos tb)

-- TODO: let only some actors/items leave smell, e.g., a Smelly Hide Armour.
-- | Add a smell trace for the actor to the level. For now, all and only
-- actors from non-spawning factions leave smell.
addSmell :: MonadAtomic m => ActorId -> m ()
addSmell aid = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  b <- getsState $ getActorBody aid
  spawn <- getsState $ isSpawnFaction (bfid b)
  let canSmell = asmell $ okind $ bkind b
  unless (bproj b || spawn || canSmell) $ do
    time <- getsState $ getLocalTime $ blid b
    lvl <- getLevel $ blid b
    let oldS = EM.lookup (bpos b) . lsmell $ lvl
        newTime = timeAdd time smellTimeout
    execCmdAtomic $ AlterSmellA (blid b) (bpos b) oldS (Just newTime)

-- | Actor moves or attacks.
-- Note that client may not be able to see an invisible monster
-- so it's the server that determines if melee took place, etc.
-- Also, only the server is authorized to check if a move is legal
-- and it needs full context for that, e.g., the initial actor position
-- to check if melee attack does not try to reach to a distant tile.
moveSer :: (MonadAtomic m, MonadServer m) => ActorId -> Vector -> m ()
moveSer source dir = do
  cops <- getsState scops
  sb <- getsState $ getActorBody source
  let lid = blid sb
  lvl <- getLevel lid
  let spos = bpos sb           -- source position
      tpos = spos `shift` dir  -- target position
  -- We start by checking actors at the the target position.
  tgt <- getsState $ posToActor tpos lid
  case tgt of
    Just target ->  -- visible or not
      -- Attacking does not require full access, adjacency is enough.
      meleeSer source target
    Nothing
      | accessible cops lvl spos tpos -> do
          -- Movement requires full access.
          execCmdAtomic $ MoveActorA source spos tpos
          addSmell source
      | otherwise ->
          -- Client foolishly tries to move into blocked, boring tile.
          execFailure sb MoveNothing

-- * MeleeSer

-- | Resolves the result of an actor moving into another.
-- Actors on blocked positions can be attacked without any restrictions.
-- For instance, an actor embedded in a wall can be attacked from
-- an adjacent position. This function is analogous to projectGroupItem,
-- but for melee and not using up the weapon.
meleeSer :: (MonadAtomic m, MonadServer m) => ActorId -> ActorId -> m ()
meleeSer source target = do
  cops@Kind.COps{coitem=Kind.Ops{opick, okind}} <- getsState scops
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  adj <- checkAdjacent sb tb
  if not adj then execFailure sb MeleeDistant
  else do
    let sfid = bfid sb
        tfid = bfid tb
    time <- getsState $ getLocalTime (blid tb)
    itemAssocs <- getsState $ getActorItem source
    (miid, item) <-
      if bproj sb   -- projectile
      then case itemAssocs of
        [(iid, item)] -> return (Just iid, item)
        _ -> assert `failure` "projectile with wrong items" `twith` itemAssocs
      else case strongestSword cops itemAssocs of
        Just (_, (iid, w)) -> return (Just iid, w)  -- weapon combat
        Nothing -> do  -- hand to hand combat
          isSp <- getsState $ isSpawnFaction sfid
          let h2hGroup | isSp = "monstrous"
                       | otherwise = "unarmed"
          h2hKind <- rndToAction $ fmap (fromMaybe $ assert `failure` h2hGroup)
                                   $ opick h2hGroup (const True)
          flavour <- getsServer sflavour
          discoRev <- getsServer sdiscoRev
          let kind = okind h2hKind
              effect = fmap maxDeep (ieffect kind)
          return ( Nothing
                 , buildItem flavour discoRev h2hKind kind effect )
    let performHit block = do
          let hitA = if block then HitBlockD else HitD
          execSfxAtomic $ StrikeD source target item hitA
          -- Deduct a hitpoint for a pierce of a projectile.
          when (bproj sb) $ execCmdAtomic $ HealActorA source (-1)
          -- Msgs inside itemEffectSem describe the target part.
          itemEffect source target miid item
    -- Projectiles can't be blocked (though can be sidestepped).
    -- Incapacitated actors can't block.
    if braced tb time && not (bproj sb) && bhp tb > 0
      then do
        blocked <- rndToAction $ chance $ 1%2
        if blocked
          then execSfxAtomic $ StrikeD source target item MissBlockD
          else performHit True
      else performHit False
    sfact <- getsState $ (EM.! sfid) . sfactionD
    -- The only way to start a war is to slap an enemy. Being hit by
    -- and hitting projectiles count as unintentional friendly fire.
    let friendlyFire = bproj sb || bproj tb
        fromDipl = EM.findWithDefault Unknown tfid (gdipl sfact)
    unless (friendlyFire || isAtWar sfact tfid || sfid == tfid) $
      execCmdAtomic $ DiplFactionA sfid tfid fromDipl War

-- * DisplaceSer

-- | Actor tries to swap positions with another.
displaceSer :: (MonadAtomic m, MonadServer m) => ActorId -> ActorId -> m ()
displaceSer source target = do
  cops <- getsState scops
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  adj <- checkAdjacent sb tb
  if not adj then execFailure sb DisplaceDistant
  else do
    let lid = blid sb
    lvl <- getLevel lid
    let spos = bpos sb
        tpos = bpos tb
    if accessible cops lvl spos tpos then do
      -- Displacing requires full access.
      execCmdAtomic $ DisplaceActorA source target
      addSmell source
    else do
      -- Client foolishly tries to displace an actor without access.
      execFailure sb DisplaceAccess

-- * AlterSer

-- | Search and/or alter the tile.
--
-- Note that if @serverTile /= freshClientTile@, @freshClientTile@
-- should not be alterable (but @serverTile@ may be).
alterSer :: (MonadAtomic m, MonadServer m)
         => ActorId -> Point -> Maybe F.Feature -> m ()
alterSer source tpos mfeat = do
  Kind.COps{cotile=cotile@Kind.Ops{okind, opick}} <- getsState scops
  sb <- getsState $ getActorBody source
  let lid = blid sb
      spos = bpos sb
  Level{lxsize} <- getLevel lid
  if not $ adjacent lxsize spos tpos then execFailure sb AlterDistant
  else do
    lvl <- getLevel lid
    let serverTile = lvl `at` tpos
        freshClientTile = hideTile cotile lvl tpos
        changeTo tgroup = do
          -- No AlterD, because the effect is obvious (e.g., opened door).
          toTile <- rndToAction $ fmap (fromMaybe $ assert `failure` tgroup)
                                  $ opick tgroup (const True)
          unless (toTile == serverTile) $
            execCmdAtomic $ AlterTileA lid tpos serverTile toTile
        feats = case mfeat of
          Nothing -> TileKind.tfeature $ okind serverTile
          Just feat2 | Tile.hasFeature cotile feat2 serverTile -> [feat2]
          Just _ -> []
        toAlter feat =
          case feat of
            F.OpenTo tgroup -> Just tgroup
            F.CloseTo tgroup -> Just tgroup
            F.ChangeTo tgroup -> Just tgroup
            _ -> Nothing
        groupsToAlter = mapMaybe toAlter feats
    as <- getsState $ actorList (const True) lid
    if null groupsToAlter && serverTile == freshClientTile then
      -- Neither searching nor altering possible; silly client.
      execFailure sb AlterNothing
    else do
      if EM.null $ lvl `atI` tpos then
        if unoccupied as tpos then do
          when (serverTile /= freshClientTile) $ do
            -- Search, in case some actors (of other factions?)
            -- don't know this tile.
            execCmdAtomic $ SearchTileA source tpos freshClientTile serverTile
          mapM_ changeTo groupsToAlter
          -- Perform an effect, if any permitted.
          void $ triggerEffect source feats
        else execFailure sb AlterBlockActor
      else execFailure sb AlterBlockItem

-- * WaitSer

-- | Update the wait/block count. Uses local, per-level time,
-- to remain correct even if the level is frozen for some global time turns.
waitSer :: MonadAtomic m => ActorId -> m ()
waitSer aid = do
  body <- getsState $ getActorBody aid
  time <- getsState $ getLocalTime $ blid body
  let fromWait = bwait body
      toWait = timeAddFromSpeed body time
  execCmdAtomic $ WaitActorA aid fromWait toWait

-- * PickupSer

pickupSer :: MonadAtomic m
          => ActorId -> ItemId -> Int -> InvChar -> m ()
pickupSer aid iid k l = assert (k > 0 `blame` "pick up no items"
                                      `twith` (aid, iid, k, l)) $ do
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
           -> m ()
projectSer source tpos eps iid container = do
  Kind.COps{cotile} <- getsState scops
  sb <- getsState $ getActorBody source
  let lid = blid sb
      spos = bpos sb
  fact <- getsState $ (EM.! bfid sb) . sfactionD
  Level{lxsize, lysize} <- getLevel lid
  foes <- getsState $ actorNotProjList (isAtWar fact) lid
  if foesAdjacent lxsize lysize spos foes
    then execFailure sb ProjectBlockFoes
    else do
      case bla lxsize lysize eps spos tpos of
        Nothing -> execFailure sb ProjectAimOnself
        Just [] -> assert `failure` "projecting from the edge of level"
                          `twith` (spos, tpos)
        Just (pos : rest) -> do
          as <- getsState $ actorList (const True) lid
          lvl <- getLevel lid
          let t = lvl `at` pos
          if not $ Tile.hasFeature cotile F.Clear t
            then execFailure sb ProjectBlockTerrain
            else if unoccupied as pos
                 then projectBla source pos rest iid container
                 else execFailure sb ProjectBlockActor

projectBla :: (MonadAtomic m, MonadServer m)
           => ActorId    -- ^ actor projecting the item (is on current lvl)
           -> Point      -- ^ starting point of the projectile
           -> [Point]    -- ^ rest of the path of the projectile
           -> ItemId     -- ^ the item to be projected
           -> Container  -- ^ whether the items comes from floor or inventory
           -> m ()
projectBla source pos rest iid container = do
  sb <- getsState $ getActorBody source
  let lid = blid sb
      -- A bit later than actor time, to prevent a move this turn.
      time = btime sb `timeAdd` timeEpsilon
  execSfxAtomic $ ProjectD source iid
  projId <- addProjectile pos rest iid lid (bfid sb) time
  execCmdAtomic $ MoveItemA iid 1 container (CActor projId (InvChar 'a'))

-- | Create a projectile actor containing the given missile.
addProjectile :: (MonadAtomic m, MonadServer m)
              => Point -> [Point] -> ItemId -> LevelId -> FactionId -> Time
              -> m ActorId
addProjectile bpos rest iid blid bfid btime = do
  Kind.COps{ coactor=coactor@Kind.Ops{okind}
           , coitem=coitem@Kind.Ops{okind=iokind} } <- getsState scops
  disco <- getsServer sdisco
  item <- getsState $ getItemBody iid
  let ik = iokind (fromJust $ jkind disco item)
      speed = speedFromWeight (iweight ik) (itoThrow ik)
      range = rangeFromSpeed speed
      adj | range < 5 = "falling"
          | otherwise = "flying"
      -- Not much details about a fast flying object.
      (object1, object2) = partItem coitem EM.empty item
      name = makePhrase [MU.AW $ MU.Text adj, object1, object2]
      dirPath = take range $ displacePath (bpos : rest)
      kind = okind $ projectileKindId coactor
      m = actorTemplate (projectileKindId coactor) (asymbol kind) name
                        (acolor kind) speed 0 (Just dirPath)
                        bpos blid btime bfid True
  acounter <- getsServer sacounter
  modifyServer $ \ser -> ser {sacounter = succ acounter}
  execCmdAtomic $ CreateActorA acounter m [(iid, item)]
  return acounter

-- * ApplySer

-- TODO: check actor has access to the item
applySer :: (MonadAtomic m, MonadServer m)
         => ActorId    -- ^ actor applying the item (is on current level)
         -> ItemId     -- ^ the item to be applied
         -> Container  -- ^ the location of the item
         -> m ()
applySer actor iid container = do
  item <- getsState $ getItemBody iid
  execSfxAtomic $ ActivateD actor iid
  itemEffect actor actor (Just iid) item
  -- TODO: don't destroy if not really used up; also, don't take time?
  execCmdAtomic $ DestroyItemA iid item 1 container

-- * TriggerSer

-- | Perform the effect specified for the tile in case it's triggered.
triggerSer :: (MonadAtomic m, MonadServer m)
           => ActorId -> Maybe F.Feature -> m ()
triggerSer aid mfeat = do
  Kind.COps{cotile=cotile@Kind.Ops{okind}} <- getsState scops
  sb <- getsState $ getActorBody aid
  let lid = blid sb
  lvl <- getLevel lid
  let tpos = bpos sb
      serverTile = lvl `at` tpos
      feats = case mfeat of
        Nothing -> TileKind.tfeature $ okind serverTile
        Just feat2 | Tile.hasFeature cotile feat2 serverTile -> [feat2]
        Just _ -> []
  go <- triggerEffect aid feats
  unless go $ execFailure sb TriggerNothing

triggerEffect :: (MonadAtomic m, MonadServer m)
              => ActorId -> [F.Feature] -> m Bool
triggerEffect aid feats = do
  sb <- getsState $ getActorBody aid
  let tpos = bpos sb
      triggerFeat feat =
        case feat of
          F.Cause ef -> do
            -- No block against tile, hence unconditional.
            execSfxAtomic $ TriggerD aid tpos feat
            void $ effectSem ef aid aid
            return True
          _ -> return False
  goes <- mapM triggerFeat feats
  return $! or goes

-- * SetPathSer

setPathSer :: (MonadAtomic m, MonadServer m)
           => ActorId -> [Vector] -> m ()
setPathSer aid path = do
  when (length path <= 2) $ do
    fromColor <- getsState $ bcolor . getActorBody aid
    let toColor = Color.BrBlack
    when (fromColor /= toColor) $
      execCmdAtomic $ ColorActorA aid fromColor toColor
  fromPath <- getsState $ bpath . getActorBody aid
  case path of
    [] -> execCmdAtomic $ PathActorA aid fromPath (Just [])
    d : lv -> do
      void $ moveSer aid d
      execCmdAtomic $ PathActorA aid fromPath (Just lv)

-- * GameRestart

gameRestartSer :: (MonadAtomic m, MonadServer m) => ActorId -> Text -> m ()
gameRestartSer aid stInfo = do
  b <- getsState $ getActorBody aid
  let fid = bfid b
  oldSt <- getsState $ gquit . (EM.! fid) . sfactionD
  modifyServer $ \ser -> ser {squit = True}  -- do this at once
  revealItems Nothing Nothing
  execCmdAtomic $ QuitFactionA fid (Just b) oldSt
                $ Just $ Status Restart (fromEnum $ blid b) stInfo

-- * GameExit

gameExitSer :: (MonadAtomic m, MonadServer m) => ActorId -> m ()
gameExitSer aid = do
  b <- getsState $ getActorBody aid
  let fid = bfid b
  oldSt <- getsState $ gquit . (EM.! fid) . sfactionD
  modifyServer $ \ser -> ser {squit = True}  -- do this at once
  execCmdAtomic $ QuitFactionA fid (Just b) oldSt
                $ Just $ Status Camping (fromEnum $ blid b) ""

-- * GameSaveSer

gameSaveSer :: MonadServer m => m ()
gameSaveSer = do
  modifyServer $ \ser -> ser {sbkpSave = True}
  modifyServer $ \ser -> ser {squit = True}  -- do this at once

-- * CfgDumpSer

cfgDumpSer :: (MonadAtomic m, MonadServer m) => ActorId -> m ()
cfgDumpSer aid = do
  fn <- dumpCfg
  b <- getsState $ getActorBody aid
  let fid = bfid b
      msg = "Server dumped current game rules configuration to file"
            <+> T.pack fn <> "."
  -- Wait with confirmation until saved; tell where the file is.
  execSfxAtomic $ MsgFidD fid msg
