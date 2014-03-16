-- | Semantics of request.
-- A couple of them do not take time, the rest does.
-- Note that since the results are atomic commands, which are executed
-- only later (on the server and some of the clients), all condition
-- are checkd by the semantic functions in the context of the state
-- before the server command. Even if one or more atomic actions
-- are already issued by the point an expression is evaluated, they do not
-- influence the outcome of the evaluation.
-- TODO: document
module Game.LambdaHack.Server.HandleRequestServer
  ( handleRequest, handleRequestTimed
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import Data.Ratio
import Data.Text (Text)

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.TileKind as TileKind
import Game.LambdaHack.Server.CommonServer
import Game.LambdaHack.Server.HandleEffectServer
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.State

-- | The semantics of server commands. The resulting boolean value
-- indicates if the command took some time.
handleRequest :: (MonadAtomic m, MonadServer m) => Request -> m Bool
handleRequest cmd = case cmd of
  ReqTimed cmd2 -> handleRequestTimed cmd2 >> return True
  ReqGameRestart aid t d names -> reqGameRestart aid t d names >> return False
  ReqGameExit aid d -> reqGameExit aid d >> return False
  ReqGameSave _ -> reqGameSave >> return False
  ReqAutomate aid -> reqAutomate aid >> return False

handleRequestTimed :: (MonadAtomic m, MonadServer m) => RequestTimed -> m ()
handleRequestTimed cmd = case cmd of
  ReqMove source target -> reqMove source target
  ReqMelee source target -> reqMelee source target
  ReqDisplace source target -> reqDisplace source target
  ReqAlter source tpos mfeat -> reqAlter source tpos mfeat
  ReqWait aid -> reqWait aid
  ReqMoveItem aid iid k fromCStore toCStore ->
    reqMoveItem aid iid k fromCStore toCStore
  ReqProject aid p eps iid cstore -> reqProject aid p eps iid cstore
  ReqApply aid iid cstore -> reqApply aid iid cstore
  ReqTrigger aid mfeat -> reqTrigger aid mfeat
  ReqSetTrajectory aid -> reqSetTrajectory aid
  ReqPongHack _ -> return ()

-- * ReqMove

-- TODO: let only some actors/items leave smell, e.g., a Smelly Hide Armour.
-- | Add a smell trace for the actor to the level. For now, only heroes
-- leave smell.
addSmell :: MonadAtomic m => ActorId -> m ()
addSmell aid = do
  cops@Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  let canSmell = asmell $ okind $ bkind b
  unless (bproj b || not (isHeroFact cops fact) || canSmell) $ do
    time <- getsState $ getLocalTime $ blid b
    lvl <- getLevel $ blid b
    let oldS = EM.lookup (bpos b) . lsmell $ lvl
        newTime = timeAdd time smellTimeout
    execUpdAtomic $ UpdAlterSmell (blid b) (bpos b) oldS (Just newTime)

-- | Actor moves or attacks.
-- Note that client may not be able to see an invisible monster
-- so it's the server that determines if melee took place, etc.
-- Also, only the server is authorized to check if a move is legal
-- and it needs full context for that, e.g., the initial actor position
-- to check if melee attack does not try to reach to a distant tile.
reqMove :: (MonadAtomic m, MonadServer m) => ActorId -> Vector -> m ()
reqMove source dir = do
  cops <- getsState scops
  sb <- getsState $ getActorBody source
  let lid = blid sb
  lvl <- getLevel lid
  let spos = bpos sb           -- source position
      tpos = spos `shift` dir  -- target position
  -- We start by checking actors at the the target position.
  tgt <- getsState $ posToActor tpos lid
  case tgt of
    Just ((target, tb), _) | not (bproj sb && bproj tb) ->  -- visible or not
      -- Attacking does not require full access, adjacency is enough.
      -- Projectiles are too small to hit each other.
      reqMelee source target
    _
      | accessible cops lvl spos tpos -> do
          -- Movement requires full access.
          execUpdAtomic $ UpdMoveActor source spos tpos
          addSmell source
      | otherwise ->
          -- Client foolishly tries to move into blocked, boring tile.
          execFailure source MoveNothing

-- * ReqMelee

-- | Resolves the result of an actor moving into another.
-- Actors on blocked positions can be attacked without any restrictions.
-- For instance, an actor embedded in a wall can be attacked from
-- an adjacent position. This function is analogous to projectGroupItem,
-- but for melee and not using up the weapon.
-- No problem if there are many projectiles at the spot. We just
-- attack the one specified.
reqMelee :: (MonadAtomic m, MonadServer m) => ActorId -> ActorId -> m ()
reqMelee source target = do
  cops@Kind.COps{coitem=coitem@Kind.Ops{opick, okind}} <- getsState scops
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  let adj = checkAdjacent sb tb
  if source == target then execFailure source MeleeSelf
  else if not adj then execFailure source MeleeDistant
  else do
    let sfid = bfid sb
        tfid = bfid tb
    sfact <- getsState $ (EM.! sfid) . sfactionD
    eqpAssocs <- getsState $ getEqpAssocs sb
    ais <- getsState $ getCarriedAssocs sb
    (miid, item) <-
      if bproj sb   -- projectile
      then case ais of
        [(iid, item)] -> return (Just iid, item)
        _ -> assert `failure` "projectile with wrong items" `twith` ais
      else case strongestSword cops eqpAssocs of
        Just (_, (iid, w)) -> return (Just iid, w)  -- weapon combat
        Nothing -> do  -- hand to hand combat
          let isHero = isHeroFact cops sfact
              h2hGroup | isHero = "unarmed"
                       | otherwise = "monstrous"
          h2hKind <- rndToAction $ fmap (fromMaybe $ assert `failure` h2hGroup)
                                   $ opick h2hGroup (const True)
          flavour <- getsServer sflavour
          discoRev <- getsServer sdiscoRev
          let kind = okind h2hKind
          let kindEffect = case causeIEffects coitem h2hKind of
                [] -> NoEffect
                eff : _TODO -> eff
              effect = fmap maxDeep kindEffect
          return ( Nothing
                 , buildItem flavour discoRev h2hKind kind effect )
    let performHit block = do
          let hitA = if block then HitBlock else Hit
          execSfxAtomic $ SfxStrike source target item hitA
          -- Deduct a hitpoint for a pierce of a projectile.
          when (bproj sb) $ execUpdAtomic $ UpdHealActor source (-1)
          -- Msgs inside itemEffect describe the target part.
          itemEffect source target miid item
    -- Projectiles can't be blocked (though can be sidestepped).
    -- Incapacitated actors can't block.
    if braced tb && not (bproj sb) && bhp tb > 0
      then do
        blocked <- rndToAction $ chance $ 1%2
        if blocked
          then execSfxAtomic $ SfxStrike source target item MissBlock
          else performHit True
      else performHit False
    -- The only way to start a war is to slap an enemy. Being hit by
    -- and hitting projectiles count as unintentional friendly fire.
    let friendlyFire = bproj sb || bproj tb
        fromDipl = EM.findWithDefault Unknown tfid (gdipl sfact)
    unless (friendlyFire || isAtWar sfact tfid || sfid == tfid) $
      execUpdAtomic $ UpdDiplFaction sfid tfid fromDipl War

-- * ReqDisplace

-- | Actor tries to swap positions with another.
reqDisplace :: (MonadAtomic m, MonadServer m) => ActorId -> ActorId -> m ()
reqDisplace source target = do
  cops <- getsState scops
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  let adj = checkAdjacent sb tb
  if not adj then execFailure source DisplaceDistant
  else do
    let lid = blid sb
    lvl <- getLevel lid
    let spos = bpos sb
        tpos = bpos tb
    -- Displacing requires full access.
    if accessible cops lvl spos tpos then do
      tgts <- getsState $ posToActors tpos lid
      case tgts of
        [] -> assert `failure` (source, sb, target, tb)
        [_] -> do
          execUpdAtomic $ UpdDisplaceActor source target
          addSmell source
          addSmell target
        _ -> execFailure source DisplaceProjectiles
    else do
      -- Client foolishly tries to displace an actor without access.
      execFailure source DisplaceAccess

-- * ReqAlter

-- | Search and/or alter the tile.
--
-- Note that if @serverTile /= freshClientTile@, @freshClientTile@
-- should not be alterable (but @serverTile@ may be).
reqAlter :: (MonadAtomic m, MonadServer m)
         => ActorId -> Point -> Maybe F.Feature -> m ()
reqAlter source tpos mfeat = do
  Kind.COps{cotile=cotile@Kind.Ops{okind, opick}} <- getsState scops
  sb <- getsState $ getActorBody source
  let lid = blid sb
      spos = bpos sb
  if not $ adjacent spos tpos then execFailure source AlterDistant
  else do
    lvl <- getLevel lid
    let serverTile = lvl `at` tpos
        freshClientTile = hideTile cotile lvl tpos
        changeTo tgroup = do
          -- No AlterD, because the effect is obvious (e.g., opened door).
          toTile <- rndToAction $ fmap (fromMaybe $ assert `failure` tgroup)
                                  $ opick tgroup (const True)
          unless (toTile == serverTile) $
            execUpdAtomic $ UpdAlterTile lid tpos serverTile toTile
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
      execFailure source AlterNothing
    else do
      if EM.null $ lvl `atI` tpos then
        if unoccupied as tpos then do
          when (serverTile /= freshClientTile) $ do
            -- Search, in case some actors (of other factions?)
            -- don't know this tile.
            execUpdAtomic $ UpdSearchTile source tpos freshClientTile serverTile
          mapM_ changeTo groupsToAlter
          -- Perform an effect, if any permitted.
          void $ triggerEffect source feats
        else execFailure source AlterBlockActor
      else execFailure source AlterBlockItem

-- * ReqWait

-- | Do nothing.
--
-- Something is sometimes done in 'LoopAction.setBWait'.
reqWait :: MonadAtomic m => ActorId -> m ()
reqWait _ = return ()

-- * ReqMoveItem

reqMoveItem :: (MonadAtomic m, MonadServer m)
            => ActorId -> ItemId -> Int -> CStore -> CStore -> m ()
reqMoveItem aid iid k fromCStore toCStore = do
  let moveItem = do
        cs <- actorConts iid k aid fromCStore
        mapM_ (\(ck, c) -> execUpdAtomic
                           $ UpdMoveItem iid ck c (CActor aid toCStore)) cs
  if k < 1 || fromCStore == toCStore then execFailure aid ItemNothing
  else if fromCStore /= CInv && toCStore /= CInv then moveItem
  else do
    Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
    b <- getsState $ getActorBody aid
    let kind = okind $ bkind b
    if calmEnough b kind then moveItem
    else execFailure aid ItemNotCalm

-- * ReqProject

reqProject :: (MonadAtomic m, MonadServer m)
           => ActorId    -- ^ actor projecting the item (is on current lvl)
           -> Point      -- ^ target position of the projectile
           -> Int        -- ^ digital line parameter
           -> ItemId     -- ^ the item to be projected
           -> CStore     -- ^ whether the items comes from floor or inventory
           -> m ()
reqProject source tpxy eps iid cstore = do
  mfail <- projectFail source tpxy eps iid cstore False
  maybe skip (execFailure source) mfail

-- * ReqApply

-- TODO: check actor has access to the item
reqApply :: (MonadAtomic m, MonadServer m)
         => ActorId  -- ^ actor applying the item (is on current level)
         -> ItemId   -- ^ the item to be applied
         -> CStore   -- ^ the location of the item
         -> m ()
reqApply aid iid cstore = do
  let applyItem = do
        item <- getsState $ getItemBody iid
        execSfxAtomic $ SfxActivate aid iid
        itemEffect aid aid (Just iid) item
        -- TODO: don't destroy if not really used up; also, don't take time?
        cs <- actorConts iid 1 aid cstore
        mapM_ (\(_, c) -> execUpdAtomic $ UpdDestroyItem iid item 1 c) cs
  if cstore /= CInv then applyItem
  else do
    Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
    b <- getsState $ getActorBody aid
    let kind = okind $ bkind b
    if calmEnough b kind then applyItem
    else execFailure aid ItemNotCalm

-- * ReqTrigger

-- | Perform the effect specified for the tile in case it's triggered.
reqTrigger :: (MonadAtomic m, MonadServer m)
           => ActorId -> Maybe F.Feature -> m ()
reqTrigger aid mfeat = do
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
  unless go $ execFailure aid TriggerNothing

triggerEffect :: (MonadAtomic m, MonadServer m)
              => ActorId -> [F.Feature] -> m Bool
triggerEffect aid feats = do
  sb <- getsState $ getActorBody aid
  let tpos = bpos sb
      triggerFeat feat =
        case feat of
          F.Cause ef -> do
            -- No block against tile, hence unconditional.
            execSfxAtomic $ SfxTrigger aid tpos feat
            void $ effectSem ef aid aid
            return True
          _ -> return False
  goes <- mapM triggerFeat feats
  return $! or goes

-- * ReqSetTrajectory

reqSetTrajectory :: (MonadAtomic m, MonadServer m) => ActorId -> m ()
reqSetTrajectory aid = do
  cops <- getsState scops
  b@Actor{bpos, btrajectory, blid, bcolor} <- getsState $ getActorBody aid
  lvl <- getLevel blid
  let clearTrajectory =
        execUpdAtomic $ UpdTrajectoryActor aid btrajectory (Just [])
  case btrajectory of
    Just (d : lv) ->
      if not $ accessibleDir cops lvl bpos d
      then clearTrajectory
      else do
        when (length lv <= 1) $ do
          let toColor = Color.BrBlack
          when (bcolor /= toColor) $
            execUpdAtomic $ UpdColorActor aid bcolor toColor
        reqMove aid d
        execUpdAtomic $ UpdTrajectoryActor aid btrajectory (Just lv)
    _ -> assert `failure` "null trajectory" `twith` (aid, b)

-- * ReqGameRestart

-- TODO: implement a handshake and send hero names there,
-- so that they are available in the first game too,
-- not only in subsequent, restarted, games.
reqGameRestart :: (MonadAtomic m, MonadServer m)
               => ActorId -> Text -> Int -> [(Int, Text)] -> m ()
reqGameRestart aid stInfo d configHeroNames = do
  modifyServer $ \ser ->
    ser {sdebugNxt = (sdebugNxt ser) { sdifficultySer = d
                                     , sdebugCli = (sdebugCli (sdebugNxt ser))
                                                     {sdifficultyCli = d}
                                     }}
  b <- getsState $ getActorBody aid
  let fid = bfid b
  oldSt <- getsState $ gquit . (EM.! fid) . sfactionD
  modifyServer $ \ser ->
    ser { squit = True  -- do this at once
        , sheroNames = EM.insert fid configHeroNames $ sheroNames ser }
  revealItems Nothing Nothing
  execUpdAtomic $ UpdQuitFaction fid (Just b) oldSt
                $ Just $ Status Restart (fromEnum $ blid b) stInfo

-- * ReqGameExit

reqGameExit :: (MonadAtomic m, MonadServer m) => ActorId -> Int -> m ()
reqGameExit aid d = do
  modifyServer $ \ser ->
    ser {sdebugNxt = (sdebugNxt ser) { sdifficultySer = d
                                     , sdebugCli = (sdebugCli (sdebugNxt ser))
                                                     {sdifficultyCli = d}
                                     }}
  b <- getsState $ getActorBody aid
  let fid = bfid b
  oldSt <- getsState $ gquit . (EM.! fid) . sfactionD
  modifyServer $ \ser -> ser {squit = True}  -- do this at once
  execUpdAtomic $ UpdQuitFaction fid (Just b) oldSt
                $ Just $ Status Camping (fromEnum $ blid b) ""

-- * ReqGameSave

reqGameSave :: MonadServer m => m ()
reqGameSave = do
  modifyServer $ \ser -> ser {sbkpSave = True}
  modifyServer $ \ser -> ser {squit = True}  -- do this at once

-- * ReqAutomate

reqAutomate :: (MonadAtomic m, MonadServer m) => ActorId -> m ()
reqAutomate aid = do
  b <- getsState $ getActorBody aid
  execUpdAtomic $ UpdAutoFaction (bfid b) True
