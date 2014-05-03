{-# LANGUAGE GADTs #-}
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
  ( handleRequestAI, handleRequestUI, reqMove
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import Data.Text (Text)

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemFeature as IF
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
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

-- | The semantics of server commands. The resulting actor id
-- is of the actor that carried out the request.
handleRequestAI :: (MonadAtomic m, MonadServer m)
                => FactionId -> ActorId -> RequestAI -> m ActorId
handleRequestAI fid aid cmd = case cmd of
  ReqAITimed cmdT -> handleRequestTimed aid cmdT >> return aid
  ReqAILeader aidNew cmd2 -> do
    switchLeader fid aidNew
    handleRequestAI fid aidNew cmd2
  ReqAIPong -> return aid

-- | The semantics of server commands. The resulting actor id
-- is of the actor that carried out the request. @Nothing@ means
-- the command took no time.
handleRequestUI :: (MonadAtomic m, MonadServer m)
                => FactionId -> RequestUI -> m (Maybe ActorId)
handleRequestUI fid cmd = case cmd of
  ReqUITimed cmdT -> do
    fact <- getsState $ (EM.! fid) . sfactionD
    let aid = fromMaybe (assert `failure` fact) $ gleader fact
    handleRequestTimed aid cmdT >> return (Just aid)
  ReqUILeader aidNew cmd2 -> do
    switchLeader fid aidNew
    handleRequestUI fid cmd2
  ReqUIGameRestart aid t d names ->
    reqGameRestart aid t d names >> return Nothing
  ReqUIGameExit aid d -> reqGameExit aid d >> return Nothing
  ReqUIGameSave -> reqGameSave >> return Nothing
  ReqUIAutomate -> reqAutomate fid >> return Nothing
  ReqUIPong _ -> return Nothing

handleRequestTimed :: (MonadAtomic m, MonadServer m)
                   => ActorId -> RequestTimed a -> m ()
handleRequestTimed aid cmd = case cmd of
  ReqMove target -> reqMove aid target
  ReqMelee target iid -> reqMelee aid target iid
  ReqDisplace target -> reqDisplace aid target
  ReqAlter tpos mfeat -> reqAlter aid tpos mfeat
  ReqWait -> reqWait aid
  ReqMoveItem iid k fromCStore toCStore ->
    reqMoveItem aid iid k fromCStore toCStore
  ReqProject p eps iid cstore -> reqProject aid p eps iid cstore
  ReqApply iid cstore -> reqApply aid iid cstore
  ReqTrigger mfeat -> reqTrigger aid mfeat

switchLeader :: MonadAtomic m
             => FactionId -> ActorId -> m ()
switchLeader fid aidNew = do
  fact <- getsState $ (EM.! fid) . sfactionD
  bPre <- getsState $ getActorBody aidNew
  let mleader = gleader fact
      leadAtoms = [UpdLeadFaction fid mleader (Just aidNew)]
  mapM_ execUpdAtomic leadAtoms
  assert (Just aidNew /= mleader
          && not (bproj bPre)
          && not (isSpawnFact fact)
         `blame` (aidNew, bPre, fid, fact)) skip
  assert (bfid bPre == fid
          `blame` "client tries to move other faction actors"
          `twith` (aidNew, bPre, fid, fact)) skip

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
        newTime = timeShift time smellTimeout
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
    Just ((target, tb), _) | not (bproj sb && bproj tb) -> do  -- visible or not
      -- Projectiles are too small to hit each other.
      -- Attacking does not require full access, adjacency is enough.
      -- Here the only weapon of projectiles is picked, too.
      weapon <- meleeServer source
      reqMelee source target weapon
    _
      | accessible cops lvl spos tpos -> do
          -- Movement requires full access.
          execUpdAtomic $ UpdMoveActor source spos tpos
          addSmell source
      | otherwise ->
          -- Client foolishly tries to move into blocked, boring tile.
          execFailure source (ReqMove dir) MoveNothing

-- * ReqMelee

-- | Resolves the result of an actor moving into another.
-- Actors on blocked positions can be attacked without any restrictions.
-- For instance, an actor embedded in a wall can be attacked from
-- an adjacent position. This function is analogous to projectGroupItem,
-- but for melee and not using up the weapon.
-- No problem if there are many projectiles at the spot. We just
-- attack the one specified.
reqMelee :: (MonadAtomic m, MonadServer m)
         => ActorId -> ActorId -> ItemId -> m ()
reqMelee source target iid = do
  itemToF <- itemToFullServer
  sallAssocs <- fullAssocsServer source [CEqp, CBody]
  tallAssocs <- fullAssocsServer target [CEqp, CBody]
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  let adj = checkAdjacent sb tb
      req = ReqMelee target iid
  if source == target then execFailure source req MeleeSelf
  else if not adj then execFailure source req MeleeDistant
  else do
    let sfid = bfid sb
        tfid = bfid tb
    sfact <- getsState $ (EM.! sfid) . sfactionD
    let block = braced tb
        shield = not (null $ strongestShield sallAssocs)
                 || not (null $ strongestShield tallAssocs)
        hitA = if block && shield
               then MissBlock
               else if block || shield
                    then HitBlock
                    else Hit
    execSfxAtomic $ SfxStrike source target iid hitA
    -- Deduct a hitpoint for a pierce of a projectile.
    when (bproj sb) $ execUpdAtomic $ UpdHealActor source (-1)
    -- Msgs inside itemEffect describe the target part.
    itemEffect source target iid (itemToF iid) CEqp
    -- The only way to start a war is to slap an enemy. Being hit by
    -- and hitting projectiles count as unintentional friendly fire.
    let friendlyFire = bproj sb || bproj tb
        fromDipl = EM.findWithDefault Unknown tfid (gdipl sfact)
    unless (friendlyFire
            || isAtWar sfact tfid  -- already at war
            || isAllied sfact tfid  -- allies never at war
            || sfid == tfid) $
      execUpdAtomic $ UpdDiplFaction sfid tfid fromDipl War

-- * ReqDisplace

-- | Actor tries to swap positions with another.
reqDisplace :: (MonadAtomic m, MonadServer m) => ActorId -> ActorId -> m ()
reqDisplace source target = do
  cops <- getsState scops
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  tfact <- getsState $ (EM.! bfid tb) . sfactionD
  let spos = bpos sb
      tpos = bpos tb
      adj = checkAdjacent sb tb
      atWar = isAtWar tfact (bfid sb)
      req = ReqDisplace target
  dEnemy <- getsState $ dispEnemy tb
  if not adj then execFailure source req DisplaceDistant
  else if atWar && not dEnemy
  then do
    weapon <- meleeServer source
    reqMelee source target weapon  -- DisplaceDying, DisplaceSupported
  else do
    let lid = blid sb
    lvl <- getLevel lid
    -- Displacing requires full access.
    if accessible cops lvl spos tpos then do
      tgts <- getsState $ posToActors tpos lid
      case tgts of
        [] -> assert `failure` (source, sb, target, tb)
        [_] -> do
          execUpdAtomic $ UpdDisplaceActor source target
          addSmell source
          addSmell target
        _ -> execFailure source req DisplaceProjectiles
    else do
      -- Client foolishly tries to displace an actor without access.
      execFailure source req DisplaceAccess

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
      req = ReqAlter tpos mfeat
  if not $ adjacent spos tpos then execFailure source req AlterDistant
  else do
    lvl <- getLevel lid
    let serverTile = lvl `at` tpos
        freshClientTile = hideTile cotile lvl tpos
        changeTo tgroup = do
          -- No AlterD, because the effect is obvious (e.g., opened door).
          toTile <- rndToAction $ fmap (fromMaybe $ assert `failure` tgroup)
                                  $ opick tgroup (const True)
          unless (toTile == serverTile) $ do
            execUpdAtomic $ UpdAlterTile lid tpos serverTile toTile
            unless (isSecretPos lvl tpos) $
              case (Tile.isExplorable cotile serverTile,
                    Tile.isExplorable cotile toTile) of
                (False, True) -> execUpdAtomic $ UpdAlterClear lid 1
                (True, False) -> execUpdAtomic $ UpdAlterClear lid (-1)
                _ -> return ()
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
        groupsToAlterTo = mapMaybe toAlter feats
    as <- getsState $ actorList (const True) lid
    if null groupsToAlterTo && serverTile == freshClientTile then
      -- Neither searching nor altering possible; silly client.
      execFailure source req AlterNothing
    else do
      if EM.null $ lvl `atI` tpos then
        if unoccupied as tpos then do
          when (serverTile /= freshClientTile) $ do
            -- Search, in case some actors (of other factions?)
            -- don't know this tile.
            execUpdAtomic $ UpdSearchTile source tpos freshClientTile serverTile
          maybe skip changeTo $ listToMaybe groupsToAlterTo
            -- TODO: pick another, if the first one void
          -- Perform an effect, if any permitted.
          void $ triggerEffect source feats
        else execFailure source req AlterBlockActor
      else execFailure source req AlterBlockItem

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
  b <- getsState $ getActorBody aid
  let moveItem = do
        when (fromCStore == CGround) $ do
          seed <- getsServer $ (EM.! iid) . sitemSeedD
          execUpdAtomic $ UpdDiscoverSeed (blid b) (bpos b) iid seed
        cs <- actorConts iid k aid fromCStore
        let gmove (ck, c) = do
              upds <- generalMoveItem iid ck c (CActor aid toCStore)
              mapM_ execUpdAtomic upds
        mapM_ gmove cs
      req = ReqMoveItem iid k fromCStore toCStore
  if k < 1 || fromCStore == toCStore then execFailure aid req ItemNothing
  else if fromCStore /= CInv && toCStore /= CInv then moveItem
  else do
    Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
    let kind = okind $ bkind b
    if calmEnough b kind then moveItem
    else execFailure aid req ItemNotCalm

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
  let req = ReqProject tpxy eps iid cstore
  maybe skip (execFailure source req) mfail

-- * ReqApply

-- TODO: check actor has access to the item
reqApply :: (MonadAtomic m, MonadServer m)
         => ActorId  -- ^ actor applying the item (is on current level)
         -> ItemId   -- ^ the item to be applied
         -> CStore   -- ^ the location of the item
         -> m ()
reqApply aid iid cstore = do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  itemToF <- itemToFullServer
  let applyItem = do
        -- We have to destroy the item before the effect affects the item
        -- or the actor holding it or standing on it (later on we could
        -- lose track of the item and wouldn't be able to destroy it) .
        -- This is OK, because we don't remove the item type
        -- from the item dictionary, just an individual copy from the container.
        item <- getsState $ getItemBody iid
        let itemFull = itemToF iid
            consumable = IF.Consumable `elem` jfeature item
        if consumable then do
          -- TODO: don't destroy if not really used up; also, don't take time?
          cs <- actorConts iid 1 aid cstore
          mapM_ (\(_, c) -> execUpdAtomic $ UpdDestroyItem iid item 1 c) cs
          execSfxAtomic $ SfxActivate aid iid 1
          itemEffect aid aid iid itemFull cstore
        else do
          let itemNew = item {jisOn = not $ jisOn item}
          bag <- getsState $ getActorBag aid cstore
          seed <- getsServer $ (EM.! iid) . sitemSeedD
          let k = bag EM.! iid
              container = CActor aid cstore
          execUpdAtomic $ UpdLoseItem iid item k container
          void $ registerItem (itemNew,
                               fromJust $ snd $ fromJust $ snd itemFull)
                              seed k container True
      req = ReqApply iid cstore
  if cstore /= CInv then applyItem
  else do
    b <- getsState $ getActorBody aid
    let kind = okind $ bkind b
    if calmEnough b kind then applyItem
    else execFailure aid req ItemNotCalm

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
      req = ReqTrigger mfeat
  go <- triggerEffect aid feats
  unless go $ execFailure aid req TriggerNothing

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
            void $ effectSem ef aid aid Nothing
            return True
          _ -> return False
  goes <- mapM triggerFeat feats
  return $! or goes

-- * ReqGameRestart

-- TODO: implement a handshake and send hero names there,
-- so that they are available in the first game too,
-- not only in subsequent, restarted, games.
reqGameRestart :: (MonadAtomic m, MonadServer m)
               => ActorId -> Text -> Int -> [(Int, (Text, Text))] -> m ()
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
  modifyServer $ \ser -> ser {sbkpSave = True}
  modifyServer $ \ser -> ser {squit = True}  -- do this at once
  execUpdAtomic $ UpdQuitFaction fid (Just b) oldSt
                $ Just $ Status Camping (fromEnum $ blid b) ""

-- * ReqGameSave

reqGameSave :: MonadServer m => m ()
reqGameSave = do
  modifyServer $ \ser -> ser {sbkpSave = True}
  modifyServer $ \ser -> ser {squit = True}  -- do this at once

-- * ReqAutomate

reqAutomate :: (MonadAtomic m, MonadServer m) => FactionId -> m ()
reqAutomate fid = execUpdAtomic $ UpdAutoFaction fid True
