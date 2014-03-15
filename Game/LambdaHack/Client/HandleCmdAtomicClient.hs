-- | Handle atomic commands received by the client.
module Game.LambdaHack.Client.HandleCmdAtomicClient
  ( cmdAtomicSemCli, cmdAtomicFilterCli
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Maybe
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Animation
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.TileKind

-- * RespUpdAtomicAI

-- | Clients keep a subset of atomic commands sent by the server
-- and add some of their own. The result of this function is the list
-- of commands kept for each command received.
cmdAtomicFilterCli :: MonadClient m => UpdAtomic -> m [UpdAtomic]
cmdAtomicFilterCli cmd = case cmd of
  UpdMoveActor aid _ toP -> do
    cmdSml <- deleteSmell aid toP
    return $ [cmd] ++ cmdSml
  UpdDisplaceActor source target -> do
    bs <- getsState $ getActorBody source
    bt <- getsState $ getActorBody target
    cmdSource <- deleteSmell source (bpos bt)
    cmdTarget <- deleteSmell target (bpos bs)
    return $ [cmd] ++ cmdSource ++ cmdTarget
  UpdAlterTile lid p fromTile toTile -> do
    Kind.COps{cotile=Kind.Ops{okind}} <- getsState scops
    lvl <- getLevel lid
    let t = lvl `at` p
    if t == fromTile
      then return [cmd]
      else do
        -- From alterTileA@ we know @t == freshClientTile@,
        -- which is uncanny, so we produce a message.
        -- It happens when a client thinks the tile is @t@,
        -- but it's @fromTile@, and @AlterTileA@ changes it
        -- to @toTile@. See @alterTileA@.
        let subject = ""  -- a hack, we we don't handle adverbs well
            verb = "turn into"
            msg = makeSentence [ "the", MU.Text $ tname $ okind t
                               , "at position", MU.Text $ tshow p
                               , "suddenly"  -- adverb
                               , MU.SubjectVerbSg subject verb
                               , MU.AW $ MU.Text $ tname $ okind toTile ]
        return [ cmd  -- reveal the tile
               , UpdMsgAll msg  -- show the message
               ]
  UpdSearchTile aid p fromTile toTile -> do
    b <- getsState $ getActorBody aid
    lvl <- getLevel $ blid b
    let t = lvl `at` p
    return $!
      if t == fromTile
      then -- Fully ignorant. (No intermediate knowledge possible.)
           [ cmd  -- show the message
           , UpdAlterTile (blid b) p fromTile toTile  -- reveal tile
           ]
      else if t == toTile
           then [cmd]  -- Already knows the tile fully, only confirm.
           else -- Misguided.
                assert `failure` "LoseTile fails to reset memory"
                       `twith` (aid, p, fromTile, toTile, b, t, cmd)
  UpdSpotTile lid ts -> do
    Kind.COps{cotile} <- getsState scops
    lvl <- getLevel lid
    -- We ignore the server resending us hidden versions of the tiles
    -- (and resending us the same data we already got).
    -- If the tiles are changed to other variants of the hidden tile,
    -- we can still verify by searching, and the UI warns us "obscured".
    let notKnown (p, t) = let tClient = lvl `at` p
                          in t /= tClient
                             && (not (isSecretPos lvl p)
                                 || t /= Tile.hideAs cotile tClient)
        newTs = filter notKnown ts
    return $! if null newTs then [] else [UpdSpotTile lid newTs]
  UpdAlterSmell lid p fromSm _toSm -> do
    lvl <- getLevel lid
    let msml = EM.lookup p $ lsmell lvl
    return $ if msml /= fromSm then
               -- Revert to the server smell before server command executes.
               -- This is needed due to our hacky removal of traversed smells
               -- in @deleteSmell@.
               [UpdAlterSmell lid p msml fromSm, cmd]
             else
               [cmd]
  UpdDiscover _ _ iid _ -> do
    disco <- getsClient sdisco
    item <- getsState $ getItemBody iid
    if jkindIx item `EM.member` disco
      then return []
      else return [cmd]
  UpdCover _ _ iid _ -> do
    disco <- getsClient sdisco
    item <- getsState $ getItemBody iid
    if jkindIx item `EM.notMember` disco
      then return []
      else return [cmd]
  UpdPerception lid outPer inPer -> do
    -- Here we cheat by setting a new perception outright instead of
    -- in @cmdAtomicSemCli@, to avoid computing perception twice.
    -- TODO: try to assert similar things as for @atomicRemember@:
    -- that posUpdAtomic of all the Lose* commands was visible in old Per,
    -- but is not visible any more.
    perOld <- getPerFid lid
    perception lid outPer inPer
    perNew <- getPerFid lid
    s <- getState
    fid <- getsClient sside
    -- Wipe out actors that just became invisible due to changed FOV.
    -- TODO: perhaps instead create LoseActor for all actors in lprio,
    -- and keep only those where seenAtomicCli is True; this is even
    -- cheaper than repeated posToActor (until it's optimized).
    let outFov = totalVisible perOld ES.\\ totalVisible perNew
        outPrio = concatMap (\p -> posToActors p lid s) $ ES.elems outFov
        fActor ((aid, b), ais) =
          -- TODO: instead of bproj, check that actor sees himself.
          if not (bproj b) && bfid b == fid
          then Nothing  -- optimization: the actor is soon lost anyway,
                        -- e.g., via DominateActorA, so don't bother
          else Just $ UpdLoseActor aid b ais
        outActor = mapMaybe fActor outPrio
    -- Wipe out remembered items on tiles that now came into view.
    Level{lfloor, lsmell} <- getLevel lid
    let inFov = totalVisible perNew ES.\\ totalVisible perOld
        pMaybe p = maybe Nothing (\x -> Just (p, x))
        inFloor = mapMaybe (\p -> pMaybe p $ EM.lookup p lfloor)
                           (ES.elems inFov)
        fItem p (iid, k) = UpdLoseItem iid (getItemBody iid s) k (CFloor lid p)
        fBag (p, bag) = map (fItem p) $ EM.assocs bag
        inItem = concatMap fBag inFloor
    -- Remembered map tiles not wiped out, due to optimization in @spotTileA@.
    -- Wipe out remembered smell on tiles that now came into smell Fov.
    let inSmellFov = smellVisible perNew ES.\\ smellVisible perOld
        inSm = mapMaybe (\p -> pMaybe p $ EM.lookup p lsmell)
                        (ES.elems inSmellFov)
        inSmell = if null inSm then [] else [UpdLoseSmell lid inSm]
    let seenNew = seenAtomicCli False fid perNew
        seenOld = seenAtomicCli False fid perOld
    -- TODO: these assertions are probably expensive
    psActor <- mapM posUpdAtomic outActor
    -- Verify that we forget only previously seen actors.
    assert (allB seenOld psActor) skip
    -- Verify that we forget only currently invisible actors.
    assert (allB (not . seenNew) psActor) skip
    psItemSmell <- mapM posUpdAtomic $ inItem ++ inSmell
    -- Verify that we forget only previously invisible items and smell.
    assert (allB (not . seenOld) psItemSmell) skip
    -- Verify that we forget only currently seen items and smell.
    assert (allB seenNew psItemSmell) skip
    return $! cmd : outActor ++ inItem ++ inSmell
  _ -> return [cmd]

deleteSmell :: MonadClient m => ActorId -> Point -> m [UpdAtomic]
deleteSmell aid pos = do
  Kind.COps{coactor = Kind.Ops{okind}} <- getsState scops
  b <- getsState $ getActorBody aid
  let canSmell = asmell $ okind $ bkind b
  if canSmell then do
    lvl <- getLevel $ blid b
    let msml = EM.lookup pos $ lsmell lvl
    return $
      maybe [] (\sml -> [UpdAlterSmell (blid b) pos (Just sml) Nothing]) msml
  else return []

-- | Effect of atomic actions on client state is calculated
-- in the global state before the command is executed.
cmdAtomicSemCli :: MonadClient m => UpdAtomic -> m ()
cmdAtomicSemCli cmd = case cmd of
  UpdCreateActor aid body _ -> createActor aid body
  UpdDestroyActor aid b _ -> destroyActor aid b True
  UpdSpotActor aid body _ -> createActor aid body
  UpdLoseActor aid b _ -> destroyActor aid b False
  UpdMoveItem iid _ c1 c2 -> moveItem iid c1 c2
  UpdLeadFaction fid source target -> do
    side <- getsClient sside
    when (side == fid) $ do
      mleader <- getsClient _sleader
      assert (mleader == source     -- somebody changed the leader for us
              || mleader == target  -- we changed the leader originally
              `blame` "unexpected leader" `twith` (cmd, mleader)) skip
      modifyClient $ \cli -> cli {_sleader = target}
  UpdDiscover lid p iid ik -> discover lid p iid ik
  UpdCover lid p iid ik -> cover lid p iid ik
  UpdPerception lid outPer inPer -> perception lid outPer inPer
  UpdRestart side sdisco sfper _ sdebugCli _ -> do
    shistory <- getsClient shistory
    isAI <- getsClient sisAI
    let cli = defStateClient shistory side isAI
    putClient cli { sdisco
                  , sfper
                  -- , sundo = [UpdAtomic cmd]
                  , scurDifficulty = sdifficultyCli sdebugCli
                  , sdebugCli }
  UpdResume _fid sfper -> modifyClient $ \cli -> cli {sfper}
  UpdKillExit _fid -> killExit
  UpdSaveBkp -> saveClient
  _ -> return ()

createActor :: MonadClient m => ActorId -> Actor -> m ()
createActor aid _b = do
  let affect tgt = case tgt of
        TEnemyPos a _ _ permit | a == aid -> TEnemy a permit
        _ -> tgt
      affect3 (tgt, mpath) = case tgt of
        TEnemyPos a _ _ permit | a == aid -> (TEnemy a permit, Nothing)
        _ -> (tgt, mpath)
  modifyClient $ \cli -> cli {stargetD = EM.map affect3 (stargetD cli)}
  modifyClient $ \cli -> cli {scursor = affect $ scursor cli}

destroyActor :: MonadClient m => ActorId -> Actor -> Bool -> m ()
destroyActor aid b destroy = do
  when destroy $ modifyClient $ updateTarget aid (const Nothing)  -- gc
  modifyClient $ \cli -> cli {sbfsD = EM.delete aid $ sbfsD cli}  -- gc
  let affect tgt = case tgt of
        TEnemy a permit | a == aid -> TEnemyPos a (blid b) (bpos b) permit
          -- Don't consider @destroy@, because even if actor dead, it makes
          -- sense to go to last known location to loot or find others.
        _ -> tgt
      affect3 (tgt, mpath) = (affect tgt, mpath)  -- old path always good
  modifyClient $ \cli -> cli {stargetD = EM.map affect3 (stargetD cli)}
  modifyClient $ \cli -> cli {scursor = affect $ scursor cli}

moveItem :: MonadClient m => ItemId -> Container -> Container -> m ()
moveItem iid c1 c2 =
  case (c1, c2) of
    (CActor _ CGround, CActor aid _) -> do
      -- Update items slots, in case the item was picked up by
      -- the other client for the same faction.
      b <- getsState $ getActorBody aid
      unless (bproj b) $ do
        side <- getsClient sside
        when (bfid b == side) $ void $ updateItemSlot aid iid
    _ -> return ()

perception :: MonadClient m => LevelId -> Perception -> Perception -> m ()
perception lid outPer inPer = do
  -- Clients can't compute FOV on their own, because they don't know
  -- if unknown tiles are clear or not. Server would need to send
  -- info about properties of unknown tiles, which complicates
  -- and makes heavier the most bulky data set in the game: tile maps.
  -- Note we assume, but do not check that @outPer@ is contained
  -- in current perception and @inPer@ has no common part with it.
  -- It would make the already very costly operation even more expensive.
  perOld <- getPerFid lid
  -- Check if new perception is already set in @cmdAtomicFilterCli@
  -- or if we are doing undo/redo, which does not involve filtering.
  -- The data structure is strict, so the cheap check can't be any simpler.
  let interAlready per =
        Just $ totalVisible per `ES.intersection` totalVisible perOld
      unset = maybe False ES.null (interAlready inPer)
              || maybe False (not . ES.null) (interAlready outPer)
  when unset $ do
    let adj Nothing = assert `failure` "no perception to alter" `twith` lid
        adj (Just per) = Just $ addPer (diffPer per outPer) inPer
        f = EM.alter adj lid
    modifyClient $ \cli -> cli {sfper = f (sfper cli)}

discover :: MonadClient m
          => LevelId -> Point -> ItemId -> Kind.Id ItemKind -> m ()
discover lid p iid ik = do
  item <- getsState $ getItemBody iid
  let f Nothing = Just ik
      f (Just ik2) = assert `failure` "already discovered"
                            `twith` (lid, p, iid, ik, ik2)
  modifyClient $ \cli -> cli {sdisco = EM.alter f (jkindIx item) (sdisco cli)}

cover :: MonadClient m
       => LevelId -> Point -> ItemId -> Kind.Id ItemKind -> m ()
cover lid p iid ik = do
  item <- getsState $ getItemBody iid
  let f Nothing = assert `failure` "already covered" `twith` (lid, p, iid, ik)
      f (Just ik2) = assert (ik == ik2 `blame` "unexpected covered item kind"
                                       `twith` (ik, ik2)) Nothing
  modifyClient $ \cli -> cli {sdisco = EM.alter f (jkindIx item) (sdisco cli)}

killExit :: MonadClient m => m ()
killExit = modifyClient $ \cli -> cli {squit = True}
