-- | Representation and computation of visiblity of atomic commands
-- by clients.
--
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Atomic.PosAtomicRead
  ( PosAtomic(..), posUpdAtomic, posSfxAtomic, iidUpdAtomic, iidSfxAtomic
  , breakUpdAtomic, lidOfPos, seenAtomicCli, seenAtomicSer
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , pointsProjBody, posProjBody, singleAid, doubleAid
  , singleContainerStash, singleContainerActor
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES

import Game.LambdaHack.Atomic.CmdAtomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Types
import Game.LambdaHack.Definition.Defs

-- All functions here that take an atomic action are executed
-- in the state just before the action is executed.

-- | The type representing visibility of atomic commands to factions,
-- based on the position of the command, etc. Note that the server
-- sees and smells all positions. Also note that hearing is not covered
-- because it gives very restricted information, so hearing doesn't equal
-- seeing (and we assume smelling actors get lots of data from smells).
data PosAtomic =
    PosSight LevelId [Point]    -- ^ whomever sees all the positions, notices
  | PosFidAndSight FactionId LevelId [Point]
                                -- ^ observers and the faction notice
  | PosSmell LevelId [Point]    -- ^ whomever smells all the positions, notices
  | PosSightLevels [(LevelId, Point)]
                                -- ^ whomever sees all the positions, notices
  | PosFid FactionId            -- ^ only the faction notices, server doesn't
  | PosFidAndSer FactionId      -- ^ faction and server notices
  | PosSer                      -- ^ only the server notices
  | PosAll                      -- ^ everybody notices
  | PosNone                     -- ^ never broadcasted, but sent manually
  deriving (Show, Eq)

-- | Produce the positions where the atomic update takes place or, more
-- generally, the conditions under which the update can be noticed by
-- a client.
--
-- The goal of this mechanics is to ensure that atomic commands involving
-- some positions visible by a client convey similar information as the client
-- would get by directly observing the changes
-- of the portion of server state limited to the visible positions.
-- Consequently, when the visible commands are later applied
-- to the client's state, the state stays consistent
-- --- in sync with the server state and correctly limited by visiblity.
-- There is some wiggle room both in what "in sync" and
-- "visible" means and how they propagate through time.
--
-- E.g., @UpdDisplaceActor@ in a black room between two enemy actors,
-- with only one actor carrying a 0-radius light would not be
-- distinguishable by looking at the state (or the screen) from @UpdMoveActor@
-- of the illuminated actor, hence such @UpdDisplaceActor@ should not be
-- observable, but @UpdMoveActor@ in similar cotext would be
-- (or the former should be perceived as the latter).
-- However, to simplify, we assign as strict visibility
-- requirements to @UpdMoveActor@ as to @UpdDisplaceActor@ and fall back
-- to @UpdSpotActor@ (which provides minimal information that does not
-- contradict state) if the visibility is lower.
posUpdAtomic :: MonadStateRead m => UpdAtomic -> m PosAtomic
posUpdAtomic cmd = case cmd of
  UpdRegisterItems{} -> return PosNone
  UpdCreateActor _ body _ -> return $! posProjBody body
  UpdDestroyActor _ body _ -> return $! posProjBody body
  UpdCreateItem _ _ _ _ c -> singleContainerStash c
  UpdDestroyItem _ _ _ _ c -> singleContainerStash c
  UpdSpotActor _ body -> return $! posProjBody body
  UpdLoseActor _ body -> return $! posProjBody body
  UpdSpotItem _ _ _ c -> singleContainerStash c
  UpdLoseItem _ _ _ c -> singleContainerStash c
  UpdSpotItemBag _ c _ -> singleContainerStash c
  UpdLoseItemBag _ c _ -> singleContainerStash c
  UpdMoveActor aid fromP toP -> do
    b <- getsState $ getActorBody aid
    -- Non-projectile actors are never totally isolated from environment;
    -- they hear, feel air movement, etc.
    return $! pointsProjBody b [fromP, toP]
  UpdWaitActor aid _ _ -> singleAid aid
  UpdDisplaceActor source target -> doubleAid source target
  UpdMoveItem _ _ aid cstore1 cstore2 -> do
    b <- getsState $ getActorBody aid
    mlidPos1 <- lidPosOfStash b cstore1
    mlidPos2 <- lidPosOfStash b cstore2
    let mlidPos = mlidPos1 `mplus` mlidPos2
    return $! maybe (posProjBody b)
                    (\lidPos -> PosSightLevels [lidPos, (blid b, bpos b)])
                    mlidPos
  UpdRefillHP aid _ -> singleAid aid
  UpdRefillCalm aid _ -> singleAid aid
  UpdTrajectory aid _ _ -> singleAid aid
  UpdQuitFaction{} -> return PosAll
  UpdSpotStashFaction _ fid lid pos -> return $! PosFidAndSight fid lid [pos]
  UpdLoseStashFaction _ fid lid pos -> return $! PosFidAndSight fid lid [pos]
  UpdLeadFaction fid _ _ -> return $! PosFidAndSer fid
  UpdDiplFaction{} -> return PosAll
  UpdDoctrineFaction fid _ _ -> return $! PosFidAndSer fid
  UpdAutoFaction{} -> return PosAll
  UpdRecordKill aid _ _ -> singleAid aid
  UpdAlterTile lid p _ _ -> return $! PosSight lid [p]
  UpdAlterExplorable{} -> return PosAll
    -- Can't have @PosSight@, because we'd end up with many accessible
    -- unknown tiles, but the game reporting 'all seen'.
  UpdAlterGold{} -> return PosAll
  UpdSearchTile aid p _ -> do
    b <- getsState $ getActorBody aid
    return $! pointsProjBody b [bpos b, p]
  UpdHideTile aid p _ -> do
    b <- getsState $ getActorBody aid
    return $! pointsProjBody b [bpos b, p]
  UpdSpotTile lid ts -> do
    let ps = map fst ts
    return $! PosSight lid ps
  UpdLoseTile lid ts -> do
    let ps = map fst ts
    return $! PosSight lid ps
  UpdSpotEntry lid ts -> do
    let ps = map fst ts
    return $! PosSight lid ps
  UpdLoseEntry lid ts -> do
    let ps = map fst ts
    return $! PosSight lid ps
  UpdAlterSmell lid p _ _ -> return $! PosSmell lid [p]
  UpdSpotSmell lid sms -> do
    let ps = map fst sms
    return $! PosSmell lid ps
  UpdLoseSmell lid sms -> do
    let ps = map fst sms
    return $! PosSmell lid ps
  UpdTimeItem _ c _ _ -> singleContainerStash c
  UpdAgeGame _ -> return PosAll
  UpdUnAgeGame _ -> return PosAll
  UpdDiscover c _ _ _ -> singleContainerActor c
    -- This implies other factions applying items from their inventory,
    -- when we can't see the position of the stash, won't Id the item
    -- for us, even when notice item usage. Thrown items will Id, though,
    -- just as triggering items from the floor or embedded items.
  UpdCover c _ _ _ -> singleContainerActor c
  UpdDiscoverKind c _ _ -> singleContainerActor c
  UpdCoverKind c _ _ -> singleContainerActor c
  UpdDiscoverAspect c _ _ -> singleContainerActor c
  UpdCoverAspect c _ _ -> singleContainerActor c
  UpdDiscoverServer{} -> return PosSer
  UpdCoverServer{} -> return PosSer
  UpdPerception{} -> return PosNone
  UpdRestart fid _ _ _ _ _ -> return $! PosFid fid
  UpdRestartServer _ -> return PosSer
  UpdResume _ _ -> return PosNone
  UpdResumeServer _ -> return PosSer
  UpdKillExit fid -> return $! PosFid fid
  UpdWriteSave -> return PosAll
  UpdHearFid fid _ _ -> return $! PosFid fid
  UpdMuteMessages fid _ -> return $! PosFid fid

-- | Produce the positions where the atomic special effect takes place.
posSfxAtomic :: MonadStateRead m => SfxAtomic -> m PosAtomic
posSfxAtomic cmd = case cmd of
  SfxStrike _ target _ -> singleAid target
  SfxRecoil _ target _ -> singleAid target
  SfxSteal _ target _ -> singleAid target
  SfxRelease _ target _ -> singleAid target
  SfxProject aid _ -> singleAid aid
  SfxReceive aid _ -> singleAid aid
  SfxApply aid _ -> singleAid aid
  SfxCheck aid _ -> singleAid aid
  SfxTrigger aid lid p _ -> do
    body <- getsState $ getActorBody aid
    return $! PosSightLevels [(lid, p), (blid body, bpos body)]
      -- @PosFidAndSightLevels@ would be better, but no big deal
  SfxShun aid lid p _ -> do
    body <- getsState $ getActorBody aid
    return $! PosSightLevels [(lid, p), (blid body, bpos body)]
  SfxEffect _ aid _ _ _ -> singleAid aid  -- sometimes we don't see source, OK
  SfxItemApplied _ c -> singleContainerActor c
  SfxMsgFid fid _ -> return $! PosFid fid
  SfxRestart -> return PosAll
  SfxCollideTile aid _ -> singleAid aid
  SfxTaunt _ aid -> singleAid aid

-- | All items introduced by the atomic command, to be used in it.
iidUpdAtomic :: UpdAtomic -> [ItemId]
iidUpdAtomic cmd = case cmd of
  UpdRegisterItems{} -> []
  UpdCreateActor{} -> []  -- iids and items needed even on server
  UpdDestroyActor{} -> []
  UpdCreateItem{} -> []
  UpdDestroyItem{} -> []
  UpdSpotActor _ body -> getCarriedIidsAndTrunk body
  UpdLoseActor{} -> []  -- already seen, so items known
  UpdSpotItem _ iid _ _ -> [iid]
  UpdLoseItem{} -> []
  UpdSpotItemBag _ _ bag -> EM.keys bag
  UpdLoseItemBag{} -> []
  UpdMoveActor{} -> []
  UpdWaitActor{} -> []
  UpdDisplaceActor{} -> []
  UpdMoveItem{} -> []
  UpdRefillHP{} -> []
  UpdRefillCalm{} -> []
  UpdTrajectory{} -> []
  UpdQuitFaction{} -> []
  UpdSpotStashFaction{} -> []
  UpdLoseStashFaction{} -> []
  UpdLeadFaction{} -> []
  UpdDiplFaction{} -> []
  UpdDoctrineFaction{} -> []
  UpdAutoFaction{} -> []
  UpdRecordKill{} -> []
  UpdAlterTile{} -> []
  UpdAlterExplorable{} -> []
  UpdAlterGold{} -> []
  UpdSearchTile{} -> []
  UpdHideTile{} -> []
  UpdSpotTile{} -> []
  UpdLoseTile{} -> []
  UpdSpotEntry{} -> []
  UpdLoseEntry{} -> []
  UpdAlterSmell{} -> []
  UpdSpotSmell{} -> []
  UpdLoseSmell{} -> []
  UpdTimeItem iid _ _ _ -> [iid]
  UpdAgeGame{} -> []
  UpdUnAgeGame{} -> []
  UpdDiscover _ iid _ _ -> [iid]
  UpdCover _ iid _ _ -> [iid]
  UpdDiscoverKind{} -> []
  UpdCoverKind{} -> []
  UpdDiscoverAspect _ iid _ -> [iid]
  UpdCoverAspect _ iid _ -> [iid]
  UpdDiscoverServer{} -> []  -- never sent to clients
  UpdCoverServer{} -> []
  UpdPerception{} -> []
  UpdRestart{} -> []
  UpdRestartServer{} -> []
  UpdResume{} -> []
  UpdResumeServer{} -> []
  UpdKillExit{} -> []
  UpdWriteSave -> []
  UpdHearFid{} -> []
  UpdMuteMessages{} -> []

-- | All items introduced by the atomic special effect, to be used in it.
iidSfxAtomic :: SfxAtomic -> [ItemId]
iidSfxAtomic cmd = case cmd of
  SfxStrike _ _ iid -> [iid]
  SfxRecoil _ _ iid -> [iid]
  SfxSteal _ _ iid -> [iid]
  SfxRelease _ _ iid -> [iid]
  SfxProject _ iid -> [iid]
  SfxReceive _ iid -> [iid]
  SfxApply _ iid -> [iid]
  SfxCheck _ iid -> [iid]
  SfxTrigger{} -> []
  SfxShun{} -> []
  SfxEffect{} -> []
  SfxItemApplied iid _ -> [iid]
  SfxMsgFid{} -> []
  SfxRestart{} -> []
  SfxCollideTile{} -> []
  SfxTaunt{} -> []

pointsProjBody :: Actor -> [Point] -> PosAtomic
pointsProjBody body ps =
  if bproj body
  then PosSight (blid body) ps
  else PosFidAndSight (bfid body) (blid body) ps

posProjBody :: Actor -> PosAtomic
posProjBody body = pointsProjBody body [bpos body]

singleAid :: MonadStateRead m => ActorId -> m PosAtomic
singleAid aid = do
  body <- getsState $ getActorBody aid
  return $! posProjBody body

doubleAid :: MonadStateRead m => ActorId -> ActorId -> m PosAtomic
doubleAid source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  -- No @PosFidAndSight@ instead of @PosSight@, because both positions
  -- need to be seen to have the enemy actor in client's state.
  return $! assert (blid sb == blid tb) $ PosSight (blid sb) [bpos sb, bpos tb]

singleContainerStash :: MonadStateRead m => Container -> m PosAtomic
singleContainerStash (CFloor lid p) = return $! PosSight lid [p]
singleContainerStash (CEmbed lid p) = return $! PosSight lid [p]
singleContainerStash (CActor aid cstore) = do
  b <- getsState $ getActorBody aid
  mlidPos <- lidPosOfStash b cstore
  return $! maybe (posProjBody b)
                  (\lidPos -> PosSightLevels [lidPos, (blid b, bpos b)])
                    -- the actor's position is needed so that a message
                    -- about the actor is not sent to a client that doesn't
                    -- know the actor; actor's faction is ignored, because
                    -- for these operations actor doesn't vanish
                  mlidPos
singleContainerStash (CTrunk fid lid p) = return $! PosFidAndSight fid lid [p]

singleContainerActor :: MonadStateRead m => Container -> m PosAtomic
singleContainerActor (CFloor lid p) = return $! PosSight lid [p]
singleContainerActor (CEmbed lid p) = return $! PosSight lid [p]
singleContainerActor (CActor aid _) = do
  b <- getsState $ getActorBody aid
  return $! posProjBody b
    -- stash position is ignored, because for these operations, nothing
    -- is added to that position; the store name is only used for flavour text
singleContainerActor (CTrunk fid lid p) = return $! PosFidAndSight fid lid [p]

lidPosOfStash :: MonadStateRead m
              => Actor -> CStore -> m (Maybe (LevelId, Point))
lidPosOfStash b cstore =
  case cstore of
    CStash -> do
      mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid b
      case mstash of
        Just{} -> return mstash
        Nothing -> error $ "manipulating void stash" `showFailure` b
    _ -> return Nothing

-- | Decompose an atomic action that is outside a client's visiblity.
-- The decomposed actions give less information that the original command,
-- but some of them may fall within the visibility range of the client.
-- The original action may give more information than even the total sum
-- of all actions it's broken into. E.g., @UpdMoveActor@
-- informs about the continued existence of the actor between
-- moves vs popping out of existence and then back in.
--
-- This is computed in server's @State@ from before performing the command.
breakUpdAtomic :: MonadStateRead m => UpdAtomic -> m [UpdAtomic]
breakUpdAtomic cmd = case cmd of
  UpdCreateItem verbose iid item kit (CActor aid CStash) -> do
    b <- getsState $ getActorBody aid
    mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid b
    case mstash of
      Just (lid, pos) ->
        return [UpdCreateItem verbose iid item kit (CFloor lid pos)]
      Nothing -> error $ "manipulating void stash" `showFailure` (aid, b, item)
  UpdDestroyItem verbose iid item kit (CActor aid CStash) -> do
    b <- getsState $ getActorBody aid
    mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid b
    case mstash of
      Just (lid, pos) ->
        return [UpdDestroyItem verbose iid item kit (CFloor lid pos)]
      Nothing -> error $ "manipulating void stash" `showFailure` (aid, b, item)
  UpdSpotItem verbose iid kit (CActor aid CStash) -> do
    b <- getsState $ getActorBody aid
    mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid b
    case mstash of
      Just (lid, pos) -> return [UpdSpotItem verbose iid kit (CFloor lid pos)]
      Nothing -> error $ "manipulating void stash" `showFailure` (aid, b, iid)
  UpdLoseItem verbose iid kit (CActor aid CStash) -> do
    b <- getsState $ getActorBody aid
    mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid b
    case mstash of
      Just (lid, pos) -> return [UpdLoseItem verbose iid kit (CFloor lid pos)]
      Nothing -> error $ "manipulating void stash" `showFailure` (aid, b, iid)
  UpdSpotItemBag verbose (CActor aid CStash) bag -> do
    b <- getsState $ getActorBody aid
    mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid b
    case mstash of
      Just (lid, pos) -> return [UpdSpotItemBag verbose (CFloor lid pos) bag]
      Nothing -> error $ "manipulating void stash" `showFailure` (aid, b, bag)
  UpdLoseItemBag verbose (CActor aid CStash) bag -> do
    b <- getsState $ getActorBody aid
    mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid b
    case mstash of
      Just (lid, pos) -> return [UpdLoseItemBag verbose (CFloor lid pos) bag]
      Nothing -> error $ "manipulating void stash" `showFailure` (aid, b, bag)
  UpdMoveItem iid k aid CStash store2 -> do
    b <- getsState $ getActorBody aid
    bag <- getsState $ getBodyStoreBag b CStash
    let (k1, it1) = bag EM.! iid
        kit = assert (k <= k1) (k, take k it1)
    mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid b
    case mstash of
      Just (lid, pos) -> return [ UpdLoseItem True iid kit (CFloor lid pos)
                                , UpdSpotItem True iid kit (CActor aid store2) ]
      Nothing -> error $ "manipulating void stash" `showFailure` (aid, b, iid)
  UpdMoveItem iid k aid store1 CStash -> do
    b <- getsState $ getActorBody aid
    bag <- getsState $ getBodyStoreBag b store1
    let (k1, it1) = bag EM.! iid
        kit = assert (k <= k1) (k, take k it1)
    mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid b
    case mstash of
      Just (lid, pos) -> return [ UpdLoseItem True iid kit (CActor aid store1)
                                , UpdSpotItem True iid kit (CFloor lid pos) ]
      Nothing -> error $ "manipulating void stash" `showFailure` (aid, b, iid)
  UpdMoveActor aid fromP toP -> do
    -- We assume other factions don't see leaders and we know the actor's
    -- faction always sees the atomic command and no other commands
    -- may be inserted between the two below, so the leader doesn't
    -- need to be updated, even when aid is the leader.
    b <- getsState $ getActorBody aid
    return [ UpdLoseActor aid b
           , UpdSpotActor aid b {bpos = toP, boldpos = Just fromP} ]
  UpdDisplaceActor source target -> do
    sb <- getsState $ getActorBody source
    tb <- getsState $ getActorBody target
    -- The order ensures the invariant that no two big actors occupy the same
    -- position is maintained. The actions about leadership are required
    -- to keep faction data (identify of the leader) consistent with actor
    -- data (the actor that is the leader exists). Here, for speed
    -- and simplicity we violate the property that in a faction
    -- that has leaders, if any eligible actor is alive,
    -- the leader is set, because for a moment there may be no leader,
    -- even though other actors of the faction may exist.
    msleader <- getsState $ gleader . (EM.! bfid sb) . sfactionD
    mtleader <- getsState $ gleader . (EM.! bfid tb) . sfactionD
    return $ [ UpdLeadFaction (bfid sb) msleader Nothing
             | Just source == msleader ]
             ++ [ UpdLeadFaction (bfid tb) mtleader Nothing
                | Just target == mtleader ]
             ++ [ UpdLoseActor source sb
                , UpdLoseActor target tb
                , UpdSpotActor source sb { bpos = bpos tb
                                         , boldpos = Just $ bpos sb }
                , UpdSpotActor target tb { bpos = bpos sb
                                         , boldpos = Just $ bpos tb } ]
             ++ [ UpdLeadFaction (bfid sb) Nothing msleader
                | Just source == msleader ]
             ++ [ UpdLeadFaction (bfid tb) Nothing mtleader
                | Just target == mtleader ]
  UpdTimeItem iid (CActor aid CStash) fromIt toIt -> do
    b <- getsState $ getActorBody aid
    mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid b
    case mstash of
      Just (lid, pos) -> return [UpdTimeItem iid (CFloor lid pos) fromIt toIt]
      Nothing -> error $ "manipulating void stash" `showFailure` (aid, b, iid)
  _ -> return []

-- | What is the main map level the @PosAtomic@ refers to, if any.
lidOfPos :: PosAtomic -> Maybe LevelId
lidOfPos posAtomic =
  case posAtomic of
    PosSight lid _ -> Just lid
    PosFidAndSight _ lid _ -> Just lid
    PosSmell lid _ -> Just lid
    PosSightLevels [] -> Nothing
    PosSightLevels ((lid, _) : _) -> Just lid
    PosFid{} -> Nothing
    PosFidAndSer{} -> Nothing
    PosSer -> Nothing
    PosAll -> Nothing
    PosNone -> Nothing

-- | Given the client, its perception and an atomic command, determine
-- if the client notices the command.
seenAtomicCli :: Bool -> FactionId -> PerLid -> PosAtomic -> Bool
seenAtomicCli knowEvents fid perLid posAtomic =
  let per = (perLid EM.!)
  in case posAtomic of
    PosSight lid ps -> all (`ES.member` totalVisible (per lid)) ps || knowEvents
    PosFidAndSight fid2 lid ps ->
      fid == fid2 || all (`ES.member` totalVisible (per lid)) ps || knowEvents
    PosSmell lid ps -> all (`ES.member` totalSmelled (per lid)) ps || knowEvents
    PosSightLevels l ->
      let visible (lid, pos) = pos `ES.member` totalVisible (per lid)
      in all visible l || knowEvents
    PosFid fid2 -> fid == fid2
    PosFidAndSer fid2 -> fid == fid2
    PosSer -> False
    PosAll -> True
    PosNone -> False

-- | Determine whether the server would see a command that has
-- the given visibilty conditions.
seenAtomicSer :: PosAtomic -> Bool
seenAtomicSer posAtomic =
  case posAtomic of
    PosFid _ -> False
    PosNone -> error $ "no position possible" `showFailure` posAtomic
    _ -> True
