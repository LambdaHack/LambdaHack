-- | Representation and computation of visiblity of atomic commands
-- by clients.
--
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Atomic.PosAtomicRead
  ( PosAtomic(..), posUpdAtomic, posSfxAtomic
  , breakUpdAtomic, seenAtomicCli, seenAtomicSer
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , posProjBody, singleAid, doubleAid, singleContainer
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumSet as ES

import Game.LambdaHack.Atomic.CmdAtomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point

-- All functions here that take an atomic action are executed
-- in the state just before the action is executed.

-- | The type representing visibility of atomic commands to factions,
-- based on the position of the command, etc. Note that the server
-- sees and smells all positions.
data PosAtomic =
    PosSight LevelId [Point]    -- ^ whomever sees all the positions, notices
  | PosFidAndSight [FactionId] LevelId [Point]
                                -- ^ observers and the faction notice
  | PosSmell LevelId [Point]    -- ^ whomever smells all the positions, notices
  | PosFid FactionId            -- ^ only the faction notices, server doesn't
  | PosFidAndSer (Maybe LevelId) FactionId
                                -- ^ faction and server notices
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
  UpdCreateActor _ body _ -> return $! posProjBody body
  UpdDestroyActor _ body _ -> return $! posProjBody body
  UpdCreateItem _ _ _ c -> singleContainer c
  UpdDestroyItem _ _ _ c -> singleContainer c
  UpdSpotActor _ body _ -> return $! posProjBody body
  UpdLoseActor _ body _ -> return $! posProjBody body
  UpdSpotItem _ _ _ _ c -> singleContainer c
  UpdLoseItem _ _ _ _ c -> singleContainer c
  UpdSpotItemBag c _ _ -> singleContainer c
  UpdLoseItemBag c _ _ -> singleContainer c
  UpdMoveActor aid fromP toP -> do
    b <- getsState $ getActorBody aid
    -- Non-projectile actors are never totally isolated from envirnoment;
    -- they hear, feel air movement, etc.
    return $! if bproj b
              then PosSight (blid b) [fromP, toP]
              else PosFidAndSight [bfid b] (blid b) [fromP, toP]
  UpdWaitActor aid _ -> singleAid aid
  UpdDisplaceActor source target -> doubleAid source target
  UpdMoveItem _ _ _ _ CSha ->
    error $ "" `showFailure` cmd  -- shared stash is private
  UpdMoveItem _ _ _ CSha _ -> error $ "" `showFailure` cmd
  UpdMoveItem _ _ aid _ _ -> singleAid aid
  UpdRefillHP aid _ -> singleAid aid
  UpdRefillCalm aid _ -> singleAid aid
  UpdTrajectory aid _ _ -> singleAid aid
  UpdQuitFaction{} -> return PosAll
  UpdLeadFaction fid _ _ -> return $ PosFidAndSer Nothing fid
  UpdDiplFaction{} -> return PosAll
  UpdTacticFaction fid _ _ -> return $! PosFidAndSer Nothing fid
  UpdAutoFaction{} -> return PosAll
  UpdRecordKill aid _ _ -> singleAid aid
  UpdAlterTile lid p _ _ -> return $! PosSight lid [p]
  UpdAlterExplorable{} -> return PosAll
    -- Can't have @PosSight@, because we'd end up with many accessible
    -- unknown tiles, but the game reporting 'all seen'.
  UpdAlterGold{} -> return PosAll
  UpdSearchTile aid p _ -> do
    b <- getsState $ getActorBody aid
    return $! PosFidAndSight [bfid b] (blid b) [bpos b, p]
  UpdHideTile aid p _ -> do
    b <- getsState $ getActorBody aid
    return $! PosFidAndSight [bfid b] (blid b) [bpos b, p]
  UpdSpotTile lid ts -> do
    let ps = map fst ts
    return $! PosSight lid ps
  UpdLoseTile lid ts -> do
    let ps = map fst ts
    return $! PosSight lid ps
  UpdAlterSmell lid p _ _ -> return $! PosSmell lid [p]
  UpdSpotSmell lid sms -> do
    let ps = map fst sms
    return $! PosSmell lid ps
  UpdLoseSmell lid sms -> do
    let ps = map fst sms
    return $! PosSmell lid ps
  UpdTimeItem _ c _ _ -> singleContainer c
  UpdAgeGame _ -> return PosAll
  UpdUnAgeGame _ -> return PosAll
  UpdDiscover c _ _ _ -> singleContainer c
  UpdCover c _ _ _ -> singleContainer c
  UpdDiscoverKind c _ _ -> singleContainer c
  UpdCoverKind c _ _ -> singleContainer c
  UpdDiscoverAspect c _ _ -> singleContainer c
  UpdCoverAspect c _ _ -> singleContainer c
  UpdDiscoverServer{} -> return PosSer
  UpdCoverServer{} -> return PosSer
  UpdPerception{} -> return PosNone
  UpdRestart fid _ _ _ _ -> return $! PosFid fid
  UpdRestartServer _ -> return PosSer
  UpdResume _ _ -> return PosNone
  UpdResumeServer _ -> return PosSer
  UpdKillExit fid -> return $! PosFid fid
  UpdWriteSave -> return PosAll

-- | Produce the positions where the atomic special effect takes place.
posSfxAtomic :: MonadStateRead m => SfxAtomic -> m PosAtomic
posSfxAtomic cmd = case cmd of
  SfxStrike _ _ _ CSha -> return PosNone  -- shared stash is private
  SfxStrike _ target _ _ -> singleAid target
  SfxRecoil _ _ _ CSha -> return PosNone  -- shared stash is private
  SfxRecoil _ target _ _ -> singleAid target
  SfxSteal _ _ _ CSha -> return PosNone  -- shared stash is private
  SfxSteal _ target _ _ -> singleAid target
  SfxRelease _ _ _ CSha -> return PosNone  -- shared stash is private
  SfxRelease _ target _ _ -> singleAid target
  SfxProject aid _ cstore -> singleContainer $ CActor aid cstore
  SfxReceive aid _ cstore -> singleContainer $ CActor aid cstore
  SfxApply aid _ cstore -> singleContainer $ CActor aid cstore
  SfxCheck aid _ cstore -> singleContainer $ CActor aid cstore
  SfxTrigger aid p -> do
    body <- getsState $ getActorBody aid
    if bproj body
    then return $! PosSight (blid body) [bpos body, p]
    else return $! PosFidAndSight [bfid body] (blid body) [bpos body, p]
  SfxShun aid p -> do
    body <- getsState $ getActorBody aid
    if bproj body
    then return $! PosSight (blid body) [bpos body, p]
    else return $! PosFidAndSight [bfid body] (blid body) [bpos body, p]
  SfxEffect _ aid _ _ -> singleAid aid  -- sometimes we don't see source, OK
  SfxMsgFid fid _ -> return $! PosFid fid
  SfxSortSlots -> return PosAll
  SfxCollideTile aid _ -> singleAid aid

posProjBody :: Actor -> PosAtomic
posProjBody body =
  if bproj body
  then PosSight (blid body) [bpos body]
  else PosFidAndSight [bfid body] (blid body) [bpos body]

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

singleContainer :: MonadStateRead m => Container -> m PosAtomic
singleContainer (CFloor lid p) = return $! PosSight lid [p]
singleContainer (CEmbed lid p) = return $! PosSight lid [p]
singleContainer (CActor aid CSha) = do  -- shared stash is private
  b <- getsState $ getActorBody aid
  return $! PosFidAndSer (Just $ blid b) (bfid b)
singleContainer (CActor aid _) = singleAid aid
singleContainer (CTrunk fid lid p) =
  return $! PosFidAndSight [fid] lid [p]

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
  UpdMoveActor aid fromP toP -> do
    -- We assume other factions don't see leaders and we know the actor's
    -- faction always sees the atomic command, so the leader doesn't
    -- need to be updated (or the actor is a projectile, hence not a leader).
    b <- getsState $ getActorBody aid
    ais <- getsState $ getCarriedAssocsAndTrunk b
    return [ UpdLoseActor aid b ais
           , UpdSpotActor aid b {bpos = toP, boldpos = Just fromP} ais ]
  UpdDisplaceActor source target -> do
    sb <- getsState $ getActorBody source
    sais <- getsState $ getCarriedAssocsAndTrunk sb
    tb <- getsState $ getActorBody target
    tais <- getsState $ getCarriedAssocsAndTrunk tb
    return [ UpdLoseActor source sb sais
           , UpdSpotActor source sb { bpos = bpos tb
                                    , boldpos = Just $ bpos sb } sais
           , UpdLoseActor target tb tais
           , UpdSpotActor target tb { bpos = bpos sb
                                    , boldpos = Just $ bpos tb } tais
           ]
  _ -> return []

-- | Given the client, its perception and an atomic command, determine
-- if the client notices the command.
seenAtomicCli :: Bool -> FactionId -> Perception -> PosAtomic -> Bool
seenAtomicCli knowEvents fid per posAtomic =
  case posAtomic of
    PosSight _ ps -> all (`ES.member` totalVisible per) ps || knowEvents
    PosFidAndSight fids _ ps ->
      fid `elem` fids || all (`ES.member` totalVisible per) ps || knowEvents
    PosSmell _ ps -> all (`ES.member` totalSmelled per) ps || knowEvents
    PosFid fid2 -> fid == fid2
    PosFidAndSer _ fid2 -> fid == fid2
    PosSer -> False
    PosAll -> True
    PosNone -> error $ "no position possible" `showFailure` fid

-- | Determine whether the server would see a command that has
-- the given visibilty conditions.
seenAtomicSer :: PosAtomic -> Bool
seenAtomicSer posAtomic =
  case posAtomic of
    PosFid _ -> False
    PosNone -> error $ "no position possible" `showFailure` posAtomic
    _ -> True
