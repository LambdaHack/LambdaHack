-- | Semantics of atomic commands shared by client and server.
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Atomic.PosAtomicRead
  ( PosAtomic(..), posUpdAtomic, posSfxAtomic
  , resetsFovCmdAtomic, breakUpdAtomic, loudUpdAtomic
  , seenAtomicCli, seenAtomicSer, lidOfPosAtomic
  , generalMoveItem
  ) where

import Control.Exception.Assert.Sugar
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES

import Game.LambdaHack.Atomic.CmdAtomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.ModeKind as ModeKind

-- All functions here that take an atomic action are executed
-- in the state just before the action is executed.

-- | The type representing visibility of actions to factions,
-- based on the position of the action, etc.
data PosAtomic =
    PosSight !LevelId ![Point]  -- ^ whomever sees all the positions, notices
  | PosFidAndSight !FactionId !LevelId ![Point]
                                -- ^ observers and the faction notice
  | PosSmell !LevelId ![Point]  -- ^ whomever smells all the positions, notices
  | PosFid !FactionId           -- ^ only the faction notices
  | PosFidAndSer !FactionId     -- ^ faction and server notices
  | PosSer                      -- ^ only the server notices
  | PosAll                      -- ^ everybody notices
  | PosNone                     -- ^ never broadcasted, but sent manually
  deriving (Show, Eq)

-- | Produces the positions where the action takes place. If a faction
-- is returned, the action is visible only for that faction, if Nothing
-- is returned, it's never visible. Empty list of positions implies
-- the action is visible always.
--
-- The goal of the mechanics: client should not get significantly
-- more information by looking at the atomic commands he is able to see
-- than by looking at the state changes they enact. E.g., @DisplaceActorA@
-- in a black room, with one actor carrying a 0-radius light would not be
-- distinguishable by looking at the state (or the screen) from @MoveActorA@
-- of the illuminated actor, hence such @DisplaceActorA@ should not be
-- observable, but @MoveActorA@ should be (or the former should be perceived
-- as the latter). However, to simplify, we assing as strict visibility
-- requirements to @MoveActorA@ as to @DisplaceActorA@ and fall back
-- to @SpotActorA@ (which provides minimal information that does not
-- contradict state) if the visibility is lower.
posUpdAtomic :: MonadStateRead m => UpdAtomic -> m PosAtomic
posUpdAtomic cmd = case cmd of
  UpdCreateActor _ body _ -> posProjBody body
  UpdDestroyActor _ body _ -> posProjBody body
  UpdCreateItem _ _ _ c -> singleContainer c
  UpdDestroyItem _ _ _ c -> singleContainer c
  UpdSpotActor _ body _ -> posProjBody body
  UpdLoseActor _ body _ -> posProjBody body
  UpdSpotItem _ _ _ c -> singleContainer c
  UpdLoseItem _ _ _ c -> singleContainer c
  UpdMoveActor aid fromP toP -> do
    (lid, _) <- posOfAid aid
    return $! PosSight lid [fromP, toP]
  UpdWaitActor aid _ -> singleAid aid
  UpdDisplaceActor source target -> do
    (slid, sp) <- posOfAid source
    (tlid, tp) <- posOfAid target
    return $! assert (slid == tlid) $ PosSight slid [sp, tp]
  UpdMoveItem _ _ aid _ _ -> singleAid aid
  UpdAgeActor aid _ -> singleAid aid
  UpdHealActor aid _ -> singleAid aid
  UpdCalmActor aid _ -> singleAid aid
  UpdHasteActor aid _ -> singleAid aid
  UpdOldFidActor aid _ _ -> singleAid aid
  UpdTrajectoryActor aid _ _ -> singleAid aid
  UpdColorActor aid _ _ -> singleAid aid
  UpdQuitFaction{} -> return PosAll
  UpdLeadFaction fid _ _ -> do
    fact <- getsState $ (EM.! fid) . sfactionD
    return $! if playerLeader $ gplayer fact
              then PosFidAndSer fid
              else PosNone
  UpdDiplFaction{} -> return PosAll
  UpdAutoFaction{} -> return PosAll
  UpdRecordKill aid _ -> singleFidAndAid aid
  UpdAlterTile lid p _ _ -> return $! PosSight lid [p]
  UpdAlterClear{} -> return PosAll
  UpdSearchTile aid p _ _ -> do
    (lid, pos) <- posOfAid aid
    return $! PosSight lid [pos, p]
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
  UpdAgeGame _ _ -> return PosAll
  UpdDiscover lid p _ _ -> return $! PosSight lid [p]
  UpdCover lid p _ _ -> return $! PosSight lid [p]
  UpdDiscoverSeed lid p _ _ -> return $! PosSight lid [p]
  UpdCoverSeed lid p _ _ -> return $! PosSight lid [p]
  UpdPerception{} -> return PosNone
  UpdRestart fid _ _ _ _ _ -> return $! PosFid fid
  UpdRestartServer _ -> return PosSer
  UpdResume fid _ -> return $! PosFid fid
  UpdResumeServer _ -> return PosSer
  UpdKillExit fid -> return $! PosFid fid
  UpdSaveBkp -> return PosAll
  UpdMsgAll{} -> return PosAll
  UpdRecordHistory fid -> return $! PosFid fid

posSfxAtomic :: MonadStateRead m => SfxAtomic -> m PosAtomic
posSfxAtomic cmd = case cmd of
  SfxStrike source target _ _ -> do
    (slid, sp) <- posOfAid source
    (tlid, tp) <- posOfAid target
    return $! assert (slid == tlid) $ PosSight slid [sp, tp]
  SfxRecoil source target _ _ -> do
    (slid, sp) <- posOfAid source
    (tlid, tp) <- posOfAid target
    return $! assert (slid == tlid) $ PosSight slid [sp, tp]
  SfxProject aid _ -> singleAid aid
  SfxCatch aid _ -> singleAid aid
  SfxActivate aid _ _ -> singleAid aid
  SfxCheck aid _ _ -> singleAid aid
  SfxTrigger aid p _ -> do
    (lid, pa) <- posOfAid aid
    return $! PosSight lid [pa, p]
  SfxShun aid p _ -> do
    (lid, pa) <- posOfAid aid
    return $! PosSight lid [pa, p]
  SfxEffect _ aid _ -> singleAid aid  -- sometimes we don't see source, OK
  SfxMsgFid fid _ -> return $! PosFid fid
  SfxMsgAll _ -> return PosAll
  SfxActorStart aid -> singleAid aid

posProjBody :: Monad m => Actor -> m PosAtomic
posProjBody body = return $!
  if bproj body
  then PosSight (blid body) [bpos body]
  else PosFidAndSight (bfid body) (blid body) [bpos body]

singleFidAndAid :: MonadStateRead m => ActorId -> m PosAtomic
singleFidAndAid aid = do
  body <- getsState $ getActorBody aid
  return $! PosFidAndSight (bfid body) (blid body) [bpos body]

singleAid :: MonadStateRead m => ActorId -> m PosAtomic
singleAid aid = do
  (lid, p) <- posOfAid aid
  return $! PosSight lid [p]

singleContainer :: MonadStateRead m => Container -> m PosAtomic
singleContainer c = do
  (lid, p) <- posOfContainer c
  return $! PosSight lid [p]

-- | Determines if a command resets FOV.
--
-- Invariant: if @resetsFovCmdAtomic@ determines we do not need
-- to reset Fov, perception (@ptotal@ to be precise, @psmell@ is irrelevant)
-- of any faction does not change upon recomputation. Otherwise,
-- save/restore would change game state.
resetsFovCmdAtomic :: UpdAtomic -> Bool
resetsFovCmdAtomic cmd = case cmd of
  -- Create/destroy actors and items.
  UpdCreateActor{} -> True  -- may have a light source
  UpdDestroyActor{} -> True
  UpdCreateItem{} -> True  -- may be a light source
  UpdDestroyItem{} -> True
  UpdSpotActor{} -> True
  UpdLoseActor{} -> True
  UpdSpotItem{} -> True
  UpdLoseItem{} -> True
  -- Move actors and items.
  UpdMoveActor{} -> True
  UpdDisplaceActor{} -> True
  UpdMoveItem{} -> True  -- light sources in inventory are doused
  -- Alter map.
  UpdAlterTile{} -> True  -- even if pos not visible initially
  UpdSpotTile{} -> True
  UpdLoseTile{} -> True
  _ -> False

-- | Decompose an atomic action. The original action is visible
-- if it's positions are visible both before and after the action
-- (in between the FOV might have changed). The decomposed actions
-- are only tested vs the FOV after the action and they give reduced
-- information that still modifies client's state to match the server state
-- wrt the current FOV and the subset of @posUpdAtomic@ that is visible.
-- The original actions give more information not only due to spanning
-- potentially more positions than those visible. E.g., @MoveActorA@
-- informs about the continued existence of the actor between
-- moves, v.s., popping out of existence and then back in.
breakUpdAtomic :: MonadStateRead m => UpdAtomic -> m [UpdAtomic]
breakUpdAtomic cmd = case cmd of
  UpdMoveActor aid _ toP -> do
    b <- getsState $ getActorBody aid
    ais <- getsState $ getCarriedAssocs b
    return [ UpdLoseActor aid b ais
           , UpdSpotActor aid b {bpos = toP, boldpos = bpos b} ais ]
  UpdDisplaceActor source target -> do
    sb <- getsState $ getActorBody source
    sais <- getsState $ getCarriedAssocs sb
    tb <- getsState $ getActorBody target
    tais <- getsState $ getCarriedAssocs tb
    return [ UpdLoseActor source sb sais
           , UpdSpotActor source sb {bpos = bpos tb, boldpos = bpos sb} sais
           , UpdLoseActor target tb tais
           , UpdSpotActor target tb {bpos = bpos sb, boldpos = bpos tb} tais
           ]
  -- No need to cover @UpdSearchTile@, because if an actor sees only
  -- one of the positions and so doesn't notice the search results,
  -- he's left with a hidden tile, which doesn't cause any trouble
  -- (because the commands doesn't change @State@ and the client-side
  -- processing of the command is lenient).
  _ -> return [cmd]

loudUpdAtomic :: FactionId -> UpdAtomic -> Bool
loudUpdAtomic fid cmd = case cmd of
  UpdDestroyActor _ body _ ->
    -- Death of a party member does not need to be heard, because it's seen.
    not $ fid == bfid body || bproj body
  _ -> False

seenAtomicCli :: Bool -> FactionId -> Perception -> PosAtomic -> Bool
seenAtomicCli knowEvents fid per posAtomic =
  case posAtomic of
    PosSight _ ps -> all (`ES.member` totalVisible per) ps || knowEvents
    PosFidAndSight fid2 _ ps ->
      fid == fid2 || all (`ES.member` totalVisible per) ps || knowEvents
    PosSmell _ ps -> all (`ES.member` smellVisible per) ps || knowEvents
    PosFid fid2 -> fid == fid2
    PosFidAndSer fid2 -> fid == fid2
    PosSer -> False
    PosAll -> True
    PosNone -> assert `failure` "no position possible" `twith` fid

seenAtomicSer :: PosAtomic -> Bool
seenAtomicSer posAtomic =
  case posAtomic of
    PosFid _ -> False
    PosNone -> False
    _ -> True

lidOfPosAtomic :: PosAtomic -> Maybe LevelId
lidOfPosAtomic pos = case pos of
  PosSight lid _ -> Just lid
  PosFidAndSight _ lid _ -> Just lid
  PosSmell lid _ -> Just lid
  _ -> Nothing

generalMoveItem :: MonadStateRead m
                => ItemId -> Int -> Container -> Container
                -> m [UpdAtomic]
generalMoveItem iid k c1 c2 = do
  case (c1, c2) of
    (CActor aid1 cstore1, CActor aid2 cstore2) | aid1 == aid2 ->
      return [UpdMoveItem iid k aid1 cstore1 cstore2]
    _ -> do
      item <- getsState $ getItemBody iid
      return [UpdLoseItem iid item k c1, UpdSpotItem iid item k c2]
