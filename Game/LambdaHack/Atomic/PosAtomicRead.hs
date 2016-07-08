-- | Semantics of atomic commands shared by client and server.
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Atomic.PosAtomicRead
  ( PosAtomic(..), posUpdAtomic, posSfxAtomic
  , resetsFovCmdAtomic, resetsLitCmdAtomic
  , resetsClearCmdAtomic, resetsFovCacheCmdAtomic
  , breakUpdAtomic, breakSfxAtomic, loudUpdAtomic
  , seenAtomicCli, seenAtomicSer, generalMoveItem, posProjBody
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Atomic.CmdAtomic
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind

-- All functions here that take an atomic action are executed
-- in the state just before the action is executed.

-- | The type representing visibility of atomic commands to factions,
-- based on the position of the command, etc. Note that the server
-- sees and smells all positions.
data PosAtomic =
    PosSight !LevelId ![Point]  -- ^ whomever sees all the positions, notices
  | PosFidAndSight ![FactionId] !LevelId ![Point]
                                -- ^ observers and the faction notice
  | PosSmell !LevelId ![Point]  -- ^ whomever smells all the positions, notices
  | PosFid !FactionId           -- ^ only the faction notices
  | PosFidAndSer !(Maybe LevelId) !FactionId  -- ^ faction and server notices
  | PosSer                      -- ^ only the server notices
  | PosAll                      -- ^ everybody notices
  | PosNone                     -- ^ never broadcasted, but sent manually
  deriving (Show, Eq)

-- | Produce the positions where the atomic update takes place.
--
-- The goal of the mechanics is to ensure the commands don't carry
-- significantly more information than their corresponding state diffs would.
-- In other words, the atomic commands involving the positions seen by a client
-- should convey similar information as the client would get by directly
-- observing the changes the commands enact on the visible portion of server
-- game state. The client is then free to change its copy of game state
-- accordingly or not --- it only partially reflects reality anyway.
--
-- E.g., @UpdDisplaceActor@ in a black room,
-- with one actor carrying a 0-radius light would not be
-- distinguishable by looking at the state (or the screen) from @UpdMoveActor@
-- of the illuminated actor, hence such @UpdDisplaceActor@ should not be
-- observable, but @UpdMoveActor@ should be (or the former should be perceived
-- as the latter). However, to simplify, we assing as strict visibility
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
  UpdSpotItem _ _ _ c -> singleContainer c
  UpdLoseItem _ _ _ c -> singleContainer c
  UpdMoveActor aid fromP toP -> do
    b <- getsState $ getActorBody aid
    -- Non-projectile actors are never totally isolated from envirnoment;
    -- they hear, feel air movement, etc.
    return $! if bproj b
              then PosSight (blid b) [fromP, toP]
              else PosFidAndSight [bfid b] (blid b) [fromP, toP]
  UpdWaitActor aid _ -> singleAid aid
  UpdDisplaceActor source target -> do
    sb <- getsState $ getActorBody source
    tb <- getsState $ getActorBody target
    let ps = [bpos sb, bpos tb]
        lid = assert (blid sb == blid tb) $ blid sb
    return $! if | bproj sb && bproj tb -> PosSight lid ps
                 | bproj sb -> PosFidAndSight [bfid tb] lid ps
                 | bproj tb -> PosFidAndSight [bfid sb] lid ps
                 | otherwise -> PosFidAndSight [bfid sb, bfid tb] lid ps
  UpdMoveItem _ _ aid _ CSha -> do  -- shared stash is private
    b <- getsState $ getActorBody aid
    return $! PosFidAndSer (Just $ blid b) (bfid b)
  UpdMoveItem _ _ aid CSha _ -> do  -- shared stash is private
    b <- getsState $ getActorBody aid
    return $! PosFidAndSer (Just $ blid b) (bfid b)
  UpdMoveItem _ _ aid _ _ -> singleAid aid
  UpdAgeActor aid _ -> singleAid aid
  UpdRefillHP aid _ -> singleAid aid
  UpdRefillCalm aid _ -> singleAid aid
  UpdFidImpressedActor aid _ _ -> singleAid aid
  UpdTrajectory aid _ _ -> singleAid aid
  UpdColorActor aid _ _ -> singleAid aid
  UpdQuitFaction{} -> return PosAll
  UpdLeadFaction fid _ _ -> do
    fact <- getsState $ (EM.! fid) . sfactionD
    return $! if fleaderMode (gplayer fact) /= LeaderNull
              then PosFidAndSer Nothing fid
              else PosNone
  UpdDiplFaction{} -> return PosAll
  UpdTacticFaction fid _ _ -> return $! PosFidAndSer Nothing fid
  UpdAutoFaction{} -> return PosAll
  UpdRecordKill aid _ _ -> singleFidAndAid aid
  UpdAlterTile lid p _ _ -> return $! PosSight lid [p]
  UpdAlterClear{} -> return PosAll
  UpdSearchTile aid p _ _ -> do
    (lid, pos) <- posOfAid aid
    return $! PosSight lid [pos, p]
  UpdLearnSecrets aid _ _ -> singleAid aid
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
  UpdAgeGame _ _ -> return PosAll
  UpdDiscover c _ _ _ _ -> singleContainer c
  UpdCover c _ _ _ _ -> singleContainer c
  UpdDiscoverKind c _ _ -> singleContainer c
  UpdCoverKind c _ _ -> singleContainer c
  UpdDiscoverSeed c _ _ _ -> singleContainer c
  UpdCoverSeed c _ _ _ -> singleContainer c
  UpdPerception{} -> return PosNone
  UpdRestart fid _ _ _ _ _ -> return $! PosFid fid
  UpdRestartServer _ -> return PosSer
  UpdResume fid _ -> return $! PosFid fid
  UpdResumeServer _ -> return PosSer
  UpdKillExit fid -> return $! PosFid fid
  UpdWriteSave -> return PosAll
  UpdMsgAll{} -> return PosAll
  UpdRecordHistory fid -> return $! PosFid fid

-- | Produce the positions where the atomic special effect takes place.
posSfxAtomic :: MonadStateRead m => SfxAtomic -> m PosAtomic
posSfxAtomic cmd = case cmd of
  SfxStrike _ _ _ CSha _ ->  -- shared stash is private
    return PosNone  -- TODO: PosSerAndFidIfSight; but probably never used
  SfxStrike source target _ _ _ -> doubleAid source target
  SfxRecoil _ _ _ CSha _ ->  -- shared stash is private
    return PosNone  -- TODO: PosSerAndFidIfSight; but probably never used
  SfxRecoil source target _ _ _ -> doubleAid source target
  SfxProject aid _ cstore -> singleContainer $ CActor aid cstore
  SfxCatch aid _ cstore -> singleContainer $ CActor aid cstore
  SfxApply aid _ cstore -> singleContainer $ CActor aid cstore
  SfxCheck aid _ cstore -> singleContainer $ CActor aid cstore
  SfxTrigger aid p _ -> do
    (lid, pa) <- posOfAid aid
    return $! PosSight lid [pa, p]
  SfxShun aid p _ -> do
    (lid, pa) <- posOfAid aid
    return $! PosSight lid [pa, p]
  SfxEffect _ aid _ _ -> singleAid aid  -- sometimes we don't see source, OK
  SfxMsgFid fid _ -> return $! PosFid fid
  SfxMsgAll _ -> return PosAll

posProjBody :: Actor -> PosAtomic
posProjBody body =
  if bproj body
  then PosSight (blid body) [bpos body]
  else PosFidAndSight [bfid body] (blid body) [bpos body]

singleFidAndAid :: MonadStateRead m => ActorId -> m PosAtomic
singleFidAndAid aid = do
  body <- getsState $ getActorBody aid
  return $! PosFidAndSight [bfid body] (blid body) [bpos body]

singleAid :: MonadStateRead m => ActorId -> m PosAtomic
singleAid aid = do
  (lid, p) <- posOfAid aid
  return $! PosSight lid [p]

doubleAid :: MonadStateRead m => ActorId -> ActorId -> m PosAtomic
doubleAid source target = do
  (slid, sp) <- posOfAid source
  (tlid, tp) <- posOfAid target
  return $! assert (slid == tlid) $ PosSight slid [sp, tp]

singleContainer :: MonadStateRead m => Container -> m PosAtomic
singleContainer (CFloor lid p) = return $! PosSight lid [p]
singleContainer (CEmbed lid p) = return $! PosSight lid [p]
singleContainer (CActor aid CSha) = do  -- shared stash is private
  b <- getsState $ getActorBody aid
  return $! PosFidAndSer (Just $ blid b) (bfid b)
singleContainer (CActor aid _) = do
  (lid, p) <- posOfAid aid
  return $! PosSight lid [p]
singleContainer (CTrunk fid lid p) = return $! PosFidAndSight [fid] lid [p]

resetsFovCmdAtomic :: UpdAtomic -> EM.EnumMap ItemId FovCache3
                   -> Either Bool [ActorId]
resetsFovCmdAtomic cmd itemFovCache = case cmd of
  -- Create/destroy actors and items.
  UpdCreateActor aid2 _ _ -> Right [aid2]
  UpdDestroyActor _ _ _ -> Left False
  UpdCreateItem iid _ _ (CActor aid2 s) -> itemAffectsSightRadius iid [s] aid2
  UpdDestroyItem iid _ _ (CActor aid2 s) -> itemAffectsSightRadius iid [s] aid2
  UpdSpotActor aid2 _ _ -> Right [aid2]
  UpdLoseActor _ _ _ -> Left False
  UpdSpotItem iid _ _ (CActor aid2 s) -> itemAffectsSightRadius iid [s] aid2
  UpdLoseItem iid _ _ (CActor aid2 s) -> itemAffectsSightRadius iid [s] aid2
  -- Move actors and items.
  UpdMoveActor aid2 _ _ -> Right [aid2]
  UpdDisplaceActor aid2 aid3 -> Right [aid2, aid3]
  UpdMoveItem iid _ aid2 s1 s2 -> itemAffectsSightRadius iid [s1, s2] aid2
  UpdRefillCalm aid2 _ -> Right [aid2]
  -- Alter map.
  UpdAlterTile{} -> Left True
  _ -> Right []
 where
  itemAffectsSightRadius iid stores aid2 =
    if not (null $ intersect stores [CEqp, COrgan])
       && case EM.lookup iid itemFovCache of
         Just FovCache3{fovSight} -> fovSight /= 0
         Nothing -> False
    then Right [aid2]
    else Right []

resetsClearCmdAtomic :: UpdAtomic -> Bool
resetsClearCmdAtomic cmd = case cmd of  -- TODO
  -- Create/destroy actors and items.
  UpdCreateActor{} -> True
  UpdDestroyActor{} -> True
  UpdCreateItem{} -> True  -- may affect sight radius
  UpdDestroyItem{} -> True
  UpdSpotActor{} -> True
  UpdLoseActor{} -> True
  UpdSpotItem{} -> True
  UpdLoseItem{} -> True
  -- Move actors and items.
  UpdMoveActor{} -> True
  UpdDisplaceActor{} -> True
  UpdMoveItem{} -> True  -- sight radius bonuses
  UpdRefillCalm{} -> True  -- Calm caps sight radius
  -- Alter map.
  UpdAlterTile{} -> True  -- even if pos not visible initially
  _ -> False

resetsFovCacheCmdAtomic :: UpdAtomic -> Bool
resetsFovCacheCmdAtomic = resetsClearCmdAtomic  -- TODO

-- | Determines if a command resets the data about lit tiles
-- (both dynamically and statically).
resetsLitCmdAtomic :: UpdAtomic -> Bool
resetsLitCmdAtomic cmd = case cmd of
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
  UpdMoveItem{} -> True  -- light sources don't shine in backpack, etc.
  -- Alter map.
  UpdAlterTile{} -> True  -- even if pos not visible initially
  _ -> False

-- | Decompose an atomic action. The original action is visible
-- if it's positions are visible both before and after the action
-- (in between the FOV might have changed). The decomposed actions
-- are only tested vs the FOV after the action and they give reduced
-- information that still modifies client's state to match the server state
-- wrt the current FOV and the subset of @posUpdAtomic@ that is visible.
-- The original actions give more information not only due to spanning
-- potentially more positions than those visible. E.g., @UpdMoveActor@
-- informs about the continued existence of the actor between
-- moves, v.s., popping out of existence and then back in.
breakUpdAtomic :: MonadStateRead m => UpdAtomic -> m [UpdAtomic]
breakUpdAtomic cmd = case cmd of
  UpdMoveActor aid _ toP -> do
    -- We assume other factions don't see leaders and we know the actor's
    -- faction always sees the atomic command, so the leader doesn't
    -- need to be updated (or the actor is a projectile, hence not a leader).
    b <- getsState $ getActorBody aid
    ais <- getsState $ getCarriedAssocs b
    return [ UpdLoseActor aid b ais
           , UpdSpotActor aid b {bpos = toP, boldpos = Just $ bpos b} ais ]
  UpdDisplaceActor source target -> do
    sb <- getsState $ getActorBody source
    sais <- getsState $ getCarriedAssocs sb
    tb <- getsState $ getActorBody target
    tais <- getsState $ getCarriedAssocs tb
    return [ UpdLoseActor source sb sais
           , UpdSpotActor source sb { bpos = bpos tb
                                    , boldpos = Just $ bpos sb } sais
           , UpdLoseActor target tb tais
           , UpdSpotActor target tb { bpos = bpos sb
                                    , boldpos = Just $ bpos tb } tais
           ]
  UpdMoveItem iid k aid cstore1 cstore2 | cstore1 == CSha  -- CSha is private
                                          || cstore2 == CSha ->
    containerMoveItem iid k (CActor aid cstore1) (CActor aid cstore2)
  -- No need to cover @UpdSearchTile@, because if an actor sees only
  -- one of the positions and so doesn't notice the search results,
  -- he's left with a hidden tile, which doesn't cause any trouble
  -- (because the commands doesn't change @State@ and the client-side
  -- processing of the command is lenient).
  _ -> return [cmd]

-- | Decompose an atomic special effect.
breakSfxAtomic :: MonadStateRead m => SfxAtomic -> m [SfxAtomic]
breakSfxAtomic cmd = case cmd of
  SfxStrike source target _ _ _ -> do
    -- Hack: make a fight detectable even if one of combatants not visible.
    sb <- getsState $ getActorBody source
    return $! [ SfxEffect (bfid sb) source (IK.RefillCalm (-1)) 0
              | not $ bproj sb ]
              ++ [SfxEffect (bfid sb) target (IK.RefillHP (-1)) (-1)]
  _ -> return [cmd]

-- | Messages for some unseen game object creation/destruction/alteration.
loudUpdAtomic :: MonadStateRead m
              => Bool -> FactionId -> UpdAtomic -> m (Maybe Text)
loudUpdAtomic local fid cmd = do
  msound <- case cmd of
    UpdDestroyActor _ body _
      -- Death of a party member does not need to be heard,
      -- because it's seen.
      | not $ fid == bfid body || bproj body -> return $ Just "shriek"
    UpdCreateItem _ _ _ (CActor _ CGround) -> return $ Just "clatter"
    UpdAlterTile _ _ fromTile _ -> do
      Kind.COps{cotile} <- getsState scops
      if Tile.isDoor cotile fromTile
        then return $ Just "creaking sound"
        else return $ Just "rumble"
    _ -> return Nothing
  let distant = if local then [] else ["distant"]
      hear sound = makeSentence [ "you hear"
                                , MU.AW $ MU.Phrase $ distant ++ [sound] ]
  return $! hear <$> msound

-- | Given the client, it's perception and an atomic command, determine
-- if the client notices the command.
seenAtomicCli :: Bool -> FactionId -> Perception -> PosAtomic -> Bool
seenAtomicCli knowEvents fid per posAtomic =
  case posAtomic of
    PosSight _ ps -> all (`ES.member` totalVisible per) ps || knowEvents
    PosFidAndSight fids _ ps ->
      fid `elem` fids || all (`ES.member` totalVisible per) ps || knowEvents
    PosSmell _ ps -> all (`ES.member` smellVisible per) ps || knowEvents
    PosFid fid2 -> fid == fid2
    PosFidAndSer _ fid2 -> fid == fid2
    PosSer -> False
    PosAll -> True
    PosNone -> assert `failure` "no position possible" `twith` fid

seenAtomicSer :: PosAtomic -> Bool
seenAtomicSer posAtomic =
  case posAtomic of
    PosFid _ -> False
    PosNone -> False
    _ -> True

-- | Generate the atomic updates that jointly perform a given item move.
generalMoveItem :: MonadStateRead m
                => ItemId -> Int -> Container -> Container
                -> m [UpdAtomic]
generalMoveItem iid k c1 c2 =
  case (c1, c2) of
    (CActor aid1 cstore1, CActor aid2 cstore2) | aid1 == aid2 ->
      return [UpdMoveItem iid k aid1 cstore1 cstore2]
    _ -> containerMoveItem iid k c1 c2

containerMoveItem :: MonadStateRead m
                  => ItemId -> Int -> Container -> Container
                  -> m [UpdAtomic]
containerMoveItem iid k c1 c2 = do
  bag <- getsState $ getCBag c1
  case iid `EM.lookup` bag of
    Nothing -> assert `failure` (iid, k, c1, c2)
    Just (_, it) -> do
      item <- getsState $ getItemBody iid
      return [ UpdLoseItem iid item (k, take k it) c1
             , UpdSpotItem iid item (k, take k it) c2 ]
