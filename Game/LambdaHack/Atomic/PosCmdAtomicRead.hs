-- | Semantics of atomic commands shared by client and server.
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Atomic.PosCmdAtomicRead
  ( PosAtomic(..), posCmdAtomic, posSfxAtomic
  , resetsFovAtomic, breakCmdAtomic, loudCmdAtomic
  , seenAtomicCli, seenAtomicSer, posOfContainer
  ) where

import Control.Exception.Assert.Sugar
import qualified Data.EnumSet as ES

import Game.LambdaHack.Atomic.CmdAtomic
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point

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
posCmdAtomic :: MonadReadState m => CmdAtomic -> m PosAtomic
posCmdAtomic cmd = case cmd of
  CreateActorA _ body _ -> posProjBody body
  DestroyActorA _ body _ -> posProjBody body
  CreateItemA _ _ _ c -> singleContainer c
  DestroyItemA _ _ _ c -> singleContainer c
  SpotActorA _ body _ -> posProjBody body
  LoseActorA _ body _ -> posProjBody body
  SpotItemA _ _ _ c -> singleContainer c
  LoseItemA _ _ _ c -> singleContainer c
  MoveActorA aid fromP toP -> do
    (lid, _) <- posOfAid aid
    return $! PosSight lid [fromP, toP]
  WaitActorA aid _ _ -> singleAid aid
  DisplaceActorA source target -> do
    (slid, sp) <- posOfAid source
    (tlid, tp) <- posOfAid target
    return $! assert (slid == tlid) $ PosSight slid [sp, tp]
  MoveItemA _ _ c1 c2 -> do  -- works even if moved between positions
    (lid1, p1) <- posOfContainer c1
    (lid2, p2) <- posOfContainer c2
    return $! assert (lid1 == lid2) $ PosSight lid1 [p1, p2]
  AgeActorA aid _ -> singleAid aid
  HealActorA aid _ -> singleAid aid
  CalmActorA aid _ -> singleAid aid
  HasteActorA aid _ -> singleAid aid
  TrajectoryActorA aid _ _ -> singleAid aid
  ColorActorA aid _ _ -> singleAid aid
  QuitFactionA{} -> return PosAll
  LeadFactionA fid _ _ -> return $! PosFidAndSer fid
  DiplFactionA{} -> return PosAll
  AutoFactionA{} -> return PosAll
  RecordKillA aid _ -> singleFidAndAid aid
  AlterTileA lid p _ _ -> return $! PosSight lid [p]
  SearchTileA aid p _ _ -> do
    (lid, pos) <- posOfAid aid
    return $! PosSight lid [pos, p]
  SpotTileA lid ts -> do
    let ps = map fst ts
    return $! PosSight lid ps
  LoseTileA lid ts -> do
    let ps = map fst ts
    return $! PosSight lid ps
  AlterSmellA lid p _ _ -> return $! PosSmell lid [p]
  SpotSmellA lid sms -> do
    let ps = map fst sms
    return $! PosSmell lid ps
  LoseSmellA lid sms -> do
    let ps = map fst sms
    return $! PosSmell lid ps
  AgeLevelA lid _ -> return $! PosSight lid []
  AgeGameA _ -> return PosAll
  DiscoverA lid p _ _ -> return $! PosSight lid [p]
  CoverA lid p _ _ -> return $! PosSight lid [p]
  PerceptionA{} -> return PosNone
  RestartA fid _ _ _ _ _ -> return $! PosFid fid
  RestartServerA _ -> return PosSer
  ResumeA fid _ -> return $! PosFid fid
  ResumeServerA _ -> return PosSer
  KillExitA fid -> return $! PosFid fid
  SaveBkpA -> return PosAll
  MsgAllA{} -> return PosAll

posSfxAtomic :: MonadReadState m => SfxAtomic -> m PosAtomic
posSfxAtomic cmd = case cmd of
  StrikeD source target _ _ -> do
    (slid, sp) <- posOfAid source
    (tlid, tp) <- posOfAid target
    return $! assert (slid == tlid) $ PosSight slid [sp, tp]
  RecoilD source target _ _ -> do
    (slid, sp) <- posOfAid source
    (tlid, tp) <- posOfAid target
    return $! assert (slid == tlid) $ PosSight slid [sp, tp]
  ProjectD aid _ -> singleAid aid
  CatchD aid _ -> singleAid aid
  ActivateD aid _ -> singleAid aid
  CheckD aid _ -> singleAid aid
  TriggerD aid p _ -> do
    (lid, pa) <- posOfAid aid
    return $! PosSight lid [pa, p]
  ShunD aid p _ -> do
    (lid, pa) <- posOfAid aid
    return $! PosSight lid [pa, p]
  EffectD aid _ -> singleAid aid
  MsgFidD fid _ -> return $! PosFid fid
  MsgAllD _ -> return PosAll
  DisplayPushD fid -> return $! PosFid fid
  DisplayDelayD fid -> return $! PosFid fid
  RecordHistoryD fid -> return $! PosFid fid

posProjBody :: Monad m => Actor -> m PosAtomic
posProjBody body = return $!
  if bproj body
  then PosSight (blid body) [bpos body]
  else PosFidAndSight (bfid body) (blid body) [bpos body]

singleFidAndAid :: MonadReadState m => ActorId -> m PosAtomic
singleFidAndAid aid = do
  body <- getsState $ getActorBody aid
  return $! PosFidAndSight (bfid body) (blid body) [bpos body]

singleAid :: MonadReadState m => ActorId -> m PosAtomic
singleAid aid = do
  b <- getsState $ getActorBody aid
  return $! PosSight (blid b) [bpos b]

posOfAid :: MonadReadState m => ActorId -> m (LevelId, Point)
posOfAid aid = do
  b <- getsState $ getActorBody aid
  return (blid b, bpos b)

posOfContainer :: MonadReadState m => Container -> m (LevelId, Point)
posOfContainer (CFloor lid p) = return (lid, p)
posOfContainer (CActor aid _) = posOfAid aid

singleContainer :: MonadReadState m => Container -> m PosAtomic
singleContainer c = do
  (lid, p) <- posOfContainer c
  return $! PosSight lid [p]

-- Determines is a command resets FOV. @Nothing@ means it always does.
-- A list of faction means it does for each of the factions.
-- This is only an optimization to save perception and spot/lose computation.
--
-- Invariant: if @resetsFovAtomic@ determines a faction does not need
-- to reset Fov, perception (@ptotal@ to be precise, @psmell@ is irrelevant)
-- of that faction does not change upon recomputation. Otherwise,
-- save/restore would change game state.
resetsFovAtomic :: MonadReadState m => CmdAtomic -> m (Maybe [FactionId])
resetsFovAtomic cmd = case cmd of
  CreateActorA _ body _ -> return $ Just [bfid body]
  DestroyActorA _ body _ -> return $ Just [bfid body]
  SpotActorA _ body _ -> return $ Just [bfid body]
  LoseActorA _ body _ -> return $ Just [bfid body]
  CreateItemA{} -> return $ Just []  -- unless shines
  DestroyItemA{} -> return $ Just []  -- ditto
  MoveActorA aid _ _ -> fmap Just $ fidOfAid aid  -- assumption: has no light
-- TODO: MoveActorCarryingLIghtA _ _ _ -> return Nothing
  DisplaceActorA source target -> do
    sfid <- fidOfAid source
    tfid <- fidOfAid target
    return $ Just $ if source == target
                    then []
                    else sfid ++ tfid
  MoveItemA{} -> return $ Just []  -- unless shiny
  AlterTileA{} -> return Nothing  -- even if pos not visible initially
  _ -> return $ Just []

fidOfAid :: MonadReadState m => ActorId -> m [FactionId]
fidOfAid aid = getsState $ (: []) . bfid . getActorBody aid

-- | Decompose an atomic action. The original action is visible
-- if it's positions are visible both before and after the action
-- (in between the FOV might have changed). The decomposed actions
-- are only tested vs the FOV after the action and they give reduced
-- information that still modifies client's state to match the server state
-- wrt the current FOV and the subset of @posCmdAtomic@ that is visible.
-- The original actions give more information not only due to spanning
-- potentially more positions than those visible. E.g., @MoveActorA@
-- informs about the continued existence of the actor between
-- moves, v.s., popping out of existence and then back in.
breakCmdAtomic :: MonadReadState m => CmdAtomic -> m [CmdAtomic]
breakCmdAtomic cmd = case cmd of
  MoveActorA aid _ toP -> do
    b <- getsState $ getActorBody aid
    ais <- getsState $ getCarriedAssocs b
    return [ LoseActorA aid b ais
           , SpotActorA aid b {bpos = toP, boldpos = bpos b} ais ]
  DisplaceActorA source target -> do
    sb <- getsState $ getActorBody source
    sais <- getsState $ getCarriedAssocs sb
    tb <- getsState $ getActorBody target
    tais <- getsState $ getCarriedAssocs tb
    return [ LoseActorA source sb sais
           , SpotActorA source sb {bpos = bpos tb, boldpos = bpos sb} sais
           , LoseActorA target tb tais
           , SpotActorA target tb {bpos = bpos sb, boldpos = bpos tb} tais
           ]
  MoveItemA iid k c1 c2 -> do
    item <- getsState $ getItemBody iid
    return [LoseItemA iid item k c1, SpotItemA iid item k c2]
  _ -> return [cmd]

loudCmdAtomic :: FactionId -> CmdAtomic -> Bool
loudCmdAtomic fid cmd = case cmd of
  DestroyActorA _ body _ ->
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
    PosNone -> assert `failure` "wrong position for server" `twith` posAtomic
    _ -> True
