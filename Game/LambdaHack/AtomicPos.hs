{-# LANGUAGE OverloadedStrings #-}
-- | Semantics of atomic commands shared by client and server.
-- See https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture.
module Game.LambdaHack.AtomicPos
  ( PosAtomic(..), posCmdAtomic, posSfxAtomic
  , resetsFovAtomic, breakCmdAtomic, loudCmdAtomic
  ) where

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.AtomicCmd
import Game.LambdaHack.AtomicSem (posOfAid, posOfContainer)
import Game.LambdaHack.Faction
import Game.LambdaHack.Level
import Game.LambdaHack.Point
import Game.LambdaHack.Utils.Assert

-- All functions here that take an atomic action are executed
-- in the state just before the action is executed.

data PosAtomic =
    PosLevel LevelId [Point]  -- ^ whomever sees all the positions, notices
  | PosSmell LevelId [Point]  -- ^ whomever smells all the positions, notices
  | PosOnly FactionId         -- ^ only the faction notices
  | PosAndSer FactionId       -- ^ faction and server notices
  | PosServer                 -- ^ only the server notices
  | PosAll                    -- ^ everybody notices
  | PosNone                   -- ^ never broadcasted, but sent manually
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
posCmdAtomic :: MonadActionRO m => CmdAtomic -> m PosAtomic
posCmdAtomic cmd = case cmd of
  CreateActorA _ body _ -> return $ PosLevel (blid body) [bpos body]
  DestroyActorA _ body _ ->
    -- The faction of the actor sometimes does not see his death
    -- (if none of the other actors is observing it).
    return $ PosLevel (blid body) [bpos body]
  CreateItemA _ _ _ c -> singleContainer c
  DestroyItemA _ _ _ c -> singleContainer c
  SpotActorA _ body _ -> return $ PosLevel (blid body) [bpos body]
  LoseActorA _ body _ -> return $ PosLevel (blid body) [bpos body]
  SpotItemA _ _ _ c -> singleContainer c
  LoseItemA _ _ _ c -> singleContainer c
  MoveActorA aid fromP toP -> do
    (lid, _) <- posOfAid aid
    return $ PosLevel lid [fromP, toP]
  WaitActorA aid _ _ -> singleAid aid
  DisplaceActorA source target -> do
    (slid, sp) <- posOfAid source
    (tlid, tp) <- posOfAid target
    return $ assert (slid == tlid) $ PosLevel slid [sp, tp]
  MoveItemA _ _ c1 c2 -> do  -- works even if moved between positions
    (lid1, p1) <- posOfContainer c1
    (lid2, p2) <- posOfContainer c2
    return $ assert (lid1 == lid2) $ PosLevel lid1 [p1, p2]
  AgeActorA aid _ -> singleAid aid
  HealActorA aid _ -> singleAid aid
  HasteActorA aid _ -> singleAid aid
  DominateActorA target _ _ -> singleAid target
  PathActorA aid _ _ -> singleAid aid
  ColorActorA aid _ _ -> singleAid aid
  QuitFactionA _ _ _ -> return PosAll
  LeadFactionA fid _ _ -> return $ PosAndSer fid
  DiplFactionA _ _ _ _ -> return PosAll
  AlterTileA lid p _ _ -> return $ PosLevel lid [p]
  SpotTileA lid ts -> do
    let ps = map fst ts
    return $ PosLevel lid ps
  LoseTileA lid ts -> do
    let ps = map fst ts
    return $ PosLevel lid ps
  AlterSecretA _ _ -> return PosNone
  AlterSmellA lid p _ _ -> return $ PosSmell lid [p]
  SpotSmellA lid sms -> do
    let ps = map fst sms
    return $ PosSmell lid ps
  LoseSmellA lid sms -> do
    let ps = map fst sms
    return $ PosSmell lid ps
  AgeLevelA lid _ ->  return $ PosLevel lid []
  AgeGameA _ ->  return PosAll
  DiscoverA lid p _ _ -> return $ PosLevel lid [p]
  CoverA lid p _ _ -> return $ PosLevel lid [p]
  PerceptionA _ _ _ -> return PosNone
  RestartA fid _ _ _ -> return $ PosOnly fid
  RestartServerA _ -> return PosServer
  ResumeA fid _ -> return $ PosOnly fid
  ResumeServerA _ -> return PosServer
  SaveExitA -> return $ PosAll
  SaveBkpA -> return $ PosAll

posSfxAtomic :: MonadActionRO m => SfxAtomic -> m PosAtomic
posSfxAtomic cmd = case cmd of
  StrikeD source target _ _ -> do
    (slid, sp) <- posOfAid source
    (tlid, tp) <- posOfAid target
    return $ assert (slid == tlid) $ PosLevel slid [sp, tp]
  RecoilD source target _ _ -> do
    (slid, sp) <- posOfAid source
    (tlid, tp) <- posOfAid target
    return $ assert (slid == tlid) $ PosLevel slid [sp, tp]
  ProjectD aid _ -> singleAid aid
  CatchD aid _ -> singleAid aid
  ActivateD aid _ -> singleAid aid
  CheckD aid _ -> singleAid aid
  TriggerD aid p _ _ -> do
    (lid, pa) <- posOfAid aid
    return $ PosLevel lid [pa, p]
  ShunD aid p _ _ -> do
    (lid, pa) <- posOfAid aid
    return $ PosLevel lid [pa, p]
  EffectD aid _ -> singleAid aid
  FailureD fid _ -> return $ PosOnly fid  -- failures are secret
  BroadcastD _ -> return $ PosAll
  DisplayPushD fid -> return $ PosOnly fid
  DisplayDelayD fid -> return $ PosOnly fid
  FlushFramesD fid -> return $ PosOnly fid
  FadeoutD fid _ -> return $ PosOnly fid
  FadeinD fid _ -> return $ PosOnly fid

singleAid :: MonadActionRO m => ActorId -> m PosAtomic
singleAid aid = do
  (lid, p) <- posOfAid aid
  return $ PosLevel lid [p]

singleContainer :: MonadActionRO m => Container -> m PosAtomic
singleContainer c = do
  (lid, p) <- posOfContainer c
  return $ PosLevel lid [p]

-- Determines is a command resets FOV. @Nothing@ means it always does.
-- A list of faction means it does for each of the factions.
-- This is only an optimization to save perception and spot/lose computation.
--
-- Invariant: if @resetsFovAtomic@ determines a faction does not need
-- to reset Fov, perception (@perActor@ to be precise, @psmell@ is irrelevant)
-- of that faction does not change upon recomputation. Otherwise,
-- save/restore would change game state.
resetsFovAtomic :: MonadActionRO m => CmdAtomic -> m (Maybe [FactionId])
resetsFovAtomic cmd = case cmd of
  CreateActorA _ body _ -> return $ Just [bfaction body]
  DestroyActorA _ body _ -> return $ Just [bfaction body]
  SpotActorA _ body _ -> return $ Just [bfaction body]
  LoseActorA _ body _ -> return $ Just [bfaction body]
  CreateItemA _ _ _ _ -> return $ Just []  -- unless shines
  DestroyItemA _ _ _ _ -> return $ Just []  -- ditto
  MoveActorA aid _ _ -> fmap Just $ fidOfAid aid  -- assumption: has no light
-- TODO: MoveActorCarryingLIghtA _ _ _ -> return Nothing
  DisplaceActorA source target -> do
    sfid <- fidOfAid source
    tfid <- fidOfAid target
    if source == target
      then return $ Just []
      else return $ Just $ sfid ++ tfid
  DominateActorA _ fromFid toFid -> return $ Just [fromFid, toFid]
  MoveItemA _ _ _ _ -> return $ Just []  -- unless shiny
  AlterTileA _ _ _ _ -> return Nothing  -- even if pos not visible initially
  _ -> return $ Just []

fidOfAid :: MonadActionRO m => ActorId -> m [FactionId]
fidOfAid aid = getsState $ (: []) . bfaction . getActorBody aid

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
breakCmdAtomic :: MonadActionRO m => CmdAtomic -> m [CmdAtomic]
breakCmdAtomic cmd = case cmd of
  MoveActorA aid _ toP -> do
    b <- getsState $ getActorBody aid
    ais <- getsState $ getActorItem aid
    return [LoseActorA aid b ais, SpotActorA aid b {bpos = toP} ais]
  DisplaceActorA source target -> do
    sb <- getsState $ getActorBody source
    sais <- getsState $ getActorItem source
    tb <- getsState $ getActorBody target
    tais <- getsState $ getActorItem target
    return [ LoseActorA source sb sais
           , SpotActorA source sb {bpos = bpos tb} sais
           , LoseActorA target tb tais
           , SpotActorA target tb {bpos = bpos sb} tais
           ]
  MoveItemA iid k c1 c2 -> do
    item <- getsState $ getItemBody iid
    return [LoseItemA iid item k c1, SpotItemA iid item k c2]
  _ -> return [cmd]

loudCmdAtomic :: FactionId -> CmdAtomic -> Bool
loudCmdAtomic fid cmd = case cmd of
  DestroyActorA _ body _ ->
    -- Death of a party member does not need to be heard, because it's seen.
    not $ fid == bfaction body || bproj body
  AlterTileA{} -> True
  _ -> False
