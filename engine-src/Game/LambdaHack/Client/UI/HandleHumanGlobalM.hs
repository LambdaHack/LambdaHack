{-# LANGUAGE TupleSections #-}
-- | Semantics of "Game.LambdaHack.Client.UI.HumanCmd"
-- client commands that return server requests.
-- A couple of them do not take time, the rest does.
-- Here prompts and menus are displayed, but any feedback resulting
-- from the commands (e.g., from inventory manipulation) is generated later on,
-- by the server, for all clients that witness the results of the commands.
module Game.LambdaHack.Client.UI.HandleHumanGlobalM
  ( -- * Meta commands
    byAreaHuman, byAimModeHuman
  , composeIfLocalHuman, composeUnlessErrorHuman, compose2ndLocalHuman
  , loopOnNothingHuman, executeIfClearHuman
    -- * Global commands that usually take time
  , waitHuman, waitHuman10, yellHuman, moveRunHuman
  , runOnceAheadHuman, moveOnceToXhairHuman
  , runOnceToXhairHuman, continueToXhairHuman
  , moveItemHuman, projectHuman, applyHuman
  , alterDirHuman, alterWithPointerHuman
  , helpHuman, hintHuman, dashboardHuman, itemMenuHuman, chooseItemMenuHuman
  , mainMenuHuman, mainMenuAutoOnHuman, mainMenuAutoOffHuman
  , settingsMenuHuman, challengesMenuHuman
  , gameScenarioIncr, gameDifficultyIncr, gameWolfToggle, gameFishToggle
    -- * Global commands that never take time
  , gameRestartHuman, gameQuitHuman, gameDropHuman, gameExitHuman, gameSaveHuman
  , doctrineHuman, automateHuman, automateToggleHuman, automateBackHuman
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , areaToRectangles, meleeAid, displaceAid, moveSearchAlter, goToXhair
  , multiActorGoTo, moveOrSelectItem, selectItemsToMove, moveItems, projectItem
  , applyItem, alterTile, alterTileAtPos, verifyAlters, verifyEscape, guessAlter
  , getVersionBlurb, generateMenu, nxtGameMode
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

-- Cabal
import qualified Paths_LambdaHack as Self (version)

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Version
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Client.Bfs
import           Game.LambdaHack.Client.BfsM
import           Game.LambdaHack.Client.CommonM
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.Request
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.ActorUI
import           Game.LambdaHack.Client.UI.Content.Input
import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.FrameM
import           Game.LambdaHack.Client.UI.Frontend (frontendName)
import           Game.LambdaHack.Client.UI.HandleHelperM
import           Game.LambdaHack.Client.UI.HandleHumanLocalM
import           Game.LambdaHack.Client.UI.HumanCmd
import           Game.LambdaHack.Client.UI.InventoryM
import           Game.LambdaHack.Client.UI.ItemDescription
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.KeyBindings
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.MsgM
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.RunM
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.Slideshow
import           Game.LambdaHack.Client.UI.SlideshowM
import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Area
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.ReqFailure
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Content.RuleKind
import           Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Content.TileKind as TK
import           Game.LambdaHack.Core.Random
import qualified Game.LambdaHack.Definition.Ability as Ability
import qualified Game.LambdaHack.Definition.Color as Color
import           Game.LambdaHack.Definition.Defs

-- * ByArea

-- | Pick command depending on area the mouse pointer is in.
-- The first matching area is chosen. If none match, only interrupt.
byAreaHuman :: MonadClientUI m
            => (HumanCmd -> m (Either MError ReqUI))
            -> [(CmdArea, HumanCmd)]
            -> m (Either MError ReqUI)
byAreaHuman cmdAction l = do
  K.PointUI x y <- getsSession spointer
  let (px, py) = (x `div` 2, y)
      pointerInArea a = do
        rs <- areaToRectangles a
        return $! any (inside $ Point {..}) $ catMaybes rs
  cmds <- filterM (pointerInArea . fst) l
  case cmds of
    [] -> do
      stopPlayBack
      return $ Left Nothing
    (_, cmd) : _ ->
      cmdAction cmd

-- Many values here are shared with "Game.LambdaHack.Client.UI.DrawM".
areaToRectangles :: MonadClientUI m => CmdArea -> m [Maybe Area]
areaToRectangles ca = map toArea <$> do
 CCUI{coscreen=ScreenContent{rwidth, rheight}} <- getsSession sccui
 case ca of
  CaMessage -> return [(0, 0, rwidth - 1, 0)]
  CaMapLeader -> do  -- takes preference over @CaMapParty@ and @CaMap@
    leader <- getLeaderUI
    b <- getsState $ getActorBody leader
    let Point{..} = bpos b
    return [(px, K.mapStartY + py, px, K.mapStartY + py)]
  CaMapParty -> do  -- takes preference over @CaMap@
    lidV <- viewedLevelUI
    side <- getsClient sside
    ours <- getsState $ filter (not . bproj) . map snd
                        . actorAssocs (== side) lidV
    let rectFromB Point{..} = (px, K.mapStartY + py, px, K.mapStartY + py)
    return $! map (rectFromB . bpos) ours
  CaMap -> return
    [( 0, K.mapStartY, rwidth - 1, K.mapStartY + rheight - 4 )]
  CaLevelNumber -> let y = rheight - 2
                   in return [(0, y, 1, y)]
  CaArenaName -> let y = rheight - 2
                     x = (rwidth - 1) `div` 2 - 11
                 in return [(3, y, x, y)]
  CaPercentSeen -> let y = rheight - 2
                       x = (rwidth - 1) `div` 2
                   in return [(x - 9, y, x, y)]
  CaXhairDesc -> let y = rheight - 2
                     x = (rwidth - 1) `div` 2 + 2
                 in return [(x, y, rwidth - 1, y)]
  CaSelected -> let y = rheight - 1
                    x = (rwidth - 1) `div` 2
                in return [(0, y, x - 24, y)]
  CaCalmGauge -> let y = rheight - 1
                     x = (rwidth - 1) `div` 2
                 in return [(x - 22, y, x - 18, y)]
  CaCalmValue -> let y = rheight - 1
                     x = (rwidth - 1) `div` 2
                 in return [(x - 17, y, x - 11, y)]
  CaHPGauge -> let y = rheight - 1
                   x = (rwidth - 1) `div` 2
               in return [(x - 9, y, x - 6, y)]
  CaHPValue -> let y = rheight - 1
                   x = (rwidth - 1) `div` 2
               in return [(x - 6, y, x, y)]
  CaLeaderDesc -> let y = rheight - 1
                      x = (rwidth - 1) `div` 2 + 2
                  in return [(x, y, rwidth - 1, y)]

-- * ByAimMode

byAimModeHuman :: MonadClientUI m
               => m (Either MError ReqUI) -> m (Either MError ReqUI)
               -> m (Either MError ReqUI)
byAimModeHuman cmdNotAimingM cmdAimingM = do
  aimMode <- getsSession saimMode
  if isNothing aimMode then cmdNotAimingM else cmdAimingM

-- * ComposeIfLocal

composeIfLocalHuman :: MonadClientUI m
                    => m (Either MError ReqUI) -> m (Either MError ReqUI)
                    -> m (Either MError ReqUI)
composeIfLocalHuman c1 c2 = do
  slideOrCmd1 <- c1
  case slideOrCmd1 of
    Left merr1 -> do
      slideOrCmd2 <- c2
      case slideOrCmd2 of
        Left merr2 -> return $ Left $ mergeMError merr1 merr2
        _ -> return slideOrCmd2
    _ -> return slideOrCmd1

-- * ComposeUnlessError

composeUnlessErrorHuman :: MonadClientUI m
                        => m (Either MError ReqUI) -> m (Either MError ReqUI)
                        -> m (Either MError ReqUI)
composeUnlessErrorHuman c1 c2 = do
  slideOrCmd1 <- c1
  case slideOrCmd1 of
    Left Nothing -> c2
    _ -> return slideOrCmd1

-- * Compose2ndLocal

compose2ndLocalHuman :: MonadClientUI m
                     => m (Either MError ReqUI) -> m (Either MError ReqUI)
                     -> m (Either MError ReqUI)
compose2ndLocalHuman c1 c2 = do
  slideOrCmd1 <- c1
  case slideOrCmd1 of
    Left merr1 -> do
      slideOrCmd2 <- c2
      case slideOrCmd2 of
        Left merr2 -> return $ Left $ mergeMError merr1 merr2
        _ -> return slideOrCmd1  -- ignore second request, keep effect
    req -> do
      void c2  -- ignore second request, keep effect
      return req

-- * LoopOnNothing

loopOnNothingHuman :: MonadClientUI m
                   => m (Either MError ReqUI)
                   -> m (Either MError ReqUI)
loopOnNothingHuman cmd = do
  res <- cmd
  case res of
    Left Nothing -> loopOnNothingHuman cmd
    _ -> return res

-- * ExecuteIfClear

executeIfClearHuman :: MonadClientUI m
                    => m (Either MError ReqUI)
                    -> m (Either MError ReqUI)
executeIfClearHuman c1 = do
  sreportNull <- getsSession sreportNull
  if sreportNull then c1 else return $ Left Nothing

-- * Wait

-- | Leader waits a turn (and blocks, etc.).
waitHuman :: MonadClientUI m => m (FailOrCmd RequestTimed)
waitHuman = do
  actorSk <- leaderSkillsClientUI
  if Ability.getSk Ability.SkWait actorSk > 0 then do
    modifySession $ \sess -> sess {swaitTimes = abs (swaitTimes sess) + 1}
    return $ Right ReqWait
  else failSer WaitUnskilled

-- * Wait10

-- | Leader waits a 1/10th of a turn (and doesn't block, etc.).
waitHuman10 :: MonadClientUI m => m (FailOrCmd RequestTimed)
waitHuman10 = do
  actorSk <- leaderSkillsClientUI
  if Ability.getSk Ability.SkWait actorSk >= 4 then do
    modifySession $ \sess -> sess {swaitTimes = abs (swaitTimes sess) + 1}
    return $ Right ReqWait10
  else failSer WaitUnskilled

-- * Yell

-- | Leader yells or yawns, if sleeping.
yellHuman :: MonadClientUI m => m (FailOrCmd RequestTimed)
yellHuman = do
  actorSk <- leaderSkillsClientUI
  if Ability.getSk Ability.SkWait actorSk > 0
     -- If waiting drained and really, potentially, no other possible action,
     -- still allow yelling.
     || Ability.getSk Ability.SkMove actorSk <= 0
     || Ability.getSk Ability.SkDisplace actorSk <= 0
     || Ability.getSk Ability.SkMelee actorSk <= 0
  then return $ Right ReqYell
  else failSer WaitUnskilled

-- * MoveDir and RunDir

moveRunHuman :: (MonadClient m, MonadClientUI m)
             => Bool -> Bool -> Bool -> Bool -> Vector
             -> m (FailOrCmd RequestTimed)
moveRunHuman initialStep finalGoal run runAhead dir = do
  actorSk <- leaderSkillsClientUI
  arena <- getArenaUI
  leader <- getLeaderUI
  sb <- getsState $ getActorBody leader
  fact <- getsState $ (EM.! bfid sb) . sfactionD
  -- Start running in the given direction. The first turn of running
  -- succeeds much more often than subsequent turns, because we ignore
  -- most of the disturbances, since the player is mostly aware of them
  -- and still explicitly requests a run, knowing how it behaves.
  sel <- getsSession sselected
  let runMembers = if runAhead || noRunWithMulti fact
                   then [leader]
                   else ES.toList (ES.delete leader sel) ++ [leader]
      runParams = RunParams { runLeader = leader
                            , runMembers
                            , runInitial = True
                            , runStopMsg = Nothing
                            , runWaiting = 0 }
      macroRun25 = ["C-comma", "C-V"]
  when (initialStep && run) $ do
    modifySession $ \cli ->
      cli {srunning = Just runParams}
    when runAhead $
      modifySession $ \cli ->
        cli {slastPlay = map K.mkKM macroRun25 ++ slastPlay cli}
  -- When running, the invisible actor is hit (not displaced!),
  -- so that running in the presence of roving invisible
  -- actors is equivalent to moving (with visible actors
  -- this is not a problem, since runnning stops early enough).
  let tpos = bpos sb `shift` dir
  -- We start by checking actors at the target position,
  -- which gives a partial information (actors can be invisible),
  -- as opposed to accessibility (and items) which are always accurate
  -- (tiles can't be invisible).
  tgts <- getsState $ posToAidAssocs tpos arena
  case tgts of
    [] -> do  -- move or search or alter
      runStopOrCmd <- moveSearchAlter run dir
      case runStopOrCmd of
        Left stopMsg -> return $ Left stopMsg
        Right runCmd ->
          -- Don't check @initialStep@ and @finalGoal@
          -- and don't stop going to target: door opening is mundane enough.
          return $ Right runCmd
    [(target, _)] | run
                    && initialStep
                    && Ability.getSk Ability.SkDisplace actorSk > 0 ->
      -- No @stopPlayBack@: initial displace is benign enough.
      -- Displacing requires accessibility, but it's checked later on.
      displaceAid target
    _ : _ : _ | run
                && initialStep
                && Ability.getSk Ability.SkDisplace actorSk > 0 ->
      failSer DisplaceMultiple
    (target, tb) : _ | not run
                       && initialStep && finalGoal
                       && bfid tb == bfid sb && not (bproj tb) -> do
      stopPlayBack  -- don't ever auto-repeat leader choice
      -- We always see actors from our own faction.
      -- Select one of adjacent actors by bumping into him. Takes no time.
      success <- pickLeader True target
      let !_A = assert (success `blame` "bump self"
                                `swith` (leader, target, tb)) ()
      failWith "by bumping"
    (target, tb) : _ | not run
                       && initialStep && finalGoal
                       && (bfid tb /= bfid sb || bproj tb)
                       && Ability.getSk Ability.SkMelee actorSk > 0 -> do
      stopPlayBack  -- don't ever auto-repeat melee
      -- No problem if there are many projectiles at the spot. We just
      -- attack the first one.
      meleeAid target
    _ : _ -> failWith "actor in the way"

-- | Actor attacks an enemy actor or his own projectile.
meleeAid :: (MonadClient m, MonadClientUI m)
         => ActorId -> m (FailOrCmd RequestTimed)
meleeAid target = do
  leader <- getLeaderUI
  sb <- getsState $ getActorBody leader
  tb <- getsState $ getActorBody target
  sfact <- getsState $ (EM.! bfid sb) . sfactionD
  mel <- pickWeaponClient leader target
  case mel of
    Nothing -> failWith "nothing to melee with"
    Just wp -> do
      let returnCmd = do
            -- Set personal target to enemy, so that AI, if it takes over
            -- the actor, is likely to continue the fight even if the foe flees.
            modifyClient $ updateTarget leader $ const $ Just $ TEnemy target
            -- Also set xhair to see the foe's HP, because it's automatically
            -- set to any new spotted actor, so it needs to be reset
            -- and also it's not useful as permanent ranged target anyway.
            modifySession $ \sess -> sess {sxhair = Just $ TEnemy target}
            return $ Right wp
          res | bproj tb || isFoe (bfid sb) sfact (bfid tb) = returnCmd
              | isFriend (bfid sb) sfact (bfid tb) = do
                let !_A = assert (bfid sb /= bfid tb) ()
                go1 <- displayYesNo ColorBW
                         "You are bound by an alliance. Really attack?"
                if not go1 then failWith "attack canceled" else returnCmd
              | otherwise = do
                go2 <- displayYesNo ColorBW
                         "This attack will start a war. Are you sure?"
                if not go2 then failWith "attack canceled" else returnCmd
      res
  -- Seeing the actor prevents altering a tile under it, but that
  -- does not limit the player, he just doesn't waste a turn
  -- on a failed altering.

-- | Actor swaps position with another.
displaceAid :: MonadClientUI m
            => ActorId -> m (FailOrCmd RequestTimed)
displaceAid target = do
  COps{coTileSpeedup} <- getsState scops
  leader <- getLeaderUI
  sb <- getsState $ getActorBody leader
  tb <- getsState $ getActorBody target
  let dozes = bwatch tb `elem` [WSleep, WWake]
  tfact <- getsState $ (EM.! bfid tb) . sfactionD
  actorMaxSk <- getsState $ getActorMaxSkills target
  dEnemy <- getsState $ dispEnemy leader target actorMaxSk
  let immobile = Ability.getSk Ability.SkMove actorMaxSk <= 0
      tpos = bpos tb
      adj = checkAdjacent sb tb
      atWar = isFoe (bfid tb) tfact (bfid sb)
  if | not adj -> failSer DisplaceDistant
     | not (bproj tb) && atWar
       && actorDying tb ->
       failSer DisplaceDying
     | not (bproj tb) && atWar
       && actorWaits tb ->
       failSer DisplaceBraced
     | not (bproj tb) && atWar
       && immobile && not dozes ->  -- roots weak if the tree sleeps
       failSer DisplaceImmobile
     | not dEnemy && atWar ->
       failSer DisplaceSupported
     | otherwise -> do
       let lid = blid sb
       lvl <- getLevel lid
       -- Displacing requires full access.
       if Tile.isWalkable coTileSpeedup $ lvl `at` tpos then
         case posToAidsLvl tpos lvl of
           [] -> error $ "" `showFailure` (leader, sb, target, tb)
           [_] -> return $ Right $ ReqDisplace target
           _ -> failSer DisplaceMultiple
       else failSer DisplaceAccess

-- | Leader moves or searches or alters. No visible actor at the position.
moveSearchAlter :: MonadClientUI m
                => Bool -> Vector -> m (FailOrCmd RequestTimed)
moveSearchAlter run dir = do
  COps{cotile, coTileSpeedup} <- getsState scops
  actorSk <- leaderSkillsClientUI
  leader <- getLeaderUI
  sb <- getsState $ getActorBody leader
  actorMaxSk <- getsState $ getActorMaxSkills leader
  let calmE = calmEnough sb actorMaxSk
      moveSkill = Ability.getSk Ability.SkMove actorSk
      alterSkill = Ability.getSk Ability.SkAlter actorSk
      applySkill = Ability.getSk Ability.SkApply actorSk
      spos = bpos sb           -- source position
      tpos = spos `shift` dir  -- target position
  itemToF <- getsState $ flip itemToFull
  localTime <- getsState $ getLocalTime (blid sb)
  embeds <- getsState $ getEmbedBag (blid sb) tpos
  lvl <- getLevel $ blid sb
  blurb <- lookAtPosition (blid sb) tpos
  let t = lvl `at` tpos
      alterMinSkill = Tile.alterMinSkill coTileSpeedup t
      canApplyEmbeds = any canApplyEmbed $ EM.assocs embeds
      canApplyEmbed (iid, kit) =
        let itemFull = itemToF iid
            legal = permittedApply localTime applySkill calmE itemFull kit
        -- Let even completely unskilled actors trigger basic embeds.
        in either (const False) (const True) legal
      alterable = Tile.isModifiable coTileSpeedup t || not (EM.null embeds)
      underFeet = tpos == spos  -- if enter and alter, be more permissive
  runStopOrCmd <-
    if -- Movement requires full access.
       | Tile.isWalkable coTileSpeedup t ->
           if moveSkill > 0 then
             -- A potential invisible actor is hit. War started without asking.
             return $ Right $ ReqMove dir
           else failSer MoveUnskilled
       -- Not walkable, so search and/or alter the tile.
       | run -> do
           -- Explicit request to examine the terrain.
           promptAdd0 blurb
           failWith $ if alterable
                      then "potentially alterable"
                      else "not alterable"
       | not alterable -> do
           let name = MU.Text $ TK.tname $ okind cotile t
           failWith $ makePhrase ["there is no point kicking", MU.AW name]
             -- misclick? related to AlterNothing but no searching possible;
             -- we don't show tile description, because it only comes from
             -- embedded items and here probably there are none (can be all
             -- charging, but that's rare)
       | not underFeet && alterSkill <= 1 -> failSer AlterUnskilled
       | not (Tile.isSuspect coTileSpeedup t)
         && not underFeet
         && alterSkill < alterMinSkill -> do
           -- Rather rare (requires high skill), so describe the tile.
           promptAdd0 blurb
           failSer AlterUnwalked
       | not $ Tile.isModifiable coTileSpeedup t || canApplyEmbeds -> do
           -- Rather rare (charging embeds or too low skill for embeds
           -- that are, e.g., `?`), so describe the tile.
           -- Unfortunately this includes cases when an actor can exploit
           -- signboard when hidden, but can't later on when revealed.
           promptAdd0 blurb
           failWith "unable to exploit the terrain"
       | EM.member tpos $ lfloor lvl -> failSer AlterBlockItem
       | occupiedBigLvl tpos lvl || occupiedProjLvl tpos lvl ->
           -- Don't mislead describing terrain, if other actor is to blame.
           failSer AlterBlockActor
       | otherwise -> do  -- promising
           verAlters <- verifyAlters (blid sb) tpos
           case verAlters of
             Right () -> return $ Right $ ReqAlter tpos
             Left err -> return $ Left err
           -- We don't use ReqMove, because we don't hit invisible actors,
           -- e.g., hidden in a wall. If server performed an attack for free
           -- on the invisible actor anyway, the player (or AI)
           -- would be tempted to repeatedly hit random walls
           -- in hopes of killing a monster residing within.
           -- If the action had a cost, misclicks would incur the cost, too.
           -- Right now the player may repeatedly alter tiles trying to learn
           -- about invisible pass-wall actors, but when an actor detected,
           -- it costs a turn and does not harm the invisible actors,
           -- so it's not so tempting.
  return $! runStopOrCmd

-- * RunOnceAhead

runOnceAheadHuman :: MonadClientUI m => m (Either MError RequestTimed)
runOnceAheadHuman = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  leader <- getLeaderUI
  keyPressed <- anyKeyPressed
  srunning <- getsSession srunning
  -- When running, stop if disturbed. If not running, stop at once.
  case srunning of
    Nothing -> do
      stopPlayBack
      return $ Left Nothing
    Just RunParams{runMembers}
      | noRunWithMulti fact && runMembers /= [leader] -> do
      stopPlayBack
      msgAdd MsgRunStop "run stop: automatic pointman change"
      return $ Left Nothing
    Just _runParams | keyPressed -> do
      discardPressedKey
      stopPlayBack
      msgAdd MsgRunStop "run stop: key pressed"
      weaveJust <$> failWith "interrupted"
    Just runParams -> do
      arena <- getArenaUI
      runOutcome <- continueRun arena runParams
      case runOutcome of
        Left stopMsg -> do
          stopPlayBack
          msgAdd MsgRunStop ("run stop:" <+> stopMsg)
          return $ Left Nothing
        Right runCmd ->
          return $ Right runCmd

-- * MoveOnceToXhair

moveOnceToXhairHuman :: (MonadClient m, MonadClientUI m)
                     => m (FailOrCmd RequestTimed)
moveOnceToXhairHuman = goToXhair True False

goToXhair :: (MonadClient m, MonadClientUI m)
          => Bool -> Bool -> m (FailOrCmd RequestTimed)
goToXhair initialStep run = do
  aimMode <- getsSession saimMode
  -- Movement is legal only outside aiming mode.
  if isJust aimMode then failWith "cannot move in aiming mode"
  else do
    leader <- getLeaderUI
    b <- getsState $ getActorBody leader
    xhairPos <- xhairToPos
    case xhairPos of
      Nothing -> failWith "crosshair position invalid"
      Just c | c == bpos b -> failWith "position reached"
      Just c -> do
        running <- getsSession srunning
        case running of
          -- Don't use running params from previous run or goto-xhair.
          Just paramOld | not initialStep -> do
            arena <- getArenaUI
            runOutcome <- multiActorGoTo arena c paramOld
            case runOutcome of
              Left stopMsg -> return $ Left stopMsg
              Right (finalGoal, dir) ->
                moveRunHuman initialStep finalGoal run False dir
          _ -> do
            let !_A = assert (initialStep || not run) ()
            (bfs, mpath) <- getCacheBfsAndPath leader c
            xhairMoused <- getsSession sxhairMoused
            case mpath of
              _ | xhairMoused && isNothing (accessBfs bfs c) ->
                failWith
                  "no route to crosshair (press again to go there anyway)"
              _ | initialStep && adjacent (bpos b) c -> do
                let dir = towards (bpos b) c
                moveRunHuman initialStep True run False dir
              Nothing -> failWith "no route to crosshair"
              Just AndPath{pathList=[]} -> failWith "almost there"
              Just AndPath{pathList = p1 : _} -> do
                let finalGoal = p1 == c
                    dir = towards (bpos b) p1
                moveRunHuman initialStep finalGoal run False dir

multiActorGoTo :: (MonadClient m, MonadClientUI m)
               => LevelId -> Point -> RunParams -> m (FailOrCmd (Bool, Vector))
multiActorGoTo arena c paramOld =
  case paramOld of
    RunParams{runMembers = []} -> failWith "selected actors no longer there"
    RunParams{runMembers = r : rs, runWaiting} -> do
      onLevel <- getsState $ memActor r arena
      if not onLevel then do
        let paramNew = paramOld {runMembers = rs}
        multiActorGoTo arena c paramNew
      else do
        sL <- getState
        modifyClient $ updateLeader r sL
        let runMembersNew = rs ++ [r]
            paramNew = paramOld { runMembers = runMembersNew
                                , runWaiting = 0}
        b <- getsState $ getActorBody r
        (bfs, mpath) <- getCacheBfsAndPath r c
        xhairMoused <- getsSession sxhairMoused
        case mpath of
          _ | xhairMoused && isNothing (accessBfs bfs c) ->
            failWith "no route to crosshair (press again to go there anyway)"
          Nothing -> failWith "no route to crosshair"
          Just AndPath{pathList=[]} -> failWith "almost there"
          Just AndPath{pathList = p1 : _} -> do
            let finalGoal = p1 == c
                dir = towards (bpos b) p1
            tgts <- getsState $ posToAids p1 arena
            case tgts of
              [] -> do
                modifySession $ \sess -> sess {srunning = Just paramNew}
                return $ Right (finalGoal, dir)
              [target] | target `elem` rs || runWaiting <= length rs ->
                -- Let r wait until all others move. Mark it in runWaiting
                -- to avoid cycles. When all wait for each other, fail.
                multiActorGoTo arena c paramNew{runWaiting=runWaiting + 1}
              _ ->
                failWith "actor in the way"

-- * RunOnceToXhair

runOnceToXhairHuman :: (MonadClient m, MonadClientUI m)
                    => m (FailOrCmd RequestTimed)
runOnceToXhairHuman = goToXhair True True

-- * ContinueToXhair

continueToXhairHuman :: (MonadClient m, MonadClientUI m)
                     => m (FailOrCmd RequestTimed)
continueToXhairHuman = goToXhair False False{-irrelevant-}

-- * MoveItem

moveItemHuman :: forall m. MonadClientUI m
              => [CStore] -> CStore -> Maybe MU.Part -> Bool
              -> m (FailOrCmd RequestTimed)
moveItemHuman cLegalRaw destCStore mverb auto = do
  actorSk <- leaderSkillsClientUI
  if Ability.getSk Ability.SkMoveItem actorSk > 0 then
    moveOrSelectItem cLegalRaw destCStore mverb auto
  else failSer MoveItemUnskilled

-- This cannot be structured as projecting or applying, with @ByItemMode@
-- and @ChooseItemToMove@, because at least in case of grabbing items,
-- more than one item is chosen, which doesn't fit @sitemSel@. Separating
-- grabbing of multiple items as a distinct command is too high a price.
moveOrSelectItem :: forall m. MonadClientUI m
                 => [CStore] -> CStore -> Maybe MU.Part -> Bool
                 -> m (FailOrCmd RequestTimed)
moveOrSelectItem cLegalRaw destCStore mverb auto = do
  itemSel <- getsSession sitemSel
  modifySession $ \sess -> sess {sitemSel = Nothing}  -- prevent surprise
  case itemSel of
    Just (iid, fromCStore, _) | fromCStore /= destCStore
                                && fromCStore `elem` cLegalRaw -> do
      leader <- getLeaderUI
      b <- getsState $ getActorBody leader
      bag <- getsState $ getBodyStoreBag b fromCStore
      case iid `EM.lookup` bag of
        Nothing ->  -- the case of old selection or selection from another actor
          moveItemHuman cLegalRaw destCStore mverb auto
        Just (k, it) -> assert (k > 0) $ do
          let eqpFree = eqpFreeN b
              kToPick | destCStore == CEqp = min eqpFree k
                      | otherwise = k
          if kToPick == 0
          then failWith "no more items can be equipped"
          else do
            socK <- pickNumber (not auto) kToPick
            case socK of
              Left Nothing -> moveItemHuman cLegalRaw destCStore mverb auto
              Left (Just err) -> return $ Left err
              Right kChosen ->
                let is = (fromCStore, [(iid, (kChosen, take kChosen it))])
                in moveItems cLegalRaw is destCStore
    _ -> do
      mis <- selectItemsToMove cLegalRaw destCStore mverb auto
      case mis of
        Left err -> return $ Left err
        Right (fromCStore, [(iid, _)]) | cLegalRaw /= [CGround] -> do
          modifySession $ \sess ->
            sess {sitemSel = Just (iid, fromCStore, False)}
          moveItemHuman cLegalRaw destCStore mverb auto
        Right is -> moveItems cLegalRaw is destCStore

selectItemsToMove :: forall m. MonadClientUI m
                  => [CStore] -> CStore -> Maybe MU.Part -> Bool
                  -> m (FailOrCmd (CStore, [(ItemId, ItemQuant)]))
selectItemsToMove cLegalRaw destCStore mverb auto = do
  let !_A = assert (destCStore `notElem` cLegalRaw) ()
  let verb = fromMaybe (MU.Text $ verbCStore destCStore) mverb
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  -- This calmE is outdated when one of the items increases max Calm
  -- (e.g., in pickup, which handles many items at once), but this is OK,
  -- the server accepts item movement based on calm at the start, not end
  -- or in the middle.
  -- The calmE is inaccurate also if an item not IDed, but that's intended
  -- and the server will ignore and warn (and content may avoid that,
  -- e.g., making all rings identified)
  actorMaxSk <- getsState $ getActorMaxSkills leader
  mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid b
  lastItemMove <- getsSession slastItemMove
  let overStash = mstash == Just (blid b, bpos b)
      calmE = calmEnough b actorMaxSk
  if destCStore == CEqp && not calmE then failSer ItemNotCalm
  else if destCStore == CGround && overStash then failSer ItemOverStash
  else do
    let cLegalE = cLegalRaw \\ ([CGround | overStash] ++ [CEqp | not calmE])
        cLegal = case lastItemMove of
          Just (lastFrom, lastDest) | lastDest == destCStore
                                      && lastFrom `elem` cLegalE ->
            lastFrom : delete lastFrom cLegalE
          _ -> cLegalE
        prompt = makePhrase ["What to", verb]
        promptEqp = makePhrase ["What consumable to", verb]
        (promptGeneric, psuit) =
          -- We prune item list only for eqp, because other stores don't have
          -- so clear cut heuristics. So when picking up a stash, either grab
          -- it to auto-store things, or equip first using the pruning
          -- and then pack/stash the rest selectively or en masse.
          if destCStore == CEqp
          then (promptEqp, return $ SuitsSomething $ \itemFull _kit ->
                 IA.goesIntoEqp $ aspectRecordFull itemFull)
          else (prompt, return SuitsEverything)
    ggi <-
      getFull psuit
              (\_ _ _ cCur _ -> prompt <+> ppItemDialogModeFrom cCur)
              (\_ _ _ cCur _ -> promptGeneric <+> ppItemDialogModeFrom cCur)
                   cLegalRaw cLegal (not auto) True
    case ggi of
      Right (l, (MStore fromCStore, _)) -> do
        modifySession $ \sess ->
          sess {slastItemMove = Just (fromCStore, destCStore)}
        return $ Right (fromCStore, l)
      Left err -> failWith err
      _ -> error $ "" `showFailure` ggi

moveItems :: forall m. MonadClientUI m
          => [CStore] -> (CStore, [(ItemId, ItemQuant)]) -> CStore
          -> m (FailOrCmd RequestTimed)
moveItems cLegalRaw (fromCStore, l) destCStore = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorMaxSk <- getsState $ getActorMaxSkills leader
  discoBenefit <- getsClient sdiscoBenefit
  let calmE = calmEnough b actorMaxSk
      ret4 :: [(ItemId, ItemQuant)] -> Int -> m [(ItemId, Int, CStore, CStore)]
      ret4 [] _ = return []
      ret4 ((iid, (k, _)) : rest) oldN = do
        let !_A = assert (k > 0) ()
            retRec toCStore = do
              let n = oldN + if toCStore == CEqp then k else 0
              l4 <- ret4 rest n
              return $ (iid, k, fromCStore, toCStore) : l4
            issueWarning = do
              let fullWarn = if eqpOverfull b (oldN + 1)
                             then EqpOverfull
                             else EqpStackFull
              msgAdd MsgWarning $ "Warning:" <+> showReqFailure fullWarn <> "."
        if cLegalRaw == [CGround] && destCStore == CStash  -- normal pickup
        then -- @CStash@ is the implicit default; refine:
             if | not (benInEqp (discoBenefit EM.! iid) && calmE) ->
                  -- If @CEqp@ is impossible, give up:
                  retRec CStash
                | eqpOverfull b (oldN + k) -> do
                  -- If this stack doesn't fit, we don't equip any part of it,
                  -- but we may equip a smaller stack later of other items
                  -- in the same pickup.
                  issueWarning
                  retRec CStash
                | otherwise ->
                  -- Prefer @CEqp@ if all conditions hold:
                  retRec CEqp
        else case destCStore of  -- player forces store, so @benInEqp@ ignored
          CEqp | eqpOverfull b (oldN + k) -> do
            -- If the chosen number from the stack doesn't fit,
            -- we don't equip any part of it and we exit item manipulation.
            issueWarning
            -- No recursive call here:
            return []
          _ -> retRec destCStore
  if CEqp `elem` [fromCStore, destCStore] && not calmE then failSer ItemNotCalm
  else do
    l4 <- ret4 l 0
    return $! if null l4
              then error $ "" `showFailure` l
              else Right $ ReqMoveItems l4

-- * Project

projectHuman :: (MonadClient m, MonadClientUI m) => m (FailOrCmd RequestTimed)
projectHuman = do
  actorSk <- leaderSkillsClientUI
  if Ability.getSk Ability.SkProject actorSk <= 0 then  -- detailed check later
    failSer ProjectUnskilled
  else do
    itemSel <- getsSession sitemSel
    case itemSel of
      Just (iid, fromCStore, _) -> do
        leader <- getLeaderUI
        b <- getsState $ getActorBody leader
        bag <- getsState $ getBodyStoreBag b fromCStore
        case iid `EM.lookup` bag of
          Nothing -> failWith "no item to fling"
          Just _kit -> do
            itemFull <- getsState $ itemToFull iid
            let i = (fromCStore, (iid, itemFull))
            projectItem i
      Nothing -> failWith "no item to fling"

projectItem :: (MonadClient m, MonadClientUI m)
            => (CStore, (ItemId, ItemFull))
            -> m (FailOrCmd RequestTimed)
projectItem (fromCStore, (iid, itemFull)) = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorMaxSk <- getsState $ getActorMaxSkills leader
  let calmE = calmEnough b actorMaxSk
  if fromCStore == CEqp && not calmE then failSer ItemNotCalm
  else do
    mpsuitReq <- psuitReq
    case mpsuitReq of
      Left err -> failWith err
      Right psuitReqFun ->
        case psuitReqFun itemFull of
          Left reqFail -> failSer reqFail
          Right (pos, _) -> do
            Benefit{benFling} <- getsClient $ (EM.! iid) . sdiscoBenefit
            go <- if benFling > 0
                  then displayYesNo ColorFull
                         "The item appears beneficial. Do you really want to fling it?"
                  else return True
            if go then do
              -- Set personal target to enemy, so that AI, if it takes over
              -- the actor, is likely to continue the fight even if the foe
              -- flees. Similarly if the crosshair points at position, etc.
              sxhair <- getsSession sxhair
              modifyClient $ updateTarget leader (const sxhair)
              -- Project.
              eps <- getsClient seps
              return $ Right $ ReqProject pos eps iid fromCStore
            else do
              modifySession $ \sess -> sess {sitemSel = Nothing}
              failWith "never mind"

-- * Apply

applyHuman :: MonadClientUI m => m (FailOrCmd RequestTimed)
applyHuman = do
  actorSk <- leaderSkillsClientUI
  if Ability.getSk Ability.SkApply actorSk <= 0 then  -- detailed check later
    failSer ApplyUnskilled
  else do
    itemSel <- getsSession sitemSel
    case itemSel of
      Just (iid, fromCStore, _) -> do
        leader <- getLeaderUI
        b <- getsState $ getActorBody leader
        bag <- getsState $ getBodyStoreBag b fromCStore
        case iid `EM.lookup` bag of
          Nothing -> failWith "no item to apply"
          Just kit -> do
            itemFull <- getsState $ itemToFull iid
            applyItem (fromCStore, (iid, (itemFull, kit)))
      Nothing -> failWith "no item to apply"

applyItem :: MonadClientUI m
          => (CStore, (ItemId, ItemFullKit))
          -> m (FailOrCmd RequestTimed)
applyItem (fromCStore, (iid, (itemFull, kit))) = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  localTime <- getsState $ getLocalTime (blid b)
  actorMaxSk <- getsState $ getActorMaxSkills leader
  actorSk <- leaderSkillsClientUI
  let skill = Ability.getSk Ability.SkApply actorSk
      calmE = calmEnough b actorMaxSk
      arItem = aspectRecordFull itemFull
  if fromCStore == CEqp && not calmE then failSer ItemNotCalm
  else case permittedApply localTime skill calmE itemFull kit of
    Left reqFail -> failSer reqFail
    Right _ -> do
      Benefit{benApply} <- getsClient $ (EM.! iid) . sdiscoBenefit
      go <-
        if | IA.checkFlag Ability.Periodic arItem
             && not (IA.checkFlag Ability.Durable arItem) ->
             -- No warning if item durable, because activation weak,
             -- but price low, due to no destruction.
             displayYesNo ColorFull
                          "Applying this periodic item will produce only the first of its effects and moreover, because it's not durable, will destroy it. Are you sure?"
           | benApply < 0 ->
             displayYesNo ColorFull
                          "The item appears harmful. Do you really want to apply it?"
           | otherwise -> return True
      if go
      then return $ Right $ ReqApply iid fromCStore
      else do
        modifySession $ \sess -> sess {sitemSel = Nothing}
        failWith "never mind"

-- * AlterDir

-- | Ask for a direction and alter a tile in the specified way, if possible.
alterDirHuman :: MonadClientUI m
              => [TriggerTile] -> m (FailOrCmd RequestTimed)
alterDirHuman ts = do
  UIOptions{uVi, uLeftHand} <- getsSession sUIOptions
  let verb1 = case ts of
        [] -> "alter"
        tr : _ -> ttverb tr
      keys = K.escKM
             : K.leftButtonReleaseKM
             : map (K.KM K.NoModifier) (K.dirAllKey uVi uLeftHand)
      prompt = makePhrase
        ["Where to", verb1 <> "? [movement key] [pointer]"]
  promptAdd0 prompt
  slides <- reportToSlideshow [K.escKM]
  km <- getConfirms ColorFull keys slides
  case K.key km of
    K.LeftButtonRelease -> do
      leader <- getLeaderUI
      b <- getsState $ getActorBody leader
      K.PointUI x y <- getsSession spointer
      let (px, py) = (x `div` 2, y - K.mapStartY)
          dir = Point px py `vectorToFrom` bpos b
      if isUnit dir
      then alterTile ts dir
      else failWith "never mind"
    _ ->
      case K.handleDir uVi uLeftHand km of
        Nothing -> failWith "never mind"
        Just dir -> alterTile ts dir

-- | Try to alter a tile using a feature in the given direction.
alterTile :: MonadClientUI m
          => [TriggerTile] -> Vector -> m (FailOrCmd RequestTimed)
alterTile ts dir = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  let tpos = bpos b `shift` dir
      pText = compassText dir
  alterTileAtPos ts tpos pText

-- | Try to alter a tile using a feature at the given position.
--
-- We don't check if the tile is interesting, e.g., if any embedded
-- item can be triggered, because the player explicitely requested
-- the action. Consequently, even if all embedded items are recharching,
-- the time will be wasted and the server will describe the failure in detail.
alterTileAtPos :: MonadClientUI m
               => [TriggerTile] -> Point -> Text
               -> m (FailOrCmd RequestTimed)
alterTileAtPos ts tpos pText = do
  cops@COps{cotile, coTileSpeedup} <- getsState scops
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorSk <- leaderSkillsClientUI
  lvl <- getLevel $ blid b
  embeds <- getsState $ getEmbedBag (blid b) tpos
  let alterSkill = Ability.getSk Ability.SkAlter actorSk
      t = lvl `at` tpos
      alterMinSkill = Tile.alterMinSkill coTileSpeedup t
      hasFeat TriggerTile{ttfeature} = Tile.hasFeature cotile ttfeature t
  case filter hasFeat ts of
    [] | not $ null ts -> failWith $ guessAlter cops ts t
    _ | not (Tile.isModifiable coTileSpeedup t)
        && EM.null embeds -> failSer AlterNothing
    _ | chessDist tpos (bpos b) > 1 -> failSer AlterDistant
    _ | alterSkill <= 1 -> failSer AlterUnskilled
    _ | not (Tile.isSuspect coTileSpeedup t)
        && alterSkill < alterMinSkill -> failSer AlterUnwalked
    trs ->
      if EM.notMember tpos $ lfloor lvl then
        if not (occupiedBigLvl tpos lvl)
           && not (occupiedProjLvl tpos lvl) then do
          let v = case trs of
                [] -> "alter"
                tr : _ -> ttverb tr
          verAlters <- verifyAlters (blid b) tpos
          case verAlters of
            Right () -> do
              let msg = makeSentence ["you", v, MU.Text pText]
              msgAdd MsgDone msg
              return $ Right $ ReqAlter tpos
            Left err -> return $ Left err
        else failSer AlterBlockActor
      else failSer AlterBlockItem

-- | Verify important effects, such as fleeing the dungeon.
--
-- This is contrived for now, the embedded items are not analyzed,
-- but only recognized by name.
verifyAlters :: MonadClientUI m => LevelId -> Point -> m (FailOrCmd ())
verifyAlters lid p = do
  COps{coTileSpeedup} <- getsState scops
  lvl <- getLevel lid
  let t = lvl `at` p
  bag <- getsState $ getEmbedBag lid p
  getKind <- getsState $ flip getIidKind
  let ks = map getKind $ EM.keys bag
  if | any (any IK.isEffEscape . IK.ieffects) ks -> verifyEscape
     | null ks && not (Tile.isModifiable coTileSpeedup t) ->
         failWith "never mind"
     | otherwise -> return $ Right ()

verifyEscape :: MonadClientUI m => m (FailOrCmd ())
verifyEscape = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  if not (fcanEscape $ gplayer fact)
  then failWith
         "This is the way out, but where would you go in this alien world?"
  else do
    (_, total) <- getsState $ calculateTotal side
    dungeonTotal <- getsState sgold
    let prompt | dungeonTotal == 0 =
                 "You finally reached the way out. Really leave now?"
               | total == 0 =
                 "Afraid of the challenge? Leaving so soon and without any treasure? Are you sure?"
               | total < dungeonTotal =
                 "You finally found the way out, but still more valuables are rumoured to hide around here. Really leave already?"
               | otherwise =
                 "This is the way out and you collected all treasure there is to find. Really leave now?"
    -- The player can back off, but we never insist,
    -- because possibly the score formula doesn't reward treasure
    -- or he is focused on winning only.
    go <- displayYesNo ColorBW prompt
    if not go
    then failWith "here's your chance!"
    else return $ Right ()

-- | Guess and report why the bump command failed.
guessAlter :: COps -> [TriggerTile] -> ContentId TileKind -> Text
guessAlter COps{cotile} (TriggerTile{ttfeature=TK.OpenTo _} : _) t
  | Tile.isClosable cotile t = "already open"
guessAlter _ (TriggerTile{ttfeature=TK.OpenTo _} : _) _ = "cannot be opened"
guessAlter COps{cotile} (TriggerTile{ttfeature=TK.CloseTo _} : _) t
  | Tile.isOpenable cotile t = "already closed"
guessAlter _ (TriggerTile{ttfeature=TK.CloseTo _} : _) _ = "cannot be closed"
guessAlter _ _ _ = "never mind"

-- * AlterWithPointer

-- | Try to alter a tile using a feature under the pointer.
alterWithPointerHuman :: MonadClientUI m
                      => [TriggerTile] -> m (FailOrCmd RequestTimed)
alterWithPointerHuman ts = do
  COps{corule=RuleContent{rXmax, rYmax}, cotile} <- getsState scops
  lidV <- viewedLevelUI
  -- Not @ScreenContent@, because not drawing here.
  lvl <- getLevel lidV
  K.PointUI x y <- getsSession spointer
  let (px, py) = (x `div` 2, y - K.mapStartY)
      tpos = Point px py
      t = lvl `at` tpos
  if px >= 0 && py >= 0 && px < rXmax && py < rYmax
  then alterTileAtPos ts tpos $ "the" <+> TK.tname (okind cotile t)
  else failWith "never mind"

-- * Help

-- | Display command help.
helpHuman :: MonadClientUI m
          => (HumanCmd -> m (Either MError ReqUI))
          -> m (Either MError ReqUI)
helpHuman cmdAction = do
  cops <- getsState scops
  ccui@CCUI{coinput, coscreen=ScreenContent{rwidth, rheight}}
    <- getsSession sccui
  fontSetup@FontSetup{monoFont} <- getFontSetup
  let keyH = keyHelp cops ccui monoFont
      splitHelp (t, okx) =
        splitOKX fontSetup rwidth rheight (textToAL t) [K.spaceKM, K.escKM] okx
      sli = toSlideshow fontSetup $ concat $ map splitHelp keyH
  -- Thus, the whole help menu corresponde to a single menu of item or lore,
  -- e.g., shared stash menu. This is especially clear when the shared stash
  -- menu contains many pages.
  ekm <- displayChoiceScreen "help" ColorFull True sli [K.spaceKM, K.escKM]
  case ekm of
    Left km -> case km `M.lookup` bcmdMap coinput of
      _ | km `elem` [K.escKM, K.spaceKM] -> return $ Left Nothing
      Just (_desc, _cats, cmd) -> cmdAction cmd
      Nothing -> weaveJust <$> failWith "never mind"
    Right _slot -> error $ "" `showFailure` ekm

-- * Hint

-- | Display hint or, if already displayed, display help.
hintHuman :: MonadClientUI m
          => (HumanCmd -> m (Either MError ReqUI))
          -> m (Either MError ReqUI)
hintHuman cmdAction = do
  hintMode <- getsSession shintMode
  if hintMode == HintWiped then
    helpHuman cmdAction
  else do
    modifySession $ \sess -> sess {shintMode = HintShown}
    promptMainKeys
    return $ Left Nothing

-- * Dashboard

-- | Display the dashboard.
dashboardHuman :: MonadClientUI m
               => (HumanCmd -> m (Either MError ReqUI))
               -> m (Either MError ReqUI)
dashboardHuman cmdAction = do
  CCUI{coinput, coscreen=ScreenContent{rwidth, rheight}} <- getsSession sccui
  fontSetup@FontSetup{monoFont} <- getFontSetup
  let keyL = 2
      (ov0, kxs0) = okxsN coinput monoFont 0 keyL (const False) False
                          CmdDashboard [] []
      al1 = textToAL "Dashboard"
  let splitHelp (al, okx) = splitOKX fontSetup rwidth (rheight - 2) al
                                     [K.escKM] okx
      sli = toSlideshow fontSetup $ splitHelp (al1, (ov0, kxs0))
      extraKeys = [K.escKM]
  ekm <- displayChoiceScreen "dashboard" ColorFull False sli extraKeys
  case ekm of
    Left km -> case km `M.lookup` bcmdMap coinput of
      _ | km == K.escKM -> weaveJust <$> failWith "never mind"
      Just (_desc, _cats, cmd) -> cmdAction cmd
      Nothing -> weaveJust <$> failWith "never mind"
    Right _slot -> error $ "" `showFailure` ekm

-- * ItemMenu

itemMenuHuman :: MonadClientUI m
              => (HumanCmd -> m (Either MError ReqUI))
              -> m (Either MError ReqUI)
itemMenuHuman cmdAction = do
  itemSel <- getsSession sitemSel
  fontSetup@FontSetup{..} <- getFontSetup
  case itemSel of
    Just (iid, fromCStore, _) -> do
      leader <- getLeaderUI
      b <- getsState $ getActorBody leader
      bUI <- getsSession $ getActorUI leader
      bag <- getsState $ getBodyStoreBag b fromCStore
      case iid `EM.lookup` bag of
        Nothing -> weaveJust <$> failWith "no item to open item menu for"
        Just kit -> do
          CCUI{coscreen=ScreenContent{rwidth, rheight}} <- getsSession sccui
          actorMaxSk <- getsState $ getActorMaxSkills leader
          itemFull <- getsState $ itemToFull iid
          localTime <- getsState $ getLocalTime (blid b)
          found <- getsState $ findIid leader (bfid b) iid
          factionD <- getsState sfactionD
          jlid <- getsSession $ (EM.! iid) . sitemUI
          let !_A = assert (not (null found) || fromCStore == CGround
                            `blame` (iid, leader)) ()
              fAlt (aid, (_, store)) = aid /= leader || store /= fromCStore
              foundAlt = filter fAlt found
              partRawActor aid = getsSession (partActor . getActorUI aid)
              ppLoc aid store = do
                parts <- ppContainerWownW partRawActor
                                          False
                                          (CActor aid store)
                return $! "[" ++ T.unpack (makePhrase parts) ++ "]"
          foundTexts <- mapM (\(aid, (_, store)) -> ppLoc aid store) foundAlt
          let foundPrefix = textToAL $
                if null foundTexts then "" else "The item is also in:"
              markParagraphs = rheight >= 45
              descAl = itemDesc markParagraphs (bfid b) factionD
                                (Ability.getSk Ability.SkHurtMelee actorMaxSk)
                                fromCStore localTime jlid itemFull kit
              (descSymAl, descBlurbAl) = span (/= Color.spaceAttrW32) descAl
              descSym = offsetOverlay $ splitAttrLine rwidth descSymAl
              descBlurb = offsetOverlayX $
                case splitAttrLine rwidth $ stringToAL "xx" ++ descBlurbAl of
                  [] -> error "splitting AttrLine loses characters"
                  al1 : rest -> (2, drop 2 al1) : map (0,) rest
              alPrefix = map (\(K.PointUI x y, al) ->
                                (K.PointUI x (y + length descBlurb), al))
                         $ offsetOverlay $ splitAttrLine rwidth foundPrefix
              ystart = length descBlurb + length alPrefix - 1
              xstart = length (snd $ last alPrefix) + 1
              foundKeys = map (K.KM K.NoModifier . K.Fun)
                              [1 .. length foundAlt]  -- starting from 1!
          let ks = zip foundKeys foundTexts
              (ovFoundRaw, kxsFound) = wrapOKX monoFont ystart xstart rwidth ks
              ovFound = alPrefix ++ ovFoundRaw
          report <- getReportUI
          CCUI{coinput} <- getsSession sccui
          actorSk <- leaderSkillsClientUI
          let calmE = calmEnough b actorMaxSk
              greyedOut cmd = not calmE && fromCStore == CEqp || case cmd of
                ByAimMode AimModeCmd{..} ->
                  greyedOut exploration || greyedOut aiming
                ComposeIfLocal cmd1 cmd2 -> greyedOut cmd1 || greyedOut cmd2
                ComposeUnlessError cmd1 cmd2 -> greyedOut cmd1 || greyedOut cmd2
                Compose2ndLocal cmd1 cmd2 -> greyedOut cmd1 || greyedOut cmd2
                MoveItem stores destCStore _ _ ->
                  fromCStore `notElem` stores
                  || destCStore == CEqp && (not calmE || eqpOverfull b 1)
                Apply{} ->
                  let skill = Ability.getSk Ability.SkApply actorSk
                  in not $ either (const False) id
                     $ permittedApply localTime skill calmE itemFull kit
                Project{} ->
                  let skill = Ability.getSk Ability.SkProject actorSk
                  in not $ either (const False) id
                     $ permittedProject False skill calmE itemFull
                _ -> False
              fmt n k h = " " <> T.justifyLeft n ' ' k <+> h
              keyL = 11
              keyCaption = fmt keyL "keys" "command"
              offset = 1 + maxYofOverlay (descBlurb ++ ovFound)
              (ov0, kxs0) = okxsN coinput monoFont offset keyL greyedOut True
                                  CmdItemMenu [keyCaption] []
              t0 = makeSentence [ MU.SubjectVerbSg (partActor bUI) "choose"
                                , "an item", MU.Text $ ppCStoreIn fromCStore ]
              al1 = renderReport report <+:> textToAL t0
              splitHelp (al, okx) = splitOKX fontSetup rwidth (rheight - 2) al
                                             [K.spaceKM, K.escKM] okx
              sli = toSlideshow fontSetup
                    $ splitHelp ( al1
                                , ( EM.insertWith (++) squareFont descSym
                                    $ EM.insertWith (++) propFont descBlurb
                                    $ EM.insertWith (++) monoFont ovFound ov0
                                      -- mono font, because there are buttons
                                  , kxsFound ++ kxs0 ))
              extraKeys = [K.spaceKM, K.escKM] ++ foundKeys
          recordHistory  -- report shown (e.g., leader switch), save to history
          ekm <- displayChoiceScreen "item menu" ColorFull False sli extraKeys
          case ekm of
            Left km -> case km `M.lookup` bcmdMap coinput of
              _ | km == K.escKM -> weaveJust <$> failWith "never mind"
              _ | km == K.spaceKM -> return $ Left Nothing
              _ | km `elem` foundKeys -> case km of
                K.KM{key=K.Fun n} -> do
                  let (newAid, (bNew, newCStore)) = foundAlt !! (n - 1)
                  fact <- getsState $ (EM.! bfid bNew) . sfactionD
                  let (autoDun, _) = autoDungeonLevel fact
                  if | blid bNew /= blid b && autoDun ->
                       weaveJust <$> failSer NoChangeDunLeader
                     | otherwise -> do
                       void $ pickLeader True newAid
                       modifySession $ \sess ->
                         sess {sitemSel = Just (iid, newCStore, False)}
                       itemMenuHuman cmdAction
                _ -> error $ "" `showFailure` km
              Just (_desc, _cats, cmd) -> do
                modifySession $ \sess ->
                  sess {sitemSel = Just (iid, fromCStore, True)}
                res <- cmdAction cmd
                modifySession $ \sess ->
                  sess {sitemSel = Just (iid, fromCStore, False)}
                return res
              Nothing -> weaveJust <$> failWith "never mind"
            Right _slot -> error $ "" `showFailure` ekm
    Nothing -> weaveJust <$> failWith "no item to open item menu for"

-- * ChooseItemMenu

chooseItemMenuHuman :: MonadClientUI m
                    => (HumanCmd -> m (Either MError ReqUI))
                    -> ItemDialogMode
                    -> m (Either MError ReqUI)
chooseItemMenuHuman cmdAction c = do
  res <- chooseItemDialogMode c
  case res of
    Right c2 -> do
      res2 <- itemMenuHuman cmdAction
      case res2 of
        Left Nothing -> chooseItemMenuHuman cmdAction c2
        _ -> return res2
    Left err -> return $ Left $ Just err

-- * MainMenu

getVersionBlurb :: MonadClientUI m => m String
getVersionBlurb = do
  COps{corule} <- getsState scops
  let exeVersion = rexeVersion corule
      libVersion = Self.version
  return $! " Version " ++ showVersion exeVersion
            ++ " (frontend: " ++ frontendName
            ++ ", engine: LambdaHack " ++ showVersion libVersion
            ++ ") "

generateMenu :: MonadClientUI m
             => (HumanCmd -> m (Either MError ReqUI))
             -> [(K.KM, (Text, HumanCmd))] -> [String] -> String
             -> m (Either MError ReqUI)
generateMenu cmdAction kds gameInfo menuName = do
  CCUI{coscreen=ScreenContent{rwidth, rheight, rmainMenuArt}} <-
    getsSession sccui
  FontSetup{..} <- getFontSetup
  versionBlurb <- getVersionBlurb
  let offset = 2
      bindings =  -- key bindings to display
        let fmt (k, (d, _)) =
              ( Just k
              , T.unpack
                $  " " <> T.justifyLeft 4 ' ' (T.pack $ K.showKM k)
                   <> " " <> d )
        in map fmt kds
      generate :: Int -> (Maybe K.KM, String) -> ((Int, AttrLine), Maybe KYX)
      generate y (mkey, binding) =
        let lenB = length binding
            yxx key = (Left [key], (K.PointUI offset y, lenB))
            myxx = yxx <$> mkey
        in ((offset, stringToAL binding), myxx)
      rawLines = zip (repeat Nothing) ("" : rmainMenuArt ++ "" : gameInfo)
                 ++ bindings
      (menuOvLines, mkyxs) = unzip $ zipWith generate [0..] rawLines
      kyxs = catMaybes mkyxs
      versionPos = K.PointUI (max 0 (2 * (rwidth - length versionBlurb)))
                             (rheight - 1)
      versionAl = take rwidth $ stringToAL versionBlurb
      ov = EM.singleton squareFont $ offsetOverlayX menuOvLines
                                     ++ [(versionPos, versionAl)]
  ekm <- displayChoiceScreen menuName ColorFull True
                             (menuToSlideshow (ov, kyxs)) [K.escKM]
  case ekm of
    Left km -> case km `lookup` kds of
      Just (_desc, cmd) -> cmdAction cmd
      Nothing -> weaveJust <$> failWith "never mind"
    Right _slot -> error $ "" `showFailure` ekm

-- | Display the main menu.
mainMenuHuman :: MonadClientUI m
              => (HumanCmd -> m (Either MError ReqUI))
              -> m (Either MError ReqUI)
mainMenuHuman cmdAction = do
  cops <- getsState scops
  CCUI{coinput=InputContent{bcmdList}} <- getsSession sccui
  gameMode <- getGameMode
  snxtScenario <- getsClient snxtScenario
  let nxtGameName = mname $ nxtGameMode cops snxtScenario
      tnextScenario = "pick next:" <+> nxtGameName
      -- Key-description-command tuples.
      kds = (K.mkKM "p", (tnextScenario, GameScenarioIncr))
            : [ (km, (desc, cmd))
              | (km, ([CmdMainMenu], desc, cmd)) <- bcmdList ]
      gameName = mname gameMode
      gameInfo = map T.unpack
                   [ " Now playing:" <+> gameName
                   , "" ]
  generateMenu cmdAction kds gameInfo "main"

-- * MainMenuAutoOn

-- | Display the main menu and set @swasAutomated@.
mainMenuAutoOnHuman :: MonadClientUI m
                    => (HumanCmd -> m (Either MError ReqUI))
                    -> m (Either MError ReqUI)
mainMenuAutoOnHuman cmdAction = do
  modifySession $ \sess -> sess {swasAutomated = True}
  mainMenuHuman cmdAction

-- * MainMenuAutoOff

-- | Display the main menu and unset @swasAutomated@.
mainMenuAutoOffHuman :: MonadClientUI m
                     => (HumanCmd -> m (Either MError ReqUI))
                     -> m (Either MError ReqUI)
mainMenuAutoOffHuman cmdAction = do
  modifySession $ \sess -> sess {swasAutomated = False}
  mainMenuHuman cmdAction

-- * SettingsMenu

-- | Display the settings menu.
settingsMenuHuman :: MonadClientUI m
                  => (HumanCmd -> m (Either MError ReqUI))
                  -> m (Either MError ReqUI)
settingsMenuHuman cmdAction = do
  markSuspect <- getsClient smarkSuspect
  markVision <- getsSession smarkVision
  markSmell <- getsSession smarkSmell
  side <- getsClient sside
  factDoctrine <- getsState $ fdoctrine . gplayer . (EM.! side) . sfactionD
  let offOn b = if b then "on" else "off"
      offOnAll n = case n of
        0 -> "none"
        1 -> "untried"
        2 -> "all"
        _ -> error $ "" `showFailure` n
      tsuspect = "mark suspect terrain:" <+> offOnAll markSuspect
      tvisible = "show visible zone:" <+> offOn markVision
      tsmell = "display smell clues:" <+> offOn markSmell
      tdoctrine = "squad doctrine:" <+> Ability.nameDoctrine factDoctrine
      -- Key-description-command tuples.
      kds = [ (K.mkKM "s", (tsuspect, MarkSuspect))
            , (K.mkKM "v", (tvisible, MarkVision))
            , (K.mkKM "c", (tsmell, MarkSmell))
            , (K.mkKM "t", (tdoctrine, Doctrine))
            , (K.mkKM "Escape", ("back to main menu", MainMenu)) ]
      gameInfo = map T.unpack
                   [ " Convenience settings:"
                   , "" ]
  generateMenu cmdAction kds gameInfo "settings"

-- * ChallengesMenu

-- | Display the challenges menu.
challengesMenuHuman :: MonadClientUI m
                    => (HumanCmd -> m (Either MError ReqUI))
                    -> m (Either MError ReqUI)
challengesMenuHuman cmdAction = do
  curChal <- getsClient scurChal
  nxtChal <- getsClient snxtChal
  let offOn b = if b then "on" else "off"
      tcurDiff = " *   difficulty:" <+> tshow (cdiff curChal)
      tnextDiff = "difficulty (lower easier):" <+> tshow (cdiff nxtChal)
      tcurWolf = " *   lone wolf:"
                 <+> offOn (cwolf curChal)
      tnextWolf = "lone wolf (very hard):"
                  <+> offOn (cwolf nxtChal)
      tcurFish = " *   cold fish:"
                 <+> offOn (cfish curChal)
      tnextFish = "cold fish (hard):"
                  <+> offOn (cfish nxtChal)
      -- Key-description-command tuples.
      kds = [ (K.mkKM "d", (tnextDiff, GameDifficultyIncr))
            , (K.mkKM "w", (tnextWolf, GameWolfToggle))
            , (K.mkKM "f", (tnextFish, GameFishToggle))
            , (K.mkKM "Escape", ("back to main menu", MainMenu)) ]
      gameInfo = map T.unpack
                   [ " Current challenges:"
                   , ""
                   , tcurDiff
                   , tcurWolf
                   , tcurFish
                   , ""
                   , " Next game challenges:"
                   , "" ]
  generateMenu cmdAction kds gameInfo "challenge"

-- * GameScenarioIncr

gameScenarioIncr :: MonadClient m => m ()
gameScenarioIncr =
  modifyClient $ \cli -> cli {snxtScenario = snxtScenario cli + 1}

-- * GameDifficultyIncr

gameDifficultyIncr :: MonadClient m => m ()
gameDifficultyIncr = do
  nxtDiff <- getsClient $ cdiff . snxtChal
  let delta = -1
      d | nxtDiff + delta > difficultyBound = 1
        | nxtDiff + delta < 1 = difficultyBound
        | otherwise = nxtDiff + delta
  modifyClient $ \cli -> cli {snxtChal = (snxtChal cli) {cdiff = d} }

-- * GameWolfToggle

gameWolfToggle :: MonadClient m => m ()
gameWolfToggle =
  modifyClient $ \cli ->
    cli {snxtChal = (snxtChal cli) {cwolf = not (cwolf (snxtChal cli))} }

-- * GameFishToggle

gameFishToggle :: MonadClient m => m ()
gameFishToggle =
  modifyClient $ \cli ->
    cli {snxtChal = (snxtChal cli) {cfish = not (cfish (snxtChal cli))} }

-- * GameRestart

gameRestartHuman :: MonadClientUI m => m (FailOrCmd ReqUI)
gameRestartHuman = do
  cops <- getsState scops
  isNoConfirms <- isNoConfirmsGame
  gameMode <- getGameMode
  snxtScenario <- getsClient snxtScenario
  let nxtGameName = mname $ nxtGameMode cops snxtScenario
  b <- if isNoConfirms
       then return True
       else displayYesNo ColorBW
            $ "You just requested a new" <+> nxtGameName
              <+> "game. The progress of the ongoing" <+> mname gameMode
              <+> "game will be lost! Are you sure?"
  if b
  then do
    snxtChal <- getsClient snxtChal
    -- This ignores all but the first word of game mode names picked
    -- via main menu and assumes the fist word of such game modes
    -- is present in their frequencies.
    let nxtGameGroup = toGroupName $ head $ T.words nxtGameName
    return $ Right $ ReqUIGameRestart nxtGameGroup snxtChal
  else do
    msg2 <- rndToActionForget $ oneOf
              [ "yea, would be a pity to leave them to die"
              , "yea, a shame to get your team stranded" ]
    failWith msg2

nxtGameMode :: COps -> Int -> ModeKind
nxtGameMode COps{comode} snxtScenario =
  let f !acc _p _i !a = a : acc
      campaignModes = ofoldlGroup' comode "campaign scenario" f []
  in campaignModes !! (snxtScenario `mod` length campaignModes)

-- * GameQuit

-- TODO: deduplicate with gameRestartHuman
gameQuitHuman :: MonadClientUI m => m (FailOrCmd ReqUI)
gameQuitHuman = do
  isNoConfirms <- isNoConfirmsGame
  gameMode <- getGameMode
  b <- if isNoConfirms
       then return True
       else displayYesNo ColorBW
            $ "If you quit, the progress of the ongoing" <+> mname gameMode
              <+> "game will be lost! Are you sure?"
  if b
  then do
    snxtChal <- getsClient snxtChal
    return $ Right $ ReqUIGameRestart "insert coin" snxtChal
  else do
    msg2 <- rndToActionForget $ oneOf
              [ "yea, would be a pity to leave them to die"
              , "yea, a shame to get your team stranded" ]
    failWith msg2

-- * GameDrop

gameDropHuman :: MonadClientUI m => m ReqUI
gameDropHuman = do
  modifySession $ \sess -> sess {sallNframes = -1}  -- hack, but we crash anyway
  promptAdd0 "Interrupt! Trashing the unsaved game. The program exits now."
  clientPrintUI "Interrupt! Trashing the unsaved game. The program exits now."
    -- this is not shown by vty frontend, but at least shown by sdl2 one
  return ReqUIGameDropAndExit

-- * GameExit

gameExitHuman :: MonadClientUI m => m ReqUI
gameExitHuman = do
  -- Announce before the saving started, since it can take a while.
  promptAdd0 "Saving game. The program stops now."
  return ReqUIGameSaveAndExit

-- * GameSave

gameSaveHuman :: MonadClientUI m => m ReqUI
gameSaveHuman = do
  -- Announce before the saving started, since it can take a while.
  promptAdd0 "Saving game backup."
  return ReqUIGameSave

-- * Doctrine

-- Note that the difference between seek-target and follow-the-leader doctrine
-- can influence even a faction with passive actors. E.g., if a passive actor
-- has an extra active skill from equipment, he moves every turn.
doctrineHuman :: MonadClientUI m => m (FailOrCmd ReqUI)
doctrineHuman = do
  fid <- getsClient sside
  fromT <- getsState $ fdoctrine . gplayer . (EM.! fid) . sfactionD
  let toT = if fromT == maxBound then minBound else succ fromT
  go <- displaySpaceEsc ColorFull
        $ "(Beware, work in progress!)"
          <+> "Current squad doctrine is" <+> Ability.nameDoctrine fromT
          <+> "(" <> Ability.describeDoctrine fromT <> ")."
          <+> "Switching doctrine to" <+> Ability.nameDoctrine toT
          <+> "(" <> Ability.describeDoctrine toT <> ")."
          <+> "This clears targets of all non-pointmen teammates."
          <+> "New targets will be picked according to new doctrine."
  if not go
  then failWith "squad doctrine change canceled"
  else return $ Right $ ReqUIDoctrine toT

-- * Automate

automateHuman :: MonadClientUI m => m (FailOrCmd ReqUI)
automateHuman = do
  clearAimMode
  go <- displaySpaceEsc ColorBW
          "Ceding control to AI (press SPACE to confirm, ESC to cancel)."
  if not go
  then failWith "automation canceled"
  else return $ Right ReqUIAutomate

-- * AutomateToggle

automateToggleHuman :: MonadClientUI m => m (FailOrCmd ReqUI)
automateToggleHuman = do
  swasAutomated <- getsSession swasAutomated
  if swasAutomated
  then failWith "automation canceled"
  else automateHuman

-- * AutomateBack

automateBackHuman :: MonadClientUI m => m (Either MError ReqUI)
automateBackHuman = do
  swasAutomated <- getsSession swasAutomated
  return $! if swasAutomated
            then Right ReqUIAutomate
            else Left Nothing
