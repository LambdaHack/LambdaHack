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
  , alterDirHuman, alterWithPointerHuman, closeDirHuman
  , helpHuman, hintHuman, dashboardHuman, itemMenuHuman, chooseItemMenuHuman
  , mainMenuHuman, mainMenuAutoOnHuman, mainMenuAutoOffHuman
  , settingsMenuHuman, challengesMenuHuman
  , gameScenarioIncr, gameDifficultyIncr, gameWolfToggle, gameFishToggle
    -- * Global commands that never take time
  , gameRestartHuman, gameQuitHuman, gameDropHuman, gameExitHuman, gameSaveHuman
  , doctrineHuman, automateHuman, automateToggleHuman, automateBackHuman
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , areaToRectangles, meleeAid, displaceAid, moveSearchAlter, alterCommon
  , goToXhair, multiActorGoTo, moveOrSelectItem, selectItemsToMove, moveItems
  , projectItem, applyItem, alterTileAtPos, verifyAlters, processTileActions
  , verifyEscape, verifyToolEffect, closeTileAtPos, msgAddDone, pickPoint
  , generateMenu, nxtGameMode
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Version
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Client.Bfs
import           Game.LambdaHack.Client.BfsM
import           Game.LambdaHack.Client.ClientOptions
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
import qualified Game.LambdaHack.Content.TileKind as TK
import qualified Game.LambdaHack.Core.Dice as Dice
import           Game.LambdaHack.Core.Random
import qualified Game.LambdaHack.Definition.Ability as Ability
import qualified Game.LambdaHack.Definition.Color as Color
import           Game.LambdaHack.Definition.Defs

-- * ByArea

-- | Pick command depending on area the mouse pointer is in.
-- The first matching area is chosen. If none match, only interrupt.
byAreaHuman :: MonadClientUI m
            => (K.KM -> HumanCmd -> m (Either MError ReqUI))
            -> [(CmdArea, HumanCmd)]
            -> m (Either MError ReqUI)
byAreaHuman cmdSemInCxtOfKM l = do
  CCUI{coinput=InputContent{brevMap}} <- getsSession sccui
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
    (_, cmd) : _ -> do
      let kmFound = case M.lookup cmd brevMap of
            Just (km : _) -> km
            _ -> K.escKM
      cmdSemInCxtOfKM kmFound cmd

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
  when (initialStep && run) $ do
    modifySession $ \sess ->
      sess {srunning = Just runParams}
    when runAhead $ macroHuman macroRun25
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
  side <- getsClient sside
  tb <- getsState $ getActorBody target
  sfact <- getsState $ (EM.! side) . sfactionD
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
          res | bproj tb || isFoe side sfact (bfid tb) = returnCmd
              | isFriend side sfact (bfid tb) = do
                let !_A = assert (side /= bfid tb) ()
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
  tfact <- getsState $ (EM.! bfid tb) . sfactionD
  actorMaxSk <- getsState $ getActorMaxSkills target
  dEnemy <- getsState $ dispEnemy leader target actorMaxSk
  let immobile = Ability.getSk Ability.SkMove actorMaxSk <= 0
      tpos = bpos tb
      adj = checkAdjacent sb tb
      atWar = isFoe (bfid tb) tfact (bfid sb)
  if | not adj -> failSer DisplaceDistant
     | not (bproj tb) && atWar
       && actorDying tb ->  -- checked separately for a better message
       failSer DisplaceDying
     | not (bproj tb) && atWar
       && actorWaits tb ->  -- checked separately for a better message
       failSer DisplaceBraced
     | not (bproj tb) && atWar
       && immobile ->  -- checked separately for a better message
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
  COps{coTileSpeedup} <- getsState scops
  actorSk <- leaderSkillsClientUI
  leader <- getLeaderUI
  sb <- getsState $ getActorBody leader
  let moveSkill = Ability.getSk Ability.SkMove actorSk
      spos = bpos sb           -- source position
      tpos = spos `shift` dir  -- target position
  alterable <- getsState $ tileAlterable (blid sb) tpos
  lvl <- getLevel $ blid sb
  let t = lvl `at` tpos
  runStopOrCmd <-
    if -- Movement requires full access.
       | Tile.isWalkable coTileSpeedup t ->
           if | moveSkill > 0 ->
                -- A potential invisible actor is hit. War started without
                -- asking.
                return $ Right $ ReqMove dir
              | bwatch sb == WSleep -> failSer MoveUnskilledAsleep
              | otherwise -> failSer MoveUnskilled
       -- Not walkable, so search and/or alter the tile.
       | run -> do
           -- Explicit request to examine the terrain.
           blurb <- lookAtPosition (blid sb) tpos
           mapM_ (uncurry msgAdd0) blurb
           failWith $ if alterable
                      then "potentially exploitable"
                      else "not exploitable"
       | otherwise -> alterCommon True tpos
  return $! runStopOrCmd

alterCommon :: MonadClientUI m => Bool -> Point -> m (FailOrCmd RequestTimed)
alterCommon bumping tpos = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  cops@COps{cotile, coTileSpeedup} <- getsState scops
  side <- getsClient sside
  factionD <- getsState sfactionD
  actorSk <- leaderSkillsClientUI
  leader <- getLeaderUI
  sb <- getsState $ getActorBody leader
  let alterSkill = Ability.getSk Ability.SkAlter actorSk
      spos = bpos sb
  alterable <- getsState $ tileAlterable (blid sb) tpos
  lvl <- getLevel $ blid sb
  localTime <- getsState $ getLocalTime (blid sb)
  embeds <- getsState $ getEmbedBag (blid sb) tpos
  itemToF <- getsState $ flip itemToFull
  getKind <- getsState $ flip getIidKind
  let t = lvl `at` tpos
      underFeet = tpos == spos  -- if enter and alter, be more permissive
  if | not alterable -> do
         let name = MU.Text $ TK.tname $ okind cotile t
             itemLook (iid, kit@(k, _)) =
               let itemFull = itemToF iid
               in partItemWsShort rwidth side factionD k localTime itemFull kit
             embedKindList =
               map (\(iid, kit) -> (getKind iid, (iid, kit))) (EM.assocs embeds)
             ilooks = map itemLook $ sortEmbeds cops t embedKindList
         failWith $ makePhrase $
           ["there is no point kicking", MU.AW name]
           ++ if EM.null embeds
              then []
              else ["with", MU.WWandW ilooks]
           -- misclick? related to AlterNothing but no searching possible;
           -- this also rules out activating embeds that only cause
           -- raw damage, with no chance of altering the tile
     | Tile.isSuspect coTileSpeedup t
       && not underFeet
       && alterSkill <= 1 -> failSer AlterUnskilled
     | not (Tile.isSuspect coTileSpeedup t)
       && not underFeet
       && alterSkill < Tile.alterMinSkill coTileSpeedup t -> do
         -- Rather rare (requires high skill), so describe the tile.
         blurb <- lookAtPosition (blid sb) tpos
         mapM_ (uncurry msgAdd0) blurb
         failSer AlterUnwalked
     | chessDist tpos (bpos sb) > 1 ->
         -- Checked late to give useful info about distant tiles.
         failSer AlterDistant
     | not underFeet && (EM.member tpos $ lfloor lvl) ->
         failSer AlterBlockItem
     | not underFeet
       && (occupiedBigLvl tpos lvl || occupiedProjLvl tpos lvl) ->
         -- Don't mislead describing terrain, if other actor is to blame.
         failSer AlterBlockActor
     | otherwise -> do  -- promising
         verAlters <- verifyAlters bumping leader tpos
         case verAlters of
           Right () ->
             if bumping then
               return $ Right $ ReqMove $ vectorToFrom tpos spos
             else do
               msgAddDone tpos "modify"
               return $ Right $ ReqAlter tpos
           Left err -> return $ Left err
         -- Even when bumping, we don't use ReqMove, because we don't want
         -- to hit invisible actors, e.g., hidden in a wall.
         -- If server performed an attack for free
         -- on the invisible actor anyway, the player (or AI)
         -- would be tempted to repeatedly hit random walls
         -- in hopes of killing a monster residing within.
         -- If the action had a cost, misclicks would incur the cost, too.
         -- Right now the player may repeatedly alter tiles trying to learn
         -- about invisible pass-wall actors, but when an actor detected,
         -- it costs a turn and does not harm the invisible actors,
         -- so it's not so tempting.

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
          _ | c == bpos b -> failWith "position reached"
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
      b <- getsState $ getActorBody r
      xhairPos <- xhairToPos
      if not onLevel || xhairPos == Just (bpos b) then do
        let paramNew = paramOld {runMembers = rs}
        multiActorGoTo arena c paramNew
      else do
        sL <- getState
        modifyClient $ updateLeader r sL
        let runMembersNew = rs ++ [r]
            paramNew = paramOld { runMembers = runMembersNew
                                , runWaiting = 0}
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
                failWith "collective running finished"  -- usually OK

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
              => [CStore] -> CStore -> Maybe Text -> Bool
              -> m (FailOrCmd RequestTimed)
moveItemHuman cLegalRaw destCStore mverb auto = do
  actorSk <- leaderSkillsClientUI
  if Ability.getSk Ability.SkMoveItem actorSk > 0 then do
    leader <- getLeaderUI
    b <- getsState $ getActorBody leader
    mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid b
    let overStash = mstash == Just (blid b, bpos b)
        calmE = calmEnough b actorSk
        cLegal = cLegalRaw \\ ([CGround | overStash] ++ [CEqp | not calmE])
    moveOrSelectItem cLegal cLegalRaw destCStore mverb auto
  else failSer MoveItemUnskilled

-- This cannot be structured as projecting or applying, with @ByItemMode@
-- and @ChooseItemToMove@, because at least in case of grabbing items,
-- more than one item is chosen, which doesn't fit @sitemSel@. Separating
-- grabbing of multiple items as a distinct command is too high a price.
moveOrSelectItem :: forall m. MonadClientUI m
                 => [CStore] -> [CStore] -> CStore -> Maybe Text -> Bool
                 -> m (FailOrCmd RequestTimed)
moveOrSelectItem cLegal cLegalRaw destCStore mverb auto = do
  itemSel <- getsSession sitemSel
  modifySession $ \sess -> sess {sitemSel = Nothing}  -- prevent surprise
  case itemSel of
    Just (iid, fromCStore, _) | fromCStore /= destCStore
                                && fromCStore `elem` cLegal -> do
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
    Just{} -> failWith "the selected item can't be so moved"
    _ -> do
      mis <- selectItemsToMove cLegal cLegalRaw destCStore mverb auto
      case mis of
        Left err -> return $ Left err
        Right (fromCStore, [(iid, _)]) | cLegalRaw /= [CGround] -> do
          modifySession $ \sess ->
            sess {sitemSel = Just (iid, fromCStore, False)}
          moveItemHuman cLegalRaw destCStore mverb auto
        Right is -> moveItems cLegalRaw is destCStore

selectItemsToMove :: forall m. MonadClientUI m
                  => [CStore] -> [CStore] -> CStore -> Maybe Text -> Bool
                  -> m (FailOrCmd (CStore, [(ItemId, ItemQuant)]))
selectItemsToMove cLegal cLegalRaw destCStore mverb auto = do
  let !_A = assert (destCStore `notElem` cLegalRaw) ()
  let verb = fromMaybe (verbCStore destCStore) mverb
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
  else if destCStore == CEqp && eqpOverfull b 1 then failSer EqpOverfull
  else if destCStore == CGround && overStash then failSer ItemOverStash
  else do
    let cLegalLast = case lastItemMove of
          Just (lastFrom, lastDest) | lastDest == destCStore
                                      && lastFrom `elem` cLegal ->
            lastFrom : delete lastFrom cLegal
          _ -> cLegal
        prompt = "What to"
        promptEqp = "What consumable to"
        ppItemDialogBody body actorSk cCur = case cCur of
          MStore CEqp | not $ calmEnough body actorSk ->
            "distractedly paw at" <+> ppItemDialogModeIn cCur
          MStore CGround | mstash == Just (blid body, bpos body) ->
            "greedily fondle" <+> ppItemDialogModeIn cCur
          _ -> case destCStore of
            CEqp | not $ calmEnough body actorSk ->
              "distractedly attempt to" <+> verb <+> ppItemDialogModeFrom cCur
            CEqp | eqpOverfull body 1 ->
              "attempt to fit into equipment" <+> ppItemDialogModeFrom cCur
            CGround | mstash == Just (blid body, bpos body) ->
              "greedily attempt to" <+> verb <+> ppItemDialogModeFrom cCur
            CEqp -> let n = sum $ map fst $ EM.elems $ beqp body
                    in verb
                       <+> "(" <> makePhrase [MU.CarWs n "item"] <+> "so far)"
                       <+> ppItemDialogModeFrom cCur
            _ -> verb <+> ppItemDialogModeFrom cCur
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
              (\body _ actorSk cCur _ ->
                 prompt <+> ppItemDialogBody body actorSk cCur)
              (\body _ actorSk cCur _ ->
                 promptGeneric <+> ppItemDialogBody body actorSk cCur)
              cLegalRaw cLegalLast (not auto) True
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
        if cLegalRaw == [CGround] && destCStore == CStash  -- normal pickup
        then -- @CStash@ is the implicit default; refine:
             if | not $ benInEqp $ discoBenefit EM.! iid -> retRec CStash
                | eqpOverfull b (oldN + 1) -> do
                  msgAdd MsgWarning $
                    "Warning:" <+> showReqFailure EqpOverfull <> "."
                  retRec CStash
                | eqpOverfull b (oldN + k) -> do
                  -- If this stack doesn't fit, we don't equip any part of it,
                  -- but we may equip a smaller stack later of other items
                  -- in the same pickup.
                  msgAdd MsgWarning $
                    "Warning:" <+> showReqFailure EqpStackFull <> "."
                  retRec CStash
                | not calmE -> do
                  msgAdd MsgWarning $
                    "Warning:" <+> showReqFailure ItemNotCalm <> "."
                  retRec CStash
                | otherwise ->
                  -- Prefer @CEqp@ if all conditions hold:
                  retRec CEqp
        else case destCStore of  -- player forces store, so @benInEqp@ ignored
          CEqp | eqpOverfull b (oldN + 1) -> do
            msgAdd MsgWarning $
              "Failure:" <+> showReqFailure EqpOverfull <> "."
            -- No recursive call here, we exit item manipulation:
            return []
          CEqp | eqpOverfull b (oldN + k) -> do
            msgAdd MsgWarning $
              "Failure:" <+> showReqFailure EqpStackFull <> "."
            return []
          CEqp | not calmE -> do
            msgAdd MsgWarning $
              "Failure:" <+> showReqFailure ItemNotCalm <> "."
            return []
          _ -> retRec destCStore
  l4 <- ret4 l 0
  if | fromCStore == CEqp && not calmE -> failSer ItemNotCalm
     | null l4 -> error $ "" `showFailure` l
     | otherwise -> return $ Right $ ReqMoveItems l4

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
            go <- if benFling >= 0
                  then displayYesNo ColorFull
                         "The item may be beneficial. Do you really want to fling it?"
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
          Nothing -> failWith "no item to trigger"
          Just kit -> do
            itemFull <- getsState $ itemToFull iid
            applyItem (fromCStore, (iid, (itemFull, kit)))
      Nothing -> failWith "no item to trigger"

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
                          "Using this periodic item will produce only the first of its effects and moreover, because it's not durable, will destroy it. Are you sure?"
           | benApply < 0 ->
             displayYesNo ColorFull
                          "The item appears harmful. Do you really want to trigger it?"
           | otherwise -> return True
      if go
      then return $ Right $ ReqApply iid fromCStore
      else do
        modifySession $ \sess -> sess {sitemSel = Nothing}
        failWith "never mind"

-- * AlterDir

-- | Ask for a direction and alter a tile, if possible.
alterDirHuman :: MonadClientUI m
              => m (FailOrCmd RequestTimed)
alterDirHuman = pickPoint "modify" >>= \case
  Just p -> alterTileAtPos p
  Nothing -> failWith "never mind"

-- | Try to alter a tile using a feature at the given position.
--
-- We don't check if the tile is interesting, e.g., if any embedded
-- item can be triggered, because the player explicitely requested
-- the action. Consequently, even if all embedded items are recharching,
-- the time will be wasted and the server will describe the failure in detail.
alterTileAtPos :: MonadClientUI m
               => Point
               -> m (FailOrCmd RequestTimed)
alterTileAtPos = alterCommon False

-- | Verify that the tile can be transformed or any embedded item effect
-- triggered and the player is aware if the effect is dangerous or grave,
-- such as ending the game.
verifyAlters :: forall m. MonadClientUI m
             => Bool -> ActorId -> Point -> m (FailOrCmd ())
verifyAlters bumping source tpos = do
  COps{cotile} <- getsState scops
  sb <- getsState $ getActorBody source
  arItem <- getsState $ aspectRecordFromIid $ btrunk sb
  embeds <- getsState $ getEmbedBag (blid sb) tpos
  lvl <- getLevel $ blid sb
  getKind <- getsState $ flip getIidKind
  let embedKindList =
        if IA.checkFlag Ability.Blast arItem
        then []  -- prevent embeds triggering each other in a loop
        else map (\(iid, kit) -> (getKind iid, (iid, kit))) (EM.assocs embeds)
      underFeet = tpos == bpos sb  -- if enter and alter, be more permissive
      feats = TK.tfeature $ okind cotile $ lvl `at` tpos
      tileActions =
        mapMaybe (Tile.parseTileAction (bproj sb) underFeet embedKindList)
                 feats
  processTileActions bumping source tpos tileActions

processTileActions :: forall m. MonadClientUI m
                   => Bool -> ActorId -> Point -> [Tile.TileAction]
                   -> m (FailOrCmd ())
processTileActions bumping source tpos tas = do
  COps{coTileSpeedup} <- getsState scops
  getKind <- getsState $ flip getIidKind
  sb <- getsState $ getActorBody source
  lvl <- getLevel $ blid sb
  sar <- getsState $ aspectRecordFromIid $ btrunk sb
  let sourceIsMist = IA.checkFlag Ability.Blast sar
                     && Dice.infDice (IK.idamage $ getKind $ btrunk sb) <= 0
      tileMinSkill = Tile.alterMinSkill coTileSpeedup $ lvl `at` tpos
      processTA :: Maybe Bool -> [Tile.TileAction] -> Bool
                -> m (FailOrCmd (Maybe (Bool, Bool)))
      processTA museResult [] bumpFailed = do
        let useResult = fromMaybe False museResult
        -- No warning will be generated if during explicit modification
        -- an embed is activated but there is not enough tools
        -- for a subsequent transformation. This is fine. Bumping would
        -- produce the warning and S-dir also displays the tool info.
        -- We can't rule out the embed is the main feature and the tool
        -- transformation is not important despite following it.
        -- We don't want spam in such a case.
        return $ Right $ if Tile.isSuspect coTileSpeedup (lvl `at` tpos)
                            || useResult && not bumpFailed
                         then Nothing  -- success of some kind
                         else Just (useResult, bumpFailed)  -- not quite
      processTA museResult (ta : rest) bumpFailed = case ta of
        Tile.EmbedAction (iid, _) -> do
          -- Embeds are activated in the order in tile definition
          -- and never after the tile is changed.
          -- We assume the item would trigger and we let the player
          -- take the risk of wasted turn to verify the assumption.
          -- If the item recharges, the wasted turns let the player wait.
          let useResult = fromMaybe False museResult
          if | sourceIsMist
               || bproj sb && tileMinSkill > 0 ->  -- local skill check
               processTA (Just useResult) rest bumpFailed
                 -- embed won't fire; try others
             | all (not . IK.isEffEscape) (IK.ieffects $ getKind iid) ->
               processTA (Just True) rest False
                 -- no escape checking needed, effect found;
                 -- also bumpFailed reset, because must have been
                 -- marginal if an embed was following it
             | otherwise -> do
               mfail <- verifyEscape
               case mfail of
                 Left err -> return $ Left err
                 Right () -> processTA (Just True) rest False
                   -- effect found, bumpFailed reset
        Tile.ToAction{} ->
          if fromMaybe True museResult
             && not (bproj sb && tileMinSkill > 0)  -- local skill check
          then return $ Right Nothing  -- tile changed, no more activations
          else processTA museResult rest bumpFailed
                 -- failed, but not due to bumping
        Tile.WithAction tools0 _ ->
          if not bumping || null tools0 then
            if fromMaybe True museResult then do
              -- UI requested, so this is voluntary, so item loss is fine.
              kitAssG <- getsState $ kitAssocs source [CGround]
              kitAssE <- getsState $ kitAssocs source [CEqp]
              let kitAss = listToolsToConsume kitAssG kitAssE
                  grps0 = map (\(x, y) -> (False, x, y)) tools0
                    -- apply if durable
                  (_, iidsToApply, grps) =
                    foldl' subtractIidfromGrps (EM.empty, [], grps0) kitAss
              if null grps then do
                let hasEffectOrDmg (_, (_, ItemFull{itemKind})) =
                      IK.idamage itemKind /= 0
                      || any IK.forApplyEffect (IK.ieffects itemKind)
                mfail <- case filter hasEffectOrDmg iidsToApply of
                  [] -> return $ Right ()
                  (store, (_, itemFull)) : _ ->
                    verifyToolEffect (blid sb) store itemFull
                case mfail of
                  Left err -> return $ Left err
                  Right () -> return $ Right Nothing  -- tile changed, done
              else processTA museResult rest bumpFailed  -- not enough tools
            else processTA museResult rest bumpFailed  -- embeds failed
          else processTA museResult rest True  -- failed due to bumping
  mfail <- processTA Nothing tas False
  case mfail of
    Left err -> return $ Left err
    Right Nothing -> return $ Right ()
    Right (Just (useResult, bumpFailed)) -> do
      let !_A = assert (not useResult || bumpFailed) ()
      blurb <- lookAtPosition (blid sb) tpos
      mapM_ (uncurry msgAdd0) blurb
      if bumpFailed then do
        revCmd <- revCmdMap
        let km = revCmd AlterDir
            msg = "bumping is not enough to transform this terrain; modify with the <"
                  <> T.pack (K.showKM km)
                  <> "> command instead"
        if useResult then do
          merr <- failMsg msg
          msgAdd MsgAlert $ showFailError $ fromJust merr
          return $ Right ()  -- effect the embed activation, though
        else failWith msg
      else failWith "unable to activate nor modify at this time"
        -- related to, among others, @SfxNoItemsForTile@ on the server

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
                 "You've finally found the way out, but you didn't gather all valuables rumoured to be laying around. Really leave already?"
               | otherwise =
                 "This is the way out and you collected all treasure there is to find. Really leave now?"
    -- The player can back off, but we never insist,
    -- because possibly the score formula doesn't reward treasure
    -- or he is focused on winning only.
    go <- displayYesNo ColorBW prompt
    if not go
    then failWith "here's your chance!"
    else return $ Right ()

verifyToolEffect :: MonadClientUI m
                 => LevelId -> CStore -> ItemFull -> m (FailOrCmd ())
verifyToolEffect lid store itemFull = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  side <- getsClient sside
  localTime <- getsState $ getLocalTime lid
  factionD <- getsState sfactionD
  let object = makePhrase
                 [partItemWsShort rwidth side factionD 1 localTime
                                  itemFull (1, [])]
      prompt = "Do you really want to transform the terrain using"
               <+> object
               <+> "that may cause substantial side-effects?"
  go <- displayYesNo ColorBW prompt
  if not go
  then failWith $ "Replace" <+> object <+> ppCStoreIn store
                  <+> "and try again."
  else return $ Right ()

-- * AlterWithPointer

-- | Try to alter a tile using a feature under the pointer.
alterWithPointerHuman :: MonadClientUI m
                      => m (FailOrCmd RequestTimed)
alterWithPointerHuman = do
  COps{corule=RuleContent{rXmax, rYmax}} <- getsState scops
  K.PointUI x y <- getsSession spointer
  let (px, py) = (x `div` 2, y - K.mapStartY)
      tpos = Point px py
  if px >= 0 && py >= 0 && px < rXmax && py < rYmax
  then alterTileAtPos tpos
  else failWith "never mind"

-- * CloseDir

-- | Close nearby open tile; ask for direction, if there is more than one.
closeDirHuman :: MonadClientUI m
              => m (FailOrCmd RequestTimed)
closeDirHuman = do
  COps{coTileSpeedup} <- getsState scops
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  lvl <- getLevel $ blid b
  let vPts = vicinityUnsafe $ bpos b
      openPts = filter (Tile.isClosable coTileSpeedup . at lvl) vPts
  case openPts of
    []  -> failSer CloseNothing
    [o] -> closeTileAtPos o
    _   -> pickPoint "close" >>= \case
      Nothing -> failWith "never mind"
      Just p -> closeTileAtPos p

-- | Close tile at given position.
closeTileAtPos :: MonadClientUI m
               => Point -> m (FailOrCmd RequestTimed)
closeTileAtPos tpos = do
  COps{coTileSpeedup} <- getsState scops
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorSk <- leaderSkillsClientUI
  alterable <- getsState $ tileAlterable (blid b) tpos
  lvl <- getLevel $ blid b
  let alterSkill = Ability.getSk Ability.SkAlter actorSk
      t = lvl `at` tpos
      isOpen = Tile.isClosable coTileSpeedup t
      isClosed = Tile.isOpenable coTileSpeedup t
  case (alterable, isClosed, isOpen) of
    (False, _, _) -> failSer CloseNothing
    (True, False, False) -> failSer CloseNonClosable
    (True, True,  False) -> failSer CloseClosed
    (True, True,  True) -> error "TileKind content validation"
    (True, False, True) ->
      if | tpos `chessDist` bpos b > 1
          -> failSer CloseDistant
         | alterSkill <= 1
          -> failSer AlterUnskilled
         | EM.member tpos $ lfloor lvl
          -> failSer AlterBlockItem
         | occupiedBigLvl tpos lvl || occupiedProjLvl tpos lvl
          -> failSer AlterBlockActor
         | otherwise
          -> do
             msgAddDone tpos "close"
             return $ Right (ReqAlter tpos)

-- | Adds message with proper names.
msgAddDone :: MonadClientUI m
           => Point -> Text
           -> m ()
msgAddDone p verb = do
  COps{cotile} <- getsState scops
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  lvl <- getLevel $ blid b
  let tname = TK.tname $ okind cotile $ lvl `at` p
      s = case T.words tname of
            [] -> "thing"
            ("open" : xs) -> T.unwords xs
            _ -> tname
      v = p `vectorToFrom` bpos b
      dir | v == Vector 0 0 = "underneath"
          | otherwise = compassText v
  msgAdd MsgDone $ "You" <+> verb <+> "the" <+> s <+> dir <> "."

-- | Prompts user to pick a point.
pickPoint :: MonadClientUI m
          => Text -> m (Maybe Point)
pickPoint verb = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  UIOptions{uVi, uLeftHand} <- getsSession sUIOptions
  let dirKeys = K.dirAllKey uVi uLeftHand
      keys = K.escKM
           : K.leftButtonReleaseKM
           : map (K.KM K.NoModifier) dirKeys
  promptAdd0 $ "Where to" <+> verb <> "? [movement key] [pointer]"
  slides <- reportToSlideshow [K.escKM]
  km <- getConfirms ColorFull keys slides
  case K.key km of
    K.LeftButtonRelease -> do
      K.PointUI x y <- getsSession spointer
      return $ Just $ Point (x `div` 2) (y - K.mapStartY)
    _ -> return $ shift (bpos b) <$> K.handleDir dirKeys km

-- * Help

-- | Display command help.
helpHuman :: MonadClientUI m
          => (K.KM -> HumanCmd -> m (Either MError ReqUI))
          -> m (Either MError ReqUI)
helpHuman cmdSemInCxtOfKM = do
  ccui@CCUI{coinput, coscreen=ScreenContent{rwidth, rheight}}
    <- getsSession sccui
  fontSetup <- getFontSetup
  let keyH = keyHelp ccui fontSetup
      splitHelp (t, okx) = splitOKX fontSetup True rwidth rheight (textToAS t)
                                    [K.spaceKM, K.escKM] okx
      sli = toSlideshow fontSetup $ concat $ map splitHelp keyH
  -- Thus, the whole help menu corresponde to a single menu of item or lore,
  -- e.g., shared stash menu. This is especially clear when the shared stash
  -- menu contains many pages.
  ekm <- displayChoiceScreen "help" ColorFull True sli [K.spaceKM, K.escKM]
  case ekm of
    Left km -> case km `M.lookup` bcmdMap coinput of
      _ | km `elem` [K.escKM, K.spaceKM] -> return $ Left Nothing
      Just (_desc, _cats, cmd) -> cmdSemInCxtOfKM km cmd
      Nothing -> weaveJust <$> failWith "never mind"
    Right _slot -> error $ "" `showFailure` ekm

-- * Hint

-- | Display hint or, if already displayed, display help.
hintHuman :: MonadClientUI m
          => (K.KM -> HumanCmd -> m (Either MError ReqUI))
          -> m (Either MError ReqUI)
hintHuman cmdSemInCxtOfKM = do
  hintMode <- getsSession shintMode
  if hintMode == HintWiped then
    helpHuman cmdSemInCxtOfKM
  else do
    modifySession $ \sess -> sess {shintMode = HintShown}
    promptMainKeys
    return $ Left Nothing

-- * Dashboard

-- | Display the dashboard.
dashboardHuman :: MonadClientUI m
               => (K.KM -> HumanCmd -> m (Either MError ReqUI))
               -> m (Either MError ReqUI)
dashboardHuman cmdSemInCxtOfKM = do
  CCUI{coinput, coscreen=ScreenContent{rwidth, rheight}} <- getsSession sccui
  fontSetup@FontSetup{..} <- getFontSetup
  let keyL = 2
      (ov0, kxs0) = okxsN coinput monoFont propFont 0 keyL (const False) False
                          CmdDashboard ([], []) ([], [])
      al1 = textToAS "Dashboard"
  let splitHelp (al, okx) = splitOKX fontSetup False rwidth (rheight - 2) al
                                     [K.escKM] okx
      sli = toSlideshow fontSetup $ splitHelp (al1, (ov0, kxs0))
      extraKeys = [K.escKM]
  ekm <- displayChoiceScreen "dashboard" ColorFull False sli extraKeys
  case ekm of
    Left km -> case km `M.lookup` bcmdMap coinput of
      _ | km == K.escKM -> weaveJust <$> failWith "never mind"
      Just (_desc, _cats, cmd) -> cmdSemInCxtOfKM km cmd
      Nothing -> weaveJust <$> failWith "never mind"
    Right _slot -> error $ "" `showFailure` ekm

-- * ItemMenu

itemMenuHuman :: MonadClientUI m
              => (K.KM -> HumanCmd -> m (Either MError ReqUI))
              -> m (Either MError ReqUI)
itemMenuHuman cmdSemInCxtOfKM = do
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
          let foundPrefix = textToAS $
                if null foundTexts then "" else "The item is also in:"
              markParagraphs = rheight >= 45
              descAl = itemDesc rwidth markParagraphs (bfid b) factionD
                                (Ability.getSk Ability.SkHurtMelee actorMaxSk)
                                fromCStore localTime jlid itemFull kit
              (descSymAl, descBlurbAl) = span (/= Color.spaceAttrW32) descAl
              descSym = offsetOverlay $ splitAttrString rwidth descSymAl
              descBlurb = offsetOverlayX $
                case splitAttrString rwidth $ stringToAS "xx" ++ descBlurbAl of
                  [] -> error "splitting AttrString loses characters"
                  al1 : rest ->
                    (2, attrStringToAL $ drop 2 $ attrLine al1) : map (0,) rest
              alPrefix = map (\(K.PointUI x y, al) ->
                                (K.PointUI x (y + length descBlurb), al))
                         $ offsetOverlay $ splitAttrString rwidth foundPrefix
              ystart = length descBlurb + length alPrefix - 1
              xstart = textSize monoFont (Color.spaceAttrW32
                                          : attrLine (snd $ last alPrefix))
              foundKeys = map (K.KM K.NoModifier . K.Fun)
                              [1 .. length foundAlt]  -- starting from 1!
          let ks = zip foundKeys foundTexts
              width = if isSquareFont monoFont then 2 * rwidth else rwidth
              (ovFoundRaw, kxsFound) = wrapOKX monoFont ystart xstart width ks
              ovFound = alPrefix ++ ovFoundRaw
          report <- getReportUI
          CCUI{coinput} <- getsSession sccui
          mstash <- getsState $ \s -> gstash $ sfactionD s EM.! (bfid b)
          actorSk <- leaderSkillsClientUI
          let calmE = calmEnough b actorMaxSk
              greyedOut cmd = not calmE && fromCStore == CEqp
                              || mstash == Just (blid b, bpos b)
                                 && fromCStore == CGround
                              || case cmd of
                ByAimMode AimModeCmd{..} ->
                  greyedOut exploration || greyedOut aiming
                ComposeIfLocal cmd1 cmd2 -> greyedOut cmd1 || greyedOut cmd2
                ComposeUnlessError cmd1 cmd2 -> greyedOut cmd1 || greyedOut cmd2
                Compose2ndLocal cmd1 cmd2 -> greyedOut cmd1 || greyedOut cmd2
                MoveItem stores destCStore _ _ ->
                  fromCStore `notElem` stores
                  || destCStore == CEqp && (not calmE || eqpOverfull b 1)
                  || destCStore == CGround && mstash == Just (blid b, bpos b)
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
              (ov0, kxs0) = okxsN coinput monoFont propFont offset keyL
                                  greyedOut True CmdItemMenu
                                  ([], ["", keyCaption]) ([], [])
              t0 = makeSentence [ MU.SubjectVerbSg (partActor bUI) "choose"
                                , "an item", MU.Text $ ppCStoreIn fromCStore ]
              alRep = renderReport report
              al1 | null alRep = textToAS t0
                  | otherwise = alRep ++ stringToAS "\n" ++ textToAS t0
              splitHelp (al, okx) =
                splitOKX fontSetup False rwidth (rheight - 2) al
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
                       itemMenuHuman cmdSemInCxtOfKM
                _ -> error $ "" `showFailure` km
              Just (_desc, _cats, cmd) -> do
                modifySession $ \sess ->
                  sess {sitemSel = Just (iid, fromCStore, True)}
                res <- cmdSemInCxtOfKM km cmd
                modifySession $ \sess ->
                  sess {sitemSel = case res of
                          Left{} -> Nothing
                          Right{} ->  Just (iid, fromCStore, False)}
                return res
              Nothing -> weaveJust <$> failWith "never mind"
            Right _slot -> error $ "" `showFailure` ekm
    Nothing -> weaveJust <$> failWith "no item to open item menu for"

-- * ChooseItemMenu

chooseItemMenuHuman :: MonadClientUI m
                    => (K.KM -> HumanCmd -> m (Either MError ReqUI))
                    -> ItemDialogMode
                    -> m (Either MError ReqUI)
chooseItemMenuHuman cmdSemInCxtOfKM c = do
  res <- chooseItemDialogMode c
  case res of
    Right c2 -> do
      res2 <- itemMenuHuman cmdSemInCxtOfKM
      case res2 of
        Left Nothing -> chooseItemMenuHuman cmdSemInCxtOfKM c2
        _ -> return res2
    Left err -> return $ Left $ Just err

-- * MainMenu

generateMenu :: MonadClientUI m
             => (K.KM -> HumanCmd -> m (Either MError ReqUI))
             -> [AttrLine] -> [(K.KM, (Text, HumanCmd))] -> [String] -> String
             -> m (Either MError ReqUI)
generateMenu cmdSemInCxtOfKM blurb kds gameInfo menuName = do
  COps{corule} <- getsState scops
  CCUI{coscreen=ScreenContent{rwidth, rheight, rmainMenuLine}} <-
    getsSession sccui
  FontSetup{..} <- getFontSetup
  let offset = if isSquareFont propFont || length blurb <= 1 then 2 else -8
      bindings =  -- key bindings to display
        let fmt (k, (d, _)) =
              ( Just k
              , T.unpack
                $  T.justifyLeft 3 ' ' (T.pack $ K.showKM k) <> " " <> d )
        in map fmt kds
      generate :: Int -> (Maybe K.KM, String) -> ((Int, AttrLine), Maybe KYX)
      generate y (mkey, binding) =
        let lenB = length binding
            yxx key = (Left [key], ( K.PointUI 2 y
                                   , ButtonWidth squareFont lenB ))
            myxx = yxx <$> mkey
        in ((2, stringToAL binding), myxx)
      titleLine = rtitle corule
                  ++ " " ++ showVersion (rexeVersion corule)
      rawLines = zip (repeat Nothing)
                     (["", titleLine ++ " " ++ rmainMenuLine, ""]
                      ++ gameInfo)
                 ++ bindings
      (menuOvLines, mkyxs) = unzip $ zipWith generate [0..] rawLines
      kyxs = catMaybes mkyxs
      introLen = length blurb
      introMaxLen = maximum $ 30 : map (textSize monoFont . attrLine) blurb
      introOv = map (\(y, al) ->
                       (K.PointUI (2 * rwidth - introMaxLen - offset) y, al))
                $ zip [max 0 (rheight - introLen - 1) ..] blurb
      ov = EM.insertWith (++) propFont introOv
           $ EM.singleton squareFont $ offsetOverlayX menuOvLines
  ekm <- displayChoiceScreen menuName ColorFull True
                             (menuToSlideshow (ov, kyxs)) [K.escKM]
  case ekm of
    Left km -> case km `lookup` kds of
      Just (_desc, cmd) -> cmdSemInCxtOfKM km cmd
      Nothing -> weaveJust <$> failWith "never mind"
    Right _slot -> error $ "" `showFailure` ekm

-- | Display the main menu.
mainMenuHuman :: MonadClientUI m
              => (K.KM -> HumanCmd -> m (Either MError ReqUI))
              -> m (Either MError ReqUI)
mainMenuHuman cmdSemInCxtOfKM = do
  CCUI{ coinput=InputContent{bcmdList}
      , coscreen=ScreenContent{rintroScreen} } <- getsSession sccui
  FontSetup{propFont} <- getFontSetup
  gameMode <- getGameMode
  curChal <- getsClient scurChal
  let offOn b = if b then "on" else "off"
      tcurDiff = "  with difficulty:" <+> tshow (cdiff curChal)
      tcurWolf = "       lone wolf:" <+> offOn (cwolf curChal)
      tcurFish = "       cold fish:" <+> offOn (cfish curChal)
      -- Key-description-command tuples.
      kds = [(km, (desc, cmd)) | (km, ([CmdMainMenu], desc, cmd)) <- bcmdList]
      gameName = mname gameMode
      gameInfo = map T.unpack
                   [ "Now playing:" <+> gameName
                   , ""
                   , tcurDiff
                   , tcurWolf
                   , tcurFish
                   , "" ]
      glueLines (l1 : l2 : rest) =
        if | null l1 -> l1 : glueLines (l2 : rest)
           | null l2 -> l1 : l2 : glueLines rest
           | otherwise -> (l1 ++ l2) : glueLines rest
      glueLines ll = ll
      backstory | isSquareFont propFont = rintroScreen
                | otherwise = glueLines rintroScreen
  generateMenu cmdSemInCxtOfKM (map stringToAL backstory) kds gameInfo "main"

-- * MainMenuAutoOn

-- | Display the main menu and set @swasAutomated@.
mainMenuAutoOnHuman :: MonadClientUI m
                    => (K.KM -> HumanCmd -> m (Either MError ReqUI))
                    -> m (Either MError ReqUI)
mainMenuAutoOnHuman cmdSemInCxtOfKM = do
  modifySession $ \sess -> sess {swasAutomated = True}
  mainMenuHuman cmdSemInCxtOfKM

-- * MainMenuAutoOff

-- | Display the main menu and unset @swasAutomated@.
mainMenuAutoOffHuman :: MonadClientUI m
                     => (K.KM -> HumanCmd -> m (Either MError ReqUI))
                     -> m (Either MError ReqUI)
mainMenuAutoOffHuman cmdSemInCxtOfKM = do
  modifySession $ \sess -> sess {swasAutomated = False}
  mainMenuHuman cmdSemInCxtOfKM

-- * SettingsMenu

-- | Display the settings menu.
settingsMenuHuman :: MonadClientUI m
                  => (K.KM -> HumanCmd -> m (Either MError ReqUI))
                  -> m (Either MError ReqUI)
settingsMenuHuman cmdSemInCxtOfKM = do
  markSuspect <- getsClient smarkSuspect
  markVision <- getsSession smarkVision
  markSmell <- getsSession smarkSmell
  noAnim <- getsClient $ fromMaybe False . snoAnim . soptions
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
      tanim = "play animations:" <+> offOn (not noAnim)
      tdoctrine = "squad doctrine:" <+> Ability.nameDoctrine factDoctrine
      -- Key-description-command tuples.
      kds = [ (K.mkKM "s", (tsuspect, MarkSuspect))
            , (K.mkKM "v", (tvisible, MarkVision))
            , (K.mkKM "c", (tsmell, MarkSmell))
            , (K.mkKM "a", (tanim, MarkAnim))
            , (K.mkKM "t", (tdoctrine, Doctrine))
            , (K.mkKM "Escape", ("back to main menu", MainMenu)) ]
      gameInfo = map T.unpack
                   [ "Tweak convenience settings:"
                   , "" ]
  generateMenu cmdSemInCxtOfKM [] kds gameInfo "settings"

-- * ChallengesMenu

-- | Display the challenges menu.
challengesMenuHuman :: MonadClientUI m
                    => (K.KM -> HumanCmd -> m (Either MError ReqUI))
                    -> m (Either MError ReqUI)
challengesMenuHuman cmdSemInCxtOfKM = do
  cops <- getsState scops
  FontSetup{propFont} <- getFontSetup
  snxtScenario <- getsClient snxtScenario
  nxtChal <- getsClient snxtChal
  let gameMode = nxtGameMode cops snxtScenario
      tnextScenario = "scenario:" <+> mname gameMode
      offOn b = if b then "on" else "off"
      tnextDiff = "difficulty (lower easier):" <+> tshow (cdiff nxtChal)
      tnextWolf = "lone wolf (very hard):"
                  <+> offOn (cwolf nxtChal)
      tnextFish = "cold fish (hard):"
                  <+> offOn (cfish nxtChal)
      -- Key-description-command tuples.
      kds = [ (K.mkKM "s", (tnextScenario, GameScenarioIncr))
            , (K.mkKM "d", (tnextDiff, GameDifficultyIncr))
            , (K.mkKM "w", (tnextWolf, GameWolfToggle))
            , (K.mkKM "f", (tnextFish, GameFishToggle))
            , (K.mkKM "g", ("start new game", GameRestart))
            , (K.mkKM "Escape", ("back to main menu", MainMenu)) ]
      gameInfo = map T.unpack
                   [ "Setup and start new game:"
                   , "" ]
      width = if isSquareFont propFont then 42 else 84
      duplicateEOL '\n' = "\n\n"
      duplicateEOL c = T.singleton c
      blurb = splitAttrString width $ textToAS $ T.concatMap duplicateEOL
              $ mdesc gameMode
                <> if T.null (mnote gameMode)
                   then "\n"  -- whitespace compensates for the lack of note
                   else "\n[Note: " <> mnote gameMode <> "]"
  generateMenu cmdSemInCxtOfKM blurb kds gameInfo "challenge"

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
    let nxtGameGroup = GroupName $ head $ T.words nxtGameName
    return $ Right $ ReqUIGameRestart nxtGameGroup snxtChal
  else do
    msg2 <- rndToActionUI $ oneOf
              [ "yea, would be a pity to leave them to die"
              , "yea, a shame to get your team stranded" ]
    failWith msg2

nxtGameMode :: COps -> Int -> ModeKind
nxtGameMode COps{comode} snxtScenario =
  let f !acc _p _i !a = a : acc
      campaignModes = ofoldlGroup' comode CAMPAIGN_SCENARIO f []
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
    return $ Right $ ReqUIGameRestart INSERT_COIN snxtChal
  else do
    msg2 <- rndToActionUI $ oneOf
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
  proceed <- displayYesNo ColorBW "Do you really want to cede control to AI?"
  if not proceed
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
