{-# LANGUAGE DataKinds, GADTs #-}
-- | Semantics of "Game.LambdaHack.Client.UI.HumanCmd"
-- client commands that return server requests.
-- A couple of them do not take time, the rest does.
-- Here prompts and menus are displayed, but any feedback resulting
-- from the commands (e.g., from inventory manipulation) is generated later on,
-- by the server, for all clients that witness the results of the commands.
module Game.LambdaHack.Client.UI.HandleHumanGlobalM
  ( -- * Meta commands
    byAreaHuman, byAimModeHuman, byItemModeHuman
  , composeIfLocalHuman, composeUnlessErrorHuman, compose2ndLocalHuman
  , loopOnNothingHuman, executeIfClearHuman
    -- * Global commands that usually take time
  , waitHuman, waitHuman10, moveRunHuman
  , runOnceAheadHuman, moveOnceToXhairHuman
  , runOnceToXhairHuman, continueToXhairHuman
  , moveItemHuman, projectHuman, applyHuman
  , alterDirHuman, alterWithPointerHuman
  , helpHuman, hintHuman, dashboardHuman, itemMenuHuman, chooseItemMenuHuman
  , mainMenuHuman, settingsMenuHuman, challengesMenuHuman
  , gameScenarioIncr, gameDifficultyIncr, gameWolfToggle, gameFishToggle
    -- * Global commands that never take time
  , gameRestartHuman, gameExitHuman, gameSaveHuman
  , tacticHuman, automateHuman
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , areaToRectangles, meleeAid, displaceAid, moveSearchAlter, goToXhair
  , multiActorGoTo, selectItemsToMove, moveItems, projectItem, applyItem
  , alterTile, alterTileAtPos, verifyAlters, verifyEscape, guessAlter
  , artWithVersion, generateMenu, nxtGameMode
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

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
import           Game.LambdaHack.Common.Ability
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.Random
import           Game.LambdaHack.Common.ReqFailure
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Content.RuleKind
import           Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Content.TileKind as TK

-- * ByArea

-- | Pick command depending on area the mouse pointer is in.
-- The first matching area is chosen. If none match, only interrupt.
byAreaHuman :: MonadClientUI m
            => (HumanCmd -> m (Either MError ReqUI))
            -> [(CmdArea, HumanCmd)]
            -> m (Either MError ReqUI)
byAreaHuman cmdAction l = do
  pointer <- getsSession spointer
  let pointerInArea a = do
        rs <- areaToRectangles a
        return $! any (inside pointer) rs
  cmds <- filterM (pointerInArea . fst) l
  case cmds of
    [] -> do
      stopPlayBack
      return $ Left Nothing
    (_, cmd) : _ ->
      cmdAction cmd

-- Many values here are shared with "Game.LambdaHack.Client.UI.DrawM".
areaToRectangles :: MonadClientUI m => CmdArea -> m [(X, Y, X, Y)]
areaToRectangles ca = case ca of
  CaMessage -> return [(0, 0, fst normalLevelBound, 0)]
  CaMapLeader -> do  -- takes preference over @CaMapParty@ and @CaMap@
    leader <- getLeaderUI
    b <- getsState $ getActorBody leader
    let Point{..} = bpos b
    return [(px, mapStartY + py, px, mapStartY + py)]
  CaMapParty -> do  -- takes preference over @CaMap@
    lidV <- viewedLevelUI
    side <- getsClient sside
    ours <- getsState $ filter (not . bproj) . map snd
                        . actorAssocs (== side) lidV
    let rectFromB Point{..} = (px, mapStartY + py, px, mapStartY + py)
    return $! map (rectFromB . bpos) ours
  CaMap -> return
    [( 0, mapStartY, fst normalLevelBound, mapStartY + snd normalLevelBound )]
  CaLevelNumber -> let y = snd normalLevelBound + 2
                   in return [(0, y, 1, y)]
  CaArenaName -> let y = snd normalLevelBound + 2
                     x = fst normalLevelBound `div` 2 - 11
                 in return [(3, y, x, y)]
  CaPercentSeen -> let y = snd normalLevelBound + 2
                       x = fst normalLevelBound `div` 2
                   in return [(x - 9, y, x, y)]
  CaXhairDesc -> let y = snd normalLevelBound + 2
                     x = fst normalLevelBound `div` 2 + 2
                 in return [(x, y, fst normalLevelBound, y)]
  CaSelected -> let y = snd normalLevelBound + 3
                    x = fst normalLevelBound `div` 2
                in return [(0, y, x - 24, y)]
  CaCalmGauge -> let y = snd normalLevelBound + 3
                     x = fst normalLevelBound `div` 2
                 in return [(x - 22, y, x - 11, y)]
  CaHPGauge -> let y = snd normalLevelBound + 3
                   x = fst normalLevelBound `div` 2
               in return [(x - 9, y, x, y)]
  CaTargetDesc -> let y = snd normalLevelBound + 3
                      x = fst normalLevelBound `div` 2 + 2
                  in return [(x, y, fst normalLevelBound, y)]

-- * ByAimMode

byAimModeHuman :: MonadClientUI m
               => m (Either MError ReqUI) -> m (Either MError ReqUI)
               -> m (Either MError ReqUI)
byAimModeHuman cmdNotAimingM cmdAimingM = do
  aimMode <- getsSession saimMode
  if isNothing aimMode then cmdNotAimingM else cmdAimingM

-- * ByItemMode

byItemModeHuman :: MonadClientUI m
                => [TriggerItem]
                -> m (Either MError ReqUI) -> m (Either MError ReqUI)
                -> m (Either MError ReqUI)
byItemModeHuman ts cmdNotChosenM cmdChosenM = do
  itemSel <- getsSession sitemSel
  let triggerSyms = triggerSymbols ts
  case itemSel of
    Just (fromCStore, iid) -> do
      leader <- getLeaderUI
      b <- getsState $ getActorBody leader
      bag <- getsState $ getBodyStoreBag b fromCStore
      itemKind <- getsState $ getIidKind iid
      case iid `EM.lookup` bag of
        Just _ | null triggerSyms || IK.isymbol itemKind `elem` triggerSyms ->
                 cmdChosenM
        _ -> cmdNotChosenM
    Nothing -> cmdNotChosenM

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
waitHuman :: MonadClientUI m => m (RequestTimed 'AbWait)
waitHuman = do
  modifySession $ \sess -> sess {swaitTimes = abs (swaitTimes sess) + 1}
  return ReqWait

-- * Wait10

-- | Leader waits a 1/10th of a turn (and doesn't block, etc.).
waitHuman10 :: MonadClientUI m => m (RequestTimed 'AbWait)
waitHuman10 = do
  modifySession $ \sess -> sess {swaitTimes = abs (swaitTimes sess) + 1}
  return ReqWait10

-- * MoveDir and RunDir

moveRunHuman :: MonadClientUI m
             => Bool -> Bool -> Bool -> Bool -> Vector
             -> m (FailOrCmd RequestAnyAbility)
moveRunHuman initialStep finalGoal run runAhead dir = do
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
  tgts <- getsState $ posToAssocs tpos arena
  case tgts of
    [] -> do  -- move or search or alter
      runStopOrCmd <- moveSearchAlter dir
      case runStopOrCmd of
        Left stopMsg -> return $ Left stopMsg
        Right runCmd ->
          -- Don't check @initialStep@ and @finalGoal@
          -- and don't stop going to target: door opening is mundane enough.
          return $ Right runCmd
    [(target, _)] | run && initialStep ->
      -- No @stopPlayBack@: initial displace is benign enough.
      -- Displacing requires accessibility, but it's checked later on.
      RequestAnyAbility <$$> displaceAid target
    _ : _ : _ | run && initialStep -> do
      let !_A = assert (all (bproj . snd) tgts) ()
      failSer DisplaceProjectiles
    (target, tb) : _ | initialStep && finalGoal -> do
      stopPlayBack  -- don't ever auto-repeat melee
      -- No problem if there are many projectiles at the spot. We just
      -- attack the first one.
      -- We always see actors from our own faction.
      if bfid tb == bfid sb && not (bproj tb) then do
        -- Select adjacent actor by bumping into him. Takes no time.
        success <- pickLeader True target
        let !_A = assert (success `blame` "bump self"
                                  `swith` (leader, target, tb)) ()
        failWith "by bumping"
      else
        -- Attacking does not require full access, adjacency is enough.
        RequestAnyAbility <$$> meleeAid target
    _ : _ -> failWith "actor in the way"

-- | Actor attacks an enemy actor or his own projectile.
meleeAid :: MonadClientUI m
         => ActorId -> m (FailOrCmd (RequestTimed 'AbMelee))
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
            -- Set personal target to the enemy position,
            -- to easily him with a ranged attack when he flees.
            let f (Just (TEnemy _ b)) = Just $ TEnemy target b
                f (Just (TPoint (TEnemyPos _ b) _ _)) = Just $ TEnemy target b
                f _ = Just $ TEnemy target False
            modifyClient $ updateTarget leader f
            return $ Right wp
          res | bproj tb || isAtWar sfact (bfid tb) = returnCmd
              | isAllied sfact (bfid tb) = do
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
            => ActorId -> m (FailOrCmd (RequestTimed 'AbDisplace))
displaceAid target = do
  COps{coTileSpeedup} <- getsState scops
  leader <- getLeaderUI
  sb <- getsState $ getActorBody leader
  tb <- getsState $ getActorBody target
  tfact <- getsState $ (EM.! bfid tb) . sfactionD
  actorMaxSk <- maxActorSkillsClient target
  disp <- getsState $ dispEnemy leader target actorMaxSk
  let immobile = EM.findWithDefault 0 AbMove actorMaxSk <= 0
      tpos = bpos tb
      adj = checkAdjacent sb tb
      atWar = isAtWar tfact (bfid sb)
  if | not adj -> failSer DisplaceDistant
     | not (bproj tb) && atWar
       && actorDying tb ->
       failSer DisplaceDying
     | not (bproj tb) && atWar
       && braced tb ->
       failSer DisplaceBraced
     | not (bproj tb) && atWar
       && immobile ->
       failSer DisplaceImmobile
     | not disp && atWar ->
       failSer DisplaceSupported
     | otherwise -> do
       let lid = blid sb
       lvl <- getLevel lid
       -- Displacing requires full access.
       if Tile.isWalkable coTileSpeedup $ lvl `at` tpos then
         case posToAidsLvl tpos lvl of
           [] -> error $ "" `showFailure` (leader, sb, target, tb)
           [_] -> return $ Right $ ReqDisplace target
           _ -> failSer DisplaceProjectiles
       else failSer DisplaceAccess

-- | Leader moves or searches or alters. No visible actor at the position.
moveSearchAlter :: MonadClientUI m => Vector -> m (FailOrCmd RequestAnyAbility)
moveSearchAlter dir = do
  COps{coTileSpeedup} <- getsState scops
  leader <- getLeaderUI
  sb <- getsState $ getActorBody leader
  ar <- getsState $ getActorAspect leader
  actorSk <- leaderSkillsClientUI
  let calmE = calmEnough sb ar
      alterSkill = EM.findWithDefault 0 AbAlter actorSk
      applySkill = EM.findWithDefault 0 AbApply actorSk
      spos = bpos sb           -- source position
      tpos = spos `shift` dir  -- target position
  itemToF <- getsState $ flip itemToFull
  localTime <- getsState $ getLocalTime (blid sb)
  embeds <- getsState $ getEmbedBag (blid sb) tpos
  lvl <- getLevel $ blid sb
  let t = lvl `at` tpos
      alterMinSkill = Tile.alterMinSkill coTileSpeedup t
      canApplyEmbeds = any canApplyEmbed $ EM.assocs embeds
      canApplyEmbed (iid, kit) =
        let itemFull = itemToF iid
            legal = permittedApply localTime applySkill calmE itemFull kit
        -- Let even completely unskilled actors trigger basic embeds.
        in either (const False) (const True) legal
      modifiable = Tile.isDoor coTileSpeedup t
                   || Tile.isChangable coTileSpeedup t
                   || Tile.isSuspect coTileSpeedup t
  runStopOrCmd <-
    -- Movement requires full access.
    if | Tile.isWalkable coTileSpeedup t ->
         -- A potential invisible actor is hit. War started without asking.
         return $ Right $ RequestAnyAbility $ ReqMove dir
       -- No free access, so search and/or alter the tile.
       | not (modifiable || canApplyEmbeds) ->
           failWith "never mind"  -- misclick? related to AlterNothing
       | alterSkill <= 1 -> failSer AlterUnskilled
       | not (Tile.isSuspect coTileSpeedup t)
         && alterSkill < alterMinSkill -> failSer AlterUnwalked
       | EM.member tpos $ lfloor lvl -> failSer AlterBlockItem
       | not $ null $ posToAidsLvl tpos lvl -> failSer AlterBlockActor
       | otherwise -> do  -- promising
            verAlters <- verifyAlters (blid sb) tpos
            case verAlters of
              Right() -> return $ Right $ RequestAnyAbility $ ReqAlter tpos
              Left err -> return $ Left err
            -- We don't use ReqMove, because we don't hit invisible actors,
            -- e.g., hidden in a wall. If server performed an attack for free
            -- on the invisible actor anyway, the player (or AI)
            -- would be tempted to repeatedly hit random walls
            -- in hopes of killing a monster lurking within.
            -- If the action had a cost, misclicks would incur the cost, too.
            -- Right now the player may repeatedly alter tiles trying to learn
            -- about invisible pass-wall actors, but when an actor detected,
            -- it costs a turn and does not harm the invisible actors,
            -- so it's not so tempting.
  return $! runStopOrCmd

-- * RunOnceAhead

runOnceAheadHuman :: MonadClientUI m => m (Either MError ReqUI)
runOnceAheadHuman = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  leader <- getLeaderUI
  UIOptions{uRunStopMsgs} <- getsSession sUIOptions
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
      if uRunStopMsgs
      then weaveJust <$> failWith "run stop: automatic leader change"
      else return $ Left Nothing
    Just _runParams | keyPressed -> do
      discardPressedKey
      stopPlayBack
      if uRunStopMsgs
      then weaveJust <$> failWith "run stop: key pressed"
      else weaveJust <$> failWith "interrupted"
    Just runParams -> do
      arena <- getArenaUI
      runOutcome <- continueRun arena runParams
      case runOutcome of
        Left stopMsg -> do
          stopPlayBack
          if uRunStopMsgs
          then weaveJust <$> failWith ("run stop:" <+> stopMsg)
          else return $ Left Nothing
        Right runCmd ->
          return $ Right $ ReqUITimed runCmd

-- * MoveOnceToXhair

moveOnceToXhairHuman :: MonadClientUI m => m (FailOrCmd RequestAnyAbility)
moveOnceToXhairHuman = goToXhair True False

goToXhair :: MonadClientUI m => Bool -> Bool -> m (FailOrCmd RequestAnyAbility)
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
      Just c | c == bpos b ->
        if initialStep
        then return $ Right $ RequestAnyAbility ReqWait
        else failWith "position reached"
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
                failWith "no route to crosshair"
              _ | initialStep && adjacent (bpos b) c -> do
                let dir = towards (bpos b) c
                moveRunHuman initialStep True run False dir
              NoPath -> failWith "no route to crosshair"
              AndPath{pathList=[]} -> failWith "almost there"
              AndPath{pathList = p1 : _} -> do
                let finalGoal = p1 == c
                    dir = towards (bpos b) p1
                moveRunHuman initialStep finalGoal run False dir

multiActorGoTo :: MonadClientUI m
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
        s <- getState
        modifyClient $ updateLeader r s
        let runMembersNew = rs ++ [r]
            paramNew = paramOld { runMembers = runMembersNew
                                , runWaiting = 0}
        b <- getsState $ getActorBody r
        (bfs, mpath) <- getCacheBfsAndPath r c
        xhairMoused <- getsSession sxhairMoused
        case mpath of
          _ | xhairMoused && isNothing (accessBfs bfs c) ->
            failWith "no route to crosshair"
          NoPath -> failWith "no route to crosshair"
          AndPath{pathList=[]} -> failWith "almost there"
          AndPath{pathList = p1 : _} -> do
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

runOnceToXhairHuman :: MonadClientUI m => m (FailOrCmd RequestAnyAbility)
runOnceToXhairHuman = goToXhair True True

-- * ContinueToXhair

continueToXhairHuman :: MonadClientUI m => m (FailOrCmd RequestAnyAbility)
continueToXhairHuman = goToXhair False False{-irrelevant-}

-- * MoveItem

-- This cannot be structured as projecting or applying, with @ByItemMode@
-- and @ChooseItemToMove@, because at least in case of grabbing items,
-- more than one item is chosen, which doesn't fit @sitemSel@. Separating
-- grabbing of multiple items as a distinct command is too high a proce.
moveItemHuman :: forall m. MonadClientUI m
              => [CStore] -> CStore -> Maybe MU.Part -> Bool
              -> m (FailOrCmd (RequestTimed 'AbMoveItem))
moveItemHuman cLegalRaw destCStore mverb auto = do
  itemSel <- getsSession sitemSel
  modifySession $ \sess -> sess {sitemSel = Nothing}  -- prevent surprise
  case itemSel of
    Just (fromCStore, iid) | fromCStore /= destCStore
                             && fromCStore `elem` cLegalRaw -> do
      leader <- getLeaderUI
      b <- getsState $ getActorBody leader
      bag <- getsState $ getBodyStoreBag b fromCStore
      case iid `EM.lookup` bag of
        Nothing ->  -- the case of old selection or selection from another actor
          moveItemHuman cLegalRaw destCStore mverb auto
        Just (k, it) -> assert (k > 0) $ do
          itemFull <- getsState $ itemToFull iid
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
                let is = ( fromCStore
                         , [(iid, (itemFull, (kChosen, take kChosen it)))] )
                in moveItems cLegalRaw is destCStore
    _ -> do
      mis <- selectItemsToMove cLegalRaw destCStore mverb auto
      case mis of
        Left err -> return $ Left err
        Right (fromCStore, [(iid, _)]) | cLegalRaw /= [CGround] -> do
          modifySession $ \sess -> sess {sitemSel = Just (fromCStore, iid)}
          moveItemHuman cLegalRaw destCStore mverb auto
        Right is -> moveItems cLegalRaw is destCStore

selectItemsToMove :: forall m. MonadClientUI m
                  => [CStore] -> CStore -> Maybe MU.Part -> Bool
                  -> m (FailOrCmd (CStore, [(ItemId, ItemFullKit)]))
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
  ar <- getsState $ getActorAspect leader
  lastItemMove <- getsSession slastItemMove
  let calmE = calmEnough b ar
      cLegalE | calmE = cLegalRaw
              | destCStore == CSha = []
              | otherwise = delete CSha cLegalRaw
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
        if destCStore == CEqp && cLegalRaw /= [CGround]
        then (promptEqp, return $ SuitsSomething $ \itemFull _kit ->
               IK.goesIntoEqp $ itemKind itemFull)
        else (prompt, return SuitsEverything)
  ggi <- getFull psuit
                 (\_ _ _ cCur -> prompt <+> ppItemDialogModeFrom cCur)
                 (\_ _ _ cCur -> promptGeneric <+> ppItemDialogModeFrom cCur)
                 cLegalRaw cLegal (not auto) True
  case ggi of
    Right (l, (MStore fromCStore, _)) -> do
      modifySession $ \sess ->
        sess {slastItemMove = Just (fromCStore, destCStore)}
      return $ Right (fromCStore, l)
    Left err -> failWith err
    _ -> error $ "" `showFailure` ggi

moveItems :: forall m. MonadClientUI m
          => [CStore] -> (CStore, [(ItemId, ItemFullKit)]) -> CStore
          -> m (FailOrCmd (RequestTimed 'AbMoveItem))
moveItems cLegalRaw (fromCStore, l) destCStore = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  ar <- getsState $ getActorAspect leader
  discoBenefit <- getsClient sdiscoBenefit
  let calmE = calmEnough b ar
      ret4 :: [(ItemId, ItemFullKit)]
           -> Int -> [(ItemId, Int, CStore, CStore)]
           -> m (FailOrCmd [(ItemId, Int, CStore, CStore)])
      ret4 [] _ acc = return $ Right $ reverse acc
      ret4 ((iid, (itemFull, (itemK, _))) : rest) oldN acc = do
        let k = itemK
            !_A = assert (k > 0) ()
            retRec toCStore =
              let n = oldN + if toCStore == CEqp then k else 0
              in ret4 rest n ((iid, k, fromCStore, toCStore) : acc)
            inEqp = benInEqp $ discoBenefit EM.! iid
        if cLegalRaw == [CGround]  -- normal pickup
        then case destCStore of  -- @CEqp@ is the implicit default; refine:
          CEqp | calmE && IK.goesIntoSha (itemKind itemFull) ->
            retRec CSha
          CEqp | inEqp && eqpOverfull b (oldN + k) -> do
            -- If this stack doesn't fit, we don't equip any part of it,
            -- but we may equip a smaller stack later in the same pickup.
            let fullWarn = if eqpOverfull b (oldN + 1)
                           then EqpOverfull
                           else EqpStackFull
            msgAdd $ "Warning:" <+> showReqFailure fullWarn <> "."
            retRec $ if calmE then CSha else CInv
          CEqp | inEqp ->
            retRec CEqp
          CEqp ->
            retRec CInv
          _ ->
            retRec destCStore
        else case destCStore of  -- player forces store, so @inEqp@ ignored
          CEqp | eqpOverfull b (oldN + k) -> do
            -- If the chosen number from the stack doesn't fit,
            -- we don't equip any part of it and we exit item manipulation.
            let fullWarn = if eqpOverfull b (oldN + 1)
                           then EqpOverfull
                           else EqpStackFull
            failSer fullWarn
          _ -> retRec destCStore
  if not calmE && CSha `elem` [fromCStore, destCStore]
  then failSer ItemNotCalm
  else do
    l4 <- ret4 l 0 []
    return $! case l4 of
      Left err -> Left err
      Right [] -> error $ "" `showFailure` l
      Right lr -> Right $ ReqMoveItems lr

-- * Project

projectHuman :: MonadClientUI m => m (FailOrCmd (RequestTimed 'AbProject))
projectHuman = do
  itemSel <- getsSession sitemSel
  case itemSel of
    Just (fromCStore, iid) -> do
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

projectItem :: MonadClientUI m
            => (CStore, (ItemId, ItemFull))
            -> m (FailOrCmd (RequestTimed 'AbProject))
projectItem (fromCStore, (iid, itemFull)) = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  ar <- getsState $ getActorAspect leader
  let calmE = calmEnough b ar
  if not calmE && fromCStore == CSha then failSer ItemNotCalm
  else do
    mpsuitReq <- psuitReq
    case mpsuitReq of
      Left err -> failWith err
      Right psuitReqFun ->
        case psuitReqFun itemFull of
          Left reqFail -> failSer reqFail
          Right (pos, _) -> do
            -- Set personal target to the aim position, to easily repeat.
            mposTgt <- leaderTgtToPos
            unless (Just pos == mposTgt) $ do
              sxhair <- getsSession sxhair
              modifyClient $ updateTarget leader (const $ Just sxhair)
            -- Project.
            eps <- getsClient seps
            return $ Right $ ReqProject pos eps iid fromCStore

-- * Apply

applyHuman :: MonadClientUI m => m (FailOrCmd (RequestTimed 'AbApply))
applyHuman = do
  itemSel <- getsSession sitemSel
  case itemSel of
    Just (fromCStore, iid) -> do
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
          -> m (FailOrCmd (RequestTimed 'AbApply))
applyItem (fromCStore, (iid, (itemFull, kit))) = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  ar <- getsState $ getActorAspect leader
  let calmE = calmEnough b ar
  if not calmE && fromCStore == CSha then failSer ItemNotCalm
  else do
    p <- permittedApplyClient
    case p itemFull kit of
      Left reqFail -> failSer reqFail
      Right _ -> return $ Right $ ReqApply iid fromCStore

-- * AlterDir

-- | Ask for a direction and alter a tile in the specified way, if possible.
alterDirHuman :: MonadClientUI m
              => [TriggerTile] -> m (FailOrCmd (RequestTimed 'AbAlter))
alterDirHuman ts = do
  UIOptions{uVi, uLaptop} <- getsSession sUIOptions
  let verb1 = case ts of
        [] -> "alter"
        tr : _ -> ttverb tr
      keys = K.escKM
             : K.leftButtonReleaseKM
             : map (K.KM K.NoModifier) (K.dirAllKey uVi uLaptop)
      prompt = makePhrase
        ["Where to", verb1 <> "? [movement key] [pointer]"]
  promptAdd0 prompt
  slides <- reportToSlideshow [K.escKM]
  km <- getConfirms ColorFull keys slides
  case K.key km of
    K.LeftButtonRelease -> do
      leader <- getLeaderUI
      b <- getsState $ getActorBody leader
      Point x y <- getsSession spointer
      let dir = Point x (y -  mapStartY) `vectorToFrom` bpos b
      if isUnit dir
      then alterTile ts dir
      else failWith "never mind"
    _ ->
      case K.handleDir uVi uLaptop km of
        Nothing -> failWith "never mind"
        Just dir -> alterTile ts dir

-- | Try to alter a tile using a feature in the given direction.
alterTile :: MonadClientUI m
          => [TriggerTile] -> Vector -> m (FailOrCmd (RequestTimed 'AbAlter))
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
               -> m (FailOrCmd (RequestTimed 'AbAlter))
alterTileAtPos ts tpos pText = do
  cops@COps{cotile, coTileSpeedup} <- getsState scops
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorSk <- leaderSkillsClientUI
  lvl <- getLevel $ blid b
  embeds <- getsState $ getEmbedBag (blid b) tpos
  let alterSkill = EM.findWithDefault 0 AbAlter actorSk
      t = lvl `at` tpos
      alterMinSkill = Tile.alterMinSkill coTileSpeedup t
      hasFeat TriggerTile{ttfeature} = Tile.hasFeature cotile ttfeature t
      modifiable = Tile.isDoor coTileSpeedup t
                   || Tile.isChangable coTileSpeedup t
                   || Tile.isSuspect coTileSpeedup t
  case filter hasFeat ts of
    [] | not $ null ts -> failWith $ guessAlter cops ts t
    _ | not modifiable && EM.null embeds -> failSer AlterNothing
    _ | not $ adjacent tpos (bpos b) -> failSer AlterDistant
    _ | alterSkill <= 1 -> failSer AlterUnskilled
    _ | not (Tile.isSuspect coTileSpeedup t)
        && alterSkill < alterMinSkill -> failSer AlterUnwalked
    trs ->
      if EM.notMember tpos $ lfloor lvl then
        if null (posToAidsLvl tpos lvl) then do
          let v = case trs of
                [] -> "alter"
                tr : _ -> ttverb tr
          verAlters <- verifyAlters (blid b) tpos
          case verAlters of
            Right() -> do
              let msg = makeSentence ["you", v, MU.Text pText]
              msgAdd msg
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
     | null ks && not (Tile.isDoor coTileSpeedup t
                       || Tile.isChangable coTileSpeedup t
                       || Tile.isSuspect coTileSpeedup t) ->
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
    go <- displayYesNo ColorFull
            "This is the way out. Really leave now?"
    if not go then failWith "game resumed"
    else do
      (_, total) <- getsState $ calculateTotal side
      if total == 0 then do
        -- The player can back off at this step. We don't insist, because
        -- possibly the score formula doesn't reward treasure, or possibly
        -- the dungeon definition rules out treasure, or this particular
        -- dungeon has none by a fluke in dungeon generation.
        go1 <- displaySpaceEsc ColorBW
          "Afraid of the challenge? Leaving so soon and without any treasure?"
        if not go1
        then failWith "here's your chance!"
        else return $ Right ()
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
                      => [TriggerTile] -> m (FailOrCmd (RequestTimed 'AbAlter))
alterWithPointerHuman ts = do
  COps{cotile} <- getsState scops
  lidV <- viewedLevelUI
  lvl@Level{lxsize, lysize} <- getLevel lidV
  Point{..} <- getsSession spointer
  let tpos = Point px (py - mapStartY)
      t = lvl `at` tpos
  if px >= 0 && py - mapStartY >= 0
     && px < lxsize && py - mapStartY < lysize
  then
    alterTileAtPos ts tpos $ "the" <+> TK.tname (okind cotile t)
  else do
    stopPlayBack
    failWith "never mind"

-- * Help

-- | Display command help.
helpHuman :: MonadClientUI m
          => (HumanCmd -> m (Either MError ReqUI))
          -> m (Either MError ReqUI)
helpHuman cmdAction = do
  cops <- getsState scops
  lidV <- viewedLevelUI
  Level{lxsize, lysize} <- getLevel lidV
  keyb <- getsSession sbinding
  let keyH = keyHelp cops keyb 1
      splitHelp (t, okx) =
        splitOKX lxsize (lysize + 3) (textToAL t) [K.spaceKM, K.escKM] okx
      sli = toSlideshow $ concat $ map splitHelp keyH
  ekm <- displayChoiceScreen "help" ColorFull True sli [K.spaceKM, K.escKM]
  case ekm of
    Left km -> case km `M.lookup` bcmdMap keyb of
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
  lidV <- viewedLevelUI
  Level{lxsize, lysize} <- getLevel lidV
  keyb <- getsSession sbinding
  let keyL = 1
      (ov0, kxs0) = okxsN keyb 1 keyL (const False) False
                          CmdDashboard [] []
      al1 = textToAL "Dashboard"
      splitHelp (al, okx) = splitOKX lxsize (lysize + 1) al [K.escKM] okx
      sli = toSlideshow $ splitHelp (al1, (ov0, kxs0))
      extraKeys = [K.escKM]
  ekm <- displayChoiceScreen "dashboard" ColorFull False sli extraKeys
  case ekm of
    Left km -> case km `M.lookup` bcmdMap keyb of
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
  case itemSel of
    Just (fromCStore, iid) -> do
      leader <- getLeaderUI
      b <- getsState $ getActorBody leader
      bUI <- getsSession $ getActorUI leader
      bag <- getsState $ getBodyStoreBag b fromCStore
      case iid `EM.lookup` bag of
        Nothing -> weaveJust <$> failWith "no item to open item menu for"
        Just kit -> do
          ar <- getsState $ getActorAspect leader
          itemFull <- getsState $ itemToFull iid
          lidV <- viewedLevelUI
          Level{lxsize, lysize} <- getLevel lidV
          localTime <- getsState $ getLocalTime (blid b)
          found <- getsState $ findIid leader (bfid b) iid
          factionD <- getsState sfactionD
          sactorUI <- getsSession sactorUI
          let !_A = assert (not (null found) || fromCStore == CGround
                            `blame` (iid, leader)) ()
              fAlt (aid, (_, store)) = aid /= leader || store /= fromCStore
              foundAlt = filter fAlt found
              foundUI = map (\(aid, bs) ->
                               (aid, bs, sactorUI EM.! aid)) foundAlt
              foundKeys = map (K.KM K.NoModifier . K.Fun)
                              [1 .. length foundUI]  -- starting from 1!
              ppLoc bUI2 store =
                let phr = makePhrase $ ppCStoreWownW False store
                                     $ partActor bUI2
                in "[" ++ T.unpack phr ++ "]"
              foundTexts = map (\(_, (_, store), bUI2) ->
                                  ppLoc bUI2 store) foundUI
              foundPrefix = textToAL $
                if null foundTexts then "" else "The item is also in:"
              desc = itemDesc False (bfid b) factionD (IA.aHurtMelee ar)
                              fromCStore localTime itemFull kit
              alPrefix = splitAttrLine lxsize $ desc <+:> foundPrefix
              ystart = length alPrefix - 1
              xstart = length (last alPrefix) + 1
              ks = zip foundKeys $ map (\(_, (_, store), bUI2) ->
                                          ppLoc bUI2 store) foundUI
              (ovFoundRaw, kxsFound) = wrapOKX ystart xstart lxsize ks
              ovFound = glueLines alPrefix ovFoundRaw
          report <- getReportUI
          keyb <- getsSession sbinding
          let calmE = calmEnough b ar
              greyedOut cmd = not calmE && fromCStore == CSha || case cmd of
                MoveItem stores destCStore _ _ ->
                  fromCStore `notElem` stores
                  || not calmE && CSha == destCStore
                  || destCStore == CEqp && eqpOverfull b 1
                _ -> False  -- project and apply commands are too complex
              fmt n k h = " " <> T.justifyLeft n ' ' k <+> h
              keyL = 11
              keyCaption = fmt keyL "keys" "command"
              offset = 1 + length ovFound
              (ov0, kxs0) = okxsN keyb offset keyL greyedOut True
                                  CmdItemMenu [keyCaption] []
              t0 = makeSentence [ MU.SubjectVerbSg (partActor bUI) "choose"
                                , "an item", MU.Text $ ppCStoreIn fromCStore ]
              al1 = renderReport report <+:> textToAL t0
              splitHelp (al, okx) =
                splitOKX lxsize (lysize + 1) al [K.spaceKM, K.escKM] okx
              sli = toSlideshow
                    $ splitHelp (al1, (ovFound ++ ov0, kxsFound ++ kxs0))
              extraKeys = [K.spaceKM, K.escKM] ++ foundKeys
          recordHistory  -- report shown (e.g., leader switch), save to history
          ekm <- displayChoiceScreen "item menu" ColorFull False sli extraKeys
          case ekm of
            Left km -> case km `M.lookup` bcmdMap keyb of
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
                         sess {sitemSel = Just (newCStore, iid)}
                       itemMenuHuman cmdAction
                _ -> error $ "" `showFailure` km
              Just (_desc, _cats, cmd) -> cmdAction cmd
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

artAtSize :: MonadClientUI m => m [Text]
artAtSize = do
  cops <- getsState scops
  let stdRuleset = getStdRuleset cops
      lxsize = fst normalLevelBound + 1
      lysize = snd normalLevelBound + 4
      xoffset = (110 - lxsize) `div` 2
      yoffset = (60 - lysize) `div` 2
      tlines = T.lines $ rmainMenuArt stdRuleset
      f = T.take lxsize . T.drop xoffset
  return $! map f $ take lysize $ drop yoffset tlines

-- We detect the place for the version string by searching for 'Version'
-- in the last line of the picture. If it doesn't fit, we shift, if everything
-- else fails, only then we crop. We don't assume any line length.
artWithVersion :: MonadClientUI m => m [String]
artWithVersion = do
  cops <- getsState scops
  let stdRuleset = getStdRuleset cops
      pasteVersion :: [Text] -> [String]
      pasteVersion art =
        let exeVersion = rexeVersion stdRuleset
            libVersion = Self.version
            version = "Version " ++ showVersion exeVersion
                      ++ " (frontend: " ++ frontendName
                      ++ ", engine: LambdaHack " ++ showVersion libVersion
                      ++ ") "
            versionLen = length version
            lastOriginal = last art
            (prefix, versionSuffix) = T.breakOn "Version" lastOriginal
            suffix = drop versionLen $ T.unpack versionSuffix
            overfillLen = versionLen - T.length versionSuffix
            prefixModified = T.unpack $ T.dropEnd overfillLen prefix
            lastModified = prefixModified ++ version ++ suffix
        in map T.unpack (init art) ++ [lastModified]
  mainMenuArt <- artAtSize
  return $! pasteVersion mainMenuArt

generateMenu :: MonadClientUI m
             => (HumanCmd -> m (Either MError ReqUI))
             -> [(K.KM, (Text, HumanCmd))] -> [String] -> String
             -> m (Either MError ReqUI)
generateMenu cmdAction kds gameInfo menuName = do
  art <- artWithVersion
  let bindingLen = 30
      emptyInfo = repeat $ replicate bindingLen ' '
      bindings =  -- key bindings to display
        let fmt (k, (d, _)) =
              ( Just k
              , T.unpack
                $ T.justifyLeft bindingLen ' '
                    $ T.justifyLeft 3 ' ' (T.pack $ K.showKM k) <> " " <> d )
        in map fmt kds
      overwrite :: [(Int, String)] -> [(String, Maybe KYX)]
      overwrite =  -- overwrite the art with key bindings and other lines
        let over [] (_, line) = ([], (line, Nothing))
            over bs@((mkey, binding) : bsRest) (y, line) =
              let (prefix, lineRest) = break (=='{') line
                  (braces, suffix)   = span  (=='{') lineRest
              in if length braces >= bindingLen
                 then
                   let lenB = length binding
                       post = drop (lenB - length braces) suffix
                       len = length prefix
                       yxx key = (Left [key], (y, len, len + lenB))
                       myxx = yxx <$> mkey
                   in (bsRest, (prefix <> binding <> post, myxx))
                 else (bs, (line, Nothing))
        in snd . mapAccumL over (zip (repeat Nothing) gameInfo
                                 ++ bindings
                                 ++ zip (repeat Nothing) emptyInfo)
      menuOverwritten = overwrite $ zip [0..] art
      (menuOvLines, mkyxs) = unzip menuOverwritten
      kyxs = catMaybes mkyxs
      ov = map stringToAL menuOvLines
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
  Binding{bcmdList} <- getsSession sbinding
  gameMode <- getGameMode
  snxtScenario <- getsClient snxtScenario
  let nxtGameName = mname $ nxtGameMode cops snxtScenario
      tnextScenario = "pick next:" <+> nxtGameName
      -- Key-description-command tuples.
      kds = (K.mkKM "p", (tnextScenario, GameScenarioIncr))
            : [ (km, (desc, cmd))
              | (km, ([CmdMainMenu], desc, cmd)) <- bcmdList ]
      bindingLen = 30
      gameName = mname gameMode
      gameInfo = map T.unpack
                   [ T.justifyLeft bindingLen ' ' ""
                   , T.justifyLeft bindingLen ' '
                     $ "Now playing:" <+> gameName
                   , T.justifyLeft bindingLen ' ' "" ]
  generateMenu cmdAction kds gameInfo "main"

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
  factTactic <- getsState $ ftactic . gplayer . (EM.! side) . sfactionD
  let offOn b = if b then "on" else "off"
      offOnAll n = case n of
        0 -> "low"
        1 -> "medium"
        2 -> "high"
        _ -> error $ "" `showFailure` n
      tsuspect = "suspect terrain:" <+> offOnAll markSuspect
      tvisible = "visible zone:" <+> offOn markVision
      tsmell = "smell clues:" <+> offOn  markSmell
      thenchmen = "tactic:" <+> tshow factTactic
      -- Key-description-command tuples.
      kds = [ (K.mkKM "s", (tsuspect, MarkSuspect))
            , (K.mkKM "v", (tvisible, MarkVision))
            , (K.mkKM "c", (tsmell, MarkSmell))
            , (K.mkKM "t", (thenchmen, Tactic))
            , (K.mkKM "Escape", ("back to main menu", MainMenu)) ]
      bindingLen = 30
      gameInfo = map T.unpack
                   [ T.justifyLeft bindingLen ' ' ""
                   , T.justifyLeft bindingLen ' ' "Convenience settings:"
                   , T.justifyLeft bindingLen ' ' "" ]
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
      tcurDiff = "*   difficulty:" <+> tshow (cdiff curChal)
      tnextDiff = "difficulty:" <+> tshow (cdiff nxtChal)
      tcurWolf = "*   lone wolf:"
                 <+> offOn (cwolf curChal)
      tnextWolf = "lone wolf:"
                  <+> offOn (cwolf nxtChal)
      tcurFish = "*   cold fish:"
                 <+> offOn (cfish curChal)
      tnextFish = "cold fish:"
                  <+> offOn (cfish nxtChal)
      -- Key-description-command tuples.
      kds = [ (K.mkKM "d", (tnextDiff, GameDifficultyIncr))
            , (K.mkKM "w", (tnextWolf, GameWolfToggle))
            , (K.mkKM "f", (tnextFish, GameFishToggle))
            , (K.mkKM "Escape", ("back to main menu", MainMenu)) ]
      bindingLen = 30
      gameInfo = map T.unpack
                   [ T.justifyLeft bindingLen ' ' "Current challenges:"
                   , T.justifyLeft bindingLen ' ' ""
                   , T.justifyLeft bindingLen ' ' tcurDiff
                   , T.justifyLeft bindingLen ' ' tcurWolf
                   , T.justifyLeft bindingLen ' ' tcurFish
                   , T.justifyLeft bindingLen ' ' ""
                   , T.justifyLeft bindingLen ' ' "Next game challenges:"
                   , T.justifyLeft bindingLen ' ' "" ]
  generateMenu cmdAction kds gameInfo "challenge"

-- * GameScenarioIncr

gameScenarioIncr :: MonadClientUI m => m ()
gameScenarioIncr =
  modifyClient $ \cli -> cli {snxtScenario = snxtScenario cli + 1}

-- * GameDifficultyIncr

gameDifficultyIncr :: MonadClientUI m => m ()
gameDifficultyIncr = do
  nxtDiff <- getsClient $ cdiff . snxtChal
  let delta = 1
      d | nxtDiff + delta > difficultyBound = 1
        | nxtDiff + delta < 1 = difficultyBound
        | otherwise = nxtDiff + delta
  modifyClient $ \cli -> cli {snxtChal = (snxtChal cli) {cdiff = d} }

-- * GameWolfToggle

gameWolfToggle :: MonadClientUI m => m ()
gameWolfToggle =
  modifyClient $ \cli ->
    cli {snxtChal = (snxtChal cli) {cwolf = not (cwolf (snxtChal cli))} }

-- * GameFishToggle

gameFishToggle :: MonadClientUI m => m ()
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
              [ "yea, would be a pity to leave them all to die"
              , "yea, a shame to get your team stranded" ]
    failWith msg2

nxtGameMode :: COps -> Int -> ModeKind
nxtGameMode COps{comode} snxtScenario =
  let f acc _p _i a = a : acc
      campaignModes = ofoldlGroup' comode "campaign scenario" f []
  in campaignModes !! (snxtScenario `mod` length campaignModes)

-- * GameExit

gameExitHuman :: MonadClientUI m => m ReqUI
gameExitHuman = do
  -- Announce before the saving started, since it can take a while.
  promptAdd1 "Saving game. The program stops now."
  return ReqUIGameSaveAndExit

-- * GameSave

gameSaveHuman :: MonadClientUI m => m ReqUI
gameSaveHuman = do
  -- Announce before the saving started, since it can take a while.
  promptAdd1 "Saving game backup."
  return ReqUIGameSave

-- * Tactic

-- Note that the difference between seek-target and follow-the-leader tactic
-- can influence even a faction with passive actors. E.g., if a passive actor
-- has an extra active skill from equipment, he moves every turn.
tacticHuman :: MonadClientUI m => m (FailOrCmd ReqUI)
tacticHuman = do
  fid <- getsClient sside
  fromT <- getsState $ ftactic . gplayer . (EM.! fid) . sfactionD
  let toT = if fromT == maxBound then minBound else succ fromT
  go <- displaySpaceEsc ColorFull
        $ "(Beware, work in progress!)"
          <+> "Current henchmen tactic is" <+> tshow fromT
          <+> "(" <> describeTactic fromT <> ")."
          <+> "Switching tactic to" <+> tshow toT
          <+> "(" <> describeTactic toT <> ")."
          <+> "This clears targets of all henchmen (non-leader teammates)."
          <+> "New targets will be picked according to new tactic."
  if not go
  then failWith "tactic change canceled"
  else return $ Right $ ReqUITactic toT

-- * Automate

automateHuman :: MonadClientUI m => m (FailOrCmd ReqUI)
automateHuman = do
  clearAimMode
  go <- displaySpaceEsc ColorBW
          "Ceding control to AI (press ESC to regain)."
  if not go
  then failWith "automation canceled"
  else return $ Right ReqUIAutomate
