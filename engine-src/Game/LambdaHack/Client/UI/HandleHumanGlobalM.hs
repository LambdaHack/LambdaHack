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
  , settingsMenuHuman, challengeMenuHuman
  , gameTutorialToggle, gameDifficultyIncr
  , gameFishToggle, gameGoodsToggle, gameWolfToggle, gameKeeperToggle
  , gameScenarioIncr
    -- * Global commands that never take time
  , gameRestartHuman, gameQuitHuman, gameDropHuman, gameExitHuman, gameSaveHuman
  , doctrineHuman, automateHuman, automateToggleHuman, automateBackHuman
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , areaToRectangles, meleeAid, displaceAid, moveSearchAlter, alterCommon
  , goToXhair, goToXhairExplorationMode, goToXhairGoTo
  , multiActorGoTo, moveOrSelectItem, selectItemsToMove, moveItems
  , projectItem, applyItem, alterTileAtPos, verifyAlters, processTileActions
  , verifyEscape, verifyToolEffect, closeTileAtPos, msgAddDone, pickPoint
  , generateMenu
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Char as Char
import           Data.Either (isLeft)
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
import           Game.LambdaHack.Client.UI.HandleHelperM
import           Game.LambdaHack.Client.UI.HandleHumanLocalM
import           Game.LambdaHack.Client.UI.HumanCmd
import           Game.LambdaHack.Client.UI.InventoryM
import           Game.LambdaHack.Client.UI.ItemDescription
import           Game.LambdaHack.Client.UI.ItemSlot (SlotChar (SlotChar))
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.KeyBindings
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.MsgM
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.PointUI
import           Game.LambdaHack.Client.UI.RunM
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.Slideshow
import           Game.LambdaHack.Client.UI.SlideshowM
import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Area
import           Game.LambdaHack.Common.ClientOptions
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
import qualified Game.LambdaHack.Content.ModeKind as MK
import           Game.LambdaHack.Content.RuleKind
import qualified Game.LambdaHack.Content.TileKind as TK
import qualified Game.LambdaHack.Core.Dice as Dice
import           Game.LambdaHack.Core.Random
import qualified Game.LambdaHack.Definition.Ability as Ability
import qualified Game.LambdaHack.Definition.Color as Color
import           Game.LambdaHack.Definition.Defs
import qualified Game.LambdaHack.Definition.DefsInternal as DefsInternal

-- * ByArea

-- | Pick command depending on area the mouse pointer is in.
-- The first matching area is chosen. If none match, only interrupt.
byAreaHuman :: MonadClientUI m
            => (K.KM -> HumanCmd -> m (Either MError ReqUI))
            -> [(CmdArea, HumanCmd)]
            -> m (Either MError ReqUI)
byAreaHuman cmdSemInCxtOfKM l = do
  CCUI{coinput=InputContent{brevMap}} <- getsSession sccui
  pUI <- getsSession spointer
  let PointSquare px py = uiToSquare pUI
      p = Point {..}  -- abuse of convention: @Point@, not @PointSquare@ used
                      -- for the whole UI screen in square font coordinates
      pointerInArea a = do
        rs <- areaToRectangles a
        return $! any (inside p) $ catMaybes rs
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
    mleader <- getsClient sleader
    case mleader of
      Nothing -> return []
      Just leader -> do
        b <- getsState $ getActorBody leader
        let PointSquare x y = mapToSquare $ bpos b
        return [(x, y, x, y)]
  CaMapParty -> do  -- takes preference over @CaMap@
    lidV <- viewedLevelUI
    side <- getsClient sside
    ours <- getsState $ filter (not . bproj) . map snd
                        . actorAssocs (== side) lidV
    let rectFromB p = let PointSquare x y = mapToSquare p
                      in (x, y, x, y)
    return $! map (rectFromB . bpos) ours
  CaMap ->
    let PointSquare xo yo = mapToSquare originPoint
        PointSquare xe ye = mapToSquare $ Point (rwidth - 1) (rheight - 4)
    in return [(xo, yo, xe, ye)]
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
  sreqDelay <- getsSession sreqDelay
  -- When server query delay is handled, don't complicate things by clearing
  -- screen instead of running the command.
  if sreportNull || sreqDelay == ReqDelayHandled
  then c1
  else return $ Left Nothing

-- * Wait

-- | Leader waits a turn (and blocks, etc.).
waitHuman :: MonadClientUI m => ActorId -> m (FailOrCmd RequestTimed)
waitHuman leader = do
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  if Ability.getSk Ability.SkWait actorCurAndMaxSk > 0 then do
    modifySession $ \sess -> sess {swaitTimes = abs (swaitTimes sess) + 1}
    return $ Right ReqWait
  else failSer WaitUnskilled

-- * Wait10

-- | Leader waits a 1/10th of a turn (and doesn't block, etc.).
waitHuman10 :: MonadClientUI m => ActorId -> m (FailOrCmd RequestTimed)
waitHuman10 leader = do
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  if Ability.getSk Ability.SkWait actorCurAndMaxSk >= 4 then do
    modifySession $ \sess -> sess {swaitTimes = abs (swaitTimes sess) + 1}
    return $ Right ReqWait10
  else failSer WaitUnskilled

-- * Yell

-- | Leader yells or yawns, if sleeping.
yellHuman :: MonadClientUI m => ActorId -> m (FailOrCmd RequestTimed)
yellHuman leader = do
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  if Ability.getSk Ability.SkWait actorCurAndMaxSk > 0
     -- If waiting drained and really, potentially, no other possible action,
     -- still allow yelling.
     || Ability.getSk Ability.SkMove actorCurAndMaxSk <= 0
     || Ability.getSk Ability.SkDisplace actorCurAndMaxSk <= 0
     || Ability.getSk Ability.SkMelee actorCurAndMaxSk <= 0
  then return $ Right ReqYell
  else failSer WaitUnskilled

-- * MoveDir and RunDir

moveRunHuman :: (MonadClient m, MonadClientUI m)
             => ActorId -> Bool -> Bool -> Bool -> Bool -> Vector
             -> m (FailOrCmd RequestTimed)
moveRunHuman leader initialStep finalGoal run runAhead dir = do
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  arena <- getArenaUI
  sb <- getsState $ getActorBody leader
  fact <- getsState $ (EM.! bfid sb) . sfactionD
  -- Start running in the given direction. The first turn of running
  -- succeeds much more often than subsequent turns, because we ignore
  -- most of the disturbances, since the player is mostly aware of them
  -- and still explicitly requests a run, knowing how it behaves.
  sel <- getsSession sselected
  let runMembers = if runAhead || noRunWithMulti fact
                   then [leader]
                   else ES.elems (ES.delete leader sel) ++ [leader]
      runParams = RunParams { runLeader = leader
                            , runMembers
                            , runInitial = True
                            , runStopMsg = Nothing
                            , runWaiting = 0 }
      initRunning = when (initialStep && run) $ do
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
      runStopOrCmd <- moveSearchAlter leader run dir
      case runStopOrCmd of
        Left stopMsg -> return $ Left stopMsg
        Right runCmd -> do
          -- Don't check @initialStep@ and @finalGoal@
          -- and don't stop going to target: door opening is mundane enough.
          initRunning
          return $ Right runCmd
    [(target, _)] | run
                    && initialStep
                    && Ability.getSk Ability.SkDisplace actorCurAndMaxSk > 0 ->
      -- No @stopPlayBack@: initial displace is benign enough.
      -- Displacing requires accessibility, but it's checked later on.
      displaceAid leader target
    _ : _ : _ | run
                && initialStep
                && Ability.getSk Ability.SkDisplace actorCurAndMaxSk > 0 ->
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
      failWith "the pointman switched by bumping"
    (target, tb) : _ | not run
                       && initialStep && finalGoal
                       && (bfid tb /= bfid sb || bproj tb) -> do
      stopPlayBack  -- don't ever auto-repeat melee
      if Ability.getSk Ability.SkMelee actorCurAndMaxSk > 0
      then -- No problem if there are many projectiles at the spot. We just
           -- attack the first one.
           meleeAid leader target
      else failSer MeleeUnskilled
    _ : _ -> failWith "actor in the way"

-- | Actor attacks an enemy actor or his own projectile.
meleeAid :: (MonadClient m, MonadClientUI m)
         => ActorId -> ActorId -> m (FailOrCmd RequestTimed)
meleeAid leader target = do
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
            => ActorId -> ActorId -> m (FailOrCmd RequestTimed)
displaceAid leader target = do
  COps{coTileSpeedup} <- getsState scops
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
                => ActorId -> Bool -> Vector -> m (FailOrCmd RequestTimed)
moveSearchAlter leader run dir = do
  COps{coTileSpeedup} <- getsState scops
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  sb <- getsState $ getActorBody leader
  let moveSkill = Ability.getSk Ability.SkMove actorCurAndMaxSk
      spos = bpos sb           -- source position
      tpos = spos `shift` dir  -- target position
  alterable <- getsState $ tileAlterable (blid sb) tpos
  lvl <- getLevel $ blid sb
  let t = lvl `at` tpos
  runStopOrCmd <-
    if Tile.isWalkable coTileSpeedup t then  -- Movement requires full access.
      if | moveSkill > 0 ->
             -- A potential invisible actor is hit. War started without asking.
             return $ Right $ ReqMove dir
         | bwatch sb == WSleep -> failSer MoveUnskilledAsleep
         | otherwise -> failSer MoveUnskilled
    else do  -- Not walkable, so search and/or alter the tile.
      let sxhair = Just $ TPoint TUnknown (blid sb) tpos
      -- Point xhair to see details with `~`.
      setXHairFromGUI sxhair
      if run then do
        -- Explicit request to examine the terrain.
        blurb <- lookAtPosition tpos (blid sb)
        mapM_ (uncurry msgAdd) blurb
        failWith $ "the terrain is" <+>
          if | Tile.isModifiable coTileSpeedup t -> "potentially modifiable"
             | alterable -> "potentially triggerable"
             | otherwise -> "completely inert"
      else alterCommon leader True tpos
  return $! runStopOrCmd

alterCommon :: MonadClientUI m
            => ActorId -> Bool -> Point -> m (FailOrCmd RequestTimed)
alterCommon leader bumping tpos = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  cops@COps{cotile, coTileSpeedup} <- getsState scops
  side <- getsClient sside
  factionD <- getsState sfactionD
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  sb <- getsState $ getActorBody leader
  let alterSkill = Ability.getSk Ability.SkAlter actorCurAndMaxSk
      spos = bpos sb
  alterable <- getsState $ tileAlterable (blid sb) tpos
  lvl <- getLevel $ blid sb
  localTime <- getsState $ getLocalTime (blid sb)
  embeds <- getsState $ getEmbedBag (blid sb) tpos
  itemToF <- getsState $ flip itemToFull
  getKind <- getsState $ flip getIidKind
  let t = lvl `at` tpos
      underFeet = tpos == spos  -- if enter and alter, be more permissive
      modificationFailureHint = msgAdd MsgTutorialHint "Some doors can be opened, stairs unbarred, treasures recovered, only if you find tools that increase your terrain modification ability and act as keys to the puzzle. To gather clues about the keys, listen to what's around you, examine items, inspect terrain, trigger, bump and harass. Once you uncover a likely tool, wield it, return and try to break through again."
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
       && alterSkill <= 1 -> do
         modificationFailureHint
         failSer AlterUnskilled
     | not (Tile.isSuspect coTileSpeedup t)
       && not underFeet
       && alterSkill < Tile.alterMinSkill coTileSpeedup t -> do
         -- Rather rare (requires high skill), so describe the tile.
         blurb <- lookAtPosition tpos (blid sb)
         mapM_ (uncurry msgAdd) blurb
         modificationFailureHint
         failSer AlterUnwalked
     | chessDist tpos (bpos sb) > 1 ->
         -- Checked late to give useful info about distant tiles.
         failSer AlterDistant
     | not underFeet
       && (occupiedBigLvl tpos lvl || occupiedProjLvl tpos lvl) ->
         -- Don't mislead describing terrain, if other actor is to blame.
         failSer AlterBlockActor
     | otherwise -> do  -- promising
         verAlters <- verifyAlters leader bumping tpos
         case verAlters of
           Right () ->
             if bumping then
               return $ Right $ ReqMove $ vectorToFrom tpos spos
             else do
               msgAddDone leader tpos "modify"
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

runOnceAheadHuman :: MonadClientUI m
                  => ActorId -> m (Either MError RequestTimed)
runOnceAheadHuman leader = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  keyPressed <- anyKeyPressed
  srunning <- getsSession srunning
  -- When running, stop if disturbed. If not running, stop at once.
  case srunning of
    Nothing -> do
      msgAdd MsgRunStopReason "run stop: nothing to do"
      return $ Left Nothing
    Just RunParams{runMembers}
      | noRunWithMulti fact && runMembers /= [leader] -> do
      msgAdd MsgRunStopReason "run stop: automatic pointman change"
      return $ Left Nothing
    Just _runParams | keyPressed -> do
      discardPressedKey
      msgAdd MsgRunStopReason "run stop: key pressed"
      weaveJust <$> failWith "interrupted"
    Just runParams -> do
      arena <- getArenaUI
      runOutcome <- continueRun arena runParams
      case runOutcome of
        Left stopMsg -> do
          msgAdd MsgRunStopReason ("run stop:" <+> stopMsg)
          return $ Left Nothing
        Right runCmd ->
          return $ Right runCmd

-- * MoveOnceToXhair

moveOnceToXhairHuman :: (MonadClient m, MonadClientUI m)
                     => ActorId -> m (FailOrCmd RequestTimed)
moveOnceToXhairHuman leader = goToXhair leader True False

goToXhair :: (MonadClient m, MonadClientUI m)
          => ActorId -> Bool -> Bool -> m (FailOrCmd RequestTimed)
goToXhair leader initialStep run = do
  aimMode <- getsSession saimMode
  -- Movement is legal only outside aiming mode.
  if isJust aimMode
  then failWith "cannot move in aiming mode"
  else goToXhairExplorationMode leader initialStep run

goToXhairExplorationMode :: (MonadClient m, MonadClientUI m)
                         => ActorId -> Bool -> Bool
                         -> m (FailOrCmd RequestTimed)
goToXhairExplorationMode leader initialStep run = do
  xhair <- getsSession sxhair
  xhairGoTo <- getsSession sxhairGoTo
  mfail <-
    if not (isNothing xhairGoTo) && xhairGoTo /= xhair
    then failWith "crosshair position changed"
    else do
      when (isNothing xhairGoTo) $  -- set it up for next steps
        modifySession $ \sess -> sess {sxhairGoTo = xhair}
      goToXhairGoTo leader initialStep run
  when (isLeft mfail) $
    modifySession $ \sess -> sess {sxhairGoTo = Nothing}
  return mfail

goToXhairGoTo :: (MonadClient m, MonadClientUI m)
              => ActorId -> Bool -> Bool -> m (FailOrCmd RequestTimed)
goToXhairGoTo leader initialStep run = do
  b <- getsState $ getActorBody leader
  mxhairPos <- mxhairToPos
  case mxhairPos of
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
              moveRunHuman leader initialStep finalGoal run False dir
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
              moveRunHuman leader initialStep True run False dir
            Nothing -> failWith "no route to crosshair"
            Just AndPath{pathList=[]} -> failWith "almost there"
            Just AndPath{pathList = p1 : _} -> do
              let finalGoal = p1 == c
                  dir = towards (bpos b) p1
              moveRunHuman leader initialStep finalGoal run False dir

multiActorGoTo :: (MonadClient m, MonadClientUI m)
               => LevelId -> Point -> RunParams -> m (FailOrCmd (Bool, Vector))
multiActorGoTo arena c paramOld =
  case paramOld of
    RunParams{runMembers = []} -> failWith "selected actors no longer there"
    RunParams{runMembers = r : rs, runWaiting} -> do
      onLevel <- getsState $ memActor r arena
      b <- getsState $ getActorBody r
      mxhairPos <- mxhairToPos
      if not onLevel || mxhairPos == Just (bpos b) then do
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
                    => ActorId -> m (FailOrCmd RequestTimed)
runOnceToXhairHuman leader = goToXhair leader True True

-- * ContinueToXhair

continueToXhairHuman :: (MonadClient m, MonadClientUI m)
                     => ActorId -> m (FailOrCmd RequestTimed)
continueToXhairHuman leader = goToXhair leader False False{-irrelevant-}

-- * MoveItem

moveItemHuman :: forall m. MonadClientUI m
              => ActorId -> [CStore] -> CStore -> Maybe Text -> Bool
              -> m (FailOrCmd RequestTimed)
moveItemHuman leader stores destCStore mverb auto = do
  let !_A = assert (destCStore `notElem` stores) ()
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  if Ability.getSk Ability.SkMoveItem actorCurAndMaxSk > 0
  then moveOrSelectItem leader stores destCStore mverb auto
  else failSer MoveItemUnskilled

-- This cannot be structured as projecting or applying, with @ByItemMode@
-- and @ChooseItemToMove@, because at least in case of grabbing items,
-- more than one item is chosen, which doesn't fit @sitemSel@. Separating
-- grabbing of multiple items as a distinct command is too high a price.
moveOrSelectItem :: forall m. MonadClientUI m
                 => ActorId -> [CStore] -> CStore -> Maybe Text -> Bool
                 -> m (FailOrCmd RequestTimed)
moveOrSelectItem leader storesRaw destCStore mverb auto = do
  b <- getsState $ getActorBody leader
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid b
  let calmE = calmEnough b actorCurAndMaxSk
      overStash = mstash == Just (blid b, bpos b)
      stores = case storesRaw of
        CEqp : rest@(_ : _) | not calmE -> rest ++ [CEqp]
        CGround : rest@(_ : _) | overStash -> rest ++ [CGround]
        _ -> storesRaw
  itemSel <- getsSession sitemSel
  modifySession $ \sess -> sess {sitemSel = Nothing}  -- prevent surprise
  case itemSel of
    _ | stores == [CGround] && overStash ->
      failWith "you can't loot items from your own stash"
    Just (_, fromCStore@CEqp, _) | fromCStore /= destCStore
                                   && fromCStore `elem` stores
                                   && not calmE ->
      failWith "neither the selected item nor any other can be unequipped"
    Just (_, fromCStore@CGround, _) | fromCStore /= destCStore
                                      && fromCStore `elem` stores
                                      && overStash ->
      failWith "you vainly paw through your own hoard"
    Just (iid, fromCStore, _) | fromCStore /= destCStore
                                && fromCStore `elem` stores -> do
      bag <- getsState $ getBodyStoreBag b fromCStore
      case iid `EM.lookup` bag of
        Nothing ->  -- the case of old selection or selection from another actor
          moveOrSelectItem leader stores destCStore mverb auto
        Just (k, it) -> assert (k > 0) $ do
          let eqpFree = eqpFreeN b
              kToPick | destCStore == CEqp = min eqpFree k
                      | otherwise = k
          if | destCStore == CEqp && not calmE -> failSer ItemNotCalm
             | destCStore == CGround && overStash -> failSer ItemOverStash
             | kToPick == 0 -> failWith "no more items can be equipped"
             | otherwise -> do
               socK <- pickNumber (not auto) kToPick
               case socK of
                 Left Nothing ->
                   moveOrSelectItem leader stores destCStore mverb auto
                 Left (Just err) -> return $ Left err
                 Right kChosen ->
                   let is = (fromCStore, [(iid, (kChosen, take kChosen it))])
                   in Right <$> moveItems leader stores is destCStore
    _ -> do
      mis <- selectItemsToMove leader stores destCStore mverb auto
      case mis of
        Left err -> return $ Left err
        Right (fromCStore, [(iid, _)]) | stores /= [CGround] -> do
          modifySession $ \sess ->
            sess {sitemSel = Just (iid, fromCStore, False)}
          moveOrSelectItem leader stores destCStore mverb auto
        Right is@(fromCStore, _) ->
          if | fromCStore == CEqp && not calmE -> failSer ItemNotCalm
             | fromCStore == CGround && overStash -> failSer ItemOverStash
             | otherwise -> Right <$> moveItems leader stores is destCStore

selectItemsToMove :: forall m. MonadClientUI m
                  => ActorId -> [CStore] -> CStore -> Maybe Text -> Bool
                  -> m (FailOrCmd (CStore, [(ItemId, ItemQuant)]))
selectItemsToMove leader stores destCStore mverb auto = do
  let verb = fromMaybe (verbCStore destCStore) mverb
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  b <- getsState $ getActorBody leader
  mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid b
  lastItemMove <- getsSession slastItemMove
  -- This calmE is outdated when one of the items increases max Calm
  -- (e.g., in pickup, which handles many items at once), but this is OK,
  -- the server accepts item movement based on calm at the start, not end
  -- or in the middle.
  -- The calmE is inaccurate also if an item not IDed, but that's intended
  -- and the server will ignore and warn (and content may avoid that,
  -- e.g., making all rings identified)
  let calmE = calmEnough b actorCurAndMaxSk
      overStash = mstash == Just (blid b, bpos b)
  if | destCStore == CEqp && not calmE -> failSer ItemNotCalm
     | destCStore == CGround && overStash -> failSer ItemOverStash
     | destCStore == CEqp && eqpOverfull b 1 -> failSer EqpOverfull
     | otherwise -> do
       let storesLast = case lastItemMove of
             Just (lastFrom, lastDest) | lastDest == destCStore
                                         && lastFrom `elem` stores ->
               lastFrom : delete lastFrom stores
             _ -> stores
           prompt = "What to"
           promptEqp = "What consumable to"
           eqpItemsN body =
             let n = sum $ map fst $ EM.elems $ beqp body
             in "(" <> makePhrase [MU.CarWs n "item"]
           ppItemDialogBody body actorSk cCur = case cCur of
             MStore CEqp | not $ calmEnough body actorSk ->
               "distractedly paw at" <+> ppItemDialogModeIn cCur
             MStore CGround | mstash == Just (blid body, bpos body) ->
               "greedily fondle" <+> ppItemDialogModeIn cCur
             _ -> case destCStore of
               CEqp | not $ calmEnough body actorSk ->
                 "distractedly attempt to" <+> verb
                 <+> ppItemDialogModeFrom cCur
               CEqp | eqpOverfull body 1 ->
                 "attempt to fit into equipment" <+> ppItemDialogModeFrom cCur
               CGround | mstash == Just (blid body, bpos body) ->
                 "greedily attempt to" <+> verb <+> ppItemDialogModeFrom cCur
               CEqp -> verb
                       <+> eqpItemsN body <+> "so far)"
                       <+> ppItemDialogModeFrom cCur
               _ -> verb <+> ppItemDialogModeFrom cCur
                    <+> if cCur == MStore CEqp
                        then eqpItemsN body <+> "now)"
                        else ""
           (promptGeneric, psuit) =
             -- We prune item list only for eqp, because other stores don't have
             -- so clear cut heuristics. So when picking up a stash, either grab
             -- it to auto-store things, or equip first using the pruning
             -- and then stash the rest selectively or en masse.
             if destCStore == CEqp
             then (promptEqp, return $ SuitsSomething $ \_ itemFull _kit ->
                    IA.goesIntoEqp $ aspectRecordFull itemFull)
             else (prompt, return SuitsEverything)
       ggi <-
         getFull leader psuit
                 (\body _ actorSk cCur _ ->
                    prompt <+> ppItemDialogBody body actorSk cCur)
                 (\body _ actorSk cCur _ ->
                    promptGeneric <+> ppItemDialogBody body actorSk cCur)
                 storesLast (not auto) True
       case ggi of
         Right (fromCStore, l) -> do
           modifySession $ \sess ->
             sess {slastItemMove = Just (fromCStore, destCStore)}
           return $ Right (fromCStore, l)
         Left err -> failWith err

moveItems :: forall m. MonadClientUI m
          => ActorId -> [CStore] -> (CStore, [(ItemId, ItemQuant)]) -> CStore
          -> m RequestTimed
moveItems leader stores (fromCStore, l) destCStore = do
  let !_A = assert (fromCStore /= destCStore && fromCStore `elem` stores) ()
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  b <- getsState $ getActorBody leader
  discoBenefit <- getsClient sdiscoBenefit
  let calmE = calmEnough b actorCurAndMaxSk
      ret4 :: [(ItemId, ItemQuant)] -> Int -> m [(ItemId, Int, CStore, CStore)]
      ret4 [] _ = return []
      ret4 ((iid, (k, _)) : rest) oldN = do
        let !_A = assert (k > 0) ()
            retRec toCStore = do
              let n = oldN + if toCStore == CEqp then k else 0
              l4 <- ret4 rest n
              return $ (iid, k, fromCStore, toCStore) : l4
        if stores == [CGround] && destCStore == CStash  -- normal pickup
        then -- @CStash@ is the implicit default; refine:
             if | not $ benInEqp $ discoBenefit EM.! iid -> retRec CStash
                | eqpOverfull b (oldN + 1) -> do
                  -- Action goes through, but changed, so keep in history.
                  msgAdd MsgActionWarning $
                    "Warning:" <+> showReqFailure EqpOverfull <> "."
                  retRec CStash
                | eqpOverfull b (oldN + k) -> do
                  -- If this stack doesn't fit, we don't equip any part of it,
                  -- but we may equip a smaller stack later of other items
                  -- in the same pickup.
                  msgAdd MsgActionWarning $
                    "Warning:" <+> showReqFailure EqpStackFull <> "."
                  retRec CStash
                | not calmE -> do
                  msgAdd MsgActionWarning $
                    "Warning:" <+> showReqFailure ItemNotCalm <> "."
                  retRec CStash
                | otherwise ->
                  -- Prefer @CEqp@ if all conditions hold:
                  retRec CEqp
        else case destCStore of  -- player forces store, so @benInEqp@ ignored
          CEqp | eqpOverfull b (oldN + 1) -> do
            -- Action aborted, so different colour and not in history.
            msgAdd MsgPromptItems $
              "Failure:" <+> showReqFailure EqpOverfull <> "."
            -- No recursive call here, we exit item manipulation,
            -- but something is moved or else outer functions would not call us.
            return []
          CEqp | eqpOverfull b (oldN + k) -> do
            msgAdd MsgPromptItems $
              "Failure:" <+> showReqFailure EqpStackFull <> "."
            return []
          _ -> retRec destCStore
  l4 <- ret4 l 0
  if null l4
  then error $ "" `showFailure` (stores, fromCStore, l, destCStore)
  else return $! ReqMoveItems l4

-- * Project

projectHuman :: (MonadClient m, MonadClientUI m)
             => ActorId -> m (FailOrCmd RequestTimed)
projectHuman leader = do
  curChal <- getsClient scurChal
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  if | ckeeper curChal ->
       failSer ProjectFinderKeeper
     | Ability.getSk Ability.SkProject actorCurAndMaxSk <= 0 ->
       -- Detailed are check later.
       failSer ProjectUnskilled
     | otherwise -> do
       itemSel <- getsSession sitemSel
       case itemSel of
         Just (_, COrgan, _) -> failWith "can't fling an organ"
         Just (iid, fromCStore, _) -> do
           b <- getsState $ getActorBody leader
           bag <- getsState $ getBodyStoreBag b fromCStore
           case iid `EM.lookup` bag of
             Nothing -> failWith "no item to fling"
             Just _kit -> do
               itemFull <- getsState $ itemToFull iid
               let i = (fromCStore, (iid, itemFull))
               projectItem leader i
         Nothing -> failWith "no item to fling"

projectItem :: (MonadClient m, MonadClientUI m)
            => ActorId -> (CStore, (ItemId, ItemFull))
            -> m (FailOrCmd RequestTimed)
projectItem leader (fromCStore, (iid, itemFull)) = do
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  b <- getsState $ getActorBody leader
  let calmE = calmEnough b actorCurAndMaxSk
  if fromCStore == CEqp && not calmE then failSer ItemNotCalm
  else do
    mpsuitReq <- psuitReq leader
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

applyHuman :: MonadClientUI m => ActorId -> m (FailOrCmd RequestTimed)
applyHuman leader = do
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  if Ability.getSk Ability.SkApply
                   actorCurAndMaxSk <= 0 then  -- detailed check later
    failSer ApplyUnskilled
  else do
    itemSel <- getsSession sitemSel
    case itemSel of
      Just (iid, fromCStore, _) -> do
        b <- getsState $ getActorBody leader
        bag <- getsState $ getBodyStoreBag b fromCStore
        case iid `EM.lookup` bag of
          Nothing -> failWith "no item to trigger"
          Just kit -> do
            itemFull <- getsState $ itemToFull iid
            applyItem leader (fromCStore, (iid, (itemFull, kit)))
      Nothing -> failWith "no item to trigger"

applyItem :: MonadClientUI m
          => ActorId -> (CStore, (ItemId, ItemFullKit))
          -> m (FailOrCmd RequestTimed)
applyItem leader (fromCStore, (iid, (itemFull, kit))) = do
  COps{corule} <- getsState scops
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  b <- getsState $ getActorBody leader
  localTime <- getsState $ getLocalTime (blid b)
  let skill = Ability.getSk Ability.SkApply actorCurAndMaxSk
      calmE = calmEnough b actorCurAndMaxSk
      arItem = aspectRecordFull itemFull
  if fromCStore == CEqp && not calmE then failSer ItemNotCalm
  else case permittedApply corule localTime skill calmE (Just fromCStore)
                           itemFull kit of
    Left reqFail -> failSer reqFail
    Right _ -> do
      Benefit{benApply} <- getsClient $ (EM.! iid) . sdiscoBenefit
      go <-
        if | IA.checkFlag Ability.Periodic arItem
             && not (IA.checkFlag Ability.Durable arItem) ->
             -- No warning if item durable, because activation weak,
             -- but price low, due to no destruction.
             displayYesNo ColorFull
                          "Triggering this periodic item may not produce all its effects (check item description) and moreover, because it's not durable, will destroy it. Are you sure?"
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
alterDirHuman :: MonadClientUI m => ActorId -> m (FailOrCmd RequestTimed)
alterDirHuman leader = pickPoint leader "modify" >>= \case
  Just p -> alterTileAtPos leader p
  Nothing -> failWith "never mind"

-- | Try to alter a tile using a feature at the given position.
--
-- We don't check if the tile is interesting, e.g., if any embedded
-- item can be triggered, because the player explicitely requested
-- the action. Consequently, even if all embedded items are recharching,
-- the time will be wasted and the server will describe the failure in detail.
alterTileAtPos :: MonadClientUI m
               => ActorId -> Point -> m (FailOrCmd RequestTimed)
alterTileAtPos leader pos = do
  sb <- getsState $ getActorBody leader
  let sxhair = Just $ TPoint TUnknown (blid sb) pos
  -- Point xhair to see details with `~`.
  setXHairFromGUI sxhair
  alterCommon leader False pos

-- | Verify that the tile can be transformed or any embedded item effect
-- triggered and the player is aware if the effect is dangerous or grave,
-- such as ending the game.
verifyAlters :: forall m. MonadClientUI m
             => ActorId -> Bool -> Point -> m (FailOrCmd ())
verifyAlters leader bumping tpos = do
  COps{cotile, coTileSpeedup} <- getsState scops
  sb <- getsState $ getActorBody leader
  arItem <- getsState $ aspectRecordFromIid $ btrunk sb
  embeds <- getsState $ getEmbedBag (blid sb) tpos
  lvl <- getLevel $ blid sb
  getKind <- getsState $ flip getIidKind
  let embedKindList =
        if IA.checkFlag Ability.Blast arItem
        then []  -- prevent embeds triggering each other in a loop
        else map (\(iid, kit) -> (getKind iid, (iid, kit))) (EM.assocs embeds)
      underFeet = tpos == bpos sb  -- if enter and alter, be more permissive
      blockedByItem = EM.member tpos (lfloor lvl)
      tile = lvl `at` tpos
      feats = TK.tfeature $ okind cotile tile
      tileActions =
        mapMaybe (Tile.parseTileAction
                    (bproj sb)
                    (underFeet || blockedByItem)  -- avoids AlterBlockItem
                    embedKindList)
                 feats
  if null tileActions
     && blockedByItem
     && not underFeet
     && Tile.isModifiable coTileSpeedup tile
  then failSer AlterBlockItem
  else processTileActions leader bumping tpos tileActions

processTileActions :: forall m. MonadClientUI m
                   => ActorId -> Bool -> Point -> [Tile.TileAction]
                   -> m (FailOrCmd ())
processTileActions leader bumping tpos tas = do
  COps{coTileSpeedup} <- getsState scops
  getKind <- getsState $ flip getIidKind
  sb <- getsState $ getActorBody leader
  lvl <- getLevel $ blid sb
  sar <- getsState $ aspectRecordFromIid $ btrunk sb
  let leaderIsMist = IA.checkFlag Ability.Blast sar
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
          if | leaderIsMist
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
              kitAssG <- getsState $ kitAssocs leader [CGround]
              kitAssE <- getsState $ kitAssocs leader [CEqp]
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
      blurb <- lookAtPosition tpos (blid sb)
      mapM_ (uncurry msgAdd) blurb
      if bumpFailed then do
        revCmd <- revCmdMap
        let km = revCmd AlterDir
            msg = "bumping is not enough to transform this terrain; modify with the '"
                  <> T.pack (K.showKM km)
                  <> "' command instead"
        if useResult then do
          merr <- failMsg msg
          msgAdd MsgPromptAction $ showFailError $ fromJust merr
          return $ Right ()  -- effect the embed activation, though
        else failWith msg
      else failWith "unable to activate nor modify at this time"
        -- related to, among others, @SfxNoItemsForTile@ on the server

verifyEscape :: MonadClientUI m => m (FailOrCmd ())
verifyEscape = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  if not (MK.fcanEscape $ gplayer fact)
  then failWith
         "This is the way out, but where would you go in this alien world?"
           -- exceptionally a full sentence, because a real question
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
    then failWith "here's your chance"
    else return $ Right ()

verifyToolEffect :: MonadClientUI m
                 => LevelId -> CStore -> ItemFull -> m (FailOrCmd ())
verifyToolEffect lid store itemFull = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  side <- getsClient sside
  localTime <- getsState $ getLocalTime lid
  factionD <- getsState sfactionD
  let (name1, powers) = partItemShort rwidth side factionD localTime
                                      itemFull quantSingle
      objectA = makePhrase [MU.AW name1, powers]
      prompt = "Do you really want to transform the terrain using"
               <+> objectA <+> ppCStoreIn store
               <+> "that may cause substantial side-effects?"
      objectThe = makePhrase ["the", name1]
  go <- displayYesNo ColorBW prompt
  if not go
  then failWith $ "replace" <+> objectThe <+> "and try again"
         -- question capitalized and ended with a dot, answer neither
  else return $ Right ()

-- * AlterWithPointer

-- | Try to alter a tile using a feature under the pointer.
alterWithPointerHuman :: MonadClientUI m
                      => ActorId -> m (FailOrCmd RequestTimed)
alterWithPointerHuman leader = do
  COps{corule=RuleContent{rWidthMax, rHeightMax}} <- getsState scops
  pUI <- getsSession spointer
  let p@(Point px py) = squareToMap $ uiToSquare pUI
  if px >= 0 && py >= 0 && px < rWidthMax && py < rHeightMax
  then alterTileAtPos leader p
  else failWith "never mind"

-- * CloseDir

-- | Close nearby open tile; ask for direction, if there is more than one.
closeDirHuman :: MonadClientUI m
              => ActorId -> m (FailOrCmd RequestTimed)
closeDirHuman leader = do
  COps{coTileSpeedup} <- getsState scops
  b <- getsState $ getActorBody leader
  lvl <- getLevel $ blid b
  let vPts = vicinityUnsafe $ bpos b
      openPts = filter (Tile.isClosable coTileSpeedup . at lvl) vPts
  case openPts of
    []  -> failSer CloseNothing
    [o] -> closeTileAtPos leader o
    _   -> pickPoint leader "close" >>= \case
      Nothing -> failWith "never mind"
      Just p -> closeTileAtPos leader p

-- | Close tile at given position.
closeTileAtPos :: MonadClientUI m
               => ActorId -> Point -> m (FailOrCmd RequestTimed)
closeTileAtPos leader tpos = do
  COps{coTileSpeedup} <- getsState scops
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  b <- getsState $ getActorBody leader
  alterable <- getsState $ tileAlterable (blid b) tpos
  lvl <- getLevel $ blid b
  let alterSkill = Ability.getSk Ability.SkAlter actorCurAndMaxSk
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
             msgAddDone leader tpos "close"
             return $ Right (ReqAlter tpos)

-- | Adds message with proper names.
msgAddDone :: MonadClientUI m => ActorId -> Point -> Text -> m ()
msgAddDone leader p verb = do
  COps{cotile} <- getsState scops
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
  msgAdd MsgActionComplete $ "You" <+> verb <+> "the" <+> s <+> dir <> "."

-- | Prompts user to pick a point.
pickPoint :: MonadClientUI m => ActorId -> Text -> m (Maybe Point)
pickPoint leader verb = do
  b <- getsState $ getActorBody leader
  UIOptions{uVi, uLeftHand} <- getsSession sUIOptions
  let dirKeys = K.dirAllKey uVi uLeftHand
      keys = K.escKM
           : K.leftButtonReleaseKM
           : map (K.KM K.NoModifier) dirKeys
  msgAdd MsgPromptGeneric $ "Where to" <+> verb <> "? [movement key] [pointer]"
  slides <- reportToSlideshow [K.escKM]
  km <- getConfirms ColorFull keys slides
  case K.key km of
    K.LeftButtonRelease -> do
      pUI <- getsSession spointer
      let p = squareToMap $ uiToSquare pUI
      return $ Just p
    _ -> return $ shift (bpos b) <$> K.handleDir dirKeys km

-- * Help

-- | Display command help.
helpHuman :: MonadClientUI m
          => (K.KM -> HumanCmd -> m (Either MError ReqUI))
          -> m (Either MError ReqUI)
helpHuman cmdSemInCxtOfKM = do
  ccui@CCUI{coinput, coscreen=ScreenContent{rwidth, rheight, rintroScreen}}
    <- getsSession sccui
  fontSetup@FontSetup{..} <- getFontSetup
  gameModeId <- getsState sgameModeId
  modeOv <- describeMode True gameModeId
  let modeH = ( "Press SPACE or PGDN to advance or ESC to see the map again."
              , (modeOv, []) )
      keyH = keyHelp ccui fontSetup
      -- This takes a list of paragraphs and returns a list of screens.
      -- Both paragraph and screen is a list of lines.
      --
      -- This would be faster, but less clear, if paragraphs were stored
      -- reversed in content. Not worth it, until we have huge manuals
      -- or run on weak mobiles. Even then, precomputation during
      -- compilation may be better.
      --
      -- Empty lines may appear at the end of pages, but it's fine,
      -- it means there is a new section on the next page.
      packIntoScreens :: [[String]] -> [[String]] -> Int -> [[String]]
      packIntoScreens [] acc _ = [intercalate [""] (reverse acc)]
      packIntoScreens ([] : ls) [] _  =
        -- Ignore empty paragraphs at the start of screen.
        packIntoScreens ls [] 0
      packIntoScreens (l : ls) [] h = assert (h == 0) $
        -- If a paragraph, even alone, is longer than screen height, it's split.
        if length l <= rheight - 3
        then packIntoScreens ls [l] (length l)
        else let (screen, rest) = splitAt (rheight - 3) l
             in screen : packIntoScreens (rest : ls) [] 0
      packIntoScreens (l : ls) acc h =
        -- The extra @+ 1@ comes from the empty line separating paragraphs,
        -- as added in @intercalate@.
        if length l + 1 + h <= rheight - 3
        then packIntoScreens ls (l : acc) (length l + 1 + h)
        else intercalate [""] (reverse acc) : packIntoScreens (l : ls) [] 0
      manualScreens = packIntoScreens (snd rintroScreen) [] 0
      sideBySide =
        if isSquareFont monoFont
        then \(screen1, screen2) ->  -- single column, two screens
          map offsetOverlay $ filter (not . null) [screen1, screen2]
        else \(screen1, screen2) ->  -- two columns, single screen
          [offsetOverlay screen1
           ++ xtranslateOverlay rwidth (offsetOverlay screen2)]
      listPairs (a : b : rest) = (a, b) : listPairs rest
      listPairs [a] = [(a, [])]
      listPairs [] = []
      -- Each screen begins with an empty line, to separate the header.
      manualOvs = map (EM.singleton monoFont)
                  $ concatMap sideBySide $ listPairs
                  $ map ((emptyAttrLine :) . map stringToAL) manualScreens
      addMnualHeader ov =
        ( "Showing PLAYING.md (best viewed in the browser)."
        , (ov, []) )
      manualH = map addMnualHeader manualOvs
      splitHelp (t, okx) = splitOKX fontSetup True rwidth rheight rwidth
                                    (textToAS t) [K.spaceKM, K.escKM] okx
      sli = toSlideshow fontSetup $ concat $ map splitHelp
            $ modeH : keyH ++ manualH
  -- Thus, the whole help menu corresponds to a single menu of item or lore,
  -- e.g., shared stash menu. This is especially clear when the shared stash
  -- menu contains many pages.
  ekm <- displayChoiceScreen "help" ColorFull True sli [K.spaceKM, K.escKM]
  case ekm of
    Left km | km `elem` [K.escKM, K.spaceKM] -> return $ Left Nothing
    Left km -> case km `M.lookup` bcmdMap coinput of
      Just (_desc, _cats, cmd) -> cmdSemInCxtOfKM km cmd
      Nothing -> weaveJust <$> failWith "never mind"
    Right _slot -> error $ "" `showFailure` ekm

-- * Hint

-- | Display hint or, if already displayed, display help.
hintHuman :: MonadClientUI m
          => (K.KM -> HumanCmd -> m (Either MError ReqUI))
          -> m (Either MError ReqUI)
hintHuman cmdSemInCxtOfKM = do
  sreportNull <- getsSession sreportNull
  if sreportNull then do
    promptMainKeys
    return $ Left Nothing
  else
    helpHuman cmdSemInCxtOfKM

-- * Dashboard

-- | Display the dashboard.
dashboardHuman :: MonadClientUI m
               => (K.KM -> HumanCmd -> m (Either MError ReqUI))
               -> m (Either MError ReqUI)
dashboardHuman cmdSemInCxtOfKM = do
  CCUI{coinput, coscreen=ScreenContent{rwidth, rheight}} <- getsSession sccui
  fontSetup@FontSetup{..} <- getFontSetup
  let offsetCol2 = 2
      (ov0, kxs0) = okxsN coinput monoFont propFont offsetCol2 (const False)
                          False CmdDashboard ([], [], []) ([], [])
      al1 = textToAS "Dashboard"
  let splitHelp (al, okx) = splitOKX fontSetup False rwidth (rheight - 2) rwidth
                                     al [K.escKM] okx
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
              => ActorId
              -> (K.KM -> HumanCmd -> m (Either MError ReqUI))
              -> m (Either MError ReqUI)
itemMenuHuman leader cmdSemInCxtOfKM = do
  COps{corule} <- getsState scops
  itemSel <- getsSession sitemSel
  fontSetup@FontSetup{..} <- getFontSetup
  case itemSel of
    Just (iid, fromCStore, _) -> do
      b <- getsState $ getActorBody leader
      bUI <- getsSession $ getActorUI leader
      bag <- getsState $ getBodyStoreBag b fromCStore
      case iid `EM.lookup` bag of
        Nothing -> weaveJust <$> failWith "no item to open item menu for"
        Just kit -> do
          CCUI{coscreen=ScreenContent{rwidth, rheight}} <- getsSession sccui
          actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
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
              descAs = itemDesc rwidth markParagraphs (bfid b) factionD
                                (Ability.getSk Ability.SkHurtMelee
                                               actorCurAndMaxSk)
                                fromCStore localTime jlid itemFull kit
              (ovLab, ovDesc) = labDescOverlay squareFont rwidth descAs
              ovPrefix = ytranslateOverlay (length ovDesc)
                         $ offsetOverlay
                         $ splitAttrString rwidth rwidth foundPrefix
              ystart = length ovDesc + length ovPrefix - 1
              xstart = textSize monoFont (Color.spaceAttrW32
                                          : attrLine (snd $ last ovPrefix))
              foundKeys = map (K.KM K.NoModifier . K.Fun)
                              [1 .. length foundAlt]  -- starting from 1!
          let ks = zip foundKeys foundTexts
              width = if isSquareFont monoFont then 2 * rwidth else rwidth
              (ovFoundRaw, kxsFound) = wrapOKX monoFont ystart xstart width ks
              ovFound = ovPrefix ++ ovFoundRaw
          report <- getReportUI True
          CCUI{coinput} <- getsSession sccui
          mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid b
          let calmE = calmEnough b actorCurAndMaxSk
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
                  let skill = Ability.getSk Ability.SkApply actorCurAndMaxSk
                  in not $ either (const False) id
                     $ permittedApply corule localTime skill calmE
                                      (Just fromCStore) itemFull kit
                Project{} ->
                  let skill = Ability.getSk Ability.SkProject actorCurAndMaxSk
                  in not $ either (const False) id
                     $ permittedProject False skill calmE itemFull
                _ -> False
              fmt n k h = " " <> T.justifyLeft n ' ' k <> " " <> h
              offsetCol2 = 11
              keyCaption = fmt offsetCol2 "keys" "command"
              offset = 1 + maxYofOverlay (ovDesc ++ ovFound)
              (ov0, kxs0) = xytranslateOKX 0 offset $
                 okxsN coinput monoFont propFont offsetCol2 greyedOut
                       True CmdItemMenu ([], [], ["", keyCaption]) ([], [])
              t0 = makeSentence [ MU.SubjectVerbSg (partActor bUI) "choose"
                                , "an item", MU.Text $ ppCStoreIn fromCStore ]
              alRep = foldr (<+:>) [] $ renderReport True report
              al1 | null alRep = textToAS t0
                  | otherwise = alRep ++ stringToAS "\n" ++ textToAS t0
              splitHelp (al, okx) =
                splitOKX fontSetup False rwidth (rheight - 2) rwidth al
                         [K.spaceKM, K.escKM] okx
              sli = toSlideshow fontSetup
                    $ splitHelp ( al1
                                , ( EM.insertWith (++) squareFont ovLab
                                    $ EM.insertWith (++) propFont ovDesc
                                    $ EM.insertWith (++) monoFont ovFound ov0
                                        -- mono font, because there are buttons
                                  , kxsFound ++ kxs0 ))
              extraKeys = [K.spaceKM, K.escKM] ++ foundKeys
          recordHistory  -- report shown (e.g., leader switch), save to history
          ekm <- displayChoiceScreen "item menu" ColorFull False sli extraKeys
          case ekm of
            Left km -> case km `M.lookup` bcmdMap coinput of
              _ | km == K.escKM -> weaveJust <$> failWith "never mind"
              _ | km == K.spaceKM ->
                chooseItemMenuHuman leader cmdSemInCxtOfKM (MStore fromCStore)
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
                       itemMenuHuman newAid cmdSemInCxtOfKM
                _ -> error $ "" `showFailure` km
              Just (_desc, _cats, cmd) -> do
                modifySession $ \sess ->
                  sess {sitemSel = Just (iid, fromCStore, True)}
                cmdSemInCxtOfKM km cmd
              Nothing -> weaveJust <$> failWith "never mind"
            Right _slot -> error $ "" `showFailure` ekm
    Nothing -> weaveJust <$> failWith "no item to open item menu for"

-- * ChooseItemMenu

chooseItemMenuHuman :: MonadClientUI m
                    => ActorId
                    -> (K.KM -> HumanCmd -> m (Either MError ReqUI))
                    -> ItemDialogMode
                    -> m (Either MError ReqUI)
chooseItemMenuHuman leader1 cmdSemInCxtOfKM c1 = do
  res2 <- chooseItemDialogMode leader1 True c1
  case res2 of
    Right leader2 -> itemMenuHuman leader2 cmdSemInCxtOfKM
    Left err -> return $ Left $ Just err

-- * MainMenu

generateMenu :: MonadClientUI m
             => (K.KM -> HumanCmd -> m (Either MError ReqUI))
             -> FontOverlayMap
             -> [(String, (Text, HumanCmd, Maybe FontOverlayMap))]
             -> [String]
             -> String
             -> m (Either MError ReqUI)
generateMenu cmdSemInCxtOfKM blurb kdsRaw gameInfo menuName = do
  COps{corule} <- getsState scops
  CCUI{coscreen=ScreenContent{rheight, rwebAddress}} <- getsSession sccui
  FontSetup{..} <- getFontSetup
  let kds = map (first K.mkKM) kdsRaw
      bindings =  -- key bindings to display
        let fmt (km, (d, _, _)) =
              ( Just km
              , T.unpack
                $ T.justifyLeft 3 ' ' (T.pack $ K.showKM km) <> " " <> d )
        in map fmt kds
      generate :: Int -> (Maybe K.KM, String) -> Maybe KYX
      generate y (mkey, binding) =
        let lenB = length binding
            yxx key = (Left key, ( PointUI 0 y
                                 , ButtonWidth squareFont lenB ))
        in yxx <$> mkey
      titleLine = rtitle corule ++ " "
                  ++ showVersion (rexeVersion corule) ++ " "
      rawLines = zip (repeat Nothing)
                     (["", titleLine ++ "[" ++ rwebAddress ++ "]", ""]
                      ++ gameInfo)
                 ++ bindings
      browserKey = ( Right $ SlotChar 1042 'a'
                   , ( PointUI (2 * length titleLine) 1
                     , ButtonWidth squareFont (2 + length rwebAddress) ) )
      kyxs = browserKey : catMaybes (zipWith generate [0..] rawLines)
      ov = EM.singleton squareFont $ offsetOverlay
                                   $ map (stringToAL . snd) rawLines
      kxy = xytranslateOKX 2 0 (ov, kyxs)
  menuIxMap <- getsSession smenuIxMap
  unless (menuName `M.member` menuIxMap) $
    modifySession $ \sess -> sess {smenuIxMap = M.insert menuName 1 menuIxMap}
  let prepareBlurb ovs =
        let introLen = 1 + maxYofFontOverlayMap ovs
            start0 = max 0 (rheight - introLen
                            - if isSquareFont propFont then 1 else 2)
        in EM.map (xytranslateOverlay (-2) (start0 - 2)) ovs
          -- subtracting 2 from X and Y to negate the indentation in
          -- @displayChoiceScreenWithRightPane@
      returnDefaultOKS = return (prepareBlurb blurb, [])
      displayInRightPane (Right _) = returnDefaultOKS
      displayInRightPane (Left km) = case km `lookup` kds of
        Just (_, _, mblurbRight) -> case mblurbRight of
          Nothing -> returnDefaultOKS
          Just blurbRight -> return (prepareBlurb blurbRight, [])
        Nothing -> error "displayInRightPane: unexpected key"
  ekm <- displayChoiceScreenWithRightPane displayInRightPane
                                          menuName ColorFull True
                                          (menuToSlideshow kxy) [K.escKM]
  case ekm of
    Left km -> case km `lookup` kds of
      Just (_desc, cmd, _) -> cmdSemInCxtOfKM km cmd
      Nothing -> weaveJust <$> failWith "never mind"
    Right (SlotChar 1042 'a') -> do
      success <- tryOpenBrowser rwebAddress
      if success
      then generateMenu cmdSemInCxtOfKM blurb kdsRaw gameInfo menuName
      else weaveJust <$> failWith "failed to open web browser"
    Right _slot -> error $ "" `showFailure` ekm

-- | Display the main menu.
mainMenuHuman :: MonadClientUI m
              => (K.KM -> HumanCmd -> m (Either MError ReqUI))
              -> m (Either MError ReqUI)
mainMenuHuman cmdSemInCxtOfKM = do
  CCUI{coscreen=ScreenContent{rintroScreen}} <- getsSession sccui
  FontSetup{propFont} <- getFontSetup
  gameMode <- getGameMode
  curChal <- getsClient scurChal
  let offOn b = if b then "on" else "off"
      -- Key-description-command tuples.
      kds = [ ("s", ("setup and start new game>", ChallengeMenu, Nothing))
            , ("x", ("save and exit to desktop", GameExit, Nothing))
            , ("c", ("tweak convenience settings>", SettingsMenu, Nothing))
            , ("t", ("toggle autoplay", AutomateToggle, Nothing))
            , ("?", ("see command help", Help, Nothing))
            , ("F12", ("switch to dashboard", Dashboard, Nothing))
            , ("Escape", ("back to playing", AutomateBack, Nothing)) ]
      gameName = MK.mname gameMode
      gameInfo = map T.unpack
                   [ "Now playing:" <+> gameName
                   , ""
                   , "   with difficulty:" <+> tshow (cdiff curChal)
                   , "       cold fish:" <+> offOn (cfish curChal)
                   , "     ready goods:" <+> offOn (cgoods curChal)
                   , "       lone wolf:" <+> offOn (cwolf curChal)
                   , "   finder keeper:" <+> offOn (ckeeper curChal)
                   , "" ]
      glueLines (l1 : l2 : rest) =
        if | null l1 -> l1 : glueLines (l2 : rest)
           | null l2 -> l1 : l2 : glueLines rest
           | otherwise -> (l1 ++ l2) : glueLines rest
      glueLines ll = ll
      backstory | isSquareFont propFont = fst rintroScreen
                | otherwise = glueLines $ fst rintroScreen
      backstoryAL = map stringToAL $ map (dropWhile (== ' ')) backstory
      blurb = attrLinesToFontMap [(propFont, backstoryAL)]
  generateMenu cmdSemInCxtOfKM blurb kds gameInfo "main"

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
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  UIOptions{uMsgWrapColumn} <- getsSession sUIOptions
  FontSetup{..} <- getFontSetup
  markSuspect <- getsClient smarkSuspect
  markVision <- getsSession smarkVision
  markSmell <- getsSession smarkSmell
  noAnim <- getsClient $ fromMaybe False . snoAnim . soptions
  side <- getsClient sside
  factDoctrine <- getsState $ MK.fdoctrine . gplayer . (EM.! side) . sfactionD
  overrideTut <- getsSession soverrideTut
  let offOn b = if b then "on" else "off"
      offOnAll n = case n of
        0 -> "none"
        1 -> "untried"
        2 -> "all"
        _ -> error $ "" `showFailure` n
      neverEver n = case n of
        0 -> "never"
        1 -> "aiming"
        2 -> "always"
        _ -> error $ "" `showFailure` n
      offOnUnset mb = case mb of
        Nothing -> "pass"
        Just b -> if b then "force on" else "force off"
      tsuspect = "mark suspect terrain:" <+> offOnAll markSuspect
      tvisible = "show visible zone:" <+> neverEver markVision
      tsmell = "display smell clues:" <+> offOn markSmell
      tanim = "play animations:" <+> offOn (not noAnim)
      tdoctrine = "squad doctrine:" <+> Ability.nameDoctrine factDoctrine
      toverride = "override tutorial hints:" <+> offOnUnset overrideTut
      width = if isSquareFont propFont
              then rwidth `div` 2
              else min uMsgWrapColumn (rwidth - 2)
      textToBlurb t = Just $ attrLinesToFontMap
        [ ( monoFont
          , splitAttrString width width
            $ textToAS t ) ]
      -- Key-description-command-text tuples.
      kds = [ ("s", ( tsuspect, MarkSuspect
                    , textToBlurb "* mark suspect terrain\nThis setting affects the ongoing and the next games. It determines which suspect terrain is marked in special color on the map: none, untried (not searched nor revealed), all. It correspondingly determines which, if any, suspect tiles are considered for mouse go-to, auto-explore and for the command that marks the nearest unexplored position." ))
            , ("v", (tvisible, MarkVision
                    , textToBlurb "* show visible zone\nThis setting affects the ongoing and the next games. It determines the conditions under which the area visible to the party is marked on the map via a gray background: never, when aiming, always." ))
            , ("c", (tsmell, MarkSmell
                    , textToBlurb "* display smell clues\nThis setting affects the ongoing and the next games. It determines whether the map displays any smell traces (regardless of who left them) detected by a party member that can track via smell (as determined by the smell radius skill; not common among humans)." ))
            , ("a", (tanim, MarkAnim
                    , textToBlurb "* play animations\nThis setting affects the ongoing and the next games. It determines whether important events, such combat, are highlighted by animations. This overrides the corresponding config file setting." ))
            , ("d", (tdoctrine, Doctrine
                    , textToBlurb "* squad doctrine\nThis setting affects the ongoing game, but does not persist to the next games. It determines the behaviour of henchmen (non-pointman characters) in the party and, in particular, if they are permitted to move autonomously or fire opportunistically (assuming they are able to, usually due to rare equipment). This setting has a poor UI that will be improved in the future." ))
            , ("t", (toverride, OverrideTut
                    , textToBlurb "* override tutorial hints\nThis setting affects the ongoing and the next games. It determines whether tutorial hints are, respectively, not overridden with respect to the setting that was chosen when starting the current game, forced to be off, forced to be on." ))
            , ("Escape", ( "back to main menu", MainMenu
                         , Just EM.empty )) ]
      gameInfo = map T.unpack
                   [ "Tweak convenience settings:"
                   , "" ]
  generateMenu cmdSemInCxtOfKM EM.empty kds gameInfo "settings"

-- * ChallengeMenu

-- | Display the challenge menu.
challengeMenuHuman :: MonadClientUI m
                   => (K.KM -> HumanCmd -> m (Either MError ReqUI))
                   -> m (Either MError ReqUI)
challengeMenuHuman cmdSemInCxtOfKM = do
  cops <- getsState scops
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  UIOptions{uMsgWrapColumn} <- getsSession sUIOptions
  FontSetup{..} <- getFontSetup
  svictories <- getsClient svictories
  snxtScenario <- getsSession snxtScenario
  nxtTutorial <- getsSession snxtTutorial
  overrideTut <- getsSession soverrideTut
  nxtChal <- getsClient snxtChal
  let (gameModeId, gameMode) = nxtGameMode cops snxtScenario
      victories = case EM.lookup gameModeId svictories of
        Nothing -> 0
        Just cm -> fromMaybe 0 (M.lookup nxtChal cm)
      star t = if victories > 0 then "*" <> t else t
      tnextScenario = "adventure:" <+> star (MK.mname gameMode)
      offOn b = if b then "on" else "off"
      starTut t = if isJust overrideTut then "*" <> t else t
      displayTutorialHints = fromMaybe nxtTutorial overrideTut
      tnextTutorial = "tutorial hints:" <+> starTut (offOn displayTutorialHints)
      tnextDiff = "difficulty level:" <+> tshow (cdiff nxtChal)
      tnextFish = "cold fish (rather hard):" <+> offOn (cfish nxtChal)
      tnextGoods = "ready goods (hard):" <+> offOn (cgoods nxtChal)
      tnextWolf = "lone wolf (very hard):" <+> offOn (cwolf nxtChal)
      tnextKeeper = "finder keeper (hard):" <+> offOn (ckeeper nxtChal)
      width = if isSquareFont propFont
              then rwidth `div` 2
              else min uMsgWrapColumn (rwidth - 2)
      widthMono = if isSquareFont propFont
                  then rwidth `div` 2
                  else rwidth - 2
      duplicateEOL '\n' = "\n\n"
      duplicateEOL c = T.singleton c
      blurb = Just $ attrLinesToFontMap
        [ ( propFont
          , splitAttrString width width
            $ textFgToAS Color.BrBlack
            $ T.concatMap duplicateEOL (MK.mdesc gameMode)
              <> "\n\n" )
        , ( monoFont
          , splitAttrString widthMono widthMono
            $ textToAS
            $ MK.mrules gameMode
              <> "\n\n" )
        , ( propFont
          , splitAttrString width width
            $ textToAS
            $ T.concatMap duplicateEOL (MK.mreason gameMode) )
        ]
      textToBlurb t = Just $ attrLinesToFontMap
        [ ( monoFont
          , splitAttrString width width  -- not widthMono!
            $ textToAS t ) ]
      -- Key-description-command-text tuples.
      kds = [ ("s", (tnextScenario, GameScenarioIncr, blurb))
            , ("t", ( tnextTutorial, GameTutorialToggle
                    , textToBlurb "* tutorial hints\nThis determines whether tutorial hint messages will be shown in the next game that's about to be started. They are rendered in pink and can be re-read from message history. Display of tutorial hints in the current game can be overridden from the convenience settings menu."))
            , ("d", ( tnextDiff, GameDifficultyIncr
                    , textToBlurb "* difficulty level\nThis determines the difficulty of survival in the next game that's about to be started. Lower numbers result in easier game. In particular, difficulty below 5 multiplies hitpoints of player characters and difficulty over 5 multiplies hitpoints of their enemies. Game score scales with difficulty."))
            , ("f", ( tnextFish, GameFishToggle
                    , textToBlurb "* cold fish\nThis challenge mode setting will affect the next game that's about to be started. When on, it makes it impossible for player characters to be healed by actors from other factions (this is a significant restriction in the long crawl adventure)."))
            , ("r", ( tnextGoods, GameGoodsToggle
                    , textToBlurb "* ready goods\nThis challenge mode setting will affect the next game that's about to be started. When on, it disables crafting for the player, making the selection of equipment, especially melee weapons, very limited, unless the player has the luck to find the rare powerful ready weapons (this applies only if the chosen adventure supports crafting at all)."))
            , ("w", ( tnextWolf, GameWolfToggle
                    , textToBlurb "* lone wolf\nThis challenge mode setting will affect the next game that's about to be started. When on, it reduces player's starting actors to exactly one, though later on new heroes may join the party. This makes the game very hard in the long run."))
            , ("k", ( tnextKeeper, GameKeeperToggle
                    , textToBlurb "* finder keeper\nThis challenge mode setting will affect the next game that's about to be started. When on, it completely disables flinging projectiles by the player, which affects not only ranged damage dealing, but also throwing of consumables that buff teammates engaged in melee combat, weaken and distract enemies, light dark corners, etc."))
            , ("g", ("start new game", GameRestart, blurb))
            , ("Escape", ("back to main menu", MainMenu, Nothing)) ]
      gameInfo = map T.unpack [ "Setup and start new game:"
                              , "" ]
  generateMenu cmdSemInCxtOfKM EM.empty kds gameInfo "challenge"

-- * GameTutorialToggle

gameTutorialToggle :: MonadClientUI m  => m ()
gameTutorialToggle = do
  nxtTutorial <- getsSession snxtTutorial
  overrideTut <- getsSession soverrideTut
  let displayTutorialHints = fromMaybe nxtTutorial overrideTut
  modifySession $ \sess -> sess { snxtTutorial = not displayTutorialHints
                                , soverrideTut = Nothing }

-- * GameDifficultyIncr

gameDifficultyIncr :: MonadClient m => m ()
gameDifficultyIncr = do
  nxtDiff <- getsClient $ cdiff . snxtChal
  let delta = -1
      d | nxtDiff + delta > difficultyBound = 1
        | nxtDiff + delta < 1 = difficultyBound
        | otherwise = nxtDiff + delta
  modifyClient $ \cli -> cli {snxtChal = (snxtChal cli) {cdiff = d} }

-- * GameFishToggle

gameFishToggle :: MonadClient m => m ()
gameFishToggle =
  modifyClient $ \cli ->
    cli {snxtChal = (snxtChal cli) {cfish = not (cfish (snxtChal cli))} }

-- * GameGoodsToggle

gameGoodsToggle :: MonadClient m => m ()
gameGoodsToggle =
  modifyClient $ \cli ->
    cli {snxtChal = (snxtChal cli) {cgoods = not (cgoods (snxtChal cli))} }

-- * GameWolfToggle

gameWolfToggle :: MonadClient m => m ()
gameWolfToggle =
  modifyClient $ \cli ->
    cli {snxtChal = (snxtChal cli) {cwolf = not (cwolf (snxtChal cli))} }

-- * GameKeeperToggle

gameKeeperToggle :: MonadClient m => m ()
gameKeeperToggle =
  modifyClient $ \cli ->
    cli {snxtChal = (snxtChal cli) {ckeeper = not (ckeeper (snxtChal cli))} }

-- * GameScenarioIncr

gameScenarioIncr :: MonadClientUI m => m ()
gameScenarioIncr = do
  cops <- getsState scops
  oldScenario <- getsSession snxtScenario
  let snxtScenario = oldScenario + 1
      snxtTutorial = MK.mtutorial $ snd $ nxtGameMode cops snxtScenario
  modifySession $ \sess -> sess {snxtScenario, snxtTutorial}

-- * GameRestart

gameRestartHuman :: MonadClientUI m => m (FailOrCmd ReqUI)
gameRestartHuman = do
  cops <- getsState scops
  noConfirmsGame <- isNoConfirmsGame
  gameMode <- getGameMode
  snxtScenario <- getsSession snxtScenario
  let nxtGameName = MK.mname $ snd $ nxtGameMode cops snxtScenario
  b <- if noConfirmsGame
       then return True
       else displayYesNo ColorBW
            $ "You just requested a new" <+> nxtGameName
              <+> "game. The progress of the ongoing" <+> MK.mname gameMode
              <+> "game will be lost! Are you sure?"
  if b
  then do
    snxtChal <- getsClient snxtChal
    -- This ignores all but the first word of game mode names picked
    -- via main menu and assumes the fist word of such game modes
    -- is present in their frequencies.
    let (mainName, _) = T.span (\c -> Char.isAlpha c || c == ' ') nxtGameName
        nxtGameGroup = DefsInternal.GroupName $ T.intercalate " "
                       $ take 2 $ T.words mainName
    return $ Right $ ReqUIGameRestart nxtGameGroup snxtChal
  else do
    msg2 <- rndToActionUI $ oneOf
              [ "yea, would be a pity to leave them to die"
              , "yea, a shame to get your team stranded" ]
    failWith msg2

-- * GameQuit

-- TODO: deduplicate with gameRestartHuman
gameQuitHuman :: MonadClientUI m => m (FailOrCmd ReqUI)
gameQuitHuman = do
  noConfirmsGame <- isNoConfirmsGame
  gameMode <- getGameMode
  b <- if noConfirmsGame
       then return True
       else displayYesNo ColorBW
            $ "If you quit, the progress of the ongoing" <+> MK.mname gameMode
              <+> "game will be lost! Are you sure?"
  if b
  then do
    snxtChal <- getsClient snxtChal
    return $ Right $ ReqUIGameRestart MK.INSERT_COIN snxtChal
  else do
    msg2 <- rndToActionUI $ oneOf
              [ "yea, would be a pity to leave them to die"
              , "yea, a shame to get your team stranded" ]
    failWith msg2

-- * GameDrop

gameDropHuman :: MonadClientUI m => m ReqUI
gameDropHuman = do
  modifySession $ \sess -> sess {sallNframes = -1}  -- hack, but we crash anyway
  msgAdd MsgPromptGeneric "Interrupt! Trashing the unsaved game. The program exits now."
  clientPrintUI "Interrupt! Trashing the unsaved game. The program exits now."
    -- this is not shown by ANSI frontend, but at least shown by sdl2 one
  return ReqUIGameDropAndExit

-- * GameExit

gameExitHuman :: Monad m => m ReqUI
gameExitHuman =
  return ReqUIGameSaveAndExit

-- * GameSave

gameSaveHuman :: MonadClientUI m => m ReqUI
gameSaveHuman = do
  -- Announce before the saving started, since it can take a while.
  msgAdd MsgInnerWorkSpam "Saving game backup."
  return ReqUIGameSave

-- * Doctrine

-- Note that the difference between seek-target and follow-the-leader doctrine
-- can influence even a faction with passive actors. E.g., if a passive actor
-- has an extra active skill from equipment, he moves every turn.
doctrineHuman :: MonadClientUI m => m (FailOrCmd ReqUI)
doctrineHuman = do
  fid <- getsClient sside
  fromT <- getsState $ MK.fdoctrine . gplayer . (EM.! fid) . sfactionD
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
