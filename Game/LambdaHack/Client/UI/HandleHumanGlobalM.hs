{-# LANGUAGE DataKinds, GADTs #-}
-- | Semantics of 'Command.Cmd' client commands that return server commands.
-- A couple of them do not take time, the rest does.
-- Here prompts and menus and displayed, but any feedback resulting
-- from the commands (e.g., from inventory manipulation) is generated later on,
-- for all clients that witness the results of the commands.
-- TODO: document
module Game.LambdaHack.Client.UI.HandleHumanGlobalM
  ( -- * Meta commands
    byAreaHuman, byAimModeHuman, byItemModeHuman
  , composeIfLocalHuman, composeUnlessErrorHuman, loopOnNothingHuman
    -- * Global commands that usually take time
  , waitHuman, moveRunHuman
  , runOnceAheadHuman, moveOnceToXhairHuman
  , runOnceToXhairHuman, continueToXhairHuman
  , moveItemHuman, projectHuman, applyHuman, alterDirHuman
  , helpHuman, itemMenuHuman, chooseItemMenuHuman, mainMenuHuman
  , gameDifficultyIncr, gameScenarioIncr
    -- * Global commands that never take time
  , gameRestartHuman, gameExitHuman, gameSaveHuman
  , tacticHuman, automateHuman
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

-- Cabal
import qualified Paths_LambdaHack as Self (version)

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Version
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.Bfs
import Game.LambdaHack.Client.BfsM
import Game.LambdaHack.Client.CommonM
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.FrameM
import Game.LambdaHack.Client.UI.Frontend (frontendName)
import Game.LambdaHack.Client.UI.HandleHelperM
import Game.LambdaHack.Client.UI.HandleHumanLocalM
import Game.LambdaHack.Client.UI.HumanCmd (CmdArea (..), Trigger (..))
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import Game.LambdaHack.Client.UI.InventoryM
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Client.UI.MsgM
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.RunM
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Client.UI.Slideshow
import Game.LambdaHack.Client.UI.SlideshowM
import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Content.TileKind as TK

-- * ByArea

-- | Pick command depending on area the mouse pointer is in.
-- The first matching area is chosen. If none match, only interrupt.
byAreaHuman :: MonadClientUI m
            => (HumanCmd.HumanCmd -> m (Either MError ReqUI))
            -> [(HumanCmd.CmdArea, HumanCmd.HumanCmd)]
            -> m (Either MError ReqUI)
{-# INLINABLE byAreaHuman #-}
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

areaToRectangles :: MonadClientUI m => HumanCmd.CmdArea -> m [(X, Y, X, Y)]
{-# INLINABLE areaToRectangles #-}
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
    ours <- getsState $ filter (not . bproj) . actorList (== side) lidV
    let rectFromB Point{..} = (px, mapStartY + py, px, mapStartY + py)
    return $! map (rectFromB . bpos) ours
  CaMap -> return
    [( 0, mapStartY, fst normalLevelBound, mapStartY + snd normalLevelBound )]
  CaArenaName -> let y = snd normalLevelBound + 2
                     x = fst normalLevelBound `div` 2 - 11
                 in return [(0, y, x, y)]
  CaPercentSeen -> let y = snd normalLevelBound + 2
                       x = fst normalLevelBound `div` 2
                   in return [(x - 9, y, x, y)]
  CaXhairDesc -> let y = snd normalLevelBound + 2
                     x = fst normalLevelBound `div` 2 + 2
                 in return [(x, y, fst normalLevelBound, y)]
  CaSelected -> let y = snd normalLevelBound + 3
                    x = fst normalLevelBound `div` 2
                in return [(0, y, x - 22, y)]  -- TODO
  CaLeaderStatus -> let y = snd normalLevelBound + 3
                        x = fst normalLevelBound `div` 2
                    in return [(x - 20, y, x, y)]
                      -- TODO: calculate and share with ClientDraw
  CaTargetDesc -> let y = snd normalLevelBound + 3
                      x = fst normalLevelBound `div` 2 + 2
                  in return [(x, y, fst normalLevelBound, y)]

-- * ByAimMode

byAimModeHuman :: MonadClientUI m
               => m (Either MError ReqUI) -> m (Either MError ReqUI)
               -> m (Either MError ReqUI)
{-# INLINABLE byAimModeHuman #-}
byAimModeHuman cmdNotAimingM cmdAimingM = do
  aimMode <- getsSession saimMode
  if isNothing aimMode then cmdNotAimingM else cmdAimingM

-- * ByItemMode

byItemModeHuman :: MonadClientUI m
                => m (Either MError ReqUI) -> m (Either MError ReqUI)
                -> m (Either MError ReqUI)
{-# INLINABLE byItemModeHuman #-}
byItemModeHuman cmdNotChosenM cmdChosenM = do
  itemSel <- getsSession sitemSel
  case itemSel of
    Just (fromCStore, iid) -> do
      leader <- getLeaderUI
      b <- getsState $ getActorBody leader
      bag <- getsState $ getBodyStoreBag b fromCStore
      case iid `EM.lookup` bag of
        Nothing -> cmdNotChosenM
        Just _ -> cmdChosenM
    Nothing -> cmdNotChosenM

-- * ComposeIfLeft

composeIfLocalHuman :: MonadClientUI m
                    => m (Either MError ReqUI) -> m (Either MError ReqUI)
                    -> m (Either MError ReqUI)
{-# INLINABLE composeIfLocalHuman #-}
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
{-# INLINABLE composeUnlessErrorHuman #-}
composeUnlessErrorHuman c1 c2 = do
  slideOrCmd1 <- c1
  case slideOrCmd1 of
    Left Nothing -> c2
    _ -> return slideOrCmd1

-- * LoopOnNothing

loopOnNothingHuman :: MonadClientUI m
                   => m (Either MError ReqUI)
                   -> m (Either MError ReqUI)
{-# INLINABLE loopOnNothingHuman #-}
loopOnNothingHuman cmd = do
  res <- cmd
  case res of
    Left Nothing -> loopOnNothingHuman cmd
    _ -> return res

-- * Wait

-- | Leader waits a turn (and blocks, etc.).
waitHuman :: MonadClientUI m => m (RequestTimed 'AbWait)
{-# INLINABLE waitHuman #-}
waitHuman = do
  modifySession $ \sess -> sess {swaitTimes = abs (swaitTimes sess) + 1}
  return ReqWait

-- * MoveDir and RunDir

moveRunHuman :: MonadClientUI m
             => Bool -> Bool -> Bool -> Bool -> Vector
             -> m (FailOrCmd RequestAnyAbility)
{-# INLINABLE moveRunHuman #-}
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
                   then [leader]  -- TODO: warn?
                   else ES.toList (ES.delete leader sel) ++ [leader]
      runParams = RunParams { runLeader = leader
                            , runMembers
                            , runInitial = True
                            , runStopMsg = Nothing
                            , runWaiting = 0 }
      macroRun25 = ["CTRL-comma", "CTRL-V"]
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
  -- TODO: stop running at invisible actor
  let tpos = bpos sb `shift` dir
  -- We start by checking actors at the target position,
  -- which gives a partial information (actors can be invisible),
  -- as opposed to accessibility (and items) which are always accurate
  -- (tiles can't be invisible).
  tgts <- getsState $ posToAssocs tpos arena
  case tgts of
    [] -> do  -- move or search or alter
      runStopOrCmd <- moveSearchAlterAid leader dir
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
                                  `twith` (leader, target, tb)) ()
        failWith "by bumping"
      else
        -- Attacking does not require full access, adjacency is enough.
        RequestAnyAbility <$$> meleeAid target
    _ : _ -> failWith "actor in the way"

-- | Actor attacks an enemy actor or his own projectile.
meleeAid :: MonadClientUI m
         => ActorId -> m (FailOrCmd (RequestTimed 'AbMelee))
{-# INLINABLE meleeAid #-}
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
                f (Just (TEnemyPos _ _ _ b)) = Just $ TEnemy target b
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
{-# INLINABLE displaceAid #-}
displaceAid target = do
  cops <- getsState scops
  leader <- getLeaderUI
  sb <- getsState $ getActorBody leader
  tb <- getsState $ getActorBody target
  tfact <- getsState $ (EM.! bfid tb) . sfactionD
  actorMaxSk <- enemyMaxAb target
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
       if accessible cops lvl tpos then
         case posToAidsLvl tpos lvl of
           [] -> assert `failure` (leader, sb, target, tb)
           [_] -> return $ Right $ ReqDisplace target
           _ -> failSer DisplaceProjectiles
       else failSer DisplaceAccess

-- | Actor moves or searches or alters. No visible actor at the position.
moveSearchAlterAid :: MonadClientUI m
                   => ActorId -> Vector -> m (FailOrCmd RequestAnyAbility)
{-# INLINABLE moveSearchAlterAid #-}
moveSearchAlterAid source dir = do
  cops@Kind.COps{ cotile=cotile@Kind.Ops{okind}
                , coTileSpeedup } <- getsState scops
  sb <- getsState $ getActorBody source
  actorSk <- actorSkillsClient source
  lvl <- getLevel $ blid sb
  let alterSkill = EM.findWithDefault 0 AbAlter actorSk
      spos = bpos sb           -- source position
      tpos = spos `shift` dir  -- target position
      t = lvl `at` tpos
      alterMinSkill = Tile.alterMinSkill coTileSpeedup t
  runStopOrCmd <- do
    -- Movement requires full access.
    if | accessible cops lvl tpos ->
         -- A potential invisible actor is hit. War started without asking.
         return $ Right $ RequestAnyAbility $ ReqMove dir
       -- No access, so search and/or alter the tile.
       | not (knownLsecret lvl)
         || isSecretPos lvl tpos  -- possible secrets here
            && (Tile.isSuspect coTileSpeedup t  -- not yet searched
                || Tile.hideAs cotile t /= t)  -- search again
         || alterMinSkill < 10
         || alterMinSkill >= 10 && alterSkill >= alterMinSkill ->
         if | alterSkill < alterMinSkill -> failSer AlterUnwalked
            | EM.member tpos $ lfloor lvl -> failSer AlterBlockItem
            | otherwise -> do
              let fs = TK.tfeature $ okind t
              verAlters <- verifyAlters fs
              case verAlters of
                Right() ->
                  return $ Right $ RequestAnyAbility $ ReqAlter tpos Nothing
                Left err -> return $ Left err
            -- We don't use MoveSer, because we don't hit invisible actors.
            -- The potential invisible actor, e.g., in a wall,
            -- making the player use a turn.
            -- If server performed an attack for free
            -- on the invisible actor anyway, the player (or AI)
            -- would be tempted to repeatedly hit random walls
            -- in hopes of killing a monster lurking within.
            -- If the action had a cost, misclicks would incur the cost, too.
            -- Right now the player may repeatedly alter tiles trying to learn
            -- about invisible pass-wall actors, but when an actor detected,
            -- it costs a turn and does not harm the invisible actors,
            -- so it's not so tempting.
       -- Ignore a known boring, not accessible tile.
       | otherwise -> failWith "never mind"
  return $! runStopOrCmd

-- * RunOnceAhead

runOnceAheadHuman :: MonadClientUI m => m (Either MError ReqUI)
{-# INLINABLE runOnceAheadHuman #-}
runOnceAheadHuman = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  leader <- getLeaderUI
  Config{configRunStopMsgs} <- getsSession sconfig
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
      if configRunStopMsgs
      then weaveJust <$> failWith "run stop: automatic leader change"
      else return $ Left Nothing
    Just _runParams | keyPressed -> do
      discardPressedKey
      stopPlayBack
      if configRunStopMsgs
      then weaveJust <$> failWith "run stop: key pressed"
      else weaveJust <$> failWith "interrupted"
    Just runParams -> do
      arena <- getArenaUI
      runOutcome <- continueRun arena runParams
      case runOutcome of
        Left stopMsg -> do
          stopPlayBack
          if configRunStopMsgs
          then weaveJust <$> failWith ("run stop:" <+> stopMsg)
          else return $ Left Nothing
        Right runCmd ->
          return $ Right $ ReqUITimed runCmd

-- * MoveOnceToXhair

moveOnceToXhairHuman :: MonadClientUI m => m (FailOrCmd RequestAnyAbility)
{-# INLINABLE moveOnceToXhairHuman #-}
moveOnceToXhairHuman = goToXhair True False

goToXhair :: MonadClientUI m
          => Bool -> Bool -> m (FailOrCmd RequestAnyAbility)
{-# INLINABLE goToXhair #-}
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
      Just c | c == bpos b -> do
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
              Left stopMsg -> failWith stopMsg
              Right (finalGoal, dir) ->
                moveRunHuman initialStep finalGoal run False dir
          _ -> do
            let !_A = assert (initialStep || not run) ()
            (bfs, mpath) <- getCacheBfsAndPath leader c
            xhairMoused <- getsSession sxhairMoused
            case mpath of
              _ | xhairMoused && isNothing (accessBfs bfs c) ->
                failWith "no route to crosshair"
              NoPath -> failWith "no route to crosshair"
              AndPath{pathList=[]} -> assert `failure` (leader, b, c)
              AndPath{pathList = p1 : _, pathSource} -> do
                let finalGoal = p1 == c
                    dir = towards pathSource p1
                moveRunHuman initialStep finalGoal run False dir

multiActorGoTo :: MonadClientUI m
               => LevelId -> Point -> RunParams
               -> m (Either Text (Bool, Vector))
{-# INLINABLE multiActorGoTo #-}
multiActorGoTo arena c paramOld =
  case paramOld of
    RunParams{runMembers = []} ->
      return $ Left "selected actors no longer there"
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
        (bfs, mpath) <- getCacheBfsAndPath r c
        xhairMoused <- getsSession sxhairMoused
        case mpath of
          _ | xhairMoused && isNothing (accessBfs bfs c) ->
            return $ Left "no route to crosshair"
          NoPath -> return $ Left "no route to crosshair"
          AndPath{pathList=[]} ->
            -- This actor already at goal; will be caught in goToXhair.
            return $ Left ""
          AndPath{pathList = p1 : _, pathSource} -> do
            let finalGoal = p1 == c
                dir = towards pathSource p1
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
                return $ Left "actor in the way"

-- * RunOnceToXhair

runOnceToXhairHuman :: MonadClientUI m => m (FailOrCmd RequestAnyAbility)
{-# INLINABLE runOnceToXhairHuman #-}
runOnceToXhairHuman = goToXhair True True

-- * ContinueToXhair

continueToXhairHuman :: MonadClientUI m => m (FailOrCmd RequestAnyAbility)
{-# INLINABLE continueToXhairHuman #-}
continueToXhairHuman = goToXhair False False{-irrelevant-}

-- * MoveItem

moveItemHuman :: forall m. MonadClientUI m
              => [CStore] -> CStore -> Maybe MU.Part -> Bool
              -> m (FailOrCmd (RequestTimed 'AbMoveItem))
{-# INLINABLE moveItemHuman #-}
moveItemHuman cLegalRaw destCStore mverb auto = do
  itemSel <- getsSession sitemSel
  case itemSel of
    Just (fromCStore, iid) | cLegalRaw /= [CGround]  -- not normal pickup
                             && fromCStore /= destCStore -> do  -- not vacuous
      leader <- getLeaderUI
      b <- getsState $ getActorBody leader
      bag <- getsState $ getBodyStoreBag b fromCStore
      case iid `EM.lookup` bag of
        Nothing -> moveItemHuman cLegalRaw destCStore mverb auto
        Just (k, it) -> do
          itemToF <- itemToFullClient
          let eqpFree = eqpFreeN b
              kToPick | destCStore == CEqp = min eqpFree k
                      | otherwise = k
          socK <- pickNumber True kToPick
          case socK of
            Left Nothing -> moveItemHuman cLegalRaw destCStore mverb auto
            Left (Just err) -> return $ Left err
            Right kChosen ->
              let is = ( fromCStore
                       , [(iid, itemToF iid (kChosen, take kChosen it))] )
              in moveItems cLegalRaw is destCStore
    _ -> do
      mis <- selectItemsToMove cLegalRaw destCStore mverb auto
      case mis of
        Left err -> return $ Left err
        Right is -> moveItems cLegalRaw is destCStore

selectItemsToMove :: forall m. MonadClientUI m
                  => [CStore] -> CStore -> Maybe MU.Part -> Bool
                  -> m (FailOrCmd (CStore, [(ItemId, ItemFull)]))
{-# INLINABLE selectItemsToMove #-}
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
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup leader actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` leader
      calmE = calmEnough b ar
      cLegal | calmE = cLegalRaw
             | destCStore == CSha = []
             | otherwise = delete CSha cLegalRaw
      prompt = makePhrase ["What to", verb]
      promptEqp = makePhrase ["What consumable to", verb]
      p :: CStore -> (Text, m Suitability)
      p cstore = if cstore `elem` [CEqp, CSha] && cLegalRaw /= [CGround]
                 then (promptEqp, return $ SuitsSomething $ \itemFull -> goesIntoEqp $ itemBase itemFull)
                 else (prompt, return SuitsEverything)
      (promptGeneric, psuit) = p destCStore
  ggi <-
    if auto
    then getAnyItems psuit prompt promptGeneric cLegalRaw cLegal False False
    else getAnyItems psuit prompt promptGeneric cLegalRaw cLegal True True
  case ggi of
    Right (l, MStore fromCStore) -> return $ Right (fromCStore, l)
    Left err -> failWith err
    _ -> assert `failure` ggi

moveItems :: forall m. MonadClientUI m
          => [CStore] -> (CStore, [(ItemId, ItemFull)]) -> CStore
          -> m (FailOrCmd (RequestTimed 'AbMoveItem))
{-# INLINABLE moveItems #-}
moveItems cLegalRaw (fromCStore, l) destCStore = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup leader actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` leader
      calmE = calmEnough b ar
      ret4 :: MonadClientUI m
           => [(ItemId, ItemFull)]
           -> Int -> [(ItemId, Int, CStore, CStore)]
           -> m (FailOrCmd [(ItemId, Int, CStore, CStore)])
      ret4 [] _ acc = return $ Right $ reverse acc
      ret4 ((iid, itemFull) : rest) oldN acc = do
        let k = itemK itemFull
            !_A = assert (k > 0) ()
            retRec toCStore =
              let n = oldN + if toCStore == CEqp then k else 0
              in ret4 rest n ((iid, k, fromCStore, toCStore) : acc)
        if cLegalRaw == [CGround]  -- normal pickup
        then case destCStore of
          CEqp | calmE && goesIntoSha (itemBase itemFull) ->
            retRec CSha
          CEqp | not $ goesIntoEqp (itemBase itemFull) ->
            retRec CInv
          CEqp | eqpOverfull b (oldN + k) -> do
            -- If this stack doesn't fit, we don't equip any part of it,
            -- but we may equip a smaller stack later in the same pickup.
            -- TODO: try to ask for a number of items, thus giving the player
            -- the option of picking up a part.
            let fullWarn = if eqpOverfull b (oldN + 1)
                           then EqpOverfull
                           else EqpStackFull
            msgAdd $ "Warning:" <+> showReqFailure fullWarn <> "."
            retRec $ if calmE then CSha else CInv
          _ ->
            retRec destCStore
        else case destCStore of
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
      Right [] -> assert `failure` l
      Right lr -> Right $ ReqMoveItems lr

-- * Project

projectHuman :: MonadClientUI m
             => [Trigger] -> m (FailOrCmd (RequestTimed 'AbProject))
{-# INLINABLE projectHuman #-}
projectHuman ts = do
  itemSel <- getsSession sitemSel
  case itemSel of
    Just (fromCStore, iid) -> do
      leader <- getLeaderUI
      b <- getsState $ getActorBody leader
      bag <- getsState $ getBodyStoreBag b fromCStore
      case iid `EM.lookup` bag of
        Nothing -> failWith "no item to fling"
        Just kit -> do
          itemToF <- itemToFullClient
          let i = (fromCStore, (iid, itemToF iid kit))
          projectItem ts i
    Nothing -> failWith "no item to fling"

projectItem :: MonadClientUI m
            => [Trigger] -> (CStore, (ItemId, ItemFull))
            -> m (FailOrCmd (RequestTimed 'AbProject))
{-# INLINABLE projectItem #-}
projectItem ts (fromCStore, (iid, itemFull)) = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup leader actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` leader
      calmE = calmEnough b ar
  if not calmE && fromCStore == CSha then failSer ItemNotCalm
  else do
    mpsuitReq <- psuitReq ts
    case mpsuitReq of
      Left err -> failWith err
      Right psuitReqFun ->
        case psuitReqFun itemFull of
          Left reqFail -> failSer reqFail
          Right (pos, _) -> do
            -- Set personal target to the aim position, to easily repeat.
            mposTgt <- leaderTgtToPos
            unless (Just pos == mposTgt) $ do
              sxhair <- getsClient sxhair
              modifyClient $ updateTarget leader (const $ Just sxhair)
            -- Project.
            eps <- getsClient seps
            return $ Right $ ReqProject pos eps iid fromCStore

-- * Apply

-- TODO: factor out item getting
applyHuman :: MonadClientUI m
           => [Trigger] -> m (FailOrCmd (RequestTimed 'AbApply))
{-# INLINABLE applyHuman #-}
applyHuman ts = do
  itemSel <- getsSession sitemSel
  case itemSel of
    Just (fromCStore, iid) -> do
      leader <- getLeaderUI
      b <- getsState $ getActorBody leader
      bag <- getsState $ getBodyStoreBag b fromCStore
      case iid `EM.lookup` bag of
        Nothing -> failWith "no item to apply"
        Just kit -> do
          itemToF <- itemToFullClient
          let i = (fromCStore, (iid, itemToF iid kit))
          applyItem ts i
    Nothing -> failWith "no item to apply"

applyItem :: MonadClientUI m
          => [Trigger] -> (CStore, (ItemId, ItemFull))
          -> m (FailOrCmd (RequestTimed 'AbApply))
{-# INLINABLE applyItem #-}
applyItem ts (fromCStore, (iid, itemFull)) = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup leader actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` leader
      calmE = calmEnough b ar
  if not calmE && fromCStore == CSha then failSer ItemNotCalm
  else do
    p <- permittedApplyClient $ triggerSymbols ts
    case p itemFull of
      Left reqFail -> failSer reqFail
      Right _ -> return $ Right $ ReqApply iid fromCStore

-- * AlterDir

-- | Ask for a direction and alter a tile in the specified way, if possible.
alterDirHuman :: MonadClientUI m
              => [Trigger] -> m (FailOrCmd (RequestTimed 'AbAlter))
{-# INLINABLE alterDirHuman #-}
alterDirHuman ts = do
  Config{configVi, configLaptop} <- getsSession sconfig
  let verb1 = case ts of
        [] -> "alter"
        tr : _ -> verb tr
      keys = K.escKM
             : K.leftButtonReleaseKM
             : map (K.KM K.NoModifier) (K.dirAllKey configVi configLaptop)
      prompt = makePhrase
        ["Where to", verb1 <> "? [movement key] [pointer]"]
  promptAdd prompt
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
      case K.handleDir configVi configLaptop km of
        Nothing -> failWith "never mind"
        Just dir -> alterTile ts dir

-- | Try to alter a tile using a feature in the given direction.
alterTile :: MonadClientUI m
          => [Trigger] -> Vector -> m (FailOrCmd (RequestTimed 'AbAlter))
{-# INLINABLE alterTile #-}
alterTile ts dir = do
  cops@Kind.COps{cotile, coTileSpeedup} <- getsState scops
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorSk <- actorSkillsClient leader
  lvl <- getLevel $ blid b
  let alterSkill = EM.findWithDefault 0 AbAlter actorSk
      tpos = bpos b `shift` dir
      t = lvl `at` tpos
      alterFeats = alterFeatures ts
      verb1 = case ts of
        [] -> "alter"
        tr : _ -> verb tr
      msg = makeSentence ["you", verb1, "towards", MU.Text $ compassText dir]
  case filter (\feat -> Tile.hasFeature cotile feat t) alterFeats of
    _ : _ | alterSkill < Tile.alterMinSkill coTileSpeedup t ->
      failSer AlterUnskilled
    [] -> failWith $ guessAlter cops alterFeats t
    fs@(feat : _) ->
      if EM.notMember tpos $ lfloor lvl then
        if null (posToAidsLvl tpos lvl) then do
          verAlters <- verifyAlters fs
          case verAlters of
            Right() -> do
              msgAdd msg
              return $ Right $ ReqAlter tpos $ Just feat
            Left err -> return $ Left err
        else failSer AlterBlockActor
      else failSer AlterBlockItem

alterFeatures :: [Trigger] -> [TK.Feature]
alterFeatures [] = []
alterFeatures (AlterFeature{feature} : ts) = feature : alterFeatures ts
alterFeatures (_ : ts) = alterFeatures ts

verifyAlters :: MonadClientUI m => [TK.Feature] -> m (FailOrCmd ())
{-# INLINABLE verifyAlters #-}
verifyAlters fs = do
  let f acc feat = case acc of
        Right() -> verifyAlter feat
        Left{} -> return acc
  foldM f (Right ()) fs

-- | Verify important features, such as fleeing the dungeon.
verifyAlter :: MonadClientUI m => TK.Feature -> m (FailOrCmd ())
{-# INLINABLE verifyAlter #-}
verifyAlter feat = case feat of
  TK.Cause IK.Escape{} -> do
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
          -- The player can back off at each of these steps.
          go1 <- displaySpaceEsc ColorBW
                   "Afraid of the challenge? Leaving so soon and empty-handed?"
          if not go1 then failWith "brave soul!"
          else do
             go2 <- displaySpaceEsc ColorBW
                     "Next time try to grab some loot before escape!"
             if not go2 then failWith "here's your chance!"
             else return $ Right ()
        else return $ Right ()
  _ -> return $ Right ()

-- | Guess and report why the bump command failed.
guessAlter :: Kind.COps -> [TK.Feature] -> Kind.Id TileKind -> Text
guessAlter Kind.COps{cotile} (TK.OpenTo _ : _) t
  | Tile.isClosable cotile t = "already open"
guessAlter _ (TK.OpenTo _ : _) _ = "cannot be opened"
guessAlter Kind.COps{cotile} (TK.CloseTo _ : _) t
  | Tile.isOpenable cotile t = "already closed"
guessAlter _ (TK.CloseTo _ : _) _ = "cannot be closed"
guessAlter _ _ _ = "never mind"

-- * Help

-- | Display command help.
helpHuman :: MonadClientUI m
          => (HumanCmd.HumanCmd -> m (Either MError ReqUI))
          -> m (Either MError ReqUI)
{-# INLINABLE helpHuman #-}
helpHuman cmdAction = do
  lidV <- viewedLevelUI
  Level{lxsize, lysize} <- getLevel lidV  -- TODO: screen length or viewLevel
  menuIxHelp <- getsSession smenuIxHelp
  keyb <- getsSession sbinding
  let keyH = keyHelp keyb 1
      splitHelp (t, okx) =
        splitOKX lxsize (lysize + 3) (textToAL t) [K.spaceKM, K.escKM] okx
      sli = toSlideshow $ concat $ map splitHelp keyH
  (ekm, pointer) <-
    displayChoiceScreen ColorFull True menuIxHelp sli [K.spaceKM, K.escKM]
  modifySession $ \sess -> sess { smenuIxHelp = pointer
                                , skeysHintMode = KeysHintBlocked }
  case ekm of
    Left km -> case km `M.lookup` bcmdMap keyb of
      _ | km == K.escKM -> return $ Left Nothing
      Just (_desc, _cats, cmd) -> cmdAction cmd
      Nothing -> weaveJust <$> failWith "never mind"
    Right _slot -> assert `failure` ekm

-- * ItemMenu

itemMenuHuman :: MonadClientUI m
              => (HumanCmd.HumanCmd -> m (Either MError ReqUI))
              -> m (Either MError ReqUI)
{-# INLINABLE itemMenuHuman #-}
itemMenuHuman cmdAction = do
  itemSel <- getsSession sitemSel
  case itemSel of
    Just (fromCStore, iid) -> do
      leader <- getLeaderUI
      b <- getsState $ getActorBody leader
      bag <- getsState $ getBodyStoreBag b fromCStore
      case iid `EM.lookup` bag of
        Nothing -> weaveJust <$> failWith "no object to open Item Menu for"
        Just kit -> do
          itemToF <- itemToFullClient
          lidV <- viewedLevelUI
          Level{lxsize, lysize} <- getLevel lidV
          localTime <- getsState $ getLocalTime (blid b)
          foundText <- itemIsFound iid leader fromCStore
          let itemFull = itemToF iid kit
              attrLine = itemDesc fromCStore localTime itemFull
              ov = splitAttrLine lxsize $ attrLine <+:> textToAL foundText
          report <- getReportUI
          keyb <- getsSession sbinding
          let fmt n k h = " " <> T.justifyLeft n ' ' k <+> h
              keyL = 11
              keyCaption = fmt keyL "keys" "command"
              offset = 1 + length ov
              (ov0, kxs0) = okxsN keyb offset keyL (const True)
                                  HumanCmd.CmdItemMenu [keyCaption] []
              t0 = makeSentence [ MU.SubjectVerbSg (partActor b) "choose"
                                , "an object", MU.Text $ ppCStoreIn fromCStore ]
              al1 = renderReport report <+:> textToAL t0
              splitHelp (al, okx) =
                splitOKX lxsize (lysize + 1) al [K.spaceKM, K.escKM] okx
              sli = toSlideshow $ splitHelp (al1, (ov ++ ov0, kxs0))
          recordHistory  -- report shown, remove it to history
          (ekm, _) <-
            displayChoiceScreen ColorFull False 2 sli [K.spaceKM, K.escKM]
          case ekm of
            Left km -> case km `M.lookup` bcmdMap keyb of
              _ | km == K.escKM -> weaveJust <$> failWith "never mind"
              _ | km == K.spaceKM -> return $ Left Nothing
              Just (_desc, _cats, cmd) -> cmdAction cmd
              Nothing -> weaveJust <$> failWith "never mind"
            Right _slot -> assert `failure` ekm
    Nothing -> weaveJust <$> failWith "no object to open Item Menu for"

-- * ChooseItemMenu

chooseItemMenuHuman :: MonadClientUI m
                    => (HumanCmd.HumanCmd -> m (Either MError ReqUI))
                    -> ItemDialogMode
                    -> m (Either MError ReqUI)
{-# INLINABLE chooseItemMenuHuman #-}
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

-- TODO: avoid String
-- | Display the main menu.
mainMenuHuman :: MonadClientUI m
              => (HumanCmd.HumanCmd -> m (Either MError ReqUI))
              -> m (Either MError ReqUI)
{-# INLINABLE mainMenuHuman #-}
mainMenuHuman cmdAction = do
  cops@Kind.COps{corule} <- getsState scops
  Binding{bcmdList} <- getsSession sbinding
  gameMode <- getGameMode
  snxtScenario <- getsClient snxtScenario
  scurDiff <- getsClient scurDiff
  snxtDiff <- getsClient snxtDiff
  let stripFrame t = tail . init $ T.lines t
      pasteVersion :: [String] -> [String]
      pasteVersion art =
        let pathsVersion = rpathsVersion $ Kind.stdRuleset corule
            version = " Version " ++ showVersion pathsVersion
                      ++ " (frontend: " ++ frontendName
                      ++ ", engine: LambdaHack " ++ showVersion Self.version
                      ++ ") "
            versionLen = length version
        in init art ++ [take (80 - versionLen) (last art) ++ version]
      tnextDiff = "new game difficulty:" <+> tshow snxtDiff
      nxtGameName = mname $ nxtGameMode cops snxtScenario
      tnextScenario = "new scenario:" <+> nxtGameName
      -- Key-description-command tuples.
      kds = [ (K.mkKM "s", (tnextScenario, HumanCmd.GameScenarioIncr))
            , (K.mkKM "d", (tnextDiff, HumanCmd.GameDifficultyIncr)) ]
            ++ [ (km, (desc, cmd))
               | (km, ([HumanCmd.CmdMainMenu], desc, cmd)) <- bcmdList ]
      statusLen = 30
      bindingLen = 28
      gameName = mname gameMode
      gameInfo = map T.unpack $
                 [ T.justifyLeft statusLen ' ' ""
                 , T.justifyLeft statusLen ' '
                   $ "Ongoing scenario:" <+> gameName
                 , T.justifyLeft statusLen ' '
                   $ "Ongoing game difficulty:" <+> tshow scurDiff
                 , T.justifyLeft statusLen ' ' "" ]
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
      mainMenuArt = rmainMenuArt $ Kind.stdRuleset corule
      artWithVersion = pasteVersion $ map T.unpack $ stripFrame mainMenuArt
      menuOverwritten = overwrite $ zip [0..] artWithVersion
      (menuOvLines, mkyxs) = unzip menuOverwritten
      kyxs = catMaybes mkyxs
      ov = map stringToAL menuOvLines
  menuIxMain <- getsSession smenuIxMain
  (ekm, pointer) <- displayChoiceScreen ColorFull True menuIxMain
                                        (menuToSlideshow (ov, kyxs)) [K.escKM]
  modifySession $ \sess -> sess {smenuIxMain = pointer}
  case ekm of
    Left km -> case km `lookup` kds of
      Just (_desc, cmd) -> cmdAction cmd
      Nothing -> weaveJust <$> failWith "never mind"
    Right _slot -> assert `failure` ekm

-- * GameScenarioIncr

gameScenarioIncr :: MonadClientUI m => m ()
{-# INLINABLE gameScenarioIncr #-}
gameScenarioIncr =
  modifyClient $ \cli -> cli {snxtScenario = snxtScenario cli + 1}

-- * GameDifficultyIncr

gameDifficultyIncr :: MonadClientUI m => m ()
{-# INLINABLE gameDifficultyIncr #-}
gameDifficultyIncr = do
  snxtDiff <- getsClient snxtDiff
  let delta = 1
      d | snxtDiff + delta > difficultyBound = 1
        | snxtDiff + delta < 1 = difficultyBound
        | otherwise = snxtDiff + delta
  modifyClient $ \cli -> cli {snxtDiff = d}

-- * GameRestart

gameRestartHuman :: MonadClientUI m => m (FailOrCmd ReqUI)
{-# INLINABLE gameRestartHuman #-}
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
    snxtDiff <- getsClient snxtDiff
    Config{configHeroNames} <- getsSession sconfig
    let nxtGameGroup = toGroupName nxtGameName  -- a tiny bit hacky
    return $ Right $ ReqUIGameRestart nxtGameGroup snxtDiff configHeroNames
  else do
    msg2 <- rndToActionForget $ oneOf
              [ "yea, would be a pity to leave them all to die"
              , "yea, a shame to get your team stranded" ]
    failWith msg2

nxtGameMode :: Kind.COps -> Int -> ModeKind
nxtGameMode Kind.COps{comode=Kind.Ops{ofoldlGroup'}} snxtScenario =
  let f acc _p _i a = a : acc
      campaignModes = ofoldlGroup' "campaign scenario" f []
  in campaignModes !! (snxtScenario `mod` length campaignModes)

-- * GameExit

gameExitHuman :: MonadClientUI m => m ReqUI
{-# INLINABLE gameExitHuman #-}
gameExitHuman = do
  -- Announce before the saving started, since it can take a while.
  promptAdd "Saving game. The program stops now."
  return ReqUIGameExit

-- * GameSave

gameSaveHuman :: MonadClientUI m => m ReqUI
{-# INLINABLE gameSaveHuman #-}
gameSaveHuman = do
  -- Announce before the saving started, since it can take a while.
  promptAdd "Saving game backup."
  return ReqUIGameSave

-- * Tactic

-- Note that the difference between seek-target and follow-the-leader tactic
-- can influence even a faction with passive actors. E.g., if a passive actor
-- has an extra active skill from equipment, he moves every turn.
-- TODO: set tactic for allied passive factions, too or all allied factions
-- and perhaps even factions with a leader should follow our leader
-- and his target, not their leader.
tacticHuman :: MonadClientUI m => m (FailOrCmd ReqUI)
{-# INLINABLE tacticHuman #-}
tacticHuman = do
  fid <- getsClient sside
  fromT <- getsState $ ftactic . gplayer . (EM.! fid) . sfactionD
  let toT = if fromT == maxBound then minBound else succ fromT
  go <- displaySpaceEsc ColorFull
        $ "Current henchmen tactic is" <+> tshow fromT
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
{-# INLINABLE automateHuman #-}
automateHuman = do
  -- BFS is not updated while automated, which would lead to corruption.
  modifySession $ \sess -> sess {saimMode = Nothing}
  go <- displaySpaceEsc ColorBW
          "Ceding control to AI (press any key to regain)."
  if not go
    then failWith "automation canceled"
    else return $ Right ReqUIAutomate
