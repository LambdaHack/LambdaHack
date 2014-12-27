{-# LANGUAGE DataKinds, GADTs #-}
-- | Semantics of 'Command.Cmd' client commands that return server commands.
-- A couple of them do not take time, the rest does.
-- Here prompts and menus and displayed, but any feedback resulting
-- from the commands (e.g., from inventory manipulation) is generated later on,
-- for all clients that witness the results of the commands.
-- TODO: document
module Game.LambdaHack.Client.UI.HandleHumanGlobalClient
  ( -- * Commands that usually take time
    moveRunHuman, waitHuman, moveItemHuman
  , projectHuman, applyHuman, alterDirHuman, triggerTileHuman
  , runOnceAheadHuman, moveOnceToCursorHuman
  , runOnceToCursorHuman, continueToCursorHuman
    -- * Commands that never take time
  , gameRestartHuman, gameExitHuman, gameSaveHuman, tacticHuman, automateHuman
  ) where

import Control.Applicative
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import Data.Maybe
import Data.Monoid
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.BfsClient
import Game.LambdaHack.Client.CommonClient
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.HandleHumanLocalClient
import Game.LambdaHack.Client.UI.HumanCmd (Trigger (..))
import Game.LambdaHack.Client.UI.InventoryClient
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.MsgClient
import Game.LambdaHack.Client.UI.RunClient
import Game.LambdaHack.Client.UI.WidgetClient
import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.TileKind (TileKind)
import qualified Game.LambdaHack.Content.TileKind as TK

-- * Move and Run

moveRunHuman :: MonadClientUI m
             => Bool -> Bool -> Bool -> Bool -> Vector
             -> m (SlideOrCmd RequestAnyAbility)
moveRunHuman initialStep finalGoal run runAhead dir = do
  tgtMode <- getsClient stgtMode
  if isJust tgtMode then
    fmap Left $ moveCursorHuman dir (if run then 10 else 1)
  else do
    arena <- getArenaUI
    leader <- getLeaderUI
    sb <- getsState $ getActorBody leader
    fact <- getsState $ (EM.! bfid sb) . sfactionD
    -- Start running in the given direction. The first turn of running
    -- succeeds much more often than subsequent turns, because we ignore
    -- most of the disturbances, since the player is mostly aware of them
    -- and still explicitly requests a run, knowing how it behaves.
    sel <- getsClient sselected
    let runMembers = if noRunWithMulti fact
                     then [leader]  -- TODO: warn?
                     else ES.toList (ES.delete leader sel) ++ [leader]
        runParams = RunParams { runLeader = leader
                              , runMembers
                              , runInitial = True
                              , runStopMsg = Nothing
                              , runWaiting = 0 }
        macroRun25 = ["CTRL-period", "CTRL-V"]
    when (initialStep && run) $ do
      modifyClient $ \cli ->
        cli {srunning = Just runParams}
      when runAhead $
        modifyClient $ \cli ->
          cli {slastPlay = map K.mkKM macroRun25 ++ slastPlay cli}
    -- When running, the invisible actor is hit (not displaced!),
    -- so that running in the presence of roving invisible
    -- actors is equivalent to moving (with visible actors
    -- this is not a problem, since runnning stops early enough).
    -- TODO: stop running at invisible actor
    let tpos = bpos sb `shift` dir
    -- We start by checking actors at the the target position,
    -- which gives a partial information (actors can be invisible),
    -- as opposed to accessibility (and items) which are always accurate
    -- (tiles can't be invisible).
    tgts <- getsState $ posToActors tpos arena
    case tgts of
      [] -> do  -- move or search or alter
        runStopOrCmd <- moveSearchAlterAid leader dir
        case runStopOrCmd of
          Left stopMsg -> failWith stopMsg
          Right runCmd ->
            -- Don't check @initialStep@ and @finalGoal@
            -- and don't stop going to target: door opening is mundane enough.
            return $ Right runCmd
      [((target, _), _)] | run && initialStep ->
        -- No @stopPlayBack@: initial displace is benign enough.
        -- Displacing requires accessibility, but it's checked later on.
        fmap RequestAnyAbility <$> displaceAid target
      _ : _ : _ | run && initialStep -> do
        assert (all (bproj . snd . fst) tgts) skip
        failSer DisplaceProjectiles
      ((target, tb), _) : _ | initialStep && finalGoal -> do
        stopPlayBack  -- don't ever auto-repeat melee
        -- No problem if there are many projectiles at the spot. We just
        -- attack the first one.
        -- We always see actors from our own faction.
        if bfid tb == bfid sb && not (bproj tb) then do
          let autoLvl = snd $ autoDungeonLevel fact
          if autoLvl then failSer NoChangeLvlLeader
          else do
            -- Select adjacent actor by bumping into him. Takes no time.
            success <- pickLeader True target
            assert (success `blame` "bump self"
                            `twith` (leader, target, tb)) skip
            return $ Left mempty
        else
          -- Attacking does not require full access, adjacency is enough.
          fmap RequestAnyAbility <$> meleeAid target
      _ : _ -> failWith "actor in the way"

-- | Actor atttacks an enemy actor or his own projectile.
meleeAid :: MonadClientUI m
         => ActorId -> m (SlideOrCmd (RequestTimed AbMelee))
meleeAid target = do
  leader <- getLeaderUI
  sb <- getsState $ getActorBody leader
  tb <- getsState $ getActorBody target
  sfact <- getsState $ (EM.! bfid sb) . sfactionD
  mel <- pickWeaponClient leader target
  case mel of
    [] -> failWith "nothing to melee with"
    wp : _ -> do
      let returnCmd = return $ Right wp
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
            => ActorId -> m (SlideOrCmd (RequestTimed AbDisplace))
displaceAid target = do
  cops <- getsState scops
  leader <- getLeaderUI
  sb <- getsState $ getActorBody leader
  tb <- getsState $ getActorBody target
  tfact <- getsState $ (EM.! bfid tb) . sfactionD
  activeItems <- activeItemsClient target
  disp <- getsState $ dispEnemy leader target activeItems
  actorSk <- getsState $ maxActorSkills target activeItems
  let immobile = EM.findWithDefault 0 AbDisplace actorSk <= 0
                 && EM.findWithDefault 0 AbMove actorSk <= 0
      spos = bpos sb
      tpos = bpos tb
      adj = checkAdjacent sb tb
      atWar = isAtWar tfact (bfid sb)
  if not adj then failSer DisplaceDistant
  else if not (bproj tb) && atWar
          && actorDying tb then failSer DisplaceDying
  else if not (bproj tb) && atWar
          && braced tb then failSer DisplaceBraced
  else if not (bproj tb) && atWar
          && immobile then failSer DisplaceImmobile
  else if not disp && atWar then failSer DisplaceSupported
  else do
    let lid = blid sb
    lvl <- getLevel lid
    -- Displacing requires full access.
    if accessible cops lvl spos tpos then do
      tgts <- getsState $ posToActors tpos lid
      case tgts of
        [] -> assert `failure` (leader, sb, target, tb)
        [_] -> do
          return $ Right $ ReqDisplace target
        _ -> failSer DisplaceProjectiles
    else failSer DisplaceAccess

-- | Actor moves or searches or alters. No visible actor at the position.
moveSearchAlterAid :: MonadClient m
                   => ActorId -> Vector -> m (Either Msg RequestAnyAbility)
moveSearchAlterAid source dir = do
  cops@Kind.COps{cotile} <- getsState scops
  sb <- getsState $ getActorBody source
  let lid = blid sb
  lvl <- getLevel lid
  let spos = bpos sb           -- source position
      tpos = spos `shift` dir  -- target position
      t = lvl `at` tpos
      runStopOrCmd =
        -- Movement requires full access.
        if accessible cops lvl spos tpos then
          -- The potential invisible actor is hit. War started without asking.
          Right $ RequestAnyAbility $ ReqMove dir
        -- No access, so search and/or alter the tile. Non-walkability is
        -- not implied by the lack of access.
        else if not (Tile.isWalkable cotile t)
                && (not (knownLsecret lvl)
                    || (isSecretPos lvl tpos  -- possible secrets here
                        && (Tile.isSuspect cotile t  -- not yet searched
                            || Tile.hideAs cotile t /= t))  -- search again
                    || Tile.isOpenable cotile t
                    || Tile.isClosable cotile t
                    || Tile.isChangeable cotile t) then
          if EM.member tpos $ lfloor lvl then
            Left $ showReqFailure AlterBlockItem
          else
            Right $ RequestAnyAbility $ ReqAlter tpos Nothing
            -- We don't use MoveSer, because we don't hit invisible actors.
            -- The potential invisible actor, e.g., in a wall or in
            -- an inaccessible doorway, is made known, taking a turn.
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
       else Left "never mind"
  return $! runStopOrCmd

-- * Wait

-- | Leader waits a turn (and blocks, etc.).
waitHuman :: MonadClientUI m => m (RequestTimed AbWait)
waitHuman = do
  modifyClient $ \cli -> cli {swaitTimes = abs (swaitTimes cli) + 1}
  return ReqWait

-- * MoveItem

moveItemHuman :: MonadClientUI m
              => [CStore] -> CStore -> (Maybe MU.Part) -> Bool
              -> m (SlideOrCmd (RequestTimed AbMoveItem))
moveItemHuman cLegalRaw destCStore mverb auto = do
  assert (destCStore `notElem` cLegalRaw) skip
  let verb = fromMaybe (MU.Text $ verbCStore destCStore) mverb
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  activeItems <- activeItemsClient leader
  let calmE = calmEnough b activeItems
      cLegal = if calmE
               then cLegalRaw
               else if destCStore == CSha
                    then []
                    else delete CSha cLegalRaw
      ret4 :: MonadClientUI m
           => CStore -> [(ItemId, ItemFull)]
           -> Int -> [(ItemId, Int, CStore, CStore)]
           -> m (Either Slideshow [(ItemId, Int, CStore, CStore)])
      ret4 _ [] _ acc = return $ Right acc
      ret4 fromCStore ((iid, itemFull) : rest) oldN acc = do
        let k = itemK itemFull
            n = k + oldN
            retRec toCStore =
              ret4 fromCStore rest n ((iid, k, fromCStore, toCStore) : acc)
        if cLegalRaw == [CGround]  -- normal pickup
        then case destCStore of
          CEqp | calmE && goesIntoSha (itemBase itemFull) ->
            retRec CSha
          CEqp | goesIntoInv (itemBase itemFull) ->
            retRec CInv
          CEqp | eqpOverfull b n -> do
            msgAdd $ "Warning:" <+> showReqFailure EqpOverfull <> "."
            retRec CInv
          _ ->
            retRec destCStore
        else case destCStore of
          CEqp | eqpOverfull b n -> failSer EqpOverfull
          _ -> retRec destCStore
  ggi <- if auto
         then getAnyItems verb cLegalRaw cLegal False False
         else getAnyItems verb cLegalRaw cLegal True True
  case ggi of
    Right (l, CActor leader2 fromCStore) -> do
      b2 <- getsState $ getActorBody leader2
      activeItems2 <- activeItemsClient leader2
      let calmE2 = calmEnough b2 activeItems2
      -- This is not ideal, because the failure message comes late,
      -- but it's simple and good enough.
      if not calmE2 && destCStore == CSha then failSer ItemNotCalm
      else do
        l4 <- ret4 fromCStore l 0 []
        return $! case l4 of
          Left sli -> Left sli
          Right [] -> assert `failure` ggi
          Right lr -> Right $ ReqMoveItems lr
    Left slides -> return $ Left slides
    _ -> assert `failure` ggi

-- * Project

projectHuman :: MonadClientUI m
             => [Trigger] -> m (SlideOrCmd (RequestTimed AbProject))
projectHuman ts = do
  leader <- getLeaderUI
  tgtPos <- leaderTgtToPos
  tgt <- getsClient $ getTarget leader
  case tgtPos of
    Nothing -> failWith "last target invalid"
    Just pos -> do
      -- Set cursor to the personal target, temporarily.
      oldCursor <- getsClient scursor
      modifyClient $ \cli -> cli {scursor = fromMaybe (scursor cli) tgt}
      -- Show the targeting line, temporarily.
      oldTgtMode <- getsClient stgtMode
      lidV <- viewedLevel
      modifyClient $ \cli -> cli {stgtMode = Just $ TgtMode lidV}
      canAim <- leaderTgtAims
      oldEps <- getsClient seps
      outcome <- case canAim of
        Right newEps -> do
          -- Modify @seps@,, temporarily.
          modifyClient $ \cli -> cli {seps = newEps}
          projectPos ts pos
        Left cause -> failWith cause
      modifyClient $ \cli -> cli { stgtMode = oldTgtMode
                                 , scursor = oldCursor
                                 , seps = oldEps }
      return outcome

projectPos :: MonadClientUI m
           => [Trigger] -> Point -> m (SlideOrCmd (RequestTimed AbProject))
projectPos ts tpos = do
  Kind.COps{cotile} <- getsState scops
  leader <- getLeaderUI
  eps <- getsClient seps
  sb <- getsState $ getActorBody leader
  let lid = blid sb
      spos = bpos sb
  Level{lxsize, lysize} <- getLevel lid
  case bla lxsize lysize eps spos tpos of
    Nothing -> failSer ProjectAimOnself
    Just [] -> assert `failure` "project from the edge of level"
                      `twith` (spos, tpos, sb, ts)
    Just (pos : _) -> do
      lvl <- getLevel lid
      let t = lvl `at` pos
      if not $ Tile.isWalkable cotile t
        then failSer ProjectBlockTerrain
        else do
          mab <- getsState $ posToActor pos lid
          if maybe True (bproj . snd . fst) mab
          then projectEps ts tpos eps
          else failSer ProjectBlockActor

projectEps :: MonadClientUI m
           => [Trigger] -> Point -> Int
           -> m (SlideOrCmd (RequestTimed AbProject))
projectEps ts tpos eps = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorSk <- actorSkillsClient leader
  let skill = EM.findWithDefault 0 AbProject actorSk
  activeItems <- activeItemsClient leader
  let cLegal = [CGround, CInv, CEqp]
      (verb1, object1) = case ts of
        [] -> ("aim", "item")
        tr : _ -> (verb tr, object tr)
      triggerSyms = triggerSymbols ts
      p itemFull@ItemFull{itemBase} =
        let legal = permittedProject triggerSyms False skill itemFull b activeItems
        in case legal of
          Left{} -> legal
          Right False -> legal
          Right True -> Right $ totalRange itemBase >= chessDist (bpos b) tpos
      prompt = makePhrase ["What", object1, "to", verb1]
      promptGeneric = "What item to fling"
  ggi <- getGroupItem (either (const False) id . p) prompt promptGeneric cLegal cLegal
  case ggi of
    Right ((iid, itemFull), CActor _ fromCStore) ->
      case p itemFull of
        Left reqFail -> failSer reqFail
        Right _ -> return $ Right $ ReqProject tpos eps iid fromCStore
    Left slides -> return $ Left slides
    _ -> assert `failure` ggi

triggerSymbols :: [Trigger] -> [Char]
triggerSymbols [] = []
triggerSymbols (ApplyItem{symbol} : ts) = symbol : triggerSymbols ts
triggerSymbols (_ : ts) = triggerSymbols ts

-- * Apply

applyHuman :: MonadClientUI m
           => [Trigger] -> m (SlideOrCmd (RequestTimed AbApply))
applyHuman ts = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorSk <- actorSkillsClient leader
  let skill = EM.findWithDefault 0 AbProject actorSk
  activeItems <- activeItemsClient leader
  localTime <- getsState $ getLocalTime (blid b)
  let cLegal = [CGround, CInv, CEqp]
      (verb1, object1) = case ts of
        [] -> ("activate", "item")
        tr : _ -> (verb tr, object tr)
      triggerSyms = triggerSymbols ts
      p itemFull = permittedApply triggerSyms localTime skill itemFull b activeItems
      prompt = makePhrase ["What", object1, "to", verb1]
      promptGeneric = "What item to activate"
  ggi <- getGroupItem (either (const False) id . p)
                      prompt promptGeneric cLegal cLegal
  case ggi of
    Right ((iid, itemFull), CActor _ fromCStore) ->
      case p itemFull of
        Left reqFail -> failSer reqFail
        Right _ -> return $ Right $ ReqApply iid fromCStore
    Left slides -> return $ Left slides
    _ -> assert `failure` ggi

-- * AlterDir

-- | Ask for a direction and alter a tile, if possible.
alterDirHuman :: MonadClientUI m
              => [Trigger] -> m (SlideOrCmd (RequestTimed AbAlter))
alterDirHuman ts = do
  Config{configVi, configLaptop} <- askConfig
  let verb1 = case ts of
        [] -> "alter"
        tr : _ -> verb tr
      keys = zipWith K.toKM (repeat K.NoModifier)
                            (K.dirAllKey configVi configLaptop)
      prompt = makePhrase ["What to", verb1 <> "? [movement key"]
  me <- displayChoiceUI prompt emptyOverlay keys
  case me of
    Left slides -> failSlides slides
    Right e -> K.handleDir configVi configLaptop e (flip alterTile ts)
                                                   (failWith "never mind")

-- | Player tries to alter a tile using a feature.
alterTile :: MonadClientUI m
          => Vector -> [Trigger] -> m (SlideOrCmd (RequestTimed AbAlter))
alterTile dir ts = do
  cops@Kind.COps{cotile} <- getsState scops
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  lvl <- getLevel $ blid b
  let tpos = bpos b `shift` dir
      t = lvl `at` tpos
      alterFeats = alterFeatures ts
  case filter (\feat -> Tile.hasFeature cotile feat t) alterFeats of
    [] -> failWith $ guessAlter cops alterFeats t
    feat : _ -> return $ Right $ ReqAlter tpos $ Just feat

alterFeatures :: [Trigger] -> [TK.Feature]
alterFeatures [] = []
alterFeatures (AlterFeature{feature} : ts) = feature : alterFeatures ts
alterFeatures (_ : ts) = alterFeatures ts

-- | Guess and report why the bump command failed.
guessAlter :: Kind.COps -> [TK.Feature] -> Kind.Id TileKind -> Msg
guessAlter Kind.COps{cotile} (TK.OpenTo _ : _) t
  | Tile.isClosable cotile t = "already open"
guessAlter _ (TK.OpenTo _ : _) _ = "cannot be opened"
guessAlter Kind.COps{cotile} (TK.CloseTo _ : _) t
  | Tile.isOpenable cotile t = "already closed"
guessAlter _ (TK.CloseTo _ : _) _ = "cannot be closed"
guessAlter _ _ _ = "never mind"

-- * TriggerTile

-- | Leader tries to trigger the tile he's standing on.
triggerTileHuman :: MonadClientUI m
                 => [Trigger] -> m (SlideOrCmd (RequestTimed AbTrigger))
triggerTileHuman ts = do
  tgtMode <- getsClient stgtMode
  if isJust tgtMode then do
    let getK tfs = case tfs of
          TriggerFeature {feature = TK.Cause (IK.Ascend k)} : _ -> Just k
          _ : rest -> getK rest
          [] -> Nothing
        mk = getK ts
    case mk of
      Nothing -> failWith "never mind"
      Just k -> fmap Left $ tgtAscendHuman k
  else triggerTile ts

-- | Player tries to trigger a tile using a feature.
triggerTile :: MonadClientUI m
            => [Trigger] -> m (SlideOrCmd (RequestTimed AbTrigger))
triggerTile ts = do
  cops@Kind.COps{cotile} <- getsState scops
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  lvl <- getLevel $ blid b
  let t = lvl `at` bpos b
      triggerFeats = triggerFeatures ts
  case filter (\feat -> Tile.hasFeature cotile feat t) triggerFeats of
    [] -> failWith $ guessTrigger cops triggerFeats t
    feat : _ -> do
      go <- verifyTrigger leader feat
      case go of
        Right () -> return $ Right $ ReqTrigger $ Just feat
        Left slides -> return $ Left slides

triggerFeatures :: [Trigger] -> [TK.Feature]
triggerFeatures [] = []
triggerFeatures (TriggerFeature{feature} : ts) = feature : triggerFeatures ts
triggerFeatures (_ : ts) = triggerFeatures ts

-- | Verify important feature triggers, such as fleeing the dungeon.
verifyTrigger :: MonadClientUI m
              => ActorId -> TK.Feature -> m (SlideOrCmd ())
verifyTrigger leader feat = case feat of
  TK.Cause IK.Escape{} -> do
    b <- getsState $ getActorBody leader
    side <- getsClient sside
    fact <- getsState $ (EM.! side) . sfactionD
    if not (fcanEscape $ gplayer fact) then failWith
      "This is the way out, but where would you go in this alien world?"
    else do
      go <- displayYesNo ColorFull "This is the way out. Really leave now?"
      if not go then failWith "game resumed"
      else do
        (_, total) <- getsState $ calculateTotal b
        if total == 0 then do
          -- The player can back off at each of these steps.
          go1 <- displayMore ColorBW
                   "Afraid of the challenge? Leaving so soon and empty-handed?"
          if not go1 then failWith "brave soul!"
          else do
             go2 <- displayMore ColorBW
                     "Next time try to grab some loot before escape!"
             if not go2 then failWith "here's your chance!"
             else return $ Right ()
        else return $ Right ()
  _ -> return $ Right ()

-- | Guess and report why the bump command failed.
guessTrigger :: Kind.COps -> [TK.Feature] -> Kind.Id TileKind -> Msg
guessTrigger Kind.COps{cotile} fs@(TK.Cause (IK.Ascend k) : _) t
  | Tile.hasFeature cotile (TK.Cause (IK.Ascend (-k))) t =
    if k > 0 then "the way goes down, not up"
    else if k < 0 then "the way goes up, not down"
    else assert `failure` fs
guessTrigger _ fs@(TK.Cause (IK.Ascend k) : _) _ =
    if k > 0 then "cannot ascend"
    else if k < 0 then "cannot descend"
    else assert `failure` fs
guessTrigger _ _ _ = "never mind"

-- * RunOnceAhead

runOnceAheadHuman :: MonadClientUI m => m (SlideOrCmd RequestAnyAbility)
runOnceAheadHuman = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  leader <- getLeaderUI
  srunning <- getsClient srunning
  -- When running, stop if disturbed. If not running, stop at once.
  case srunning of
    Nothing -> do
      stopPlayBack
      return $ Left mempty
    Just RunParams{runMembers}
      | noRunWithMulti fact && runMembers /= [leader] -> do
      stopPlayBack
      Config{configRunStopMsgs} <- askConfig
      if configRunStopMsgs
      then failWith "run stop: automatic leader change"
      else return $ Left mempty
    Just runParams -> do
      arena <- getArenaUI
      runOutcome <- continueRun arena runParams
      case runOutcome of
        Left stopMsg -> do
          stopPlayBack
          Config{configRunStopMsgs} <- askConfig
          if configRunStopMsgs
          then failWith $ "run stop:" <+> stopMsg
          else return $ Left mempty
        Right runCmd ->
          return $! Right runCmd

-- * MoveOnceToCursor

moveOnceToCursorHuman :: MonadClientUI m => m (SlideOrCmd RequestAnyAbility)
moveOnceToCursorHuman = goToCursor True False

goToCursor :: MonadClientUI m
           => Bool -> Bool -> m (SlideOrCmd RequestAnyAbility)
goToCursor initialStep run = do
  tgtMode <- getsClient stgtMode
  -- Movement is legal only outside targeting mode.
  if isJust tgtMode then failWith "cannot move in targeting mode"
  else do
    leader <- getLeaderUI
    b <- getsState $ getActorBody leader
    cursorPos <- cursorToPos
    case cursorPos of
      Nothing -> failWith "no leader"
      Just c | c == bpos b ->
        if initialStep
        then return $ Right $ RequestAnyAbility ReqWait
        else do
          report <- getsClient sreport
          if nullReport report
          then return $ Left mempty
          -- Mark that the messages are accumulated, not just from last move.
          else failWith "cursor now reached"
      Just c -> do
        running <- getsClient srunning
        case running of
          -- Don't use running params from previous run or goto-cursor.
          Just paramOld | not initialStep -> do
            arena <- getArenaUI
            runOutcome <- multiActorGoTo arena c paramOld
            case runOutcome of
              Left stopMsg -> failWith stopMsg
              Right (finalGoal, dir) ->
                moveRunHuman initialStep finalGoal run False dir
          _ -> do
            assert (initialStep || not run) skip
            (_, mpath) <- getCacheBfsAndPath leader c
            case mpath of
              Nothing -> failWith "no route to cursor"
              Just [] -> assert `failure` (leader, b, c)
              Just (p1 : _) -> do
                let finalGoal = p1 == c
                    dir = towards (bpos b) p1
                moveRunHuman initialStep finalGoal run False dir

multiActorGoTo :: MonadClient m
               => LevelId -> Point -> RunParams
               -> m (Either Msg (Bool, Vector))
multiActorGoTo arena c paramOld = do
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
        b <- getsState $ getActorBody r
        (_, mpath) <- getCacheBfsAndPath r c
        case mpath of
          Nothing -> return $ Left "no route to cursor"
          Just [] ->
            -- This actor already at goal; will be caught in goToCursor.
            return $ Left ""
          Just (p1 : _) -> do
            let finalGoal = p1 == c
                dir = towards (bpos b) p1
                tpos = bpos b `shift` dir
            tgts <- getsState $ posToActors tpos arena
            case tgts of
              [] -> do
                modifyClient $ \cli -> cli {srunning = Just paramNew}
                return $ Right (finalGoal, dir)
              [((target, _), _)]
                | target `elem` rs || runWaiting <= length rs ->
                -- Let r wait until all others move. Mark it in runWaiting
                -- to avoid cycles. When all wait for each other, fail.
                multiActorGoTo arena c paramNew{runWaiting=runWaiting + 1}
              _ ->
                 return $ Left "actor in the way"

-- * RunOnceToCursor

runOnceToCursorHuman :: MonadClientUI m => m (SlideOrCmd RequestAnyAbility)
runOnceToCursorHuman = goToCursor True True

-- * ContinueToCursor

continueToCursorHuman :: MonadClientUI m => m (SlideOrCmd RequestAnyAbility)
continueToCursorHuman = goToCursor False False{-irrelevant-}

-- * GameRestart; does not take time

gameRestartHuman :: MonadClientUI m => GroupName ModeKind -> m (SlideOrCmd RequestUI)
gameRestartHuman t = do
  let restart = do
        leader <- getLeaderUI
        DebugModeCli{sdifficultyCli} <- getsClient sdebugCli
        Config{configHeroNames} <- askConfig
        return $ Right
               $ ReqUIGameRestart leader t sdifficultyCli configHeroNames
  escAI <- getsClient sescAI
  if escAI == EscAIExited then restart
  else do
    let msg = "You just requested a new" <+> tshow t <+> "game."
    b1 <- displayMore ColorFull msg
    if not b1 then failWith "never mind"
    else do
      b2 <- displayYesNo ColorBW
              "Current progress will be lost! Really restart the game?"
      msg2 <- rndToAction $ oneOf
                [ "yea, would be a pity to leave them all to die"
                , "yea, a shame to get your own team stranded" ]
      if not b2 then failWith msg2
      else restart

-- * GameExit; does not take time

gameExitHuman :: MonadClientUI m => m (SlideOrCmd RequestUI)
gameExitHuman = do
  go <- displayYesNo ColorFull "Really save and exit?"
  if go then do
    leader <- getLeaderUI
    DebugModeCli{sdifficultyCli} <- getsClient sdebugCli
    return $ Right $ ReqUIGameExit leader sdifficultyCli
  else failWith "save and exit canceled"

-- * GameSave; does not take time

gameSaveHuman :: MonadClientUI m => m RequestUI
gameSaveHuman = do
  -- Announce before the saving started, since it can take some time
  -- and may slow down the machine, even if not block the client.
  -- TODO: do not save to history:
  msgAdd "Saving game backup."
  return ReqUIGameSave

-- * Tactic; does not take time

-- Note that the difference between seek-target and follow-the-leader tactic
-- can influence even a faction with passive actors. E.g., if a passive actor
-- has an extra active skill from equipment, he moves every turn.
-- TODO: set tactic for allied passive factions, too or all allied factions
-- and perhaps even factions with a leader should follow our leader
-- and his target, not their leader.
tacticHuman :: MonadClientUI m => m (SlideOrCmd RequestUI)
tacticHuman = do
  fid <- getsClient sside
  fromT <- getsState $ ftactic . gplayer . (EM.! fid) . sfactionD
  let toT = if fromT == maxBound then minBound else succ fromT
  go <- displayMore ColorFull
        $ "Switching tactic to"
          <+> tshow toT
          <> ". (This clears targets.)"
  if not go
    then failWith "tactic change canceled"
    else return $ Right $ ReqUITactic toT

-- * Automate; does not take time

automateHuman :: MonadClientUI m => m (SlideOrCmd RequestUI)
automateHuman = do
  -- BFS is not updated while automated, which would lead to corruption.
  modifyClient $ \cli -> cli {stgtMode = Nothing}
  escAI <- getsClient sescAI
  if escAI == EscAIExited then return $ Right ReqUIAutomate
  else do
    go <- displayMore ColorBW "Ceding control to AI (ESC to regain)."
    if not go
      then failWith "automation canceled"
      else return $ Right ReqUIAutomate
