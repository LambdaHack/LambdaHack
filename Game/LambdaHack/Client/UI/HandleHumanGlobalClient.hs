{-# LANGUAGE DataKinds, GADTs #-}
-- | Semantics of 'Command.Cmd' client commands that return server commands.
-- A couple of them do not take time, the rest does.
-- Here prompts and menus and displayed, but any feedback resulting
-- from the commands (e.g., from inventory manipulation) is generated later on,
-- for all clients that witness the results of the commands.
-- TODO: document
module Game.LambdaHack.Client.UI.HandleHumanGlobalClient
  ( -- * Commands that usually take time
    byAreaHuman, byModeHuman, sequenceHuman, moveRunHuman, waitHuman
  , moveItemHuman, projectHuman, applyHuman, alterDirHuman, triggerTileHuman
  , runOnceAheadHuman, moveOnceToCursorHuman
  , runOnceToCursorHuman, continueToCursorHuman
    -- * Commands that never take time
  , cancelHuman, acceptHuman, mainMenuHuman, settingsMenuHuman, helpHuman
  , gameRestartHuman, gameExitHuman, gameSaveHuman
  , tacticHuman, automateHuman
  ) where

import Prelude ()
import Prelude.Compat

-- Cabal
import qualified Paths_LambdaHack as Self (version)

import Control.Exception.Assert.Sugar
import Control.Monad (filterM, liftM2, when)
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List (delete, mapAccumL)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.BfsClient
import Game.LambdaHack.Client.CommonClient
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.Frontend (frontendName)
import Game.LambdaHack.Client.UI.HandleHelperClient
import Game.LambdaHack.Client.UI.HumanCmd (CmdArea (..), Trigger (..))
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import Game.LambdaHack.Client.UI.InventoryClient
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Client.UI.MsgClient
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.RunClient
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Client.UI.WidgetClient
import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
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

-- * Pick command by area

-- | Pick command depending on area the mouse pointer is in.
-- The first matching area is chosen. If none match, only interrupt.
byAreaHuman :: MonadClientUI m
            => (HumanCmd.HumanCmd -> m (SlideOrCmd RequestUI))
            -> [(HumanCmd.CmdArea, HumanCmd.HumanCmd)]
            -> m (SlideOrCmd RequestUI)
byAreaHuman cmdAction l = do
  pointer <- getsSession spointer
  let pointerInArea a = do
        rs <- areaToRectangles a
        return $! any (inside pointer) rs
  cmds <- filterM (pointerInArea . fst) l
  case cmds of
    [] -> do
      stopPlayBack
      return $ Left mempty
    (_, cmd) : _ ->
      cmdAction cmd

areaToRectangles :: MonadClientUI m => HumanCmd.CmdArea -> m [(X, Y, X, Y)]
areaToRectangles ca = case ca of
  CaMessage -> return [(0, 0, fst normalLevelBound, 0)]
  CaMapLeader -> do  -- takes preference over @CaMapParty@ and @CaMap@
    leader <- getLeaderUI
    b <- getsState $ getActorBody leader
    let Point{..} = bpos b
    return [(px, mapStartY + py, px, mapStartY + py)]
  CaMapParty -> do  -- takes preference over @CaMap@
    lidV <- viewedLevel
    side <- getsClient sside
    ours <- getsState $ filter (not . bproj . snd)
                        . actorAssocs (== side) lidV
    let rectFromB Point{..} = (px, mapStartY + py, px, mapStartY + py)
    return $! map (rectFromB . bpos . snd) ours
  CaMap -> return
    [( 0, mapStartY, fst normalLevelBound, mapStartY + snd normalLevelBound )]
  CaArenaName -> let y = snd normalLevelBound + 2
                     x = fst normalLevelBound `div` 2
                 in return [(0, y, x, y)]
  CaXhairDesc -> let y = snd normalLevelBound + 2
                     x = fst normalLevelBound `div` 2 + 2
                 in return [(x, y, fst normalLevelBound, y)]
  CaSelected -> let y = snd normalLevelBound + 3
                    x = fst normalLevelBound `div` 2
                in return [(0, y, x - 20, y)]  -- TODO
  CaLeaderStatus -> let y = snd normalLevelBound + 3
                        x = fst normalLevelBound `div` 2
                    in return [(x - 19, y, x, y)]
                      -- TODO: calculate and share with ClientDraw
  CaTargetDesc -> let y = snd normalLevelBound + 3
                      x = fst normalLevelBound `div` 2 + 2
                  in return [(x, y, fst normalLevelBound, y)]
  CaRectangle r -> return [r]
  CaUnion ca1 ca2 -> liftM2 (++) (areaToRectangles ca1) (areaToRectangles ca2)

-- * Pick command by mode

byModeHuman :: MonadClientUI m
            => m (SlideOrCmd RequestUI) -> m (SlideOrCmd RequestUI)
            -> m (SlideOrCmd RequestUI)
byModeHuman cmdNormalM cmdAimingM = do
  tgtMode <- getsSession stgtMode
  if isJust tgtMode then cmdAimingM else cmdNormalM

-- * Execute commands until any returns a server command.

sequenceHuman :: MonadClientUI m
             => (HumanCmd.HumanCmd -> m (SlideOrCmd RequestUI))
             -> [HumanCmd.HumanCmd]
             -> m (SlideOrCmd RequestUI)
sequenceHuman cmdAction l =
  let seqCmd acc [] = return $ Left acc
      seqCmd acc (c : rest) = do
        slideOrCmd <- cmdAction c
        case slideOrCmd of
          Left slides -> seqCmd (if slides == mempty then acc else slides) rest
          Right{} -> return slideOrCmd
  in seqCmd mempty l

-- * Move and Run

moveRunHuman :: MonadClientUI m
             => Bool -> Bool -> Bool -> Bool -> Vector
             -> m (SlideOrCmd RequestAnyAbility)
moveRunHuman initialStep finalGoal run runAhead dir = do
  tgtMode <- getsSession stgtMode
  if isJust tgtMode then
    Left <$> moveCursorHuman dir (if run then 10 else 1)
  else do
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
      [(target, _)] | run && initialStep ->
        -- No @stopPlayBack@: initial displace is benign enough.
        -- Displacing requires accessibility, but it's checked later on.
        fmap RequestAnyAbility <$> displaceAid target
      _ : _ : _ | run && initialStep -> do
        let !_A = assert (all (bproj . snd) tgts) ()
        failSer DisplaceProjectiles
      (target, tb) : _ | initialStep && finalGoal -> do
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
            let !_A = assert (success `blame` "bump self"
                                      `twith` (leader, target, tb)) ()
            return $ Left mempty
        else
          -- Attacking does not require full access, adjacency is enough.
          fmap RequestAnyAbility <$> meleeAid target
      _ : _ -> failWith "actor in the way"

-- | Actor atttacks an enemy actor or his own projectile.
meleeAid :: MonadClientUI m
         => ActorId -> m (SlideOrCmd (RequestTimed 'AbMelee))
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
            => ActorId -> m (SlideOrCmd (RequestTimed 'AbDisplace))
displaceAid target = do
  cops <- getsState scops
  leader <- getLeaderUI
  sb <- getsState $ getActorBody leader
  tb <- getsState $ getActorBody target
  tfact <- getsState $ (EM.! bfid tb) . sfactionD
  activeItems <- activeItemsClient target
  disp <- getsState $ dispEnemy leader target activeItems
  let actorMaxSk = sumSkills activeItems
      immobile = EM.findWithDefault 0 AbMove actorMaxSk <= 0
      spos = bpos sb
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
       if accessible cops lvl spos tpos then do
         tgts <- getsState $ posToActors tpos lid
         case tgts of
           [] -> assert `failure` (leader, sb, target, tb)
           [_] -> return $ Right $ ReqDisplace target
           _ -> failSer DisplaceProjectiles
       else failSer DisplaceAccess

-- | Actor moves or searches or alters. No visible actor at the position.
moveSearchAlterAid :: MonadClient m
                   => ActorId -> Vector -> m (Either Msg RequestAnyAbility)
moveSearchAlterAid source dir = do
  cops@Kind.COps{cotile} <- getsState scops
  sb <- getsState $ getActorBody source
  actorSk <- actorSkillsClient source
  lvl <- getLevel $ blid sb
  let skill = EM.findWithDefault 0 AbAlter actorSk
      spos = bpos sb           -- source position
      tpos = spos `shift` dir  -- target position
      t = lvl `at` tpos
      runStopOrCmd
        -- Movement requires full access.
        | accessible cops lvl spos tpos =
            -- A potential invisible actor is hit. War started without asking.
            Right $ RequestAnyAbility $ ReqMove dir
        -- No access, so search and/or alter the tile. Non-walkability is
        -- not implied by the lack of access.
        | not (Tile.isWalkable cotile t)
          && (not (knownLsecret lvl)
              || (isSecretPos lvl tpos  -- possible secrets here
                  && (Tile.isSuspect cotile t  -- not yet searched
                      || Tile.hideAs cotile t /= t))  -- search again
              || Tile.isOpenable cotile t
              || Tile.isClosable cotile t
              || Tile.isChangeable cotile t)
          = if | skill < 1 ->
                 Left $ showReqFailure AlterUnskilled
               | EM.member tpos $ lfloor lvl ->
                 Left $ showReqFailure AlterBlockItem
               | otherwise ->
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
        | otherwise = Left "never mind"
  return $! runStopOrCmd

-- * Wait

-- | Leader waits a turn (and blocks, etc.).
waitHuman :: MonadClientUI m => m (RequestTimed 'AbWait)
waitHuman = do
  modifySession $ \sess -> sess {swaitTimes = abs (swaitTimes sess) + 1}
  return ReqWait

-- * MoveItem

moveItemHuman :: forall m. MonadClientUI m
              => [CStore] -> CStore -> Maybe MU.Part -> Bool
              -> m (SlideOrCmd (RequestTimed 'AbMoveItem))
moveItemHuman cLegalRaw destCStore mverb auto = do
  itemSel <- getsSession sitemSel
  case itemSel of
    Just (fromCStore, iid) | cLegalRaw /= [CGround]  -- not normal pickup
                             && fromCStore /= destCStore -> do  -- not vacuous
      modifySession $ \sess -> sess {sitemSel = Nothing}
      leader <- getLeaderUI
      bag <- getsState $ getActorBag leader fromCStore
      case iid `EM.lookup` bag of
        Nothing -> moveItemHuman cLegalRaw destCStore mverb auto  -- used up
        Just (k, it) -> do
          itemToF <- itemToFullClient
          b <- getsState $ getActorBody leader
          let eqpFree = eqpFreeN b
              kToPick | destCStore == CEqp = min eqpFree k
                      | otherwise = k
          socK <- pickNumber True kToPick
          case socK of
            Left slides -> return $ Left slides
            Right kChosen ->
              let is = ( fromCStore
                       , [(iid, itemToF iid (kChosen, take kChosen it))] )
              in moveItems cLegalRaw is destCStore
    _ -> do
      mis <- selectItemsToMove cLegalRaw destCStore mverb auto
      case mis of
        Left slides -> return $ Left slides
        Right is -> moveItems cLegalRaw is destCStore

selectItemsToMove :: forall m. MonadClientUI m
                  => [CStore] -> CStore -> Maybe MU.Part -> Bool
                  -> m (SlideOrCmd (CStore, [(ItemId, ItemFull)]))
selectItemsToMove cLegalRaw destCStore mverb auto = do
  let !_A = assert (destCStore `notElem` cLegalRaw) ()
  let verb = fromMaybe (MU.Text $ verbCStore destCStore) mverb
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  activeItems <- activeItemsClient leader
  -- This calmE is outdated when one of the items increases max Calm
  -- (e.g., in pickup, which handles many items at once), but this is OK,
  -- the server accepts item movement based on calm at the start, not end
  -- or in the middle.
  -- The calmE is inaccurate also if an item not IDed, but that's intended
  -- and the server will ignore and warn (and content may avoid that,
  -- e.g., making all rings identified)
  let calmE = calmEnough b activeItems
      cLegal | calmE = cLegalRaw
             | destCStore == CSha = []
             | otherwise = delete CSha cLegalRaw
      prompt = makePhrase ["What to", verb]
      promptEqp = makePhrase ["What consumable to", verb]
      p :: CStore -> (Text, m Suitability)
      p cstore = if cstore `elem` [CEqp, CSha] && cLegalRaw /= [CGround]
                 then (promptEqp, return $ SuitsSomething goesIntoEqp)
                 else (prompt, return SuitsEverything)
      (promptGeneric, psuit) = p destCStore
  ggi <-
    if auto
    then getAnyItems psuit prompt promptGeneric cLegalRaw cLegal False False
    else getAnyItems psuit prompt promptGeneric cLegalRaw cLegal True True
  case ggi of
    Right (l, MStore fromCStore) -> return $ Right (fromCStore, l)
    Left slides -> return $ Left slides
    _ -> assert `failure` ggi

moveItems :: forall m. MonadClientUI m
          => [CStore] -> (CStore, [(ItemId, ItemFull)]) -> CStore
          -> m (SlideOrCmd (RequestTimed 'AbMoveItem))
moveItems cLegalRaw (fromCStore, l) destCStore = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  activeItems <- activeItemsClient leader
  let calmE = calmEnough b activeItems
      ret4 :: MonadClientUI m
           => [(ItemId, ItemFull)]
           -> Int -> [(ItemId, Int, CStore, CStore)]
           -> m (Either Slideshow [(ItemId, Int, CStore, CStore)])
      ret4 [] _ acc = return $ Right $ reverse acc
      ret4 ((iid, itemFull) : rest) oldN acc = do
        let k = itemK itemFull
            !_A = assert (k > 0) ()
            retRec toCStore =
              let n = oldN + if toCStore == CEqp then k else 0
              in ret4 rest n ((iid, k, fromCStore, toCStore) : acc)
        if cLegalRaw == [CGround]  -- normal pickup
        then case destCStore of
          CEqp | calmE && goesIntoSha itemFull ->
            retRec CSha
          CEqp | not $ goesIntoEqp itemFull ->
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
      Left sli -> Left sli
      Right [] -> assert `failure` l
      Right lr -> Right $ ReqMoveItems lr

-- * Project

projectHuman :: MonadClientUI m
             => [Trigger] -> m (SlideOrCmd (RequestTimed 'AbProject))
projectHuman ts = projectHumanState ts INoSuitable

-- * Apply

applyHuman :: MonadClientUI m
           => [Trigger] -> m (SlideOrCmd (RequestTimed 'AbApply))
applyHuman ts = do
  -- TODO: factor all the code below out
  itemSel <- getsSession sitemSel
  case itemSel of
    Just (fromCStore, iid) -> do
      leader <- getLeaderUI
      bag <- getsState $ getActorBag leader fromCStore
      case iid `EM.lookup` bag of
        Nothing -> do  -- used up
          modifySession $ \sess -> sess {sitemSel = Nothing}
          applyHuman ts
        Just kit -> do
          itemToF <- itemToFullClient
          let i = (fromCStore, (iid, itemToF iid kit))
          applyItem ts i
    Nothing -> do
      mis <- selectItemToApply ts
      case mis of
        Left slides -> return $ Left slides
        Right i -> applyItem ts i

permittedApplyClient :: MonadClientUI m
                     => [Char] -> m (ItemFull -> Either ReqFailure Bool)
permittedApplyClient triggerSyms = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorSk <- actorSkillsClient leader
  let skill = EM.findWithDefault 0 AbApply actorSk
  activeItems <- activeItemsClient leader
  localTime <- getsState $ getLocalTime (blid b)
  return $ permittedApply localTime skill b activeItems triggerSyms

selectItemToApply :: forall m. MonadClientUI m
                  => [Trigger] -> m (SlideOrCmd (CStore, (ItemId, ItemFull)))
selectItemToApply ts = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  activeItems <- activeItemsClient leader
  let calmE = calmEnough b activeItems
      cLegalRaw = [CGround, CInv, CEqp, CSha]
      cLegal | calmE = cLegalRaw
             | otherwise = delete CSha cLegalRaw
      (verb1, object1) = case ts of
        [] -> ("apply", "item")
        tr : _ -> (verb tr, object tr)
      prompt = makePhrase ["What", object1, "to", verb1]
      promptGeneric = "What to apply"
      psuit :: m Suitability
      psuit = do
        mp <- permittedApplyClient $ triggerSymbols ts
        return $ SuitsSomething $ either (const False) id . mp
  ggi <- getGroupItem
           psuit prompt promptGeneric False cLegalRaw cLegal ISuitable
  case ggi of
    Right ((iid, itemFull), MStore fromCStore) ->
      return $ Right (fromCStore, (iid, itemFull))
    Left slides -> return $ Left slides
    _ -> assert `failure` ggi

applyItem :: MonadClientUI m
          => [Trigger] -> (CStore, (ItemId, ItemFull))
          -> m (SlideOrCmd (RequestTimed 'AbApply))
applyItem ts (fromCStore, (iid, itemFull)) = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  activeItems <- activeItemsClient leader
  let calmE = calmEnough b activeItems
  if not calmE && fromCStore == CSha then failSer ItemNotCalm
  else do
    p <- permittedApplyClient $ triggerSymbols ts
    case p itemFull of
      Left reqFail -> failSer reqFail
      Right _ -> return $ Right $ ReqApply iid fromCStore

-- * AlterDir

-- TODO: accept mouse, too
-- | Ask for a direction and alter a tile, if possible.
alterDirHuman :: MonadClientUI m
              => [Trigger] -> m (SlideOrCmd (RequestTimed 'AbAlter))
alterDirHuman ts = do
  Config{configVi, configLaptop} <- askConfig
  let verb1 = case ts of
        [] -> "alter"
        tr : _ -> verb tr
      keys = K.escKM : map (K.KM K.NoModifier)
                           (K.dirAllKey configVi configLaptop)
      prompt = makePhrase ["What to", verb1 <> "? [movement key, ESC]"]
  km <- displayChoiceLine prompt mempty keys
  K.handleDir configVi configLaptop km (`alterTile` ts) (failWith "never mind")

-- | Player tries to alter a tile using a feature.
alterTile :: MonadClientUI m
          => Vector -> [Trigger] -> m (SlideOrCmd (RequestTimed 'AbAlter))
alterTile dir ts = do
  cops@Kind.COps{cotile} <- getsState scops
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorSk <- actorSkillsClient leader
  lvl <- getLevel $ blid b
  as <- getsState $ actorList (const True) (blid b)
  let skill = EM.findWithDefault 0 AbAlter actorSk
      tpos = bpos b `shift` dir
      t = lvl `at` tpos
      alterFeats = alterFeatures ts
  case filter (\feat -> Tile.hasFeature cotile feat t) alterFeats of
    _ | skill < 1 -> failSer AlterUnskilled
    [] -> failWith $ guessAlter cops alterFeats t
    feat : _ ->
      if EM.notMember tpos $ lfloor lvl then
        if unoccupied as tpos then
          return $ Right $ ReqAlter tpos $ Just feat
        else failSer AlterBlockActor
      else failSer AlterBlockItem

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
                 => [Trigger] -> m (SlideOrCmd (RequestTimed 'AbTrigger))
triggerTileHuman ts = do
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
        Right () -> return $ Right $ ReqTrigger feat
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
    if | k > 0 -> "the way goes down, not up"
       | k < 0 -> "the way goes up, not down"
       | otherwise -> assert `failure` fs
guessTrigger _ fs@(TK.Cause (IK.Ascend k) : _) _ =
    if | k > 0 -> "cannot ascend"
       | k < 0 -> "cannot descend"
       | otherwise -> assert `failure` fs
guessTrigger _ _ _ = "never mind"

-- * RunOnceAhead

runOnceAheadHuman :: MonadClientUI m => m (SlideOrCmd RequestAnyAbility)
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
      return $ Left mempty
    Just RunParams{runMembers}
      | noRunWithMulti fact && runMembers /= [leader] -> do
      stopPlayBack
      Config{configRunStopMsgs} <- askConfig
      if configRunStopMsgs
      then failWith "run stop: automatic leader change"
      else return $ Left mempty
    Just _runParams | keyPressed -> do
      discardPressedKey
      stopPlayBack
      Config{configRunStopMsgs} <- askConfig
      if configRunStopMsgs
      then failWith "run stop: key pressed"
      else failWith "interrupted"
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
          return $ Right runCmd

-- * MoveOnceToCursor

moveOnceToCursorHuman :: MonadClientUI m => m (SlideOrCmd RequestAnyAbility)
moveOnceToCursorHuman = goToCursor True False

goToCursor :: MonadClientUI m
           => Bool -> Bool -> m (SlideOrCmd RequestAnyAbility)
goToCursor initialStep run = do
  tgtMode <- getsSession stgtMode
  -- Movement is legal only outside targeting mode.
  if isJust tgtMode then failWith "cannot move in aiming mode"
  else do
    leader <- getLeaderUI
    b <- getsState $ getActorBody leader
    cursorPos <- cursorToPos
    case cursorPos of
      Nothing -> failWith "crosshair position invalid"
      Just c | c == bpos b -> do
        stopPlayBack
        if initialStep
        then return $ Right $ RequestAnyAbility ReqWait
        else return $ Left mempty
      Just c -> do
        running <- getsSession srunning
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
            let !_A = assert (initialStep || not run) ()
            (_, mpath) <- getCacheBfsAndPath leader c
            case mpath of
              Nothing -> failWith "no route to crosshair"
              Just [] -> assert `failure` (leader, b, c)
              Just (p1 : _) -> do
                let finalGoal = p1 == c
                    dir = towards (bpos b) p1
                moveRunHuman initialStep finalGoal run False dir

multiActorGoTo :: MonadClientUI m
               => LevelId -> Point -> RunParams
               -> m (Either Msg (Bool, Vector))
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
        b <- getsState $ getActorBody r
        (_, mpath) <- getCacheBfsAndPath r c
        case mpath of
          Nothing -> return $ Left "no route to crosshair"
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
                modifySession $ \sess -> sess {srunning = Just paramNew}
                return $ Right (finalGoal, dir)
              [(target, _)]
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

-- * Cancel

-- | End targeting mode, rejecting the current position.
cancelHuman :: MonadClientUI m => m (SlideOrCmd RequestUI)
cancelHuman = do
  stgtMode <- getsSession stgtMode
  let !_A = assert (isJust stgtMode) ()
  modifySession $ \sess -> sess {stgtMode = Nothing}
  failWith "target not set"

-- * Help; does not take time, but some of the commands do

-- | Display command help.
helpHuman :: MonadClientUI m
          => (HumanCmd.HumanCmd -> m (SlideOrCmd RequestUI))
          -> m (SlideOrCmd RequestUI)
helpHuman cmdAction = do
  keyb <- askBinding
  menuIxHelp <- getsSession smenuIxHelp
  (ekm, pointer) <-
    displayChoiceScreen True menuIxHelp (keyHelp keyb) [K.spaceKM]
  modifySession $ \sess -> sess {smenuIxHelp = pointer}
  case ekm of
    Left km -> case km `M.lookup` bcmdMap keyb of
      _ | km `K.elemOrNull` [K.spaceKM, K.escKM] -> return $ Left mempty
      Just (_desc, _cats, cmd) -> cmdAction cmd
      Nothing -> failWith "never mind"
    Right _slot -> assert `failure` ekm

-- * Accept

-- | Accept the current x-hair position as target, ending
-- aiming mode, if active.
acceptHuman :: MonadClientUI m => m (SlideOrCmd RequestUI)
acceptHuman = do
  endTargeting
  endTargetingMsg
  modifySession $ \sess -> sess {stgtMode = Nothing}
  return $ Left mempty

endTargetingMsg :: MonadClientUI m => m ()
endTargetingMsg = do
  leader <- getLeaderUI
  (targetMsg, _) <- targetDescLeader leader
  subject <- partAidLeader leader
  msgAdd $ makeSentence [MU.SubjectVerbSg subject "target", MU.Text targetMsg]

-- * MainMenu; does not take time, but some of the commands do

-- TODO: avoid String
-- | Display the main menu.
mainMenuHuman :: MonadClientUI m
              => (HumanCmd.HumanCmd -> m (SlideOrCmd RequestUI))
              -> m (SlideOrCmd RequestUI)
mainMenuHuman cmdAction = do
  Kind.COps{corule} <- getsState scops
  Binding{bcmdList} <- askBinding
  gameMode <- getGameMode
  scurDiff <- getsClient scurDiff
  snxtDiff <- getsClient snxtDiff
  let stripFrame t = tail . init $ T.lines t
      pasteVersion art =
        let pathsVersion = rpathsVersion $ Kind.stdRuleset corule
            version = " Version " ++ showVersion pathsVersion
                      ++ " (frontend: " ++ frontendName
                      ++ ", engine: LambdaHack " ++ showVersion Self.version
                      ++ ") "
            versionLen = length version
        in init art ++ [take (80 - versionLen) (last art) ++ version]
      -- Key-description-command tuples.
      kds = [ (km, (desc, cmd))
            | (km, (desc, [HumanCmd.CmdMainMenu], cmd)) <- bcmdList ]
      statusLen = 30
      bindingLen = 28
      gameName = makePhrase [MU.Capitalize $ MU.Text $ mname gameMode]
      gameInfo = [ T.justifyLeft statusLen ' '
                   $ "Current scenario:" <+> gameName
                 , T.justifyLeft statusLen ' '
                   $ "Current game difficulty:" <+> tshow scurDiff
                 , T.justifyLeft statusLen ' '
                   $ "Next game difficulty:" <+> tshow snxtDiff
                 , T.justifyLeft statusLen ' ' "" ]
      emptyInfo = repeat $ T.justifyLeft bindingLen ' ' ""
      bindings =  -- key bindings to display
        let fmt (k, (d, _)) =
              ( Just k
              , T.justifyLeft bindingLen ' '
                  $ T.justifyLeft 3 ' ' (K.showKM k) <> " " <> d )
        in map fmt kds
      overwrite =  -- overwrite the art with key bindings and other lines
        let over [] (_, line) = ([], (T.pack line, Nothing))
            over bs@((mkey, binding) : bsRest) (y, line) =
              let (prefix, lineRest) = break (=='{') line
                  (braces, suffix)   = span  (=='{') lineRest
              in if length braces >= bindingLen
                 then
                   let lenB = T.length binding
                       pre = T.pack prefix
                       post = T.drop (lenB - bindingLen) (T.pack suffix)
                       len = T.length pre
                       yxx key = (Left key, (y, len, len + lenB))
                       myxx = yxx <$> mkey
                   in (bsRest, (pre <> binding <> post, myxx))
                 else (bs, (T.pack line, Nothing))
        in snd . mapAccumL over (zip (repeat Nothing) gameInfo
                                 ++ bindings
                                 ++ zip (repeat Nothing) emptyInfo)
      mainMenuArt = rmainMenuArt $ Kind.stdRuleset corule
      artWithVersion = pasteVersion $ map T.unpack $ stripFrame mainMenuArt
      menuOverwritten = overwrite $ zip [0..] artWithVersion
      (menuOvLines, mkyxs) = unzip menuOverwritten
      kyxs = catMaybes mkyxs
      ov = toOverlay menuOvLines
  menuIxMain <- getsSession smenuIxMain
  (ekm, pointer) <- displayChoiceScreen True menuIxMain [(ov, kyxs)] []
  modifySession $ \sess -> sess {smenuIxMain = pointer}
  case ekm of
    Left km -> case km `lookup` kds of
      Just (_desc, cmd) -> cmdAction cmd
      Nothing -> failWith "never mind"
    Right _slot -> assert `failure` ekm

-- * SettingsMenu; does not take time

-- TODO: display "on"/"off" after Mark* commands
-- TODO: display tactics at the top; somehow return to this menu after Tactics
-- | Display the settings menu.
settingsMenuHuman :: MonadClientUI m
                  => (HumanCmd.HumanCmd -> m (SlideOrCmd RequestUI))
                  -> m (SlideOrCmd RequestUI)
settingsMenuHuman cmdAction = do
  Kind.COps{corule} <- getsState scops
  Binding{bcmdList} <- askBinding
  let stripFrame t = tail . init $ T.lines t
      pasteVersion art =  -- TODO: factor out
        let pathsVersion = rpathsVersion $ Kind.stdRuleset corule
            version = " Version " ++ showVersion pathsVersion
                      ++ " (frontend: " ++ frontendName
                      ++ ", engine: LambdaHack " ++ showVersion Self.version
                      ++ ") "
            versionLen = length version
        in init art ++ [take (80 - versionLen) (last art) ++ version]
      -- Key-description-command tuples.
      kds = [ (km, (desc, cmd))
            | (km, (desc, [HumanCmd.CmdSettingsMenu], cmd)) <- bcmdList ]
      statusLen = 30
      bindingLen = 28
      gameInfo = replicate 4 $ T.justifyLeft statusLen ' ' ""
      emptyInfo = repeat $ T.justifyLeft bindingLen ' ' ""
      bindings =  -- key bindings to display
        let fmt (k, (d, _)) =
              ( Just k
              , T.justifyLeft bindingLen ' '
                  $ T.justifyLeft 3 ' ' (K.showKM k) <> " " <> d )
        in map fmt kds
      overwrite =  -- overwrite the art with key bindings and other lines
        let over [] (_, line) = ([], (T.pack line, Nothing))
            over bs@((mkey, binding) : bsRest) (y, line) =
              let (prefix, lineRest) = break (=='{') line
                  (braces, suffix)   = span  (=='{') lineRest
              in if length braces >= bindingLen
                 then
                   let lenB = T.length binding
                       pre = T.pack prefix
                       post = T.drop (lenB - bindingLen) (T.pack suffix)
                       len = T.length pre
                       yxx key = (Left key, (y, len, len + lenB))
                       myxx = yxx <$> mkey
                   in (bsRest, (pre <> binding <> post, myxx))
                 else (bs, (T.pack line, Nothing))
        in snd . mapAccumL over (zip (repeat Nothing) gameInfo
                                 ++ bindings
                                 ++ zip (repeat Nothing) emptyInfo)
      mainMenuArt = rmainMenuArt $ Kind.stdRuleset corule
      artWithVersion = pasteVersion $ map T.unpack $ stripFrame mainMenuArt
      menuOverwritten = overwrite $ zip [0..] artWithVersion
      (menuOvLines, mkyxs) = unzip menuOverwritten
      kyxs = catMaybes mkyxs
      ov = toOverlay menuOvLines
  menuIxSettings <- getsSession smenuIxSettings
  (ekm, pointer) <- displayChoiceScreen True menuIxSettings [(ov, kyxs)] []
  modifySession $ \sess -> sess {smenuIxSettings = pointer}
  case ekm of
    Left km -> case km `lookup` kds of
      Just (_desc, cmd) -> cmdAction cmd
      Nothing -> failWith "never mind"
    Right _slot -> assert `failure` ekm

-- * GameRestart; does not take time

gameRestartHuman :: MonadClientUI m
                 => GroupName ModeKind -> m (SlideOrCmd RequestUI)
gameRestartHuman t = do
  gameMode <- getGameMode
  b2 <- displayYesNo ColorBW $
          "You just requested a new" <+> tshow t
          <+> "game. The progress of the current" <+> mname gameMode
          <+> "game will be lost! Please confirm."
  msg2 <- rndToAction $ oneOf
            [ "yea, would be a pity to leave them all to die"
            , "yea, a shame to get your team stranded" ]
  if not b2
  then failWith msg2
  else do
    leader <- getLeaderUI
    snxtDiff <- getsClient snxtDiff
    Config{configHeroNames} <- askConfig
    return $ Right
           $ ReqUIGameRestart leader t snxtDiff configHeroNames

-- * GameExit; does not take time

gameExitHuman :: MonadClientUI m => m (SlideOrCmd RequestUI)
gameExitHuman = do
  leader <- getLeaderUI
  return $ Right $ ReqUIGameExit leader

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
        $ "Current tactic is '" <> tshow fromT
          <> "'. Switching tactic to '" <> tshow toT
          <> "'. (This clears targets.)"
  if not go
    then failWith "tactic change canceled"
    else return $ Right $ ReqUITactic toT

-- * Automate; does not take time

automateHuman :: MonadClientUI m => m (SlideOrCmd RequestUI)
automateHuman = do
  -- BFS is not updated while automated, which would lead to corruption.
  modifySession $ \sess -> sess {stgtMode = Nothing}
  go <- displayMore ColorBW "Ceding control to AI (press any key to regain)."
  if not go
    then failWith "automation canceled"
    else return $ Right ReqUIAutomate
