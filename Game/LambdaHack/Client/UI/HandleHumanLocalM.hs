{-# LANGUAGE TupleSections #-}
-- | Semantics of 'HumanCmd' client commands that do not return
-- server commands. None of such commands takes game time.
-- TODO: document
module Game.LambdaHack.Client.UI.HandleHumanLocalM
  ( -- * Meta commands
    macroHuman
    -- * Local commands
  , clearHuman, chooseItemHuman, chooseItemDialogMode
  , chooseItemProjectHuman, chooseItemApplyHuman
  , psuitReq, triggerSymbols, permittedApplyClient
  , pickLeaderHuman, pickLeaderWithPointerHuman
  , memberCycleHuman, memberBackHuman
  , selectActorHuman, selectNoneHuman, selectWithPointerHuman
  , repeatHuman, recordHuman, historyHuman
  , markVisionHuman, markSmellHuman, markSuspectHuman, settingsMenuHuman
    -- * Commands specific to aiming
  , cancelHuman, acceptHuman, tgtClearHuman
  , moveXhairHuman, aimTgtHuman, aimFloorHuman, aimEnemyHuman
  , aimAscendHuman, epsIncrHuman
  , xhairUnknownHuman, xhairItemHuman, xhairStairHuman
  , xhairPointerFloorHuman, xhairPointerEnemyHuman
  , aimPointerFloorHuman, aimPointerEnemyHuman
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

-- Cabal
import qualified Paths_LambdaHack as Self (version)

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Ord
import qualified Data.Text as T
import Data.Version
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.BfsM
import Game.LambdaHack.Client.CommonM
import Game.LambdaHack.Client.ItemSlot
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.DrawM
import Game.LambdaHack.Client.UI.FrameM
import Game.LambdaHack.Client.UI.Frontend (frontendName)
import Game.LambdaHack.Client.UI.HandleHelperM
import Game.LambdaHack.Client.UI.HumanCmd (Trigger (..))
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import Game.LambdaHack.Client.UI.InventoryM
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Client.UI.MsgM
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.OverlayM
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Client.UI.Slideshow
import Game.LambdaHack.Client.UI.SlideshowM
import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind (isUknownSpace)
import qualified Game.LambdaHack.Content.TileKind as TK

-- * Macro

macroHuman :: MonadClientUI m => [String] -> m ()
macroHuman kms = do
  modifySession $ \sess -> sess {slastPlay = map K.mkKM kms ++ slastPlay sess}
  Config{configRunStopMsgs} <- getsSession sconfig
  when configRunStopMsgs $
    promptAdd $ "Macro activated:" <+> T.pack (intercalate " " kms)

-- * Clear

-- | Clear current messages, cycle key hints mode.
clearHuman :: MonadClientUI m => m ()
clearHuman = do
  keysHintMode <- getsSession skeysHintMode
  when (keysHintMode == KeysHintPresent) $ historyHuman
  modifySession $ \sess -> sess {skeysHintMode =
    let n = fromEnum (skeysHintMode sess) + 1
    in toEnum $ if n > fromEnum (maxBound :: KeysHintMode) then 0 else n}

-- * ChooseItem

-- | Display items from a given container store and possibly let the user
-- chose one.
chooseItemHuman :: MonadClientUI m => ItemDialogMode -> m MError
chooseItemHuman c = fst <$> chooseItemDialogMode c

chooseItemDialogMode :: MonadClientUI m
                     => ItemDialogMode -> m (MError, ItemDialogMode)
chooseItemDialogMode c = do
  let subject = partActor
      verbSha body ar = if calmEnough body ar
                        then "notice"
                        else "paw distractedly"
      prompt body ar c2 =
        let (tIn, t) = ppItemDialogMode c2
        in case c2 of
        MStore CGround ->  -- TODO: variant for actors without (unwounded) feet
          makePhrase
            [ MU.Capitalize $ MU.SubjectVerbSg (subject body) "notice"
            , MU.Text "at"
            , MU.WownW (MU.Text $ bpronoun body) $ MU.Text "feet" ]
        MStore CSha ->
          makePhrase
            [ MU.Capitalize
              $ MU.SubjectVerbSg (subject body) (verbSha body ar)
            , MU.Text tIn
            , MU.Text t ]
        MStore COrgan ->
          makePhrase
            [ MU.Capitalize $ MU.SubjectVerbSg (subject body) "feel"
            , MU.Text tIn
            , MU.WownW (MU.Text $ bpronoun body) $ MU.Text t ]
        MOwned ->
          makePhrase
            [ MU.Capitalize $ MU.SubjectVerbSg (subject body) "recall"
            , MU.Text tIn
            , MU.Text t ]
        MStats ->
          makePhrase
            [ MU.Capitalize $ MU.SubjectVerbSg (subject body) "estimate"
            , MU.WownW (MU.Text $ bpronoun body) $ MU.Text t ]
        _ ->
          makePhrase
            [ MU.Capitalize $ MU.SubjectVerbSg (subject body) "see"
            , MU.Text tIn
            , MU.WownW (MU.Text $ bpronoun body) $ MU.Text t ]
  ggi <- getStoreItem prompt c
  recordHistory  -- object chosen, wipe out already shown msgs
  case ggi of
    Right ((iid, itemFull), c2) -> do
      leader <- getLeaderUI
      b <- getsState $ getActorBody leader
      case c2 of
        MStore COrgan -> do
          let symbol = jsymbol (itemBase itemFull)
              blurb | symbol == '+' = "temporary condition"
                    | otherwise = "organ"
              -- TODO: also forbid on the server, except in special cases.
              prompt2 = makeSentence [ partActor b, "can't choose"
                                     , MU.AW blurb ]
          promptAdd prompt2
          lidV <- viewedLevelUI
          Level{lxsize, lysize} <- getLevel lidV
          localTime <- getsState $ getLocalTime (blid b)
          foundText <- itemIsFound iid leader COrgan
          let attrLine = itemDesc COrgan localTime itemFull
              ov = splitAttrLine lxsize $ attrLine <+:> toAttrLine foundText
          slides <-
            overlayToSlideshow (lysize + 1) [K.spaceKM, K.escKM] (ov, [])
          km <- getConfirms ColorFull [K.spaceKM, K.escKM] slides
          if km == K.spaceKM
          then chooseItemDialogMode c2
          else (, c2) <$> failMsg "never mind"
        MStore fromCStore -> do
          modifySession $ \sess -> sess {sitemSel = Just (fromCStore, iid)}
          return (Nothing, c2)
        MOwned -> do
          found <- getsState $ findIid leader (bfid b) iid
          let (newAid, bestStore) = case leader `lookup` found of
                Just (_, store) -> (leader, store)
                Nothing -> case found of
                  (aid, (_, store)) : _ -> (aid, store)
                  [] -> assert `failure` iid
          modifySession $ \sess -> sess {sitemSel = Just (bestStore, iid)}
          void $ pickLeader True newAid
          return (Nothing, c2)
        MStats -> assert `failure` ggi
    Left err -> (, c) <$> failMsg err

-- * ChooseItemProject

chooseItemProjectHuman :: forall m. MonadClientUI m
                       => [Trigger] -> m MError
chooseItemProjectHuman ts = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup leader actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` leader
  let calmE = calmEnough b ar
      cLegalRaw = [CGround, CInv, CEqp, CSha]
      cLegal | calmE = cLegalRaw
             | otherwise = delete CSha cLegalRaw
      (verb1, object1) = case ts of
        [] -> ("aim", "item")
        tr : _ -> (verb tr, object tr)
      psuit :: m Suitability
      psuit = do
        mpsuitReq <- psuitReq ts
        case mpsuitReq of
          -- If xhair aim invalid, no item is considered a (suitable) missile.
          Left err -> return $ SuitsNothing err
          Right psuitReqFun ->
            return $ SuitsSomething $ either (const False) snd . psuitReqFun
      prompt = makePhrase ["What", object1, "to", verb1]
      promptGeneric = "What to fling"
  ggi <- getGroupItem psuit prompt promptGeneric cLegalRaw cLegal
  case ggi of
    Right ((iid, _itemFull), MStore fromCStore) -> do
      modifySession $ \sess -> sess {sitemSel = Just (fromCStore, iid)}
      return Nothing
    Left err -> failMsg err
    _ -> assert `failure` ggi

permittedProjectClient :: MonadClientUI m
                       => [Char] -> m (ItemFull -> Either ReqFailure Bool)
permittedProjectClient triggerSyms = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorSk <- actorSkillsClient leader
  let skill = EM.findWithDefault 0 AbProject actorSk
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup leader actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` leader
  return $ permittedProject False skill b ar triggerSyms

projectCheck :: MonadClientUI m => Point -> m (Maybe ReqFailure)
projectCheck tpos = do
  Kind.COps{coTileSpeedup} <- getsState scops
  leader <- getLeaderUI
  eps <- getsClient seps
  sb <- getsState $ getActorBody leader
  let lid = blid sb
      spos = bpos sb
  Level{lxsize, lysize} <- getLevel lid
  case bla lxsize lysize eps spos tpos of
    Nothing -> return $ Just ProjectAimOnself
    Just [] -> assert `failure` "project from the edge of level"
                      `twith` (spos, tpos, sb)
    Just (pos : _) -> do
      lvl <- getLevel lid
      let t = lvl `at` pos
      if not $ Tile.isWalkable coTileSpeedup t
        then return $ Just ProjectBlockTerrain
        else do
          lab <- getsState $ posToActors pos lid
          if all (bproj . snd) lab
          then return Nothing
          else return $ Just ProjectBlockActor

posFromXhair :: MonadClientUI m => m (Either Text Point)
posFromXhair = do
  leader <- getLeaderUI
  lidV <- viewedLevelUI
  canAim <- aidTgtAims leader lidV Nothing
  case canAim of
    Right newEps -> do
      -- Modify @seps@, permanently.
      modifyClient $ \cli -> cli {seps = newEps}
      mpos <- aidTgtToPos leader lidV Nothing
      case mpos of
        Nothing -> assert `failure` (leader, lidV)
        Just pos -> do
          munit <- projectCheck pos
          case munit of
            Nothing -> return $ Right pos
            Just reqFail -> return $ Left $ showReqFailure reqFail
    Left cause -> return $ Left cause

psuitReq :: MonadClientUI m
         => [Trigger]
         -> m (Either Text (ItemFull -> Either ReqFailure (Point, Bool)))
psuitReq ts = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  mpos <- posFromXhair
  p <- permittedProjectClient $ triggerSymbols ts
  case mpos of
    Left err -> return $ Left err
    Right pos -> return $ Right $ \itemFull@ItemFull{itemBase} ->
      case p itemFull of
        Left err -> Left err
        Right False -> Right (pos, False)
        Right True -> Right (pos, totalRange itemBase >= chessDist (bpos b) pos)

triggerSymbols :: [Trigger] -> [Char]
triggerSymbols [] = []
triggerSymbols (ApplyItem{symbol} : ts) = symbol : triggerSymbols ts
triggerSymbols (_ : ts) = triggerSymbols ts

-- * ChooseItemApply

chooseItemApplyHuman :: forall m. MonadClientUI m => [Trigger] -> m MError
chooseItemApplyHuman ts = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup leader actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` leader
  let calmE = calmEnough b ar
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
  ggi <- getGroupItem psuit prompt promptGeneric cLegalRaw cLegal
  case ggi of
    Right ((iid, _itemFull), MStore fromCStore) -> do
      modifySession $ \sess -> sess {sitemSel = Just (fromCStore, iid)}
      return Nothing
    Left err -> failMsg err
    _ -> assert `failure` ggi

permittedApplyClient :: MonadClientUI m
                     => [Char] -> m (ItemFull -> Either ReqFailure Bool)
permittedApplyClient triggerSyms = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorSk <- actorSkillsClient leader
  let skill = EM.findWithDefault 0 AbApply actorSk
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup leader actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` leader
  localTime <- getsState $ getLocalTime (blid b)
  return $ permittedApply localTime skill b ar triggerSyms

-- * PickLeader

pickLeaderHuman :: MonadClientUI m => Int -> m MError
pickLeaderHuman k = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  arena <- getArenaUI
  mhero <- getsState $ tryFindHeroK side k
  allA <- getsState $ EM.assocs . sactorD
  let mactor = let factionA = filter (\(_, body) ->
                     not (bproj body) && bfid body == side) allA
                   hs = sortBy (comparing keySelected) factionA
               in case drop k hs of
                 [] -> Nothing
                 aidb : _ -> Just aidb
      mchoice = mhero `mplus` mactor
      (autoDun, autoLvl) = autoDungeonLevel fact
  case mchoice of
    Nothing -> failMsg "no such member of the party"
    Just (aid, b)
      | blid b /= arena && autoDun ->
          failMsg $ showReqFailure NoChangeDunLeader
      | autoLvl ->
          failMsg $ showReqFailure NoChangeLvlLeader
      | otherwise -> do
          void $ pickLeader True aid
          return Nothing

-- * PickLeaderWithPointer

pickLeaderWithPointerHuman :: MonadClientUI m => m MError
pickLeaderWithPointerHuman = do
  lidV <- viewedLevelUI
  Level{lysize} <- getLevel lidV
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  arena <- getArenaUI
  ours <- getsState $ filter (not . bproj . snd)
                      . actorAssocs (== side) lidV
  let viewed = sortBy (comparing keySelected) ours
      (autoDun, autoLvl) = autoDungeonLevel fact
      pick (aid, b) =
        if | blid b /= arena && autoDun ->
               failMsg $ showReqFailure NoChangeDunLeader
           | autoLvl ->
               failMsg $ showReqFailure NoChangeLvlLeader
           | otherwise -> do
               void $ pickLeader True aid
               return Nothing
  Point{..} <- getsSession spointer
  -- Pick even if no space in status line for the actor's symbol.
  if | py == lysize + 2 && px == 0 -> memberBackHuman
     | py == lysize + 2 ->
         case drop (px - 1) viewed of
           [] -> return Nothing  -- relaxed, due to subtleties of selected display
           aidb : _ -> pick aidb
     | otherwise ->
         case find (\(_, b) -> bpos b == Point px (py - mapStartY)) ours of
           Nothing -> failMsg "not pointing at an actor"
           Just aidb -> pick aidb

-- * MemberCycle

-- | Switches current member to the next on the level, if any, wrapping.
memberCycleHuman :: MonadClientUI m => m MError
memberCycleHuman = memberCycle True

-- * MemberBack

-- | Switches current member to the previous in the whole dungeon, wrapping.
memberBackHuman :: MonadClientUI m => m MError
memberBackHuman = memberBack True

-- * SelectActor

-- TODO: make the message (and for selectNoneHuman, pickLeader, etc.)
-- optional, since they have a clear representation in the UI elsewhere.
selectActorHuman :: MonadClientUI m => m ()
selectActorHuman = do
  leader <- getLeaderUI
  selectAidHuman leader

selectAidHuman :: MonadClientUI m => ActorId -> m ()
selectAidHuman leader = do
  body <- getsState $ getActorBody leader
  wasMemeber <- getsSession $ ES.member leader . sselected
  let upd = if wasMemeber
            then ES.delete leader  -- already selected, deselect instead
            else ES.insert leader
  modifySession $ \sess -> sess {sselected = upd $ sselected sess}
  let subject = partActor body
  promptAdd $ makeSentence [subject, if wasMemeber
                                     then "deselected"
                                     else "selected"]

-- * SelectNone

selectNoneHuman :: (MonadClientUI m, MonadClient m) => m ()
selectNoneHuman = do
  side <- getsClient sside
  lidV <- viewedLevelUI
  oursIds <- getsState $ actorRegularIds (== side) lidV
  let ours = ES.fromDistinctAscList oursIds
  oldSel <- getsSession sselected
  let wasNone = ES.null $ ES.intersection ours oldSel
      upd = if wasNone
            then ES.union  -- already all deselected; select all instead
            else ES.difference
  modifySession $ \sess -> sess {sselected = upd (sselected sess) ours}
  let subject = "all party members on the level"
  promptAdd $ makeSentence [subject, if wasNone
                                     then "selected"
                                     else "deselected"]

-- * SelectWithPointer

selectWithPointerHuman :: MonadClientUI m => m MError
selectWithPointerHuman = do
  lidV <- viewedLevelUI
  Level{lysize} <- getLevel lidV
  side <- getsClient sside
  ours <- getsState $ filter (not . bproj . snd)
                      . actorAssocs (== side) lidV
  let viewed = sortBy (comparing keySelected) ours
  Point{..} <- getsSession spointer
  -- Select even if no space in status line for the actor's symbol.
  if | py == lysize + 2 && px == 0 -> selectNoneHuman >> return Nothing
     | py == lysize + 2 ->
         case drop (px - 1) viewed of
           [] -> failMsg "not pointing at an actor"
           (aid, _) : _ -> selectAidHuman aid >> return Nothing
     | otherwise ->
         case find (\(_, b) -> bpos b == Point px (py - mapStartY)) ours of
           Nothing -> failMsg "not pointing at an actor"
           Just (aid, _) -> selectAidHuman aid >> return Nothing

-- * Repeat

-- Note that walk followed by repeat should not be equivalent to run,
-- because the player can really use a command that does not stop
-- at terrain change or when walking over items.
repeatHuman :: MonadClientUI m => Int -> m ()
repeatHuman n = do
  (_, seqPrevious, k) <- getsSession slastRecord
  let macro = concat $ replicate n $ reverse seqPrevious
  modifySession $ \sess -> sess {slastPlay = macro ++ slastPlay sess}
  let slastRecord = ([], [], if k == 0 then 0 else maxK)
  modifySession $ \sess -> sess {slastRecord}

maxK :: Int
maxK = 100

-- * Record

recordHuman :: MonadClientUI m => m ()
recordHuman = do
  (_seqCurrent, seqPrevious, k) <- getsSession slastRecord
  case k of
    0 -> do
      let slastRecord = ([], [], maxK)
      modifySession $ \sess -> sess {slastRecord}
      promptAdd $ "Macro will be recorded for up to"
                  <+> tshow maxK
                  <+> "actions. Stop recording with the same key."
    _ -> do
      let slastRecord = (seqPrevious, [], 0)
      modifySession $ \sess -> sess {slastRecord}
      promptAdd $ "Macro recording stopped after"
                  <+> tshow (maxK - k - 1) <+> "actions."

-- * History

historyHuman :: MonadClientUI m => m ()
historyHuman = do
  history <- getsSession shistory
  arena <- getArenaUI
  Level{lxsize, lysize} <- getLevel arena
  localTime <- getsState $ getLocalTime arena
  global <- getsState stime
  let histLines = linesHistory history
      turnsGlobal = global `timeFitUp` timeTurn
      turnsLocal = localTime `timeFitUp` timeTurn
      msg = makeSentence
        [ "You survived for"
        , MU.CarWs turnsGlobal "half-second turn"
        , "(this level:"
        , MU.Text (tshow turnsLocal) <> ")" ]
      rh = renderHistory history
      kxs = [ (Right sn, (slotPrefix sn, 0, lxsize))
            | sn <- take (length rh) intSlots ]
  promptAdd msg
  okxs <- overlayToSlideshow (lysize + 3) [K.escKM] (rh, kxs)
  let displayAllHistory = do
        menuIxHistory <- getsSession smenuIxHistory
        (ekm, pointer) <-
          displayChoiceScreen ColorFull True menuIxHistory okxs [K.escKM]
        modifySession $ \sess -> sess {smenuIxHistory = pointer}
        case ekm of
          Left km | km == K.escKM ->
            promptAdd "Try to survive a few seconds more, if you can."
          Right SlotChar{..} | slotChar == 'a' -> displayOneReport slotPrefix
          _ -> assert `failure` ekm
      displayOneReport histSlot = do
        let timeReport = case drop histSlot histLines of
              [] -> assert `failure` histSlot
              tR : _ -> tR
            (tturns, ov0) = splitReportForHistory lxsize timeReport
            prompt = toAttrLine "The full past message at time "
                     ++ tturns ++ toAttrLine "."
        promptAddAttr prompt
        slides <-
          overlayToSlideshow (lysize + 1) [K.spaceKM, K.escKM] (ov0, [])
        km <- getConfirms ColorFull [K.spaceKM, K.escKM] slides
        if km == K.spaceKM
        then displayAllHistory
        else promptAdd "Try to learn from your previous mistakes."
  displayAllHistory

-- * MarkVision

markVisionHuman :: MonadClientUI m => m ()
markVisionHuman = do
  modifySession toggleMarkVision
  cur <- getsSession smarkVision
  promptAdd $ "Visible area display toggled" <+> if cur then "on." else "off."

-- * MarkSmell

markSmellHuman :: MonadClientUI m => m ()
markSmellHuman = do
  modifySession toggleMarkSmell
  cur <- getsSession smarkSmell
  promptAdd $ "Smell display toggled" <+> if cur then "on." else "off."

-- * MarkSuspect

markSuspectHuman :: MonadClientUI m => m ()
markSuspectHuman = do
  -- @condBFS@ depends on the setting we change here.
  invalidateBfsAll
  modifyClient toggleMarkSuspect
  cur <- getsClient smarkSuspect
  promptAdd $
    "Suspect terrain display toggled" <+> if cur then "on." else "off."

-- * SettingsMenu

-- TODO: display "on"/"off" after Mark* commands
-- TODO: display tactics at the top; somehow return to this menu after Tactics
-- | Display the settings menu.
settingsMenuHuman :: MonadClientUI m
                  => (HumanCmd.HumanCmd -> m (Either MError ReqUI))
                  -> m (Either MError ReqUI)
settingsMenuHuman cmdAction = do
  Kind.COps{corule} <- getsState scops
  Binding{bcmdList} <- getsSession sbinding
  let stripFrame t = tail . init $ T.lines t
      pasteVersion :: [String] -> [String]
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
            | (km, ([HumanCmd.CmdSettingsMenu], desc, cmd)) <- bcmdList ]
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
      overwrite :: [(Int, String)] -> [(Text, Maybe KYX)]
      overwrite =  -- overwrite the art with key bindings and other lines
        let over [] (_, line) = ([], (T.pack line, Nothing))
            over bs@((mkey, binding) : bsRest) (y, line) =
              let (prefix, lineRest) = break (=='{') line
                  (braces, suffix)   = span  (=='{') lineRest
              in if length braces >= bindingLen
                 then
                   let lenB = T.length binding
                       pre = T.pack prefix
                       post = T.drop (lenB - length braces) (T.pack suffix)
                       len = T.length pre
                       yxx key = (Left [key], (y, len, len + lenB))
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
      ov = map toAttrLine menuOvLines
  menuIxSettings <- getsSession smenuIxSettings
  (ekm, pointer) <- displayChoiceScreen ColorFull True menuIxSettings
                                        (menuToSlideshow (ov, kyxs)) [K.escKM]
  modifySession $ \sess -> sess {smenuIxSettings = pointer}
  case ekm of
    Left km -> case km `lookup` kds of
      Just (_desc, cmd) -> cmdAction cmd
      Nothing -> weaveJust <$> failWith "never mind"
    Right _slot -> assert `failure` ekm

-- * Cancel

-- | End aiming mode, rejecting the current position.
cancelHuman :: MonadClientUI m => m ()
cancelHuman = do
  saimMode <- getsSession saimMode
  when (isJust saimMode) $ do
    modifySession $ \sess -> sess {saimMode = Nothing}
    promptAdd "Target not set."

-- * Accept

-- | Accept the current x-hair position as target, ending
-- aiming mode, if active.
acceptHuman :: MonadClientUI m => m ()
acceptHuman = do
  endAiming
  endAimingMsg
  modifySession $ \sess -> sess {saimMode = Nothing}

-- | End aiming mode, accepting the current position.
endAiming :: MonadClientUI m => m ()
endAiming = do
  leader <- getLeaderUI
  sxhair <- getsClient sxhair
  modifyClient $ updateTarget leader $ const $ Just sxhair

endAimingMsg :: MonadClientUI m => m ()
endAimingMsg = do
  leader <- getLeaderUI
  (targetMsg, _) <- targetDescLeader leader
  subject <- partAidLeader leader
  promptAdd $
    makeSentence [MU.SubjectVerbSg subject "target", MU.Text targetMsg]

-- * TgtClear

tgtClearHuman :: MonadClientUI m => m ()
tgtClearHuman = do
  modifySession $ \sess -> sess {sitemSel = Nothing}
  leader <- getLeaderUI
  tgt <- getsClient $ getTarget leader
  case tgt of
    Just _ -> do
      modifyClient $ updateTarget leader (const Nothing)
    Nothing -> do
      sxhairOld <- getsClient sxhair
      b <- getsState $ getActorBody leader
      let sxhair = case sxhairOld of
            TEnemy _ permit -> TEnemy leader permit
            TEnemyPos _ _ _ permit -> TEnemy leader permit
            TPoint{} -> TPoint (blid b) (bpos b)
            TVector{} -> TVector (Vector 0 0)
      modifyClient $ \cli -> cli {sxhair}
      doLook

-- | Perform look around in the current position of the xhair.
-- Does nothing outside aiming mode.
doLook :: MonadClientUI m => m ()
doLook = do
  saimMode <- getsSession saimMode
  case saimMode of
    Nothing -> return ()
    Just aimMode -> do
      leader <- getLeaderUI
      let lidV = aimLevelId aimMode
      lvl <- getLevel lidV
      xhairPos <- xhairToPos
      per <- getPerFid lidV
      b <- getsState $ getActorBody leader
      let p = fromMaybe (bpos b) xhairPos
          canSee = ES.member p (totalVisible per)
      inhabitants <- if canSee
                     then getsState $ posToActors p lidV
                     else return []
      seps <- getsClient seps
      mnewEps <- makeLine False b p seps
      itemToF <- itemToFullClient
      let aims = isJust mnewEps
          enemyMsg = case inhabitants of
            [] -> ""
            (_, body) : rest ->
                 -- Even if it's the leader, give his proper name, not 'you'.
                 let subjects = map (partActor . snd) inhabitants
                     subject = MU.WWandW subjects
                     verb = "be here"
                     desc =
                       if not (null rest)  -- many actors, only list names
                       then ""
                       else case itemDisco $ itemToF (btrunk body) (1, []) of
                         Nothing -> ""  -- no details, only show the name
                         Just ItemDisco{itemKind} -> IK.idesc itemKind
                     pdesc = if desc == "" then "" else "(" <> desc <> ")"
                 in makeSentence [MU.SubjectVerbSg subject verb] <+> pdesc
          vis | isUknownSpace $ lvl `at` p = "that is"
              | not canSee = "you remember"
              | not aims = "you are aware of"
              | otherwise = "you see"
      -- Show general info about current position.
      lookMsg <- lookAt True vis canSee p leader enemyMsg
      promptAdd lookMsg

-- * MoveXhair

-- | Move the xhair. Assumes aiming mode.
moveXhairHuman :: MonadClientUI m => Vector -> Int -> m MError
moveXhairHuman dir n = do
  leader <- getLeaderUI
  saimMode <- getsSession saimMode
  let lidV = maybe (assert `failure` leader) aimLevelId saimMode
  Level{lxsize, lysize} <- getLevel lidV
  lpos <- getsState $ bpos . getActorBody leader
  sxhair <- getsClient sxhair
  lidTgt <- lidOfTarget $ Just sxhair  -- hack to get valid pos from invalid tgt
  xhairPos <- aidTgtToPos leader lidTgt $ Just sxhair
  let cpos = fromMaybe lpos xhairPos
      shiftB pos = shiftBounded lxsize lysize pos dir
      newPos = iterate shiftB cpos !! n
  if newPos == cpos then failMsg "never mind"
  else do
    let tgt = case sxhair of
          TVector{} -> TVector $ newPos `vectorToFrom` lpos
          _ -> TPoint lidV newPos
    modifyClient $ \cli -> cli {sxhair = tgt}
    doLook
    return Nothing

lidOfTarget :: MonadClientUI m => Maybe Target -> m LevelId
lidOfTarget tgt = case tgt of
  Just (TEnemy a _) -> do
    body <- getsState $ getActorBody a
    return $! blid body
  Just (TEnemyPos _ lid _ _) -> return lid
  Just (TPoint lid _) -> return lid
  Just (TVector _) -> do
    leader <- getLeaderUI
    getsState $ blid . getActorBody leader
  Nothing -> do
    sxhair <- getsClient sxhair
    lidOfTarget $ Just sxhair

-- * AimTgt

-- | Start aiming, setting xhair to personal leader' target.
-- To be used in conjuction with other commands.
aimTgtHuman :: MonadClientUI m => m MError
aimTgtHuman = do
  -- (Re)start aiming at the current level.
  lidV <- viewedLevelUI
  modifySession $ \sess -> sess {saimMode = Just $ AimMode lidV}
  -- Set xhair to the personal target, permanently.
  leader <- getLeaderUI
  tgt <- getsClient $ getTarget leader
  modifyClient $ \cli -> cli {sxhair = fromMaybe (sxhair cli) tgt}
  doLook
  failMsg "aiming started"

-- * AimFloor

-- | Cycle aiming mode. Do not change position of the xhair,
-- switch among things at that position.
aimFloorHuman :: MonadClientUI m => m ()
aimFloorHuman = do
  lidV <- viewedLevelUI
  leader <- getLeaderUI
  lpos <- getsState $ bpos . getActorBody leader
  xhairPos <- xhairToPos
  sxhair <- getsClient sxhair
  saimMode <- getsSession saimMode
  bsAll <- getsState $ actorAssocs (const True) lidV
  let xhair = fromMaybe lpos xhairPos
      tgt = case sxhair of
        _ | isNothing saimMode ->  -- first key press: keep target
          sxhair
        TEnemy a True -> TEnemy a False
        TEnemy{} -> TPoint lidV xhair
        TEnemyPos{} -> TPoint lidV xhair
        TPoint{} -> TVector $ xhair `vectorToFrom` lpos
        TVector{} ->
          -- For projectiles, we pick here the first that would be picked
          -- by '*', so that all other projectiles on the tile come next,
          -- without any intervening actors from other tiles.
          case find (\(_, m) -> Just (bpos m) == xhairPos) bsAll of
            Just (im, _) -> TEnemy im True
            Nothing -> TPoint lidV xhair
  modifySession $ \sess -> sess {saimMode = Just $ AimMode lidV}
  modifyClient $ \cli -> cli {sxhair = tgt}
  doLook

-- * AimEnemy

aimEnemyHuman :: MonadClientUI m => m ()
aimEnemyHuman = do
  lidV <- viewedLevelUI
  leader <- getLeaderUI
  lpos <- getsState $ bpos . getActorBody leader
  xhairPos <- xhairToPos
  sxhair <- getsClient sxhair
  saimMode <- getsSession saimMode
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  bsAll <- getsState $ actorAssocs (const True) lidV
  let ordPos (_, b) = (chessDist lpos $ bpos b, bpos b)
      dbs = sortBy (comparing ordPos) bsAll
      pickUnderXhair =  -- switch to the actor under xhair, if any
        let i = fromMaybe (-1)
                $ findIndex ((== xhairPos) . Just . bpos . snd) dbs
        in splitAt i dbs
      (permitAnyActor, (lt, gt)) = case sxhair of
        TEnemy a permit | isJust saimMode ->  -- pick next enemy
          let i = fromMaybe (-1) $ findIndex ((== a) . fst) dbs
          in (permit, splitAt (i + 1) dbs)
        TEnemy a permit ->  -- first key press, retarget old enemy
          let i = fromMaybe (-1) $ findIndex ((== a) . fst) dbs
          in (permit, splitAt i dbs)
        TEnemyPos _ _ _ permit -> (permit, pickUnderXhair)
        _ -> (False, pickUnderXhair)  -- the sensible default is only-foes
      gtlt = gt ++ lt
      isEnemy b = isAtWar fact (bfid b)
                  && not (bproj b)
                  && bhp b > 0
      lf = filter (isEnemy . snd) gtlt
      tgt | permitAnyActor = case gtlt of
        (a, _) : _ -> TEnemy a True
        [] -> sxhair  -- no actors in sight, stick to last target
          | otherwise = case lf of
        (a, _) : _ -> TEnemy a False
        [] -> sxhair  -- no seen foes in sight, stick to last target
  -- Register the chosen enemy, to pick another on next invocation.
  modifySession $ \sess -> sess {saimMode = Just $ AimMode lidV}
  modifyClient $ \cli -> cli {sxhair = tgt}
  doLook

-- * AimAscend

-- | Change the displayed level in aiming mode to (at most)
-- k levels shallower. Enters aiming mode, if not already in one.
aimAscendHuman :: MonadClientUI m => Int -> m MError
aimAscendHuman k = do
  Kind.COps{cotile=cotile@Kind.Ops{okind}} <- getsState scops
  dungeon <- getsState sdungeon
  sxhairOld <- getsClient sxhair
  xhairPos <- xhairToPos
  lidV <- viewedLevelUI
  lvl <- getLevel lidV
  let rightStairs = case xhairPos of
        Nothing -> Nothing
        Just cpos ->
          let tile = lvl `at` cpos
          in if Tile.hasFeature cotile (TK.Cause $ IK.Ascend k) tile
             then Just cpos
             else Nothing
  case rightStairs of
    Just cpos -> do  -- stairs, in the right direction
      (nln, npos) <- getsState $ whereTo lidV cpos k . sdungeon
      let !_A = assert (nln /= lidV `blame` "stairs looped" `twith` nln) ()
      nlvl <- getLevel nln
      -- Do not freely reveal the other end of the stairs.
      let ascDesc (TK.Cause (IK.Ascend _)) = True
          ascDesc _ = False
          sxhair =
            if any ascDesc $ TK.tfeature $ okind (nlvl `at` npos)
            then TPoint nln npos  -- already known as an exit, focus on it
            else sxhairOld  -- unknown, do not reveal
      modifyClient $ \cli -> cli {sxhair}
      modifySession $ \sess -> sess {saimMode = Just (AimMode nln)}
      doLook
      return Nothing
    Nothing ->  -- no stairs in the right direction
      case ascendInBranch dungeon k lidV of
        [] -> failMsg "no more levels in this direction"
        nln : _ -> do
          modifySession $ \sess -> sess {saimMode = Just (AimMode nln)}
          doLook
          return Nothing

-- * EpsIncr

-- | Tweak the @eps@ parameter of the aiming digital line.
epsIncrHuman :: MonadClientUI m => Bool -> m ()
epsIncrHuman b = do
  saimMode <- getsSession saimMode
  lidV <- viewedLevelUI
  modifySession $ \sess -> sess {saimMode = Just $ AimMode lidV}
  modifyClient $ \cli -> cli {seps = seps cli + if b then 1 else -1}
  invalidateBfsAll -- actually only paths, but that's cheap enough
  flashAiming
  modifySession $ \sess -> sess {saimMode}

--- Flash the aiming line and path.
flashAiming :: MonadClientUI m => m ()
flashAiming = do
  lidV <- viewedLevelUI
  animate lidV pushAndDelay

-- * XhairUnknown

xhairUnknownHuman :: MonadClientUI m => m MError
xhairUnknownHuman = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  mpos <- closestUnknown leader
  case mpos of
    Nothing -> failMsg "no more unknown spots left"
    Just p -> do
      let tgt = TPoint (blid b) p
      modifyClient $ \cli -> cli {sxhair = tgt}
      doLook
      return Nothing

-- * XhairItem

xhairItemHuman :: MonadClientUI m => m MError
xhairItemHuman = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  items <- closestItems leader
  case items of
    [] -> failMsg "no more items remembered or visible"
    (_, (p, _)) : _ -> do
      let tgt = TPoint (blid b) p
      modifyClient $ \cli -> cli {sxhair = tgt}
      doLook
      return Nothing

-- * XhairStair

xhairStairHuman :: MonadClientUI m => Bool -> m MError
xhairStairHuman up = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  stairs <- closestTriggers (Just up) leader
  case sortBy (flip compare) $ runFrequency stairs of
    [] -> failMsg $ "no stairs" <+> if up then "up" else "down"
    (_, p) : _ -> do
      let tgt = TPoint (blid b) p
      modifyClient $ \cli -> cli {sxhair = tgt}
      doLook
      return Nothing

-- * XhairPointerFloor

xhairPointerFloorHuman :: MonadClientUI m => m ()
xhairPointerFloorHuman = do
  saimMode <- getsSession saimMode
  xhairPointerFloor False
  modifySession $ \sess -> sess {saimMode}

xhairPointerFloor :: MonadClientUI m => Bool -> m ()
xhairPointerFloor verbose = do
  lidV <- viewedLevelUI
  Level{lxsize, lysize} <- getLevel lidV
  Point{..} <- getsSession spointer
  if px >= 0 && py - mapStartY >= 0
     && px < lxsize && py - mapStartY < lysize
  then do
    oldXhair <- getsClient sxhair
    let sxhair = TPoint lidV $ Point px (py - mapStartY)
        sxhairMoused = sxhair /= oldXhair
    modifySession $ \sess ->
      sess { saimMode = Just $ AimMode lidV
           , sxhairMoused }
    modifyClient $ \cli -> cli {sxhair}
    if verbose then doLook else flashAiming
  else stopPlayBack

-- * XhairPointerEnemy

xhairPointerEnemyHuman :: MonadClientUI m => m ()
xhairPointerEnemyHuman = do
  saimMode <- getsSession saimMode
  xhairPointerEnemy False
  modifySession $ \sess -> sess {saimMode}

xhairPointerEnemy :: MonadClientUI m => Bool -> m ()
xhairPointerEnemy verbose = do
  lidV <- viewedLevelUI
  Level{lxsize, lysize} <- getLevel lidV
  Point{..} <- getsSession spointer
  if px >= 0 && py - mapStartY >= 0
     && px < lxsize && py - mapStartY < lysize
  then do
    bsAll <- getsState $ actorAssocs (const True) lidV
    oldXhair <- getsClient sxhair
    let newPos = Point px (py - mapStartY)
        sxhair =
          case find (\(_, m) -> bpos m == newPos) bsAll of
            Just (im, _) -> TEnemy im True
            Nothing -> TPoint lidV newPos
        sxhairMoused = sxhair /= oldXhair
    modifySession $ \sess ->
      sess { saimMode = Just $ AimMode lidV
           , sxhairMoused }
    modifyClient $ \cli -> cli {sxhair}
    if verbose then doLook else flashAiming
  else stopPlayBack

-- * AimPointerFloor

aimPointerFloorHuman :: MonadClientUI m => m ()
aimPointerFloorHuman = xhairPointerFloor True

-- * AimPointerEnemy

aimPointerEnemyHuman :: MonadClientUI m => m ()
aimPointerEnemyHuman = xhairPointerEnemy True
