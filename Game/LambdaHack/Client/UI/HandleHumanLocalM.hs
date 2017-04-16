-- | Semantics of 'HumanCmd' client commands that do not return
-- server commands. None of such commands takes game time.
module Game.LambdaHack.Client.UI.HandleHumanLocalM
  ( -- * Meta commands
    macroHuman
    -- * Local commands
  , clearHuman, sortSlotsHuman, chooseItemHuman, chooseItemDialogMode
  , chooseItemProjectHuman, chooseItemApplyHuman
  , psuitReq, triggerSymbols, permittedApplyClient
  , pickLeaderHuman, pickLeaderWithPointerHuman
  , memberCycleHuman, memberBackHuman
  , selectActorHuman, selectNoneHuman, selectWithPointerHuman
  , repeatHuman, recordHuman, historyHuman
  , markVisionHuman, markSmellHuman, markSuspectHuman
    -- * Commands specific to aiming
  , cancelHuman, acceptHuman, tgtClearHuman, itemClearHuman
  , moveXhairHuman, aimTgtHuman, aimFloorHuman, aimEnemyHuman, aimItemHuman
  , aimAscendHuman, epsIncrHuman
  , xhairUnknownHuman, xhairItemHuman, xhairStairHuman
  , xhairPointerFloorHuman, xhairPointerEnemyHuman
  , aimPointerFloorHuman, aimPointerEnemyHuman
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

-- Cabal

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Ord
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.BfsM
import Game.LambdaHack.Client.CommonM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.ActorUI
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.DrawM
import Game.LambdaHack.Client.UI.EffectDescription
import Game.LambdaHack.Client.UI.FrameM
import Game.LambdaHack.Client.UI.HandleHelperM
import Game.LambdaHack.Client.UI.HumanCmd (Trigger (..))
import Game.LambdaHack.Client.UI.InventoryM
import Game.LambdaHack.Client.UI.ItemSlot
import qualified Game.LambdaHack.Client.UI.Key as K
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Client.UI.MsgM
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.OverlayM
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Client.UI.SlideshowM
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
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.TileKind (isUknownSpace)

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

-- * SortSlots

sortSlotsHuman :: MonadClientUI m => m ()
sortSlotsHuman = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  sortSlots (bfid b) (Just b)
  promptAdd "Items sorted by kind and stats."

-- * ChooseItem

-- | Display items from a given container store and possibly let the user
-- chose one.
chooseItemHuman :: MonadClientUI m => ItemDialogMode -> m MError
chooseItemHuman c = either Just (const Nothing) <$> chooseItemDialogMode c

chooseItemDialogMode :: MonadClientUI m
                     => ItemDialogMode -> m (FailOrCmd ItemDialogMode)
chooseItemDialogMode c = do
  let subject = partActor
      verbSha body ar = if calmEnough body ar
                        then "notice"
                        else "paw distractedly"
      prompt body bodyUI ar c2 =
        let (tIn, t) = ppItemDialogMode c2
        in case c2 of
        MStore CGround ->
          makePhrase
            [ MU.Capitalize $ MU.SubjectVerbSg (subject bodyUI) "notice"
            , MU.Text "at"
            , MU.WownW (MU.Text $ bpronoun bodyUI) $ MU.Text "feet" ]
        MStore CSha ->
          makePhrase
            [ MU.Capitalize
              $ MU.SubjectVerbSg (subject bodyUI) (verbSha body ar)
            , MU.Text tIn
            , MU.Text t ]
        MStore COrgan ->
          makePhrase
            [ MU.Capitalize $ MU.SubjectVerbSg (subject bodyUI) "feel"
            , MU.Text tIn
            , MU.WownW (MU.Text $ bpronoun bodyUI) $ MU.Text t ]
        MOwned ->
          makePhrase
            [ MU.Capitalize $ MU.SubjectVerbSg (subject bodyUI) "recall"
            , MU.Text tIn
            , MU.Text t ]
        MStats ->
          makePhrase
            [ MU.Capitalize $ MU.SubjectVerbSg (subject bodyUI) "estimate"
            , MU.WownW (MU.Text $ bpronoun bodyUI) $ MU.Text t ]
        MLoreItem ->
          makePhrase
            [ MU.Capitalize $ MU.SubjectVerbSg (subject bodyUI) "recall"
            , MU.WownW (MU.Text $ bpronoun bodyUI) $ MU.Text t ]
        MLoreOrgan ->
          makePhrase
            [ MU.Capitalize $ MU.SubjectVerbSg (subject bodyUI) "recall"
            , MU.WownW (MU.Text $ bpronoun bodyUI) $ MU.Text t ]
        _ ->
          makePhrase
            [ MU.Capitalize $ MU.SubjectVerbSg (subject bodyUI) "see"
            , MU.Text tIn
            , MU.WownW (MU.Text $ bpronoun bodyUI) $ MU.Text t ]
  ggi <- getStoreItem prompt c
  recordHistory  -- item chosen, wipe out already shown msgs
  case ggi of
    (Right (iid, itemFull), (c2, _)) -> do
      leader <- getLeaderUI
      b <- getsState $ getActorBody leader
      bUI <- getsSession $ getActorUI leader
      let displayLore store prompt2 = do
            promptAdd prompt2
            lidV <- viewedLevelUI
            Level{lxsize, lysize} <- getLevel lidV
            localTime <- getsState $ getLocalTime (blid b)
            factionD <- getsState sfactionD
            actorAspect <- getsClient sactorAspect
            let ar = case EM.lookup leader actorAspect of
                  Just aspectRecord -> aspectRecord
                  Nothing -> assert `failure` leader
                attrLine = itemDesc (bfid b) factionD (aHurtMelee ar)
                                    store localTime itemFull
                ov = splitAttrLine lxsize attrLine
            slides <-
              overlayToSlideshow (lysize + 1) [K.spaceKM, K.escKM] (ov, [])
            km <- getConfirms ColorFull [K.spaceKM, K.escKM] slides
            if km == K.spaceKM
            then chooseItemDialogMode c2
            else failWith "never mind"
      case c2 of
        MStore COrgan -> do
          let symbol = jsymbol (itemBase itemFull)
              blurb | symbol == '+' = "temporary condition"
                    | otherwise = "organ"
              prompt2 = makeSentence [ partActor bUI, "can't choose"
                                     , MU.AW blurb ]
          displayLore COrgan prompt2
        MStore fromCStore -> do
          modifySession $ \sess -> sess {sitemSel = Just (fromCStore, iid)}
          return $ Right c2
        MOwned -> do
          found <- getsState $ findIid leader (bfid b) iid
          let (newAid, bestStore) = case leader `lookup` found of
                Just (_, store) -> (leader, store)
                Nothing -> case found of
                  (aid, (_, store)) : _ -> (aid, store)
                  [] -> assert `failure` iid
          modifySession $ \sess -> sess {sitemSel = Just (bestStore, iid)}
          arena <- getArenaUI
          b2 <- getsState $ getActorBody newAid
          fact <- getsState $ (EM.! bfid b2) . sfactionD
          let (autoDun, _) = autoDungeonLevel fact
          if | blid b2 /= arena && autoDun ->
               failSer NoChangeDunLeader
             | otherwise -> do
               void $ pickLeader True newAid
               return $ Right c2
        MStats -> assert `failure` ggi
        MLoreItem -> displayLore CGround
          (makeSentence [ MU.SubjectVerbSg (partActor bUI) "remember"
                        , "item lore" ])
        MLoreOrgan -> displayLore COrgan
          (makeSentence [ MU.SubjectVerbSg (partActor bUI) "remember"
                        , "organ lore" ])
    (Left _, (MStats, ekm)) -> case ekm of
      Right slot -> do
        let eqpSlot = statSlots !! fromJust (elemIndex slot allZeroSlots)
        leader <- getLeaderUI
        b <- getsState $ getActorBody leader
        bUI <- getsSession $ getActorUI leader
        actorAspect <- getsClient sactorAspect
        let ar = case EM.lookup leader actorAspect of
              Just aspectRecord -> aspectRecord
              Nothing -> assert `failure` leader
            valueText = slotToDecorator eqpSlot b $ prEqpSlot eqpSlot ar
            prompt2 = makeSentence
              [ MU.WownW (partActor bUI) (MU.Text $ slotToName eqpSlot)
              , "is", MU.Text valueText ]
              <+> slotToDesc eqpSlot
        go <- displaySpaceEsc ColorFull prompt2
        if go
        then chooseItemDialogMode MStats
        else failWith "never mind"
      Left _ -> failWith "never mind"
    (Left err, _) -> failWith err

-- * ChooseItemProject

chooseItemProjectHuman :: forall m. MonadClientUI m => [Trigger] -> m MError
chooseItemProjectHuman ts = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup leader actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` leader
  let calmE = calmEnough b ar
      cLegalRaw = [CGround, CInv, CSha, CEqp]
      cLegal | calmE = cLegalRaw
             | otherwise = delete CSha cLegalRaw
      (verb1, object1) = case ts of
        [] -> ("aim", "item")
        tr : _ -> (verb tr, object tr)
  mpsuitReq <- psuitReq ts
  case mpsuitReq of
    -- If xhair aim invalid, no item is considered a (suitable) missile.
    Left err -> failMsg err
    Right psuitReqFun -> do
      let psuit =
            return $ SuitsSomething $ either (const False) snd . psuitReqFun
          prompt = makePhrase ["What", object1, "to", verb1]
          promptGeneric = "What to fling"
      ggi <- getGroupItem psuit prompt promptGeneric cLegalRaw cLegal
      case ggi of
        Right ((iid, _itemFull), (MStore fromCStore, _)) -> do
          modifySession $ \sess -> sess {sitemSel = Just (fromCStore, iid)}
          return Nothing
        Left err -> failMsg err
        _ -> assert `failure` ggi

permittedProjectClient :: MonadClientUI m
                       => [Char] -> m (ItemFull -> Either ReqFailure Bool)
permittedProjectClient triggerSyms = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorSk <- leaderSkillsClientUI
  let skill = EM.findWithDefault 0 AbProject actorSk
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup leader actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` leader
      calmE = calmEnough b ar
  return $ permittedProject False skill calmE triggerSyms

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
          lab <- getsState $ posToAssocs pos lid
          if all (bproj . snd) lab
          then return Nothing
          else return $ Just ProjectBlockActor

-- | Check whether one is permitted to aim (for projecting) at a target
-- (this is only checked for actor targets so that the player doesn't miss
-- enemy getting out of sight; but for positions we let player
-- shoot at obstacles, e.g., to destroy them, and shoot at a lying item
-- and then at its posision, after enemy picked up the item).
-- Returns a different @seps@ if needed to reach the target actor.
--
-- Note: Perception is not enough for the check,
-- because the target actor can be obscured by a glass wall
-- or be out of sight range, but in weapon range.
xhairLegalEps :: MonadClientUI m => m (Either Text Int)
xhairLegalEps = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  lidV <- viewedLevelUI
  let !_A = assert (lidV == blid b) ()
      findNewEps onlyFirst pos = do
        oldEps <- getsClient seps
        mnewEps <- makeLine onlyFirst b pos oldEps
        case mnewEps of
          Just newEps -> return $ Right newEps
          Nothing ->
            return $ Left
                   $ if onlyFirst
                     then "aiming blocked at the first step"
                     else "aiming line to the opponent blocked somewhere"
  xhair <- getsSession sxhair
  case xhair of
    TEnemy a _ -> do
      body <- getsState $ getActorBody a
      let pos = bpos body
      if blid body == lidV
      then findNewEps False pos
      else assert `failure` (xhair, body, lidV)
    TPoint TEnemyPos{} _ _ ->
      return $ Left "selected opponent not visible"
    TPoint _ lid pos ->
      if lid == lidV
      then findNewEps True pos
      else assert `failure` (xhair, lidV)
    TVector v -> do
      Level{lxsize, lysize} <- getLevel lidV
      let shifted = shiftBounded lxsize lysize (bpos b) v
      if shifted == bpos b && v /= Vector 0 0
      then return $ Left "selected translation is void"
      else findNewEps True shifted

posFromXhair :: MonadClientUI m => m (Either Text Point)
posFromXhair = do
  canAim <- xhairLegalEps
  case canAim of
    Right newEps -> do
      -- Modify @seps@, permanently.
      modifyClient $ \cli -> cli {seps = newEps}
      sxhair <- getsSession sxhair
      mpos <- xhairToPos
      case mpos of
        Nothing -> assert `failure` sxhair
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
  lidV <- viewedLevelUI
  if lidV /= blid b
  then return $ Left "can't project on remote levels"
  else do
    mpos <- posFromXhair
    p <- permittedProjectClient $ triggerSymbols ts
    case mpos of
      Left err -> return $ Left err
      Right pos -> return $ Right $ \itemFull@ItemFull{itemBase} ->
        case p itemFull of
          Left err -> Left err
          Right False -> Right (pos, False)
          Right True ->
            Right (pos, totalRange itemBase >= chessDist (bpos b) pos)

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
      cLegalRaw = [CGround, CInv, CSha, CEqp]
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
    Right ((iid, _itemFull), (MStore fromCStore, _)) -> do
      modifySession $ \sess -> sess {sitemSel = Just (fromCStore, iid)}
      return Nothing
    Left err -> failMsg err
    _ -> assert `failure` ggi

permittedApplyClient :: MonadClientUI m
                     => [Char] -> m (ItemFull -> Either ReqFailure Bool)
permittedApplyClient triggerSyms = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorSk <- leaderSkillsClientUI
  let skill = EM.findWithDefault 0 AbApply actorSk
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup leader actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` leader
      calmE = calmEnough b ar
  localTime <- getsState $ getLocalTime (blid b)
  return $ permittedApply localTime skill calmE triggerSyms

-- * PickLeader

pickLeaderHuman :: MonadClientUI m => Int -> m MError
pickLeaderHuman k = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  arena <- getArenaUI
  sactorUI <- getsSession sactorUI
  mhero <- getsState $ tryFindHeroK sactorUI side k
  allA <- getsState $ EM.assocs . sactorD  -- not only on one level
  let allOurs = filter (\(_, body) ->
        not (bproj body) && bfid body == side) allA
      allOursUI = map (\(aid, b) -> (aid, b, sactorUI EM.! aid)) allOurs
      hs = sortBy (comparing keySelected) allOursUI
      mactor = case drop k hs of
                 [] -> Nothing
                 (aid, b, _) : _ -> Just (aid, b)
      mchoice = mhero `mplus` mactor
      (autoDun, _) = autoDungeonLevel fact
  case mchoice of
    Nothing -> failMsg "no such member of the party"
    Just (aid, b)
      | blid b /= arena && autoDun ->
          failMsg $ showReqFailure NoChangeDunLeader
      | otherwise -> do
          void $ pickLeader True aid
          return Nothing

-- * PickLeaderWithPointer

pickLeaderWithPointerHuman :: MonadClientUI m => m MError
pickLeaderWithPointerHuman = pickLeaderWithPointer

-- * MemberCycle

-- | Switches current member to the next on the viewed level, if any, wrapping.
memberCycleHuman :: MonadClientUI m => m MError
memberCycleHuman = memberCycle True

-- * MemberBack

-- | Switches current member to the previous in the whole dungeon, wrapping.
memberBackHuman :: MonadClientUI m => m MError
memberBackHuman = memberBack True

-- * SelectActor

selectActorHuman :: MonadClientUI m => m ()
selectActorHuman = do
  leader <- getLeaderUI
  selectAidHuman leader

selectAidHuman :: MonadClientUI m => ActorId -> m ()
selectAidHuman leader = do
  bodyUI <- getsSession $ getActorUI leader
  wasMemeber <- getsSession $ ES.member leader . sselected
  let upd = if wasMemeber
            then ES.delete leader  -- already selected, deselect instead
            else ES.insert leader
  modifySession $ \sess -> sess {sselected = upd $ sselected sess}
  let subject = partActor bodyUI
  promptAdd $ makeSentence [subject, if wasMemeber
                                     then "deselected"
                                     else "selected"]

-- * SelectNone

selectNoneHuman :: MonadClientUI m => m ()
selectNoneHuman = do
  side <- getsClient sside
  lidV <- viewedLevelUI
  oursIds <- getsState $ fidActorRegularIds side lidV
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
  sactorUI <- getsSession sactorUI
  let oursUI = map (\(aid, b) -> (aid, b, sactorUI EM.! aid)) ours
      viewed = sortBy (comparing keySelected) oursUI
  Point{..} <- getsSession spointer
  -- Select even if no space in status line for the actor's symbol.
  if | py == lysize + 2 && px == 0 -> selectNoneHuman >> return Nothing
     | py == lysize + 2 ->
         case drop (px - 1) viewed of
           [] -> failMsg "not pointing at an actor"
           (aid, _, _) : _ -> selectAidHuman aid >> return Nothing
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

historyHuman :: forall m. MonadClientUI m => m ()
historyHuman = do
  history <- getsSession shistory
  arena <- getArenaUI
  Level{lxsize, lysize} <- getLevel arena
  localTime <- getsState $ getLocalTime arena
  global <- getsState stime
  let rh = renderHistory history
      turnsGlobal = global `timeFitUp` timeTurn
      turnsLocal = localTime `timeFitUp` timeTurn
      msg = makeSentence
        [ "You survived for"
        , MU.CarWs turnsGlobal "half-second turn"
        , "(this level:"
        , MU.Text (tshow turnsLocal) <> ")" ]
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
          Right SlotChar{..} | slotChar == 'a' ->
            displayOneReport slotPrefix
          _ -> assert `failure` ekm
      displayOneReport :: Int -> m ()
      displayOneReport histSlot = do
        let timeReport = case drop histSlot rh of
              [] -> assert `failure` histSlot
              tR : _ -> tR
            ov0 = splitReportForHistory lxsize timeReport
            prompt = makeSentence
              [ "the", MU.Ordinal $ histSlot + 1
              , "record of all history follows" ]
            histBound = lengthHistory history - 1
            keys = [K.spaceKM, K.escKM] ++ [K.upKM | histSlot /= 0]
                                        ++ [K.downKM | histSlot /= histBound]
        promptAdd prompt
        slides <- overlayToSlideshow (lysize + 1) keys (ov0, [])
        km <- getConfirms ColorFull keys slides
        case K.key km of
          K.Space -> displayAllHistory
          K.Up -> displayOneReport $ histSlot - 1
          K.Down -> displayOneReport $ histSlot + 1
          K.Esc -> promptAdd "Try to learn from your previous mistakes."
          _ -> assert `failure` km
  displayAllHistory

-- * MarkVision

markVisionHuman :: MonadClientUI m => m ()
markVisionHuman = modifySession toggleMarkVision

-- * MarkSmell

markSmellHuman :: MonadClientUI m => m ()
markSmellHuman = modifySession toggleMarkSmell

-- * MarkSuspect

markSuspectHuman :: MonadClientUI m => m ()
markSuspectHuman = do
  -- @condBFS@ depends on the setting we change here.
  invalidateBfsAll
  modifyClient cycleMarkSuspect

-- * Cancel

-- | End aiming mode, rejecting the current position.
cancelHuman :: MonadClientUI m => m ()
cancelHuman = do
  saimMode <- getsSession saimMode
  when (isJust saimMode) $ do
    clearAimMode
    promptAdd "Target not set."

-- * Accept

-- | Accept the current x-hair position as target, ending
-- aiming mode, if active.
acceptHuman :: MonadClientUI m => m ()
acceptHuman = do
  endAiming
  endAimingMsg
  clearAimMode

-- | End aiming mode, accepting the current position.
endAiming :: MonadClientUI m => m ()
endAiming = do
  leader <- getLeaderUI
  sxhair <- getsSession sxhair
  modifyClient $ updateTarget leader $ const $ Just sxhair

endAimingMsg :: MonadClientUI m => m ()
endAimingMsg = do
  leader <- getLeaderUI
  (mtargetMsg, _) <- targetDescLeader leader
  let targetMsg = fromJust mtargetMsg
  subject <- partAidLeader leader
  promptAdd $
    makeSentence [MU.SubjectVerbSg subject "target", MU.Text targetMsg]

-- * TgtClear

tgtClearHuman :: MonadClientUI m => m ()
tgtClearHuman = do
  leader <- getLeaderUI
  tgt <- getsClient $ getTarget leader
  case tgt of
    Just _ -> do
      modifyClient $ updateTarget leader (const Nothing)
    Nothing -> do
      clearXhair
      doLook

-- * ItemClear

itemClearHuman :: MonadClientUI m => m ()
itemClearHuman = do
  modifySession $ \sess -> sess {sitemSel = Nothing}

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
      inhabitants <- getsState $ posToAssocs p lidV
      sactorUI <- getsSession sactorUI
      let inhabitantsUI =
            map (\(aid2, b2) -> (aid2, b2, sactorUI EM.! aid2)) inhabitants
      seps <- getsClient seps
      mnewEps <- makeLine False b p seps
      itemToF <- itemToFullClient
      let aims = isJust mnewEps
          enemyMsg = case inhabitants of
            [] -> ""
            (_, body) : rest ->
                 -- Even if it's the leader, give his proper name, not 'you'.
                 let subjects = map (\(_, _, bUI) ->
                       partActor bUI) inhabitantsUI
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
          canSee = ES.member p (totalVisible per)
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
  sxhair <- getsSession sxhair
  xhairPos <- xhairToPos
  let cpos = fromMaybe lpos xhairPos
      shiftB pos = shiftBounded lxsize lysize pos dir
      newPos = iterate shiftB cpos !! n
  if newPos == cpos then failMsg "never mind"
  else do
    let tgt = case sxhair of
          TVector{} -> TVector $ newPos `vectorToFrom` lpos
          _ -> TPoint TAny lidV newPos
    modifySession $ \sess -> sess {sxhair = tgt}
    doLook
    return Nothing

-- * AimTgt

-- | Start aiming.
aimTgtHuman :: MonadClientUI m => m MError
aimTgtHuman = do
  -- (Re)start aiming at the current level.
  lidV <- viewedLevelUI
  modifySession $ \sess -> sess {saimMode = Just $ AimMode lidV}
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
  sxhair <- getsSession sxhair
  saimMode <- getsSession saimMode
  bsAll <- getsState $ actorAssocs (const True) lidV
  let xhair = fromMaybe lpos xhairPos
      tgt = case sxhair of
        _ | isNothing saimMode ->  -- first key press: keep target
          sxhair
        TEnemy a True -> TEnemy a False
        TEnemy{} -> TPoint TAny lidV xhair
        TPoint{} -> TVector $ xhair `vectorToFrom` lpos
        TVector{} ->
          -- For projectiles, we pick here the first that would be picked
          -- by '*', so that all other projectiles on the tile come next,
          -- without any intervening actors from other tiles.
          case find (\(_, m) -> Just (bpos m) == xhairPos) bsAll of
            Just (im, _) -> TEnemy im True
            Nothing -> TPoint TAny lidV xhair
  modifySession $ \sess -> sess {saimMode = Just $ AimMode lidV}
  modifySession $ \sess -> sess {sxhair = tgt}
  doLook

-- * AimEnemy

aimEnemyHuman :: MonadClientUI m => m ()
aimEnemyHuman = do
  lidV <- viewedLevelUI
  leader <- getLeaderUI
  lpos <- getsState $ bpos . getActorBody leader
  xhairPos <- xhairToPos
  sxhair <- getsSession sxhair
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
        TPoint (TEnemyPos _ permit) _ _ -> (permit, pickUnderXhair)
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
  modifySession $ \sess -> sess {sxhair = tgt}
  doLook

-- * AimItem

aimItemHuman :: MonadClientUI m => m ()
aimItemHuman = do
  lidV <- viewedLevelUI
  leader <- getLeaderUI
  lpos <- getsState $ bpos . getActorBody leader
  xhairPos <- xhairToPos
  sxhair <- getsSession sxhair
  saimMode <- getsSession saimMode
  bsAll <- getsState $ EM.keys . lfloor . (EM.! lidV) . sdungeon
  let ordPos p = (chessDist lpos p, p)
      dbs = sortBy (comparing ordPos) bsAll
      pickUnderXhair =  -- switch to the item under xhair, if any
        let i = fromMaybe (-1)
                $ findIndex ((== xhairPos) . Just) dbs
        in splitAt i dbs
      (lt, gt) = case sxhair of
        TPoint _ lid pos | isJust saimMode && lid == lidV ->  -- pick next item
          let i = fromMaybe (-1) $ findIndex (== pos) dbs
          in splitAt (i + 1) dbs
        TPoint _ lid pos | lid == lidV ->  -- first key press, retarget old item
          let i = fromMaybe (-1) $ findIndex (== pos) dbs
          in splitAt i dbs
        _ -> pickUnderXhair
      gtlt = gt ++ lt
      tgt = case gtlt of
        p : _ -> TPoint TAny lidV p
        [] -> sxhair  -- no items remembered, stick to last target
  -- Register the chosen enemy, to pick another on next invocation.
  modifySession $ \sess -> sess {saimMode = Just $ AimMode lidV}
  modifySession $ \sess -> sess {sxhair = tgt}
  doLook

-- * AimAscend

-- | Change the displayed level in aiming mode to (at most)
-- k levels shallower. Enters aiming mode, if not already in one.
aimAscendHuman :: MonadClientUI m => Int -> m MError
aimAscendHuman k = do
  dungeon <- getsState sdungeon
  lidV <- viewedLevelUI
  let up = k > 0
  case ascendInBranch dungeon up lidV of
    [] -> failMsg "no more levels in this direction"
    _ : _ -> do
      let ascendOne lid = case ascendInBranch dungeon up lid of
            [] -> lid
            nlid : _ -> nlid
          lidK = iterate ascendOne lidV !! abs k
      leader <- getLeaderUI
      lpos <- getsState $ bpos . getActorBody leader
      xhairPos <- xhairToPos
      let cpos = fromMaybe lpos xhairPos
          tgt = TPoint TAny lidK cpos
      modifySession $ \sess -> sess { saimMode = Just (AimMode lidK)
                                    , sxhair = tgt }
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
      let sxhair = TPoint TUnknown (blid b) p
      modifySession $ \sess -> sess {sxhair}
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
    (_, (p, bag)) : _ -> do
      let sxhair = TPoint (TItem bag) (blid b) p
      modifySession $ \sess -> sess {sxhair}
      doLook
      return Nothing

-- * XhairStair

xhairStairHuman :: MonadClientUI m => Bool -> m MError
xhairStairHuman up = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  stairs <- closestTriggers (if up then ViaStairsUp else ViaStairsDown) leader
  case sortBy (flip compare) stairs of
    [] -> failMsg $ "no stairs" <+> if up then "up" else "down"
    (_, (p, (p0, bag))) : _ -> do
      let sxhair = TPoint (TEmbed bag p0) (blid b) p
      modifySession $ \sess -> sess {sxhair}
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
    oldXhair <- getsSession sxhair
    let sxhair = TPoint TAny lidV $ Point px (py - mapStartY)
        sxhairMoused = sxhair /= oldXhair
    modifySession $ \sess ->
      sess { saimMode = Just $ AimMode lidV
           , sxhair
           , sxhairMoused }
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
    oldXhair <- getsSession sxhair
    let newPos = Point px (py - mapStartY)
        sxhair =
          case find (\(_, m) -> bpos m == newPos) bsAll of
            Just (im, _) -> TEnemy im True
            Nothing -> TPoint TAny lidV newPos
        sxhairMoused = sxhair /= oldXhair
    modifySession $ \sess ->
      sess { saimMode = Just $ AimMode lidV
           , sxhairMoused }
    modifySession $ \sess -> sess {sxhair}
    if verbose then doLook else flashAiming
  else stopPlayBack

-- * AimPointerFloor

aimPointerFloorHuman :: MonadClientUI m => m ()
aimPointerFloorHuman = xhairPointerFloor True

-- * AimPointerEnemy

aimPointerEnemyHuman :: MonadClientUI m => m ()
aimPointerEnemyHuman = xhairPointerEnemy True
