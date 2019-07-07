-- | Semantics of "Game.LambdaHack.Client.UI.HumanCmd"
-- client commands that do not return server requests,,
-- but only change internal client state.
-- None of such commands takes game time.
module Game.LambdaHack.Client.UI.HandleHumanLocalM
  ( -- * Meta commands
    macroHuman
    -- * Local commands
  , chooseItemHuman, chooseItemDialogMode
  , chooseItemProjectHuman, chooseItemApplyHuman
  , psuitReq, triggerSymbols, pickLeaderHuman, pickLeaderWithPointerHuman
  , memberCycleHuman, memberBackHuman
  , selectActorHuman, selectNoneHuman, selectWithPointerHuman
  , repeatHuman, recordHuman, allHistoryHuman, lastHistoryHuman
  , markVisionHuman, markSmellHuman, markSuspectHuman, markAnimHuman
  , printScreenHuman
    -- * Commands specific to aiming
  , cancelHuman, acceptHuman, clearTargetIfItemClearHuman, itemClearHuman
  , moveXhairHuman, aimTgtHuman, aimFloorHuman, aimEnemyHuman, aimItemHuman
  , aimAscendHuman, epsIncrHuman
  , xhairUnknownHuman, xhairItemHuman, xhairStairHuman
  , xhairPointerFloorHuman, xhairPointerEnemyHuman
  , aimPointerFloorHuman, aimPointerEnemyHuman
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , permittedProjectClient, projectCheck, xhairLegalEps, posFromXhair
  , permittedApplyClient, selectAid, eitherHistory, endAiming, endAimingMsg
  , doLook, flashAiming
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Ord
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Client.BfsM
import           Game.LambdaHack.Client.ClientOptions
import           Game.LambdaHack.Client.CommonM
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.ActorUI
import           Game.LambdaHack.Client.UI.Animation
import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Client.UI.DrawM
import           Game.LambdaHack.Client.UI.EffectDescription
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.FrameM
import           Game.LambdaHack.Client.UI.HandleHelperM
import           Game.LambdaHack.Client.UI.HumanCmd
import           Game.LambdaHack.Client.UI.InventoryM
import           Game.LambdaHack.Client.UI.ItemSlot
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.MsgM
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.Slideshow
import           Game.LambdaHack.Client.UI.SlideshowM
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
import           Game.LambdaHack.Common.ReqFailure
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind (fhasGender)
import qualified Game.LambdaHack.Content.PlaceKind as PK
import           Game.LambdaHack.Content.RuleKind
import qualified Game.LambdaHack.Definition.Ability as Ability
import qualified Game.LambdaHack.Definition.Color as Color
import           Game.LambdaHack.Definition.Defs

-- * Macro

macroHuman :: MonadClientUI m => [String] -> m ()
macroHuman kms = do
  modifySession $ \sess -> sess {slastPlay = map K.mkKM kms ++ slastPlay sess}
  msgAdd MsgMacro $ "Macro activated:" <+> T.pack (intercalate " " kms)

-- * ChooseItem

-- | Display items from a given container store and possibly let the user
-- chose one.
chooseItemHuman :: MonadClientUI m => ItemDialogMode -> m MError
chooseItemHuman c = either Just (const Nothing) <$> chooseItemDialogMode c

chooseItemDialogMode :: MonadClientUI m
                     => ItemDialogMode -> m (FailOrCmd ItemDialogMode)
chooseItemDialogMode c = do
  CCUI{coscreen=ScreenContent{rwidth, rheight}} <- getsSession sccui
  COps{coitem} <- getsState scops
  FontSetup{..} <- getFontSetup
  side <- getsClient sside
  let prompt :: Actor -> ActorUI -> Ability.Skills -> ItemDialogMode -> State
             -> Text
      prompt body bodyUI actorMaxSk c2 s =
        let (tIn, t) = ppItemDialogMode c2
            subject = partActor bodyUI
            f (k, _) acc = k + acc
            countItems store = EM.foldr' f 0 $ getBodyStoreBag body store s
        in case c2 of
        MStore CGround ->
          let n = countItems CGround
              nItems = MU.CarAWs n "item"
          in makePhrase
               [ MU.Capitalize $ MU.SubjectVerbSg subject "notice"
               , nItems, "at"
               , MU.WownW (MU.Text $ bpronoun bodyUI) $ MU.Text "feet" ]
        MStore CEqp ->
          let n = countItems CEqp
              (verbEqp, nItems) =
                if | n == 0 -> ("find nothing", "")
                   | calmEnough body actorMaxSk -> ("find", MU.CarAWs n "item")
                   | otherwise -> ("paw distractedly", "")
          in makePhrase
               [ MU.Capitalize $ MU.SubjectVerbSg subject verbEqp
               , nItems, MU.Text tIn
               , MU.WownW (MU.Text $ bpronoun bodyUI) $ MU.Text t ]
        MStore cstore ->
          let n = countItems cstore
              nItems = MU.CarAWs n "item"
              verb = case cstore of
                COrgan -> "feel"
                CStash -> "notice"
                _ -> "see"
              ownObject = case cstore of
                CStash -> ["our", MU.Text t]
                _ -> [MU.WownW (MU.Text $ bpronoun bodyUI) $ MU.Text t]
          in makePhrase $
               [ MU.Capitalize $ MU.SubjectVerbSg subject verb
               , nItems, MU.Text tIn ] ++ ownObject
        MOrgans ->
          makePhrase
            [ MU.Capitalize $ MU.SubjectVerbSg subject "feel"
            , MU.Text tIn
            , MU.WownW (MU.Text $ bpronoun bodyUI) $ MU.Text t ]
        MOwned ->
          -- We assume "gold grain", not "grain" with label "of gold":
          let currencyName = IK.iname $ okind coitem
                             $ ouniqGroup coitem "currency"
              dungeonTotal = sgold s
              (_, total) = calculateTotal side s
              n = countItems CStash
              verbOwned = if | n == 0 -> "find nothing among"
                             | otherwise -> "review"
          in makePhrase
               [ MU.Text $ spoilsBlurb currencyName total dungeonTotal
               , MU.Capitalize $ MU.SubjectVerbSg subject verbOwned
               , MU.Text t ]
        MSkills ->
          makePhrase
            [ MU.Capitalize $ MU.SubjectVerbSg subject "estimate"
            , MU.WownW (MU.Text $ bpronoun bodyUI) $ MU.Text t ]
        MLore{} ->
          makePhrase
            [ MU.Capitalize $ MU.Text t ]
        MPlaces ->
          makePhrase
            [ MU.Capitalize $ MU.Text t ]
  ggi <- getStoreItem prompt c
  recordHistory  -- item chosen, wipe out already shown msgs
  leader <- getLeaderUI
  actorMaxSk <- getsState $ getActorMaxSkills leader
  let meleeSkill = Ability.getSk Ability.SkHurtMelee actorMaxSk
  bUI <- getsSession $ getActorUI leader
  case ggi of
    (Right (iid, itemBag, lSlots), (c2, _)) ->
      case c2 of
        MStore fromCStore -> do
          modifySession $ \sess ->
            sess {sitemSel = Just (iid, fromCStore, False)}
          return $ Right c2
        MOrgans -> do
          let blurb itemFull =
                if IA.checkFlag Ability.Condition $ aspectRecordFull itemFull
                then "condition"
                else "organ"
              promptFun _ itemFull _ =
                makeSentence [ partActor bUI, "can't remove"
                             , MU.AW $ blurb itemFull ]
              ix0 = fromMaybe (error $ show iid)
                    $ findIndex (== iid) $ EM.elems lSlots
          go <- displayItemLore itemBag meleeSkill promptFun ix0 lSlots
          if go then chooseItemDialogMode c2 else failWith "never mind"
        MOwned -> do
          found <- getsState $ findIid leader side iid
          let (newAid, bestStore) = case leader `lookup` found of
                Just (_, store) -> (leader, store)
                Nothing -> case found of
                  (aid, (_, store)) : _ -> (aid, store)
                  [] -> error $ "" `showFailure` iid
          modifySession $ \sess ->
            sess {sitemSel = Just (iid, bestStore, False)}
          arena <- getArenaUI
          b2 <- getsState $ getActorBody newAid
          fact <- getsState $ (EM.! side) . sfactionD
          let (autoDun, _) = autoDungeonLevel fact
          if | newAid == leader -> return $ Right c2
             | blid b2 /= arena && autoDun ->
               failSer NoChangeDunLeader
             | otherwise -> do
               -- We switch leader only here, not in lore screens, because
               -- lore is only about inspecting items, no activation submenu.
               void $ pickLeader True newAid
               return $ Right c2
        MSkills -> error $ "" `showFailure` ggi
        MLore slore -> do
          let ix0 = fromMaybe (error $ show iid)
                    $ findIndex (== iid) $ EM.elems lSlots
              promptFun _ _ _ =
                makeSentence [ MU.SubjectVerbSg (partActor bUI) "remember"
                             , MU.AW $ MU.Text (headingSLore slore) ]
          go <- displayItemLore itemBag meleeSkill promptFun ix0 lSlots
          if go then chooseItemDialogMode c2 else failWith "never mind"
        MPlaces -> error $ "" `showFailure` ggi
    (Left err, (MSkills, ekm)) -> case ekm of
      Right slot0 -> assert (err == "skills") $ do
        let slotListBound = length skillSlots - 1
            displayOneSlot slotIndex = do
              b <- getsState $ getActorBody leader
              let slot = allSlots !! slotIndex
                  skill = skillSlots !! fromMaybe (error $ show slot)
                                                  (elemIndex slot allSlots)
                  valueText =
                    skillToDecorator skill b $ Ability.getSk skill actorMaxSk
                  prompt2 = makeSentence
                    [ MU.WownW (partActor bUI) (MU.Text $ skillName skill)
                    , "is", MU.Text valueText ]
                  ov0 = EM.singleton propFont
                        $ offsetOverlay
                        $ indentSplitAttrString rwidth
                        $ textToAS $ skillDesc skill
                  keys = [K.spaceKM, K.escKM]
                         ++ [K.upKM | slotIndex /= 0]
                         ++ [K.downKM | slotIndex /= slotListBound]
              promptAdd0 prompt2
              slides <- overlayToSlideshow (rheight - 2) keys (ov0, [])
              km <- getConfirms ColorFull keys slides
              case K.key km of
                K.Space -> chooseItemDialogMode MSkills
                K.Up -> displayOneSlot $ slotIndex - 1
                K.Down -> displayOneSlot $ slotIndex + 1
                K.Esc -> failWith "never mind"
                _ -> error $ "" `showFailure` km
            slotIndex0 = fromMaybe (error "displayOneSlot: illegal slot")
                         $ elemIndex slot0 allSlots
        displayOneSlot slotIndex0
      Left _ -> failWith "never mind"
    (Left err, (MPlaces, ekm)) -> case ekm of
      Right slot0 -> assert (err == "places") $ do
        COps{coplace} <- getsState scops
        soptions <- getsClient soptions
        places <- getsState $ EM.assocs . placesFromState coplace soptions
        let slotListBound = length places - 1
            displayOneSlot slotIndex = do
              let slot = allSlots !! slotIndex
                  (pk, figures@(es, _, _, _)) =
                    places !! fromMaybe (error $ show slot)
                                        (elemIndex slot allSlots)
                  pkind = okind coplace pk
                  partsPhrase = makePhrase $ placeParts figures
                  prompt2 = makeSentence
                    [ MU.SubjectVerbSg (partActor bUI) "remember"
                    , MU.Text $ PK.pname pkind ]
                  freqsText = "Frequencies:" <+> T.intercalate " "
                    (map (\(grp, n) -> "(" <> fromGroupName grp
                                       <> ", " <> tshow n <> ")")
                     $ PK.pfreq pkind)
                  onLevels | ES.null es = []
                           | otherwise =
                    [makeSentence
                       [ "Appears on"
                       , MU.CarWs (ES.size es) "level" <> ":"
                       , MU.WWandW $ map MU.Car $ sort
                                   $ map (abs . fromEnum) $ ES.elems es ]]
                  -- Ideally, place layout would be in SquareFont and the rest
                  -- in PropFont, but this is mostly a debug screen, so KISS.
                  ov0 = EM.singleton monoFont
                        $ offsetOverlay
                        $ concatMap (indentSplitAttrString rwidth . textToAS)
                        $ (if sexposePlaces soptions
                           then [ "", partsPhrase
                                , "", freqsText
                                , "" ] ++ PK.ptopLeft pkind
                           else [])
                          ++ [""] ++ onLevels
                  keys = [K.spaceKM, K.escKM]
                         ++ [K.upKM | slotIndex /= 0]
                         ++ [K.downKM | slotIndex /= slotListBound]
              promptAdd0 prompt2
              slides <- overlayToSlideshow (rheight - 2) keys (ov0, [])
              km <- getConfirms ColorFull keys slides
              case K.key km of
                K.Space -> chooseItemDialogMode MPlaces
                K.Up -> displayOneSlot $ slotIndex - 1
                K.Down -> displayOneSlot $ slotIndex + 1
                K.Esc -> failWith "never mind"
                _ -> error $ "" `showFailure` km
            slotIndex0 = fromMaybe (error "displayOneSlot: illegal slot")
                         $ elemIndex slot0 allSlots
        displayOneSlot slotIndex0
      Left _ -> failWith "never mind"
    (Left err, _) -> failWith err

-- * ChooseItemProject

chooseItemProjectHuman :: forall m. (MonadClient m, MonadClientUI m)
                       => [TriggerItem] -> m MError
chooseItemProjectHuman ts = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorMaxSk <- getsState $ getActorMaxSkills leader
  mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid b
  let overStash = mstash == Just (blid b, bpos b)
      calmE = calmEnough b actorMaxSk
      cLegalRaw = [CGround, CStash, CEqp]
      cLegal = [CGround | not overStash] ++ [CStash] ++ [CEqp | calmE]
      (verb1, object1) = case ts of
        [] -> ("aim", "item")
        tr : _ -> (tiverb tr, tiobject tr)
      triggerSyms = triggerSymbols ts
  mpsuitReq <- psuitReq
  case mpsuitReq of
    -- If xhair aim invalid, no item is considered a (suitable) missile.
    Left err -> failMsg err
    Right psuitReqFun -> do
      itemSel <- getsSession sitemSel
      case itemSel of
        Just (_, _, True) -> return Nothing
        Just (iid, fromCStore, False) -> do
          -- We don't validate vs @ts@ here, because player has selected
          -- this item, so he knows what he's doing (unless really absurd).
          itemFull <- getsState $ itemToFull iid
          bag <- getsState $ getBodyStoreBag b fromCStore
          case iid `EM.lookup` bag of
            Just _ | either (const False) snd (psuitReqFun itemFull) ->
              return Nothing
            _ -> do
              modifySession $ \sess -> sess {sitemSel = Nothing}
              chooseItemProjectHuman ts
        Nothing -> do
          let psuit =
                return $ SuitsSomething $ \itemFull _kit ->
                  either (const False) snd (psuitReqFun itemFull)
                  && (null triggerSyms
                      || IK.isymbol (itemKind itemFull) `elem` triggerSyms)
              prompt = makePhrase ["What", object1, "to", verb1]
              promptGeneric = "What to fling"
          ggi <- getGroupItem psuit prompt promptGeneric cLegalRaw cLegal
          case ggi of
            Right (iid, (MStore fromCStore, _)) -> do
              modifySession $ \sess ->
                sess {sitemSel = Just (iid, fromCStore, False)}
              return Nothing
            Left err -> failMsg err
            _ -> error $ "" `showFailure` ggi

permittedProjectClient :: MonadClientUI m
                       => m (ItemFull -> Either ReqFailure Bool)
permittedProjectClient = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorMaxSk <- getsState $ getActorMaxSkills leader
  actorSk <- leaderSkillsClientUI
  let skill = Ability.getSk Ability.SkProject actorSk
      calmE = calmEnough b actorMaxSk
  return $ permittedProject False skill calmE

projectCheck :: MonadClientUI m => Point -> m (Maybe ReqFailure)
projectCheck tpos = do
  COps{corule=RuleContent{rXmax, rYmax}, coTileSpeedup} <- getsState scops
  leader <- getLeaderUI
  eps <- getsClient seps
  sb <- getsState $ getActorBody leader
  let lid = blid sb
      spos = bpos sb
  -- Not @ScreenContent@, because not drawing here.
  case bla rXmax rYmax eps spos tpos of
    Nothing -> return $ Just ProjectAimOnself
    Just [] -> error $ "project from the edge of level"
                       `showFailure` (spos, tpos, sb)
    Just (pos : _) -> do
      lvl <- getLevel lid
      let t = lvl `at` pos
      if not $ Tile.isWalkable coTileSpeedup t
        then return $ Just ProjectBlockTerrain
        else if occupiedBigLvl pos lvl
             then return $ Just ProjectBlockActor
             else return Nothing

-- | Check whether one is permitted to aim (for projecting) at a target.
-- The check is stricter for actor targets, assuming the player simply wants
-- to hit a single actor. In order to fine tune trick-shots, e.g., piercing
-- many actors, other aiming modes should be used.
-- Returns a different @seps@ if needed to reach the target.
--
-- Note: Simple Perception check is not enough for the check,
-- e.g., because the target actor can be obscured by a glass wall.
xhairLegalEps :: MonadClientUI m => m (Either Text Int)
xhairLegalEps = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  lidV <- viewedLevelUI
  let !_A = assert (lidV == blid b) ()
      findNewEps onlyFirst pos = do
        oldEps <- getsClient seps
        mnewEps <- makeLine onlyFirst b pos oldEps
        return $! case mnewEps of
          Just newEps -> Right newEps
          Nothing -> Left $ if onlyFirst
                            then "aiming blocked at the first step"
                            else "aiming line blocked somewhere"
  xhair <- getsSession sxhair
  case xhair of
    Nothing -> return $ Left "no aim designated"
    Just (TEnemy a) -> do
      body <- getsState $ getActorBody a
      let pos = bpos body
      if blid body == lidV
      then findNewEps False pos
      else return $ Left "can't fling at an enemy on remote level"
    Just (TNonEnemy a) -> do
      body <- getsState $ getActorBody a
      let pos = bpos body
      if blid body == lidV
      then findNewEps False pos
      else return $ Left "can't fling at a non-enemy on remote level"
    Just (TPoint TEnemyPos{} _ _) ->
      return $ Left "selected opponent not visible"
    Just (TPoint _ lid pos) ->
      if lid == lidV
      then findNewEps True pos  -- @True@ to help pierce many foes, etc.
      else return $ Left "can't fling at a target on remote level"
    Just (TVector v) -> do
      -- Not @ScreenContent@, because not drawing here.
      COps{corule=RuleContent{rXmax, rYmax}} <- getsState scops
      let shifted = shiftBounded rXmax rYmax (bpos b) v
      if shifted == bpos b && v /= Vector 0 0
      then return $ Left "selected translation is void"
      else findNewEps True shifted  -- @True@, because the goal is vague anyway

posFromXhair :: (MonadClient m, MonadClientUI m) => m (Either Text Point)
posFromXhair = do
  canAim <- xhairLegalEps
  case canAim of
    Right newEps -> do
      -- Modify @seps@, permanently.
      modifyClient $ \cli -> cli {seps = newEps}
      mpos <- xhairToPos
      case mpos of
        Nothing -> error $ "" `showFailure` mpos
        Just pos -> do
          munit <- projectCheck pos
          case munit of
            Nothing -> return $ Right pos
            Just reqFail -> return $ Left $ showReqFailure reqFail
    Left cause -> return $ Left cause

-- | On top of @permittedProjectClient@, it also checks legality
-- of aiming at the target and projection range. It also modifies @eps@.
psuitReq :: (MonadClient m, MonadClientUI m)
         => m (Either Text (ItemFull -> Either ReqFailure (Point, Bool)))
psuitReq = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  lidV <- viewedLevelUI
  if lidV /= blid b
  then return $ Left "can't fling on remote level"
  else do
    mpos <- posFromXhair
    p <- permittedProjectClient
    case mpos of
      Left err -> return $ Left err
      Right pos -> return $ Right $ \itemFull ->
        case p itemFull of
          Left err -> Left err
          Right False -> Right (pos, False)
          Right True ->
            let arItem = aspectRecordFull itemFull
            in Right (pos, IA.totalRange arItem (itemKind itemFull)
                           >= chessDist (bpos b) pos)

triggerSymbols :: [TriggerItem] -> [Char]
triggerSymbols [] = []
triggerSymbols (TriggerItem{tisymbols} : ts) = tisymbols ++ triggerSymbols ts

-- * ChooseItemApply

chooseItemApplyHuman :: forall m. MonadClientUI m => [TriggerItem] -> m MError
chooseItemApplyHuman ts = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorMaxSk <- getsState $ getActorMaxSkills leader
  mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid b
  let overStash = mstash == Just (blid b, bpos b)
      calmE = calmEnough b actorMaxSk
      cLegalRaw = [CGround, CStash, CEqp]
      cLegal = [CGround | not overStash] ++ [CStash] ++ [CEqp | calmE]
      (verb1, object1) = case ts of
        [] -> ("apply", "item")
        tr : _ -> (tiverb tr, tiobject tr)
      triggerSyms = triggerSymbols ts
      prompt = makePhrase ["What", object1, "to", verb1]
      promptGeneric = "What to apply"
  itemSel <- getsSession sitemSel
  case itemSel of
    Just (_, _, True) -> return Nothing
    Just (iid, fromCStore, False) -> do
      -- We don't validate vs @ts@ here, because player has selected
      -- this item, so he knows what he's doing (unless really absurd).
      itemFull <- getsState $ itemToFull iid
      bag <- getsState $ getBodyStoreBag b fromCStore
      mp <- permittedApplyClient
      case iid `EM.lookup` bag of
        Just kit | either (const False) id (mp itemFull kit) ->
          return Nothing
        _ -> do
          modifySession $ \sess -> sess {sitemSel = Nothing}
          chooseItemApplyHuman ts
    Nothing -> do
      let psuit :: m Suitability
          psuit = do
            mp <- permittedApplyClient
            return $ SuitsSomething $ \itemFull kit ->
              either (const False) id (mp itemFull kit)
              && (null triggerSyms
                  || IK.isymbol (itemKind itemFull) `elem` triggerSyms)
      ggi <- getGroupItem psuit prompt promptGeneric cLegalRaw cLegal
      case ggi of
        Right (iid, (MStore fromCStore, _)) -> do
          modifySession $ \sess ->
            sess {sitemSel = Just (iid, fromCStore, False)}
          return Nothing
        Left err -> failMsg err
        _ -> error $ "" `showFailure` ggi

permittedApplyClient :: MonadClientUI m
                     => m (ItemFull -> ItemQuant -> Either ReqFailure Bool)
permittedApplyClient = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorMaxSk <- getsState $ getActorMaxSkills leader
  actorSk <- leaderSkillsClientUI
  let skill = Ability.getSk Ability.SkApply actorSk
      calmE = calmEnough b actorMaxSk
  localTime <- getsState $ getLocalTime (blid b)
  return $ permittedApply localTime skill calmE

-- * PickLeader

pickLeaderHuman :: MonadClientUI m => Int -> m MError
pickLeaderHuman k = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  arena <- getArenaUI
  sactorUI <- getsSession sactorUI
  mhero <- getsState $ tryFindHeroK sactorUI side k
  allOurs <- getsState $ fidActorNotProjGlobalAssocs side -- not only on level
  let allOursUI = map (\(aid, b) -> (aid, b, sactorUI EM.! aid)) allOurs
      hs = sortOn keySelected allOursUI
      mactor = case drop k hs of
                 [] -> Nothing
                 (aid, b, _) : _ -> Just (aid, b)
      mchoice = if fhasGender (gplayer fact) then mhero else mactor
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

-- | Switch current member to the next on the viewed level, if any, wrapping.
memberCycleHuman :: MonadClientUI m => m MError
memberCycleHuman = memberCycle True

-- * MemberBack

-- | Switch current member to the previous in the whole dungeon, wrapping.
memberBackHuman :: MonadClientUI m => m MError
memberBackHuman = memberBack True

-- * SelectActor

selectActorHuman :: MonadClientUI m => m ()
selectActorHuman = do
  leader <- getLeaderUI
  selectAid leader

selectAid :: MonadClientUI m => ActorId -> m ()
selectAid leader = do
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
  COps{corule=RuleContent{rYmax}} <- getsState scops
  lidV <- viewedLevelUI
  -- Not @ScreenContent@, because not drawing here.
  side <- getsClient sside
  ours <- getsState $ filter (not . bproj . snd)
                      . actorAssocs (== side) lidV
  sactorUI <- getsSession sactorUI
  let oursUI = map (\(aid, b) -> (aid, b, sactorUI EM.! aid)) ours
      viewed = sortOn keySelected oursUI
  K.PointUI x y <- getsSession spointer
  let (px, py) = (x `div` 2, y - K.mapStartY)
  -- Select even if no space in status line for the actor's symbol.
  if | py == rYmax + 1 && px == 0 -> selectNoneHuman >> return Nothing
     | py == rYmax + 1 ->
         case drop (px - 1) viewed of
           [] -> failMsg "not pointing at an actor"
           (aid, _, _) : _ -> selectAid aid >> return Nothing
     | otherwise ->
         case find (\(_, b) -> bpos b == Point px py) ours of
           Nothing -> failMsg "not pointing at an actor"
           Just (aid, _) -> selectAid aid >> return Nothing

-- * Repeat

-- Note that walk followed by repeat should not be equivalent to run,
-- because the player can really use a command that does not stop
-- at terrain change or when walking over items.
repeatHuman :: MonadClientUI m => Int -> m ()
repeatHuman n = do
  LastRecord _ seqPrevious k <- getsSession slastRecord
  let macro = concat $ replicate n $ reverse seqPrevious
  modifySession $ \sess -> sess {slastPlay = macro ++ slastPlay sess}
  let slastRecord = LastRecord [] [] (if k == 0 then 0 else maxK)
  modifySession $ \sess -> sess {slastRecord}

maxK :: Int
maxK = 100

-- * Record

recordHuman :: MonadClientUI m => m ()
recordHuman = do
  lastPlayOld <- getsSession slastPlay
  LastRecord _seqCurrent seqPrevious k <- getsSession slastRecord
  case k of
    0 -> do
      let slastRecord = LastRecord [] [] maxK
      modifySession $ \sess -> sess {slastRecord}
      when (null lastPlayOld) $
        -- Don't spam if recording is a part of playing back a macro.
        promptAdd0 $ "Macro will be recorded for up to"
                     <+> tshow maxK
                     <+> "actions. Stop recording with the same key."
    _ -> do
      let slastRecord = LastRecord seqPrevious [] 0
      modifySession $ \sess -> sess {slastRecord}
      when (null lastPlayOld) $
        -- Don't spam if recording is a part of playing back a macro.
        promptAdd0 $ "Macro recording stopped after"
                     <+> tshow (maxK - k - 1) <+> "actions."

-- * AllHistory

allHistoryHuman :: MonadClientUI m => m ()
allHistoryHuman = eitherHistory True

eitherHistory :: forall m. MonadClientUI m => Bool -> m ()
eitherHistory showAll = do
  CCUI{coscreen=ScreenContent{rwidth, rheight}} <- getsSession sccui
  history <- getsSession shistory
  arena <- getArenaUI
  localTime <- getsState $ getLocalTime arena
  global <- getsState stime
  FontSetup{..} <- getFontSetup
  let renderedHistory = renderHistory history
      histBound = length renderedHistory
      splitRow al =
        let (spNo, spYes) = span (/= Color.spaceAttrW32) al
            par1 = case linesAttr spYes of
              [] -> emptyAttrLine
              [l] -> l
              ls -> attrStringToAL $ intercalate [Color.spaceAttrW32]
                                   $ map attrLine ls
        in (attrStringToAL spNo, (textSize monoFont spNo, par1))
      (histLab, histDesc) = unzip $ map splitRow renderedHistory
      rhLab = EM.singleton monoFont $ offsetOverlay histLab
      rhDesc = EM.singleton propFont $ offsetOverlayX histDesc
      turnsGlobal = global `timeFitUp` timeTurn
      turnsLocal = localTime `timeFitUp` timeTurn
      msg = makeSentence
        [ "You survived for"
        , MU.CarWs turnsGlobal "half-second turn"
        , "(this level:"
        , MU.Car turnsLocal <> ")" ]
      kxs = [ (Right sn, ( K.PointUI 0 (slotPrefix sn)
                         , ButtonWidth propFont 1000 ))
            | sn <- take histBound intSlots ]
  promptAdd0 msg
  okxs <- overlayToSlideshow (rheight - 2) [K.returnKM, K.escKM]
                             (EM.unionWith (++) rhLab rhDesc, kxs)
  let displayAllHistory = do
        ekm <- displayChoiceScreen "history" ColorFull False okxs
                                   [K.spaceKM, K.escKM]
        case ekm of
          Left km | km == K.escKM ->
            promptAdd0 "Try to survive a few seconds more, if you can."
          Left km | km == K.spaceKM ->  -- click in any unused space
            promptAdd0 "Steady on."
          Right SlotChar{..} | slotChar == 'a' ->
            displayOneReport slotPrefix
          _ -> error $ "" `showFailure` ekm
      displayOneReport :: Int -> m ()
      displayOneReport histSlot = do
        let timeReport = case drop histSlot renderedHistory of
              [] -> error $ "" `showFailure` histSlot
              tR : _ -> tR
            ov0 = EM.singleton propFont $ offsetOverlay
                  $ indentSplitAttrString rwidth timeReport
            prompt = makeSentence
              [ "the", MU.Ordinal $ histSlot + 1
              , "record of all history follows" ]
            keys = [K.spaceKM, K.escKM]
                   ++ [K.upKM | histSlot /= 0]
                   ++ [K.downKM | histSlot /= histBound - 1]
        promptAdd0 prompt
        slides <- overlayToSlideshow (rheight - 2) keys (ov0, [])
        km <- getConfirms ColorFull keys slides
        case K.key km of
          K.Space -> displayAllHistory
          K.Up -> displayOneReport $ histSlot - 1
          K.Down -> displayOneReport $ histSlot + 1
          K.Esc -> promptAdd0 "Try to learn from your previous mistakes."
          _ -> error $ "" `showFailure` km
  if showAll
  then displayAllHistory
  else displayOneReport 0

-- * LastHistory

lastHistoryHuman :: MonadClientUI m => m ()
lastHistoryHuman = eitherHistory False

-- * MarkVision

markVisionHuman :: MonadClientUI m => m ()
markVisionHuman = modifySession toggleMarkVision

-- * MarkSmell

markSmellHuman :: MonadClientUI m => m ()
markSmellHuman = modifySession toggleMarkSmell

-- * MarkSuspect

markSuspectHuman :: MonadClient m => m ()
markSuspectHuman = do
  -- @condBFS@ depends on the setting we change here.
  invalidateBfsAll
  modifyClient cycleMarkSuspect

-- * MarkAnim

markAnimHuman :: MonadClient m => m ()
markAnimHuman = do
  noAnim <- getsClient $ fromMaybe False . snoAnim . soptions
  modifyClient $ \cli ->
    cli {soptions = (soptions cli) {snoAnim = Just $ not noAnim}}

-- * PrintScreen

printScreenHuman :: MonadClientUI m => m ()
printScreenHuman = do
  promptAdd "Screenshot printed."
  printScreen

-- * Cancel

-- | End aiming mode, rejecting the current position.
cancelHuman :: MonadClientUI m => m ()
cancelHuman = do
  saimMode <- getsSession saimMode
  when (isJust saimMode) clearAimMode

-- * Accept

-- | Accept the current crosshair position as target, ending
-- aiming mode, if active.
acceptHuman :: (MonadClient m, MonadClientUI m) => m ()
acceptHuman = do
  endAiming
  endAimingMsg
  clearAimMode

-- | End aiming mode, accepting the current position.
endAiming :: (MonadClient m, MonadClientUI m) => m ()
endAiming = do
  leader <- getLeaderUI
  sxhair <- getsSession sxhair
  modifyClient $ updateTarget leader $ const sxhair

endAimingMsg :: MonadClientUI m => m ()
endAimingMsg = do
  leader <- getLeaderUI
  subject <- partActorLeader leader
  tgt <- getsClient $ getTarget leader
  (mtargetMsg, _) <- targetDesc tgt
  promptAdd $ case mtargetMsg of
    Nothing ->
      makeSentence [MU.SubjectVerbSg subject "clear target"]
    Just targetMsg ->
      makeSentence [MU.SubjectVerbSg subject "target", MU.Text targetMsg]

-- * ClearTargetIfItemClear

clearTargetIfItemClearHuman :: (MonadClient m, MonadClientUI m) => m ()
clearTargetIfItemClearHuman = do
  itemSel <- getsSession sitemSel
  when (isNothing itemSel) $ do
    modifySession $ \sess -> sess {sxhair = Nothing}
    leader <- getLeaderUI
    modifyClient $ updateTarget leader (const Nothing)
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
      mxhairPos <- xhairToPos
      b <- getsState $ getActorBody leader
      let xhairPos = fromMaybe (bpos b) mxhairPos
      blurb <- lookAtPosition lidV xhairPos
      promptAdd0 blurb

-- * ItemClear

itemClearHuman :: MonadClientUI m => m ()
itemClearHuman = modifySession $ \sess -> sess {sitemSel = Nothing}

-- * MoveXhair

-- | Move the xhair. Assumes aiming mode.
moveXhairHuman :: MonadClientUI m => Vector -> Int -> m MError
moveXhairHuman dir n = do
  COps{corule=RuleContent{rXmax, rYmax}} <- getsState scops
  leader <- getLeaderUI
  saimMode <- getsSession saimMode
  let lidV = maybe (error $ "" `showFailure` leader) aimLevelId saimMode
  -- Not @ScreenContent@, because not drawing here.
  lpos <- getsState $ bpos . getActorBody leader
  xhair <- getsSession sxhair
  mxhairPos <- xhairToPos
  let xhairPos = fromMaybe lpos mxhairPos
      shiftB pos = shiftBounded rXmax rYmax pos dir
      newPos = iterate shiftB xhairPos !! n
  if newPos == xhairPos then failMsg "never mind"
  else do
    let sxhair = case xhair of
          Just TVector{} -> Just $ TVector $ newPos `vectorToFrom` lpos
          _ -> Just $ TPoint TKnown lidV newPos
    modifySession $ \sess -> sess {sxhair}
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
  mxhairPos <- xhairToPos
  xhair <- getsSession sxhair
  saimMode <- getsSession saimMode
  bsAll <- getsState $ actorAssocs (const True) lidV
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  let xhairPos = fromMaybe lpos mxhairPos
      sxhair = case xhair of
        _ | isNothing saimMode ->  -- first key press: keep target
          xhair
        Just TEnemy{} -> Just $ TPoint TKnown lidV xhairPos
        Just TNonEnemy{} -> Just $ TPoint TKnown lidV xhairPos
        Just TPoint{} | xhairPos /= lpos ->
          Just $ TVector $ xhairPos `vectorToFrom` lpos
        Just TVector{} ->
          -- If many actors, we pick here the first that would be picked
          -- by '*', so that all other projectiles on the tile come next,
          -- when pressing "*", without any intervening actors from other tiles.
          -- This is why we use @actorAssocs@ above instead of @posToAidAssocs@.
          case find (\(_, b) -> Just (bpos b) == mxhairPos) bsAll of
            Just (aid, b) -> Just $ if isFoe side fact (bfid b)
                                    then TEnemy aid
                                    else TNonEnemy aid
            Nothing -> Just $ TPoint TUnknown lidV xhairPos
        _ -> xhair
  modifySession $ \sess -> sess { saimMode = Just $ AimMode lidV
                                , sxhair }
  doLook

-- * AimEnemy

aimEnemyHuman :: MonadClientUI m => m ()
aimEnemyHuman = do
  lidV <- viewedLevelUI
  leader <- getLeaderUI
  lpos <- getsState $ bpos . getActorBody leader
  mxhairPos <- xhairToPos
  xhair <- getsSession sxhair
  saimMode <- getsSession saimMode
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  bsAll <- getsState $ actorAssocs (const True) lidV
  let -- On the same position, big actors come before projectiles.
      ordPos (_, b) = (chessDist lpos $ bpos b, bpos b, bproj b)
      dbs = sortOn ordPos bsAll
      pickUnderXhair =  -- switch to the actor under xhair, if any
        fromMaybe (-1) $ findIndex ((== mxhairPos) . Just . bpos . snd) dbs
      (pickEnemies, i) = case xhair of
        Just (TEnemy a) | isJust saimMode ->  -- pick next enemy
          (True, 1 + fromMaybe (-1) (findIndex ((== a) . fst) dbs))
        Just (TEnemy a) ->  -- first key press, retarget old enemy
          (True, fromMaybe (-1) $ findIndex ((== a) . fst) dbs)
        Just (TNonEnemy a) | isJust saimMode ->  -- pick next non-enemy
          (False, 1 + fromMaybe (-1) (findIndex ((== a) . fst) dbs))
        Just (TNonEnemy a) ->  -- first key press, retarget old non-enemy
          (False, fromMaybe (-1) $ findIndex ((== a) . fst) dbs)
        _ -> (True, pickUnderXhair)
      (lt, gt) = splitAt i dbs
      isEnemy b = isFoe side fact (bfid b)
                  && not (bproj b)
                  && bhp b > 0
      cond = if pickEnemies then isEnemy else not . isEnemy
      lf = filter (cond . snd) $ gt ++ lt
      sxhair = case lf of
        (a, _) : _ -> Just $ if pickEnemies then TEnemy a else TNonEnemy a
        [] -> xhair  -- no seen foes in sight, stick to last target
  -- Register the chosen enemy, to pick another on next invocation.
  modifySession $ \sess -> sess { saimMode = Just $ AimMode lidV
                                , sxhair }
  doLook

-- * AimItem

aimItemHuman :: MonadClientUI m => m ()
aimItemHuman = do
  lidV <- viewedLevelUI
  leader <- getLeaderUI
  lpos <- getsState $ bpos . getActorBody leader
  mxhairPos <- xhairToPos
  xhair <- getsSession sxhair
  saimMode <- getsSession saimMode
  bsAll <- getsState $ EM.keys . lfloor . (EM.! lidV) . sdungeon
  let ordPos p = (chessDist lpos p, p)
      dbs = sortOn ordPos bsAll
      pickUnderXhair =  -- switch to the item under xhair, if any
        let i = fromMaybe (-1)
                $ findIndex ((== mxhairPos) . Just) dbs
        in splitAt i dbs
      (lt, gt) = case xhair of
        Just (TPoint _ lid pos)
          | isJust saimMode && lid == lidV ->  -- pick next item
            let i = fromMaybe (-1) $ findIndex (== pos) dbs
            in splitAt (i + 1) dbs
        Just (TPoint _ lid pos)
          | lid == lidV ->  -- first key press, retarget old item
            let i = fromMaybe (-1) $ findIndex (== pos) dbs
            in splitAt i dbs
        _ -> pickUnderXhair
      gtlt = gt ++ lt
      sxhair = case gtlt of
        p : _ -> Just $ TPoint TKnown lidV p  -- don't force AI to collect it
        [] -> xhair  -- no items remembered, stick to last target
  -- Register the chosen enemy, to pick another on next invocation.
  modifySession $ \sess -> sess { saimMode = Just $ AimMode lidV
                                , sxhair }
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
      mxhairPos <- xhairToPos
      let xhairPos = fromMaybe lpos mxhairPos
          sxhair = Just $ TPoint TKnown lidK xhairPos
      modifySession $ \sess -> sess { saimMode = Just (AimMode lidK)
                                    , sxhair }
      doLook
      return Nothing

-- * EpsIncr

-- | Tweak the @eps@ parameter of the aiming digital line.
epsIncrHuman :: (MonadClient m, MonadClientUI m) => Bool -> m ()
epsIncrHuman b = do
  saimMode <- getsSession saimMode
  lidV <- viewedLevelUI
  modifySession $ \sess -> sess {saimMode = Just $ AimMode lidV}
  modifyClient $ \cli -> cli {seps = seps cli + if b then 1 else -1}
  invalidateBfsPathAll
  flashAiming
  modifySession $ \sess -> sess {saimMode}

-- Flash the aiming line and path.
flashAiming :: MonadClientUI m => m ()
flashAiming = do
  lidV <- viewedLevelUI
  animate lidV pushAndDelay

-- * XhairUnknown

xhairUnknownHuman :: (MonadClient m, MonadClientUI m) => m MError
xhairUnknownHuman = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  mpos <- closestUnknown leader
  case mpos of
    Nothing -> failMsg "no more unknown spots left"
    Just p -> do
      let sxhair = Just $ TPoint TUnknown (blid b) p
      modifySession $ \sess -> sess {sxhair}
      doLook
      return Nothing

-- * XhairItem

xhairItemHuman :: (MonadClient m, MonadClientUI m) => m MError
xhairItemHuman = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  items <- closestItems leader
  case items of
    [] -> failMsg "no more reachable items remembered or visible"
    _ -> do
      let (_, (p, bag)) = maximumBy (comparing fst) items
          sxhair = Just $ TPoint (TItem bag) (blid b) p
      modifySession $ \sess -> sess {sxhair}
      doLook
      return Nothing

-- * XhairStair

xhairStairHuman :: (MonadClient m, MonadClientUI m) => Bool -> m MError
xhairStairHuman up = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  stairs <- closestTriggers (if up then ViaStairsUp else ViaStairsDown) leader
  case stairs of
    [] -> failMsg $ "no reachable stairs" <+> if up then "up" else "down"
    _ -> do
      let (_, (p, (p0, bag))) = maximumBy (comparing fst) stairs
          sxhair = Just $ TPoint (TEmbed bag p0) (blid b) p
      modifySession $ \sess -> sess {sxhair}
      doLook
      return Nothing

-- * XhairPointerFloor

xhairPointerFloorHuman :: MonadClientUI m => m ()
xhairPointerFloorHuman = do
  saimMode <- getsSession saimMode
  aimPointerFloorHuman
  modifySession $ \sess -> sess {saimMode}

-- * XhairPointerEnemy

xhairPointerEnemyHuman :: MonadClientUI m => m ()
xhairPointerEnemyHuman = do
  saimMode <- getsSession saimMode
  aimPointerEnemyHuman
  modifySession $ \sess -> sess {saimMode}

-- * AimPointerFloor

aimPointerFloorHuman :: MonadClientUI m => m ()
aimPointerFloorHuman = do
  COps{corule=RuleContent{rXmax, rYmax}} <- getsState scops
  lidV <- viewedLevelUI
  -- Not @ScreenContent@, because not drawing here.
  K.PointUI x y <- getsSession spointer
  let (px, py) = (x `div` 2, y - K.mapStartY)
  if px >= 0 && py >= 0 && px < rXmax && py < rYmax
  then do
    oldXhair <- getsSession sxhair
    let sxhair = Just $ TPoint TUnknown lidV $ Point px py
        sxhairMoused = sxhair /= oldXhair
    modifySession $ \sess ->
      sess { saimMode = Just $ AimMode lidV
           , sxhair
           , sxhairMoused }
    doLook
  else stopPlayBack

-- * AimPointerEnemy

aimPointerEnemyHuman :: MonadClientUI m => m ()
aimPointerEnemyHuman = do
  COps{corule=RuleContent{rXmax, rYmax}} <- getsState scops
  lidV <- viewedLevelUI
  -- Not @ScreenContent@, because not drawing here.
  K.PointUI x y <- getsSession spointer
  let (px, py) = (x `div` 2, y - K.mapStartY)
  if px >= 0 && py >= 0 && px < rXmax && py < rYmax
  then do
    bsAll <- getsState $ actorAssocs (const True) lidV
    oldXhair <- getsSession sxhair
    side <- getsClient sside
    fact <- getsState $ (EM.! side) . sfactionD
    let newPos = Point px py
        sxhair =
          -- If many actors, we pick here the first that would be picked
          -- by '*', so that all other projectiles on the tile come next,
          -- when pressing "*", without any intervening actors from other tiles.
          -- This is why we use @actorAssocs@ above instead of @posToAidAssocs@.
          case find (\(_, b) -> bpos b == newPos) bsAll of
            Just (aid, b) -> Just $ if isFoe side fact (bfid b)
                                    then TEnemy aid
                                    else TNonEnemy aid
            Nothing -> Just $ TPoint TUnknown lidV newPos
        sxhairMoused = sxhair /= oldXhair
    modifySession $ \sess ->
      sess { saimMode = Just $ AimMode lidV
           , sxhairMoused
           , sxhair }
    doLook
  else stopPlayBack
