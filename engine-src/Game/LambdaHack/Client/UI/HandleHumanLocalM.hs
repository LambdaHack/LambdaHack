-- | Semantics of "Game.LambdaHack.Client.UI.HumanCmd"
-- client commands that do not return server requests,,
-- but only change internal client state.
-- None of such commands takes game time.
module Game.LambdaHack.Client.UI.HandleHumanLocalM
  ( -- * Meta commands
    macroHuman, macroHumanTransition
    -- * Local commands
  , chooseItemHuman, chooseItemDialogMode
  , chooseItemProjectHuman, chooseItemApplyHuman
  , psuitReq, triggerSymbols, pickLeaderHuman, pickLeaderWithPointerHuman
  , pointmanCycleHuman, pointmanCycleLevelHuman
  , selectActorHuman, selectNoneHuman, selectWithPointerHuman
  , repeatHuman, repeatHumanTransition
  , repeatLastHuman, repeatLastHumanTransition
  , recordHuman, recordHumanTransition, allHistoryHuman
  , markVisionHuman, markSmellHuman, markSuspectHuman, markAnimHuman
  , overrideTutHuman
  , printScreenHuman
    -- * Commands specific to aiming
  , cancelHuman, acceptHuman, detailCycleHuman
  , clearTargetIfItemClearHuman, itemClearHuman
  , moveXhairHuman, aimTgtHuman, aimFloorHuman, aimEnemyHuman, aimItemHuman
  , aimAscendHuman, epsIncrHuman
  , xhairUnknownHuman, xhairItemHuman, xhairStairHuman
  , xhairPointerFloorHuman, xhairPointerMuteHuman, xhairPointerEnemyHuman
  , aimPointerFloorHuman, aimPointerEnemyHuman
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , chooseItemDialogModeLore, projectCheck
  , posFromXhair, permittedApplyClient, endAiming, endAimingMsg
  , flashAiming
#endif
    -- * Operations both internal and used in unit tests
  , permittedProjectClient, xhairLegalEps
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Either
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Client.BfsM
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
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import           Game.LambdaHack.Client.UI.InventoryM
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.MsgM
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.PointUI
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.Slideshow
import           Game.LambdaHack.Client.UI.SlideshowM
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
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
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.FactionKind as FK
import qualified Game.LambdaHack.Content.ItemKind as IK
import qualified Game.LambdaHack.Content.ModeKind as MK
import           Game.LambdaHack.Content.RuleKind
import qualified Game.LambdaHack.Definition.Ability as Ability
import qualified Game.LambdaHack.Definition.Color as Color
import           Game.LambdaHack.Definition.Defs

-- * Macro

macroHuman :: MonadClientUI m => [String] -> m ()
macroHuman ks = do
  modifySession $ \sess ->
    let kms = K.mkKM <$> ks
        (smacroFrameNew, smacroStackMew) =
           macroHumanTransition kms (smacroFrame sess) (smacroStack sess)
    in sess { smacroFrame = smacroFrameNew
            , smacroStack = smacroStackMew }
  msgAdd MsgMacroOperation $ "Macro activated:" <+> T.pack (unwords ks)

-- | Push a new macro frame to the stack whenever repeating a macro.
macroHumanTransition :: [K.KM] -> KeyMacroFrame -> [KeyMacroFrame]
                     -> (KeyMacroFrame, [KeyMacroFrame])
macroHumanTransition kms macroFrame macroFrames =
  let smacroFrameNew = emptyMacroFrame {keyPending = KeyMacro kms}
  in (smacroFrameNew, macroFrame : macroFrames)

-- * ChooseItem

-- | Display items from a given container store and possibly let the user
-- chose one.
chooseItemHuman :: MonadClientUI m => ActorId -> ItemDialogMode -> m MError
chooseItemHuman leader c =
  either Just (const Nothing) <$> chooseItemDialogMode leader False c

chooseItemDialogModeLore :: forall m . MonadClientUI m
                         => m (Maybe ResultItemDialogMode)
chooseItemDialogModeLore = do
  schosenLoreOld <- getsSession schosenLore
  (inhabitants, embeds) <- case schosenLoreOld of
    ChosenLore inh emb -> return (inh, emb)
    ChosenNothing -> computeChosenLore
  bagHuge <- getsState $ EM.map (const quantSingle) . sitemD
  itemToF <- getsState $ flip itemToFull
  ItemRoles itemRoles <- getsSession sroles
  let rlore :: ItemId -> SLore -> ChosenLore -> m (Maybe ResultItemDialogMode)
      rlore iid slore schosenLore = do
        let itemRole = itemRoles EM.! slore
            bagAll = EM.filterWithKey (\iid2 _ -> iid2 `ES.member` itemRole)
                                      bagHuge
        modifySession $ \sess -> sess {schosenLore}
        let iids = sortIids itemToF $ EM.assocs bagAll
            slot = toEnum $ fromMaybe (error $ "" `showFailure` (iid, iids))
                          $ elemIndex iid $ map fst iids
        return $ Just $ RLore slore slot iids
  case inhabitants of
    (_, b) : rest -> do
      let iid = btrunk b
      arItem <- getsState $ aspectRecordFromIid iid
      let slore | not $ bproj b = STrunk
                | IA.checkFlag Ability.Blast arItem = SBlast
                | otherwise = SItem
      rlore iid slore (ChosenLore rest embeds)
    [] ->
      case embeds of
        (iid, _) : rest -> do
          let slore = SEmbed
          rlore iid slore (ChosenLore inhabitants rest)
        [] -> do
          modifySession $ \sess -> sess {schosenLore = ChosenNothing}
          return Nothing

chooseItemDialogMode :: forall m. MonadClientUI m
                     => ActorId -> Bool -> ItemDialogMode
                     -> m (FailOrCmd ActorId)
chooseItemDialogMode leader0 permitLoreCycle c = do
  CCUI{coscreen=ScreenContent{rwidth, rheight}} <- getsSession sccui
  FontSetup{propFont} <- getFontSetup
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  (ggi, loreFound) <- do
    mggiLore <- if permitLoreCycle && c == MLore SItem
                then chooseItemDialogModeLore
                else return Nothing
    case mggiLore of
      Just rlore -> return (Right rlore, True)
      Nothing -> do
        ggi <- getStoreItem leader0 c
        return (ggi, False)
  -- Pointman could have been changed in @getStoreItem@ above.
  mleader <- getsClient sleader
  -- When run inside a test, without mleader, assume leader not changed.
  let leader = fromMaybe leader0 mleader
  recordHistory  -- item chosen, wipe out already shown msgs
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  let meleeSkill = Ability.getSk Ability.SkHurtMelee actorCurAndMaxSk
  bUI <- getsSession $ getActorUI leader
  case ggi of
    Right result -> case result of
      RStore fromCStore [iid] -> do
        modifySession $ \sess ->
          sess {sitemSel = Just (iid, fromCStore, False)}
        return $ Right leader
      RStore{} -> error $ "" `showFailure` result
      ROwned iid -> do
        found <- getsState $ findIid leader side iid
        let (newAid, bestStore) = case leader `lookup` found of
              Just (_, store) -> (leader, store)
              Nothing -> case found of
                (aid, (_, store)) : _ -> (aid, store)
                [] -> error $ "" `showFailure` result
        modifySession $ \sess ->
          sess {sitemSel = Just (iid, bestStore, False)}
        arena <- getArenaUI
        b2 <- getsState $ getActorBody newAid
        let banned = bannedPointmanSwitchBetweenLevels fact
        if | newAid == leader -> return $ Right leader
           | blid b2 /= arena && banned ->
             failSer NoChangeDunLeader
           | otherwise -> do
             -- We switch leader only here, not when processing results
             -- of lore screens, because lore is only about inspecting items.
             --
             -- This is a bit too verbose in aiming mode, but verbosity
             -- here is good to turn player's attention to the switch.
             void $ pickLeader True newAid
             return $ Right newAid
      RLore slore slot iids -> do
        let promptFun _ itemFull _ = case slore of
              SBody ->
                let blurb = if IA.checkFlag Ability.Condition
                               $ aspectRecordFull itemFull
                            then "condition"
                            else "organ"
                in makeSentence [partActor bUI, "is aware of" ,MU.AW blurb]
              _ ->
                makeSentence [ MU.SubjectVerbSg (partActor bUI) "remember"
                             , MU.AW $ MU.Text (headingSLore slore) ]
        schosenLore <- getsSession schosenLore
        let lorePending = loreFound && case schosenLore of
              ChosenLore [] [] -> False
              _ -> True
            renderOneItem =
              okxItemLoreMsg promptFun meleeSkill (MLore slore) iids
            extraKeys = [K.mkChar '~' | lorePending]
            slotBound = length iids - 1
        km <- displayOneMenuItem renderOneItem extraKeys slotBound slot
        case K.key km of
          K.Space -> do
            modifySession $ \sess -> sess {schosenLore = ChosenNothing}
            chooseItemDialogMode leader False (MLore slore)
          K.Char '~' -> chooseItemDialogMode leader True c
          K.Esc -> do
            modifySession $ \sess -> sess {schosenLore = ChosenNothing}
            failWith "never mind"
          _ -> error $ "" `showFailure` km
      RSkills slot0 -> do
        -- This can be used in the future, e.g., to increase stats from
        -- level-up stat points, so let's keep it even if it shows
        -- no extra info compared to right pane display in menu.
        let renderOneItem slot = do
              (prompt2, attrString) <- skillCloseUp leader slot
              let ov0 = EM.singleton propFont
                        $ offsetOverlay
                        $ splitAttrString rwidth rwidth attrString
              msgAdd MsgPromptGeneric prompt2
              return (ov0, [])
            extraKeys = []
            slotBound = length skillsInDisplayOrder - 1
        km <- displayOneMenuItem renderOneItem extraKeys slotBound slot0
        case K.key km of
          K.Space -> chooseItemDialogMode leader False MSkills
          K.Esc -> failWith "never mind"
          _ -> error $ "" `showFailure` km
      RPlaces slot0 -> do
        COps{coplace} <- getsState scops
        soptions <- getsClient soptions
        -- This is computed just once for the whole series of up and down arrow
        -- navigations, avoid quadratic blowup.
        places <- getsState $ EM.assocs
                              . placesFromState coplace (sexposePlaces soptions)
        let renderOneItem slot = do
              (prompt2, blurbs) <-
                placeCloseUp places (sexposePlaces soptions) slot
              let splitText = splitAttrString rwidth rwidth
                  ov0 = attrLinesToFontMap
                        $ map (second $ concatMap splitText) blurbs
              msgAdd MsgPromptGeneric prompt2
              return (ov0, [])
            extraKeys = []
            slotBound = length places - 1
        km <- displayOneMenuItem renderOneItem extraKeys slotBound slot0
        case K.key km of
          K.Space -> chooseItemDialogMode leader False MPlaces
          K.Esc -> failWith "never mind"
          _ -> error $ "" `showFailure` km
      RFactions slot0 -> do
        sroles <- getsSession sroles
        factions <- getsState $ factionsFromState sroles
        let renderOneItem slot = do
              (prompt2, blurbs) <- factionCloseUp factions slot
              let splitText = splitAttrString rwidth rwidth
                  ov0 = attrLinesToFontMap
                        $ map (second $ concatMap splitText) blurbs
              msgAdd MsgPromptGeneric prompt2
              return (ov0, [])
            extraKeys = []
            slotBound = length factions - 1
        km <- displayOneMenuItem renderOneItem extraKeys slotBound slot0
        case K.key km of
          K.Space -> chooseItemDialogMode leader False MFactions
          K.Esc -> failWith "never mind"
          _ -> error $ "" `showFailure` km
      RModes slot0 -> do
        let displayOneMenuItemBig :: (MenuSlot -> m OKX)
                                  -> [K.KM] -> Int -> MenuSlot
                                  -> m K.KM
            displayOneMenuItemBig renderOneItem extraKeys slotBound slot = do
              let keys = [K.spaceKM, K.escKM]
                         ++ [K.upKM | fromEnum slot > 0]
                         ++ [K.downKM | fromEnum slot < slotBound]
                         ++ extraKeys
              okx <- renderOneItem slot
              -- Here it differs from @displayOneMenuItem@,
              slides <- overlayToSlideshow rheight keys okx
              ekm2 <- displayChoiceScreen "" ColorFull True slides keys
              let km = either id (error $ "" `showFailure` ekm2) ekm2
              -- Here it stops differing.
              case K.key km of
                K.Up -> displayOneMenuItemBig renderOneItem extraKeys
                                              slotBound $ pred slot
                K.Down -> displayOneMenuItemBig renderOneItem extraKeys
                                                slotBound $ succ slot
                _ -> return km
        COps{comode} <- getsState scops
        svictories <- getsSession svictories
        nxtChal <- getsClient snxtChal
          -- mark victories only for current difficulty
        let f !acc _p !i !a = (i, a) : acc
            campaignModes = ofoldlGroup' comode MK.CAMPAIGN_SCENARIO f []
            renderOneItem slot = do
              let (gameModeId, gameMode) = campaignModes !! fromEnum slot
              ov0 <- describeMode False gameModeId
              let victories = case EM.lookup gameModeId svictories of
                    Nothing -> 0
                    Just cm -> fromMaybe 0 (M.lookup nxtChal cm)
                  verb = if victories > 0 then "remember" else "forsee"
                  prompt2 = makeSentence
                    [ MU.SubjectVerbSg "you" verb
                    , MU.Text $ "the '" <> MK.mname gameMode <> "' adventure" ]
              msgAdd MsgPromptGeneric prompt2
              return (ov0, [])
            extraKeys = []
            slotBound = length campaignModes - 1
        km <- displayOneMenuItemBig renderOneItem extraKeys slotBound slot0
        case K.key km of
          K.Space -> chooseItemDialogMode leader False MModes
          K.Esc -> failWith "never mind"
          _ -> error $ "" `showFailure` km
    Left err -> failWith err

-- * ChooseItemProject

chooseItemProjectHuman :: forall m. (MonadClient m, MonadClientUI m)
                       => ActorId -> [HumanCmd.TriggerItem] -> m MError
chooseItemProjectHuman leader ts = do
  b <- getsState $ getActorBody leader
  mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid b
  let overStash = mstash == Just (blid b, bpos b)
      storesBase = [CStash, CEqp]
      stores | overStash = storesBase ++ [CGround]
             | otherwise = CGround : storesBase
      (verb1, object1) = case ts of
        [] -> ("aim", "item")
        tr : _ -> (HumanCmd.tiverb tr, HumanCmd.tiobject tr)
      verb = makePhrase [verb1]
      triggerSyms = triggerSymbols ts
  mpsuitReq <- psuitReq leader
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
            Just _ | isRight (psuitReqFun itemFull) ->
              -- The player knows what he's doing. We warn him about range
              -- and experimenting with unknown precious items is fine.
              return Nothing
            _ -> do
              modifySession $ \sess -> sess {sitemSel = Nothing}
              chooseItemProjectHuman leader ts
        Nothing -> do
          let psuit =
                return $ SuitsSomething $ \_ itemFull _kit ->
                  -- Here the player does not explicitly pick an item,
                  -- so we may exclude precious unknown items, etc.
                  either (const False) snd (psuitReqFun itemFull)
                  && (null triggerSyms
                      || IK.isymbol (itemKind itemFull) `elem` triggerSyms)
              prompt = makePhrase ["What", object1, "to"]
              promptGeneric = "What to"
          ggi <- getGroupItem leader psuit prompt promptGeneric verb "fling"
                              stores
          case ggi of
            Right (fromCStore, iid) -> do
              modifySession $ \sess ->
                sess {sitemSel = Just (iid, fromCStore, False)}
              return Nothing
            Left err -> failMsg err

permittedProjectClient :: MonadClientUI m
                       => ActorId -> m (ItemFull -> Either ReqFailure Bool)
permittedProjectClient leader = do
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  b <- getsState $ getActorBody leader
  let skill = Ability.getSk Ability.SkProject actorCurAndMaxSk
      calmE = calmEnough b actorCurAndMaxSk
  return $ permittedProject False skill calmE

projectCheck :: MonadClientUI m => ActorId -> Point -> m (Maybe ReqFailure)
projectCheck leader tpos = do
  COps{coTileSpeedup} <- getsState scops
  eps <- getsClient seps
  sb <- getsState $ getActorBody leader
  let lid = blid sb
      spos = bpos sb
  -- Not @ScreenContent@, because not drawing here.
  case bresenhamsLineAlgorithm eps spos tpos of
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
xhairLegalEps :: MonadClientUI m => ActorId -> m (Either Text Int)
xhairLegalEps leader = do
  cops@COps{corule=RuleContent{rWidthMax, rHeightMax}} <- getsState scops
  b <- getsState $ getActorBody leader
  lidV <- viewedLevelUI
  let !_A = assert (lidV == blid b) ()
      findNewEps onlyFirst pos = do
        lvl <- getLevel (blid b)
        oldEps <- getsClient seps
        return $! case makeLine onlyFirst b pos oldEps cops lvl of
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
      let shifted = shiftBounded rWidthMax rHeightMax (bpos b) v
      if shifted == bpos b && v /= Vector 0 0
      then return $ Left "selected translation is void"
      else findNewEps True shifted  -- @True@, because the goal is vague anyway

posFromXhair :: (MonadClient m, MonadClientUI m)
             => ActorId -> m (Either Text Point)
posFromXhair leader = do
  canAim <- xhairLegalEps leader
  case canAim of
    Right newEps -> do
      -- Modify @seps@, permanently.
      modifyClient $ \cli -> cli {seps = newEps}
      mxhairPos <- mxhairToPos
      case mxhairPos of
        Nothing -> error $ "" `showFailure` mxhairPos
        Just pos -> do
          munit <- projectCheck leader pos
          case munit of
            Nothing -> return $ Right pos
            Just reqFail -> return $ Left $ showReqFailure reqFail
    Left cause -> return $ Left cause

-- | On top of `permittedProjectClient`, it also checks legality
-- of aiming at the target and projection range. It also modifies @eps@.
psuitReq :: (MonadClient m, MonadClientUI m)
         => ActorId
         -> m (Either Text (ItemFull -> Either ReqFailure (Point, Bool)))
psuitReq leader = do
  b <- getsState $ getActorBody leader
  lidV <- viewedLevelUI
  if lidV /= blid b
  then return $ Left "can't fling on remote level"
  else do
    mpos <- posFromXhair leader
    p <- permittedProjectClient leader
    case mpos of
      Left err -> return $ Left err
      Right pos -> return $ Right $ \itemFull ->
        case p itemFull of
          Left err -> Left err
          Right False -> Right (pos, False)
          Right True ->
            let arItem = aspectRecordFull itemFull
            in Right (pos, 1 + IA.totalRange arItem (itemKind itemFull)
                           >= chessDist (bpos b) pos)

-- $setup
-- >>> import Game.LambdaHack.Definition.DefsInternal
--

-- |
-- >>> let trigger1 = HumanCmd.TriggerItem{tiverb="verb", tiobject="object", tisymbols=[toContentSymbol 'a', toContentSymbol 'b']}
-- >>> let trigger2 = HumanCmd.TriggerItem{tiverb="verb2", tiobject="object2", tisymbols=[toContentSymbol 'c']}
-- >>> triggerSymbols [trigger1, trigger2]
-- [ContentSymbol 'a',ContentSymbol 'b',ContentSymbol 'c']
--
-- >>> triggerSymbols []
-- []
triggerSymbols :: [HumanCmd.TriggerItem] -> [ContentSymbol IK.ItemKind]
triggerSymbols [] = []
triggerSymbols (HumanCmd.TriggerItem{tisymbols} : ts) =
  tisymbols ++ triggerSymbols ts

-- * ChooseItemApply

chooseItemApplyHuman :: forall m. MonadClientUI m
                     => ActorId -> [HumanCmd.TriggerItem] -> m MError
chooseItemApplyHuman leader ts = do
  b <- getsState $ getActorBody leader
  mstash <- getsState $ \s -> gstash $ sfactionD s EM.! bfid b
  let overStash = mstash == Just (blid b, bpos b)
      storesBase = [CStash, CEqp, COrgan]
      stores | overStash = storesBase ++ [CGround]
             | otherwise = CGround : storesBase
      (verb1, object1) = case ts of
        [] -> ("trigger", "item")
        tr : _ -> (HumanCmd.tiverb tr, HumanCmd.tiobject tr)
      verb = makePhrase [verb1]
      triggerSyms = triggerSymbols ts
      prompt = makePhrase ["What", object1, "to"]
      promptGeneric = "What to"
  itemSel <- getsSession sitemSel
  case itemSel of
    Just (_, _, True) -> return Nothing
    Just (iid, fromCStore, False) -> do
      -- We don't validate vs @ts@ here, because player has selected
      -- this item, so he knows what he's doing (unless really absurd).
      itemFull <- getsState $ itemToFull iid
      bag <- getsState $ getBodyStoreBag b fromCStore
      mp <- permittedApplyClient leader
      case iid `EM.lookup` bag of
        Just kit | fromRight False (mp (Just fromCStore) itemFull kit) ->
          return Nothing
        _ -> do
          modifySession $ \sess -> sess {sitemSel = Nothing}
          chooseItemApplyHuman leader ts
    Nothing -> do
      let psuit :: m Suitability
          psuit = do
            mp <- permittedApplyClient leader
            return $ SuitsSomething $ \cstore itemFull kit ->
              fromRight False (mp cstore itemFull kit)
              && (null triggerSyms
                  || IK.isymbol (itemKind itemFull) `elem` triggerSyms)
      ggi <- getGroupItem leader psuit prompt promptGeneric verb "trigger"
                          stores
      case ggi of
        Right (fromCStore, iid) -> do
          modifySession $ \sess ->
            sess {sitemSel = Just (iid, fromCStore, False)}
          return Nothing
        Left err -> failMsg err

permittedApplyClient :: MonadClientUI m
                     => ActorId
                     -> m (Maybe CStore -> ItemFull -> ItemQuant
                           -> Either ReqFailure Bool)
permittedApplyClient leader = do
  COps{corule} <- getsState scops
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  b <- getsState $ getActorBody leader
  let skill = Ability.getSk Ability.SkApply actorCurAndMaxSk
      calmE = calmEnough b actorCurAndMaxSk
  localTime <- getsState $ getLocalTime (blid b)
  return $ permittedApply corule localTime skill calmE

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
      mchoice = if FK.fhasGender (gkind fact) then mhero else mactor
      banned = bannedPointmanSwitchBetweenLevels fact
  case mchoice of
    Nothing -> failMsg "no such member of the party"
    Just (aid, b)
      | blid b /= arena && banned ->
          failMsg $ showReqFailure NoChangeDunLeader
      | otherwise -> do
          void $ pickLeader True aid
          return Nothing

-- * PickLeaderWithPointer

pickLeaderWithPointerHuman :: MonadClientUI m => ActorId -> m MError
pickLeaderWithPointerHuman = pickLeaderWithPointer

-- * PointmanCycle

-- | Switch current pointman to the next on the viewed level, if any, wrapping.
pointmanCycleLevelHuman :: MonadClientUI m => ActorId -> Direction -> m MError
pointmanCycleLevelHuman leader = pointmanCycleLevel leader True

-- * PointmanBack

-- | Switch current pointman to the previous in the whole dungeon, wrapping.
pointmanCycleHuman :: MonadClientUI m => ActorId -> Direction -> m MError
pointmanCycleHuman leader = pointmanCycle leader True

-- * SelectActor

selectActorHuman :: MonadClientUI m => ActorId -> m ()
selectActorHuman leader = do
  bodyUI <- getsSession $ getActorUI leader
  wasMemeber <- getsSession $ ES.member leader . sselected
  let upd = if wasMemeber
            then ES.delete leader  -- already selected, deselect instead
            else ES.insert leader
  modifySession $ \sess -> sess {sselected = upd $ sselected sess}
  let subject = partActor bodyUI
  msgAdd MsgActionAlert $ makeSentence [subject, if wasMemeber
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
  msgAdd MsgActionAlert $ makeSentence [subject, if wasNone
                                                 then "selected"
                                                 else "deselected"]

-- * SelectWithPointer

selectWithPointerHuman :: MonadClientUI m => m MError
selectWithPointerHuman = do
  COps{corule=RuleContent{rHeightMax}} <- getsState scops
  lidV <- viewedLevelUI
  -- Not @ScreenContent@, because not drawing here.
  side <- getsClient sside
  ours <- getsState $ filter (not . bproj . snd)
                      . actorAssocs (== side) lidV
  sactorUI <- getsSession sactorUI
  let oursUI = map (\(aid, b) -> (aid, b, sactorUI EM.! aid)) ours
      viewed = sortOn keySelected oursUI
  pUI <- getsSession spointer
  let p@(Point px py) = squareToMap $ uiToSquare pUI
  -- Select even if no space in status line for the actor's symbol.
  if | py == rHeightMax + 1 && px == 0 -> selectNoneHuman >> return Nothing
     | py == rHeightMax + 1 ->
         case drop (px - 1) viewed of
           [] -> failMsg "not pointing at an actor"
           (aid, _, _) : _ -> selectActorHuman aid >> return Nothing
     | otherwise ->
         case find (\(_, b) -> bpos b == p) ours of
           Nothing -> failMsg "not pointing at an actor"
           Just (aid, _) -> selectActorHuman aid >> return Nothing

-- * Repeat

-- Note that walk followed by repeat should not be equivalent to run,
-- because the player can really use a command that does not stop
-- at terrain change or when walking over items.
repeatHuman :: MonadClientUI m => Int -> m ()
repeatHuman n =
  modifySession $ \sess ->
    let (smacroFrameNew, smacroStackMew) =
           repeatHumanTransition n (smacroFrame sess) (smacroStack sess)
    in sess { smacroFrame = smacroFrameNew
            , smacroStack = smacroStackMew }

repeatHumanTransition :: Int -> KeyMacroFrame -> [KeyMacroFrame]
                      -> (KeyMacroFrame, [KeyMacroFrame])
repeatHumanTransition n macroFrame macroFrames =
  let kms = concat . replicate n . unKeyMacro . fromRight mempty
            $ keyMacroBuffer macroFrame
  in macroHumanTransition kms macroFrame macroFrames

-- * RepeatLast

-- Note that walk followed by repeat should not be equivalent to run,
-- because the player can really use a command that does not stop
-- at terrain change or when walking over items.
repeatLastHuman :: MonadClientUI m => Int -> m ()
repeatLastHuman n = modifySession $ \sess ->
  sess {smacroFrame = repeatLastHumanTransition n (smacroFrame sess) }

repeatLastHumanTransition :: Int -> KeyMacroFrame -> KeyMacroFrame
repeatLastHumanTransition n macroFrame =
  let macro = KeyMacro . concat . replicate n . maybeToList $ keyLast macroFrame
  in macroFrame { keyPending = macro <> keyPending macroFrame }

-- * Record

-- | Starts and stops recording of macros.
recordHuman :: MonadClientUI m => m ()
recordHuman = do
  smacroFrameOld <- getsSession smacroFrame
  let (smacroFrameNew, msg) = recordHumanTransition smacroFrameOld
  modifySession $ \sess -> sess {smacroFrame = smacroFrameNew}
  macroStack <- getsSession smacroStack
  unless (T.null msg || not (null macroStack)) $ msgAdd MsgPromptGeneric msg

recordHumanTransition :: KeyMacroFrame -> (KeyMacroFrame, Text)
recordHumanTransition macroFrame =
  let (buffer, msg) = case keyMacroBuffer macroFrame of
        Right _ ->
          -- Start recording in-game macro.
          (Left [], "Recording a macro. Stop recording with the same key.")
        Left xs ->
          -- Stop recording in-game macro.
          (Right . KeyMacro . reverse $ xs, "Macro recording stopped.")
      smacroFrameNew = macroFrame {keyMacroBuffer = buffer}
  in (smacroFrameNew, msg)

-- * AllHistory

allHistoryHuman :: forall m. MonadClientUI m => m ()
allHistoryHuman = do
  CCUI{coscreen=ScreenContent{rwidth, rheight}} <- getsSession sccui
  history <- getsSession shistory
  arena <- getArenaUI
  localTime <- getsState $ getLocalTime arena
  global <- getsState stime
  FontSetup{..} <- getFontSetup
  let renderedHistoryRaw = renderHistory history
      histLenRaw = length renderedHistoryRaw
      placeholderLine = textFgToAS Color.BrBlack
        "Newest_messages_are_at_the_bottom._Press_END_to_get_there."
      placeholderCount =
        (- histLenRaw `mod` (rheight - 4)) `mod` (rheight - 4)
      renderedHistory = replicate placeholderCount placeholderLine
                        ++ renderedHistoryRaw
      histLen = placeholderCount + histLenRaw
      splitRow as =
        let (tLab, tDesc) = span (/= Color.spaceAttrW32) as
            labLen = textSize monoFont tLab
            par1 = case filter (/= emptyAttrLine) $ linesAttr tDesc of
              [] -> emptyAttrLine
              [l] -> l
              ls -> attrStringToAL $ intercalate [Color.spaceAttrW32]
                                   $ map attrLine ls
        in (attrStringToAL tLab, (labLen, par1))
      (tsLab, tsDesc) = unzip $ map splitRow renderedHistory
      ovs = EM.insertWith (++) monoFont (offsetOverlay tsLab)
            $ EM.singleton propFont $ offsetOverlayX tsDesc
      turnsGlobal = global `timeFitUp` timeTurn
      turnsLocal = localTime `timeFitUp` timeTurn
      msg = makeSentence
        [ "You survived for"
        , MU.CarWs turnsGlobal "half-second turn"
        , "(this level:"
        , MU.Car turnsLocal <> ")" ]
      kxs = [ (Right sn, ( PointUI 0 (fromEnum sn)
                         , ButtonWidth propFont 1000 ))
            | sn <- take histLen natSlots ]
  msgAdd MsgPromptGeneric msg
  let keysAllHistory =
        K.returnKM
#ifndef USE_JSFILE
        : K.mkChar '.'
#endif
        : [K.spaceKM, K.escKM]
  slides <- overlayToSlideshow (rheight - 2) keysAllHistory (ovs, kxs)
  let historyLines = case reverse $ concatMap snd $ slideshow slides of
        (Left{}, _) : rest -> rest  -- don't count the @--more--@ line
        l -> l
      maxIx = length historyLines - 1 - length keysAllHistory
      menuName = "history"
  modifySession $ \sess ->
    sess {smenuIxMap = M.insert menuName maxIx $ smenuIxMap sess}
  let displayAllHistory = do
        ekm <- displayChoiceScreen menuName ColorFull False slides
                                   keysAllHistory
        case ekm of
          Left km | km == K.mkChar '.' -> do
            let t = T.unlines $ map (T.pack . map Color.charFromW32)
                                    renderedHistoryRaw
            path <- dumpTextFile t "history.txt"
            msgAdd MsgPromptGeneric $ "All of history dumped to file" <+> T.pack path <> "."
          Left km | km == K.escKM ->
            msgAdd MsgPromptGeneric "Try to survive a few seconds more, if you can."
          Left km | km == K.spaceKM ->
            msgAdd MsgPromptGeneric "Steady on."
          Left km | km == K.returnKM ->
            msgAdd MsgPromptGeneric "Press RET when history message selected to see it in full."
          Right slot ->
            displayOneReport $ toEnum $ max 0 $ fromEnum slot - placeholderCount
          _ -> error $ "" `showFailure` ekm
      displayOneReport :: MenuSlot -> m ()
      displayOneReport slot0 = do
        let renderOneItem slot = do
              let timeReport = case drop (fromEnum slot)
                                         renderedHistoryRaw of
                    [] -> error $ "" `showFailure` slot
                    tR : _ -> tR
                  markParagraph c | Color.charFromW32 c == '\n' = [c, c]
                  markParagraph c = [c]
                  reportWithParagraphs = concatMap markParagraph timeReport
                  (ovLab, ovDesc) =
                    labDescOverlay monoFont rwidth reportWithParagraphs
                  ov0 = EM.insertWith (++) monoFont ovLab
                        $ EM.singleton propFont ovDesc
                  prompt = makeSentence
                    [ "the", MU.Ordinal $ fromEnum slot + 1
                    , "most recent record follows" ]
              msgAdd MsgPromptGeneric prompt
              return (ov0, [])
            extraKeys = []
            slotBound = histLenRaw - 1
        km <- displayOneMenuItem renderOneItem extraKeys slotBound slot0
        case K.key km of
          K.Space -> displayAllHistory
          K.Esc -> msgAdd MsgPromptGeneric
                          "Try to learn from your previous mistakes."
          _ -> error $ "" `showFailure` km
  displayAllHistory

-- * MarkVision

markVisionHuman :: MonadClientUI m => Int -> m ()
markVisionHuman delta = modifySession $ cycleMarkVision delta

-- * MarkSmell

markSmellHuman :: MonadClientUI m => m ()
markSmellHuman = modifySession toggleMarkSmell

-- * MarkSuspect

markSuspectHuman :: MonadClient m => Int -> m ()
markSuspectHuman delta = do
  -- @condBFS@ depends on the setting we change here.
  invalidateBfsAll
  modifyClient (cycleMarkSuspect delta)

-- * MarkAnim

markAnimHuman :: MonadClient m => m ()
markAnimHuman = do
  noAnim <- getsClient $ fromMaybe False . snoAnim . soptions
  modifyClient $ \cli ->
    cli {soptions = (soptions cli) {snoAnim = Just $ not noAnim}}

-- * OverrideTut

overrideTutHuman :: MonadClientUI m => Int -> m ()
overrideTutHuman delta = modifySession $ cycleOverrideTut delta

-- * PrintScreen

printScreenHuman :: MonadClientUI m => m ()
printScreenHuman = do
  msgAdd MsgActionAlert "Screenshot printed."
  printScreen

-- * Cancel

-- | End aiming mode, rejecting the current position, unless when on
-- remote level, in which case, return to our level.
cancelHuman :: MonadClientUI m => m ()
cancelHuman = do
  maimMode <- getsSession saimMode
  case maimMode of
    Just aimMode -> do
      let lidV = aimLevelId aimMode
      lidOur <- getArenaUI
      if lidV == lidOur
      then clearAimMode
      else do
        xhairPos <- xhairToPos
        let sxhair = Just $ TPoint TKnown lidOur xhairPos
        modifySession $ \sess ->
          sess {saimMode = Just aimMode {aimLevelId = lidOur}}
        setXHairFromGUI sxhair
        doLook
    Nothing -> return ()

-- * Accept

-- | Accept the current crosshair position as target, ending
-- aiming mode, if active.
acceptHuman :: (MonadClient m, MonadClientUI m) => ActorId -> m ()
acceptHuman leader = do
  endAiming leader
  endAimingMsg leader
  clearAimMode

-- | End aiming mode, accepting the current position.
endAiming :: (MonadClient m, MonadClientUI m) => ActorId -> m ()
endAiming leader = do
  sxhair <- getsSession sxhair
  modifyClient $ updateTarget leader $ const sxhair

endAimingMsg :: MonadClientUI m => ActorId -> m ()
endAimingMsg leader = do
  subject <- partActorLeader leader
  tgt <- getsClient $ getTarget leader
  (mtargetMsg, _) <- targetDesc tgt
  msgAdd MsgActionAlert $ case mtargetMsg of
    Nothing ->
      makeSentence [MU.SubjectVerbSg subject "clear target"]
    Just targetMsg ->
      makeSentence [MU.SubjectVerbSg subject "target", MU.Text targetMsg]

-- * DetailCycle

-- | Cycle detail level of aiming mode descriptions, starting up.
detailCycleHuman :: MonadClientUI m => m ()
detailCycleHuman = do
  modifySession $ \sess -> sess {saimMode =
    (\aimMode -> aimMode {detailLevel = detailCycle $ detailLevel aimMode})
                 <$> saimMode sess}
  doLook

detailCycle :: DetailLevel -> DetailLevel
detailCycle detail = if detail == minBound then maxBound else pred detail

-- * ClearTargetIfItemClear

clearTargetIfItemClearHuman :: (MonadClient m, MonadClientUI m)
                            => ActorId -> m ()
clearTargetIfItemClearHuman leader = do
  itemSel <- getsSession sitemSel
  when (isNothing itemSel) $ do
    setXHairFromGUI Nothing
    modifyClient $ updateTarget leader (const Nothing)
    doLook

-- * ItemClear

itemClearHuman :: MonadClientUI m => m ()
itemClearHuman = modifySession $ \sess -> sess {sitemSel = Nothing}

-- * MoveXhair

-- | Move the xhair. Assumes aiming mode.
moveXhairHuman :: MonadClientUI m => Vector -> Int -> m MError
moveXhairHuman dir n = do
  -- Not @ScreenContent@, because not drawing here.
  COps{corule=RuleContent{rWidthMax, rHeightMax}} <- getsState scops
  saimMode <- getsSession saimMode
  let lidV = maybe (error $ "" `showFailure` (dir, n)) aimLevelId saimMode
  xhair <- getsSession sxhair
  xhairPos <- xhairToPos
  let shiftB pos = shiftBounded rWidthMax rHeightMax pos dir
      newPos = iterate shiftB xhairPos !! n
  if newPos == xhairPos then failMsg "never mind"
  else do
    mleader <- getsClient sleader
    sxhair <- case (xhair, mleader) of
     (Just TVector{}, Just leader) -> do
       lpos <- getsState $ bpos . getActorBody leader
       return $ Just $ TVector $ newPos `vectorToFrom` lpos
     _ -> return $ Just $ TPoint TKnown lidV newPos
    setXHairFromGUI sxhair
    doLook
    return Nothing

-- * AimTgt

-- | Start aiming.
aimTgtHuman :: MonadClientUI m => m ()
aimTgtHuman = do
  -- (Re)start aiming at the current level.
  lidV <- viewedLevelUI
  modifySession $ \sess -> sess {saimMode =
    let newDetail = maybe defaultDetailLevel detailLevel (saimMode sess)
    in Just $ AimMode lidV newDetail}
  doLook
  msgAdd MsgPromptAction "*flinging started; press again to project*"

-- * AimFloor

-- | Cycle aiming mode. Do not change position of the xhair,
-- switch target to point at different things at that position.
aimFloorHuman :: MonadClientUI m => m ()
aimFloorHuman = do
  lidV <- viewedLevelUI
  mleader <- getsClient sleader
  mlpos <- case mleader of
    Nothing -> return Nothing
    Just leader -> getsState $ Just . bpos . getActorBody leader
  xhairPos <- xhairToPos
  xhair <- getsSession sxhair
  saimMode <- getsSession saimMode
  bsAll <- getsState $ actorAssocs (const True) lidV
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  let sxhair = case xhair of
        _ | isNothing saimMode ->  -- first key press: keep target
          xhair
        Just TEnemy{} -> Just $ TPoint TKnown lidV xhairPos
        Just TNonEnemy{} -> Just $ TPoint TKnown lidV xhairPos
        Just TPoint{} | Just lpos <- mlpos, xhairPos /= lpos ->
          Just $ TVector $ xhairPos `vectorToFrom` lpos
        Just TVector{} ->
          -- If many actors, we pick here the first that would be picked
          -- by '*', so that all other projectiles on the tile come next,
          -- when pressing '*', without any intervening actors from other tiles.
          -- This is why we use @actorAssocs@ above instead of @posToAidAssocs@.
          case find (\(_, b) -> bpos b == xhairPos) bsAll of
            Just (aid, b) -> Just $ if isFoe side fact (bfid b)
                                    then TEnemy aid
                                    else TNonEnemy aid
            Nothing -> Just $ TPoint TUnknown lidV xhairPos
        _ -> xhair
  modifySession $ \sess -> sess {saimMode =
    let newDetail = maybe defaultDetailLevel detailLevel saimMode
    in Just $ AimMode lidV newDetail}
  setXHairFromGUI sxhair
  doLook

-- * AimEnemy

aimEnemyHuman :: MonadClientUI m => m ()
aimEnemyHuman = do
  lidV <- viewedLevelUI
  mleader <- getsClient sleader
  mlpos <- case mleader of
    Nothing -> return Nothing
    Just leader -> getsState $ Just . bpos . getActorBody leader
  mxhairPos <- mxhairToPos
  xhair <- getsSession sxhair
  saimMode <- getsSession saimMode
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  bsAll <- getsState $ actorAssocs (const True) lidV
  let -- On the same position, big actors come before projectiles.
      ordPos lpos (_, b) = (chessDist lpos $ bpos b, bpos b, bproj b)
      dbs = case mlpos of
        Nothing -> bsAll
        Just lpos -> sortOn (ordPos lpos) bsAll
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
  modifySession $ \sess -> sess {saimMode =
    let newDetail = maybe defaultDetailLevel detailLevel saimMode
    in Just $ AimMode lidV newDetail}
  setXHairFromGUI sxhair
  doLook

-- * AimItem

aimItemHuman :: MonadClientUI m => m ()
aimItemHuman = do
  side <- getsClient sside
  lidV <- viewedLevelUI
  mleader <- getsClient sleader
  mlpos <- case mleader of
    Nothing -> return Nothing
    Just leader -> getsState $ Just . bpos . getActorBody leader
  mxhairPos <- mxhairToPos
  xhair <- getsSession sxhair
  saimMode <- getsSession saimMode
  Level{lfloor} <- getLevel lidV
  mstash <- getsState $ \s -> gstash $ sfactionD s EM.! side
  -- Don't consider own stash an ordinary pile of items.
  let lfloorBarStash = case mstash of
        Just (lid, pos) | lid == lidV -> EM.delete pos lfloor
        _ -> lfloor
      bsAll = EM.keys lfloorBarStash
      ordPos lpos p = (chessDist lpos p, p)
      dbs = case mlpos of
        Nothing -> bsAll
        Just lpos -> sortOn (ordPos lpos) bsAll
      pickUnderXhair =  -- switch to the item under xhair, if any
        let i = fromMaybe (-1)
                $ findIndex ((== mxhairPos) . Just) dbs
        in splitAt i dbs
      (lt, gt) = case xhair of
        Just (TPoint _ lid pos)
          | isJust saimMode && lid == lidV ->  -- pick next item
            let i = fromMaybe (-1) $ elemIndex pos dbs
            in splitAt (i + 1) dbs
        Just (TPoint _ lid pos)
          | lid == lidV ->  -- first key press, retarget old item
            let i = fromMaybe (-1) $ elemIndex pos dbs
            in splitAt i dbs
        _ -> pickUnderXhair
      gtlt = gt ++ lt
      sxhair = case gtlt of
        p : _ -> Just $ TPoint TKnown lidV p  -- don't force AI to collect it
        [] -> xhair  -- no items remembered, stick to last target
  -- Register the chosen enemy, to pick another on next invocation.
  modifySession $ \sess -> sess {saimMode =
    let newDetail = maybe defaultDetailLevel detailLevel saimMode
    in Just $ AimMode lidV newDetail}
  setXHairFromGUI sxhair
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
      xhairPos <- xhairToPos
      let sxhair = Just $ TPoint TKnown lidK xhairPos
      modifySession $ \sess -> sess {saimMode =
        let newDetail = maybe defaultDetailLevel detailLevel (saimMode sess)
        in Just $ AimMode lidK newDetail}
      setXHairFromGUI sxhair
      doLook
      return Nothing

-- * EpsIncr

-- | Tweak the @eps@ parameter of the aiming digital line.
epsIncrHuman :: (MonadClient m, MonadClientUI m) => Direction -> m ()
epsIncrHuman d = do
  -- Perform the change:
  let sepsDelta = case d of
        Forward -> 1
        Backward -> -1
  modifyClient $ \cli -> cli {seps = seps cli + sepsDelta}
  invalidateBfsPathAll
  -- Provide UI feedback:
  -- Hack @sreportNull@ to display the new line even if no earlier messages.
  modifySession $ \sess -> sess {sreportNull = False}
  saimMode <- getsSession saimMode
  lidV <- viewedLevelUI
  modifySession $ \sess -> sess {saimMode =
    let newDetail = maybe DetailLow detailLevel saimMode
    in Just $ AimMode lidV newDetail}
  flashAiming
  modifySession $ \sess -> sess {saimMode}
  -- The change may not affect the line shape, hence 'possibly'.
  msgAdd MsgPromptAction "Aiming line (possibly) modified."

-- Flash the aiming line and path.
flashAiming :: MonadClientUI m => m ()
flashAiming = do
  lidV <- viewedLevelUI
  animate lidV pushAndDelay

-- * XhairUnknown

xhairUnknownHuman :: (MonadClient m, MonadClientUI m) => ActorId -> m MError
xhairUnknownHuman leader = do
  b <- getsState $ getActorBody leader
  mpos <- closestUnknown leader
  case mpos of
    Nothing -> failMsg "no more unknown spots left"
    Just p -> do
      let sxhair = Just $ TPoint TUnknown (blid b) p
      setXHairFromGUI sxhair
      doLook
      return Nothing

-- * XhairItem

xhairItemHuman :: (MonadClient m, MonadClientUI m) => ActorId -> m MError
xhairItemHuman leader = do
  b <- getsState $ getActorBody leader
  items <- closestItems leader
  case items of
    [] -> failMsg "no more reachable items remembered or visible"
    _ -> do
      let (_, (p, bag)) = maximumBy (comparing fst) items
          sxhair = Just $ TPoint (TItem bag) (blid b) p
      setXHairFromGUI sxhair
      doLook
      return Nothing

-- * XhairStair

xhairStairHuman :: (MonadClient m, MonadClientUI m)
                => ActorId -> Bool -> m MError
xhairStairHuman leader up = do
  b <- getsState $ getActorBody leader
  stairs <- closestTriggers (if up then ViaStairsUp else ViaStairsDown) leader
  case stairs of
    [] -> failMsg $ "no reachable stairs" <+> if up then "up" else "down"
    _ -> do
      let (_, (p, (p0, bag))) = maximumBy (comparing fst) stairs
          sxhair = Just $ TPoint (TEmbed bag p0) (blid b) p
      setXHairFromGUI sxhair
      doLook
      return Nothing

-- * XhairPointerFloor

xhairPointerFloorHuman :: MonadClientUI m => m ()
xhairPointerFloorHuman = do
  saimMode <- getsSession saimMode
  aimPointerFloorHuman
  when (isNothing saimMode) $
    modifySession $ \sess -> sess {saimMode}

-- * XhairPointerMute

xhairPointerMuteHuman :: MonadClientUI m => m ()
xhairPointerMuteHuman = do
  saimMode <- getsSession saimMode
  aimPointerFloorLoud False
  when (isNothing saimMode) $
    modifySession $ \sess -> sess {saimMode}

-- * XhairPointerEnemy

xhairPointerEnemyHuman :: MonadClientUI m => m ()
xhairPointerEnemyHuman = do
  saimMode <- getsSession saimMode
  aimPointerEnemyHuman
  when (isNothing saimMode) $
    modifySession $ \sess -> sess {saimMode}

-- * AimPointerFloor

aimPointerFloorHuman :: MonadClientUI m => m ()
aimPointerFloorHuman = aimPointerFloorLoud True

aimPointerFloorLoud :: MonadClientUI m => Bool -> m ()
aimPointerFloorLoud loud = do
  COps{corule=RuleContent{rWidthMax, rHeightMax}} <- getsState scops
  lidV <- viewedLevelUI
  -- Not @ScreenContent@, because not drawing here.
  pUI <- getsSession spointer
  let p = squareToMap $ uiToSquare pUI
  if insideP (0, 0, rWidthMax - 1, rHeightMax - 1) p
  then do
    oldXhair <- getsSession sxhair
    let sxhair = Just $ TPoint TUnknown lidV p
        sxhairMoused = sxhair /= oldXhair
        detailSucc = if sxhairMoused
                     then detailLevel
                     else detailCycle . detailLevel
    modifySession $ \sess ->
      sess { saimMode =
               let newDetail = maybe defaultDetailLevel detailSucc
                                     (saimMode sess)
               in Just $ AimMode lidV newDetail
           , sxhairMoused }
    setXHairFromGUI sxhair
    when loud doLook
  else stopPlayBack

-- * AimPointerEnemy

aimPointerEnemyHuman :: MonadClientUI m => m ()
aimPointerEnemyHuman = do
  COps{corule=RuleContent{rWidthMax, rHeightMax}} <- getsState scops
  lidV <- viewedLevelUI
  -- Not @ScreenContent@, because not drawing here.
  pUI <- getsSession spointer
  let p = squareToMap $ uiToSquare pUI
  if insideP (0, 0, rWidthMax - 1, rHeightMax - 1) p
  then do
    bsAll <- getsState $ actorAssocs (const True) lidV
    oldXhair <- getsSession sxhair
    side <- getsClient sside
    fact <- getsState $ (EM.! side) . sfactionD
    let sxhair =
          -- If many actors, we pick here the first that would be picked
          -- by '*', so that all other projectiles on the tile come next,
          -- when pressing '*', without any intervening actors from other tiles.
          -- This is why we use @actorAssocs@ above instead of @posToAidAssocs@.
          case find (\(_, b) -> bpos b == p) bsAll of
            Just (aid, b) -> Just $ if isFoe side fact (bfid b)
                                    then TEnemy aid
                                    else TNonEnemy aid
            Nothing -> Just $ TPoint TUnknown lidV p
        sxhairMoused = sxhair /= oldXhair
        detailSucc = if sxhairMoused
                     then detailLevel
                     else detailCycle . detailLevel
    modifySession $ \sess ->
      sess { saimMode =
               let newDetail = maybe defaultDetailLevel detailSucc
                                     (saimMode sess)
               in Just $ AimMode lidV newDetail
           , sxhairMoused }
    setXHairFromGUI sxhair
    doLook
  else stopPlayBack
