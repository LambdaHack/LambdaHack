-- | UI of inventory management.
module Game.LambdaHack.Client.UI.InventoryM
  ( Suitability(..), ResultItemDialogMode(..)
  , getFull, getGroupItem, getStoreItem
  , skillCloseUp, placeCloseUp, factionCloseUp
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , ItemDialogState(..), accessModeBag, storeItemPrompt, getItem
  , DefItemKey(..), transition
  , runDefMessage, runDefAction, runDefSkills, skillsInRightPane
  , runDefPlaces, placesInRightPane
  , runDefFactions, factionsInRightPane
  , runDefModes, runDefInventory
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Either
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Function
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.ActorUI
import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Client.UI.EffectDescription
import           Game.LambdaHack.Client.UI.HandleHelperM
import           Game.LambdaHack.Client.UI.HumanCmd
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
import           Game.LambdaHack.Common.ClientOptions
import           Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Faction as Faction
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Common.Types
import qualified Game.LambdaHack.Content.FactionKind as FK
import qualified Game.LambdaHack.Content.ItemKind as IK
import qualified Game.LambdaHack.Content.PlaceKind as PK
import qualified Game.LambdaHack.Definition.Ability as Ability
import qualified Game.LambdaHack.Definition.Color as Color
import           Game.LambdaHack.Definition.Defs

data ItemDialogState = ISuitable | IAll
  deriving (Show, Eq)

data ResultItemDialogMode =
    RStore CStore [ItemId]
  | ROwned ItemId
  | RLore SLore MenuSlot [(ItemId, ItemQuant)]
  | RSkills MenuSlot
  | RPlaces MenuSlot
  | RFactions MenuSlot
  | RModes MenuSlot
  deriving Show

accessModeBag :: ActorId -> State -> ItemDialogMode -> ItemBag
accessModeBag leader s (MStore cstore) = let b = getActorBody leader s
                                         in getBodyStoreBag b cstore s
accessModeBag leader s MOwned = let fid = bfid $ getActorBody leader s
                                in combinedItems fid s
accessModeBag _ _ MSkills = EM.empty
accessModeBag leader s (MLore SBody) = let b = getActorBody leader s
                                       in getBodyStoreBag b COrgan s
accessModeBag _ s MLore{} = EM.map (const quantSingle) $ sitemD s
accessModeBag _ _ MPlaces = EM.empty
accessModeBag _ _ MFactions = EM.empty
accessModeBag _ _ MModes = EM.empty

-- | Let a human player choose any item from a given group.
-- Note that this does not guarantee the chosen item belongs to the group,
-- as the player can override the choice.
-- Used e.g., for applying and projecting.
getGroupItem :: MonadClientUI m
             => ActorId
             -> m Suitability
                          -- ^ which items to consider suitable
             -> Text      -- ^ specific prompt for only suitable items
             -> Text      -- ^ generic prompt
             -> Text      -- ^ the verb to use
             -> Text      -- ^ the generic verb to use
             -> [CStore]  -- ^ stores to cycle through
             -> m (Either Text (CStore, ItemId))
getGroupItem leader psuit prompt promptGeneric verb verbGeneric stores = do
  side <- getsClient sside
  mstash <- getsState $ \s -> gstash $ sfactionD s EM.! side
  let ppItemDialogBody v body actorSk cCur = case cCur of
        MStore CEqp | not $ calmEnough body actorSk ->
          "distractedly attempt to" <+> v <+> ppItemDialogModeIn cCur
        MStore CGround | mstash == Just (blid body, bpos body) ->
          "greedily attempt to" <+> v <+> ppItemDialogModeIn cCur
        _ -> v <+> ppItemDialogModeFrom cCur
  soc <- getFull leader psuit
                 (\body _ actorSk cCur _ ->
                    prompt <+> ppItemDialogBody verb body actorSk cCur)
                 (\body _ actorSk cCur _ ->
                    promptGeneric
                    <+> ppItemDialogBody verbGeneric body actorSk cCur)
                 stores True False
  case soc of
    Left err -> return $ Left err
    Right (rstore, [(iid, _)]) -> return $ Right (rstore, iid)
    Right _ -> error $ "" `showFailure` soc

-- | Display all items from a store and let the human player choose any
-- or switch to any other store.
-- Used, e.g., for viewing inventory and item descriptions.
getStoreItem :: MonadClientUI m
             => ActorId         -- ^ the pointman
             -> ItemDialogMode  -- ^ initial mode
             -> m (Either Text ResultItemDialogMode)
getStoreItem leader cInitial = do
  side <- getsClient sside
  let -- No @COrgan@, because triggerable organs are rare and,
      -- if really needed, accessible directly from the trigger menu.
      itemCs = map MStore [CStash, CEqp, CGround]
      -- This should match, including order, the items in standardKeysAndMouse
      -- marked with CmdDashboard up to @MSkills@.
      leaderCs = itemCs ++ [MOwned, MLore SBody, MSkills]
      -- No @SBody@, because repeated in other lores and included elsewhere.
      itemLoreCs = map MLore [minBound..SEmbed]
      -- This should match, including order, the items in standardKeysAndMouse
      -- marked with CmdDashboard past @MSkills@ and up to @MModes@.
      loreCs = itemLoreCs ++ [MPlaces, MFactions, MModes]
  let !_A1 = assert (null (leaderCs `intersect` loreCs)) ()
      !_A2 = assert (sort (leaderCs ++ loreCs ++ [MStore COrgan])
                     == map MStore [minBound..maxBound]
                        ++ [MOwned, MSkills]
                        ++ map MLore [minBound..maxBound]
                        ++ [MPlaces, MFactions, MModes]) ()
      allCs | cInitial `elem` leaderCs = leaderCs
            | cInitial `elem` loreCs = loreCs
            | otherwise = assert (cInitial == MStore COrgan) leaderCs
                            -- werrd content, but let it be
      (pre, rest) = break (== cInitial) allCs
      post = dropWhile (== cInitial) rest
      remCs = post ++ pre
      prompt = storeItemPrompt side
  getItem leader (return SuitsEverything) prompt prompt cInitial remCs
          True False

storeItemPrompt :: FactionId
                -> Actor -> ActorUI -> Ability.Skills -> ItemDialogMode -> State
                -> Text
storeItemPrompt side body bodyUI actorCurAndMaxSk c2 s =
  let COps{coitem} = scops s
      fact = sfactionD s EM.! side
      (tIn, t) = ppItemDialogMode c2
      subject = partActor bodyUI
      f (k, _) acc = k + acc
      countItems store = EM.foldr' f 0 $ getBodyStoreBag body store s
  in case c2 of
    MStore CGround ->
      let n = countItems CGround
          nItems = MU.CarAWs n "item"
          verbGround = if gstash fact == Just (blid body, bpos body)
                       then "fondle greedily"
                       else "notice"
      in makePhrase
           [ MU.Capitalize $ MU.SubjectVerbSg subject verbGround
           , nItems, "at"
           , MU.WownW (MU.Text $ bpronoun bodyUI) $ MU.Text "feet" ]
    MStore CEqp ->
      let n = countItems CEqp
          (verbEqp, nItems) =
            if | n == 0 -> ("find nothing", "")
               | calmEnough body actorCurAndMaxSk ->
                   ("find", MU.CarAWs n "item")
               | otherwise -> ("paw distractedly at", MU.CarAWs n "item")
      in makePhrase
           [ MU.Capitalize $ MU.SubjectVerbSg subject verbEqp
           , nItems, MU.Text tIn
           , MU.WownW (MU.Text $ bpronoun bodyUI) $ MU.Text t ]
    MStore cstore ->
      let n = countItems cstore
          nItems = MU.CarAWs n "item"
          (verb, onLevel) = case cstore of
            COrgan -> ("feel", [])
            CStash ->
              ( "notice"
              , case gstash fact of
                  Just (lid, _) ->
                    map MU.Text ["on level", tshow $ abs $ fromEnum lid]
                  Nothing -> [] )
          ownObject = case cstore of
            CStash -> ["our", MU.Text t]
            _ -> [MU.WownW (MU.Text $ bpronoun bodyUI) $ MU.Text t]
      in makePhrase $
           [ MU.Capitalize $ MU.SubjectVerbSg subject verb
           , nItems, MU.Text tIn ] ++ ownObject ++ onLevel
    MOwned ->
      -- We assume "gold grain", not "grain" with label "of gold":
      let currencyName = IK.iname $ okind coitem
                         $ ouniqGroup coitem IK.S_CURRENCY
          dungeonTotal = sgold s
          (_, total) = calculateTotal side s
      in T.init $ spoilsBlurb currencyName total dungeonTotal
        -- no space for more, e.g., the pointman, but it can't be changed anyway
    MSkills ->
      makePhrase
        [ MU.Capitalize $ MU.SubjectVerbSg subject "estimate"
        , MU.WownW (MU.Text $ bpronoun bodyUI) $ MU.Text t ]
    MLore SBody ->
      makePhrase
        [ MU.Capitalize $ MU.SubjectVerbSg subject "feel"
        , MU.Text tIn
        , MU.WownW (MU.Text $ bpronoun bodyUI) $ MU.Text t ]
    MLore slore ->
      makePhrase
        [ MU.Capitalize $ MU.Text $
            if slore == SEmbed
            then "terrain (including crafting recipes)"
            else t ]
    MPlaces ->
      makePhrase
        [ MU.Capitalize $ MU.Text t ]
    MFactions ->
      makePhrase
        [ MU.Capitalize $ MU.Text t ]
    MModes ->
      makePhrase
        [ MU.Capitalize $ MU.Text t ]

-- | Let the human player choose a single, preferably suitable,
-- item from a list of items. Don't display stores empty for all actors.
-- Start with a non-empty store.
getFull :: MonadClientUI m
        => ActorId
        -> m Suitability    -- ^ which items to consider suitable
        -> (Actor -> ActorUI -> Ability.Skills -> ItemDialogMode -> State
            -> Text)        -- ^ specific prompt for only suitable items
        -> (Actor -> ActorUI -> Ability.Skills -> ItemDialogMode -> State
            -> Text)        -- ^ generic prompt
        -> [CStore]         -- ^ stores to cycle through
        -> Bool             -- ^ whether to ask, when the only item
                            --   in the starting mode is suitable
        -> Bool             -- ^ whether to permit multiple items as a result
        -> m (Either Text (CStore, [(ItemId, ItemQuant)]))
getFull leader psuit prompt promptGeneric stores askWhenLone permitMulitple = do
  mpsuit <- psuit
  let psuitFun = case mpsuit of
        SuitsEverything -> \_ _ _ -> True
        SuitsSomething f -> f
  -- Move the first store that is non-empty for suitable items for this actor
  -- to the front, if any.
  b <- getsState $ getActorBody leader
  getCStoreBag <- getsState $ \s cstore -> getBodyStoreBag b cstore s
  let hasThisActor = not . EM.null . getCStoreBag
  case filter hasThisActor stores of
    [] -> do
      let dialogModes = map MStore stores
          ts = map (MU.Text . ppItemDialogModeIn) dialogModes
      return $ Left $ "no items" <+> makePhrase [MU.WWxW "nor" ts]
    haveThis@(headThisActor : _) -> do
      itemToF <- getsState $ flip itemToFull
      let suitsThisActor store =
            let bag = getCStoreBag store
            in any (\(iid, kit) -> psuitFun (Just store) (itemToF iid) kit)
                   (EM.assocs bag)
          firstStore = fromMaybe headThisActor $ find suitsThisActor haveThis
          -- Don't display stores totally empty for all actors.
          breakStores cInit =
            let (pre, rest) = break (== cInit) stores
                post = dropWhile (== cInit) rest
            in (MStore cInit, map MStore $ post ++ pre)
          (modeFirst, modeRest) = breakStores firstStore
      res <- getItem leader psuit prompt promptGeneric modeFirst modeRest
                     askWhenLone permitMulitple
      case res of
        Left t -> return $ Left t
        Right (RStore fromCStore iids) -> do
          let bagAll = getCStoreBag fromCStore
              f iid = (iid, bagAll EM.! iid)
          return $ Right (fromCStore, map f iids)
        Right _ -> error $ "" `showFailure` res

-- | Let the human player choose a single, preferably suitable,
-- item from a list of items.
getItem :: MonadClientUI m
        => ActorId
        -> m Suitability    -- ^ which items to consider suitable
        -> (Actor -> ActorUI -> Ability.Skills -> ItemDialogMode -> State
            -> Text)        -- ^ specific prompt for only suitable items
        -> (Actor -> ActorUI -> Ability.Skills -> ItemDialogMode -> State
            -> Text)        -- ^ generic prompt
        -> ItemDialogMode   -- ^ first mode to display
        -> [ItemDialogMode] -- ^ the (rest of) modes
        -> Bool             -- ^ whether to ask, when the only item
                            --   in the starting mode is suitable
        -> Bool             -- ^ whether to permit multiple items as a result
        -> m (Either Text ResultItemDialogMode)
getItem leader psuit prompt promptGeneric cCur cRest askWhenLone
        permitMulitple = do
  accessCBag <- getsState $ accessModeBag leader
  let storeAssocs = EM.assocs . accessCBag
      allAssocs = concatMap storeAssocs (cCur : cRest)
  case (allAssocs, cCur) of
    ([(iid, _)], MStore rstore) | null cRest && not askWhenLone ->
      return $ Right $ RStore rstore [iid]
    _ -> transition leader psuit prompt promptGeneric permitMulitple
                    cCur cRest ISuitable

data DefItemKey m = DefItemKey
  { defLabel  :: Either Text K.KM
  , defCond   :: Bool
  , defAction :: ~(m (Either Text ResultItemDialogMode))
      -- this field may be expensive or undefined when @defCond@ is false
  }

data Suitability =
    SuitsEverything
  | SuitsSomething (Maybe CStore -> ItemFull -> ItemQuant -> Bool)

transition :: forall m. MonadClientUI m
           => ActorId
           -> m Suitability
           -> (Actor -> ActorUI -> Ability.Skills -> ItemDialogMode -> State
               -> Text)
           -> (Actor -> ActorUI -> Ability.Skills -> ItemDialogMode -> State
               -> Text)
           -> Bool
           -> ItemDialogMode
           -> [ItemDialogMode]
           -> ItemDialogState
           -> m (Either Text ResultItemDialogMode)
transition leader psuit prompt promptGeneric permitMulitple
           cCur cRest itemDialogState = do
  let recCall cCur2 cRest2 itemDialogState2 = do
        -- Pointman could have been changed by keypresses near the end of
        -- the current recursive call, so refresh it for the next call.
        mleader <- getsClient sleader
        -- When run inside a test, without mleader, assume leader not changed.
        let leader2 = fromMaybe leader mleader
        transition leader2 psuit prompt promptGeneric permitMulitple
                   cCur2 cRest2 itemDialogState2
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  body <- getsState $ getActorBody leader
  bodyUI <- getsSession $ getActorUI leader
  fact <- getsState $ (EM.! bfid body) . sfactionD
  hs <- partyAfterLeader leader
  revCmd <- revCmdMap
  promptChosen <- getsState $ \s -> case itemDialogState of
    ISuitable -> prompt body bodyUI actorCurAndMaxSk cCur s <> ":"
    IAll -> promptGeneric body bodyUI actorCurAndMaxSk cCur s <> ":"
  let keyDefsCommon :: [(K.KM, DefItemKey m)]
      keyDefsCommon = filter (defCond . snd)
        [ let km = K.mkChar '<'
          in (km, changeContainerDef Backward $ Right km)
        , let km = K.mkChar '>'
          in (km, changeContainerDef Forward $ Right km)
        , cycleKeyDef Forward
        , cycleKeyDef Backward
        , cycleLevelKeyDef Forward
        , cycleLevelKeyDef Backward
        , (K.KM K.NoModifier K.LeftButtonRelease, DefItemKey
           { defLabel = Left ""
           , defCond = maySwitchLeader cCur && not (null hs)
           , defAction = do
               -- This is verbose even in aiming mode, displaying
               -- terrain description, but it's fine, mouse may do that.
               merror <- pickLeaderWithPointer leader
               case merror of
                 Nothing -> recCall cCur cRest itemDialogState
                 Just{} -> return $ Left "not a menu item nor teammate position"
                             -- don't inspect the error, it's expected
           })
        , (K.escKM, DefItemKey
           { defLabel = Right K.escKM
           , defCond = True
           , defAction = return $ Left "never mind"
           })
        ]
      cycleLevelKeyDef direction =
        let km = revCmd $ PointmanCycleLevel direction
        in (km, DefItemKey
                { defLabel = Left ""
                , defCond = maySwitchLeader cCur
                            && any (\(_, b, _) -> blid b == blid body) hs
                , defAction = do
                    err <- pointmanCycleLevel leader False direction
                    let !_A = assert (isNothing err `blame` err) ()
                    recCall cCur cRest itemDialogState
                })
      changeContainerDef direction defLabel =
        let (cCurAfterCalm, cRestAfterCalm) = nextContainers direction
        in DefItemKey
          { defLabel
          , defCond = cCurAfterCalm /= cCur
          , defAction = recCall cCurAfterCalm cRestAfterCalm itemDialogState
          }
      nextContainers direction = case direction of
        Forward -> case cRest ++ [cCur] of
          c1 : rest -> (c1, rest)
          [] -> error $ "" `showFailure` cRest
        Backward -> case reverse $ cCur : cRest of
          c1 : rest -> (c1, reverse rest)
          [] -> error $ "" `showFailure` cRest
      banned = bannedPointmanSwitchBetweenLevels fact
      maySwitchLeader MStore{} = True
      maySwitchLeader MOwned = False
      maySwitchLeader MSkills = True
      maySwitchLeader (MLore SBody) = True
      maySwitchLeader MLore{} = False
      maySwitchLeader MPlaces = False
      maySwitchLeader MFactions = False
      maySwitchLeader MModes = False
      cycleKeyDef direction =
        let km = revCmd $ PointmanCycle direction
        in (km, DefItemKey
               { defLabel = if direction == Forward then Right km else Left ""
               , defCond = maySwitchLeader cCur && not (banned || null hs)
               , defAction = do
                   err <- pointmanCycle leader False direction
                   let !_A = assert (isNothing err `blame` err) ()
                   recCall cCur cRest itemDialogState
               })
  case cCur of
    MSkills -> runDefSkills keyDefsCommon promptChosen leader
    MPlaces -> runDefPlaces keyDefsCommon promptChosen
    MFactions -> runDefFactions keyDefsCommon promptChosen
    MModes -> runDefModes keyDefsCommon promptChosen
    _ -> do
      bagHuge <- getsState $ \s -> accessModeBag leader s cCur
      itemToF <- getsState $ flip itemToFull
      mpsuit <- psuit  -- when throwing, this sets eps and checks xhair validity
      psuitFun <- case mpsuit of
        SuitsEverything -> return $ \_ _ _ -> True
        SuitsSomething f -> return f  -- When throwing, this function takes
                                      -- missile range into accout.
      ItemRoles itemRoles <- getsSession sroles
      let slore = loreFromMode cCur
          itemRole = itemRoles EM.! slore
          bagAll = EM.filterWithKey (\iid _ -> iid `ES.member` itemRole) bagHuge
          mstore = case cCur of
            MStore store -> Just store
            _ -> Nothing
          filterP = psuitFun mstore . itemToF
          bagSuit = EM.filterWithKey filterP bagAll
          bagFiltered = case itemDialogState of
            ISuitable -> bagSuit
            IAll -> bagAll
          iids = sortIids itemToF $ EM.assocs bagFiltered
          keyDefsExtra =
            [ let km = K.mkChar '+'
              in (km, DefItemKey
               { defLabel = Right km
               , defCond = bagAll /= bagSuit
               , defAction = recCall cCur cRest $ case itemDialogState of
                                                    ISuitable -> IAll
                                                    IAll -> ISuitable
               })
            , let km = K.mkChar '*'
              in (km, useMultipleDef $ Right km)
            , let km = K.mkChar '!'
              in (km, useMultipleDef $ Left "")  -- alias close to 'g'
            ]
          useMultipleDef defLabel = DefItemKey
            { defLabel
            , defCond = permitMulitple && not (null iids)
            , defAction = case cCur of
                MStore rstore -> return $! Right $ RStore rstore $ map fst iids
                _ -> error "transition: multiple items not for MStore"
            }
          keyDefs = keyDefsCommon ++ filter (defCond . snd) keyDefsExtra
      runDefInventory keyDefs promptChosen leader cCur iids

runDefMessage :: MonadClientUI m
              => [(K.KM, DefItemKey m)]
              -> Text
              -> m ()
runDefMessage keyDefs prompt = do
  let wrapB s = "[" <> s <> "]"
      keyLabelsRaw = lefts $ map (defLabel . snd) keyDefs
      keyLabels = filter (not . T.null) keyLabelsRaw
      choice = T.intercalate " " $ map wrapB $ nub keyLabels
        -- switch to Data.Containers.ListUtils.nubOrd when we drop GHC 8.4.4
  msgAdd MsgPromptGeneric $ prompt <+> choice

runDefAction :: MonadClientUI m
             => [(K.KM, DefItemKey m)]
             -> (MenuSlot -> Either Text ResultItemDialogMode)
             -> KeyOrSlot
             -> m (Either Text ResultItemDialogMode)
runDefAction keyDefs slotDef ekm = case ekm of
  Left km -> case km `lookup` keyDefs of
    Just keyDef -> defAction keyDef
    Nothing -> error $ "unexpected key:" `showFailure` K.showKM km
  Right slot -> return $! slotDef slot

runDefSkills :: MonadClientUI m
             => [(K.KM, DefItemKey m)] -> Text -> ActorId
             -> m (Either Text ResultItemDialogMode)
runDefSkills keyDefsCommon promptChosen leader = do
  CCUI{coscreen=ScreenContent{rheight}} <- getsSession sccui
  runDefMessage keyDefsCommon promptChosen
  let itemKeys = map fst keyDefsCommon
      keys = rights $ map (defLabel . snd) keyDefsCommon
  okx <- skillsOverlay leader
  sli <- overlayToSlideshow (rheight - 2) keys okx
  ekm <- displayChoiceScreenWithDefItemKey
           (skillsInRightPane leader) sli itemKeys (show MSkills)
  runDefAction keyDefsCommon (Right . RSkills) ekm

skillsInRightPane :: MonadClientUI m => ActorId -> Int -> MenuSlot -> m OKX
skillsInRightPane leader width slot = do
  FontSetup{propFont} <- getFontSetup
  (prompt, attrString) <- skillCloseUp leader slot
  let promptAS | T.null prompt = []
               | otherwise = textFgToAS Color.Brown $ prompt <> "\n\n"
      ov = EM.singleton propFont $ offsetOverlay
                                 $ splitAttrString width width
                                 $ promptAS ++ attrString
  return (ov, [])

runDefPlaces :: MonadClientUI m
             => [(K.KM, DefItemKey m)] -> Text
             -> m (Either Text ResultItemDialogMode)
runDefPlaces keyDefsCommon promptChosen = do
  COps{coplace} <- getsState scops
  CCUI{coscreen=ScreenContent{rheight}} <- getsSession sccui
  soptions <- getsClient soptions
  places <- getsState $ EM.assocs
                      . placesFromState coplace (sexposePlaces soptions)
  runDefMessage keyDefsCommon promptChosen
  let itemKeys = map fst keyDefsCommon
      keys = rights $ map (defLabel . snd) keyDefsCommon
  okx <- placesOverlay
  sli <- overlayToSlideshow (rheight - 2) keys okx
  ekm <- displayChoiceScreenWithDefItemKey
           (placesInRightPane places) sli itemKeys (show MPlaces)
  runDefAction keyDefsCommon (Right . RPlaces) ekm

placesInRightPane :: MonadClientUI m
                  => [( ContentId PK.PlaceKind
                      , (ES.EnumSet LevelId, Int, Int, Int) )]
                  -> Int -> MenuSlot
                  -> m OKX
placesInRightPane places width slot = do
  FontSetup{propFont} <- getFontSetup
  soptions <- getsClient soptions
  (prompt, blurbs) <- placeCloseUp places (sexposePlaces soptions) slot
  let promptAS | T.null prompt = []
               | otherwise = textFgToAS Color.Brown $ prompt <> "\n\n"
      splitText = splitAttrString width width
      ov = attrLinesToFontMap
           $ map (second $ concatMap splitText)
           $ (propFont, [promptAS]) : blurbs
  return (ov, [])

runDefFactions :: MonadClientUI m
               => [(K.KM, DefItemKey m)] -> Text
               -> m (Either Text ResultItemDialogMode)
runDefFactions keyDefsCommon promptChosen = do
  CCUI{coscreen=ScreenContent{rheight}} <- getsSession sccui
  sroles <- getsSession sroles
  factions <- getsState $ factionsFromState sroles
  runDefMessage keyDefsCommon promptChosen
  let itemKeys = map fst keyDefsCommon
      keys = rights $ map (defLabel . snd) keyDefsCommon
  okx <- factionsOverlay
  sli <- overlayToSlideshow (rheight - 2) keys okx
  ekm <- displayChoiceScreenWithDefItemKey
           (factionsInRightPane factions)
           sli itemKeys (show MFactions)
  runDefAction keyDefsCommon (Right . RFactions) ekm

factionsInRightPane :: MonadClientUI m
                    => [(FactionId, Faction)]
                    -> Int -> MenuSlot
                    -> m OKX
factionsInRightPane factions width slot = do
  FontSetup{propFont} <- getFontSetup
  (prompt, blurbs) <- factionCloseUp factions slot
  let promptAS | T.null prompt = []
               | otherwise = textFgToAS Color.Brown $ prompt <> "\n\n"
      splitText = splitAttrString width width
      ov = attrLinesToFontMap
           $ map (second $ concatMap splitText)
           $ (propFont, [promptAS]) : blurbs
  return (ov, [])

runDefModes :: MonadClientUI m
            => [(K.KM, DefItemKey m)] -> Text
            -> m (Either Text ResultItemDialogMode)
runDefModes keyDefsCommon promptChosen = do
  CCUI{coscreen=ScreenContent{rheight}} <- getsSession sccui
  runDefMessage keyDefsCommon promptChosen
  let itemKeys = map fst keyDefsCommon
      keys = rights $ map (defLabel . snd) keyDefsCommon
  okx <- modesOverlay
  sli <- overlayToSlideshow (rheight - 2) keys okx
  -- Modes would cover the whole screen, so we don't display in right pane.
  -- But we display and highlight menu bullets.
  ekm <- displayChoiceScreenWithDefItemKey
           (\_ _ -> return emptyOKX) sli itemKeys (show MModes)
  runDefAction keyDefsCommon (Right . RModes) ekm

runDefInventory :: MonadClientUI m
                => [(K.KM, DefItemKey m)]
                -> Text
                -> ActorId
                -> ItemDialogMode
                -> [(ItemId, ItemQuant)]
                -> m (Either Text ResultItemDialogMode)
runDefInventory keyDefs promptChosen leader dmode iids = do
  CCUI{coscreen=ScreenContent{rheight}} <- getsSession sccui
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  let meleeSkill = Ability.getSk Ability.SkHurtMelee actorCurAndMaxSk
      slotDef :: MenuSlot -> Either Text ResultItemDialogMode
      slotDef slot =
        let iid = fst $ iids !! fromEnum slot
        in Right $ case dmode of
          MStore rstore -> RStore rstore [iid]
          MOwned -> ROwned iid
          MLore rlore -> RLore rlore slot iids
          _ -> error $ "" `showFailure` dmode
      promptFun _iid _itemFull _k = ""
        -- TODO, e.g., if the party still owns any copies, if the actor
        -- was ever killed by us or killed ours, etc.
        -- This can be the same prompt or longer than what entering
        -- the item screen shows.
  runDefMessage keyDefs promptChosen
  let itemKeys = map fst keyDefs
      keys = rights $ map (defLabel . snd) keyDefs
  okx <- itemOverlay iids dmode
  sli <- overlayToSlideshow (rheight - 2) keys okx
  ekm <- displayChoiceScreenWithDefItemKey
           (okxItemLoreInline promptFun meleeSkill dmode iids)
           sli itemKeys (show dmode)
  runDefAction keyDefs slotDef ekm

skillCloseUp :: MonadClientUI m => ActorId -> MenuSlot -> m (Text, AttrString)
skillCloseUp leader slot = do
  b <- getsState $ getActorBody leader
  bUI <- getsSession $ getActorUI leader
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  let skill = skillsInDisplayOrder !! fromEnum slot
      valueText = skillToDecorator skill b
                  $ Ability.getSk skill actorCurAndMaxSk
      prompt = makeSentence
        [ MU.WownW (partActor bUI) (MU.Text $ skillName skill)
        , "is", MU.Text valueText ]
      attrString = textToAS $ skillDesc skill
  return (prompt, attrString)

placeCloseUp :: MonadClientUI m
             => [(ContentId PK.PlaceKind, (ES.EnumSet LevelId, Int, Int, Int))]
             -> Bool
             -> MenuSlot
             -> m (Text, [(DisplayFont, [AttrString])])
placeCloseUp places sexposePlaces slot = do
  COps{coplace} <- getsState scops
  FontSetup{..} <- getFontSetup
  let (pk, (es, ne, na, _)) = places !! fromEnum slot
      pkind = okind coplace pk
      prompt = makeSentence ["you remember", MU.Text $ PK.pname pkind]
      freqsText = "Frequencies:" <+> T.intercalate " "
        (map (\(grp, n) -> "(" <> displayGroupName grp
                           <> ", " <> tshow n <> ")")
         $ PK.pfreq pkind)
      onLevels | ES.null es = []
               | otherwise = [makeSentence
                               [ "Appears on"
                               , MU.CarWs (ES.size es) "level" <> ":"
                               , MU.WWandW $ map MU.Car $ sort
                                 $ map (abs . fromEnum) $ ES.elems es ]]
      placeParts = ["it has" | ne > 0 || na > 0]
                   ++ [MU.CarWs ne "entrance" | ne > 0]
                   ++ ["and" | ne > 0 && na > 0]
                   ++ [MU.CarWs na "surrounding" | na > 0]
      partsSentence | null placeParts = []
                    | otherwise = [makeSentence placeParts, "\n"]
      blurbs = [(propFont, partsSentence)]
               ++ [(monoFont, [freqsText, "\n"]) | sexposePlaces]
               ++ [(squareFont, PK.ptopLeft pkind ++ ["\n"]) | sexposePlaces]
               ++ [(propFont, onLevels)]
  return (prompt, map (second $ map textToAS) blurbs)

factionCloseUp :: MonadClientUI m
               => [(FactionId, Faction)]
               -> MenuSlot
               -> m (Text, [(DisplayFont, [AttrString])])
factionCloseUp factions slot = do
  side <- getsClient sside
  FontSetup{propFont} <- getFontSetup
  factionD <- getsState sfactionD
  let (fid, fact@Faction{gkind=FK.FactionKind{..}, ..}) =
        factions !! fromEnum slot
      (name, person) = if fhasGender  -- but we ignore "Controlled", etc.
                       then (makePhrase [MU.Ws $ MU.Text fname], MU.PlEtc)
                       else (fname, MU.Sg3rd)
      (youThey, prompt) =
        if fid == side
        then ("You", makeSentence  ["you are the", MU.Text name])
        else ("They", makeSentence ["you are wary of the", MU.Text name])
               -- wary even if the faction is allied
      ts1 =
        -- Display only the main groups, not to spam.
        case map fst $ filter ((>= 100) . snd) fgroups of
          [] -> []  -- only initial actors in the faction?
          [fgroup] ->
            [makeSentence [ "the faction consists of"
                          , MU.Ws $ MU.Text $ displayGroupName fgroup ]]
          grps -> [makeSentence
                    [ "the faction attracts members such as:"
                    ,  MU.WWandW $ map (MU.Text . displayGroupName) grps ]]
        ++ [if fskillsOther == Ability.zeroSkills  -- simplified
            then youThey <+> "don't care about each other and crowd and stampede all at once, sometimes brutally colliding by accident."
            else youThey <+> "pay attention to each other and take care to move one at a time."]
        ++ [ if fcanEscape
             then "The faction is able to take part in races to an area exit."
             else "The faction doesn't escape areas of conflict and attempts to block exits instead."]
        ++ [ "When all members are incapacitated, the faction dissolves."
           | fneverEmpty ]
        ++ [if fhasGender
            then "Its members are known to have sexual dimorphism and use gender pronouns."
            else "Its members seem to prefer naked ground for sleeping."]
        ++ [ "Its ranks swell with time."
           | fspawnsFast ]
        ++ [ "The faction is able to maintain activity on a level on its own, with a pointman coordinating each tactical maneuver."
           | fhasPointman ]
      -- Changes to all of these have visibility @PosAll@, so the player
      -- knows them fully, except for @gvictims@, which is coupled to tracking
      -- other factions' actors and so only incremented when we've seen
      -- their actor killed (mostly likely killed by us).
      ts2 =  -- reporting regardless of whether any of the factions are dead
        let renderDiplGroup [] = error "renderDiplGroup: null"
            renderDiplGroup ((fid2, diplomacy) : rest) = MU.Phrase
              [ MU.Text $ tshowDiplomacy diplomacy
              , "with"
              , MU.WWandW $ map renderFact2 $ fid2 : map fst rest ]
            renderFact2 fid2 = MU.Text $ Faction.gname (factionD EM.! fid2)
            valid (fid2, diplomacy) = isJust (lookup fid2 factions)
                                      && diplomacy /= Unknown
            knownAssocsGroups = groupBy ((==) `on` snd) $ sortOn snd
                                $ filter valid $ EM.assocs gdipl
        in [ makeSentence [ MU.SubjectVerb person MU.Yes (MU.Text name) "be"
                          , MU.WWandW $ map renderDiplGroup knownAssocsGroups ]
           | not (null knownAssocsGroups) ]
      ts3 =
        case gquit of
          Just Status{..} | not $ isHorrorFact fact ->
            ["The faction has already" <+> FK.nameOutcomePast stOutcome
             <+> "around level" <+> tshow (abs stDepth) <> "."]
          _ -> []
        ++ let nkilled = sum $ EM.elems gvictims
               personKilled = if nkilled == 1 then MU.Sg3rd else MU.PlEtc
           in [ makeSentence $
                  [ "so far," | isNothing gquit ]
                  ++ [ "at least"
                     , MU.CardinalWs nkilled "member"
                     , MU.SubjectVerb personKilled
                                      MU.Yes
                                      "of this faction"
                                      "have been incapacitated" ]
              | nkilled > 0 ]
        ++ let adjective = if isNothing gquit then "current" else "last"
               verb = if isNothing gquit then "is" else "was"
           in ["Its" <+> adjective <+> "doctrine" <+> verb
               <+> "'" <> Ability.nameDoctrine gdoctrine
               <> "' (" <> Ability.describeDoctrine gdoctrine <> ")."]
      -- Description of the score polynomial would go into a separate section,
      -- but it's hard to make it sound non-technical enough.
      blurbs = intersperse ["\n"] $ filter (not . null) [ts1, ts2, ts3]
  return (prompt, map (\t -> (propFont, map textToAS t)) blurbs)
