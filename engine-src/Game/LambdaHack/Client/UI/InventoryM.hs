-- | UI of inventory management.
module Game.LambdaHack.Client.UI.InventoryM
  ( Suitability(..), ResultItemDialogMode(..)
  , slotsOfItemDialogMode, getFull, getGroupItem, getStoreItem
  , skillCloseUp, placeCloseUp
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , ItemDialogState(..), accessModeBag, storeItemPrompt, getItem
  , DefItemKey(..), transition, runDefItemKey, inventoryInRightPane
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Either
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.ActorUI
import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Client.UI.EffectDescription
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.HandleHelperM
import           Game.LambdaHack.Client.UI.HumanCmd
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
import           Game.LambdaHack.Common.ClientOptions
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Common.Types
import qualified Game.LambdaHack.Content.ItemKind as IK
import qualified Game.LambdaHack.Content.PlaceKind as PK
import qualified Game.LambdaHack.Definition.Ability as Ability
import qualified Game.LambdaHack.Definition.Color as Color
import           Game.LambdaHack.Definition.Defs

data ItemDialogState = ISuitable | IAll
  deriving (Show, Eq)

data ResultItemDialogMode =
    RStore CStore [ItemId]
  | ROrgans ItemId ItemBag SingleItemSlots
  | ROwned ItemId
  | RSkills SlotChar
  | RLore SLore ItemId ItemBag SingleItemSlots
  | RPlaces SlotChar
  | RModes SlotChar
  deriving Show

accessModeBag :: ActorId -> State -> ItemDialogMode -> ItemBag
accessModeBag leader s (MStore cstore) = let b = getActorBody leader s
                                         in getBodyStoreBag b cstore s
accessModeBag leader s MOrgans = let b = getActorBody leader s
                                 in getBodyStoreBag b COrgan s
accessModeBag leader s MOwned = let fid = bfid $ getActorBody leader s
                                in combinedItems fid s
accessModeBag _ _ MSkills = EM.empty
accessModeBag _ s MLore{} = EM.map (const quantSingle) $ sitemD s
accessModeBag _ _ MPlaces = EM.empty
accessModeBag _ _ MModes = EM.empty

-- This is the only place slots are sorted. As a side-effect,
-- slots in inventories always agree with slots of item lore.
-- Not so for organ menu, because many lore maps point there.
-- Sorting in @updateItemSlot@ would not be enough, because, e.g.,
-- identifying an item should change its slot position.
slotsOfItemDialogMode :: MonadClientUI m => ItemDialogMode -> m SingleItemSlots
slotsOfItemDialogMode cCur = do
  itemToF <- getsState $ flip itemToFull
  ItemSlots itemSlotsPre <- getsSession sslots
  case cCur of
    MOrgans -> do
      let newSlots = EM.adjust (sortSlotMap itemToF) SOrgan
                     $ EM.adjust (sortSlotMap itemToF) STrunk
                     $ EM.adjust (sortSlotMap itemToF) SCondition itemSlotsPre
      modifySession $ \sess -> sess {sslots = ItemSlots newSlots}
      return $! mergeItemSlots itemToF [ newSlots EM.! SOrgan
                                       , newSlots EM.! STrunk
                                       , newSlots EM.! SCondition ]
    MSkills -> return EM.empty
    MPlaces -> return EM.empty
    MModes -> return EM.empty
    _ -> do
      let slore = IA.loreFromMode cCur
          newSlots = EM.adjust (sortSlotMap itemToF) slore itemSlotsPre
      modifySession $ \sess -> sess {sslots = ItemSlots newSlots}
      return $! newSlots EM.! slore

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
  let itemCs = map MStore [CStash, CEqp, CGround]
        -- No @COrgan@, because triggerable organs are rare and,
        -- if really needed, accessible directly from the trigger menu.
      loreCs = map MLore [minBound..maxBound] ++ [MPlaces, MModes]
      allCs = case cInitial of
        MLore{} -> loreCs
        MPlaces -> loreCs
        MModes -> loreCs
        _ -> itemCs ++ [MOwned, MOrgans, MSkills]
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
            _ -> ("see", [])
          ownObject = case cstore of
            CStash -> ["our", MU.Text t]
            _ -> [MU.WownW (MU.Text $ bpronoun bodyUI) $ MU.Text t]
      in makePhrase $
           [ MU.Capitalize $ MU.SubjectVerbSg subject verb
           , nItems, MU.Text tIn ] ++ ownObject ++ onLevel
    MOrgans ->
      makePhrase
        [ MU.Capitalize $ MU.SubjectVerbSg subject "feel"
        , MU.Text tIn
        , MU.WownW (MU.Text $ bpronoun bodyUI) $ MU.Text t ]
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
    MLore slore ->
      makePhrase
        [ MU.Capitalize $ MU.Text $
            if slore == SEmbed
            then "terrain (including crafting recipes)"
            else t ]
    MPlaces ->
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
  , defAction :: m (Either Text ResultItemDialogMode)
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
        let leader2 = fromMaybe (error "UI manipulation killed the pointman")
                                mleader
        transition leader2 psuit prompt promptGeneric permitMulitple
                   cCur2 cRest2 itemDialogState2
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  body <- getsState $ getActorBody leader
  bodyUI <- getsSession $ getActorUI leader
  fact <- getsState $ (EM.! bfid body) . sfactionD
  hs <- partyAfterLeader leader
  bagAll <- getsState $ \s -> accessModeBag leader s cCur
  itemToF <- getsState $ flip itemToFull
  revCmd <- revCmdMap
  mpsuit <- psuit  -- when throwing, this sets eps and checks xhair validity
  psuitFun <- case mpsuit of
    SuitsEverything -> return $ \_ _ _ -> True
    SuitsSomething f -> return f  -- When throwing, this function takes
                                  -- missile range into accout.
  lSlots <- slotsOfItemDialogMode cCur
  let getResult :: [ItemId] -> Either Text ResultItemDialogMode
      getResult iids = Right $ case cCur of
        MStore rstore -> RStore rstore iids
        MOrgans -> case iids of
          [iid] -> ROrgans iid bagAll bagItemSlotsAll
          _ -> error $ "" `showFailure` (cCur, iids)
        MOwned -> case iids of
          [iid] -> ROwned iid
          _ -> error $ "" `showFailure` (cCur, iids)
        MSkills -> error $ "" `showFailure` cCur
        MLore rlore -> case iids of
          [iid] -> RLore rlore iid bagAll bagItemSlotsAll
          _ -> error $ "" `showFailure` (cCur, iids)
        MPlaces ->  error $ "" `showFailure` cCur
        MModes -> error $ "" `showFailure` cCur
      mstore = case cCur of
        MStore store -> Just store
        _ -> Nothing
      filterP iid = psuitFun mstore (itemToF iid)
      bagAllSuit = EM.filterWithKey filterP bagAll
      bagItemSlotsAll = EM.filter (`EM.member` bagAll) lSlots
      bag = EM.fromList $ map (\iid -> (iid, bagAll EM.! iid))
                              (EM.elems bagItemSlotsAll)
      suitableItemSlotsAll = EM.filter (`EM.member` bagAllSuit) lSlots
      bagSuit = EM.fromList $ map (\iid -> (iid, bagAllSuit EM.! iid))
                                  (EM.elems suitableItemSlotsAll)
      nextContainers direction = case direction of
        Forward -> case cRest ++ [cCur] of
          c1 : rest -> (c1, rest)
          [] -> error $ "" `showFailure` cRest
        Backward -> case reverse $ cCur : cRest of
          c1 : rest -> (c1, reverse rest)
          [] -> error $ "" `showFailure` cRest
  (bagFiltered, promptChosen) <- getsState $ \s ->
    case itemDialogState of
      ISuitable -> (bagSuit, prompt body bodyUI actorCurAndMaxSk cCur s <> ":")
      IAll -> (bag, promptGeneric body bodyUI actorCurAndMaxSk cCur s <> ":")
  let (autoDun, _) = autoDungeonLevel fact
      multipleSlots = if itemDialogState == IAll
                      then bagItemSlotsAll
                      else suitableItemSlotsAll
      maySwitchLeader MOwned = False
      maySwitchLeader MLore{} = False
      maySwitchLeader MPlaces = False
      maySwitchLeader MModes = False
      maySwitchLeader _ = True
      cycleKeyDef direction =
        let km = revCmd $ PointmanCycle direction
        in (km, DefItemKey
               { defLabel = if direction == Forward then Right km else Left ""
               , defCond = maySwitchLeader cCur && not (autoDun || null hs)
               , defAction = do
                   err <- pointmanCycle leader False direction
                   let !_A = assert (isNothing err `blame` err) ()
                   recCall cCur cRest itemDialogState
               })
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
      keyDefs :: [(K.KM, DefItemKey m)]
      keyDefs = filter (defCond . snd) $
        [ let km = K.mkChar '<'
          in (km, changeContainerDef Backward $ Right km)
        , let km = K.mkChar '>'
          in (km, changeContainerDef Forward $ Right km)
        , let km = K.mkChar '+'
          in (km, DefItemKey
           { defLabel = Right km
           , defCond = bag /= bagSuit
           , defAction = recCall cCur cRest $ case itemDialogState of
                                                ISuitable -> IAll
                                                IAll -> ISuitable
           })
        , let km = K.mkChar '*'
          in (km, useMultipleDef $ Right km)
        , let km = K.mkChar '!'
          in (km, useMultipleDef $ Left "")  -- alias close to 'g'
        , cycleKeyDef Forward
        , cycleKeyDef Backward
        , cycleLevelKeyDef Forward
        , cycleLevelKeyDef Backward
        , (K.KM K.NoModifier K.LeftButtonRelease, DefItemKey
           { defLabel = Left ""
           , defCond = maySwitchLeader cCur && not (null hs)
           , defAction = do
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
      changeContainerDef direction defLabel =
        let (cCurAfterCalm, cRestAfterCalm) = nextContainers direction
        in DefItemKey
          { defLabel
          , defCond = cCurAfterCalm /= cCur
          , defAction = recCall cCurAfterCalm cRestAfterCalm itemDialogState
          }
      useMultipleDef defLabel = DefItemKey
        { defLabel
        , defCond = permitMulitple && not (EM.null multipleSlots)
        , defAction = let eslots = EM.elems multipleSlots
                      in return $! getResult eslots
        }
      slotDef :: SlotChar -> m (Either Text ResultItemDialogMode)
      slotDef slot = case EM.lookup slot bagItemSlotsAll of
        Nothing -> error $ "unexpected slot"
                           `showFailure` (slot, bagItemSlotsAll)
        Just iid -> return $! getResult [iid]
      processSpecialOverlay :: OKX -> (SlotChar -> ResultItemDialogMode)
                            -> m (Either Text ResultItemDialogMode)
      processSpecialOverlay io resultConstructor = do
        let slotDef2 :: SlotChar -> m (Either Text ResultItemDialogMode)
            slotDef2 = return . Right . resultConstructor
        runDefItemKey leader lSlots bagFiltered keyDefs slotDef2 io
                      promptChosen cCur
  case cCur of
    MSkills -> do
      io <- skillsOverlay leader
      processSpecialOverlay io RSkills
    MPlaces -> do
      io <- placesOverlay
      processSpecialOverlay io RPlaces
    MModes -> do
      io <- modesOverlay
      processSpecialOverlay io RModes
    _ -> do
      let displayRanged =
            cCur `notElem` [MStore COrgan, MOrgans, MLore SOrgan, MLore STrunk]
      io <- itemOverlay lSlots (blid body) bagFiltered displayRanged
      runDefItemKey leader lSlots bagFiltered keyDefs slotDef io
                    promptChosen cCur

runDefItemKey :: MonadClientUI m
              => ActorId
              -> SingleItemSlots
              -> ItemBag
              -> [(K.KM, DefItemKey m)]
              -> (SlotChar -> m (Either Text ResultItemDialogMode))
              -> OKX
              -> Text
              -> ItemDialogMode
              -> m (Either Text ResultItemDialogMode)
runDefItemKey leader lSlots bag keyDefs slotDef okx prompt cCur = do
  let itemKeys = map fst keyDefs
      wrapB s = "[" <> s <> "]"
      (keyLabelsRaw, keys) = partitionEithers $ map (defLabel . snd) keyDefs
      keyLabels = filter (not . T.null) keyLabelsRaw
      choice = T.intercalate " " $ map wrapB $ nub keyLabels
        -- switch to Data.Containers.ListUtils.nubOrd when we drop GHC 8.4.4
  msgAdd MsgPromptGeneric $ prompt <+> choice
  CCUI{coscreen=ScreenContent{rheight}} <- getsSession sccui
  ekm <- do
    sli <- overlayToSlideshow (rheight - 2) keys okx
    displayChoiceScreenWithRightPane
      (inventoryInRightPane leader lSlots bag cCur) True
      (show cCur) ColorFull False sli itemKeys
  case ekm of
    Left km -> case km `lookup` keyDefs of
      Just keyDef -> defAction keyDef
      Nothing -> error $ "unexpected key:" `showFailure` K.showKM km
    Right slot -> slotDef slot

inventoryInRightPane :: MonadClientUI m
                     => ActorId -> SingleItemSlots -> ItemBag -> ItemDialogMode
                     -> KeyOrSlot
                     -> m OKX
inventoryInRightPane leader lSlots bag c ekm = case ekm of
  Left{} -> return emptyOKX
  Right slot -> do
    CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
    FontSetup{..} <- getFontSetup
    let -- Lower width, to permit extra vertical space at the start,
        -- because gameover menu prompts are sometimes wide and/or long.
       width = rwidth - 2
    case c of
      _ | isSquareFont propFont -> return emptyOKX
      MSkills -> do
        (prompt, attrString) <- skillCloseUp leader slot
        let promptAS | T.null prompt = []
                     | otherwise = textFgToAS Color.Brown $ prompt <> "\n\n"
            ov = EM.singleton propFont $ offsetOverlay
                                       $ splitAttrString width width
                                       $ promptAS ++ attrString
        return (ov, [])
      MPlaces -> do
        COps{coplace} <- getsState scops
        soptions <- getsClient soptions
        -- This is very slow when many places are exposed,
        -- because this is computed once per place menu keypress.
        -- Fortunately, the mode after entering a place and with pressing
        -- up and down arrow keys is not quadratic, so should be used instead,
        -- particularly with @sexposePlaces@.
        places <- getsState $ EM.assocs
                              . placesFromState coplace (sexposePlaces soptions)
        (prompt, blurbs) <-
          placeCloseUp places (sexposePlaces soptions) slot
        let promptAS | T.null prompt = []
                     | otherwise = textFgToAS Color.Brown $ prompt <> "\n\n"
            ov = attrLinesToFontMap
                 $ map (second $ concatMap (splitAttrString width width))
                 $ (propFont, [promptAS]) : map (second $ map textToAS) blurbs
        return (ov, [])
      MModes -> return emptyOKX
        -- modes cover the right part of screen, so let's keep it empty
      _ -> do
        let ix0 = fromMaybe (error $ show slot)
                            (elemIndex slot $ EM.keys lSlots)
            promptFun _iid _itemFull _k = ""
              -- TODO, e.g., if the party still owns any copies, if the actor
              -- was ever killed by us or killed ours, etc.
              -- This can be the same prompt or longer than what entering
              -- the item screen shows.
        -- Some prop fonts are wider than mono (e.g., in dejavuBold font set),
        -- so the width in these artificial texts full of digits and strange
        -- characters needs to be smaller than @rwidth - 2@ that would suffice
        -- for mono.
        let widthAt = width - 5
        okxItemLorePointedAt propFont widthAt True bag 0 promptFun ix0 lSlots

skillCloseUp :: MonadClientUI m => ActorId -> SlotChar -> m (Text, AttrString)
skillCloseUp leader slot = do
  b <- getsState $ getActorBody leader
  bUI <- getsSession $ getActorUI leader
  actorCurAndMaxSk <- getsState $ getActorMaxSkills leader
  let skill = skillSlots !! slotPrefix slot
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
             -> SlotChar
             -> m (Text, [(DisplayFont, [Text])])
placeCloseUp places sexposePlaces slot = do
  COps{coplace} <- getsState scops
  FontSetup{..} <- getFontSetup
  let (pk, (es, ne, na, _)) = places !! slotPrefix slot
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
      -- Ideally, place layout would be in SquareFont and the rest
      -- in PropFont, but this is mostly a debug screen, so KISS.
      blurbs = [(propFont, partsSentence)]
               ++ [(monoFont, [freqsText, "\n"]) | sexposePlaces]
               ++ [(squareFont, PK.ptopLeft pkind ++ ["\n"]) | sexposePlaces]
               ++ [(propFont, onLevels)]
  return (prompt, blurbs)
