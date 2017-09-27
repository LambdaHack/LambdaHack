{-# LANGUAGE DataKinds #-}
-- | Inventory management and party cycling.
module Game.LambdaHack.Client.UI.InventoryM
  ( Suitability(..)
  , getFull, getGroupItem, getStoreItem
  , ppItemDialogMode, ppItemDialogModeFrom
#ifdef EXPOSE_INTERNAL
  , storeFromMode
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Char as Char
import Data.Either
import qualified Data.EnumMap.Strict as EM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Tuple (swap)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.CommonM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.ActorUI
import Game.LambdaHack.Client.UI.HandleHelperM
import Game.LambdaHack.Client.UI.HumanCmd
import Game.LambdaHack.Client.UI.ItemSlot
import qualified Game.LambdaHack.Client.UI.Key as K
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.MsgM
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Client.UI.Slideshow
import Game.LambdaHack.Client.UI.SlideshowM
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State

data ItemDialogState = ISuitable | IAll
  deriving (Show, Eq)

ppItemDialogMode :: ItemDialogMode -> (Text, Text)
ppItemDialogMode (MStore cstore) = ppCStore cstore
ppItemDialogMode MOwned = ("in", "our possession")
ppItemDialogMode MStats = ("among", "strengths")
ppItemDialogMode MLoreItem = ("among", "item lore")
ppItemDialogMode MLoreOrgan = ("among", "organ lore")

ppItemDialogModeIn :: ItemDialogMode -> Text
ppItemDialogModeIn c = let (tIn, t) = ppItemDialogMode c in tIn <+> t

ppItemDialogModeFrom :: ItemDialogMode -> Text
ppItemDialogModeFrom c = let (_tIn, t) = ppItemDialogMode c in "from" <+> t

storeFromMode :: ItemDialogMode -> CStore
storeFromMode c = case c of
  MStore cstore -> cstore
  MOwned -> CGround  -- needed to decide display mode in textAllAE
  MStats -> CGround
  MLoreItem -> CGround
  MLoreOrgan -> COrgan

accessModeBag :: ActorId -> State -> ItemDialogMode -> ItemBag
accessModeBag leader s (MStore cstore) = let b = getActorBody leader s
                                         in getBodyStoreBag b cstore s
accessModeBag leader s MOwned = let fid = bfid $ getActorBody leader s
                                in sharedAllOwnedFid False fid s
accessModeBag _ _ MStats = EM.empty
accessModeBag _ s MLoreItem = EM.map (const (1, [])) $ sitemD s
accessModeBag _ s MLoreOrgan = EM.map (const (1, [])) $ sitemD s

-- | Let a human player choose any item from a given group.
-- Note that this does not guarantee the chosen item belongs to the group,
-- as the player can override the choice.
-- Used e.g., for applying and projecting.
getGroupItem :: MonadClientUI m
             => m Suitability
                          -- ^ which items to consider suitable
             -> Text      -- ^ specific prompt for only suitable items
             -> Text      -- ^ generic prompt
             -> [CStore]  -- ^ initial legal modes
             -> [CStore]  -- ^ legal modes after Calm taken into account
             -> m (Either Text ( (ItemId, ItemFull)
                               , (ItemDialogMode, Either K.KM SlotChar) ))
getGroupItem psuit prompt promptGeneric
             cLegalRaw cLegalAfterCalm = do
  soc <- getFull psuit
                 (\_ _ _ cCur -> prompt <+> ppItemDialogModeFrom cCur)
                 (\_ _ _ cCur -> promptGeneric <+> ppItemDialogModeFrom cCur)
                 cLegalRaw cLegalAfterCalm True False
  case soc of
    Left err -> return $ Left err
    Right ([(iid, itemFull)], cekm) -> return $ Right ((iid, itemFull), cekm)
    Right _ -> error $ "" `showFailure` soc

-- | Display all items from a store and let the human player choose any
-- or switch to any other store.
-- Used, e.g., for viewing inventory and item descriptions.
getStoreItem :: MonadClientUI m
             => (Actor -> ActorUI -> AspectRecord -> ItemDialogMode -> Text)
                                 -- ^ how to describe suitable items
             -> ItemDialogMode   -- ^ initial mode
             -> m ( Either Text (ItemId, ItemFull)
                  , (ItemDialogMode, Either K.KM SlotChar) )
getStoreItem prompt cInitial = do
  let itemCs = map MStore [CInv, CGround, CEqp, CSha]
      allCs | cInitial `elem` [MLoreItem, MLoreOrgan] = [MLoreItem, MLoreOrgan]
            | otherwise = itemCs ++ [MOwned, MStore COrgan, MStats]
      (pre, rest) = break (== cInitial) allCs
      post = dropWhile (== cInitial) rest
      remCs = post ++ pre
  soc <- getItem (return SuitsEverything)
                 prompt prompt cInitial remCs
                 True False (cInitial : remCs)
  case soc of
    (Left err, cekm) -> return (Left err, cekm)
    (Right [(iid, itemFull)], cekm) -> return (Right (iid, itemFull), cekm)
    (Right{}, _) -> error $ "" `showFailure` soc

-- | Let the human player choose a single, preferably suitable,
-- item from a list of items. Don't display stores empty for all actors.
-- Start with a non-empty store.
getFull :: MonadClientUI m
        => m Suitability    -- ^ which items to consider suitable
        -> (Actor -> ActorUI -> AspectRecord -> ItemDialogMode -> Text)
                            -- ^ specific prompt for only suitable items
        -> (Actor -> ActorUI -> AspectRecord -> ItemDialogMode -> Text)
                            -- ^ generic prompt
        -> [CStore]         -- ^ initial legal modes
        -> [CStore]         -- ^ legal modes with Calm taken into account
        -> Bool             -- ^ whether to ask, when the only item
                            --   in the starting mode is suitable
        -> Bool             -- ^ whether to permit multiple items as a result
        -> m (Either Text ( [(ItemId, ItemFull)]
                          , (ItemDialogMode, Either K.KM SlotChar) ))
getFull psuit prompt promptGeneric cLegalRaw cLegalAfterCalm
        askWhenLone permitMulitple = do
  side <- getsClient sside
  leader <- getLeaderUI
  let aidNotEmpty store aid = do
        body <- getsState $ getActorBody aid
        bag <- getsState $ getBodyStoreBag body store
        return $! not $ EM.null bag
      partyNotEmpty store = do
        as <- getsState $ fidActorNotProjAssocs side
        bs <- mapM (aidNotEmpty store . fst) as
        return $! or bs
  mpsuit <- psuit
  let psuitFun = case mpsuit of
        SuitsEverything -> const True
        SuitsSomething f -> f
  -- Move the first store that is non-empty for suitable items for this actor
  -- to the front, if any.
  b <- getsState $ getActorBody leader
  getCStoreBag <- getsState $ \s cstore -> getBodyStoreBag b cstore s
  let hasThisActor = not . EM.null . getCStoreBag
  case filter hasThisActor cLegalAfterCalm of
    [] ->
      if isNothing (find hasThisActor cLegalRaw) then do
        let contLegalRaw = map MStore cLegalRaw
            tLegal = map (MU.Text . ppItemDialogModeIn) contLegalRaw
            ppLegal = makePhrase [MU.WWxW "nor" tLegal]
        return $ Left $ "no items" <+> ppLegal
      else return $ Left $ showReqFailure ItemNotCalm
    haveThis@(headThisActor : _) -> do
      itemToF <- itemToFullClient
      let suitsThisActor store =
            let bag = getCStoreBag store
            in any (\(iid, kit) -> psuitFun $ itemToF iid kit) $ EM.assocs bag
          firstStore = fromMaybe headThisActor $ find suitsThisActor haveThis
      -- Don't display stores totally empty for all actors.
      cLegal <- filterM partyNotEmpty cLegalRaw
      let breakStores cInit =
            let (pre, rest) = break (== cInit) cLegal
                post = dropWhile (== cInit) rest
            in (MStore cInit, map MStore $ post ++ pre)
      let (modeFirst, modeRest) = breakStores firstStore
      res <- getItem psuit prompt promptGeneric modeFirst modeRest
                     askWhenLone permitMulitple (map MStore cLegal)
      case res of
        (Left x, _) -> return $ Left x
        (Right x, cekm) -> return $ Right (x, cekm)

-- | Let the human player choose a single, preferably suitable,
-- item from a list of items.
getItem :: MonadClientUI m
        => m Suitability
                            -- ^ which items to consider suitable
        -> (Actor -> ActorUI -> AspectRecord -> ItemDialogMode -> Text)
                            -- ^ specific prompt for only suitable items
        -> (Actor -> ActorUI -> AspectRecord -> ItemDialogMode -> Text)
                            -- ^ generic prompt
        -> ItemDialogMode   -- ^ first mode, legal or not
        -> [ItemDialogMode] -- ^ the (rest of) legal modes
        -> Bool             -- ^ whether to ask, when the only item
                            --   in the starting mode is suitable
        -> Bool             -- ^ whether to permit multiple items as a result
        -> [ItemDialogMode] -- ^ all legal modes
        -> m ( Either Text [(ItemId, ItemFull)]
             , (ItemDialogMode, Either K.KM SlotChar) )
getItem psuit prompt promptGeneric cCur cRest askWhenLone permitMulitple
        cLegal = do
  leader <- getLeaderUI
  accessCBag <- getsState $ accessModeBag leader
  let storeAssocs = EM.assocs . accessCBag
      allAssocs = concatMap storeAssocs (cCur : cRest)
  case (cRest, allAssocs) of
    ([], [(iid, k)]) | not askWhenLone -> do
      itemToF <- itemToFullClient
      ItemSlots itemSlots organSlots <- getsSession sslots
      let isOrgan = cCur `elem` [MStore COrgan, MLoreOrgan]
          lSlots = if isOrgan then organSlots else itemSlots
          slotChar = fromMaybe (error $ "" `showFailure` (iid, lSlots))
                     $ lookup iid $ map swap $ EM.assocs lSlots
      return (Right [(iid, itemToF iid k)], (cCur, Right slotChar))
    _ ->
      transition psuit prompt promptGeneric permitMulitple cLegal
                 0 cCur cRest ISuitable

data DefItemKey m = DefItemKey
  { defLabel  :: Either Text K.KM
  , defCond   :: Bool
  , defAction :: Either K.KM SlotChar
                 -> m ( Either Text [(ItemId, ItemFull)]
                      , (ItemDialogMode, Either K.KM SlotChar) )
  }

data Suitability =
    SuitsEverything
  | SuitsSomething (ItemFull -> Bool)

transition :: forall m. MonadClientUI m
           => m Suitability
           -> (Actor -> ActorUI -> AspectRecord -> ItemDialogMode -> Text)
           -> (Actor -> ActorUI -> AspectRecord -> ItemDialogMode -> Text)
           -> Bool
           -> [ItemDialogMode]
           -> Int
           -> ItemDialogMode
           -> [ItemDialogMode]
           -> ItemDialogState
           -> m ( Either Text [(ItemId, ItemFull)]
                , (ItemDialogMode, Either K.KM SlotChar) )
transition psuit prompt promptGeneric permitMulitple cLegal
           numPrefix cCur cRest itemDialogState = do
  let recCall = transition psuit prompt promptGeneric permitMulitple cLegal
  ItemSlots itemSlots organSlots <- getsSession sslots
  leader <- getLeaderUI
  body <- getsState $ getActorBody leader
  bodyUI <- getsSession $ getActorUI leader
  actorAspect <- getsClient sactorAspect
  let ar = fromMaybe (error $ "" `showFailure` leader)
                     (EM.lookup leader actorAspect)
  fact <- getsState $ (EM.! bfid body) . sfactionD
  hs <- partyAfterLeader leader
  bagAll <- getsState $ \s -> accessModeBag leader s cCur
  itemToF <- itemToFullClient
  Binding{brevMap} <- getsSession sbinding
  mpsuit <- psuit  -- when throwing, this sets eps and checks xhair validity
  psuitFun <- case mpsuit of
    SuitsEverything -> return $ const True
    SuitsSomething f -> return f
      -- When throwing, this function takes missile range into accout.
  let getSingleResult :: ItemId -> (ItemId, ItemFull)
      getSingleResult iid = (iid, itemToF iid (bagAll EM.! iid))
      getResult :: Either K.KM SlotChar -> ItemId
                -> ( Either Text [(ItemId, ItemFull)]
                   , (ItemDialogMode, Either K.KM SlotChar) )
      getResult ekm iid = (Right [getSingleResult iid], (cCur, ekm))
      getMultResult :: Either K.KM SlotChar -> [ItemId]
                    -> ( Either Text [(ItemId, ItemFull)]
                       , (ItemDialogMode, Either K.KM SlotChar) )
      getMultResult ekm iids = (Right $ map getSingleResult iids, (cCur, ekm))
      filterP iid kit = psuitFun $ itemToF iid kit
      bagAllSuit = EM.filterWithKey filterP bagAll
      isOrgan = cCur `elem` [MStore COrgan, MLoreOrgan]
      lSlots = if isOrgan then organSlots else itemSlots
      bagItemSlotsAll = EM.filter (`EM.member` bagAll) lSlots
      -- Predicate for slot matching the current prefix, unless the prefix
      -- is 0, in which case we display all slots, even if they require
      -- the user to start with number keys to get to them.
      -- Could be generalized to 1 if prefix 1x exists, etc., but too rare.
      hasPrefixOpen x _ = slotPrefix x == numPrefix || numPrefix == 0
      bagItemSlotsOpen = EM.filterWithKey hasPrefixOpen bagItemSlotsAll
      hasPrefix x _ = slotPrefix x == numPrefix
      bagItemSlots = EM.filterWithKey hasPrefix bagItemSlotsOpen
      bag = EM.fromList $ map (\iid -> (iid, bagAll EM.! iid))
                              (EM.elems bagItemSlotsOpen)
      suitableItemSlotsAll = EM.filter (`EM.member` bagAllSuit) lSlots
      suitableItemSlotsOpen =
        EM.filterWithKey hasPrefixOpen suitableItemSlotsAll
      bagSuit = EM.fromList $ map (\iid -> (iid, bagAllSuit EM.! iid))
                                  (EM.elems suitableItemSlotsOpen)
      (autoDun, _) = autoDungeonLevel fact
      multipleSlots = if itemDialogState == IAll
                      then bagItemSlotsAll
                      else suitableItemSlotsAll
      revCmd dflt cmd = case M.lookup cmd brevMap of
        Nothing -> dflt
        Just (k : _) -> k
        Just [] -> error $ "" `showFailure` brevMap
      keyDefs :: [(K.KM, DefItemKey m)]
      keyDefs = filter (defCond . snd) $
        [ let km = K.mkChar '?'
          in (km, DefItemKey
           { defLabel = Right km
           , defCond = bag /= bagSuit
           , defAction = \_ -> recCall numPrefix cCur cRest
                               $ case itemDialogState of
                                   ISuitable -> IAll
                                   IAll -> ISuitable
           })
        , let km = K.mkChar '/'
          in (km, changeContainerDef $ Right km)
        , (K.mkKP '/', changeContainerDef $ Left "")
        , let km = K.mkChar '!'
          in (km, useMultipleDef $ Right km)
        , (K.mkKP '*', useMultipleDef $ Left "")
        , let km = revCmd (K.KM K.NoModifier K.Tab) MemberCycle
          in (km, DefItemKey
           { defLabel = Right km
           , defCond = not (cCur == MOwned
                            || not (any (\(_, b, _) -> blid b == blid body) hs))
           , defAction = \_ -> do
               err <- memberCycle False
               let !_A = assert (isNothing err `blame` err) ()
               (cCurUpd, cRestUpd) <- legalWithUpdatedLeader cCur cRest
               recCall numPrefix cCurUpd cRestUpd itemDialogState
           })
        , let km = revCmd (K.KM K.NoModifier K.BackTab) MemberBack
          in (km, DefItemKey
           { defLabel = Right km
           , defCond = not (cCur == MOwned || autoDun || null hs)
           , defAction = \_ -> do
               err <- memberBack False
               let !_A = assert (isNothing err `blame` err) ()
               (cCurUpd, cRestUpd) <- legalWithUpdatedLeader cCur cRest
               recCall numPrefix cCurUpd cRestUpd itemDialogState
           })
        , (K.KM K.NoModifier K.LeftButtonRelease, DefItemKey
           { defLabel = Left ""
           , defCond = not (cCur == MOwned || null hs)
           , defAction = \_ -> do
               void pickLeaderWithPointer  -- error ignored; update anyway
               (cCurUpd, cRestUpd) <- legalWithUpdatedLeader cCur cRest
               recCall numPrefix cCurUpd cRestUpd itemDialogState
           })
        , let km = revCmd (K.KM K.NoModifier $ K.Char '^') SortSlots
          in (km, DefItemKey
           { defLabel = if cCur == MOwned then Right km else Left ""
           , defCond = True
           , defAction = \_ -> do
               sortSlots (bfid body) (Just body)
               recCall numPrefix cCur cRest itemDialogState
           })
        , (K.escKM, DefItemKey
           { defLabel = Right K.escKM
           , defCond = True
           , defAction = \ekm -> return (Left "never mind", (cCur, ekm))
           })
        ]
        ++ numberPrefixes
      changeContainerDef defLabel = DefItemKey
        { defLabel
        , defCond = not $ null cRest
        , defAction = \_ -> do
            let calmE = calmEnough body ar
                mcCur = filter (`elem` cLegal) [cCur]
                (cCurAfterCalm, cRestAfterCalm) = case cRest ++ mcCur of
                  c1@(MStore CSha) : c2 : rest | not calmE ->
                    (c2, c1 : rest)
                  [MStore CSha] | not calmE -> error $ "" `showFailure` cRest
                  c1 : rest -> (c1, rest)
                  [] -> error $ "" `showFailure` cRest
            recCall numPrefix cCurAfterCalm cRestAfterCalm itemDialogState
        }
      useMultipleDef defLabel = DefItemKey
        { defLabel
        , defCond = permitMulitple && not (EM.null multipleSlots)
        , defAction = \ekm ->
            let eslots = EM.elems multipleSlots
            in return $ getMultResult ekm eslots
        }
      prefixCmdDef d =
        (K.mkChar $ Char.intToDigit d, DefItemKey
           { defLabel = Left ""
           , defCond = True
           , defAction = \_ ->
               recCall (10 * numPrefix + d) cCur cRest itemDialogState
           })
      numberPrefixes = map prefixCmdDef [0..9]
      lettersDef :: DefItemKey m
      lettersDef = DefItemKey
        { defLabel = Left ""
        , defCond = True
        , defAction = \ekm ->
            let slot = case ekm of
                  Left K.KM{key} -> case key of
                    K.Char l -> SlotChar numPrefix l
                    _ -> error $ "unexpected key:"
                                 `showFailure` K.showKey key
                  Right sl -> sl
            in case EM.lookup slot bagItemSlotsAll of
              Nothing -> error $ "unexpected slot"
                                 `showFailure` (slot, bagItemSlots)
              Just iid -> return $! getResult (Right slot) iid
        }
      (bagFiltered, promptChosen) =
        case itemDialogState of
          ISuitable -> (bagSuit, prompt body bodyUI ar cCur <> ":")
          IAll      -> (bag, promptGeneric body bodyUI ar cCur <> ":")
  case cCur of
    MStats -> do
      io <- statsOverlay leader
      let slotLabels = map fst $ snd io
          slotKeys = mapMaybe (keyOfEKM numPrefix) slotLabels
          statsDef :: DefItemKey m
          statsDef = DefItemKey
            { defLabel = Left ""
            , defCond = True
            , defAction = \ekm ->
                let slot = case ekm of
                      Left K.KM{key} -> case key of
                        K.Char l -> SlotChar numPrefix l
                        _ -> error $ "unexpected key:"
                                     `showFailure` K.showKey key
                      Right sl -> sl
                in return (Left "stats", (MStats, Right slot))
            }
      runDefItemKey keyDefs statsDef io slotKeys promptChosen MStats
    _ -> do
      io <- itemOverlay (storeFromMode cCur) (blid body) bagFiltered
      let slotKeys = mapMaybe (keyOfEKM numPrefix . Right)
                     $ EM.keys bagItemSlots
      runDefItemKey keyDefs lettersDef io slotKeys promptChosen cCur

keyOfEKM :: Int -> Either [K.KM] SlotChar -> Maybe K.KM
keyOfEKM _ (Left kms) = error $ "" `showFailure` kms
keyOfEKM numPrefix (Right SlotChar{..}) | slotPrefix == numPrefix =
  Just $ K.mkChar slotChar
keyOfEKM _ _ = Nothing

legalWithUpdatedLeader :: MonadClientUI m
                       => ItemDialogMode
                       -> [ItemDialogMode]
                       -> m (ItemDialogMode, [ItemDialogMode])
legalWithUpdatedLeader cCur cRest = do
  leader <- getLeaderUI
  let newLegal = cCur : cRest  -- not updated in any way yet
  b <- getsState $ getActorBody leader
  actorAspect <- getsClient sactorAspect
  let ar = fromMaybe (error $ "" `showFailure` leader)
                     (EM.lookup leader actorAspect)
      calmE = calmEnough b ar
      legalAfterCalm = case newLegal of
        c1@(MStore CSha) : c2 : rest | not calmE -> (c2, c1 : rest)
        [MStore CSha] | not calmE -> (MStore CGround, newLegal)
        c1 : rest -> (c1, rest)
        [] -> error $ "" `showFailure` (cCur, cRest)
  return legalAfterCalm

-- We don't create keys from slots in @okx@, so they have to be
-- exolicitly given in @slotKeys@.
runDefItemKey :: MonadClientUI m
              => [(K.KM, DefItemKey m)]
              -> DefItemKey m
              -> OKX
              -> [K.KM]
              -> Text
              -> ItemDialogMode
              -> m ( Either Text [(ItemId, ItemFull)]
                   , (ItemDialogMode, Either K.KM SlotChar) )
runDefItemKey keyDefs lettersDef okx slotKeys prompt cCur = do
  let itemKeys = slotKeys ++ map fst keyDefs
      wrapB s = "[" <> s <> "]"
      (keyLabelsRaw, keys) = partitionEithers $ map (defLabel . snd) keyDefs
      keyLabels = filter (not . T.null) keyLabelsRaw
      choice = T.intercalate " " $ map wrapB $ nub keyLabels
  promptAdd $ prompt <+> choice
  lidV <- viewedLevelUI
  Level{lysize} <- getLevel lidV
  ekm <- do
    okxs <- overlayToSlideshow (lysize + 1) keys okx
    !lastSlot <- getsSession slastSlot
    let allOKX = concatMap snd $ slideshow okxs
        pointer =
          case findIndex ((== Right lastSlot) . fst) allOKX of
            Just p | cCur `notElem` [MStats, MLoreItem, MLoreOrgan] -> p
            _ -> case findIndex (isRight . fst) allOKX of
              Just p -> p
              _ -> 0
    (okm, pointer2) <- displayChoiceScreen ColorFull False pointer okxs itemKeys
    -- Remember item pointer, unless not a proper item container. Remember
    -- even if not moved, in case the initial position was a default.
    case drop pointer2 allOKX of
      (Right slastSlot, _) : _
        | cCur `notElem` [MStats, MLoreItem, MLoreOrgan] ->
        modifySession $ \sess -> sess {slastSlot}
      _ -> return ()
    return okm
  case ekm of
    Left km -> case km `lookup` keyDefs of
      Just keyDef -> defAction keyDef ekm
      Nothing -> defAction lettersDef ekm  -- pressed; with current prefix
    Right _slot -> defAction lettersDef ekm  -- selected; with the given prefix
