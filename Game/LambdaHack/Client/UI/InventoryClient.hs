{-# LANGUAGE DataKinds #-}
-- | Inventory management and party cycling.
-- TODO: document
module Game.LambdaHack.Client.UI.InventoryClient
  ( Suitability(..)
  , getGroupItem, getAnyItems, getStoreItem
  , storeFromMode, ppItemDialogMode
  ) where

import Prelude ()
import Prelude.Compat

import Control.Exception.Assert.Sugar
import Control.Monad (filterM, void)
import qualified Data.Char as Char
import Data.Either (rights)
import qualified Data.EnumMap.Strict as EM
import Data.List (delete, find, findIndex, intersect, nub)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.CommonClient
import Game.LambdaHack.Client.ItemSlot
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.HandleHelperClient
import Game.LambdaHack.Client.UI.HumanCmd
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Client.UI.MsgClient
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.WidgetClient
import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Content.ItemKind as IK

data ItemDialogState = ISuitable | IAll | INoSuitable | INoAll
  deriving (Show, Eq)

ppItemDialogMode :: ItemDialogMode -> (Text, Text)
ppItemDialogMode (MStore cstore) = ppCStore cstore
ppItemDialogMode MOwned = ("in", "our possession")
ppItemDialogMode MStats = ("among", "strenghts")

ppItemDialogModeIn :: ItemDialogMode -> Text
ppItemDialogModeIn c = let (tIn, t) = ppItemDialogMode c in tIn <+> t

ppItemDialogModeFrom :: ItemDialogMode -> Text
ppItemDialogModeFrom c = let (_tIn, t) = ppItemDialogMode c in "from" <+> t

storeFromMode :: ItemDialogMode -> CStore
storeFromMode c = case c of
  MStore cstore -> cstore
  MOwned -> CGround  -- needed to decide display mode in textAllAE
  MStats -> CGround  -- needed to decide display mode in textAllAE

accessModeBag :: ActorId -> State -> ItemDialogMode -> ItemBag
accessModeBag leader s (MStore cstore) = getActorBag leader cstore s
accessModeBag leader s MOwned = let fid = bfid $ getActorBody leader s
                                in sharedAllOwnedFid False fid s
accessModeBag _ _ MStats = EM.empty

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
             -> m (SlideOrCmd ((ItemId, ItemFull), ItemDialogMode))
getGroupItem psuit prompt promptGeneric
             cLegalRaw cLegalAfterCalm = do
  soc <- getFull psuit
                 (\_ _ cCur -> prompt <+> ppItemDialogModeFrom cCur)
                 (\_ _ cCur -> promptGeneric <+> ppItemDialogModeFrom cCur)
                 cLegalRaw cLegalAfterCalm True False
  case soc of
    Left sli -> return $ Left sli
    Right ([(iid, itemFull)], c) -> return $ Right ((iid, itemFull), c)
    Right _ -> assert `failure` soc

-- | Let the human player choose any item from a list of items
-- and let him specify the number of items.
-- Used, e.g., for picking up and inventory manipulation.
getAnyItems :: MonadClientUI m
            => m Suitability
                         -- ^ which items to consider suitable
            -> Text      -- ^ specific prompt for only suitable items
            -> Text      -- ^ generic prompt
            -> [CStore]  -- ^ initial legal modes
            -> [CStore]  -- ^ legal modes after Calm taken into account
            -> Bool      -- ^ whether to ask, when the only item
                         --   in the starting mode is suitable
            -> Bool      -- ^ whether to ask for the number of items
            -> m (SlideOrCmd ([(ItemId, ItemFull)], ItemDialogMode))
getAnyItems psuit prompt promptGeneric cLegalRaw cLegalAfterCalm askWhenLone askNumber = do
  soc <- getFull psuit
                 (\_ _ cCur -> prompt <+> ppItemDialogModeFrom cCur)
                 (\_ _ cCur -> promptGeneric <+> ppItemDialogModeFrom cCur)
                 cLegalRaw cLegalAfterCalm askWhenLone True
  case soc of
    Left _ -> return soc
    Right ([(iid, itemFull)], c) -> do
      socK <- pickNumber askNumber $ itemK itemFull
      case socK of
        Left slides -> return $ Left slides
        Right k ->
          return $ Right ([(iid, itemFull{itemK=k})], c)
    Right _ -> return soc

-- | Display all items from a store and let the human player choose any
-- or switch to any other store.
-- Used, e.g., for viewing inventory and item descriptions.
getStoreItem :: MonadClientUI m
             => (Actor -> [ItemFull] -> ItemDialogMode -> Text)
                                 -- ^ how to describe suitable items
             -> ItemDialogMode   -- ^ initial mode
             -> m (SlideOrCmd ((ItemId, ItemFull), ItemDialogMode))
getStoreItem prompt cInitial = do
  let allCs = map MStore [CEqp, CInv, CSha]
              ++ [MOwned]
              ++ map MStore [CGround, COrgan]
              ++ [MStats]
      (pre, rest) = break (== cInitial) allCs
      post = dropWhile (== cInitial) rest
      remCs = post ++ pre
  soc <- getItem (return SuitsEverything)
                 prompt prompt cInitial remCs
                 True False (cInitial:remCs)
  case soc of
    Left sli -> return $ Left sli
    Right ([(iid, itemFull)], c) -> return $ Right ((iid, itemFull), c)
    Right _ -> assert `failure` soc

-- | Let the human player choose a single, preferably suitable,
-- item from a list of items. Don't display stores empty for all actors.
-- Start with a non-empty store.
getFull :: MonadClientUI m
        => m Suitability
                            -- ^ which items to consider suitable
        -> (Actor -> [ItemFull] -> ItemDialogMode -> Text)
                            -- ^ specific prompt for only suitable items
        -> (Actor -> [ItemFull] -> ItemDialogMode -> Text)
                            -- ^ generic prompt
        -> [CStore]         -- ^ initial legal modes
        -> [CStore]         -- ^ legal modes with Calm taken into account
        -> Bool             -- ^ whether to ask, when the only item
                            --   in the starting mode is suitable
        -> Bool             -- ^ whether to permit multiple items as a result
        -> m (SlideOrCmd ([(ItemId, ItemFull)], ItemDialogMode))
getFull psuit prompt promptGeneric cLegalRaw cLegalAfterCalm
        askWhenLone permitMulitple = do
  side <- getsClient sside
  leader <- getLeaderUI
  let aidNotEmpty store aid = do
        bag <- getsState $ getCBag (CActor aid store)
        return $! not $ EM.null bag
      partyNotEmpty store = do
        as <- getsState $ fidActorNotProjAssocs side
        bs <- mapM (aidNotEmpty store . fst) as
        return $! or bs
  mpsuit <- psuit
  let psuitFun = case mpsuit of
        SuitsEverything -> const True
        SuitsNothing _ -> const False
        SuitsSomething f -> f
  -- Move the first store that is non-empty for suitable items for this actor
  -- to the front, if any.
  getCStoreBag <- getsState $ \s cstore -> getCBag (CActor leader cstore) s
  let hasThisActor = not . EM.null . getCStoreBag
  case filter hasThisActor cLegalAfterCalm of
    [] ->
      if isNothing (find hasThisActor cLegalRaw) then do
        let contLegalRaw = map MStore cLegalRaw
            tLegal = map (MU.Text . ppItemDialogModeIn) contLegalRaw
            ppLegal = makePhrase [MU.WWxW "nor" tLegal]
        failWith $ "no items" <+> ppLegal
      else failSer ItemNotCalm
    haveThis@(headThisActor : _) -> do
      itemToF <- itemToFullClient
      let suitsThisActor store =
            let bag = getCStoreBag store
            in any (\(iid, kit) -> psuitFun $ itemToF iid kit) $ EM.assocs bag
          cThisActor cDef = fromMaybe cDef $ find suitsThisActor haveThis
      -- Don't display stores totally empty for all actors.
      cLegal <- filterM partyNotEmpty cLegalRaw
      let breakStores cInit =
            let (pre, rest) = break (== cInit) cLegal
                post = dropWhile (== cInit) rest
            in (MStore cInit, map MStore $ post ++ pre)
      -- The last used store may go before even the first nonempty store.
      lastStoreList <- getsClient slastStore
      let legalLast = intersect lastStoreList cLegalAfterCalm
      firstStore <- case legalLast of
        [] -> return $! cThisActor headThisActor
        lastStore : _ -> return $! if cThisActor lastStore == CGround
                                   then CGround
                                   else lastStore
      let (modeFirst, modeRest) = breakStores firstStore
      getItem psuit prompt promptGeneric modeFirst modeRest
              askWhenLone permitMulitple (map MStore cLegal)

-- | Let the human player choose a single, preferably suitable,
-- item from a list of items.
getItem :: MonadClientUI m
        => m Suitability
                            -- ^ which items to consider suitable
        -> (Actor -> [ItemFull] -> ItemDialogMode -> Text)
                            -- ^ specific prompt for only suitable items
        -> (Actor -> [ItemFull] -> ItemDialogMode -> Text)
                            -- ^ generic prompt
        -> ItemDialogMode   -- ^ first mode, legal or not
        -> [ItemDialogMode] -- ^ the (rest of) legal modes
        -> Bool             -- ^ whether to ask, when the only item
                            --   in the starting mode is suitable
        -> Bool             -- ^ whether to permit multiple items as a result
        -> [ItemDialogMode] -- ^ all legal modes
        -> m (SlideOrCmd ([(ItemId, ItemFull)], ItemDialogMode))
getItem psuit prompt promptGeneric cCur cRest askWhenLone permitMulitple
        cLegal = do
  leader <- getLeaderUI
  accessCBag <- getsState $ accessModeBag leader
  let storeAssocs = EM.assocs . accessCBag
      allAssocs = concatMap storeAssocs (cCur : cRest)
  case (cRest, allAssocs) of
    ([], [(iid, k)]) | not askWhenLone -> do
      itemToF <- itemToFullClient
      return $ Right ([(iid, itemToF iid k)], cCur)
    _ ->
      transition psuit prompt promptGeneric permitMulitple cLegal
                 0 cCur cRest ISuitable

data DefItemKey m = DefItemKey
  { defLabel  :: Text  -- ^ can be undefined if not @defCond@
  , defCond   :: !Bool
  , defAction :: Either K.KM SlotChar
                 -> m (SlideOrCmd ([(ItemId, ItemFull)], ItemDialogMode))
  }

data Suitability =
    SuitsEverything
  | SuitsNothing Text
  | SuitsSomething (ItemFull -> Bool)

transition :: forall m. MonadClientUI m
           => m Suitability
           -> (Actor -> [ItemFull] -> ItemDialogMode -> Text)
           -> (Actor -> [ItemFull] -> ItemDialogMode -> Text)
           -> Bool
           -> [ItemDialogMode]
           -> Int
           -> ItemDialogMode
           -> [ItemDialogMode]
           -> ItemDialogState
           -> m (SlideOrCmd ([(ItemId, ItemFull)], ItemDialogMode))
transition psuit prompt promptGeneric permitMulitple cLegal
           numPrefix cCur cRest itemDialogState = do
  let recCall =
        transition psuit prompt promptGeneric permitMulitple cLegal
  (itemSlots, organSlots) <- getsClient sslots
  leader <- getLeaderUI
  body <- getsState $ getActorBody leader
  activeItems <- activeItemsClient leader
  fact <- getsState $ (EM.! bfid body) . sfactionD
  hs <- partyAfterLeader leader
  bagAll <- getsState $ \s -> accessModeBag leader s cCur
  lastSlot <- getsClient slastSlot
  itemToF <- itemToFullClient
  Binding{brevMap} <- askBinding
  mpsuit <- psuit  -- when throwing, this sets eps and checks xhair validity
  (suitsEverything, psuitFun) <- case mpsuit of
    SuitsEverything -> return (True, const True)
    SuitsNothing err -> do
      slides <- promptToSlideshow $ toAttrLine $ err <+> tmoreMsg
      void $ getConfirms ColorFull [K.spaceKM] [K.escKM] slides
      return (False, const False)
    -- When throwing, this function takes missile range into accout.
    SuitsSomething f -> return (False, f)
  let getSingleResult :: ItemId -> (ItemId, ItemFull)
      getSingleResult iid = (iid, itemToF iid (bagAll EM.! iid))
      getResult :: ItemId -> ([(ItemId, ItemFull)], ItemDialogMode)
      getResult iid = ([getSingleResult iid], cCur)
      getMultResult :: [ItemId] -> ([(ItemId, ItemFull)], ItemDialogMode)
      getMultResult iids = (map getSingleResult iids, cCur)
      filterP iid kit = psuitFun $ itemToF iid kit
      bagAllSuit = EM.filterWithKey filterP bagAll
      isOrgan = cCur == MStore COrgan
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
      suitableItemSlots = EM.filterWithKey hasPrefix suitableItemSlotsOpen
      bagSuit = EM.fromList $ map (\iid -> (iid, bagAllSuit EM.! iid))
                                  (EM.elems suitableItemSlotsOpen)
      (autoDun, autoLvl) = autoDungeonLevel fact
      multipleSlots = if itemDialogState `elem` [IAll, INoAll]
                      then bagItemSlotsAll
                      else suitableItemSlotsAll
      revCmd dflt cmd = case M.lookup cmd brevMap of
        Nothing -> dflt
        Just (k : _) -> k
        Just [] -> assert `failure` brevMap
      keyDefs :: [(K.KM, DefItemKey m)]
      keyDefs = filter (defCond . snd) $
        [ (K.KM K.NoModifier $ K.Char '?', DefItemKey
           { defLabel = "?"
           , defCond = not (EM.null bag)
           , defAction = \_ -> recCall numPrefix cCur cRest
                               $ case itemDialogState of
               INoSuitable -> if EM.null bagSuit then IAll else ISuitable
               ISuitable -> if suitsEverything then INoAll else IAll
               IAll -> if EM.null bag then INoSuitable else INoAll
               INoAll -> if suitsEverything then ISuitable else INoSuitable
           })
        , (K.KM K.NoModifier $ K.Char '/', DefItemKey
           { defLabel = "/"
           , defCond = not $ null cRest
           , defAction = \_ -> do
               let calmE = calmEnough body activeItems
                   mcCur = filter (`elem` cLegal) [cCur]
                   (cCurAfterCalm, cRestAfterCalm) = case cRest ++ mcCur of
                     c1@(MStore CSha) : c2 : rest | not calmE ->
                       (c2, c1 : rest)
                     [MStore CSha] | not calmE -> assert `failure` cRest
                     c1 : rest -> (c1, rest)
                     [] -> assert `failure` cRest
               recCall numPrefix cCurAfterCalm cRestAfterCalm itemDialogState
           })
        , (K.KM K.NoModifier $ K.Char '*', DefItemKey
           { defLabel = "*"
           , defCond = permitMulitple && not (EM.null multipleSlots)
           , defAction = \_ ->
               let eslots = EM.elems multipleSlots
               in return $ Right $ getMultResult eslots
           })
        , (K.escKM, DefItemKey
           { defLabel = ""
           , defCond = True
           , defAction = \_ -> failWith "never mind"
           })
        , (K.returnKM, DefItemKey
           { defLabel = if lastSlot `EM.member` labelItemSlotsOpen
                        then "RET(" <> slotLabel lastSlot
                        else "RET"
           , defCond = not (EM.null labelItemSlotsOpen)
                       && EM.null bagFiltered
           , defAction = \_ -> case EM.lookup lastSlot labelItemSlotsOpen of
               Just iid -> return $ Right $ getResult iid
               Nothing -> case EM.minViewWithKey labelItemSlotsOpen of
                 Nothing -> assert `failure` "labelItemSlotsOpen empty"
                                   `twith` labelItemSlotsOpen
                 Just ((l, _), _) -> do
                   lastStore <- getsClient slastStore
                   let store = storeFromMode cCur
                       newStore = store : delete store lastStore
                   modifyClient $ \cli -> cli { slastSlot = l
                                              , slastStore = newStore }
                   recCall numPrefix cCur cRest itemDialogState
           })
        , let km = revCmd (K.KM K.NoModifier K.Tab) MemberCycle
          in (km, DefItemKey
           { defLabel = K.showKM km
           , defCond = not (cCur == MOwned
                            || autoLvl
                            || not (any (\(_, b) -> blid b == blid body) hs))
           , defAction = \_ -> do
               err <- memberCycle False
               let !_A = assert (err == mempty `blame` err) ()
               (cCurUpd, cRestUpd) <- legalWithUpdatedLeader cCur cRest
               recCall numPrefix cCurUpd cRestUpd itemDialogState
           })
        , let km = revCmd (K.KM K.NoModifier K.BackTab) MemberBack
          in (km, DefItemKey
           { defLabel = K.showKM km
           , defCond = not (cCur == MOwned || autoDun || null hs)
           , defAction = \_ -> do
               err <- memberBack False
               let !_A = assert (err == mempty `blame` err) ()
               (cCurUpd, cRestUpd) <- legalWithUpdatedLeader cCur cRest
               recCall numPrefix cCurUpd cRestUpd itemDialogState
           })
        ]
        ++ numberPrefixes
      prefixCmdDef d =
        (K.KM K.NoModifier $ K.Char $ Char.intToDigit d, DefItemKey
           { defLabel = ""
           , defCond = True
           , defAction = \_ ->
               recCall (10 * numPrefix + d) cCur cRest itemDialogState
           })
      numberPrefixes = map prefixCmdDef [0..9]
      lettersDef :: DefItemKey m
      lettersDef = DefItemKey
        { defLabel = slotRange $ EM.keys labelItemSlots
        , defCond = True
        , defAction = \ekm ->
            let slot = case ekm of
                  Left K.KM{key} -> case key of
                    K.Char l -> SlotChar numPrefix l
                    _ -> assert `failure` "unexpected key:"
                                `twith` K.showKey key
                  Right sl -> sl
            in case EM.lookup slot bagItemSlotsAll of
              Nothing -> assert `failure` "unexpected slot"
                                `twith` (slot, bagItemSlots)
              Just iid -> return $ Right $ getResult iid
        }
      (labelItemSlotsOpen, labelItemSlots, bagFiltered, promptChosen) =
        case itemDialogState of
          ISuitable   -> (suitableItemSlotsOpen,
                          suitableItemSlots,
                          bagSuit,
                          prompt body activeItems cCur <> ":")
          IAll        -> (bagItemSlotsOpen,
                          bagItemSlots,
                          bag,
                          promptGeneric body activeItems cCur <> ":")
          INoSuitable -> (suitableItemSlotsOpen,
                          suitableItemSlots,
                          EM.empty,
                          prompt body activeItems cCur <> ":")
          INoAll      -> (bagItemSlotsOpen,
                          bagItemSlots,
                          EM.empty,
                          promptGeneric body activeItems cCur <> ":")
  case cCur of
    MStats -> do
      io <- statsOverlay leader
      let slotLabels = map fst $ snd io
          slotKeys = mapMaybe (keyOfEKM numPrefix) slotLabels
          statsDef :: DefItemKey m
          statsDef = DefItemKey
            { defLabel = slotRange $ rights slotLabels
            , defCond = True
            , defAction = \ekm ->
            let _slot = case ekm of
                  Left K.KM{key} -> case key of
                    K.Char l -> SlotChar numPrefix l
                    _ -> assert `failure` "unexpected key:"
                                `twith` K.showKey key
                  Right sl -> sl
            in failWith "stats affect character actions"  -- TODO
            }
      runDefItemKey keyDefs statsDef io slotKeys promptChosen MStats
    _ -> do
      io <- itemOverlay (storeFromMode cCur) (blid body) bagFiltered
      let slotKeys = mapMaybe (keyOfEKM numPrefix . Right)
                     $ EM.keys bagItemSlots
      runDefItemKey keyDefs lettersDef io slotKeys promptChosen cCur

statsOverlay :: MonadClient m => ActorId -> m OKX
statsOverlay aid = do
  b <- getsState $ getActorBody aid
  activeItems <- activeItemsClient aid
  let block n = n + if braced b then 50 else 0
      prSlot :: SlotChar -> (IK.EqpSlot, Int -> Text) -> (Text, KYX)
      prSlot c (eqpSlot, f) =
        let fullText t =
              makePhrase [ MU.Text $ slotLabel c
                         , MU.Text $ T.justifyLeft 22 ' '
                                   $ IK.slotName eqpSlot
                         , MU.Text t ]
            valueText = f $ sumSlotNoFilter eqpSlot activeItems
            ft = fullText valueText
        in (ft, (Right c, (undefined, 0, T.length ft)))
      -- Some values can be negative, for others 0 is equivalent but shorter.
      slotList =  -- TODO:  [IK.EqpSlotAddHurtMelee..IK.EqpSlotAddLight]
        [ (IK.EqpSlotAddHurtMelee, \t -> tshow t <> "%")
        -- TODO: not applicable right now, IK.EqpSlotAddHurtRanged
        , (IK.EqpSlotAddArmorMelee, \t -> "[" <> tshow (block t) <> "%]")
        , (IK.EqpSlotAddArmorRanged, \t -> "{" <> tshow (block t) <> "%}")
        , (IK.EqpSlotAddMaxHP, \t -> tshow $ max 0 t)
        , (IK.EqpSlotAddMaxCalm, \t -> tshow $ max 0 t)
        , (IK.EqpSlotAddSpeed, \t -> tshow (max 0 t) <> "m/10s")
        , (IK.EqpSlotAddSight, \t ->
            tshow (max 0 $ min (fromIntegral $ bcalm b `div` (5 * oneM)) t)
            <> "m")
        , (IK.EqpSlotAddSmell, \t -> tshow (max 0 t) <> "m")
        , (IK.EqpSlotAddLight, \t -> tshow (max 0 t) <> "m")
        ]
      skills = sumSkills activeItems
      -- TODO: are negative total skills meaningful?
      -- TODO: unduplicate with prSlot
      prAbility :: SlotChar -> Ability.Ability -> (Text, KYX)
      prAbility c ability =
        let fullText t =
              makePhrase [ MU.Text $ slotLabel c
                         , MU.Text $ T.justifyLeft 22 ' '
                           $ "ability" <+> tshow ability
                         , MU.Text t ]
            valueText = tshow $ EM.findWithDefault 0 ability skills
            ft = fullText valueText
        in (ft, (Right c, (undefined, 0, T.length ft)))
      abilityList = [minBound..maxBound]
      reslot c = either (prSlot c) (prAbility c)
      zipReslot = zipWith reslot allZeroSlots
      (ts, kxs) = unzip $ zipReslot $ map Left slotList ++ map Right abilityList
  return (toOverlay ts, kxs)

legalWithUpdatedLeader :: MonadClientUI m
                       => ItemDialogMode
                       -> [ItemDialogMode]
                       -> m (ItemDialogMode, [ItemDialogMode])
legalWithUpdatedLeader cCur cRest = do
  leader <- getLeaderUI
  let newLegal = cCur : cRest  -- not updated in any way yet
  b <- getsState $ getActorBody leader
  activeItems <- activeItemsClient leader
  let calmE = calmEnough b activeItems
      legalAfterCalm = case newLegal of
        c1@(MStore CSha) : c2 : rest | not calmE -> (c2, c1 : rest)
        [MStore CSha] | not calmE -> (MStore CGround, newLegal)
        c1 : rest -> (c1, rest)
        [] -> assert `failure` (cCur, cRest)
  return legalAfterCalm

runDefItemKey :: MonadClientUI m
              => [(K.KM, DefItemKey m)]
              -> DefItemKey m
              -> OKX
              -> [K.KM]
              -> Text
              -> ItemDialogMode
              -> m (SlideOrCmd ([(ItemId, ItemFull)], ItemDialogMode))
runDefItemKey keyDefs lettersDef okx slotKeys prompt cCur = do
  let itemKeys = slotKeys ++ map fst keyDefs
      choice = let letterRange = defLabel lettersDef
                   keyLabelsRaw = letterRange : map (defLabel . snd) keyDefs
                   keyLabels = filter (not . T.null) keyLabelsRaw
               in "[" <> T.intercalate ", " (nub keyLabels) <> ", ESC]"
      attrL = toAttrLine $ prompt <+> choice
  arena <- getArenaUI
  Level{lysize} <- getLevel arena
  ekm <- if null $ overlay $ fst okx
         then
           Left <$> displayChoiceLine attrL (fst okx) itemKeys
         else do
           lastSlot <- getsClient slastSlot
           let lastPointer = case findIndex ((== Right lastSlot) . fst)
                                            (snd okx) of
                 Just p | cCur /= MStats -> p
                 _ -> 0
           okxs <- splitOKX (lysize + 1) attrL okx
           (okm, pointer) <- displayChoiceScreen False lastPointer okxs itemKeys
           -- Only remember item pointer, if moved and if not stats.
           case drop pointer $ snd okx of
             (Right newSlot, _) : _ | pointer /= lastPointer
                                      && cCur /= MStats -> do
               lastStore <- getsClient slastStore
               let store = storeFromMode cCur
                   newStore = store : delete store lastStore
               modifyClient $ \cli -> cli { slastSlot = newSlot
                                          , slastStore = newStore }
             _ -> return ()
           return okm
  case ekm of
    Left km -> case km `lookup` keyDefs of
      Just keyDef -> defAction keyDef ekm
      Nothing -> defAction lettersDef ekm  -- pressed; with current prefix
    Right _slot -> defAction lettersDef ekm  -- selected; with the given prefix
