{-# LANGUAGE DataKinds #-}
-- | Inventory management and party cycling.
-- TODO: document
module Game.LambdaHack.Client.UI.InventoryClient
  ( Suitability(..), ItemDialogState(..)
  , getGroupItem, getAnyItems, getStoreItem
  , pickNumber, describeItemC, projectHumanState, triggerSymbols
  ) where

import Prelude ()
import Prelude.Compat

import Control.Exception.Assert.Sugar
import Control.Monad (filterM, void, when)
import qualified Data.Char as Char
import Data.Either (isRight, rights)
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
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Client.UI.WidgetClient
import qualified Game.LambdaHack.Common.Ability as Ability
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
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
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
             -> Bool      -- ^ whether to enable setting cursor with mouse
             -> [CStore]  -- ^ initial legal modes
             -> [CStore]  -- ^ legal modes after Calm taken into account
             -> ItemDialogState  -- ^ the dialog state to start in
             -> m (SlideOrCmd ((ItemId, ItemFull), ItemDialogMode))
getGroupItem psuit prompt promptGeneric
             cursor cLegalRaw cLegalAfterCalm initalState = do
  soc <- getFull psuit
                 (\_ _ cCur -> prompt <+> ppItemDialogModeFrom cCur)
                 (\_ _ cCur -> promptGeneric <+> ppItemDialogModeFrom cCur)
                 cursor cLegalRaw cLegalAfterCalm True False initalState
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
                 False cLegalRaw cLegalAfterCalm
                 askWhenLone True ISuitable
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
                 prompt prompt False cInitial remCs
                 True False (cInitial:remCs) ISuitable
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
        -> Bool             -- ^ whether to enable setting cursor with mouse
        -> [CStore]         -- ^ initial legal modes
        -> [CStore]         -- ^ legal modes with Calm taken into account
        -> Bool             -- ^ whether to ask, when the only item
                            --   in the starting mode is suitable
        -> Bool             -- ^ whether to permit multiple items as a result
        -> ItemDialogState  -- ^ the dialog state to start in
        -> m (SlideOrCmd ([(ItemId, ItemFull)], ItemDialogMode))
getFull psuit prompt promptGeneric cursor cLegalRaw cLegalAfterCalm
        askWhenLone permitMulitple initalState = do
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
      getItem psuit prompt promptGeneric cursor modeFirst modeRest
              askWhenLone permitMulitple (map MStore cLegal) initalState

-- | Let the human player choose a single, preferably suitable,
-- item from a list of items.
getItem :: MonadClientUI m
        => m Suitability
                            -- ^ which items to consider suitable
        -> (Actor -> [ItemFull] -> ItemDialogMode -> Text)
                            -- ^ specific prompt for only suitable items
        -> (Actor -> [ItemFull] -> ItemDialogMode -> Text)
                            -- ^ generic prompt
        -> Bool             -- ^ whether to enable setting cursor with mouse
        -> ItemDialogMode   -- ^ first mode, legal or not
        -> [ItemDialogMode] -- ^ the (rest of) legal modes
        -> Bool             -- ^ whether to ask, when the only item
                            --   in the starting mode is suitable
        -> Bool             -- ^ whether to permit multiple items as a result
        -> [ItemDialogMode] -- ^ all legal modes
        -> ItemDialogState  -- ^ the dialog state to start in
        -> m (SlideOrCmd ([(ItemId, ItemFull)], ItemDialogMode))
getItem psuit prompt promptGeneric cursor cCur cRest askWhenLone permitMulitple
        cLegal initalState = do
  leader <- getLeaderUI
  accessCBag <- getsState $ accessModeBag leader
  let storeAssocs = EM.assocs . accessCBag
      allAssocs = concatMap storeAssocs (cCur : cRest)
  case (cRest, allAssocs) of
    ([], [(iid, k)]) | not askWhenLone -> do
      itemToF <- itemToFullClient
      return $ Right ([(iid, itemToF iid k)], cCur)
    _ ->
      transition psuit prompt promptGeneric cursor permitMulitple cLegal
                 0 cCur cRest initalState

data DefItemKey m = DefItemKey
  { defLabel  :: Text  -- ^ can be undefined if not @defCond@
  , defCond   :: !Bool
  , defAction :: Either K.KM SlotChar
                 -> m (SlideOrCmd ([(ItemId, ItemFull)], ItemDialogMode))
  }

data Suitability =
    SuitsEverything
  | SuitsNothing Msg
  | SuitsSomething (ItemFull -> Bool)

transition :: forall m. MonadClientUI m
           => m Suitability
           -> (Actor -> [ItemFull] -> ItemDialogMode -> Text)
           -> (Actor -> [ItemFull] -> ItemDialogMode -> Text)
           -> Bool
           -> Bool
           -> [ItemDialogMode]
           -> Int
           -> ItemDialogMode
           -> [ItemDialogMode]
           -> ItemDialogState
           -> m (SlideOrCmd ([(ItemId, ItemFull)], ItemDialogMode))
transition psuit prompt promptGeneric cursor permitMulitple cLegal
           numPrefix cCur cRest itemDialogState = do
  let recCall =
        transition psuit prompt promptGeneric cursor permitMulitple cLegal
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
  mpsuit <- psuit  -- when throwing, this sets eps and checks cursor validity
  (suitsEverything, psuitFun) <- case mpsuit of
    SuitsEverything -> return (True, const True)
    SuitsNothing err -> do
      slides <- promptToSlideshow $ err <+> moreMsg
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
        , let km = revCmd (K.KM K.NoModifier (K.KP '/')) TgtFloor
          in cursorCmdDef False km tgtFloorHuman
        , let hackyCmd = Alias "" TgtFloor  -- no keypad, but arrows enough
              km = revCmd (K.KM K.NoModifier K.RightButtonPress) hackyCmd
          in cursorCmdDef False km tgtFloorHuman
        , let km = revCmd (K.KM K.NoModifier (K.KP '*')) TgtEnemy
          in cursorCmdDef False km tgtEnemyHuman
        , let hackyCmd = Alias "" TgtEnemy  -- no keypad, but arrows enough
              km = revCmd (K.KM K.NoModifier K.RightButtonPress) hackyCmd
          in cursorCmdDef False km tgtEnemyHuman
        , let km = revCmd (K.KM K.NoModifier K.BackSpace) TgtClear
          in cursorCmdDef False km tgtClearHuman
        ]
        ++ numberPrefixes
        ++ [ let plusMinus = K.Char $ if b then '+' else '-'
                 km = revCmd (K.KM K.NoModifier plusMinus) (EpsIncr b)
             in cursorCmdDef False km (epsIncrHuman b)
           | b <- [True, False]
           ]
        ++ arrows
        ++ [
          let km = revCmd (K.KM K.NoModifier K.MiddleButtonPress)
                          CursorPointerEnemy
          in cursorCmdDef False km (cursorPointerEnemy False False)
        , let km = revCmd (K.KM K.Shift K.MiddleButtonPress)
                          CursorPointerFloor
          in cursorCmdDef False km (cursorPointerFloor False False)
        , let km = revCmd (K.KM K.NoModifier K.RightButtonPress)
                          TgtPointerEnemy
          in cursorCmdDef True km (cursorPointerEnemy True True)
        ]
      prefixCmdDef d =
        (K.KM K.NoModifier $ K.Char $ Char.intToDigit d, DefItemKey
           { defLabel = ""
           , defCond = True
           , defAction = \_ ->
               recCall (10 * numPrefix + d) cCur cRest itemDialogState
           })
      numberPrefixes = map prefixCmdDef [0..9]
      cursorCmdDef verbose km cmd =
        (km, DefItemKey
           { defLabel = "keypad, mouse"
           , defCond = cursor && EM.null bagFiltered
           , defAction = \_ -> do
               look <- cmd
               when verbose $
                 void $ getConfirms ColorFull [K.spaceKM] [K.escKM] look
               recCall numPrefix cCur cRest itemDialogState
           })
      arrows =
        let kCmds = K.moveBinding False False
                                  (`moveCursorHuman` 1) (`moveCursorHuman` 10)
        in map (uncurry $ cursorCmdDef False) kCmds
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
  arena <- getArenaUI
  Level{lysize} <- getLevel arena
  ekm <- if null $ overlay $ fst okx
         then
           Left <$> displayChoiceLine (prompt <+> choice) (fst okx) itemKeys
         else do
           lastSlot <- getsClient slastSlot
           let lastPointer = case findIndex ((== Right lastSlot) . fst)
                                            (snd okx) of
                 Just p | cCur /= MStats -> p
                 _ -> 0
           okxs <- splitOKX (lysize + 1) (prompt <+> choice) okx
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

pickNumber :: MonadClientUI m => Bool -> Int -> m (SlideOrCmd Int)
pickNumber askNumber kAll = do
  let gatherNumber kDefaultRaw = do
        let kDefault = min kAll kDefaultRaw
            kprompt = "Choose number [digits, BACKSPACE, RET("
                      <> tshow kDefault
                      <> "), ESC]"
        ov : _ <- slideshow <$> overlayToSlideshow kprompt mempty
        frame <- drawOverlay ColorFull False ov
        kkm <- promptGetInt frame
        case K.key kkm of
          K.Char l | kDefault == kAll -> gatherNumber $ Char.digitToInt l
          K.Char l -> gatherNumber $ kDefault * 10 + Char.digitToInt l
          K.BackSpace -> gatherNumber $ kDefault `div` 10
          K.Return -> return $ Right kDefault
          K.Esc -> failWith "never mind"
          _ -> assert `failure` "unexpected key:" `twith` kkm
  if | kAll == 0 -> failWith "no number of items can be chosen"
     | kAll == 1 || not askNumber -> return $ Right kAll
     | otherwise -> do
         num <- gatherNumber kAll
         case num of
           Right 0 -> failWith "zero items chosen"
           _ -> return num

-- | Create a list of item names.
_floorItemOverlay :: MonadClientUI m
                  => LevelId -> Point
                  -> m (SlideOrCmd RequestUI)
_floorItemOverlay _lid _p = describeItemC MOwned {-CFloor lid p-}

describeItemC :: forall m. MonadClientUI m
              => ItemDialogMode -> m (SlideOrCmd RequestUI)
describeItemC c = do
  let subject = partActor
      verbSha body activeItems = if calmEnough body activeItems
                                 then "notice"
                                 else "paw distractedly"
      prompt body activeItems c2 =
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
              $ MU.SubjectVerbSg (subject body) (verbSha body activeItems)
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
  case ggi of
    Right ((iid, itemFull), c2) -> do
      leader <- getLeaderUI
      b <- getsState $ getActorBody leader
      activeItems <- activeItemsClient leader
      let calmE = calmEnough b activeItems
      localTime <- getsState $ getLocalTime (blid b)
      let io = itemDesc (storeFromMode c2) localTime itemFull
      case c2 of
        MStore COrgan -> do
          let symbol = jsymbol (itemBase itemFull)
              blurb | symbol == '+' = "drop temporary conditions"
                    | otherwise = "amputate organs"
          -- TODO: also forbid on the server, except in special cases.
          Left <$> overlayToSlideshow ("Can't"
                                       <+> blurb
                                       <> ", but here's the description.") io
        MStore CSha | not calmE ->
          Left <$> overlayToSlideshow "Not enough calm to take items from the shared stash, but here's the description." io
        MStore fromCStore -> do
          keyb <- askBinding
          let eqpFree = eqpFreeN b
              moveItems toCStore | toCStore == fromCStore =
                failWith "vacuus command"
              moveItems CEqp | eqpFree == 0 = failWith "no free equipment slot"
              moveItems CSha | not calmE = failWith "not calm enough"
              moveItems toCStore = do
                let k = itemK itemFull
                    kToPick | toCStore == CEqp = min eqpFree k
                            | otherwise = k
                socK <- pickNumber True kToPick
                case socK of
                  Left slides -> return $ Left slides
                  Right kChosen ->
                    return $ Right $ timedToUI
                           $ ReqMoveItems [(iid, kChosen, fromCStore, toCStore)]
              (_, (ov, kyxs)) = keyHelp keyb
              renumber y (km, (_, x1, x2)) = (km, (y, x1, x2))
              zipRenumber y = zipWith renumber [y..]
              okx = (io <> ov, zipRenumber (length (overlay io) + 2) kyxs)
              -- TODO: split okx; also handle io larger than screen size
              serverCmd cmd = case cmd of
                Alias _ cmd2 -> serverCmd cmd2
                MoveItem _ store _ _ _ -> moveItems store
                Sequence _ (MoveItem _ store _ _ _ : _) -> moveItems store
                Sequence _ (_ : (MoveItem _ store _ _ _) : _) -> moveItems store
                -- TODO: with throw, move cursor afterwards and press RET
                Project{} -> fmap timedToUI <$> projectHumanState [] INoAll
                Apply{} -> return $ Right $ timedToUI $ ReqApply iid fromCStore
                _ -> failWith "never mind"
              hackAlias = [ K.KM K.NoModifier (K.Char '.')
                          , K.KM K.NoModifier (K.Char 'd') ]
          (ekm, _) <- displayChoiceScreen False 0 [okx] $ K.escKM : hackAlias
          case ekm of
            Left km | km == K.escKM -> describeItemC c
            Left km -> case km `M.lookup` bcmdMap keyb of
              Nothing -> failWith "never mind"
              Just (_desc, _cats, cmd) -> serverCmd cmd
            Right _slot -> assert `failure` ekm
        MOwned -> do
          -- We can't move items from MOwned, because different copies may come
          -- from different stores and we can't guess player's intentions.
          found <- getsState $ findIid leader (bfid b) iid
          let !_A = assert (not (null found) `blame` ggi) ()
          let ppLoc (_, CSha) = MU.Text $ ppCStoreIn CSha <+> "of the party"
              ppLoc (b2, store) = MU.Text $ ppCStoreIn store <+> "of"
                                                             <+> bname b2
              foundTexts = map ppLoc found
              prompt2 = makeSentence ["The item is", MU.WWandW foundTexts]
          Left <$> overlayToSlideshow prompt2 io
        MStats -> assert `failure` ggi
    Left slides -> return $ Left slides

projectHumanState :: MonadClientUI m
                  => [Trigger] -> ItemDialogState
                  -> m (SlideOrCmd (RequestTimed 'Ability.AbProject))
projectHumanState ts initalState = do
  leader <- getLeaderUI
  lidV <- viewedLevel
  oldTgtMode <- getsSession stgtMode
  -- Show the targeting line, temporarily.
  modifySession $ \sess -> sess {stgtMode = Just $ TgtMode lidV}
  -- Set cursor to the personal target, permanently.
  tgt <- getsClient $ getTarget leader
  modifyClient $ \cli -> cli {scursor = fromMaybe (scursor cli) tgt}
  -- Determine the item to project.
  itemSel <- getsSession sitemSel
  outcome <- case itemSel of
    Just (fromCStore, iid) -> do
      bag <- getsState $ getActorBag leader fromCStore
      case iid `EM.lookup` bag of
        Nothing -> do  -- used up
          modifySession $ \sess -> sess {sitemSel = Nothing}
          projectHumanState ts initalState
        Just kit -> do
          itemToF <- itemToFullClient
          let i = (fromCStore, (iid, itemToF iid kit))
          projectItem ts i
    Nothing -> do
      mis <- selectItemToProject ts initalState
      case mis of
        Left slides -> return $ Left slides
        Right i -> projectItem ts i
  -- Bring back old aiming mode.
  modifySession $ \sess -> sess {stgtMode = oldTgtMode}
  -- Set personal target to eventual cursor potision, permanently.
  when (isRight outcome) $ endTargeting
  return outcome

posFromCursor :: MonadClientUI m => m (Either Msg Point)
posFromCursor = do
  leader <- getLeaderUI
  lidV <- viewedLevel
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

projectCheck :: MonadClientUI m => Point -> m (Maybe ReqFailure)
projectCheck tpos = do
  Kind.COps{cotile} <- getsState scops
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
      if not $ Tile.isWalkable cotile t
        then return $ Just ProjectBlockTerrain
        else do
          lab <- getsState $ posToActors pos lid
          if all (bproj . snd) lab
          then return Nothing
          else return $ Just ProjectBlockActor

permittedProjectClient :: MonadClientUI m
                       => [Char] -> m (ItemFull -> Either ReqFailure Bool)
permittedProjectClient triggerSyms = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  actorSk <- actorSkillsClient leader
  let skill = EM.findWithDefault 0 Ability.AbProject actorSk
  activeItems <- activeItemsClient leader
  return $ permittedProject False skill b activeItems triggerSyms

psuitReq :: MonadClientUI m
         => [Trigger] -> m (Either Msg (ItemFull -> Either ReqFailure (Point, Bool)))
psuitReq ts = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  mpos <- posFromCursor
  p <- permittedProjectClient $ triggerSymbols ts
  case mpos of
    Left err -> return $ Left err
    Right pos -> return $ Right $ \itemFull@ItemFull{itemBase} ->
      case p itemFull of
        Left slides -> Left slides
        Right False -> Right (pos, False)
        Right True -> Right (pos, totalRange itemBase >= chessDist (bpos b) pos)

selectItemToProject :: forall m. MonadClientUI m
                    => [Trigger] -> ItemDialogState
                    -> m (SlideOrCmd (CStore, (ItemId, ItemFull)))
selectItemToProject ts initalState = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  activeItems <- activeItemsClient leader
  let calmE = calmEnough b activeItems
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
          -- If target invalid, no item is considered a (suitable) missile.
          Left err -> return $ SuitsNothing err
          Right psuitReqFun ->
            return $ SuitsSomething $ either (const False) snd . psuitReqFun
      prompt = makePhrase ["What", object1, "to", verb1]
      promptGeneric = "What to fling"
  ggi <- getGroupItem
           psuit prompt promptGeneric True cLegalRaw cLegal initalState
  case ggi of
    Right ((iid, itemFull), MStore fromCStore) ->
      return $ Right (fromCStore, (iid, itemFull))
    Left slides -> return $ Left slides
    _ -> assert `failure` ggi

projectItem :: MonadClientUI m
            => [Trigger] -> (CStore, (ItemId, ItemFull))
            -> m (SlideOrCmd (RequestTimed 'Ability.AbProject))
projectItem ts (fromCStore, (iid, itemFull)) = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  activeItems <- activeItemsClient leader
  let calmE = calmEnough b activeItems
  if not calmE && fromCStore == CSha then failSer ItemNotCalm
  else do
    mpsuitReq <- psuitReq ts
    case mpsuitReq of
      Left err -> failWith err
      Right psuitReqFun ->
        case psuitReqFun itemFull of
          Left reqFail -> failSer reqFail
          Right (pos, _) -> do
            eps <- getsClient seps
            return $ Right $ ReqProject pos eps iid fromCStore

triggerSymbols :: [Trigger] -> [Char]
triggerSymbols [] = []
triggerSymbols (ApplyItem{symbol} : ts) = symbol : triggerSymbols ts
triggerSymbols (_ : ts) = triggerSymbols ts
