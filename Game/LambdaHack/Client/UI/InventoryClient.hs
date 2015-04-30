{-# LANGUAGE DataKinds #-}
-- | Inventory management and party cycling.
-- TODO: document
module Game.LambdaHack.Client.UI.InventoryClient
  ( Suitability(..)
  , getGroupItem, getAnyItems, getStoreItem
  , memberCycle, memberBack, pickLeader
  , cursorPointerFloor, cursorPointerEnemy
  , moveCursorHuman, tgtFloorHuman, tgtEnemyHuman, epsIncrHuman, tgtClearHuman
  , doLook, describeItemC
  ) where

import Control.Applicative
import Control.Exception.Assert.Sugar
import Control.Monad
import Data.Char (intToDigit)
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.CommonClient
import Game.LambdaHack.Client.ItemSlot
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.HumanCmd
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.MsgClient
import Game.LambdaHack.Client.UI.WidgetClient
import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemDescription
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Vector
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
             -> m (SlideOrCmd ((ItemId, ItemFull), ItemDialogMode))
getGroupItem psuit prompt promptGeneric cursor cLegalRaw cLegalAfterCalm = do
  let dialogState = if cursor then INoSuitable else ISuitable
  soc <- getFull psuit
                 (\_ _ cCur -> prompt <+> ppItemDialogModeFrom cCur)
                 (\_ _ cCur -> promptGeneric <+> ppItemDialogModeFrom cCur)
                 cursor cLegalRaw cLegalAfterCalm True False dialogState
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
      lastStore <- getsClient slastStore
      firstStore <-
        if lastStore `notElem` cLegalAfterCalm
        then return $! cThisActor headThisActor
        else do
          (itemSlots, organSlots) <- getsClient sslots
          let lSlots = if lastStore == COrgan then organSlots else itemSlots
          lastSlot <- getsClient slastSlot
          case EM.lookup lastSlot lSlots of
            Nothing -> return $! cThisActor headThisActor
            Just lastIid -> case EM.lookup lastIid $ getCStoreBag lastStore of
              Nothing -> return $! cThisActor headThisActor
              Just kit -> do
                let lastItemFull = itemToF lastIid kit
                    lastSuits = psuitFun lastItemFull
                    cLast = cThisActor lastStore
                return $! if lastSuits && cLast /= CGround
                          then lastStore
                          else cLast
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
  , defAction :: K.KM -> m (SlideOrCmd ([(ItemId, ItemFull)], ItemDialogMode))
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
      void $ getInitConfirms ColorFull [] $ slides <> toSlideshow Nothing [[]]
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
      keyDefs :: [(K.KM, DefItemKey m)]
      keyDefs = filter (defCond . snd) $
        [ (K.toKM K.NoModifier $ K.Char '?', DefItemKey
           { defLabel = "?"
           , defCond = not (EM.null bag)
           , defAction = \_ -> recCall numPrefix cCur cRest
                               $ case itemDialogState of
               INoSuitable -> if EM.null bagSuit then IAll else ISuitable
               ISuitable -> if suitsEverything then INoAll else IAll
               IAll -> if EM.null bag then INoSuitable else INoAll
               INoAll -> if suitsEverything then ISuitable else INoSuitable
           })
        , (K.toKM K.NoModifier $ K.Char '/', DefItemKey
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
        , (K.toKM K.NoModifier $ K.Char '*', DefItemKey
           { defLabel = "*"
           , defCond = permitMulitple && not (EM.null multipleSlots)
           , defAction = \_ ->
               let eslots = EM.elems multipleSlots
               in return $ Right $ getMultResult eslots
           })
        , (K.toKM K.NoModifier K.Return, DefItemKey
           { defLabel = if lastSlot `EM.member` labelItemSlotsOpen
                        then let l = makePhrase [slotLabel lastSlot]
                             in "RET(" <> l <> ")"  -- l is on the screen list
                        else "RET"
           , defCond = not (EM.null labelItemSlotsOpen)
           , defAction = \_ -> case EM.lookup lastSlot labelItemSlotsOpen of
               Just iid -> return $ Right $ getResult iid
               Nothing -> case EM.minViewWithKey labelItemSlotsOpen of
                 Nothing -> assert `failure` "labelItemSlotsOpen empty"
                                   `twith` labelItemSlotsOpen
                 Just ((l, _), _) -> do
                   modifyClient $ \cli ->
                     cli { slastSlot = l
                         , slastStore = storeFromMode cCur }
                   recCall numPrefix cCur cRest itemDialogState
           })
        , let km = M.findWithDefault (K.toKM K.NoModifier K.Tab)
                                     MemberCycle brevMap
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
        , let km = M.findWithDefault (K.toKM K.NoModifier K.BackTab)
                                     MemberBack brevMap
          in (km, DefItemKey
           { defLabel = K.showKM km
           , defCond = not (cCur == MOwned || autoDun || null hs)
           , defAction = \_ -> do
               err <- memberBack False
               let !_A = assert (err == mempty `blame` err) ()
               (cCurUpd, cRestUpd) <- legalWithUpdatedLeader cCur cRest
               recCall numPrefix cCurUpd cRestUpd itemDialogState
           })
        , let km = M.findWithDefault (K.toKM K.NoModifier (K.KP '/'))
                                     TgtFloor brevMap
          in cursorCmdDef False km tgtFloorHuman
        , let hackyCmd = Macro "" ["KP_Divide"]  -- no keypad, but arrows enough
              km = M.findWithDefault (K.toKM K.NoModifier K.RightButtonPress)
                                     hackyCmd brevMap
          in cursorCmdDef False km tgtEnemyHuman
        , let km = M.findWithDefault (K.toKM K.NoModifier (K.KP '*'))
                                     TgtEnemy brevMap
          in cursorCmdDef False km tgtEnemyHuman
        , let hackyCmd = Macro "" ["KP_Multiply"]  -- no keypad, but arrows OK
              km = M.findWithDefault (K.toKM K.NoModifier K.RightButtonPress)
                                     hackyCmd brevMap
          in cursorCmdDef False km tgtEnemyHuman
        , let km = M.findWithDefault (K.toKM K.NoModifier K.BackSpace)
                                     TgtClear brevMap
          in cursorCmdDef False km tgtClearHuman
        ]
        ++ numberPrefixes
        ++ [ let plusMinus = K.Char $ if b then '+' else '-'
                 km = M.findWithDefault (K.toKM K.NoModifier plusMinus)
                                        (EpsIncr b) brevMap
             in cursorCmdDef False km (epsIncrHuman b)
           | b <- [True, False]
           ]
        ++ arrows
        ++ [
          let km = M.findWithDefault (K.toKM K.NoModifier K.MiddleButtonPress)
                                     CursorPointerEnemy brevMap
          in cursorCmdDef False km (cursorPointerEnemy False False)
        , let km = M.findWithDefault (K.toKM K.Shift K.MiddleButtonPress)
                                     CursorPointerFloor brevMap
          in cursorCmdDef False km (cursorPointerFloor False False)
        , let km = M.findWithDefault (K.toKM K.NoModifier K.RightButtonPress)
                                     TgtPointerEnemy brevMap
          in cursorCmdDef True km (cursorPointerEnemy True True)
        ]
      prefixCmdDef d =
        (K.toKM K.NoModifier $ K.Char (intToDigit d), DefItemKey
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
                 void $ getInitConfirms ColorFull []
                      $ look <> toSlideshow Nothing [[]]
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
        , defAction = \K.KM{key} -> case key of
            K.Char l -> case EM.lookup (SlotChar numPrefix l) bagItemSlots of
              Nothing -> assert `failure` "unexpected slot"
                                `twith` (l, bagItemSlots)
              Just iid -> return $ Right $ getResult iid
            _ -> assert `failure` "unexpected key:" `twith` K.showKey key
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
  io <- case cCur of
    MStats -> statsOverlay leader -- TODO: describe each stat when selected
    _ -> itemOverlay (storeFromMode cCur) (blid body) bagFiltered
  runDefItemKey keyDefs lettersDef io bagItemSlots promptChosen

statsOverlay :: MonadClient m => ActorId -> m Overlay
statsOverlay aid = do
  b <- getsState $ getActorBody aid
  activeItems <- activeItemsClient aid
  let block n = n + if braced b then 50 else 0
      prSlot :: (IK.EqpSlot, Int -> Text) -> Text
      prSlot (eqpSlot, f) =
        let fullText t =
              "    "
              <> makePhrase [ MU.Text $ T.justifyLeft 22 ' '
                                      $ IK.slotName eqpSlot
                            , MU.Text t ]
              <> "  "
            valueText = f $ sumSlotNoFilter eqpSlot activeItems
        in fullText valueText
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
      prAbility :: Ability.Ability -> Text
      prAbility ability =
        let fullText t =
              "    "
              <> makePhrase [ MU.Text $ T.justifyLeft 22 ' '
                              $ "ability" <+> tshow ability
                            , MU.Text t ]
              <> "  "
            valueText = tshow $ EM.findWithDefault 0 ability skills
        in fullText valueText
      abilityList = [minBound..maxBound]
  return $! toOverlay $ map prSlot slotList ++ map prAbility abilityList

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
              -> Overlay
              -> EM.EnumMap SlotChar ItemId
              -> Text
              -> m (SlideOrCmd ([(ItemId, ItemFull)], ItemDialogMode))
runDefItemKey keyDefs lettersDef io labelItemSlots prompt = do
  let itemKeys =
        let slotKeys = map (K.Char . slotChar) (EM.keys labelItemSlots)
            defKeys = map fst keyDefs
        in map (K.toKM K.NoModifier) slotKeys ++ defKeys
      choice = let letterRange = defLabel lettersDef
                   keyLabelsRaw = letterRange : map (defLabel . snd) keyDefs
                   keyLabels = filter (not . T.null) keyLabelsRaw
               in "[" <> T.intercalate ", " (nub keyLabels)
  akm <- displayChoiceUI (prompt <+> choice) io itemKeys
  case akm of
    Left slides -> failSlides slides
    Right km ->
      case lookup km{K.pointer=Nothing} keyDefs of
        Just keyDef -> defAction keyDef km
        Nothing -> defAction lettersDef km

pickNumber :: MonadClientUI m => Bool -> Int -> m (SlideOrCmd Int)
pickNumber askNumber kAll = do
  let kDefault = kAll
  if askNumber && kAll > 1 then do
    let tDefault = tshow kDefault
        kbound = min 9 kAll
        kprompt = "Choose number [1-" <> tshow kbound
                  <> ", RET(" <> tDefault <> ")"
        kkeys = map (K.toKM K.NoModifier)
                $ map (K.Char . Char.intToDigit) [1..kbound]
                  ++ [K.Return]
    kkm <- displayChoiceUI kprompt emptyOverlay kkeys
    case kkm of
      Left slides -> failSlides slides
      Right K.KM{key} ->
        case key of
          K.Char l -> return $ Right $ Char.digitToInt l
          K.Return -> return $ Right kDefault
          _ -> assert `failure` "unexpected key:" `twith` kkm
  else return $ Right kAll

-- | Switches current member to the next on the level, if any, wrapping.
memberCycle :: MonadClientUI m => Bool -> m Slideshow
memberCycle verbose = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  leader <- getLeaderUI
  body <- getsState $ getActorBody leader
  hs <- partyAfterLeader leader
  let autoLvl = snd $ autoDungeonLevel fact
  case filter (\(_, b) -> blid b == blid body) hs of
    _ | autoLvl -> failMsg $ showReqFailure NoChangeLvlLeader
    [] -> failMsg "cannot pick any other member on this level"
    (np, b) : _ -> do
      success <- pickLeader verbose np
      let !_A = assert (success `blame` "same leader" `twith` (leader, np, b)) ()
      return mempty

-- | Switches current member to the previous in the whole dungeon, wrapping.
memberBack :: MonadClientUI m => Bool -> m Slideshow
memberBack verbose = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  leader <- getLeaderUI
  hs <- partyAfterLeader leader
  let autoDun = fst $ autoDungeonLevel fact
  case reverse hs of
    _ | autoDun -> failMsg $ showReqFailure NoChangeDunLeader
    [] -> failMsg "no other member in the party"
    (np, b) : _ -> do
      success <- pickLeader verbose np
      let !_A = assert (success `blame` "same leader" `twith` (leader, np, b)) ()
      return mempty

partyAfterLeader :: MonadStateRead m => ActorId -> m [(ActorId, Actor)]
partyAfterLeader leader = do
  faction <- getsState $ bfid . getActorBody leader
  allA <- getsState $ EM.assocs . sactorD
  let factionA = filter (\(_, body) ->
        not (bproj body) && bfid body == faction) allA
      hs = sortBy (comparing keySelected) factionA
      i = fromMaybe (-1) $ findIndex ((== leader) . fst) hs
      (lt, gt) = (take i hs, drop (i + 1) hs)
  return $! gt ++ lt

-- | Select a faction leader. False, if nothing to do.
pickLeader :: MonadClientUI m => Bool -> ActorId -> m Bool
pickLeader verbose aid = do
  leader <- getLeaderUI
  stgtMode <- getsClient stgtMode
  if leader == aid
    then return False -- already picked
    else do
      pbody <- getsState $ getActorBody aid
      let !_A = assert (not (bproj pbody)
                        `blame` "projectile chosen as the leader"
                        `twith` (aid, pbody)) ()
      -- Even if it's already the leader, give his proper name, not 'you'.
      let subject = partActor pbody
      when verbose $ msgAdd $ makeSentence [subject, "picked as a leader"]
      -- Update client state.
      s <- getState
      modifyClient $ updateLeader aid s
      -- Move the cursor, if active, to the new level.
      case stgtMode of
        Nothing -> return ()
        Just _ ->
          modifyClient $ \cli -> cli {stgtMode = Just $ TgtMode $ blid pbody}
      -- Inform about items, etc.
      lookMsg <- lookAt False "" True (bpos pbody) aid ""
      when verbose $ msgAdd lookMsg
      return True

cursorPointerFloor :: MonadClientUI m => Bool -> Bool -> m Slideshow
cursorPointerFloor verbose addMoreMsg = do
  km <- getsClient slastKM
  lidV <- viewedLevel
  Level{lxsize, lysize} <- getLevel lidV
  case K.pointer km of
    Just(newPos@Point{..}) | px >= 0 && py >= 0
                             && px < lxsize && py < lysize -> do
      let scursor = TPoint lidV newPos
      modifyClient $ \cli -> cli {scursor, stgtMode = Just $ TgtMode lidV}
      if verbose then
        doLook addMoreMsg
      else do
        displayPush ""  -- flash the targeting line and path
        displayDelay  -- for a bit longer
        return mempty
    _ -> do
      stopPlayBack
      return mempty

cursorPointerEnemy :: MonadClientUI m => Bool -> Bool -> m Slideshow
cursorPointerEnemy verbose addMoreMsg = do
  km <- getsClient slastKM
  lidV <- viewedLevel
  Level{lxsize, lysize} <- getLevel lidV
  case K.pointer km of
    Just(newPos@Point{..}) | px >= 0 && py >= 0
                             && px < lxsize && py < lysize -> do
      bsAll <- getsState $ actorAssocs (const True) lidV
      let scursor =
            case find (\(_, m) -> bpos m == newPos) bsAll of
              Just (im, _) -> TEnemy im True
              Nothing -> TPoint lidV newPos
      modifyClient $ \cli -> cli {scursor, stgtMode = Just $ TgtMode lidV}
      if verbose then
        doLook addMoreMsg
      else do
        displayPush ""  -- flash the targeting line and path
        displayDelay  -- for a bit longer
        return mempty
    _ -> do
      stopPlayBack
      return mempty

-- | Move the cursor. Assumes targeting mode.
moveCursorHuman :: MonadClientUI m => Vector -> Int -> m Slideshow
moveCursorHuman dir n = do
  leader <- getLeaderUI
  stgtMode <- getsClient stgtMode
  let lidV = maybe (assert `failure` leader) tgtLevelId stgtMode
  Level{lxsize, lysize} <- getLevel lidV
  lpos <- getsState $ bpos . getActorBody leader
  scursor <- getsClient scursor
  cursorPos <- cursorToPos
  let cpos = fromMaybe lpos cursorPos
      shiftB pos = shiftBounded lxsize lysize pos dir
      newPos = iterate shiftB cpos !! n
  if newPos == cpos then failMsg "never mind"
  else do
    let tgt = case scursor of
          TVector{} -> TVector $ newPos `vectorToFrom` lpos
          _ -> TPoint lidV newPos
    modifyClient $ \cli -> cli {scursor = tgt}
    doLook False

-- | Cycle targeting mode. Do not change position of the cursor,
-- switch among things at that position.
tgtFloorHuman :: MonadClientUI m => m Slideshow
tgtFloorHuman = do
  lidV <- viewedLevel
  leader <- getLeaderUI
  lpos <- getsState $ bpos . getActorBody leader
  cursorPos <- cursorToPos
  scursor <- getsClient scursor
  stgtMode <- getsClient stgtMode
  bsAll <- getsState $ actorAssocs (const True) lidV
  let cursor = fromMaybe lpos cursorPos
      tgt = case scursor of
        _ | isNothing stgtMode ->  -- first key press: keep target
          scursor
        TEnemy a True -> TEnemy a False
        TEnemy{} -> TPoint lidV cursor
        TEnemyPos{} -> TPoint lidV cursor
        TPoint{} -> TVector $ cursor `vectorToFrom` lpos
        TVector{} ->
          -- For projectiles, we pick here the first that would be picked
          -- by '*', so that all other projectiles on the tile come next,
          -- without any intervening actors from other tiles.
          case find (\(_, m) -> Just (bpos m) == cursorPos) bsAll of
            Just (im, _) -> TEnemy im True
            Nothing -> TPoint lidV cursor
  modifyClient $ \cli -> cli {scursor = tgt, stgtMode = Just $ TgtMode lidV}
  doLook False

tgtEnemyHuman :: MonadClientUI m => m Slideshow
tgtEnemyHuman = do
  lidV <- viewedLevel
  leader <- getLeaderUI
  lpos <- getsState $ bpos . getActorBody leader
  cursorPos <- cursorToPos
  scursor <- getsClient scursor
  stgtMode <- getsClient stgtMode
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  bsAll <- getsState $ actorAssocs (const True) lidV
  let ordPos (_, b) = (chessDist lpos $ bpos b, bpos b)
      dbs = sortBy (comparing ordPos) bsAll
      pickUnderCursor =  -- switch to the enemy under cursor, if any
        let i = fromMaybe (-1)
                $ findIndex ((== cursorPos) . Just . bpos . snd) dbs
        in splitAt i dbs
      (permitAnyActor, (lt, gt)) = case scursor of
            TEnemy a permit | isJust stgtMode ->  -- pick next enemy
              let i = fromMaybe (-1) $ findIndex ((== a) . fst) dbs
              in (permit, splitAt (i + 1) dbs)
            TEnemy a permit ->  -- first key press, retarget old enemy
              let i = fromMaybe (-1) $ findIndex ((== a) . fst) dbs
              in (permit, splitAt i dbs)
            TEnemyPos _ _ _ permit -> (permit, pickUnderCursor)
            _ -> (False, pickUnderCursor)  -- the sensible default is only-foes
      gtlt = gt ++ lt
      isEnemy b = isAtWar fact (bfid b)
                  && not (bproj b)
      lf = filter (isEnemy . snd) gtlt
      tgt | permitAnyActor = case gtlt of
        (a, _) : _ -> TEnemy a True
        [] -> scursor  -- no actors in sight, stick to last target
          | otherwise = case lf of
        (a, _) : _ -> TEnemy a False
        [] -> scursor  -- no seen foes in sight, stick to last target
  -- Register the chosen enemy, to pick another on next invocation.
  modifyClient $ \cli -> cli {scursor = tgt, stgtMode = Just $ TgtMode lidV}
  doLook False

-- | Tweak the @eps@ parameter of the targeting digital line.
epsIncrHuman :: MonadClientUI m => Bool -> m Slideshow
epsIncrHuman b = do
  stgtMode <- getsClient stgtMode
  if isJust stgtMode
    then do
      modifyClient $ \cli -> cli {seps = seps cli + if b then 1 else -1}
      return mempty
    else failMsg "never mind"  -- no visual feedback, so no sense

tgtClearHuman :: MonadClientUI m => m Slideshow
tgtClearHuman = do
  leader <- getLeaderUI
  tgt <- getsClient $ getTarget leader
  case tgt of
    Just _ -> do
      modifyClient $ updateTarget leader (const Nothing)
      return mempty
    Nothing -> do
      scursorOld <- getsClient scursor
      b <- getsState $ getActorBody leader
      let scursor = case scursorOld of
            TEnemy _ permit -> TEnemy leader permit
            TEnemyPos _ _ _ permit -> TEnemy leader permit
            TPoint{} -> TPoint (blid b) (bpos b)
            TVector{} -> TVector (Vector 0 0)
      modifyClient $ \cli -> cli {scursor}
      doLook False

-- | Perform look around in the current position of the cursor.
-- Normally expects targeting mode and so that a leader is picked.
doLook :: MonadClientUI m => Bool -> m Slideshow
doLook addMoreMsg = do
  Kind.COps{cotile=Kind.Ops{ouniqGroup}} <- getsState scops
  let unknownId = ouniqGroup "unknown space"
  stgtMode <- getsClient stgtMode
  case stgtMode of
    Nothing -> return mempty
    Just tgtMode -> do
      leader <- getLeaderUI
      let lidV = tgtLevelId tgtMode
      lvl <- getLevel lidV
      cursorPos <- cursorToPos
      per <- getPerFid lidV
      b <- getsState $ getActorBody leader
      let p = fromMaybe (bpos b) cursorPos
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
                     desc = if not (null rest)  -- many actors
                            then ""
                            else case itemDisco $ itemToF (btrunk body) (1, []) of
                              Nothing -> ""
                              Just ItemDisco{itemKind} -> IK.idesc itemKind
                     pdesc = if desc == "" then "" else "(" <> desc <> ")"
                 in makeSentence [MU.SubjectVerbSg subject verb] <+> pdesc
          vis | lvl `at` p == unknownId = "that is"
              | not canSee = "you remember"
              | not aims = "you are aware of"
              | otherwise = "you see"
      -- Show general info about current position.
      lookMsg <- lookAt True vis canSee p leader enemyMsg
{- targeting is kind of a menu (or at least mode), so this is menu inside
   a menu, which is messy, hence disabled until UI overhauled:
      -- Check if there's something lying around at current position.
      is <- getsState $ getCBag $ CFloor lidV p
      if EM.size is <= 2 then
        promptToSlideshow lookMsg
      else do
        msgAdd lookMsg  -- TODO: do not add to history
        floorItemOverlay lidV p
-}
      promptToSlideshow $ lookMsg <+> if addMoreMsg then moreMsg else ""


-- | Create a list of item names.
_floorItemOverlay :: MonadClientUI m
                  => LevelId -> Point
                  -> m (SlideOrCmd (RequestTimed 'Ability.AbMoveItem))
_floorItemOverlay _lid _p = describeItemC MOwned {-CFloor lid p-}

describeItemC :: MonadClientUI m
              => ItemDialogMode
              -> m (SlideOrCmd (RequestTimed 'Ability.AbMoveItem))
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
          let prompt2 = "Where to move the item?"
              fstores :: [(K.Key, (CStore, Text))]
              fstores =
                filter ((/= fromCStore) . fst . snd) $
                  [ (K.Char 'e', (CEqp, "'e'quipment"))
                  , (K.Char 'p', (CInv, "inventory 'p'ack")) ]
                  ++ [ (K.Char 's', (CSha, "shared 's'tash")) | calmE ]
                  ++ [ (K.Char 'g', (CGround, "'g'round")) ]
              choice = "[" <> T.intercalate ", " (map (snd . snd) fstores)
              keys = map (K.toKM K.NoModifier . K.Char) "epsg"
          akm <- displayChoiceUI (prompt2 <+> choice) io keys
          case akm of
            Left slides -> failSlides slides
            Right km -> do
              socK <- pickNumber True $ itemK itemFull
              case socK of
                Left slides -> return $ Left slides
                Right k -> do
                  let lr toCStore = return $ Right $ ReqMoveItems
                                     [(iid, k, fromCStore, toCStore)]
                  case lookup (K.key km) fstores of
                    Just (store, _) -> lr store
                    Nothing -> return $ Left mempty
        MOwned -> do
          -- We can't move items from MOwned, because different copies may come
          -- from different stores and we can't guess player's intentions.
          found <- getsState $ findIid leader (bfid b) iid
          let !_A = assert (not (null found) `blame` ggi) ()
          let ppLoc (_, CSha) = MU.Text $ ppCStoreIn CSha <+> "of the party"
              ppLoc (b2, store) = MU.Text $ ppCStoreIn store <+> "of" <+> bname b2
              foundTexts = map ppLoc found
              prompt2 = makeSentence ["The item is", MU.WWandW foundTexts]
          Left <$> overlayToSlideshow prompt2 io
        MStats -> assert `failure` ggi
    Left slides -> return $ Left slides
