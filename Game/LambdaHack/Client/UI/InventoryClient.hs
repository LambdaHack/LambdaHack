-- | Inventory management.
-- TODO: document
module Game.LambdaHack.Client.UI.InventoryClient
  ( getGroupItem, getAnyItem, getStoreItem
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.IntMap.Strict as IM
import Data.List
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.CommonClient
import Game.LambdaHack.Client.ItemSlot
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.MsgClient
import Game.LambdaHack.Client.UI.WidgetClient
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.RuleKind

-- | Let a human player choose any item from a given group.
-- Note that this does not guarantee the chosen item belongs to the group,
-- as the player can override the choice.
getGroupItem :: MonadClientUI m
             => [Char]    -- ^ accepted item symbols
             -> MU.Part   -- ^ name of the item group
             -> MU.Part   -- ^ the verb describing the action
             -> [CStore]  -- ^ initial legal containers
             -> [CStore]  -- ^ legal containers after Calm taken into account
             -> m (SlideOrCmd ((ItemId, Item), (Int, CStore)))
getGroupItem syms itemsName verb cLegalRaw cLegalAfterCalm = do
  leader <- getLeaderUI
  getCStoreBag <- getsState $ \s cstore -> getCBag (CActor leader cstore) s
  let cNotEmpty = not . EM.null . getCStoreBag
      cLegal = filter cNotEmpty cLegalAfterCalm  -- don't display empty stores
      p i = jsymbol i `elem` syms
      tsuitable = makePhrase [MU.Capitalize (MU.Ws itemsName)]
  getItem p tsuitable verb cLegalRaw cLegal True INone

-- | Let the human player choose any item from a list of items
-- and let him specify the number of items.
getAnyItem :: MonadClientUI m
           => MU.Part   -- ^ the verb describing the action
           -> [CStore]  -- ^ initial legal containers
           -> [CStore]  -- ^ legal containers after Calm taken into account
           -> Bool      -- ^ whether to ask, when the only item
                        --   in the starting container is suitable
           -> Bool      -- ^ whether to ask for the number of items
           -> m (SlideOrCmd ((ItemId, Item), (Int, CStore)))
getAnyItem verb cLegalRaw cLegalAfterCalm askWhenLone askNumber = do
  soc <- getItem (const True) "Items" verb
                 cLegalRaw cLegalAfterCalm askWhenLone INone
  case soc of
    Left slides -> return $ Left slides
    Right (iidItem, (kAll, c)) -> do
      socK <- pickNumber askNumber kAll
      case socK of
        Left slides -> return $ Left slides
        Right k -> return $ Right (iidItem, (k, c))

-- | Display all items from a store and let the human player choose any
-- or switch to any other non-empty store.
getStoreItem :: MonadClientUI m
             => Text      -- ^ how to describe displayed items
             -> MU.Part   -- ^ the verb describing the action
             -> CStore  -- ^ initial container
             -> m (SlideOrCmd ((ItemId, Item), (Int, CStore)))
getStoreItem itemDescr verb cInitial = do
  let cLegalRaw = cInitial : delete cInitial [minBound..maxBound]
  getItem (const True) itemDescr verb cLegalRaw cLegalRaw True ISuitable

data ItemDialogState = INone | ISuitable | IAll deriving Eq

-- | Let the human player choose a single, preferably suitable,
-- item from a list of items.
getItem :: MonadClientUI m
        => (Item -> Bool)   -- ^ which items to consider suitable
        -> Text             -- ^ how to describe suitable items
        -> MU.Part          -- ^ the verb describing the action
        -> [CStore]         -- ^ initial legal containers
        -> [CStore]         -- ^ legal containers after Calm taken into account
        -> Bool             -- ^ whether to ask, when the only item
                            --   in the starting container is suitable
        -> ItemDialogState  -- ^ the dialog state to start in
        -> m (SlideOrCmd ((ItemId, Item), (Int, CStore)))
getItem p tsuitable verb cLegalRaw cLegal askWhenLone initalState = do
  Kind.COps{corule} <- getsState scops
  let RuleKind{rsharedInventory} = Kind.stdRuleset corule
  leader <- getLeaderUI
  getCStoreBag <- getsState $ \s cstore -> getCBag (CActor leader cstore) s
  let storeAssocs = EM.assocs . getCStoreBag
      allAssocs = concatMap storeAssocs cLegal
      rawAssocs = concatMap storeAssocs cLegalRaw
  case (cLegal, allAssocs) of
    ([cStart], [(iid, k)]) | not askWhenLone -> do
      item <- getsState $ getItemBody iid
      return $ Right ((iid, item), (k, cStart))
    (cStart : _, _ : _) -> do
      when (CGround `elem` cLegal) $
        mapM_ (updateItemSlot leader) $ EM.keys $ getCStoreBag CGround
      let cStartPrev = if cStart == CGround
                       then if CEqp `elem` cLegal then CEqp else CInv
                       else cStart
      transition p tsuitable verb cLegal initalState cStart cStartPrev
    _ -> if null rawAssocs then do
           let tLegal = map (MU.Text . ppCStore rsharedInventory) cLegalRaw
               ppLegal = makePhrase [MU.WWxW "nor" tLegal]
           failWith $ "no items" <+> ppLegal
         else failSer ItemNotCalm

ppCStore :: Bool -> CStore -> Text
ppCStore _ CEqp = "in personal equipment"
ppCStore rsharedInventory CInv = if rsharedInventory
                                 then "in shared inventory"
                                 else "in inventory"
ppCStore _ CGround = "on the floor"

data DefItemKey m = DefItemKey
  { defLabel  :: Text
  , defCond   :: Bool
  , defAction :: K.Key -> m (SlideOrCmd ((ItemId, Item), (Int, CStore)))
  }

transition :: forall m. MonadClientUI m
           => (Item -> Bool)  -- ^ which items to consider suitable
           -> Text            -- ^ how to describe suitable items
           -> MU.Part         -- ^ the verb describing the action
           -> [CStore]
           -> ItemDialogState
           -> CStore
           -> CStore
           -> m (SlideOrCmd ((ItemId, Item), (Int, CStore)))
transition p tsuitable verb cLegal itemDialogState cCur cPrev = do
  assert (not $ null cLegal) skip  -- and all in cLegal are non-empty
  Kind.COps{corule} <- getsState scops
  let RuleKind{rsharedInventory} = Kind.stdRuleset corule
  (letterSlots, numberSlots) <- getsClient sslots
  leader <- getLeaderUI
  bag <- getsState $ getCBag (CActor leader cCur)
  let getResult :: ItemId -> State -> ((ItemId, Item), (Int, CStore))
      getResult iid s = ((iid, getItemBody iid s), (bag EM.! iid, cCur))
      bagLetterSlots = EM.filter (`EM.member` bag) letterSlots
      bagNumberSlots = IM.filter (`EM.member` bag) numberSlots
  suitableLetterSlots <- getsState $ \s ->
    EM.filter (p . flip getItemBody s) bagLetterSlots
  suitableNumberSlots <- getsState $ \s ->
    IM.filter (p . flip getItemBody s) bagNumberSlots
  let keyDefs :: [(K.Key, DefItemKey m)]
      keyDefs = filter (defCond . snd)
        [ (K.Char '?', DefItemKey
           { defLabel = "?"
           , defCond = True
           , defAction = \_ -> case itemDialogState of
               INone ->
                 if EM.null suitableLetterSlots && IM.null suitableNumberSlots
                 then transition p tsuitable verb cLegal IAll cCur cPrev
                 else transition p tsuitable verb cLegal ISuitable cCur cPrev
               ISuitable | suitableLetterSlots /= bagLetterSlots
                           || suitableNumberSlots /= bagNumberSlots ->
                 transition p tsuitable verb cLegal IAll cCur cPrev
               _ -> transition p tsuitable verb cLegal INone cCur cPrev
           })
        , (K.Char '-', DefItemKey
           { defLabel = "-"
           , defCond = CGround `elem` cLegal
                       && (CInv `elem` cLegal || CEqp `elem` cLegal)
           , defAction = \_ ->
               let cNext = if cCur == CGround then cPrev else CGround
               in transition p tsuitable verb cLegal itemDialogState cNext cCur
           })
        , (K.Char '/', DefItemKey
           { defLabel = "/"
           , defCond = CInv `elem` cLegal && CEqp `elem` cLegal
           , defAction = \_ ->
               let cNext = if cCur == CInv then CEqp else CInv
               in transition p tsuitable verb cLegal itemDialogState cNext cCur
           })
        , (K.Return, DefItemKey
           { defLabel = case EM.maxViewWithKey suitableLetterSlots of
               Nothing -> assert `failure` "no suitable items"
                                 `twith` suitableLetterSlots
               Just ((l, _), _) -> "RET(" <> T.singleton (slotChar l) <> ")"
           , defCond = not $ EM.null suitableLetterSlots
           , defAction = \_ -> case EM.maxView suitableLetterSlots of
               Nothing -> assert `failure` "no suitable items"
                                 `twith` suitableLetterSlots
               Just (iid, _) -> fmap Right $ getsState $ getResult iid
           })
        , (K.Char '0', DefItemKey  -- TODO: accept any number and pick the item
           { defLabel = "0"
           , defCond = not $ IM.null bagNumberSlots
           , defAction = \_ -> case IM.minView bagNumberSlots of
               Nothing -> assert `failure` "no numbered items"
                                 `twith` bagNumberSlots
               Just (iid, _) -> fmap Right $ getsState $ getResult iid
           })
        ]
      lettersDef :: DefItemKey m
      lettersDef = DefItemKey
        { defLabel = slotRange $ EM.keys labelLetterSlots
        , defCond = True
        , defAction = \key -> case key of
            K.Char l -> case EM.lookup (SlotChar l) bagLetterSlots of
              Nothing -> assert `failure` "unexpected slot"
                                `twith` (l, bagLetterSlots)
              Just iid -> fmap Right $ getsState $ getResult iid
            _ -> assert `failure` "unexpected key:" `twith` K.showKey key
        }
      ppCur = ppCStore rsharedInventory cCur
      (labelLetterSlots, overLetterSlots, overNumberSlots, prompt) =
        case itemDialogState of
          INone     -> (suitableLetterSlots,
                        EM.empty, IM.empty,
                        makePhrase ["What to", verb MU.:> "?"])
          ISuitable -> (suitableLetterSlots,
                        suitableLetterSlots, suitableNumberSlots,
                        tsuitable <+> ppCur <> ":")
          IAll      -> (bagLetterSlots,
                        bagLetterSlots, bagNumberSlots,
                        "Items" <+> ppCur <> ":")
  io <- itemOverlay bag (overLetterSlots, overNumberSlots)
  runDefItemKey keyDefs lettersDef io labelLetterSlots prompt

runDefItemKey :: MonadClientUI m
              => [(K.Key, DefItemKey m)]
              -> DefItemKey m
              -> Overlay
              -> EM.EnumMap SlotChar ItemId
              -> Text
              -> m (SlideOrCmd ((ItemId, Item), (Int, CStore)))
runDefItemKey keyDefs lettersDef io labelLetterSlots prompt = do
  let itemKeys =
        let slotKeys = map (K.Char . slotChar) (EM.keys labelLetterSlots)
            defKeys = map fst keyDefs
        in zipWith K.KM (repeat K.NoModifier) $ slotKeys ++ defKeys
      choice = let letterRange = defLabel lettersDef
                   letterLabel | T.null letterRange = []
                               | otherwise = [letterRange]
                   keyLabels = letterLabel ++ map (defLabel . snd) keyDefs
               in "[" <> T.intercalate ", " keyLabels
  akm <- displayChoiceUI (prompt <+> choice) io itemKeys
  case akm of
    Left slides -> failSlides slides
    Right K.KM{..} -> do
      assert (modifier == K.NoModifier) skip
      case lookup key keyDefs of
        Just keyDef -> defAction keyDef key
        Nothing -> defAction lettersDef key

pickNumber :: MonadClientUI m => Bool -> Int -> m (SlideOrCmd Int)
pickNumber askNumber kAll = do
  let kDefault = kAll
  if askNumber && kAll > 1 then do
    let tDefault = tshow kDefault
        kbound = min 9 kAll
        kprompt = "Choose number [1-" <> tshow kbound
                  <> ", RET(" <> tDefault <> ")"
        kkeys = zipWith K.KM (repeat K.NoModifier)
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
