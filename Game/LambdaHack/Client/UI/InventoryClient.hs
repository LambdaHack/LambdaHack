-- | Inventory management.
-- TODO: document
module Game.LambdaHack.Client.UI.InventoryClient
  ( floorItemOverlay, getGroupItem, getAnyItem
  ) where

import Control.Arrow ((&&&))
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import Data.Function
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

-- | Create a list of item names.
floorItemOverlay :: MonadClient m => ItemBag -> m Overlay
floorItemOverlay bag = do
  Kind.COps{coitem} <- getsState scops
  s <- getState
  disco <- getsClient sdisco
  let is = zip (EM.assocs bag) (map Left allSlots ++ map Right [0..])
      pr ((iid, k), l) =
         makePhrase [ slotLabel l
                    , partItemWs coitem disco k (getItemBody iid s) ]
         <> " "
  return $! toOverlay $ map pr is

allItemsName :: Text
allItemsName = "Items"

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
  let p i = jsymbol i `elem` syms
      tsuitable = makePhrase [MU.Capitalize (MU.Ws itemsName)]
  getItem p tsuitable verb cLegalRaw cLegalAfterCalm True

-- | Let the human player choose any item from a list of items
-- and let his specify the number of items.
getAnyItem :: MonadClientUI m
           => MU.Part   -- ^ the verb describing the action
           -> [CStore]  -- ^ initial legal containers
           -> [CStore]  -- ^ legal containers after Calm taken into account
           -> Bool      -- ^ whether to ask, when the only item
                        --   in the starting container is suitable
           -> Bool      -- ^ whether to ask for the number of items
           -> m (SlideOrCmd ((ItemId, Item), (Int, CStore)))
getAnyItem verb cLegalRaw cLegalAfterCalm askWhenLone askNumber = do
  soc <- getItem (const True) allItemsName verb
                 cLegalRaw cLegalAfterCalm askWhenLone
  case soc of
    Left slides -> return $ Left slides
    Right (iidItem, (kAll, c)) -> do
      socK <- pickNumber askNumber kAll
      case socK of
        Left slides -> return $ Left slides
        Right k -> return $ Right (iidItem, (k, c))

data ItemDialogState = INone | ISuitable | IAll deriving Eq

-- | Let the human player choose a single, preferably suitable,
-- item from a list of items.
getItem :: MonadClientUI m
        => (Item -> Bool)  -- ^ which items to consider suitable
        -> Text            -- ^ how to describe suitable items
        -> MU.Part         -- ^ the verb describing the action
        -> [CStore]        -- ^ initial legal containers
        -> [CStore]        -- ^ legal containers after Calm taken into account
        -> Bool            -- ^ whether to ask, when the only item
                           --   in the starting container is suitable
        -> m (SlideOrCmd ((ItemId, Item), (Int, CStore)))
getItem p tsuitable verb cLegalRaw cLegalAfterCalm askWhenLone = do
  Kind.COps{corule} <- getsState scops
  let RuleKind{rsharedInventory} = Kind.stdRuleset corule
  leader <- getLeaderUI
  getCStoreBag <- getsState $ \s cstore -> getCBag (CActor leader cstore) s
  let cNotEmpty = not . EM.null . getCStoreBag
      cLegal = filter cNotEmpty cLegalAfterCalm
      storeAssocs = EM.assocs . getCStoreBag
      allAssocs = concatMap storeAssocs cLegal
  case (cLegal, allAssocs) of
    ([], _) -> do
      let cLegalInitial = filter cNotEmpty cLegalRaw
      if null cLegalInitial then do
        let tLegal = map (MU.Text . ppCStore rsharedInventory) cLegalRaw
            ppLegal = makePhrase [MU.WWxW "nor" tLegal]
        failWith $ "no items" <+> ppLegal
      else failSer ItemNotCalm
    ([cStart], [(iid, k)]) | not askWhenLone -> do
      item <- getsState $ getItemBody iid
      return $ Right ((iid, item), (k, cStart))
    (cStart : _, _) -> do
      when (CGround `elem` cLegal) $
        mapM_ (updateItemSlot leader) $ EM.keys $ getCStoreBag CGround
      let cStartPrev = if cStart == CGround
                       then if CEqp `elem` cLegal then CEqp else CInv
                       else cStart
      perform p tsuitable verb cLegal INone cStart cStartPrev

ppCStore :: Bool -> CStore -> Text
ppCStore _ CEqp = "in personal equipment"
ppCStore rsharedInventory CInv = if rsharedInventory
                                 then "in shared inventory"
                                 else "in inventory"
ppCStore _ CGround = "on the floor"

perform :: MonadClientUI m
        => (Item -> Bool)  -- ^ which items to consider suitable
        -> Text            -- ^ how to describe suitable items
        -> MU.Part         -- ^ the verb describing the action
        -> [CStore]
        -> ItemDialogState
        -> CStore
        -> CStore
        -> m (SlideOrCmd ((ItemId, Item), (Int, CStore)))
perform p tsuitable verb cLegal itemDialogState cCur cPrev = do
  assert (not $ null cLegal) skip
  Kind.COps{corule} <- getsState scops
  let RuleKind{rsharedInventory} = Kind.stdRuleset corule
  (letterSlots, numberSlots) <- getsClient sslots
  leader <- getLeaderUI
  getCStoreBag <- getsState $ \s cstore -> getCBag (CActor leader cstore) s
  let cNotEmpty = not . EM.null . getCStoreBag
      isCFull c = c `elem` cLegal && cNotEmpty c
      bag = getCStoreBag cCur
      sl = EM.filter (`EM.member` bag) letterSlots
      slNumberSlots = IM.filter (`EM.member` bag) numberSlots
      getItemData :: (a, ItemId) -> State -> ((ItemId, Item), (Int, a))
      getItemData (l, iid) s =
        ((iid, getItemBody iid s), (bag EM.! iid, l))
  is0 <- mapM (getsState . getItemData) $ EM.assocs sl
  isN0 <- mapM (getsState . getItemData) $ IM.assocs slNumberSlots
  let slP = EM.fromAscList $ map (snd . snd &&& fst . fst)
                           $ filter (\((_, item), _) -> p item) is0
      floorFull = isCFull CGround
      (floorMsg, floorKey)
        | floorFull && (isCFull CInv || isCFull CEqp) = (", -", [K.Char '-'])
        | otherwise = ("", [])
      invEqpFull = isCFull CInv && isCFull CEqp
      (invEqpMsg, invEqpKey) | invEqpFull = (", /", [K.Char '/'])
                             | otherwise = ("", [])
      (numberMsg, numberKey)
        | not $ IM.null slNumberSlots = (", 0", [K.Char '0'])
        | otherwise = ("", [])
      isp = filter (p . snd . fst) is0
      bestFull = not $ null isp
      (bestMsg, bestKey)
        | bestFull =
          let bestSlot = slotChar $ maximum $ map (snd . snd) isp
          in (", RET(" <> T.singleton bestSlot <> ")", [K.Return])
        | otherwise = ("", [])
      keys ims2 =
        let mls = map (snd . snd) ims2
            ks = map (K.Char . slotChar) mls
                 ++ [K.Char '?'] ++ floorKey ++ invEqpKey
                 ++ bestKey ++ numberKey
        in zipWith K.KM (repeat K.NoModifier) ks
      choice ims2 =
        if null ims2
        then "[?" <> floorMsg <> invEqpMsg <> numberMsg
        else let mls = map (snd . snd) ims2
                 r = slotRange mls
             in "[" <> r <> ", ?"
                <> floorMsg <> invEqpMsg <> bestMsg <> numberMsg
      isn = ppCStore rsharedInventory cCur
      prompt = makePhrase ["What to", verb MU.:> "?"]
      (ims, slOver, msg) = case itemDialogState of
        INone     -> (isp, EM.empty, prompt)
        ISuitable -> (isp, slP, tsuitable <+> isn <> ".")
        IAll      -> (is0, sl, allItemsName <+> isn <> ".")
  io <- itemOverlay bag (slOver, IM.empty)
  akm <- displayChoiceUI (msg <+> choice ims) io (keys is0)
  case akm of
    Left slides -> failSlides slides
    Right km@K.KM{..} -> do
      assert (modifier == K.NoModifier) skip
      case key of
        K.Char '?' -> case itemDialogState of
          INone -> if EM.null slP
                   then perform p tsuitable verb cLegal IAll cCur cPrev
                   else perform p tsuitable verb cLegal ISuitable cCur cPrev
          ISuitable | tsuitable /= allItemsName ->
            perform p tsuitable verb cLegal IAll cCur cPrev
          _ -> perform p tsuitable verb cLegal INone cCur cPrev
        K.Char '-' ->
          let cNext = if cCur == CGround then cPrev else CGround
          in perform p tsuitable verb cLegal itemDialogState cNext cCur
        K.Char '/' ->
          let cNext = if cCur == CInv then CEqp else CInv
          in perform p tsuitable verb cLegal itemDialogState cNext cCur
        K.Char '0' ->
          case isN0 of
            [] -> assert `failure` "no numbered items"
                              `twith` (km, slNumberSlots)
            ((iidItem), (k, _)) : _ -> return $ Right (iidItem, (k, cCur))
        K.Char l ->
          case find ((SlotChar l ==) . snd . snd) is0 of
            Nothing -> assert `failure` "unexpected slot"
                              `twith` (km, l,  is0)
            Just (iidItem, (k, _)) -> return $ Right (iidItem, (k, cCur))
        K.Return ->
          let (iidItem, (k, _)) =
                maximumBy (compare `on` snd . snd) isp
          in return $ Right (iidItem, (k, cCur))
        _ -> assert `failure` "unexpected key:" `twith` akm

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
