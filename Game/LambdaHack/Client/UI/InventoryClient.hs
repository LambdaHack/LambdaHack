-- | Inventory management.
-- TODO: document
module Game.LambdaHack.Client.UI.InventoryClient
  ( floorItemOverlay, getGroupItem, getAnyItem
  ) where

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

-- | Let a human player choose any item with a given group name.
-- Note that this does not guarantee the chosen item belongs to the group,
-- as the player can override the choice.
getGroupItem :: MonadClientUI m
             => MU.Part   -- ^ name of the group
             -> [Char]    -- ^ accepted item symbols
             -> MU.Part   -- ^ the verb of the prompt
             -> [CStore]  -- ^ initial legal containers
             -> [CStore]  -- ^ legal containers after Calm taken into account
             -> Bool      -- ^ whether to ask for the number of items
             -> Bool      -- ^ whether the default is all, instead of one
             -> m (SlideOrCmd ((ItemId, Item), (Int, CStore)))
getGroupItem itemsName syms prompt = do
  let choice i = jsymbol i `elem` syms
      header = makePhrase [MU.Capitalize (MU.Ws itemsName)]
  getItem True prompt choice header

-- | Let the human player choose any item from a list of items.
getAnyItem :: MonadClientUI m
           => Bool
           -> MU.Part
           -> [CStore]
           -> [CStore]
           -> Bool
           -> Bool
           -> m (SlideOrCmd ((ItemId, Item), (Int, CStore)))
getAnyItem askWhenLone prompt =
  getItem askWhenLone prompt (const True) allItemsName

data ItemDialogState = INone | ISuitable | IAll deriving Eq

-- | Let the human player choose a single, preferably suitable,
-- item from a list of items.
getItem :: forall m. MonadClientUI m
        => Bool            -- ^ whether to ask if the item alone
                           --   in the starting container and suitable
        -> MU.Part         -- ^ the verb of the prompt
        -> (Item -> Bool)  -- ^ which items to consider suitable
        -> Text            -- ^ how to describe suitable items
        -> [CStore]        -- ^ initial legal containers
        -> [CStore]        -- ^ legal containers after Calm taken into account
        -> Bool            -- ^ whether to ask for the number of items
        -> Bool            -- ^ whether the default is all, instead of one
        -> m (SlideOrCmd ((ItemId, Item), (Int, CStore)))
getItem _ _ _ _ _ [] _ _ = failSer ItemNotCalm
getItem askWhenLone verb p ptext cLegalRaw cLegalAfterCalm
        askNumber allNumber = do
 Kind.COps{corule} <- getsState scops
 let RuleKind{rsharedInventory} = Kind.stdRuleset corule
     ppCStore CEqp = "in personal equipment"
     ppCStore CInv = if rsharedInventory
                     then "in shared inventory"
                     else "in inventory"
     ppCStore CGround = "on the floor"
 leader <- getLeaderUI
 s <- getState
 let cNotEmpty cstore = not (EM.null (getCBag (CActor leader cstore) s))
     cLegal = filter cNotEmpty cLegalAfterCalm
     storeAssocs cstore = EM.assocs (getCBag (CActor leader cstore) s)
     allAssocs = concatMap storeAssocs cLegal
 case (cLegal, allAssocs) of
   ([], _) -> do
     let cLegalInitial = filter cNotEmpty cLegalRaw
     if null cLegalInitial then do
       let ppLegal = makePhrase
             [MU.WWxW "nor" $ map (MU.Text . ppCStore) cLegalRaw]
       failWith $ "no items" <+> ppLegal
     else failSer ItemNotCalm
   ([cStart], [(iid, k)]) | not askWhenLone ->
     return $ Right ((iid, getItemBody iid s), (k, cStart))
   (cStart : _, _) -> do
     when (CGround `elem` cLegal) $
       mapM_ (updateItemSlot leader)
       $ EM.keys $ getCBag (CActor leader CGround) s
     if False then failSer PickupOverfull  -- TODO, when eqp size is limited
     else do
       (letterSlots, numberSlots) <- getsClient sslots
       let pickNumber soc =
             case soc of
               Left slides -> return $ Left slides
               Right (iidItem, (kAll, c)) -> do
                 let kDefault = if allNumber then kAll else 1
                     kRet k = return $ Right (iidItem, (k, c))
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
                         K.Char l -> kRet $! Char.digitToInt l
                         K.Return -> kRet kDefault
                         _ -> assert `failure` "unexpected key:" `twith` kkm
                 else kRet kDefault
           isCFull c = length cLegal > 1
                       && c `elem` cLegal
                       && cNotEmpty c
           perform :: ItemDialogState -> CStore -> CStore
                   -> m (SlideOrCmd ((ItemId, Item), (Int, CStore)))
           perform itemDialogState cCur cPrev = do
             bag <- getsState $ getCBag $ CActor leader cCur
             let sl = EM.filter (`EM.member` bag) letterSlots
                 slNumberSlots = IM.filter (`EM.member` bag) numberSlots
                 slP = EM.filter (\iid -> p (getItemBody iid s)) sl
                 checkItem (l, iid) =
                   ((iid, getItemBody iid s), (bag EM.! iid, l))
                 is0 = map checkItem $ EM.assocs sl
                 floorFull = isCFull CGround
                 (floorMsg, floorKey) | floorFull = (", -", [K.Char '-'])
                                      | otherwise = ("", [])
                 invEqpFull = isCFull CInv && isCFull CEqp
                 (invEqpMsg, invEqpKey) | invEqpFull = (", /", [K.Char '/'])
                                        | otherwise = ("", [])
                 isp = filter (p . snd . fst) is0
                 bestFull = not $ null isp
                 (bestMsg, bestKey)
                   | bestFull =
                     let bestSlot = slotChar $ maximum $ map (snd . snd) isp
                     in (", RET(" <> T.singleton bestSlot <> ")", [K.Return])
                   | otherwise = ("", [])
                 numberKey = if IM.null slNumberSlots then [] else [K.Char '.']
                 keys ims2 =
                   let mls = map (snd . snd) ims2
                       ks = map (K.Char . slotChar) mls
                            ++ [K.Char '?'] ++ floorKey ++ invEqpKey
                            ++ bestKey ++ numberKey
                   in zipWith K.KM (repeat K.NoModifier) ks
                 choice ims2 =
                   if null ims2
                   then "[?" <> floorMsg <> invEqpMsg
                   else let mls = map (snd . snd) ims2
                            r = slotRange mls
                        in "[" <> r <> ", ?" <> floorMsg <> invEqpMsg <> bestMsg
                 isn = ppCStore cCur
                 prompt = makePhrase ["What to", verb MU.:> "?"]
                 (ims, slOver, msg) = case itemDialogState of
                   INone     -> (isp, EM.empty, prompt)
                   ISuitable -> (isp, slP, ptext <+> isn <> ".")
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
                              then perform IAll cCur cPrev
                              else perform ISuitable cCur cPrev
                     ISuitable | ptext /= allItemsName ->
                       perform IAll cCur cPrev
                     _ -> perform INone cCur cPrev
                   K.Char '-' | floorFull && (isCFull CInv || isCFull CEqp) ->
                     let cNext = if cCur == CGround then cPrev else CGround
                     in perform itemDialogState cNext cCur
                   K.Char '/' | invEqpFull ->
                     let cNext = if cCur == CInv then CEqp else CInv
                     in perform itemDialogState cNext cCur
                   K.Char '.' | not $ IM.null slNumberSlots ->
                     case IM.minViewWithKey numberSlots of
                       Nothing -> assert `failure` "no numbered items"
                                         `twith` (km, slNumberSlots)
                       Just ((_, iid), _) ->
                         return $ Right ( (iid, getItemBody iid s)
                                        , (bag EM.! iid, cCur) )
                   K.Char l ->
                     case find ((SlotChar l ==) . snd . snd) is0 of
                       Nothing -> assert `failure` "unexpected slot"
                                         `twith` (km, l,  is0)
                       Just (iidItem, (k, _)) ->
                         return $ Right (iidItem, (k, cCur))
                   K.Return | bestFull ->
                     let (iidItem, (k, _)) =
                           maximumBy (compare `on` snd . snd) isp
                     in return $ Right (iidItem, (k, cCur))
                   _ -> assert `failure` "unexpected key:" `twith` akm
       let cStartPrev = if cStart == CGround
                        then if isCFull CEqp then CEqp else CInv
                        else cStart
       soc <- perform INone cStart cStartPrev
       pickNumber soc
