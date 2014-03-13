-- | Inventory management.
-- TODO: document
module Game.LambdaHack.Client.Inventory
  ( floorItemOverlay, itemOverlay, getGroupItem, getAnyItem
  ) where

import Control.Exception.Assert.Sugar
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import Data.Function
import Data.List
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.ItemSlot
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Key as K
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.ServerCmd
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.RuleKind

-- | Create a list of item names.
floorItemOverlay :: MonadClient m => ItemBag -> m Overlay
floorItemOverlay bag = do
  Kind.COps{coitem} <- getsState scops
  s <- getState
  disco <- getsClient sdisco
  let is = zip (EM.assocs bag) (allSlots ++ repeat (SlotChar ' '))
      pr ((iid, k), l) =
         makePhrase [ slotLabel l
                    , partItemWs coitem disco k (getItemBody iid s) ]
         <> " "
  return $! toOverlay $ map pr is

-- | Create a list of item names.
itemOverlay :: MonadClient m => ItemBag -> ItemSlots -> m Overlay
itemOverlay bag inv = do
  Kind.COps{coitem} <- getsState scops
  s <- getState
  disco <- getsClient sdisco
  let pr (l, iid) =
         makePhrase [ slotLabel l
                    , partItemWs coitem disco (bag EM.! iid)
                                 (getItemBody iid s) ]
         <> " "
  return $! toOverlay $ map pr $ EM.assocs inv

allObjectsName :: Text
allObjectsName = "Objects"

-- | Let a human player choose any item with a given group name.
-- Note that this does not guarantee the chosen item belongs to the group,
-- as the player can override the choice.
getGroupItem :: MonadClientUI m
             => MU.Part  -- ^ name of the group
             -> [Char]   -- ^ accepted item symbols
             -> MU.Part  -- ^ the verb of the prompt
             -> [CStore] -- ^ legal containers
             -> Bool     -- ^ whether to ask for the number of items
             -> Bool     -- ^ whether the default is all, instead of one
             -> m (SlideOrCmd ((ItemId, Item), (Int, CStore)))
getGroupItem object syms prompt = do
  let choice i = jsymbol i `elem` syms
      header = makePhrase [MU.Capitalize (MU.Ws object)]
  getItem True prompt choice header

-- | Let the human player choose any item from a list of items.
getAnyItem :: MonadClientUI m
           => Bool
           -> MU.Part
           -> [CStore]
           -> Bool
           -> Bool
           -> m (SlideOrCmd ((ItemId, Item), (Int, CStore)))
getAnyItem askWhenLone prompt =
  getItem askWhenLone prompt (const True) allObjectsName

data ItemDialogState = INone | ISuitable | IAll deriving Eq

-- | Let the human player choose a single, preferably suitable,
-- item from a list of items.
getItem :: forall m. MonadClientUI m
        => Bool            -- ^ whether to ask if the item alone
                           --   in the starting container and suitable
        -> MU.Part         -- ^ the verb of the prompt
        -> (Item -> Bool)  -- ^ which items to consider suitable
        -> Text            -- ^ how to describe suitable items
        -> [CStore]        -- ^ legal containers
        -> Bool            -- ^ whether to ask for the number of items
        -> Bool            -- ^ whether the default is all, instead of one
        -> m (SlideOrCmd ((ItemId, Item), (Int, CStore)))
getItem _ _ _ _ [] _ _ = failSer ItemNotCalm
getItem askWhenLone verb p ptext cLegalRaw askNumber allNumber = do
 Kind.COps{corule} <- getsState scops
 let RuleKind{rsharedInventory} = Kind.stdRuleset corule
 leader <- getLeaderUI
 s <- getState
 let cNotEmpty cstore = not (EM.null (getCBag (CActor leader cstore) s))
     cLegal = filter cNotEmpty cLegalRaw
 if null cLegal then failWith "no objects found"  -- TODO: sometimes ItemNotCalm
 else do
  let cStart = head cLegal
  let storeAssocs cstore = EM.assocs (getCBag (CActor leader cstore) s)
      mloneItem = case concatMap storeAssocs cLegal of
                    [(iid, k)] -> Just ((iid, getItemBody iid s), k)
                    _ -> Nothing
  slots <- getsClient sslots
  let ask = do
        if all (null . EM.elems)
           $ map (\cstore -> getCBag (CActor leader cstore) s) cLegal
        then failWith "no objects found"
        else if not askWhenLone && fmap (p . snd . fst) mloneItem == Just True
        then case mloneItem of
          Nothing -> assert `failure` cStart
          Just (iidItem, k) -> return $ Right (iidItem, (k, cStart))
        else do
          let cStartPrev = if cStart == CGround
                           then if isCFull CEqp then CEqp else CInv
                           else cStart
          soc <- perform INone cStart cStartPrev
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
        let inv = EM.filter (`EM.member` bag) slots
            invP = EM.filter (\iid -> p (getItemBody iid s)) inv
            checkItem (l, iid) = ((iid, getItemBody iid s), (bag EM.! iid, l))
            is0 = map checkItem $ EM.assocs inv
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
            keys ims2 =
              let mls = map (snd . snd) ims2
                  ks = map (K.Char . slotChar) mls
                       ++ [K.Char '?'] ++ floorKey ++ invEqpKey ++ bestKey
              in zipWith K.KM (repeat K.NoModifier) ks
            choice ims2 =
              if null ims2
              then "[?" <> floorMsg <> invEqpMsg
              else let mls = map (snd . snd) ims2
                       r = slotRange mls
                   in "[" <> r <> ", ?" <> floorMsg <> invEqpMsg <> bestMsg
            isn = case cCur of
              CEqp -> "in personal equipment"
              CInv -> if rsharedInventory
                      then "in shared inventory"
                      else "in inventory"
              CGround -> "on the floor"
            prompt = makePhrase ["What to", verb MU.:> "?"]
            (ims, invOver, msg) = case itemDialogState of
              INone     -> (isp, EM.empty, prompt)
              ISuitable -> (isp, invP, ptext <+> isn <> ".")
              IAll      -> (is0, inv, allObjectsName <+> isn <> ".")
        io <- itemOverlay bag invOver
        akm <- displayChoiceUI (msg <+> choice ims) io (keys is0)
        case akm of
          Left slides -> failSlides slides
          Right km@K.KM{..} -> do
            assert (modifier == K.NoModifier) skip
            case key of
              K.Char '?' -> case itemDialogState of
                INone -> if EM.null invP
                         then perform IAll cCur cPrev
                         else perform ISuitable cCur cPrev
                ISuitable | ptext /= allObjectsName -> perform IAll cCur cPrev
                _ -> perform INone cCur cPrev
              K.Char '-' | floorFull && (isCFull CInv || isCFull CEqp) ->
                let cNext = if cCur == CGround then cPrev else CGround
                in perform itemDialogState cNext cCur
              K.Char '/' | invEqpFull ->
                let cNext = if cCur == CInv then CEqp else CInv
                in perform itemDialogState cNext cCur
              K.Char l ->
                case find ((SlotChar l ==) . snd . snd) is0 of
                  Nothing -> assert `failure` "unexpected inventory slot"
                                    `twith` (km, l,  is0)
                  Just (iidItem, (k, _)) ->
                    return $ Right (iidItem, (k, cCur))
              K.Return | bestFull ->
                let (iidItem, (k, _)) = maximumBy (compare `on` snd . snd) isp
                in return $ Right (iidItem, (k, cCur))
              _ -> assert `failure` "unexpected key:" `twith` akm
  ask
