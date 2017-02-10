-- | Helper functions for both inventory management and human commands.
module Game.LambdaHack.Client.UI.HandleHelperM
  ( MError, FailOrCmd
  , showFailError, mergeMError, failWith, failSer, failMsg, weaveJust
  , sortSlots, memberCycle, memberBack, partyAfterLeader
  , pickLeader, pickLeaderWithPointer
  , itemOverlay, statsOverlay, pickNumber
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Function
import Data.Ord
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.CommonM
import Game.LambdaHack.Client.ItemSlot
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.MsgM
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.OverlayM
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Client.UI.Slideshow
import Game.LambdaHack.Client.UI.SlideshowM
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemDescription
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State

newtype FailError = FailError {failError :: Text}
  deriving Show

showFailError :: FailError -> Text
showFailError (FailError err) = "*" <> err <> "*"

type MError = Maybe FailError

mergeMError :: MError -> MError -> MError
mergeMError Nothing Nothing = Nothing
mergeMError merr1@Just{} Nothing = merr1
mergeMError Nothing merr2@Just{} = merr2
mergeMError (Just err1) (Just err2) =
  Just $ FailError $ failError err1 <+> "and" <+> failError err2

type FailOrCmd a = Either FailError a

failWith :: MonadClientUI m => Text -> m (FailOrCmd a)
failWith err = assert (not $ T.null err) $ return $ Left $ FailError err

failSer :: MonadClientUI m => ReqFailure -> m (FailOrCmd a)
failSer = failWith . showReqFailure

failMsg :: MonadClientUI m => Text -> m MError
failMsg err = assert (not $ T.null err) $ return $ Just $ FailError err

weaveJust :: FailOrCmd a -> Either MError a
weaveJust (Left ferr) = Left $ Just ferr
weaveJust (Right a) = Right a

sortSlots :: MonadClientUI m => FactionId -> Maybe Actor -> m ()
sortSlots fid mbody = do
  itemToF <- itemToFullClient
  s <- getState
  let -- If apperance the same, keep the order from before sort.
      apperance ItemFull{itemBase} =
        (jsymbol itemBase, jname itemBase, jflavour itemBase)
      compareItemFull itemFull1 itemFull2 =
        case ( jsymbol (itemBase itemFull1)
             , jsymbol (itemBase itemFull2) ) of
          ('$', '$') -> EQ
          ('$', _) -> LT
          (_, '$') -> GT
          _ -> case (itemDisco itemFull1, itemDisco itemFull2) of
            (Nothing, Nothing) -> comparing apperance itemFull1 itemFull2
            (Nothing, Just{}) -> LT
            (Just{}, Nothing) -> GT
            (Just id1, Just id2) ->
              case compare (itemKindId id1) (itemKindId id2) of
                EQ -> comparing itemAspect id1 id2
                ot -> ot
      sortSlotMap :: Bool -> EM.EnumMap SlotChar ItemId
                  -> EM.EnumMap SlotChar ItemId
      sortSlotMap onlyOrgans em =
        let onPerson = sharedAllOwnedFid onlyOrgans fid s
            onGround = maybe EM.empty
                         -- consider floor only under the acting actor
                       (\b -> getFloorBag (blid b) (bpos b) s)
                       mbody
            inBags = ES.unions $ map EM.keysSet
                     $ onPerson : [ onGround | not onlyOrgans]
            f = (`ES.member` inBags)
            (nearItems, farItems) = partition f $ EM.elems em
            g iid = (iid, itemToF iid (1, []))
            sortItemIds l =
              map fst $ sortBy (compareItemFull `on` snd) $ map g l
            nearItemAsc = zip newSlots $ sortItemIds nearItems
            farLen = if isNothing mbody then 0 else length allZeroSlots
            farSlots = drop (length nearItemAsc + farLen) newSlots
            farItemAsc = zip farSlots $ sortItemIds farItems
            newSlots = concatMap allSlots [0..]
        in EM.fromDistinctAscList $ nearItemAsc ++ farItemAsc
  ItemSlots itemSlots organSlots <- getsClient sslots
  let newSlots = ItemSlots (sortSlotMap False itemSlots)
                           (sortSlotMap True organSlots)
  modifyClient $ \cli -> cli {sslots = newSlots}

-- | Switches current member to the next on the level, if any, wrapping.
memberCycle :: MonadClientUI m => Bool -> m MError
memberCycle verbose = do
  leader <- getLeaderUI
  body <- getsState $ getActorBody leader
  hs <- partyAfterLeader leader
  case filter (\(_, b) -> blid b == blid body) hs of
    [] -> failMsg "cannot pick any other member on this level"
    (np, b) : _ -> do
      success <- pickLeader verbose np
      let !_A = assert (success `blame` "same leader"
                                `twith` (leader, np, b)) ()
      return Nothing

-- | Switches current member to the previous in the whole dungeon, wrapping.
memberBack :: MonadClientUI m => Bool -> m MError
memberBack verbose = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  leader <- getLeaderUI
  hs <- partyAfterLeader leader
  let (autoDun, _) = autoDungeonLevel fact
  case reverse hs of
    _ | autoDun -> failMsg $ showReqFailure NoChangeDunLeader
    [] -> failMsg "no other member in the party"
    (np, b) : _ -> do
      success <- pickLeader verbose np
      let !_A = assert (success `blame` "same leader"
                                `twith` (leader, np, b)) ()
      return Nothing

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
  saimMode <- getsSession saimMode
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
      -- Move the xhair, if active, to the new level.
      case saimMode of
        Nothing -> return ()
        Just _ ->
          modifySession $ \sess -> sess {saimMode = Just $ AimMode $ blid pbody}
      -- Inform about items, etc.
      lookMsg <- lookAt False "" True (bpos pbody) aid ""
      when verbose $ msgAdd lookMsg
      return True

pickLeaderWithPointer :: MonadClientUI m => m MError
pickLeaderWithPointer = do
  lidV <- viewedLevelUI
  Level{lysize} <- getLevel lidV
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  arena <- getArenaUI
  ours <- getsState $ filter (not . bproj . snd)
                      . actorAssocs (== side) lidV
  let viewed = sortBy (comparing keySelected) ours
      (autoDun, _) = autoDungeonLevel fact
      pick (aid, b) =
        if | blid b /= arena && autoDun ->
               failMsg $ showReqFailure NoChangeDunLeader
           | otherwise -> do
               void $ pickLeader True aid
               return Nothing
  Point{..} <- getsSession spointer
  -- Pick even if no space in status line for the actor's symbol.
  if | py == lysize + 2 && px == 0 -> memberBack True
     | py == lysize + 2 ->
         case drop (px - 1) viewed of
           [] -> return Nothing  -- relaxed, due to subtleties of selected display
           aidb : _ -> pick aidb
     | otherwise ->
         case find (\(_, b) -> bpos b == Point px (py - mapStartY)) ours of
           Nothing -> failMsg "not pointing at an actor"
           Just aidb -> pick aidb

-- | Create a list of item names.
itemOverlay :: MonadClient m => CStore -> LevelId -> ItemBag -> m OKX
itemOverlay store lid bag = do
  localTime <- getsState $ getLocalTime lid
  itemToF <- itemToFullClient
  ItemSlots itemSlots organSlots <- getsClient sslots
  side <- getsClient sside
  factionD <- getsState sfactionD
  let isOrgan = store == COrgan
      lSlots = if isOrgan then organSlots else itemSlots
      !_A = assert (all (`elem` EM.elems lSlots) (EM.keys bag)
                    `blame` (store, lid, bag, lSlots)) ()
      pr (l, iid) =
        case EM.lookup iid bag of
          Nothing -> Nothing
          Just kit@(k, _) ->
            let itemFull = itemToF iid kit
                colorSymbol = viewItem $ itemBase itemFull
                phrase =
                  makePhrase [partItemWsRanged side factionD
                                               k store localTime itemFull]
                al = textToAL (slotLabel l)
                     <+:> [colorSymbol]
                     <+:> textToAL phrase
                kx = (Right l, (undefined, 0, length al))
            in Just ([al], kx)
      (ts, kxs) = unzip $ mapMaybe pr $ EM.assocs lSlots
      renumber y (km, (_, x1, x2)) = (km, (y, x1, x2))
  return (concat ts, zipWith renumber [0..] kxs)

statsOverlay :: MonadClient m => ActorId -> m OKX
statsOverlay aid = do
  b <- getsState $ getActorBody aid
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup aid actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` aid
      tshow200 n = let n200 = min 200 $ max (-200) n
                   in tshow n200 <> if n200 /= n then "$" else ""
      tshowBlock k n = tshow200 $ n + if braced b then k else 0
      prSlot :: (Y, SlotChar) -> (AspectRecord -> Int, Text, Int -> Text)
             -> (Text, KYX)
      prSlot (y, c) (accessor, blurb, decorator) =
        let fullText t =
              makePhrase [ MU.Text $ slotLabel c
                         , MU.Text $ T.justifyLeft 22 ' ' blurb
                         , MU.Text t ]
            valueText = decorator $ accessor ar
            ft = fullText valueText
        in (ft, (Right c, (y, 0, T.length ft)))
      showIntWith1 :: Int -> Text
      showIntWith1 k =
        let l = k `div` 10
            x = k - l * 10
        in tshow l <> if x == 0 then "" else "." <> tshow x
      -- Some values can be negative, for others 0 is equivalent but shorter.
      tshowRadius r = if r == 0 then "0m" else tshow (r - 1) <> ".5m"
      slotList =
        [ (aHurtMelee, "to melee damage", \t -> tshow200 t <> "%")
        , (aArmorMelee, "melee armor", \t -> "[" <> tshowBlock 50 t <> "%]")
        , (aArmorRanged, "ranged armor",  \t -> "{" <> tshowBlock 25 t <> "%}")
        , (aMaxHP, "max HP", \t -> tshow $ max 0 t)
        , (aMaxCalm, "max Calm", \t -> tshow $ max 0 t)
        , (aSpeed, "speed", \t -> showIntWith1 t <> "m/s")
        , (aSight, "sight radius", \t ->
            let tmax = max 0 t
                tcapped = min (fromEnum $ bcalm b `div` (5 * oneM)) tmax
            in tshowRadius tcapped <+> if tcapped == tmax
                                       then ""
                                       else "(max" <+> tshowRadius tmax <> ")")
        , (aSmell, "smell radius", \t -> tshowRadius (max 0 t))
        , (aShine, "shine radius", \t -> tshowRadius (max 0 t))
        , (aNocto, "night vision radius", \t -> tshowRadius (max 0 t)) ]
        ++ [ (EM.findWithDefault 0 ab . aSkills, tshow ab <+> "ability", tshow)
           | ab <- [minBound..maxBound] ]
      zipReslot = zipWith prSlot $ zip [0..] allZeroSlots
      (ts, kxs) = unzip $ zipReslot slotList
  return (map textToAL ts, kxs)

pickNumber :: MonadClientUI m => Bool -> Int -> m (Either MError Int)
pickNumber askNumber kAll = do
  let shownKeys = [ K.returnKM, K.mkChar '+', K.mkChar '-'
                  , K.spaceKM, K.escKM ]
      frontKeyKeys = K.backspaceKM : shownKeys ++ map K.mkChar ['0'..'9']
      gatherNumber pointer kCurRaw = do
        let kCur = min kAll $ max 1 kCurRaw
            kprompt = "Choose number:" <+> tshow kCur
        promptAdd kprompt
        sli <- reportToSlideshow shownKeys
        (Left kkm, pointer2) <-
          displayChoiceScreen ColorFull False pointer sli frontKeyKeys
        case K.key kkm of
          K.Char '+' ->
            gatherNumber pointer2 $ if kCur + 1 > kAll then 1 else kCur + 1
          K.Char '-' ->
            gatherNumber pointer2 $ if kCur - 1 < 1 then kAll else kCur - 1
          K.Char l | kCur == kAll ->  gatherNumber pointer2 $ Char.digitToInt l
          K.Char l -> gatherNumber pointer2 $ kCur * 10 + Char.digitToInt l
          K.BackSpace -> gatherNumber pointer2 $ kCur `div` 10
          K.Return -> return $ Right kCur
          K.Esc -> weaveJust <$> failWith "never mind"
          K.Space -> return $ Left Nothing
          _ -> assert `failure` "unexpected key:" `twith` kkm
  if | kAll == 0 -> weaveJust <$> failWith "no number of items can be chosen"
     | kAll == 1 || not askNumber -> return $ Right kAll
     | otherwise -> do
         res <- gatherNumber 0 kAll
         case res of
           Right k | k <= 0 -> assert `failure` (res, kAll)
           _ -> return res
