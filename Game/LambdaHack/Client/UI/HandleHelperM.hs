-- | Helper functions for both inventory management and human commands.
module Game.LambdaHack.Client.UI.HandleHelperM
  ( FailError, showFailError, MError, mergeMError, FailOrCmd, failWith
  , failSer, failMsg, weaveJust
  , ppSLore, loreFromMode, loreFromContainer, sortSlots
  , memberCycle, memberBack, partyAfterLeader, pickLeader, pickLeaderWithPointer
  , itemOverlay, statsOverlay, pickNumber
  , lookAtTile, lookAtActors, lookAtItems
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Function
import           Data.Ord
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Client.CommonM
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.ActorUI
import           Game.LambdaHack.Client.UI.EffectDescription
import           Game.LambdaHack.Client.UI.ItemDescription
import           Game.LambdaHack.Client.UI.ItemSlot
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.MsgM
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.Slideshow
import           Game.LambdaHack.Client.UI.SlideshowM
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.ReqFailure
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Content.ItemKind as IK
import qualified Game.LambdaHack.Content.TileKind as TK

-- | Message describing the cause of failure of human command.
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

ppSLore :: SLore -> Text
ppSLore SItem = "item"
ppSLore SOrgan = "organ"
ppSLore STrunk = "creature"
ppSLore SEmbed = "terrain"
ppSLore SBlast = "blast"
ppSLore STmp = "temporary condition"

loreFromMode :: ItemDialogMode -> SLore
loreFromMode c = case c of
  MStore COrgan -> SOrgan
  MStore _ -> SItem
  MOwned -> SItem
  MStats -> SItem  -- dummy
  MLore slore -> slore

loreFromContainer :: Item -> Container -> SLore
loreFromContainer item c = case c of
  CFloor{} -> SItem
  CEmbed{} -> SEmbed
  CActor _ store -> if | isBlast item -> SBlast
                       | isTmpCondition item -> STmp
                       | otherwise -> loreFromMode $ MStore store
  CTrunk{} -> if isBlast item then SBlast else STrunk

sortSlots :: MonadClientUI m => FactionId -> Maybe Actor -> m ()
sortSlots fid mbody = do
  itemToF <- getsState itemToFull
  s <- getState
  let -- If apperance the same, keep the order from before sort.
      apperance ItemFull{itemBase} =
        (jsymbol itemBase, jname itemBase, jflavour itemBase)
      compareItemFull itemFull1 itemFull2 =
        case (itemDisco itemFull1, itemDisco itemFull2) of
          (Nothing, Nothing) -> comparing apperance itemFull1 itemFull2
          (Nothing, Just{}) -> LT
          (Just{}, Nothing) -> GT
          (Just id1, Just id2) ->
            case compare (itemKindId id1) (itemKindId id2) of
              EQ -> comparing itemAspect id1 id2
              ot -> ot
      sortSlotMap :: SLore -> EM.EnumMap SlotChar ItemId
                  -> EM.EnumMap SlotChar ItemId
      sortSlotMap slore em =
        let partySet = partyItemSet slore fid mbody s
            (nearItems, farItems) = partition (`ES.member` partySet)
                                    $ EM.elems em
            f iid = (iid, itemToF iid (1, []))
            sortItemIds l = map fst $ sortBy (compareItemFull `on` snd)
                            $ map f l
            newSlots = concatMap allSlots [0..]
        in EM.fromDistinctAscList $ zip newSlots
           $ sortItemIds nearItems ++ sortItemIds farItems
  ItemSlots itemSlots <- getsSession sslots
  let newSlots = ItemSlots $ EM.mapWithKey sortSlotMap itemSlots
  modifySession $ \sess -> sess {sslots = newSlots}

-- | Switches current member to the next on the level, if any, wrapping.
memberCycle :: MonadClientUI m => Bool -> m MError
memberCycle verbose = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  lidV <- viewedLevelUI
  leader <- getLeaderUI
  body <- getsState $ getActorBody leader
  hs <- partyAfterLeader leader
  let (autoDun, _) = autoDungeonLevel fact
  case filter (\(_, b, _) -> blid b == lidV) hs of
    _ | autoDun && lidV /= blid body ->
      failMsg $ showReqFailure NoChangeDunLeader
    [] -> failMsg "cannot pick any other member on this level"
    (np, b, _) : _ -> do
      success <- pickLeader verbose np
      let !_A = assert (success `blame` "same leader"
                                `swith` (leader, np, b)) ()
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
    (np, b, _) : _ -> do
      success <- pickLeader verbose np
      let !_A = assert (success `blame` "same leader"
                                `swith` (leader, np, b)) ()
      return Nothing

partyAfterLeader :: MonadClientUI m => ActorId -> m [(ActorId, Actor, ActorUI)]
partyAfterLeader leader = do
  side <- getsState $ bfid . getActorBody leader
  sactorUI <- getsSession sactorUI
  allA <- getsState $ EM.assocs . sactorD  -- not only on one level
  let allOurs = filter (\(_, body) ->
        not (bproj body) && bfid body == side) allA
      allOursUI = map (\(aid, b) -> (aid, b, sactorUI EM.! aid)) allOurs
      hs = sortBy (comparing keySelected) allOursUI
      i = fromMaybe (-1) $ findIndex (\(aid, _, _) -> aid == leader) hs
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
      body <- getsState $ getActorBody aid
      bodyUI <- getsSession $ getActorUI aid
      let !_A = assert (not (bproj body)
                        `blame` "projectile chosen as the leader"
                        `swith` (aid, body)) ()
      -- Even if it's already the leader, give his proper name, not 'you'.
      let subject = partActor bodyUI
      when verbose $ msgAdd $ makeSentence [subject, "picked as a leader"]
      -- Update client state.
      s <- getState
      modifyClient $ updateLeader aid s
      -- Move the xhair, if active, to the new level.
      case saimMode of
        Nothing -> return ()
        Just _ ->
          modifySession $ \sess -> sess {saimMode = Just $ AimMode $ blid body}
      -- Inform about items, etc.
      itemsBlurb <- lookAtItems True (bpos body) aid
      when verbose $ msgAdd itemsBlurb
      return True

pickLeaderWithPointer :: MonadClientUI m => m MError
pickLeaderWithPointer = do
  lidV <- viewedLevelUI
  Level{lysize} <- getLevel lidV
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  arena <- getArenaUI
  sactorUI <- getsSession sactorUI
  ours <- getsState $ filter (not . bproj . snd)
                      . actorAssocs (== side) lidV
  let oursUI = map (\(aid, b) -> (aid, b, sactorUI EM.! aid)) ours
      viewed = sortBy (comparing keySelected) oursUI
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
           (aid, b, _) : _ -> pick (aid, b)
     | otherwise ->
         case find (\(_, b, _) -> bpos b == Point px (py - mapStartY)) oursUI of
           Nothing -> failMsg "not pointing at an actor"
           Just (aid, b, _) -> pick (aid, b)

itemOverlay :: MonadClientUI m => SLore -> LevelId -> ItemBag -> m OKX
itemOverlay slore lid bag = do
  localTime <- getsState $ getLocalTime lid
  itemToF <- getsState itemToFull
  ItemSlots itemSlots <- getsSession sslots
  side <- getsClient sside
  factionD <- getsState sfactionD
  combEqp <- getsState $ combinedEqp side
  combOrgan <- getsState $ combinedOrgan side
  let lSlots = itemSlots EM.! slore
      !_A = assert (all (`elem` EM.elems lSlots) (EM.keys bag)
                    `blame` (lid, bag, lSlots)) ()
      markEqp iid t = if iid `EM.member` combEqp || iid `EM.member` combOrgan
                      then T.snoc (T.init t) '>'
                      else t
      pr (l, iid) =
        case EM.lookup iid bag of
          Nothing -> Nothing
          Just kit@(k, _) ->
            let itemFull = itemToF iid kit
                colorSymbol = viewItem $ itemBase itemFull
                phrase = makePhrase
                           [partItemWsRanged side factionD k localTime itemFull]
                al = textToAL (markEqp iid $ slotLabel l)
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
  ar <- getsState $ getActorAspect aid
  let prSlot :: (Y, SlotChar) -> IK.EqpSlot -> (Text, KYX)
      prSlot (y, c) eqpSlot =
        let statName = slotToName eqpSlot
            fullText t =
              makePhrase [ MU.Text $ slotLabel c
                         , MU.Text $ T.justifyLeft 22 ' ' statName
                         , MU.Text t ]
            valueText = slotToDecorator eqpSlot b $ prEqpSlot eqpSlot ar
            ft = fullText valueText
        in (ft, (Right c, (y, 0, T.length ft)))
      (ts, kxs) = unzip $ zipWith prSlot (zip [0..] allZeroSlots) statSlots
  return (map textToAL ts, kxs)

pickNumber :: MonadClientUI m => Bool -> Int -> m (Either MError Int)
pickNumber askNumber kAll = assert (kAll >= 1) $ do
  let shownKeys = [ K.returnKM, K.mkChar '+', K.mkChar '-'
                  , K.spaceKM, K.escKM ]
      frontKeyKeys = K.backspaceKM : shownKeys ++ map K.mkChar ['0'..'9']
      gatherNumber pointer kCur = assert (1 <= kCur && kCur <= kAll) $ do
        let kprompt = "Choose number:" <+> tshow kCur
        promptAdd kprompt
        sli <- reportToSlideshow shownKeys
        (ekkm, pointer2) <-
          displayChoiceScreen ColorFull False pointer sli frontKeyKeys
        case ekkm of
          Left kkm ->
            case K.key kkm of
              K.Char '+' ->
                gatherNumber pointer2 $ if kCur + 1 > kAll then 1 else kCur + 1
              K.Char '-' ->
                gatherNumber pointer2 $ if kCur - 1 < 1 then kAll else kCur - 1
              K.Char l | kCur * 10 + Char.digitToInt l > kAll ->
                gatherNumber pointer2
                $ if Char.digitToInt l == 0
                  then kAll
                  else min kAll (Char.digitToInt l)
              K.Char l -> gatherNumber pointer2 $ kCur * 10 + Char.digitToInt l
              K.BackSpace -> gatherNumber pointer2 $ max 1 (kCur `div` 10)
              K.Return -> return $ Right kCur
              K.Esc -> weaveJust <$> failWith "never mind"
              K.Space -> return $ Left Nothing
              _ -> error $ "unexpected key" `showFailure` kkm
          Right sc -> error $ "unexpected slot char" `showFailure` sc
  if | kAll == 1 || not askNumber -> return $ Right kAll
     | otherwise -> do
         res <- gatherNumber 0 kAll
         case res of
           Right k | k <= 0 -> error $ "" `showFailure` (res, kAll)
           _ -> return res

-- | Produces a textual description of the tile at a position.
lookAtTile :: MonadClientUI m
           => Bool       -- ^ can be seen right now?
           -> Point      -- ^ position to describe
           -> ActorId    -- ^ the actor that looks
           -> LevelId    -- ^ level the position is at
           -> m Text
lookAtTile canSee p aid lidV = do
  Kind.COps{cotile=Kind.Ops{okind}} <- getsState scops
  b <- getsState $ getActorBody aid
  lvl <- getLevel lidV
  embeds <- getsState $ EM.keys . getEmbedBag lidV p
  itemToF <- getsState itemToFull
  seps <- getsClient seps
  mnewEps <- makeLine False b p seps
  let aims = isJust mnewEps
      tile = lvl `at` p
      vis | TK.isUknownSpace tile = "that is"
          | not canSee = "you remember"
          | not aims = "you are aware of"
          | otherwise = "you see"
      tilePart = MU.AW $ MU.Text $ TK.tname $ okind tile
      desc = case embeds of
        iid : rest | all (== iid) rest ->
          case itemDisco $ itemToF iid (1, []) of
            Nothing -> ""
            Just ItemDisco{itemKind} -> IK.idesc itemKind
        _ -> ""  -- displaying many would require some glue or convention
      pdesc = if desc == "" then "" else "(" <> desc <> ")"
  return $! makeSentence [MU.Text vis, tilePart] <+> pdesc

-- | Produces a textual description of actors at a position.
lookAtActors :: MonadClientUI m
             => Point      -- ^ position to describe
             -> LevelId    -- ^ level the position is at
             -> m Text
lookAtActors p lidV = do
  side <- getsClient sside
  inhabitants <- getsState $ posToAssocs p lidV
  sactorUI <- getsSession sactorUI
  let inhabitantsUI =
        map (\(aid2, b2) -> (aid2, b2, sactorUI EM.! aid2)) inhabitants
  itemToF <- getsState itemToFull
  factionD <- getsState sfactionD
  s <- getState
  let actorsBlurb = case inhabitants of
        [] -> ""
        (_, body) : rest ->
          let Item{jfid} = getItemBody (btrunk body) s
              bfact = factionD EM.! bfid body
              -- Even if it's the leader, give his proper name, not 'you'.
              subjects = map (\(_, _, bUI) -> partActor bUI)
                             inhabitantsUI
              subject = MU.WWandW subjects
              verb = "be here"
              factDesc = case jfid of
                Just tfid | tfid /= bfid body ->
                  let dominatedBy = if bfid body == side
                                    then "us"
                                    else gname bfact
                      tfact = factionD EM.! tfid
                  in "Originally of" <+> gname tfact
                     <> ", now fighting for" <+> dominatedBy <> "."
                _ | bfid body == side -> ""  -- just one of us
                _ | bproj body -> "Launched by" <+> gname bfact <> "."
                _ -> "One of" <+> gname bfact <> "."
              idesc = case itemDisco $ itemToF (btrunk body) (1, []) of
                Nothing -> ""  -- no details, only show the name
                Just ItemDisco{itemKind} -> IK.idesc itemKind
              -- If many different actors (projectiles), only list names.
              sameTrunks = all (\(_, b) -> btrunk b == btrunk body) rest
              desc = if sameTrunks then factDesc <+> idesc else ""
              -- Both description and faction blurb may be empty.
              pdesc = if desc == "" then "" else "(" <> desc <> ")"
          in makeSentence [MU.SubjectVerbSg subject verb] <+> pdesc
  return $! actorsBlurb

-- | Produces a textual description of items at a position.
lookAtItems :: MonadClientUI m
            => Bool       -- ^ can be seen right now?
            -> Point      -- ^ position to describe
            -> ActorId    -- ^ the actor that looks
            -> m Text
lookAtItems canSee p aid = do
  itemToF <- getsState itemToFull
  b <- getsState $ getActorBody aid
  -- Not using @viewedLevelUI@, because @aid@ may be temporarily not a leader.
  saimMode <- getsSession saimMode
  let lidV = maybe (blid b) aimLevelId saimMode
  localTime <- getsState $ getLocalTime lidV
  subject <- partAidLeader aid
  is <- getsState $ getFloorBag lidV p
  side <- getsClient sside
  factionD <- getsState sfactionD
  let verb = MU.Text $ if | p == bpos b && lidV == blid b -> "stand on"
                          | canSee -> "notice"
                          | otherwise -> "remember"
      nWs (iid, kit@(k, _)) =
        partItemWs side factionD k localTime (itemToF iid kit)
  return $! if EM.size is == 0 then ""
            else makeSentence [ MU.SubjectVerbSg subject verb
                              , MU.WWandW $ map nWs $ EM.assocs is]
