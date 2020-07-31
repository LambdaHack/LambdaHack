-- | Helper functions for both inventory management and human commands.
module Game.LambdaHack.Client.UI.HandleHelperM
  ( FailError, showFailError, MError, mergeMError, FailOrCmd, failWith
  , failSer, failMsg, weaveJust
  , memberCycle, memberCycleLevel, partyAfterLeader, pickLeader, pickLeaderWithPointer
  , itemOverlay, skillsOverlay, placesFromState, placesOverlay
  , pickNumber, guardItemSize, lookAtItems, lookAtStash, lookAtPosition
  , displayItemLore, viewLoreItems, cycleLore, spoilsBlurb
  , ppContainerWownW
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , lookAtTile, lookAtActors, guardItemVerbs
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Client.CommonM
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.ActorUI
import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Client.UI.EffectDescription
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.ItemDescription
import           Game.LambdaHack.Client.UI.ItemSlot
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.MsgM
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.Slideshow
import           Game.LambdaHack.Client.UI.SlideshowM
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.ClientOptions
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Perception
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.ReqFailure
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import qualified Game.LambdaHack.Content.ItemKind as IK
import qualified Game.LambdaHack.Content.PlaceKind as PK
import qualified Game.LambdaHack.Content.TileKind as TK
import qualified Game.LambdaHack.Definition.Ability as Ability
import qualified Game.LambdaHack.Definition.Color as Color
import           Game.LambdaHack.Definition.Defs

-- | Message describing the cause of failure of human command.
newtype FailError = FailError {failError :: Text}
  deriving (Show, Eq)

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

-- | Switches current member to the next on the level, if any, wrapping.
memberCycleLevel :: MonadClientUI m => Bool -> Direction -> m MError
memberCycleLevel verbose direction = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  lidV <- viewedLevelUI
  leader <- getLeaderUI
  body <- getsState $ getActorBody leader
  hs <- partyAfterLeader leader
  let (autoDun, _) = autoDungeonLevel fact
  let hsSort = case direction of
        Forward -> hs
        Backward -> reverse hs
  case filter (\(_, b, _) -> blid b == lidV) hsSort of
    _ | autoDun && lidV /= blid body ->
      failMsg $ showReqFailure NoChangeDunLeader
    [] -> failMsg "cannot pick any other member on this level"
    (np, b, _) : _ -> do
      success <- pickLeader verbose np
      let !_A = assert (success `blame` "same leader"
                                `swith` (leader, np, b)) ()
      return Nothing

-- | Switches current member to the previous in the whole dungeon, wrapping.
memberCycle :: MonadClientUI m => Bool -> Direction -> m MError
memberCycle verbose direction = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  leader <- getLeaderUI
  hs <- partyAfterLeader leader
  let (autoDun, _) = autoDungeonLevel fact
  let hsSort = case direction of
        Forward -> hs
        Backward -> reverse hs
  case hsSort of
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
  allOurs <- getsState $ fidActorNotProjGlobalAssocs side -- not only on level
  let allOursUI = map (\(aid, b) -> (aid, b, sactorUI EM.! aid)) allOurs
      hs = sortOn keySelected allOursUI
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
      when verbose $
        msgAdd MsgDone $ makeSentence [subject, "picked as a pointman"]
      -- Update client state.
      updateClientLeader aid
      -- Move the xhair, if active, to the new level.
      case saimMode of
        Nothing -> return ()
        Just _ ->
          modifySession $ \sess -> sess {saimMode = Just $ AimMode $ blid body}
      -- Inform about items, etc.
      itemsBlurb <- lookAtItems True (bpos body) aid
      stashBlurb <- lookAtStash (blid body) (bpos body)
      when verbose $ msgAdd MsgAtFeet $ itemsBlurb <+> stashBlurb
      return True

pickLeaderWithPointer :: MonadClientUI m => m MError
pickLeaderWithPointer = do
  CCUI{coscreen=ScreenContent{rheight}} <- getsSession sccui
  lidV <- viewedLevelUI
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  arena <- getArenaUI
  sactorUI <- getsSession sactorUI
  ours <- getsState $ filter (not . bproj . snd)
                      . actorAssocs (== side) lidV
  let oursUI = map (\(aid, b) -> (aid, b, sactorUI EM.! aid)) ours
      viewed = sortOn keySelected oursUI
      (autoDun, _) = autoDungeonLevel fact
      pick (aid, b) =
        if | blid b /= arena && autoDun ->
               failMsg $ showReqFailure NoChangeDunLeader
           | otherwise -> do
               void $ pickLeader True aid
               return Nothing
  K.PointUI x y <- getsSession spointer
  let (px, py) = (x `div` 2, y - K.mapStartY)
  -- Pick even if no space in status line for the actor's symbol.
  if | py == rheight - 2 && px == 0 -> memberCycle True Forward
     | py == rheight - 2 ->
         case drop (px - 1) viewed of
           [] -> return Nothing
             -- relaxed, due to subtleties of display of selected actors
           (aid, b, _) : _ -> pick (aid, b)
     | otherwise ->
         case find (\(_, b, _) -> bpos b == Point px py) oursUI of
           Nothing -> failMsg "not pointing at an actor"
           Just (aid, b, _) -> pick (aid, b)

itemOverlay :: MonadClientUI m
            => SingleItemSlots -> LevelId -> ItemBag -> Bool -> m OKX
itemOverlay lSlots lid bag displayRanged = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  localTime <- getsState $ getLocalTime lid
  itemToF <- getsState $ flip itemToFull
  side <- getsClient sside
  factionD <- getsState sfactionD
  combGround <- getsState $ combinedGround side
  combOrgan <- getsState $ combinedOrgan side
  combEqp <- getsState $ combinedEqp side
  stashBag <- getsState $ getFactionStashBag side
  discoBenefit <- getsClient sdiscoBenefit
  FontSetup{..} <- getFontSetup
  let !_A = assert (all (`elem` EM.elems lSlots) (EM.keys bag)
                    `blame` (lid, bag, lSlots)) ()
      markEqp iid t =
        if | (iid `EM.member` combOrgan
             || iid `EM.member` combEqp)
             && iid `EM.notMember` stashBag
             && iid `EM.notMember` combGround -> T.snoc (T.init t) ']'
               -- all ready to fight with
           | iid `EM.member` stashBag -> T.snoc (T.init t) '}'
               -- some spares in shared stash
           | otherwise -> t
      pr (l, iid) =
        case EM.lookup iid bag of
          Nothing -> Nothing
          Just kit@(k, _) ->
            let itemFull = itemToF iid
                colorSymbol =
                  if isJust $ lookup IK.CONDITION $ IK.ifreq $ itemKind itemFull
                  then let color = if benInEqp (discoBenefit EM.! iid)
                                   then Color.BrGreen
                                   else Color.BrRed
                       in Color.attrChar2ToW32 color
                                               (IK.isymbol $ itemKind itemFull)
                  else viewItem itemFull
                phrase = makePhrase
                  [partItemWsRanged rwidth side factionD displayRanged
                                    DetailMedium 4 k localTime itemFull kit]
                al1 = attrStringToAL
                      $ textToAS (markEqp iid $ slotLabel l)
                        ++ [Color.spaceAttrW32 | isSquareFont propFont]
                        ++ [colorSymbol]
                xal2 = ( textSize squareFont $ attrLine al1
                       , attrStringToAL $ Color.spaceAttrW32 : textToAS phrase )
                kx = (Right l, ( K.PointUI 0 0
                               , ButtonWidth propFont (5 + T.length phrase) ))
            in Just ((al1, xal2), kx)
      (ts, kxs) = unzip $ mapMaybe pr $ EM.assocs lSlots
      (tsLab, tsDesc) = unzip ts
      ovsLab = EM.singleton squareFont $ offsetOverlay tsLab
      ovsDesc = EM.singleton propFont $ offsetOverlayX tsDesc
      renumber y (km, (K.PointUI x _, len)) = (km, (K.PointUI x y, len))
  return (EM.unionWith (++) ovsLab ovsDesc, zipWith renumber [0..] kxs )

skillsOverlay :: MonadClientUI m => ActorId -> m OKX
skillsOverlay aid = do
  b <- getsState $ getActorBody aid
  actorMaxSk <- getsState $ getActorMaxSkills aid
  FontSetup{..} <- getFontSetup
  let prSlot :: (Int, SlotChar) -> Ability.Skill
             -> ((AttrLine, (Int, AttrLine), (Int, AttrLine)), KYX)
      prSlot (y, c) skill =
        let skName = " " <> skillName skill
            slotLab = slotLabel c
            lab = textToAL slotLab
            labLen = textSize squareFont $ attrLine lab
            indentation = if isSquareFont propFont then 42 else 20
            valueText = skillToDecorator skill b
                        $ Ability.getSk skill actorMaxSk
            triple = ( lab
                     , (labLen, textToAL skName)
                     , (labLen + indentation, textToAL valueText) )
        in (triple, (Right c, ( K.PointUI 0 y
                              , ButtonWidth propFont (28 + T.length slotLab) )))
      (ts, kxs) = unzip $ zipWith prSlot (zip [0..] allSlots) skillSlots
      (skLab, skDescr, skValue) = unzip3 ts
      skillLab = EM.singleton squareFont $ offsetOverlay skLab
      skillDescr = EM.singleton propFont $ offsetOverlayX skDescr
      skillValue = EM.singleton monoFont $ offsetOverlayX skValue
  return (EM.unionsWith (++) [skillLab, skillDescr, skillValue], kxs)

placesFromState :: ContentData PK.PlaceKind -> ClientOptions -> State
                -> EM.EnumMap (ContentId PK.PlaceKind)
                              (ES.EnumSet LevelId, Int, Int, Int)
placesFromState coplace ClientOptions{sexposePlaces} s =
  let addEntries (!es1, !ne1, !na1, !nd1) (!es2, !ne2, !na2, !nd2) =
        let !es = ES.union es1 es2
            !ne = ne1 + ne2
            !na = na1 + na2
            !nd = nd1 + nd2
        in (es, ne, na, nd)
      placesFromLevel :: (LevelId, Level)
                      -> EM.EnumMap (ContentId PK.PlaceKind)
                                    (ES.EnumSet LevelId, Int, Int, Int)
      placesFromLevel (!lid, Level{lentry}) =
        let f (PK.PEntry pk) em =
              EM.insertWith addEntries pk (ES.singleton lid, 1, 0, 0) em
            f (PK.PAround pk) em =
              EM.insertWith addEntries pk (ES.singleton lid, 0, 1, 0) em
            f (PK.PExists pk) em =
              EM.insertWith addEntries pk (ES.singleton lid, 0, 0, 1) em
        in EM.foldr' f EM.empty lentry
      insertZeros !em !pk _ = EM.insert pk (ES.empty, 0, 0, 0) em
      initialPlaces | not sexposePlaces = EM.empty
                    | otherwise = ofoldlWithKey' coplace insertZeros EM.empty
  in EM.unionWith addEntries
       initialPlaces
       (EM.unionsWith addEntries $ map placesFromLevel $ EM.assocs $ sdungeon s)

placesOverlay :: MonadClientUI m => m OKX
placesOverlay = do
  COps{coplace} <- getsState scops
  soptions <- getsClient soptions
  places <- getsState $ placesFromState coplace soptions
  FontSetup{..} <- getFontSetup
  let prSlot :: (Int, SlotChar)
             -> (ContentId PK.PlaceKind, (ES.EnumSet LevelId, Int, Int, Int))
             -> (AttrLine, (Int, AttrLine), KYX)
      prSlot (y, c) (pk, (es, _, _, _)) =
        let placeName = PK.pname $ okind coplace pk
            markPlace t = if ES.null es
                          then T.snoc (T.init t) '>'
                          else t
            !tSlot = markPlace $ slotLabel c  -- free @places@ as you go
            !lenSlot = 2 * T.length tSlot
            !tBlurb = " "
                      <> placeName
                      <+> if ES.null es
                          then ""
                          else "("
                               <> makePhrase [MU.CarWs (ES.size es) "level"]
                               <> ")"
            !lenButton = lenSlot + T.length tBlurb
            !pButton = K.PointUI 0 y
            !widthButton = ButtonWidth propFont lenButton
        in ( textToAL tSlot
           , (lenSlot, textToAL tBlurb)
           , (Right c, (pButton, widthButton)) )
      (plLab, plDesc, kxs) = unzip3 $ zipWith prSlot (zip [0..] allSlots)
                                    $ EM.assocs places
      placeLab = EM.singleton squareFont $ offsetOverlay plLab
      placeDesc = EM.singleton propFont $ offsetOverlayX plDesc
  return (EM.unionWith (++) placeLab placeDesc, kxs)

pickNumber :: MonadClientUI m => Bool -> Int -> m (Either MError Int)
pickNumber askNumber kAll = assert (kAll >= 1) $ do
  let shownKeys = [ K.returnKM, K.spaceKM, K.mkChar '+', K.mkChar '-'
                  , K.backspaceKM, K.escKM ]
      frontKeyKeys = shownKeys ++ map K.mkChar ['0'..'9']
      gatherNumber kCur = assert (1 <= kCur && kCur <= kAll) $ do
        let kprompt = "Choose number:" <+> tshow kCur
        promptAdd0 kprompt
        sli <- reportToSlideshow shownKeys
        ekkm <- displayChoiceScreen "" ColorFull False sli frontKeyKeys
        case ekkm of
          Left kkm ->
            case K.key kkm of
              K.Char '+' ->
                gatherNumber $ if kCur + 1 > kAll then 1 else kCur + 1
              K.Char '-' ->
                gatherNumber $ if kCur - 1 < 1 then kAll else kCur - 1
              K.Char l | kCur * 10 + Char.digitToInt l > kAll ->
                gatherNumber $ if Char.digitToInt l == 0
                               then kAll
                               else min kAll (Char.digitToInt l)
              K.Char l -> gatherNumber $ kCur * 10 + Char.digitToInt l
              K.BackSpace -> gatherNumber $ max 1 (kCur `div` 10)
              K.Return -> return $ Right kCur
              K.Esc -> weaveJust <$> failWith "never mind"
              K.Space -> return $ Left Nothing
              _ -> error $ "unexpected key" `showFailure` kkm
          Right sc -> error $ "unexpected slot char" `showFailure` sc
  if | kAll == 1 || not askNumber -> return $ Right kAll
     | otherwise -> do
         res <- gatherNumber kAll
         case res of
           Right k | k <= 0 -> error $ "" `showFailure` (res, kAll)
           _ -> return res

-- | Produces a textual description of the tile at a position.
lookAtTile :: MonadClientUI m
           => Bool       -- ^ can be seen right now?
           -> Point      -- ^ position to describe
           -> ActorId    -- ^ the actor that looks
           -> LevelId    -- ^ level the position is at
           -> m (Text, Text, [(Text, Text)])
lookAtTile canSee p aid lidV = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  cops@COps{cotile, coplace} <- getsState scops
  side <- getsClient sside
  factionD <- getsState sfactionD
  b <- getsState $ getActorBody aid
  lvl <- getLevel lidV
  embeds <- getsState $ getEmbedBag lidV p
  itemToF <- getsState $ flip itemToFull
  seps <- getsClient seps
  localTime <- getsState $ getLocalTime lidV
  getKind <- getsState $ flip getIidKind
  let aims = isJust $ makeLine False b p seps cops lvl
      tkid = lvl `at` p
      tile = okind cotile tkid
      vis | TK.tname tile == "unknown space" = "that is"
          | not canSee = "you remember"
          | not aims = "you are aware of"  -- walkable path a proxy for in LOS
          | otherwise = "you see"
      tilePart = MU.AW $ MU.Text $ TK.tname tile
      entrySentence pk blurb =
        makeSentence [blurb, MU.Text $ PK.pname $ okind coplace pk]
      placeBlurb = case EM.lookup p $ lentry lvl of
        Nothing -> ""
        Just (PK.PEntry pk) -> entrySentence pk "it is an entrance to"
        Just (PK.PAround pk) -> entrySentence pk "it surrounds"
        Just (PK.PExists _) -> ""
      itemLook (iid, kit@(k, _)) =
        let itemFull = itemToF iid
            arItem = aspectRecordFull itemFull
            nWs = partItemWsLong rwidth side factionD k localTime itemFull kit
            verb = if k == 1 || IA.checkFlag Ability.Condition arItem
                   then "is"
                   else "are"
            ik = itemKind itemFull
            desc = IK.idesc ik
        in (makeSentence ["There", verb, nWs], desc)
      embedKindList =
        map (\(iid, kit) -> (getKind iid, (iid, kit))) (EM.assocs embeds)
      embedList = map itemLook $ sortEmbeds cops tkid embedKindList
  return (makeSentence [vis, tilePart], placeBlurb, embedList)

-- | Produces a textual description of actors at a position.
lookAtActors :: MonadClientUI m
             => Point      -- ^ position to describe
             -> LevelId    -- ^ level the position is at
             -> m (Text, Text)
lookAtActors p lidV = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  side <- getsClient sside
  inhabitants <- getsState $ \s -> posToAidAssocs p lidV s
  sactorUI <- getsSession sactorUI
  let inhabitantsUI =
        map (\(aid2, b2) -> (aid2, b2, sactorUI EM.! aid2)) inhabitants
  factionD <- getsState sfactionD
  localTime <- getsState $ getLocalTime lidV
  s <- getState
  let actorsBlurb = case inhabitants of
        [] -> ("", "")
        (_, body) : rest ->
          let itemFull = itemToFull (btrunk body) s
              bfact = factionD EM.! bfid body
              -- Even if it's the leader, give his proper name, not 'you'.
              subjects = map (\(_, _, bUI) -> partActor bUI)
                             inhabitantsUI
              -- No "a" prefix even if singular and inanimate, to distinguish
              -- from items lying on the floor (and to simplify code).
              (subject, person) = squashedWWandW subjects
              resideVerb = case bwatch body of
                WWatch -> "be here"
                WWait 0 -> "idle here"
                WWait _ -> "brace for impact"
                WSleep -> "sleep here"
                WWake -> "be waking up"
              flyVerb | bproj body = "zip through here"
                      | isJust $ btrajectory body = "move through here"
                      | otherwise = resideVerb
              guardVerbs = guardItemVerbs body s
              verbs = flyVerb : guardVerbs
              projDesc | not $ bproj body = ""
                       | otherwise =
                let kit = beqp body EM.! btrunk body
                    ps = [partItemMediumAW rwidth side factionD localTime
                                           itemFull kit]
                    tailWords = tail . T.words . makePhrase
                in if tailWords ps == tailWords subjects
                   then ""
                   else makeSentence $ "this is" : ps
              factDesc = case jfid $ itemBase itemFull of
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
              idesc = IK.idesc $ itemKind itemFull
              -- If many different actors, only list names.
              sameTrunks = all (\(_, b) -> btrunk b == btrunk body) rest
              desc = if sameTrunks then projDesc <+> factDesc <+> idesc else ""
              -- Both description and faction blurb may be empty.
              pdesc = if desc == "" then "" else "(" <> desc <> ")"
              onlyIs = bwatch body == WWatch && null guardVerbs
          in if | bhp body <= 0 && not (bproj body) ->
                  ( makeSentence
                      (MU.SubjectVerbSg (head subjects) "lie here"
                       : if null guardVerbs
                         then []
                         else [ MU.SubjectVVxV "and" MU.Sg3rd MU.No
                                               "and" guardVerbs
                              , "any more" ])
                    <+> case subjects of
                          _ : projs@(_ : _) ->
                            let (subjectProjs, personProjs) =
                                  squashedWWandW projs
                            in makeSentence
                                 [MU.SubjectVerb personProjs MU.Yes
                                                 subjectProjs "can be seen"]
                          _ -> ""
                  , "" )
                | null rest || onlyIs ->
                  ( makeSentence
                      [MU.SubjectVVxV "and" person MU.Yes subject verbs]
                  , pdesc )
                | otherwise ->
                  ( makeSentence [subject, "can be seen"]
                    <+> if onlyIs
                        then ""
                        else makeSentence [MU.SubjectVVxV "and" MU.Sg3rd MU.Yes
                                                          (head subjects) verbs]
                  , "" )
  return actorsBlurb

guardItemVerbs :: Actor -> State -> [MU.Part]
guardItemVerbs body s =
  -- We only hint while, in reality, currently the client knows
  -- all the items in eqp of the foe. But we may remove the knowledge
  -- in the future and, anyway, it would require a dedicated
  -- UI mode beyond a couple of items per actor.
  let itemsSize = guardItemSize body s
      belongingsVerbs | itemsSize == 1 = ["fondle a trinket"]
                      | itemsSize > 1 = ["guard a hoard"]
                      | otherwise = []
  in if bproj body
     then []
     else belongingsVerbs

guardItemSize :: Actor -> State -> Int
guardItemSize body s =
  let toReport iid =
        let itemKind = getIidKind iid s
        in fromMaybe 0 (lookup IK.UNREPORTED_INVENTORY (IK.ifreq itemKind)) <= 0
  in length $ filter toReport $ EM.keys (beqp body)

-- | Produces a textual description of items at a position.
lookAtItems :: MonadClientUI m
            => Bool       -- ^ can be seen right now?
            -> Point      -- ^ position to describe
            -> ActorId    -- ^ the actor that looks
            -> m Text
lookAtItems canSee p aid = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  itemToF <- getsState $ flip itemToFull
  b <- getsState $ getActorBody aid
  -- Not using @viewedLevelUI@, because @aid@ may be temporarily not a leader.
  saimMode <- getsSession saimMode
  let lidV = maybe (blid b) aimLevelId saimMode
  localTime <- getsState $ getLocalTime lidV
  subject <- partActorLeader aid
  is <- getsState $ getFloorBag lidV p
  side <- getsClient sside
  factionD <- getsState sfactionD
  globalTime <- getsState stime
  getKind <- getsState $ flip getIidKindId
  let standingOn = p == bpos b && lidV == blid b
      verb = MU.Text $ if | standingOn -> if bhp b > 0
                                          then "stand on"
                                          else "fall over"
                          | canSee -> "notice"
                          | otherwise -> "remember"
      nWs (iid, kit@(k, _)) =
        partItemWs rwidth side factionD k localTime (itemToF iid) kit
      object = case EM.assocs is of
        ii : _ : _ : _ | standingOn && bfid b == side ->
          MU.Phrase [nWs ii, "and other items"]
          -- the actor is ours, so can see details with inventory commands
        iis -> MU.WWandW $ map nWs $ map snd $ sortOn fst
               $ map (\(iid, kit) -> (getKind iid, (iid, kit))) iis
  -- Here @squashedWWandW@ is not needed, because identical items at the same
  -- position are already merged in the floor item bag and multiple identical
  -- messages concerning different positions are merged with <x7>
  -- to distinguish from a stack of items at a single position.
  return $! if EM.null is || globalTime == timeZero
            then ""
            else makeSentence [MU.SubjectVerbSg subject verb, object]

lookAtStash :: MonadClientUI m => LevelId -> Point -> m Text
lookAtStash lidV p = do
  side <- getsClient sside
  factionD <- getsState sfactionD
  let locateStash (fid, fact) = case gstash fact of
        Just (lid, pos) | lid == lidV  && pos == p ->
          Just $ if fid == side
                 then "Here is the shared inventory stash of your team."
                 else gname fact
                      <+> "set up their shared inventory stash there."
        _ -> Nothing
  return $! T.intercalate " " $ mapMaybe locateStash $ EM.assocs factionD

-- | Produces a textual description of everything at the requested
-- level's position.
lookAtPosition :: MonadClientUI m => LevelId -> Point -> m [(MsgClass, Text)]
lookAtPosition lidV p = do
  COps{cotile} <- getsState scops
  leader <- getLeaderUI
  per <- getPerFid lidV
  let canSee = ES.member p (totalVisible per)
  -- Show general info about current position.
  (tileBlurb, placeBlurb, embedsList) <- lookAtTile canSee p leader lidV
  (actorsBlurb, actorsDesc) <- lookAtActors p lidV
  itemsBlurb <- lookAtItems canSee p leader
  stashBlurb <- lookAtStash lidV p
  lvl@Level{lsmell, ltime} <- getLevel lidV
  let smellBlurb = case EM.lookup p lsmell of
        Just sml | sml > ltime ->
          let Delta t = smellTimeout `timeDeltaSubtract`
                          (sml `timeDeltaToFrom` ltime)
              seconds = t `timeFitUp` timeSecond
          in "A smelly body passed here around" <+> tshow seconds <> "s ago."
        _ -> ""
  embeds <- getsState $ getEmbedBag lidV p
  getKind <- getsState $ flip getIidKind
  let embedKindList = map (\(iid, kit) -> (getKind iid, (iid, kit)))
                          (EM.assocs embeds)
      feats = TK.tfeature $ okind cotile $ lvl `at` p
      tileActions = mapMaybe (Tile.parseTileAction False False embedKindList)
                             feats
      isEmbedAction Tile.EmbedAction{} = True
      isEmbedAction _ = False
      embedVerb = [ "activated"
                  | any isEmbedAction tileActions
                    && any (\(itemKind, _) -> not $ null $ IK.ieffects itemKind)
                           embedKindList ]
      isToAction Tile.ToAction{} = True
      isToAction _ = False
      isWithAction Tile.WithAction{} = True
      isWithAction _ = False
      isEmptyWithAction (Tile.WithAction [] _) = True
      isEmptyWithAction _ = False
      alterVerb | any isEmptyWithAction tileActions = ["very easily modified"]
                | any isToAction tileActions = ["easily modified"]
                | any isWithAction tileActions = ["potentially modified"]
                | otherwise = []
      verbs = embedVerb ++ alterVerb
      alterBlurb = if null verbs
                   then ""
                   else makeSentence ["can be", MU.WWandW verbs]
      toolFromAction (Tile.WithAction grps _) = Just grps
      toolFromAction _ = Nothing
      toolsToAlterWith = mapMaybe toolFromAction tileActions
      tItems = describeToolsAlternative toolsToAlterWith
      transformBlurb = if T.null tItems
                       then ""
                       else "The following items on the ground or in equipment enable special transformations:"
                            <+> tItems <> "."  -- not telling to what terrain
      actorEOL = if T.null actorsDesc
                    || T.null itemsBlurb && null embedsList
                 then ""
                 else "\n"
      itemsEOL = if T.null actorsDesc && T.null itemsBlurb
                    || null embedsList
                 then ""
                 else "\n"
  return $ [ (MsgPromptWarning, stashBlurb)
           , (MsgPromptThreat, actorsBlurb)
           , (MsgPrompt, actorsDesc
                         <> actorEOL)
           , (MsgPrompt, smellBlurb)
           , (MsgPromptItem, itemsBlurb
                             <> itemsEOL)
           , (MsgPromptFocus, tileBlurb)
           , (MsgPrompt, placeBlurb) ]
           ++ concatMap (\(embedName, embedDesc) ->
                [ (MsgPromptMention, embedName)
                , (MsgPrompt, embedDesc) ]) embedsList
           ++ [ (MsgPromptItem, alterBlurb <+> transformBlurb) ]

displayItemLore :: MonadClientUI m
                => ItemBag -> Int -> (ItemId -> ItemFull -> Int -> Text) -> Int
                -> SingleItemSlots
                -> m Bool
displayItemLore itemBag meleeSkill promptFun slotIndex lSlots = do
  CCUI{coscreen=ScreenContent{rwidth, rheight}} <- getsSession sccui
  side <- getsClient sside
  arena <- getArenaUI
  let lSlotsElems = EM.elems lSlots
      lSlotsBound = length lSlotsElems - 1
      iid2 = lSlotsElems !! slotIndex
      kit2@(k, _) = itemBag EM.! iid2
  itemFull2 <- getsState $ itemToFull iid2
  localTime <- getsState $ getLocalTime arena
  factionD <- getsState sfactionD
  -- The hacky level 0 marks items never seen, but sent by server at gameover.
  jlid <- getsSession $ fromMaybe (toEnum 0) <$> EM.lookup iid2 . sitemUI
  FontSetup{propFont} <- getFontSetup
  let attrLine = itemDesc rwidth True side factionD meleeSkill
                          CGround localTime jlid itemFull2 kit2
      ov = EM.singleton propFont $ offsetOverlay
           $ splitAttrString rwidth attrLine
      keys = [K.spaceKM, K.escKM]
             ++ [K.upKM | slotIndex /= 0]
             ++ [K.downKM | slotIndex /= lSlotsBound]
  promptAdd0 $ promptFun iid2 itemFull2 k
  slides <- overlayToSlideshow (rheight - 2) keys (ov, [])
  km <- getConfirms ColorFull keys slides
  case K.key km of
    K.Space -> return True
    K.Up ->
      displayItemLore itemBag meleeSkill promptFun (slotIndex - 1) lSlots
    K.Down ->
      displayItemLore itemBag meleeSkill promptFun (slotIndex + 1) lSlots
    K.Esc -> return False
    _ -> error $ "" `showFailure` km

viewLoreItems :: MonadClientUI m
              => String -> SingleItemSlots -> ItemBag -> Text
              -> (Int -> SingleItemSlots -> m Bool) -> Bool
              -> m K.KM
viewLoreItems menuName lSlotsRaw trunkBag prompt examItem displayRanged = do
  CCUI{coscreen=ScreenContent{rheight}} <- getsSession sccui
  arena <- getArenaUI
  itemToF <- getsState $ flip itemToFull
  let keysPre = [K.spaceKM, K.mkChar '<', K.mkChar '>', K.escKM]
      lSlots = sortSlotMap itemToF lSlotsRaw
  promptAdd0 prompt
  io <- itemOverlay lSlots arena trunkBag displayRanged
  itemSlides <- overlayToSlideshow (rheight - 2) keysPre io
  let keyOfEKM (Left km) = km
      keyOfEKM (Right SlotChar{slotChar}) = [K.mkChar slotChar]
      allOKX = concatMap snd $ slideshow itemSlides
      keysMain = keysPre ++ concatMap (keyOfEKM . fst) allOKX
      viewAtSlot slot = do
        let ix0 = fromMaybe (error $ show slot)
                            (findIndex (== slot) $ EM.keys lSlots)
        go2 <- examItem ix0 lSlots
        if go2
        then viewLoreItems menuName lSlots trunkBag prompt
                           examItem displayRanged
        else return K.escKM
  ekm <- displayChoiceScreen menuName ColorFull False itemSlides keysMain
  case ekm of
    Left km | km `elem` [K.spaceKM, K.mkChar '<', K.mkChar '>', K.escKM] ->
      return km
    Left K.KM{key=K.Char l} -> viewAtSlot $ SlotChar 0 l
      -- other prefixes are not accessible via keys; tough luck; waste of effort
    Left km -> error $ "" `showFailure` km
    Right slot -> viewAtSlot slot

cycleLore :: MonadClientUI m => [m K.KM] -> [m K.KM] -> m ()
cycleLore _ [] = return ()
cycleLore seen (m : rest) = do  -- @seen@ is needed for SPACE to end cycling
  km <- m
  if | km == K.spaceKM -> cycleLore (m : seen) rest
     | km == K.mkChar '>' -> if null rest
                             then cycleLore [] (reverse $ m : seen)
                             else cycleLore (m : seen) rest
     | km == K.mkChar '<' -> case seen of
                               prev : ps -> cycleLore ps (prev : m : rest)
                               [] -> case reverse (m : rest) of
                                 prev : ps -> cycleLore ps [prev]
                                 [] -> error "cycleLore: screens disappeared"
     | km == K.escKM -> return ()
     | otherwise -> error "cycleLore: unexpected key"

spoilsBlurb :: Text -> Int -> Int -> Text
spoilsBlurb currencyName total dungeonTotal =
  if | dungeonTotal == 0 ->
         "All the spoils of your team are of the practical kind."
     | total == 0 -> "Your team haven't found any genuine treasure yet."
     | otherwise -> makeSentence
         [ "your team's spoils are worth"
         , MU.CarAWs total $ MU.Text currencyName
         , "out of the rumoured total"
         , MU.Cardinal dungeonTotal ]

ppContainerWownW :: MonadClientUI m
                 => (ActorId -> m MU.Part) -> Bool -> Container -> m [MU.Part]
ppContainerWownW ownerFun addPrepositions c = case c of
  CFloor{} -> return ["nearby"]
  CEmbed{} -> return ["embedded nearby"]
  CActor aid store -> do
    side <- getsClient sside
    b <- getsState $ getActorBody aid
    owner <- ownerFun aid
    fidName <- getsState $ gname . (EM.! bfid b) . sfactionD
    let (preposition, noun) = ppCStore store
        prep = [MU.Text preposition | addPrepositions]
    return $! prep ++ case store of
      CGround -> MU.Text noun : if bproj b then [] else ["under", owner]
      CStash -> if bfid b /= side
                then [MU.WownW (MU.Text fidName) (MU.Text noun)]
                else [MU.Text noun]
      _ -> [MU.WownW owner (MU.Text noun)]
  CTrunk{} -> error $ "" `showFailure` c
