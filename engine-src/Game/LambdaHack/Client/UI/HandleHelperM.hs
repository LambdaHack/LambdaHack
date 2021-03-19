{-# LANGUAGE TupleSections #-}
-- | Helper functions for both inventory management and human commands.
module Game.LambdaHack.Client.UI.HandleHelperM
  ( FailError, showFailError, MError, mergeMError, FailOrCmd, failWith
  , failSer, failMsg, weaveJust
  , memberCycle, memberCycleLevel, partyAfterLeader
  , pickLeader, pickLeaderWithPointer
  , itemOverlay, skillsOverlay
  , placesFromState, placesOverlay
  , describeMode, modesOverlay
  , pickNumber, guardItemSize, lookAtItems, lookAtStash, lookAtPosition
  , displayItemLore, viewLoreItems, cycleLore, spoilsBlurb
  , ppContainerWownW, nxtGameMode
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
import qualified Data.Map.Strict as M
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
import           Game.LambdaHack.Client.UI.PointUI
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.Slideshow
import           Game.LambdaHack.Client.UI.SlideshowM
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.ClientOptions
import           Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.HighScore as HighScore
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
import qualified Game.LambdaHack.Content.ModeKind as MK
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
memberCycleLevel :: (MonadClient m, MonadClientUI m)
                 => Bool -> Direction -> m MError
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
memberCycle :: (MonadClient m, MonadClientUI m) => Bool -> Direction -> m MError
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
pickLeader :: (MonadClient m, MonadClientUI m) => Bool -> ActorId -> m Bool
pickLeader verbose aid = do
  leader <- getLeaderUI
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
        msgAdd MsgPointmanSwap $ makeSentence [subject, "picked as a pointman"]
      -- Update client state.
      updateClientLeader aid
      -- Move the xhair, if active, to the new level.
      modifySession $ \sess -> sess {saimMode =
        (\aimMode -> aimMode {aimLevelId = blid body}) <$> saimMode sess}
      -- Inform about items, etc.
      (itemsBlurb, _) <- lookAtItems True (bpos body) aid Nothing
      stashBlurb <- lookAtStash (blid body) (bpos body)
      when verbose $ msgAdd MsgAtFeetMinor $ stashBlurb <+> itemsBlurb
      return True

pickLeaderWithPointer :: (MonadClient m, MonadClientUI m) => m MError
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
  PointUI x y <- getsSession spointer
  let (px, py) = (x `div` 2, y - mapStartY)
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
  let !_A = assert (allB (`elem` EM.elems lSlots) (EM.keys bag)
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
                arItem = aspectRecordFull itemFull
                colorSymbol =
                  if IA.checkFlag Ability.Condition arItem
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
                kx = (Right l, ( PointUI 0 0
                               , ButtonWidth propFont (5 + T.length phrase) ))
            in Just ((al1, xal2), kx)
      (ts, kxs) = unzip $ mapMaybe pr $ EM.assocs lSlots
      (tsLab, tsDesc) = unzip ts
      ovsLab = EM.singleton squareFont $ offsetOverlay tsLab
      ovsDesc = EM.singleton propFont $ offsetOverlayX tsDesc
      renumber y (km, (PointUI x _, len)) = (km, (PointUI x y, len))
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
        in (triple, (Right c, ( PointUI 0 y
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
  FontSetup{..} <- getFontSetup
  places <- getsState $ placesFromState coplace soptions
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
            !pButton = PointUI 0 y
            !widthButton = ButtonWidth propFont lenButton
        in ( textToAL tSlot
           , (lenSlot, textToAL tBlurb)
           , (Right c, (pButton, widthButton)) )
      (plLab, plDesc, kxs) = unzip3 $ zipWith prSlot (zip [0..] allSlots)
                                    $ EM.assocs places
      placeLab = EM.singleton squareFont $ offsetOverlay plLab
      placeDesc = EM.singleton propFont $ offsetOverlayX plDesc
  return (EM.unionWith (++) placeLab placeDesc, kxs)

describeMode :: MonadClientUI m
             => Bool -> ContentId MK.ModeKind
             -> m (EM.EnumMap DisplayFont Overlay)
describeMode addTitle gameModeId = do
  COps{comode} <- getsState scops
  CCUI{coscreen=ScreenContent{rwidth}}
    <- getsSession sccui
  FontSetup{..} <- getFontSetup
  scoreDict <- getsState shigh
  scampings <- getsClient scampings
  srestarts <- getsClient srestarts
  let gameMode = okind comode gameModeId
      duplicateEOL '\n' = "\n\n"
      duplicateEOL c = T.singleton c
      sections =
        [ ( textFgToAS Color.BrGreen "The story so far:"
          , T.concatMap duplicateEOL (MK.mdesc gameMode) )
        , ( textFgToAS Color.cMeta "Rules of the game:"
          , MK.mrules gameMode )
        , ( textFgToAS Color.BrCyan "Running commentary:"
          , T.concatMap duplicateEOL (MK.mreason gameMode) )
        , ( textFgToAS Color.cGreed "Hints, not needed unless stuck:"
          , T.concatMap duplicateEOL (MK.mhint gameMode) )
        ]
      renderSection :: (AttrString, Text) -> Maybe [(DisplayFont, AttrString)]
      renderSection (header, desc) =
        if T.null desc
        then Nothing
        else Just [(monoFont, header), (propFont, textToAS desc)]
      title = if addTitle
              then "\nYou are surviving the '"
                   <> MK.mname gameMode
                   <> "' adventure.\n"
              else ""
      blurb = map (second $ splitAttrString (rwidth - 2) (rwidth - 2)) $
        (propFont, textToAS (title <> "\n"))
        : concat (intersperse [(monoFont, textToAS "\n")]
                              (mapMaybe renderSection sections))
      -- Colour is used to delimit the section when displayed in one
      -- column, when using square fonts only.
      blurbEnd = map (second $ splitAttrString (rwidth - 2) (rwidth - 2)) $
        ( propFont
        , textFgToAS Color.Brown
                     "\nThis adventure's endings experienced so far:\n\n" )
          : if null sectionsEndAS
            then [(monoFont, textToAS "*none*")]
            else sectionsEndAS
      sectionsEndAS = concat (intersperse [(monoFont, textToAS "\n")]
                                          (mapMaybe renderSection sectionsEnd))
      sectionsEnd = map outcomeSection [minBound..maxBound]
      outcomeSection :: MK.Outcome -> (AttrString, Text)
      outcomeSection outcome =
        ( renderOutcome outcome
        , if not (outcomeSeen outcome)
          then ""  -- a possible spoiler and lack of sense of progression
          else T.concatMap duplicateEOL
               $ fromMaybe "" $ lookup outcome
               $ MK.mendMsg gameMode ++ endMsgDefault  -- left-biased
        )
      -- These are not added to @mendMsg@, because they only fit here.
      endMsgDefault =
        [ (MK.Restart, "No shame there is in noble defeat and there is honour in perseverance. Sometimes there are ways and places to turn rout into victory.")
        , (MK.Camping, "Don't fear to take breaks. While you move, others move, even on distant floors, but while you stay still, the world stays still.")
        ]
      scoreRecords = maybe [] HighScore.unTable $ EM.lookup gameModeId scoreDict
      outcomeSeen :: MK.Outcome -> Bool
      outcomeSeen outcome = case outcome of
        MK.Camping -> gameModeId `ES.member` scampings
        MK.Restart -> gameModeId `ES.member` srestarts
        _ -> outcome `elem` map (stOutcome . HighScore.getStatus) scoreRecords
      -- Camping not taken into account.
      lastOutcome :: MK.Outcome
      lastOutcome = if null scoreRecords
                    then MK.Restart  -- only if nothing else
                    else stOutcome . HighScore.getStatus
                         $ maximumBy (comparing HighScore.getDate) scoreRecords
      renderOutcome :: MK.Outcome -> AttrString
      renderOutcome outcome =
        let color | outcome `elem` MK.deafeatOutcomes = Color.cVeryBadEvent
                  | outcome `elem` MK.victoryOutcomes = Color.cVeryGoodEvent
                  | otherwise = Color.cNeutralEvent
            lastRemark
              | outcome /= lastOutcome = ""
              | outcome `elem` MK.deafeatOutcomes = "(last suffered ending)"
              | outcome `elem` MK.victoryOutcomes = "(last achieved ending)"
              | otherwise = "(last seen ending)"
        in textToAS "Game over message when"
           <+:> (textFgToAS color (T.toTitle $ MK.nameOutcomePast outcome)
                 <+:> textToAS lastRemark)
           <> textToAS ":"
      shiftPointUI x (PointUI x0 y0) = PointUI (x0 + x) y0
  return $! if isSquareFont propFont
            then EM.singleton squareFont  -- single column, single font
                 $ offsetOverlayX
                 $ map (\t -> (2, t))
                 $ concatMap snd $ blurb ++ blurbEnd
            else EM.unionWith (++)
                 (EM.map (map (first $ shiftPointUI 1))
                  $ attrLinesToFontMap 0 blurb)
                 (EM.map (map (first $ shiftPointUI $ rwidth + 1))
                  $ attrLinesToFontMap 0 blurbEnd)

modesOverlay :: MonadClientUI m => m OKX
modesOverlay = do
  COps{comode} <- getsState scops
  FontSetup{..} <- getFontSetup
  svictories <- getsClient svictories
  nxtChal <- getsClient snxtChal  -- mark victories only for current difficulty
  let f !acc _p !i !a = (i, a) : acc
      campaignModes = ofoldlGroup' comode MK.CAMPAIGN_SCENARIO f []
      prSlot :: (Int, SlotChar)
             -> (ContentId MK.ModeKind, MK.ModeKind)
             -> (AttrLine, (Int, AttrLine), KYX)
      prSlot (y, c) (gameModeId, gameMode) =
        let modeName = MK.mname gameMode
            victories = case EM.lookup gameModeId svictories of
              Nothing -> 0
              Just cm -> fromMaybe 0 (M.lookup nxtChal cm)
            markMode t = if victories > 0
                         then T.snoc (T.init t) '>'
                         else t
            !tSlot = markMode $ slotLabel c
            !lenSlot = 2 * T.length tSlot
            !tBlurb = " " <> modeName
            !lenButton = lenSlot + T.length tBlurb
            !pButton = PointUI 0 y
            !widthButton = ButtonWidth propFont lenButton
        in ( textToAL tSlot
           , (lenSlot, textToAL tBlurb)
           , (Right c, (pButton, widthButton)) )
      (plLab, plDesc, kxs) =
        unzip3 $ zipWith prSlot (zip [0..] allSlots) campaignModes
      placeLab = EM.singleton squareFont $ offsetOverlay plLab
      placeDesc = EM.singleton propFont $ offsetOverlayX plDesc
  return (EM.unionWith (++) placeLab placeDesc, kxs)

pickNumber :: (MonadClient m, MonadClientUI m)
           => Bool -> Int -> m (Either MError Int)
pickNumber askNumber kAll = assert (kAll >= 1) $ do
  let shownKeys = [ K.returnKM, K.spaceKM, K.mkChar '+', K.mkChar '-'
                  , K.backspaceKM, K.escKM ]
      frontKeyKeys = shownKeys ++ map K.mkChar ['0'..'9']
      gatherNumber kCur = assert (1 <= kCur && kCur <= kAll) $ do
        let kprompt = "Choose number:" <+> tshow kCur
        msgAdd MsgPromptGeneric kprompt
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
           => Bool             -- ^ can be seen right now?
           -> Point            -- ^ position to describe
           -> ActorId          -- ^ the actor that looks
           -> LevelId          -- ^ level the position is at
           -> Maybe MU.Person  -- ^ grammatical person of the item(s), if any
           -> m (Text, Text, [((Int, MU.Part), Text)])
lookAtTile canSee p aid lidV mperson = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  cops@COps{cotile, coplace} <- getsState scops
  side <- getsClient sside
  factionD <- getsState sfactionD
  b <- getsState $ getActorBody aid
  lvl <- getLevel lidV
  saimMode <- getsSession saimMode
  embeds <- getsState $ getEmbedBag lidV p
  itemToF <- getsState $ flip itemToFull
  seps <- getsClient seps
  localTime <- getsState $ getLocalTime lidV
  getKind <- getsState $ flip getIidKind
  let inhabitants = posToAidsLvl p lvl
      detail = maybe DetailAll detailLevel saimMode
      aims = isJust $ makeLine False b p seps cops lvl
      tkid = lvl `at` p
      tile = okind cotile tkid
      vis | TK.tname tile == "unknown space" = "that is"
          | not (null inhabitants) && bpos b /= p = "the terrain here is"
          | not canSee = "you remember"
          | not aims = "you are aware of"  -- walkable path a proxy for in LOS
          | otherwise = "you see"
      vperson = case mperson of
        Nothing -> vis
        Just MU.Sg1st -> error "an item speaks in first person"
        Just MU.Sg3rd -> "It is laying on"
        Just MU.PlEtc -> "They lay on"
      tilePart = MU.AW $ MU.Text $ TK.tname tile
      entrySentence pk blurb =
        makeSentence [blurb, MU.Text $ PK.pname $ okind coplace pk]
      placeBlurb = case EM.lookup p $ lentry lvl of
        Nothing -> ""
        Just (PK.PEntry pk) -> entrySentence pk "it is an entrance to"
        Just (PK.PAround pk) -> entrySentence pk "it surrounds"
        Just (PK.PExists _) -> ""
      embedLook (iid, kit@(k, _)) =
        let itemFull = itemToF iid
            nWs = partItemWsDetail detail
                                   rwidth side factionD k localTime itemFull kit
            desc = IK.idesc $ itemKind itemFull
        in ((k, nWs), desc)
      embedKindList =
        map (\(iid, kit) -> (getKind iid, (iid, kit))) (EM.assocs embeds)
      embedList = map embedLook $ sortEmbeds cops tkid embedKindList
  return (makeSentence [vperson, tilePart], placeBlurb, embedList)

-- | Produces a textual description of actors at a position.
lookAtActors :: MonadClientUI m
             => Point      -- ^ position to describe
             -> LevelId    -- ^ level the position is at
             -> m (Text, Maybe (MU.Part, Bool), Text)
lookAtActors p lidV = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  side <- getsClient sside
  inhabitants <- getsState $ posToAidAssocs p lidV
  factionD <- getsState sfactionD
  localTime <- getsState $ getLocalTime lidV
  saimMode <- getsSession saimMode
  let detail = maybe DetailAll detailLevel saimMode
  case inhabitants of
    [] -> return ("", Nothing, "")
    (aid, body) : rest -> do
      actorPronoun <- partPronounLeader aid
      itemFull <- getsState $ itemToFull $ btrunk body
      guardVerbs <- getsState $ guardItemVerbs body
      subjects <- mapM (partActorLeader . fst) inhabitants
      let bfact = factionD EM.! bfid body
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
          verbs = flyVerb : guardVerbs
          projDesc | not (bproj body) || detail < DetailAll = ""
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
              let dominatedBy = if bfid body == side then "us" else gname bfact
                  tfact = factionD EM.! tfid
              in "Originally of" <+> gname tfact
                 <> ", now fighting for" <+> dominatedBy <> "."
            _ | detail < DetailAll -> ""  -- only domination worth spamming
            _ | bfid body == side -> ""  -- just one of us
            _ | bproj body -> "Launched by" <+> gname bfact <> "."
            _ -> "One of" <+> gname bfact <> "."
          idesc = if detail < DetailAll
                  then ""
                  else IK.idesc $ itemKind itemFull
          -- If many different actors, only list names.
          sameTrunks = all (\(_, b) -> btrunk b == btrunk body) rest
          desc = wrapInParens $ projDesc <+> factDesc <+> idesc
          onlyIs = bwatch body == WWatch && null guardVerbs
          allBlurb = makeSentence [MU.SubjectVVxV "and" person MU.Yes
                                                  subject verbs]
          headBlurb = makeSentence [MU.SubjectVVxV "and" MU.Sg3rd MU.Yes
                                                   (head subjects) verbs]
          andProjectiles = case subjects of
            _ : projs@(_ : _) ->
              let (subjectProjs, personProjs) =
                    squashedWWandW projs
              in makeSentence
                   [MU.SubjectVerb personProjs MU.Yes
                                   subjectProjs "can be seen"]
            _ -> ""
          actorAlive = bhp body >= 0
          mactorPronounAlive =
            if bproj body then Nothing else Just (actorPronoun, actorAlive)
      return $!
        if | not actorAlive && not (bproj body) ->
             ( makeSentence
                 (MU.SubjectVerbSg (head subjects) "lie here"
                  : if null guardVerbs
                    then []
                    else [ MU.SubjectVVxV "and" MU.Sg3rd MU.No
                                          "and" guardVerbs
                         , "any more" ])
             , mactorPronounAlive
             , wrapInParens desc <+> andProjectiles )
           | sameTrunks ->  -- only non-proj or several similar projectiles
             ( allBlurb
             , mactorPronounAlive
             , desc )
           | not (bproj body) && onlyIs ->
             ( headBlurb
             , mactorPronounAlive
             , desc <+> andProjectiles )
           | not (bproj body) ->
             ( makeSentence [subject, "can be seen"] <+> headBlurb
             , mactorPronounAlive
             , desc )
           | otherwise -> assert (bproj body && not (null rest))
             ( makeSentence [subject, "can be seen"]
             , Nothing
             , "" )

guardItemVerbs :: Actor -> State -> [MU.Part]
guardItemVerbs body s =
  -- We only hint while, in reality, currently the client knows
  -- all the items in eqp of the foe. But we may remove the knowledge
  -- in the future and, anyway, it would require a dedicated
  -- UI mode beyond a couple of items per actor.
  let itemsSize = guardItemSize body s
      belongingsVerbs | itemsSize == 1 = ["fondle a trinket"]
                      | itemsSize > 1 = ["haul a hoard"]
                      | otherwise = []
  in if bproj body then [] else belongingsVerbs

guardItemSize :: Actor -> State -> Int
guardItemSize body s =
  let toReport iid =
        let itemKind = getIidKind iid s
        in fromMaybe 0 (lookup IK.UNREPORTED_INVENTORY (IK.ifreq itemKind)) <= 0
  in length $ filter toReport $ EM.keys (beqp body)

-- | Produces a textual description of items at a position.
lookAtItems :: MonadClientUI m
            => Bool     -- ^ can be seen right now?
            -> Point    -- ^ position to describe
            -> ActorId  -- ^ the actor that looks
            -> Maybe (MU.Part, Bool)
                        -- ^ pronoun for the big actor at the position, if any,
                        --   and whether the big actor is alive
            -> m (Text, Maybe MU.Person)
lookAtItems canSee p aid mactorPronounAlive = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  side <- getsClient sside
  itemToF <- getsState $ flip itemToFull
  b <- getsState $ getActorBody aid
  -- Not using @viewedLevelUI@, because @aid@ may be temporarily not a leader.
  saimMode <- getsSession saimMode
  let lidV = maybe (blid b) aimLevelId saimMode
      standingOn = p == bpos b && lidV == blid b
      -- In exploration mode the detail level depends on whether the actor
      -- that looks stand over the items, because then he can check details
      -- with inventory commands (or look in aiming mode).
      detailExploration =
        if standingOn && bfid b == side then DetailMedium else DetailAll
      detail = maybe detailExploration detailLevel saimMode
  localTime <- getsState $ getLocalTime lidV
  subjectAid <- partActorLeader aid
  is <- getsState $ getFloorBag lidV p
  factionD <- getsState sfactionD
  globalTime <- getsState stime
  getKind <- getsState $ flip getIidKindId
  leaderPronoun <- partPronounLeader aid
  let mLeader = if standingOn then Just (leaderPronoun, bhp b >= 0) else Nothing
      mactorPronounAliveLeader = maybe mLeader Just mactorPronounAlive
      (subject, verb) = case mactorPronounAliveLeader of
        Just (actorPronoun, actorAlive) ->
          (actorPronoun, if actorAlive then "stand over" else "fall over")
        Nothing -> (subjectAid, if canSee then "notice" else "remember")
      nWs (iid, kit@(k, _)) =
        partItemWsDetail detail
                         rwidth side factionD k localTime (itemToF iid) kit
      (object, person) = case EM.assocs is of
        [(_, (k, _))] | detail == DetailLow ->
          (if k == 1 then "an item" else "an item stack", MU.Sg3rd)
        _ | detail == DetailLow -> ("some items", MU.PlEtc)
        ii : _ : _ : _ | detail <= DetailMedium ->
          (MU.Phrase [nWs ii, "and other items"], MU.PlEtc)
        [ii@(_, (1, _))] -> (nWs ii, MU.Sg3rd)
        iis -> (MU.WWandW $ map nWs $ sortOn (getKind . fst) iis, MU.PlEtc)
  -- Here @squashedWWandW@ is not needed, because identical items at the same
  -- position are already merged in the floor item bag and multiple identical
  -- messages concerning different positions are merged with <x7>
  -- to distinguish from a stack of items at a single position.
  return ( if EM.null is || globalTime == timeZero
           then ""
           else makeSentence [MU.SubjectVerbSg subject (MU.Text verb), object]
         , if isNothing mactorPronounAlive then Just person else Nothing )

lookAtStash :: MonadClientUI m => LevelId -> Point -> m Text
lookAtStash lidV p = do
  side <- getsClient sside
  factionD <- getsState sfactionD
  let locateStash (fid, fact) = case gstash fact of
        Just (lid, pos) | lid == lidV  && pos == p ->
          Just $ if fid == side
                 then "Here is the shared inventory stash of your team."
                 else gname fact
                      <+> "set up their shared inventory stash here."
        _ -> Nothing
  return $! T.intercalate " " $ mapMaybe locateStash $ EM.assocs factionD

-- | Produces a textual description of everything at the requested
-- level's position.
lookAtPosition :: MonadClientUI m
               => LevelId -> Point -> m [(MsgClassShow, Text)]
lookAtPosition lidV p = do
  COps{cotile} <- getsState scops
  side <- getsClient sside
  leader <- getLeaderUI
  per <- getPerFid lidV
  let canSee = ES.member p (totalVisible per)
  (actorsBlurb, mactorPronounAlive, actorsDesc) <- lookAtActors p lidV
  (itemsBlurb, mperson) <- lookAtItems canSee p leader mactorPronounAlive
  let tperson = if T.null itemsBlurb then Nothing else mperson
  (tileBlurb, placeBlurb, embedsList) <- lookAtTile canSee p leader lidV tperson
  inhabitants <- getsState $ posToAidAssocs p lidV
  let actorMsgClass =
        if (bfid . snd <$> inhabitants) == [side]
        then MsgPromptGeneric  -- our single proj or non-proj; tame
        else MsgPromptActors
  stashBlurb <- lookAtStash lidV p
  lvl@Level{lsmell, ltime} <- getLevel lidV
  saimMode <- getsSession saimMode
  let detail = maybe DetailAll detailLevel saimMode
      smellBlurb = case EM.lookup p lsmell of
        Just sml | sml > ltime ->
          let Delta t = smellTimeout `timeDeltaSubtract`
                          (sml `timeDeltaToFrom` ltime)
              seconds = t `timeFitUp` timeSecond
          in "A smelly body passed here around" <+> tshow seconds <> "s ago."
        _ -> ""
  embeds <- getsState $ getEmbedBag lidV p
  getKind <- getsState $ flip getIidKind
  let ppEmbedName :: (Int, MU.Part) -> Text
      ppEmbedName (k, part) =
        let verb = if k == 1 then "is" else "are"
        in makeSentence ["There", verb, part]
      embedKindList = map (\(iid, kit) -> (getKind iid, (iid, kit)))
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
      modifyBlurb = alterBlurb <+> transformBlurb
      midEOL = if detail < DetailHigh
                  || T.null stashBlurb && T.null actorsDesc
                  || T.null smellBlurb && T.null itemsBlurb
                  || null embedsList && T.null modifyBlurb
               then ""
               else "\n"
      ms = [ (MsgPromptAction, stashBlurb)
           , (actorMsgClass, actorsBlurb)
           , (MsgPromptGeneric, actorsDesc <> midEOL) ]
           ++ [(MsgPromptGeneric, smellBlurb) | detail >= DetailHigh]
           ++ [(MsgPromptItems, itemsBlurb <> midEOL)]
           ++ [(MsgPromptFocus, tileBlurb) | detail >= DetailHigh
                                             || detail == DetailMedium
                                                && not (null embedsList)]
           ++ [(MsgPromptGeneric, placeBlurb) | detail >= DetailHigh]
           ++ case detail of
                DetailAll ->
                  concatMap (\(embedName, embedDesc) ->
                    [ (MsgPromptMention, ppEmbedName embedName)
                    , (MsgPromptGeneric, embedDesc) ]) embedsList
                DetailLow ->
                  [(MsgPromptMention, case embedsList of
                    [] -> ""
                    [((k, _), _)] ->
                      ppEmbedName (1, if k == 1
                                      then "an embedded item"
                                      else "a stack of embedded items")
                    _ -> ppEmbedName (9, "some embedded items"))]
                _ -> let n = sum $ map (fst . fst) embedsList
                         wWandW = MU.WWandW $ map (snd . fst) embedsList
                     in [(MsgPromptMention, ppEmbedName (n, wWandW)) | n > 0]
           ++ [(MsgPromptModify, modifyBlurb) | detail == DetailAll]
  return $! if all (T.null . snd) ms && detail > DetailLow
            then [(MsgPromptFocus, tileBlurb)]
            else ms

displayItemLore ::(MonadClient m, MonadClientUI m)
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
  FontSetup{..} <- getFontSetup
  let descAl = itemDesc rwidth True side factionD meleeSkill
                        CGround localTime jlid itemFull2 kit2
      (descSymAl, descBlurbAl) = span (/= Color.spaceAttrW32) descAl
      descSym = offsetOverlay $ splitAttrString rwidth rwidth descSymAl
      descBlurb = offsetOverlayX $
        case splitAttrString rwidth rwidth $ stringToAS "xx" ++ descBlurbAl of
          [] -> error "splitting AttrString loses characters"
          al1 : rest ->
            (2, attrStringToAL $ drop 2 $ attrLine al1) : map (0,) rest
      ov = EM.insertWith (++) squareFont descSym
           $ EM.singleton propFont descBlurb
      keys = [K.spaceKM, K.escKM]
             ++ [K.upKM | slotIndex /= 0]
             ++ [K.downKM | slotIndex /= lSlotsBound]
  msgAdd MsgPromptGeneric $ promptFun iid2 itemFull2 k
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

viewLoreItems :: (MonadClient m, MonadClientUI m)
              => String -> SingleItemSlots -> ItemBag -> Text
              -> (Int -> SingleItemSlots -> m Bool) -> Bool
              -> m K.KM
viewLoreItems menuName lSlotsRaw trunkBag prompt examItem displayRanged = do
  CCUI{coscreen=ScreenContent{rheight}} <- getsSession sccui
  arena <- getArenaUI
  itemToF <- getsState $ flip itemToFull
  let keysPre = [K.spaceKM, K.mkChar '<', K.mkChar '>', K.escKM]
      lSlots = sortSlotMap itemToF lSlotsRaw
  msgAdd MsgPromptGeneric prompt
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

nxtGameMode :: COps -> Int -> (ContentId MK.ModeKind, MK.ModeKind)
nxtGameMode COps{comode} snxtScenario =
  let f !acc _p !i !a = (i, a) : acc
      campaignModes = ofoldlGroup' comode MK.CAMPAIGN_SCENARIO f []
  in campaignModes !! (snxtScenario `mod` length campaignModes)
