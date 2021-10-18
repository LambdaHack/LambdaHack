-- | Helper functions for both inventory management and human commands.
module Game.LambdaHack.Client.UI.HandleHelperM
  ( FailError, showFailError, MError, mergeMError, FailOrCmd, failWith
  , failSer, failMsg, weaveJust
  , pointmanCycle, pointmanCycleLevel, partyAfterLeader
  , pickLeader, pickLeaderWithPointer
  , itemOverlay, skillsOverlay
  , placesFromState, placesOverlay
  , describeMode, modesOverlay
  , pickNumber, guardItemSize, lookAtItems, lookAtStash, lookAtPosition
  , displayItemLore, okxItemLorePointedAt, cycleLore, spoilsBlurb
  , ppContainerWownW, nxtGameMode
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , itemOverlayFromState, lookAtTile, lookAtActors, guardItemVerbs
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

-- | Switches current pointman to the next on the level, if any, wrapping.
pointmanCycleLevel :: MonadClientUI m
                   => ActorId -> Bool -> Direction -> m MError
pointmanCycleLevel leader verbose direction = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  lidV <- viewedLevelUI
  body <- getsState $ getActorBody leader
  hs <- partyAfterLeader leader
  let (autoDun, _) = autoDungeonLevel fact
  let hsSort = case direction of
        Forward -> hs
        Backward -> reverse hs
  case filter (\(_, b, _) -> blid b == lidV) hsSort of
    _ | autoDun && lidV /= blid body ->
      failMsg $ showReqFailure NoChangeDunLeader
    [] -> failMsg "cannot pick any other pointman on this level"
    (np, b, _) : _ -> do
      success <- pickLeader verbose np
      let !_A = assert (success `blame` "same leader"
                                `swith` (leader, np, b)) ()
      return Nothing

-- | Switches current pointman to the previous in the whole dungeon, wrapping.
pointmanCycle :: MonadClientUI m
              => ActorId -> Bool -> Direction -> m MError
pointmanCycle leader verbose direction = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
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
  side <- getsClient sside
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
  mleader <- getsClient sleader
  if mleader == Just aid
    then return False -- already picked
    else do
      body <- getsState $ getActorBody aid
      bodyUI <- getsSession $ getActorUI aid
      let !_A = assert (not (bproj body)
                        `blame` "projectile chosen as the pointman"
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
      (itemsBlurb, _) <-
        lookAtItems True (bpos body) (blid body) (Just aid) Nothing
      stashBlurb <- lookAtStash (bpos body) (blid body)
      when verbose $ msgAdd MsgAtFeetMinor $ stashBlurb <+> itemsBlurb
      return True

pickLeaderWithPointer :: MonadClientUI m => ActorId -> m MError
pickLeaderWithPointer leader = do
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
  pUI <- getsSession spointer
  let p@(Point px py) = squareToMap $ uiToSquare pUI
  -- Pick even if no space in status line for the actor's symbol.
  if | py == rheight - 2 && px == 0 -> pointmanCycle leader True Forward
     | py == rheight - 2 ->
         case drop (px - 1) viewed of
           [] -> return Nothing
             -- relaxed, due to subtleties of display of selected actors
           (aid, b, _) : _ -> pick (aid, b)
     | otherwise ->
         case find (\(_, b, _) -> bpos b == p) oursUI of
           Nothing -> failMsg "not pointing at an actor"
           Just (aid, b, _) -> pick (aid, b)

itemOverlay :: MonadClientUI m
            => SingleItemSlots -> LevelId -> ItemBag -> Bool -> m OKX
itemOverlay lSlots lid bag displayRanged = do
  sccui <- getsSession sccui
  side <- getsClient sside
  discoBenefit <- getsClient sdiscoBenefit
  fontSetup <- getFontSetup
  okx <- getsState $ itemOverlayFromState lSlots lid bag displayRanged
                                          sccui side discoBenefit fontSetup
  return $! okx

itemOverlayFromState :: SingleItemSlots -> LevelId -> ItemBag -> Bool
                     -> CCUI -> FactionId -> DiscoveryBenefit -> FontSetup
                     -> State
                     -> OKX
itemOverlayFromState lSlots lid bag displayRanged
                     sccui side discoBenefit FontSetup{..} s =
  let CCUI{coscreen=ScreenContent{rwidth}} = sccui
      localTime = getLocalTime lid s
      itemToF = flip itemToFull s
      factionD = sfactionD s
      combGround = combinedGround side s
      combOrgan = combinedOrgan side s
      combEqp = combinedEqp side s
      stashBag = getFactionStashBag side s
      !_A = assert (allB (`elem` EM.elems lSlots) (EM.keys bag)
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
      pr :: (SlotChar, ItemId) -> Maybe (AttrString, AttrString, KeyOrSlot)
      pr (c, iid) =
        case EM.lookup iid bag of
          Nothing -> Nothing
          Just kit@(k, _) ->
            let itemFull = itemToF iid
                arItem = aspectRecordFull itemFull
                colorSymbol =
                  if IA.checkFlag Ability.Condition arItem
                  then viewItemBenefitColored discoBenefit iid itemFull
                  else viewItem itemFull
                phrase = makePhrase
                  [partItemWsRanged rwidth side factionD displayRanged
                                    DetailMedium 4 k localTime itemFull kit]
                !tLab = markEqp iid $ slotLabel c
                aLab = textToAS tLab ++ [colorSymbol]
                !tDesc = " " <> phrase
            in Just (aLab, textToAS tDesc, Right c)
      l = mapMaybe pr $ EM.assocs lSlots
  in labDescOKX squareFont propFont l

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
            indentation = if isSquareFont propFont then 52 else 26
            valueText = skillToDecorator skill b
                        $ Ability.getSk skill actorMaxSk
            triple = ( lab
                     , (labLen, textToAL skName)
                     , (indentation, textToAL valueText) )
            lenButton = 26 + T.length valueText
        in (triple, (Right c, ( PointUI 0 y
                              , ButtonWidth propFont lenButton )))
      (ts, kxs) = unzip $ zipWith prSlot (zip [0..] allSlots) skillSlots
      (skLab, skDescr, skValue) = unzip3 ts
      skillLab = EM.singleton squareFont $ offsetOverlay skLab
      skillDescr = EM.singleton propFont $ offsetOverlayX skDescr
      skillValue = EM.singleton monoFont $ offsetOverlayX skValue
  return (EM.unionsWith (++) [skillLab, skillDescr, skillValue], kxs)

-- | Extract whole-dungeon statistics for each place kind,
-- counting the number of occurrences of each type of `PlaceEntry`
-- for the given place kind and gathering the set of levels
-- on which any entry for that place kind can be found.
placesFromState :: ContentData PK.PlaceKind -> Bool -> State
                -> EM.EnumMap (ContentId PK.PlaceKind)
                              (ES.EnumSet LevelId, Int, Int, Int)
placesFromState coplace sexposePlaces s =
  let addEntries (!es1, !nEntries1, !nArounds1, !nExists1)
                 (!es2, !nEntries2, !nArounds2, !nExists2) =
        let !es = ES.union es1 es2
            !nEntries = nEntries1 + nEntries2
            !nArounds = nArounds1 + nArounds2
            !nExists = nExists1 + nExists2
        in (es, nEntries, nArounds, nExists)
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
             -- go through place entrances and depending on the place
             -- add an entry for it, whether Entry/Around/Exists,
             -- the effect being we're counting #s of each type
      insertZeros !em !pk _ = EM.insert pk (ES.empty, 0, 0, 0) em
      -- The initial places are overwritten except for those
      -- that have no entries in the dungeon at all,
      -- and in `sexposePlaces` debug mode these will be shown even though
      -- the stats will be zeros (which is a valuable warning!).
      initialPlaces | not sexposePlaces = EM.empty
                    | otherwise = ofoldlWithKey' coplace insertZeros EM.empty
  in EM.unionWith addEntries
       initialPlaces
       (EM.unionsWith addEntries $ map placesFromLevel $ EM.assocs $ sdungeon s)
        -- gather per-place-kind statistics for each level,
        -- then aggregate them over all levels, remembering that the place
        -- appeared on the given level (but not how man times)

placesOverlay :: MonadClientUI m => m OKX
placesOverlay = do
  COps{coplace} <- getsState scops
  soptions <- getsClient soptions
  FontSetup{..} <- getFontSetup
  places <- getsState $ placesFromState coplace (sexposePlaces soptions)
  let prSlot :: SlotChar
             -> (ContentId PK.PlaceKind, (ES.EnumSet LevelId, Int, Int, Int))
             -> (AttrString, AttrString, KeyOrSlot)
      prSlot c (pk, (es, _, _, _)) =
        let placeName = PK.pname $ okind coplace pk
            markPlace t = if ES.null es
                          then T.snoc (T.init t) '>'
                          else t
            !tLab = markPlace $ slotLabel c  -- ! to free @places@ as you go
            !tDesc = " "
                     <> placeName
                     <+> if ES.null es
                         then ""
                         else "("
                              <> makePhrase [MU.CarWs (ES.size es) "level"]
                              <> ")"
        in (textToAS tLab, textToAS tDesc, Right c)
      l = zipWith prSlot allSlots $ EM.assocs places
  return $! labDescOKX squareFont propFont l

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
  side <- getsClient sside
  total <- getsState $ snd . calculateTotal side
  dungeonTotal <- getsState sgold
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
      survivingHow = if | total == 0 -> "(barely)"
                        | total < dungeonTotal `div` 2 -> "(so far)"
                        | otherwise -> ""
      title = if addTitle
              then "\nYou are"
                   <+> survivingHow
                   <+> "surviving the '"
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
  return $! if isSquareFont propFont
            then EM.singleton squareFont  -- single column, single font
                 $ xtranslateOverlay 2 $ offsetOverlay
                 $ concatMap snd $ blurb ++ blurbEnd
            else EM.unionWith (++)
                 (EM.map (xtranslateOverlay 1)
                  $ attrLinesToFontMap blurb)
                 (EM.map (xtranslateOverlay $ rwidth + 1)
                  $ attrLinesToFontMap blurbEnd)

modesOverlay :: MonadClientUI m => m OKX
modesOverlay = do
  COps{comode} <- getsState scops
  FontSetup{..} <- getFontSetup
  svictories <- getsClient svictories
  nxtChal <- getsClient snxtChal  -- mark victories only for current difficulty
  let f !acc _p !i !a = (i, a) : acc
      campaignModes = ofoldlGroup' comode MK.CAMPAIGN_SCENARIO f []
      prSlot :: SlotChar
             -> (ContentId MK.ModeKind, MK.ModeKind)
             -> (AttrString, AttrString, KeyOrSlot)
      prSlot c (gameModeId, gameMode) =
        let modeName = MK.mname gameMode
            victories = case EM.lookup gameModeId svictories of
              Nothing -> 0
              Just cm -> fromMaybe 0 (M.lookup nxtChal cm)
            markMode t = if victories > 0
                         then T.snoc (T.init t) '>'
                         else t
            !tLab = markMode $ slotLabel c
            !tDesc = " " <> modeName
        in (textToAS tLab, textToAS tDesc, Right c)
      l = zipWith prSlot allSlots campaignModes
  return $! labDescOKX squareFont propFont l

pickNumber :: MonadClientUI m => Bool -> Int -> m (Either MError Int)
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
           -> LevelId          -- ^ level the position is at
           -> Maybe ActorId    -- ^ the actor that looks
           -> Maybe MU.Person  -- ^ grammatical person of the item(s), if any
           -> m (Text, Text, [(Int, MU.Part)])
lookAtTile canSee p lidV maid mperson = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  cops@COps{cotile, coplace} <- getsState scops
  side <- getsClient sside
  factionD <- getsState sfactionD
  mb <- getsState $ \s -> flip getActorBody s <$> maid
  lvl <- getLevel lidV
  saimMode <- getsSession saimMode
  embeds <- getsState $ getEmbedBag lidV p
  itemToF <- getsState $ flip itemToFull
  seps <- getsClient seps
  localTime <- getsState $ getLocalTime lidV
  getKind <- getsState $ flip getIidKind
  let inhabitants = posToAidsLvl p lvl
      detail = maybe DetailAll detailLevel saimMode
      aims = isJust $ maybe Nothing (\b -> makeLine False b p seps cops lvl) mb
      tkid = lvl `at` p
      tile = okind cotile tkid
      vis | TK.tname tile == "unknown space" = "that is"
          | not (null inhabitants)
            && (bpos <$> mb) /= Just p = "the terrain here is"
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
        in (k, nWs)
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
            -> LevelId  -- ^ level the position is at
            -> Maybe ActorId
                        -- ^ the actor that looks
            -> Maybe (MU.Part, Bool)
                        -- ^ pronoun for the big actor at the position, if any,
                        --   and whether the big actor is alive
            -> m (Text, Maybe MU.Person)
lookAtItems canSee p lidV maid mactorPronounAlive = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  side <- getsClient sside
  itemToF <- getsState $ flip itemToFull
  mb <- getsState $ \s -> flip getActorBody s <$> maid
  -- Not using @viewedLevelUI@, because @aid@ may be temporarily not a leader.
  saimMode <- getsSession saimMode
  let standingOn = Just p == (bpos <$> mb) && Just lidV == (blid <$> mb)
      -- In exploration mode the detail level depends on whether the actor
      -- that looks stand over the items, because then he can check details
      -- with inventory commands (or look in aiming mode).
      detailExploration = if standingOn && Just side == (bfid <$> mb)
                          then DetailMedium
                          else DetailAll
      detail = maybe detailExploration detailLevel saimMode
  localTime <- getsState $ getLocalTime lidV
  is <- getsState $ getFloorBag lidV p
  factionD <- getsState sfactionD
  globalTime <- getsState stime
  getKind <- getsState $ flip getIidKindId
  mLeader <- case maid of
    Just aid | standingOn -> do
      leaderPronoun <- partPronounLeader aid
      return $ Just (leaderPronoun, (bhp <$> mb) >= Just 0)
    _ -> return Nothing
  let mactorPronounAliveLeader = maybe mLeader Just mactorPronounAlive
  (subject, verb) <- case mactorPronounAliveLeader of
    Just (actorPronoun, actorAlive) ->
      return (actorPronoun, if actorAlive then "stand over" else "fall over")
    Nothing -> case maid of
      Just aid -> do
        subjectAid <- partActorLeader aid
        return (subjectAid, if canSee then "notice" else "remember")
      Nothing ->
        return ("one", if canSee then "can see" else "may remember")
  let nWs (iid, kit@(k, _)) =
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

lookAtStash :: MonadClientUI m => Point -> LevelId -> m Text
lookAtStash p lidV = do
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
               => Point -> LevelId -> m [(MsgClassShow, Text)]
lookAtPosition p lidV = do
  COps{cotile} <- getsState scops
  side <- getsClient sside
  per <- getPerFid lidV
  let canSee = ES.member p (totalVisible per)
  (actorsBlurb, mactorPronounAlive, actorsDesc) <- lookAtActors p lidV
  mleader <- getsClient sleader
  (itemsBlurb, mperson) <-
    lookAtItems canSee p lidV mleader mactorPronounAlive
  let tperson = if T.null itemsBlurb then Nothing else mperson
  (tileBlurb, placeBlurb, embedsList) <-
    lookAtTile canSee p lidV mleader tperson
  inhabitants <- getsState $ posToAidAssocs p lidV
  let actorMsgClass =
        if (bfid . snd <$> inhabitants) == [side]
        then MsgPromptGeneric  -- our single proj or non-proj; tame
        else MsgPromptActors
  stashBlurb <- lookAtStash p lidV
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
                DetailLow -> []  -- not to obscure aiming line
                DetailMedium ->
                  [(MsgPromptMention, case embedsList of
                    [] -> ""
                    [(k, _)] ->
                      ppEmbedName (1, if k == 1
                                      then "an embedded item"
                                      else "a stack of embedded items")
                    _ -> ppEmbedName (9, "some embedded items"))]
                _ -> let n = sum $ map fst embedsList
                         wWandW = MU.WWandW $ map snd embedsList
                     in [(MsgPromptMention, ppEmbedName (n, wWandW)) | n > 0]
           ++ [(MsgPromptModify, modifyBlurb) | detail == DetailAll]
  return $! if all (T.null . snd) ms && detail > DetailLow
            then [(MsgPromptFocus, tileBlurb)]
            else ms

displayItemLore :: MonadClientUI m
                => ItemBag -> Int -> (ItemId -> ItemFull -> Int -> Text) -> Int
                -> SingleItemSlots -> Bool
                -> m K.KM
displayItemLore itemBag meleeSkill promptFun slotIndex lSlots addTilde = do
  CCUI{coscreen=ScreenContent{rwidth, rheight}} <- getsSession sccui
  FontSetup{propFont} <- getFontSetup
  let lSlotsElems = EM.elems lSlots
      lSlotsBound = length lSlotsElems - 1
  let keys = [K.spaceKM, K.escKM]
             ++ [K.mkChar '~' | addTilde]
             ++ [K.upKM | slotIndex /= 0]
             ++ [K.downKM | slotIndex /= lSlotsBound]
  okx <- okxItemLorePointedAt
           propFont rwidth False itemBag meleeSkill promptFun slotIndex lSlots
  slides <- overlayToSlideshow (rheight - 2) keys okx
  km <- getConfirms ColorFull keys slides
  case K.key km of
    K.Up -> displayItemLore itemBag meleeSkill promptFun (slotIndex - 1)
                            lSlots addTilde
    K.Down -> displayItemLore itemBag meleeSkill promptFun (slotIndex + 1)
                              lSlots addTilde
    _ -> return km

okxItemLorePointedAt :: MonadClientUI m
                     => DisplayFont -> Int -> Bool -> ItemBag -> Int
                     -> (ItemId -> ItemFull -> Int -> Text)
                     -> Int -> SingleItemSlots
                     -> m OKX
okxItemLorePointedAt descFont width inlineMsg itemBag meleeSkill promptFun
                     slotIndex lSlots = do
  FontSetup{squareFont} <- getFontSetup
  side <- getsClient sside
  arena <- getArenaUI
  let lSlotsElems = EM.elems lSlots
      iid2 = lSlotsElems !! slotIndex
      kit2@(k, _) = itemBag EM.! iid2
  itemFull2 <- getsState $ itemToFull iid2
  localTime <- getsState $ getLocalTime arena
  factionD <- getsState sfactionD
  -- The hacky level 0 marks items never seen, but sent by server at gameover.
  jlid <- getsSession $ fromMaybe (toEnum 0) <$> EM.lookup iid2 . sitemUI
  let descAs = itemDesc width True side factionD meleeSkill
                        CGround localTime jlid itemFull2 kit2
      (ovLab, ovDesc) = labDescOverlay squareFont width descAs
      prompt = promptFun iid2 itemFull2 k
      promptBlurb | T.null prompt = []
                  | otherwise = offsetOverlay $ splitAttrString width width
                                $ textFgToAS Color.Brown $ prompt <> "\n\n"
  (descSym2, descBlurb2) <-
    if inlineMsg
    then do
      let len = length promptBlurb
      return ( ytranslateOverlay len ovLab
             , promptBlurb ++ ytranslateOverlay len ovDesc )
    else do
      msgAdd MsgPromptGeneric prompt
      return (ovLab, ovDesc)
  let ov = EM.insertWith (++) squareFont descSym2
           $ EM.singleton descFont descBlurb2
  return (ov, [])

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
