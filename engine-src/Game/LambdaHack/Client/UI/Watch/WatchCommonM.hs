{-# LANGUAGE TupleSections #-}
-- | Display atomic commands received by the client.
module Game.LambdaHack.Client.UI.Watch.WatchCommonM
  ( pushFrame, fadeOutOrIn
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , ppHearMsg, ppHearDistanceAdjective, ppHearDistanceAdverb
  , updateItemSlot, markDisplayNeeded, lookAtMove
  , aidVerbMU, aidVerbDuplicateMU, itemVerbMUGeneral, itemVerbMU
  , itemVerbMUShort, itemAidVerbMU, mitemAidVerbMU, itemAidDistinctMU
  , manyItemsAidVerbMU
  , createActorUI, destroyActorUI, spotItemBag, moveActor, displaceActorUI
  , moveItemUI, quitFactionUI
  , displayGameOverLoot, displayGameOverAnalytics, viewLoreItems
  , discover, ppSfxMsg, strike
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Concurrent (threadDelay)
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import           Data.Int (Int64)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Tuple
import           GHC.Exts (inline)
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Atomic
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.ActorUI
import           Game.LambdaHack.Client.UI.Animation
import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Client.UI.DrawM
import           Game.LambdaHack.Client.UI.EffectDescription
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.FrameM
import           Game.LambdaHack.Client.UI.HandleHelperM
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import           Game.LambdaHack.Client.UI.ItemDescription
import           Game.LambdaHack.Client.UI.ItemSlot
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.MsgM
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.Slideshow
import           Game.LambdaHack.Client.UI.SlideshowM
import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Analytics
import           Game.LambdaHack.Common.ClientOptions
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.ReqFailure
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Content.CaveKind (cdesc)
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Content.ModeKind as MK
import           Game.LambdaHack.Content.RuleKind
import qualified Game.LambdaHack.Content.TileKind as TK
import qualified Game.LambdaHack.Core.Dice as Dice
import           Game.LambdaHack.Core.Frequency
import           Game.LambdaHack.Core.Random
import qualified Game.LambdaHack.Definition.Ability as Ability
import qualified Game.LambdaHack.Definition.Color as Color
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.Flavour

updateItemSlot :: MonadClientUI m => Container -> ItemId -> m ()
updateItemSlot c iid = do
  arItem <- getsState $ aspectRecordFromIid iid
  ItemSlots itemSlots <- getsSession sslots
  let slore = IA.loreFromContainer arItem c
      lSlots = itemSlots EM.! slore
  case lookup iid $ map swap $ EM.assocs lSlots of
    Nothing -> do
      let l = assignSlot lSlots
          f = EM.insert l iid
          newSlots = ItemSlots $ EM.adjust f slore itemSlots
      modifySession $ \sess -> sess {sslots = newSlots}
    Just _l -> return ()  -- slot already assigned

markDisplayNeeded :: MonadClientUI m => LevelId -> m ()
markDisplayNeeded lid = do
  lidV <- viewedLevelUI
  when (lidV == lid) $ modifySession $ \sess -> sess {sdisplayNeeded = True}

lookAtMove :: MonadClientUI m => ActorId -> m ()
lookAtMove aid = do
  mleader <- getsClient sleader
  body <- getsState $ getActorBody aid
  side <- getsClient sside
  aimMode <- getsSession saimMode
  when (not (bproj body)
        && bfid body == side
        && isNothing aimMode) $ do  -- aiming does a more extensive look
    stashBlurb <- lookAtStash (blid body) (bpos body)
    (itemsBlurb, _) <- lookAtItems True (bpos body) aid Nothing
    let msgClass = if Just aid == mleader
                   then MsgAtFeetMajor
                   else MsgAtFeetMinor
        blurb = stashBlurb <+> itemsBlurb
    unless (T.null blurb) $
      msgAdd msgClass blurb
  fact <- getsState $ (EM.! bfid body) . sfactionD
  adjBigAssocs <- getsState $ adjacentBigAssocs body
  adjProjAssocs <- getsState $ adjacentProjAssocs body
  if not (bproj body) && bfid body == side then do
    let foe (_, b2) = isFoe (bfid body) fact (bfid b2)
        adjFoes = filter foe $ adjBigAssocs ++ adjProjAssocs
    unless (null adjFoes) stopPlayBack
  else when (isFoe (bfid body) fact side) $ do
    let our (_, b2) = bfid b2 == side
        adjOur = filter our adjBigAssocs
    unless (null adjOur) stopPlayBack

aidVerbMU :: (MonadClientUI m, MsgShared a) => a -> ActorId -> MU.Part -> m ()
aidVerbMU msgClass aid verb = do
  subject <- partActorLeader aid
  msgAdd msgClass $ makeSentence [MU.SubjectVerbSg subject verb]

aidVerbDuplicateMU :: (MonadClientUI m, MsgShared a)
                   => a -> ActorId -> MU.Part -> m Bool
aidVerbDuplicateMU msgClass aid verb = do
  subject <- partActorLeader aid
  msgAddDuplicate msgClass (makeSentence [MU.SubjectVerbSg subject verb])

itemVerbMUGeneral :: MonadClientUI m
                  => Bool -> ItemId -> ItemQuant -> MU.Part -> Container
                  -> m Text
itemVerbMUGeneral verbose iid kit@(k, _) verb c = assert (k > 0) $ do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  lid <- getsState $ lidFromC c
  localTime <- getsState $ getLocalTime lid
  itemFull <- getsState $ itemToFull iid
  side <- getsClient sside
  factionD <- getsState sfactionD
  let arItem = aspectRecordFull itemFull
      partItemWsChosen | verbose = partItemWs
                       | otherwise = partItemWsShort
      subject = partItemWsChosen rwidth side factionD k localTime itemFull kit
      msg | k > 1 && not (IA.checkFlag Ability.Condition arItem) =
              makeSentence [MU.SubjectVerb MU.PlEtc MU.Yes subject verb]
          | otherwise = makeSentence [MU.SubjectVerbSg subject verb]
  return $! msg

itemVerbMU :: (MonadClientUI m, MsgShared a)
           => a -> ItemId -> ItemQuant -> MU.Part -> Container -> m ()
itemVerbMU msgClass iid kit verb c = do
  msg <- itemVerbMUGeneral True iid kit verb c
  msgAdd msgClass msg

itemVerbMUShort :: (MonadClientUI m, MsgShared a)
                => a -> ItemId -> ItemQuant -> MU.Part -> Container
                -> m ()
itemVerbMUShort msgClass iid kit verb c = do
  msg <- itemVerbMUGeneral False iid kit verb c
  msgAdd msgClass msg

itemAidVerbMU :: (MonadClientUI m, MsgShared a)
              => a -> ActorId -> MU.Part -> ItemId -> Either Int Int
              -> m ()
itemAidVerbMU msgClass aid verb iid ek = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  body <- getsState $ getActorBody aid
  side <- getsClient sside
  factionD <- getsState sfactionD
  let lid = blid body
      fakeKit = quantSingle
  localTime <- getsState $ getLocalTime lid
  subject <- partActorLeader aid
  -- The item may no longer be in @c@, but it was.
  itemFull <- getsState $ itemToFull iid
  let object = case ek of
        Left n ->
          partItemWs rwidth side factionD n localTime itemFull fakeKit
        Right n ->
          let (name1, powers) =
                partItemShort rwidth side factionD localTime itemFull fakeKit
          in MU.Phrase ["the", MU.Car1Ws n name1, powers]
      msg = makeSentence [MU.SubjectVerbSg subject verb, object]
  msgAdd msgClass msg

mitemAidVerbMU :: (MonadClientUI m, MsgShared a)
               => a -> ActorId -> MU.Part -> ItemId -> Maybe MU.Part
               -> m ()
mitemAidVerbMU msgClass aid verb iid msuffix = do
  itemD <- getsState sitemD
  case msuffix of
    Just suffix | iid `EM.member` itemD ->
      itemAidVerbMU msgClass aid (MU.Phrase [verb, suffix]) iid (Right 1)
    _ -> do
#ifdef WITH_EXPENSIVE_ASSERTIONS
      side <- getsClient sside
      b <- getsState $ getActorBody aid
      bUI <- getsSession $ getActorUI aid
      -- It's not actually expensive, but it's particularly likely
      -- to fail with wild content, indicating server game rules logic
      -- needs to be fixed/extended.
      -- Observer from another faction may receive the effect information
      -- from the server, because the affected actor is visible,
      -- but the position of the item may be out of FOV. This is fine;
      -- the message is then shorter, because only the effect was seen,
      -- while the cause remains misterious.
      assert (isNothing msuffix  -- item description not requested
              || bfid b /= side  -- not from affected faction; only observing
              `blame` "item never seen by the affected actor"
              `swith` (aid, b, bUI, verb, iid, msuffix)) $
#endif
        aidVerbMU msgClass aid verb

itemAidDistinctMU :: MonadClientUI m
                  => MsgClassDistinct -> ActorId -> MU.Part -> MU.Part -> ItemId
                  -> m ()
itemAidDistinctMU msgClass aid verbShow verbSave iid = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  body <- getsState $ getActorBody aid
  side <- getsClient sside
  factionD <- getsState sfactionD
  let lid = blid body
      fakeKit = quantSingle
  localTime <- getsState $ getLocalTime lid
  subject <- partActorLeader aid
  -- The item may no longer be in @c@, but it was.
  itemFull <- getsState $ itemToFull iid
  let object = let (name, powers) =
                     partItem rwidth side factionD localTime itemFull fakeKit
               in MU.Phrase [name, powers]
      t1 = makeSentence [MU.SubjectVerbSg subject verbShow, object]
      t2 = makeSentence [MU.SubjectVerbSg subject verbSave, object]
      dotsIfShorter = if t1 == t2 then "" else ".."
  msgAddDistinct msgClass (t1 <> dotsIfShorter, t2)

manyItemsAidVerbMU :: (MonadClientUI m, MsgShared a)
                   => a -> ActorId -> MU.Part
                   -> [(ItemId, ItemQuant)] -> (Int -> Either (Maybe Int) Int)
                   -> m ()
manyItemsAidVerbMU msgClass aid verb sortedAssocs ekf = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  body <- getsState $ getActorBody aid
  side <- getsClient sside
  factionD <- getsState sfactionD
  let lid = blid body
      fakeKit = quantSingle
  localTime <- getsState $ getLocalTime lid
  subject <- partActorLeader aid
  -- The item may no longer be in @c@, but it was.
  itemToF <- getsState $ flip itemToFull
  let object (iid, (k, _)) =
        let itemFull = itemToF iid
        in case ekf k of
          Left (Just n) ->
            partItemWs rwidth side factionD n localTime itemFull fakeKit
          Left Nothing ->
            let (name, powers) =
                  partItem rwidth side factionD localTime itemFull fakeKit
            in MU.Phrase [name, powers]
          Right n ->
            let (name1, powers) =
                  partItemShort rwidth side factionD localTime itemFull fakeKit
            in MU.Phrase ["the", MU.Car1Ws n name1, powers]
      msg = makeSentence [ MU.SubjectVerbSg subject verb
                         , MU.WWandW $ map object sortedAssocs]
  msgAdd msgClass msg

data Threat =
    ThreatNone
  | ThreatUnarmed
  | ThreatArmed
  | ThreatAnotherUnarmed
  | ThreatAnotherArmed
  deriving Eq

createActorUI :: MonadClientUI m => Bool -> ActorId -> Actor -> m ()
createActorUI born aid body = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  side <- getsClient sside
  factionD <- getsState sfactionD
  let fact = factionD EM.! bfid body
  localTime <- getsState $ getLocalTime $ blid body
  itemFull@ItemFull{itemBase, itemKind} <- getsState $ itemToFull (btrunk body)
  actorUI <- getsSession sactorUI
  let arItem = aspectRecordFull itemFull
  unless (aid `EM.member` actorUI) $ do
    UIOptions{uHeroNames} <- getsSession sUIOptions
    let baseColor = flavourToColor $ jflavour itemBase
        basePronoun | not (bproj body)
                      && IK.isymbol itemKind == '@'
                      && fhasGender (gplayer fact) = "he"
                    | otherwise = "it"
        nameFromNumber fn k = if k == 0
                              then makePhrase [MU.Ws $ MU.Text fn, "Captain"]
                              else fn <+> tshow k
        heroNamePronoun k =
          if gcolor fact /= Color.BrWhite
          then (nameFromNumber (fname $ gplayer fact) k, "he")
          else fromMaybe (nameFromNumber (fname $ gplayer fact) k, "he")
               $ lookup k uHeroNames
        (n, bsymbol) =
          if | bproj body -> (0, if IA.checkFlag Ability.Blast arItem
                                 then IK.isymbol itemKind
                                 else '*')
             | baseColor /= Color.BrWhite -> (0, IK.isymbol itemKind)
             | otherwise -> case bnumber body of
                 Nothing ->
                   error $ "numbered actor without server-assigned number"
                           `showFailure` (aid, body)
                 Just bn -> (bn, if 0 < bn && bn < 10
                                 then Char.intToDigit bn
                                 else '@')
        (object1, object2) =
          partItemShortest rwidth (bfid body) factionD localTime
                           itemFull quantSingle
        (bname, bpronoun) =
          if | bproj body ->
               let adj = case btrajectory body of
                     Just (tra, _) | length tra < 5 -> "falling"
                     _ -> "flying"
               in (makePhrase [adj, object1, object2], basePronoun)
             | baseColor /= Color.BrWhite ->
               (makePhrase [object1, object2], basePronoun)
             | otherwise -> heroNamePronoun n
        bcolor | bproj body = if IA.checkFlag Ability.Blast arItem
                              then baseColor
                              else Color.BrWhite
               | baseColor == Color.BrWhite = gcolor fact
               | otherwise = baseColor
        bUI = ActorUI{..}
    modifySession $ \sess ->
      sess {sactorUI = EM.insert aid bUI actorUI}
  mapM_ (\(iid, store) -> do
           let c = if not (bproj body) && iid == btrunk body
                   then CTrunk (bfid body) (blid body) (bpos body)
                   else CActor aid store
           updateItemSlot c iid
           recordItemLid iid c)
        ((btrunk body, CEqp)  -- store will be overwritten, unless projectile
         : filter ((/= btrunk body) . fst) (getCarriedIidCStore body))
  if | bproj body -> do
       when (bfid body /= side)
         stopPlayBack
       pushFrame False  -- make sure first (seen (again)) position displayed
     | bfid body == side -> do
       let upd = ES.insert aid
       modifySession $ \sess -> sess {sselected = upd $ sselected sess}
       unless (EM.null actorUI) $ do  -- don't announce the very first party member
         when born $ do
           let verb = "join you"
           aidVerbMU MsgSpottedActor aid verb
           msgAdd MsgTutorialHint "You survive this mission, or die trying, as a team. After a few moves, feel free to switch the controlled teammate (marked on the map with the yellow box) using the Tab key to another party member (marked with a green box)."  -- assuming newbies don't remap their keys
           animate (blid body) $ actorX (bpos body)
     | otherwise -> do
       -- Don't spam if the actor was already visible
       -- (but, e.g., on a tile that is invisible this turn
       -- (in that case move is broken down to lose+spot)
       -- or on a distant tile, via teleport while the observer
       -- teleported, too).
       lastLost <- getsSession slastLost
       if ES.member aid lastLost
       then markDisplayNeeded (blid body)
       else do
         stopPlayBack
         let verb = if born then "appear suddenly" else "be spotted"
         threat <-
           if isFoe (bfid body) fact side then do
             -- Aim even if nobody can shoot at the enemy.
             -- Let's home in on him and then we can aim or melee.
             -- We set permit to False, because it's technically
             -- very hard to check aimability here, because we are
             -- in-between turns and, e.g., leader's move has not yet
             -- been taken into account.
             xhair <- getsSession sxhair
             case xhair of
               Just (TVector _) -> return ()  -- explicitly set; keep it
               _ -> modifySession $ \sess ->
                      sess { sxhair = Just $ TEnemy aid
                           , sitemSel = Nothing } -- reset flinging totally
             foes <- getsState $ foeRegularList side (blid body)
             itemsSize <- getsState $ guardItemSize body
             if length foes <= 1 then
               if itemsSize == 0 then do
                 msgAdd MsgSpottedThreat "You are not alone!"
                 return ThreatUnarmed
               else do
                 msgAdd MsgSpottedThreat "Armed intrusion ahead!"
                 return ThreatArmed
             else
               if itemsSize == 0 then
                 return ThreatAnotherUnarmed
               else do
                 msgAdd MsgSpottedThreat "Another threat, armed!"
                 return ThreatAnotherArmed
           else return ThreatNone  -- member of neutral faction
         aidVerbMU MsgSpottedActor aid verb
         friendAssocs <- getsState $ friendRegularAssocs side (blid body)
         case threat of
           ThreatNone -> return ()  -- too rare to care ATM
           ThreatUnarmed ->
             msgAdd MsgTutorialHint "Enemies are normally dealt with using melee (by bumping when adjacent) or ranged combat (by 'f'linging items at them)."  -- assuming newbies don't remap their keys
           ThreatArmed ->
             msgAdd MsgTutorialHint "Enemies can be dealt with not only via combat, but also with clever use of terrain effects, stealth (not emitting nor reflecting light) or hasty retreat (particularly when foes are asleep or drowsy)."
           _ | length friendAssocs <= 1 -> return ()  -- one member on level
           ThreatAnotherUnarmed ->
             msgAdd MsgTutorialHint "When dealing with groups of enemies, remember than you fight as a team. After a few moves, switch the controlled teammate (marked on the map with the yellow box) using the Tab key to another party member (marked with a green box). Avoid meleeing alone."
           ThreatAnotherArmed ->
             msgAdd MsgTutorialHint "When dealing with groups of armed enemies, remember than you fight as a team. After a few moves, switch the controlled teammate (marked on the map with the yellow box) using the Tab key to another party member (marked with a green box). Retreat, if necessary to form a front line. Soften the foes with missiles, especially of exploding kind."
         animate (blid body) $ actorX (bpos body)

destroyActorUI :: MonadClientUI m => Bool -> ActorId -> Actor -> m ()
destroyActorUI destroy aid b = do
  trunk <- getsState $ getItemBody $ btrunk b
  let baseColor = flavourToColor $ jflavour trunk
  unless (baseColor == Color.BrWhite) $  -- keep setup for heroes, etc.
    modifySession $ \sess -> sess {sactorUI = EM.delete aid $ sactorUI sess}
  let dummyTarget = TPoint TKnown (blid b) (bpos b)
      affect tgt = case tgt of
        Just (TEnemy a) | a == aid -> Just $
          if destroy then
            -- If *really* nothing more interesting, the actor will
            -- go to last known location to perhaps find other foes.
            dummyTarget
          else
            -- If enemy only hides (or we stepped behind obstacle) find him.
            TPoint (TEnemyPos a) (blid b) (bpos b)
        Just (TNonEnemy a) | a == aid -> Just dummyTarget
        _ -> tgt
  modifySession $ \sess -> sess {sxhair = affect $ sxhair sess}
  unless (bproj b || destroy) $
    modifySession $ \sess -> sess {slastLost = ES.insert aid $ slastLost sess}
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  let gameOver = isJust $ gquit fact  -- we are the UI faction, so we determine
  unless gameOver $ do
    when (bfid b == side && not (bproj b)) $ do
      stopPlayBack
      let upd = ES.delete aid
      modifySession $ \sess -> sess {sselected = upd $ sselected sess}
      when destroy $ do
        mleader <- getsClient sleader
        when (isJust mleader)
          -- This is especially handy when the dead actor was a leader
          -- on a different level than the new one:
          clearAimMode
    -- If pushed, animate spotting again, to draw attention to pushing.
    markDisplayNeeded (blid b)

spotItemBag :: forall m. MonadClientUI m
            => Bool -> Container -> ItemBag -> m ()
spotItemBag verbose c bag = do
  -- This is due to a move, or similar, which will be displayed,
  -- so no extra @markDisplayNeeded@ needed here and in similar places.
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  side <- getsClient sside
  getKind <- getsState $ flip getIidKindId
  lid <- getsState $ lidFromC c
  localTime <- getsState $ getLocalTime lid
  factionD <- getsState sfactionD
  -- Queried just once, so many copies of a new item can be reported. OK.
  ItemSlots itemSlots <- getsSession sslots
  sxhairOld <- getsSession sxhair
  let resetXhair = case c of
        CFloor _ p -> case sxhairOld of
          Just TEnemy{} -> return ()  -- probably too important to overwrite
          Just (TPoint TEnemyPos{} _ _) -> return ()
          Just (TPoint TStash{} _ _) -> return ()
          Just (TVector _) -> return ()  -- explicitly set; keep it
          _ -> do
            -- Don't steal xhair if it's only an item on another level.
            -- For enemies, OTOH, capture xhair to alarm player.
            lidV <- viewedLevelUI
            when (lid == lidV) $ do
              bagFloor <- getsState $ getFloorBag lid p
              modifySession $ \sess ->
                sess { sxhair = Just $ TPoint (TItem bagFloor) lidV p
                     , sitemSel = Nothing }  -- reset flinging totally
        _ -> return ()
      locatedWhere = ppContainer factionD c
      beLocated = MU.Text $
        "be located" <+> if locatedWhere == ppContainer EM.empty c
                         then ""  -- boring
                         else locatedWhere
      subjectMaybe :: (ItemId, ItemQuant) -> m (Maybe (Int, MU.Part, MU.Part))
      subjectMaybe (iid, kit@(k, _)) = do
        recordItemLid iid c
        itemFull <- getsState $ itemToFull iid
        let arItem = aspectRecordFull itemFull
            slore = IA.loreFromContainer arItem c
        case lookup iid $ map swap $ EM.assocs $ itemSlots EM.! slore of
          Nothing -> do  -- never seen or would have a slot
            updateItemSlot c iid
            case c of
              CFloor{} -> do
                let subjectShort = partItemWsShortest rwidth side factionD k
                                                      localTime itemFull kit
                    subjectLong = partItemWsLong rwidth side factionD k
                                                 localTime itemFull kit
                return $ Just (k, subjectShort, subjectLong)
              _ -> return Nothing
          _ -> return Nothing  -- this item or another with the same @iid@
                               -- seen already (has a slot assigned); old news
      -- @SortOn@ less efficient here, because function cheap.
      sortItems = sortOn (getKind . fst)
      sortedAssocs = sortItems $ EM.assocs bag
  subjectMaybes <- mapM subjectMaybe sortedAssocs
  let subjects = catMaybes subjectMaybes
      sendMsg plural = do
        let subjectShort = MU.WWandW $ map (\(_, part, _) -> part) subjects
            subjectLong = MU.WWandW $ map (\(_, _, part) -> part) subjects
            msg subject =
              if plural
              then makeSentence [MU.SubjectVerb MU.PlEtc MU.Yes
                                                subject beLocated]
              else makeSentence [MU.SubjectVerbSg subject beLocated]
            msgShort = msg subjectShort
            msgLong = msg subjectLong
            dotsIfShorter = if msgShort == msgLong then "" else ".."
        resetXhair
        msgAddDistinct MsgSpottedItem (msgShort <> dotsIfShorter, msgLong)
  case subjects of
    [] -> return ()
    [(1, _, _)] -> sendMsg False
    _ -> sendMsg True
  when verbose $ case c of
    CActor aid store -> do
      let verb = MU.Text $ verbCStore store
      b <- getsState $ getActorBody aid
      fact <- getsState $ (EM.! bfid b) . sfactionD
      let underAI = isAIFact fact
      mleader <- getsClient sleader
      if Just aid == mleader && not underAI then
        manyItemsAidVerbMU MsgItemMovement aid verb sortedAssocs Right
      else when (not (bproj b) && bhp b > 0) $  -- don't announce death drops
        manyItemsAidVerbMU MsgItemMovement aid verb sortedAssocs (Left . Just)
    _ -> return ()

recordItemLid :: MonadClientUI m => ItemId -> Container -> m ()
recordItemLid iid c = do
  mjlid <- getsSession $ EM.lookup iid . sitemUI
  when (isNothing mjlid) $ do
    lid <- getsState $ lidFromC c
    modifySession $ \sess ->
      sess {sitemUI = EM.insert iid lid $ sitemUI sess}

moveActor :: MonadClientUI m => ActorId -> Point -> Point -> m ()
moveActor aid source target = do
  -- If source and target tile distant, assume it's a teleportation
  -- and display an animation. Note: jumps and pushes go through all
  -- intervening tiles, so won't be considered. Note: if source or target
  -- not seen, the (half of the) animation would be boring, just a delay,
  -- not really showing a transition, so we skip it (via 'breakUpdAtomic').
  -- The message about teleportation is sometimes shown anyway, just as the X.
  body <- getsState $ getActorBody aid
  if adjacent source target
  then markDisplayNeeded (blid body)
  else do
    let ps = (source, target)
    animate (blid body) $ teleport ps
  lookAtMove aid

displaceActorUI :: MonadClientUI m => ActorId -> ActorId -> m ()
displaceActorUI source target = do
  mleader <- getsClient sleader
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  spart <- partActorLeader source
  tpart <- partActorLeader target
  let msgClass = if mleader `elem` map Just [source, target]
                 then MsgActionMajor  -- to interrupt run after a displace;
                 else MsgActionMinor  -- configurable
      msg = makeSentence [MU.SubjectVerbSg spart "displace", tpart]
  msgAdd msgClass msg
  lookAtMove source
  when (bfid sb /= bfid tb) $
    lookAtMove target  -- in case only this one is ours
  side <- getsClient sside
  -- Ours involved, but definitely not requested by player via UI.
  when (side `elem` [bfid sb, bfid tb] && mleader /= Just source) stopPlayBack
  let ps = (bpos tb, bpos sb)
  animate (blid sb) $ swapPlaces ps

-- @UpdMoveItem@ is relatively rare (except within the player's faction),
-- but it ensures that even if only one of the stores is visible
-- (e.g., stash floor is not or actor posision is not), some messages
-- will be printed (via verbose @UpdLoseItem@).
moveItemUI :: MonadClientUI m
           => ItemId -> Int -> ActorId -> CStore -> CStore
           -> m ()
moveItemUI iid k aid cstore1 cstore2 = do
  let verb = MU.Text $ verbCStore cstore2
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  let underAI = isAIFact fact
  mleader <- getsClient sleader
  ItemSlots itemSlots <- getsSession sslots
  case lookup iid $ map swap $ EM.assocs $ itemSlots EM.! SItem of
    Just _l ->
      -- So far organs can't be put into stash, so no need to call
      -- @updateItemSlot@ to add or reassign lore category.
      if cstore1 == CGround && Just aid == mleader && not underAI then
        itemAidVerbMU MsgItemMovement aid verb iid (Right k)
      else when (not (bproj b) && bhp b > 0) $  -- don't announce death drops
        itemAidVerbMU MsgItemMovement aid verb iid (Left k)
    Nothing -> error $
      "" `showFailure` (iid, k, aid, cstore1, cstore2)

-- The item may be used up already and so not present in the container,
-- e.g., if the item destroyed itself. This is OK. Message is still needed.
discover :: MonadClientUI m => Container -> ItemId -> m ()
discover c iid = do
  COps{coitem} <- getsState scops
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  lid <- getsState $ lidFromC c
  globalTime <- getsState stime
  localTime <- getsState $ getLocalTime lid
  itemFull <- getsState $ itemToFull iid
  bag <- getsState $ getContainerBag c
  side <- getsClient sside
  factionD <- getsState sfactionD
  (noMsg, nameWhere) <- case c of
    CActor aidOwner storeOwner -> do
      bOwner <- getsState $ getActorBody aidOwner
      name <- if bproj bOwner
              then return []
              else ppContainerWownW partActorLeader True c
      let arItem = aspectRecordFull itemFull
          inMetaGame = IA.checkFlag Ability.MetaGame arItem
          isOurOrgan = bfid bOwner == side
                       && storeOwner == COrgan
                       && not inMetaGame
            -- assume own faction organs known intuitively,
            -- except backstories and other meta game items
      return (isOurOrgan, name)
    CTrunk _ _ p | p == originPoint -> return (True, [])
      -- the special reveal at game over, using fake @CTrunk@; don't spam
    _ -> return (False, [])
  let kit = EM.findWithDefault quantSingle iid bag
              -- may be used up by that time
      knownName = makePhrase
        [partItemMediumAW rwidth side factionD localTime itemFull kit]
      flav = flavourToName $ jflavour $ itemBase itemFull
      (object1, object2) =
        partItemShortest rwidth side factionD localTime itemFull kit
      name1 = makePhrase [object1, object2]
      -- Make sure the two names in the message differ.
      (ikObvious, itemKind) = case jkind $ itemBase itemFull of
        IdentityObvious ik -> (True, ik)
        IdentityCovered _ix ik -> (False, ik)
          -- fake kind (template); OK, we talk about appearances
      name2 = IK.iname $ okind coitem itemKind
      name = if ikObvious && T.unwords (tail (T.words knownName)) /= name1
             then name1  -- avoid "a pair turns out to be"
             else name2  -- avoid "chip of scientific explanation"
      unknownName = MU.Phrase $ [MU.Text flav, MU.Text name] ++ nameWhere
      msg = makeSentence
        [ "the"
        , MU.SubjectVerbSg unknownName "turn out to be"
        , MU.Text knownName ]
  unless (noMsg || globalTime == timeZero) $  -- no spam about initial equipment
    msgAdd MsgItemDiscovery msg

ppHearMsg :: MonadClientUI m => Maybe Int -> HearMsg -> m Text
ppHearMsg distance hearMsg = case hearMsg of
  HearUpd cmd -> do
    COps{coTileSpeedup} <- getsState scops
    let sound = case cmd of
          UpdDestroyActor{} -> "shriek"
          UpdCreateItem{} -> "clatter"
          UpdTrajectory{} -> "thud"  -- A non-blast projectle hits a tile.
          UpdAlterTile _ _ fromTile toTile ->
            if | Tile.isOpenable coTileSpeedup fromTile
                 && Tile.isClosable coTileSpeedup toTile
                 || Tile.isClosable coTileSpeedup fromTile
                    && Tile.isOpenable coTileSpeedup toTile -> "creaking sound"
               | Tile.isWalkable coTileSpeedup fromTile
                 && Tile.isWalkable coTileSpeedup toTile -> "splash"
               | otherwise -> "rumble"
          UpdAlterExplorable _ k ->
            if k > 0 then "grinding noise" else "fizzing noise"
          _ -> error $ "" `showFailure` cmd
        adjective = MU.Text $ ppHearDistanceAdjective distance
        msg = makeSentence ["you hear", MU.AW $ MU.Phrase [adjective, sound]]
    return $! msg
  HearStrike ik -> do
    COps{coitem} <- getsState scops
    let verb = IK.iverbHit $ okind coitem ik
        adverb = MU.Text $ ppHearDistanceAdverb distance
        msg = makeSentence [ "you", adverb, "hear something"
                           , MU.Text verb, "someone" ]
    return $! msg
  HearSummon isProj grp p -> do
    let verb = if isProj then "something lure" else "somebody summon"
        part = MU.Text $ displayGroupName grp
        object = if p == 1  -- works, because exact number sent, not dice
                 then MU.AW part
                 else MU.Ws part
        adverb = MU.Text $ ppHearDistanceAdverb distance
    return $! makeSentence ["you", adverb, "hear", verb, object]
  HearCollideTile -> do
    let adverb = MU.Text $ ppHearDistanceAdverb distance
    return $! makeSentence ["you", adverb, "hear someone crash into something"]
  HearTaunt t -> do
    let adverb = MU.Text $ ppHearDistanceAdverb distance
    return $! makePhrase ["You", adverb, "overhear", MU.Text t]

ppHearDistanceAdjective :: Maybe Int -> Text
ppHearDistanceAdjective Nothing = "indistinct"
ppHearDistanceAdjective (Just 0) = "very close"
ppHearDistanceAdjective (Just 1) = "close"
ppHearDistanceAdjective (Just 2) = ""
ppHearDistanceAdjective (Just 3) = "remote"
ppHearDistanceAdjective (Just 4) = "distant"
ppHearDistanceAdjective (Just _) = "far-off"

ppHearDistanceAdverb :: Maybe Int -> Text
ppHearDistanceAdverb Nothing = "indistinctly"
ppHearDistanceAdverb (Just 0) = "very clearly"
ppHearDistanceAdverb (Just 1) = "clearly"
ppHearDistanceAdverb (Just 2) = ""
ppHearDistanceAdverb (Just 3) = "remotely"
ppHearDistanceAdverb (Just 4) = "distantly"
ppHearDistanceAdverb (Just _) = "barely"

-- | Push the frame depicting the current level to the frame queue.
-- Only one line of the report is shown, as in animations,
-- because it may not be our turn, so we can't clear the message
-- to see what is underneath.
pushFrame :: MonadClientUI m => Bool -> m ()
pushFrame delay = do
  -- The delay before reaction to keypress was too long in case of many
  -- projectiles flying and ending flight, so frames need to be skipped.
  keyPressed <- anyKeyPressed
  unless keyPressed $ do
    lidV <- viewedLevelUI
    FontSetup{propFont} <- getFontSetup
    frame <- basicFrameWithoutReport lidV propFont
    -- Pad with delay before and after to let player see, e.g., door being
    -- opened a few ticks after it came into vision, the same turn.
    displayFrames lidV $
      if delay then [Nothing, Just frame, Nothing] else [Just frame]

fadeOutOrIn :: MonadClientUI m => Bool -> m ()
fadeOutOrIn out = do
  arena <- getArenaUI
  CCUI{coscreen} <- getsSession sccui
  animMap <- rndToActionUI $ fadeout coscreen out 2
  animFrs <- renderAnimFrames True arena animMap
  displayFrames arena (tail animFrs)  -- no basic frame between fadeout and in
