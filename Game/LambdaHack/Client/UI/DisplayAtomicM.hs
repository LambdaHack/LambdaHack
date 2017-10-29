-- | Display atomic commands received by the client.
module Game.LambdaHack.Client.UI.DisplayAtomicM
  ( displayRespUpdAtomicUI, displayRespSfxAtomicUI
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M
import Data.Tuple
import GHC.Exts (inline)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.CommonM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.ActorUI
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.FrameM
import Game.LambdaHack.Client.UI.HandleHelperM
import Game.LambdaHack.Client.UI.ItemDescription
import Game.LambdaHack.Client.UI.ItemSlot
import qualified Game.LambdaHack.Client.UI.Key as K
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Client.UI.MsgM
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.OverlayM
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Client.UI.Slideshow
import Game.LambdaHack.Client.UI.SlideshowM
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Color as Color
import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind
import qualified Game.LambdaHack.Content.TileKind as TK

-- * RespUpdAtomicUI

-- | Visualize atomic actions sent to the client. This is done
-- in the global state after the command is executed and after
-- the client state is modified by the command.
displayRespUpdAtomicUI :: MonadClientUI m
                       => Bool -> State -> UpdAtomic -> m ()
{-# INLINE displayRespUpdAtomicUI #-}
displayRespUpdAtomicUI verbose oldState cmd = case cmd of
  -- Create/destroy actors and items.
  UpdCreateActor aid body _ -> createActorUI True aid body
  UpdDestroyActor aid body _ -> destroyActorUI True aid body
  UpdCreateItem iid _ kit c -> do
    case c of
      CActor aid store -> do
        slastSlot <- updateItemSlotSide store aid iid
        case store of
          COrgan -> do
            bag <- getsState $ getContainerBag c
            let more = case EM.lookup iid bag of
                  Nothing -> False
                  Just kit2 -> fst kit2 /= fst kit
                verb = MU.Text $
                  "become" <+> case fst kit of
                                 1 -> if more then "more" else ""
                                 k -> if more then "additionally" else ""
                                      <+> tshow k <> "-fold"
            -- This describes all such items already among organs,
            -- which is useful, because it shows "charging".
            itemAidVerbMU aid verb iid (Left Nothing) COrgan
          _ -> do
            ownerFun <- partActorLeaderFun
            let wown = ppContainerWownW ownerFun True c
            itemVerbMU iid kit (MU.Text $ makePhrase $ "appear" : wown) c
            mleader <- getsClient _sleader
            when (Just aid == mleader) $
              modifySession $ \sess -> sess {slastSlot}
      CEmbed lid _ -> markDisplayNeeded lid
      CFloor lid _ -> do
        -- If you want an item to be assigned to @slastSlot@, create it
        -- in @CActor aid CGround@, not in @CFloor@.
        void $ updateItemSlot CGround Nothing iid
        itemVerbMU iid kit (MU.Text $ "appear" <+> ppContainer c) c
        markDisplayNeeded lid
      CTrunk{} -> error $ "" `showFailure` c
    stopPlayBack
  UpdDestroyItem iid _ kit c -> do
    itemVerbMU iid kit "disappear" c
    lid <- getsState $ lidFromC c
    markDisplayNeeded lid
  UpdSpotActor aid body _ -> createActorUI False aid body
  UpdLoseActor aid body _ -> destroyActorUI False aid body
  UpdSpotItem verbose2 iid _ kit c -> do
    -- This is due to a move, or similar, which will be displayed,
    -- so no extra @markDisplayNeeded@ needed here and in similar places.
    ItemSlots itemSlots _ <- getsSession sslots
    case lookup iid $ map swap $ EM.assocs itemSlots of
      Nothing ->  -- never seen or would have a slot
        case c of
          CActor aid store ->
            -- Most probably an actor putting item in or out of shared stash.
            void $ updateItemSlotSide store aid iid
          CEmbed{} -> return ()
          CFloor lid p -> do
            void $ updateItemSlot CGround Nothing iid
            sxhairOld <- getsSession sxhair
            case sxhairOld of
              TEnemy{} -> return ()  -- probably too important to overwrite
              TPoint TEnemyPos{} _ _ -> return ()
              _ -> do
                -- Don't steal xhair if it's only an item on another level.
                -- For enemies, OTOH, capture xhair to alarm player.
                lidV <- viewedLevelUI
                when (lid == lidV) $ do
                  bag <- getsState $ getFloorBag lid p
                  modifySession $ \sess ->
                    sess {sxhair = TPoint (TItem bag) lidV p}
            itemVerbMU iid kit "be spotted" c
            stopPlayBack
          CTrunk{} -> return ()
      _ -> return ()  -- seen already (has a slot assigned)
    when verbose2 $ case c of
      CActor aid store | store `elem` [CEqp, CInv] -> do
        -- Actor fetching an item from shared stash, most probably.
        bUI <- getsSession $ getActorUI aid
        subject <- partActorLeader aid bUI
        let ownW = ppCStoreWownW False store subject
            verb = MU.Text $ makePhrase $ "be added to" : ownW
        itemVerbMU iid kit verb c
      _ -> return ()
  UpdLoseItem False _ _ _ _ -> return ()
  -- The message is rather cryptic, so let's disable it until it's decided
  -- if anemy inventories should be displayed, etc.
  {-
  UpdLoseItem True iid _ kit c@(CActor aid store) | store /= CSha -> do
    -- Actor putting an item into shared stash, most probably.
    side <- getsClient sside
    b <- getsState $ getActorBody aid
    subject <- partActorLeader aid b
    let ownW = ppCStoreWownW store subject
        verb = MU.Text $ makePhrase $ "be removed from" : ownW
    when (bfid b == side) $ itemVerbMU iid kit verb c
  -}
  UpdLoseItem{} -> return ()
  -- Move actors and items.
  UpdMoveActor aid source target -> moveActor aid source target
  UpdWaitActor aid _ -> when verbose $ aidVerbMU aid "wait"
  UpdDisplaceActor source target -> displaceActorUI source target
  UpdMoveItem iid k aid c1 c2 -> moveItemUI iid k aid c1 c2
  -- Change actor attributes.
  UpdRefillHP _ 0 -> return ()
  UpdRefillHP aid n -> do
    when verbose $
      aidVerbMU aid $ MU.Text $ (if n > 0 then "heal" else "lose")
                                <+> tshow (abs n `divUp` oneM) <> "HP"
    b <- getsState $ getActorBody aid
    bUI <- getsSession $ getActorUI aid
    arena <- getArenaUI
    side <- getsClient sside
    if | bproj b && (length (beqp b) == 0 || isNothing (btrajectory b)) ->
           return ()  -- ignore caught proj or one hitting a wall
       | bhp b <= 0 && n < 0
         && (bfid b == side && not (bproj b) || arena == blid b) -> do
         let (firstFall, hurtExtra) = case (bfid b == side, bproj b) of
               (True, True) -> ("drop down", "tumble down")
               (True, False) -> ("fall down", "fall to pieces")
               (False, True) -> ("plummet", "crash")
               (False, False) -> ("collapse", "be reduced to a bloody pulp")
             verbDie = if alreadyDeadBefore then hurtExtra else firstFall
             alreadyDeadBefore = bhp b - n <= 0
         subject <- partActorLeader aid bUI
         let msgDie = makeSentence [MU.SubjectVerbSg subject verbDie]
         msgAdd msgDie
         -- We show death anims only if not dead already before this refill.
         let deathAct | alreadyDeadBefore =
                        twirlSplash (bpos b, bpos b) Color.Red Color.Red
                      | bfid b == side = deathBody (bpos b)
                      | otherwise = shortDeathBody (bpos b)
         unless (bproj b) $ animate (blid b) deathAct
       | otherwise -> do
         when (n >= bhp b && bhp b > 0) $
           actorVerbMU aid bUI "return from the brink of death"
         mleader <- getsClient _sleader
         when (Just aid == mleader) $ do
           actorAspect <- getsClient sactorAspect
           let ar = fromMaybe (error $ "" `showFailure` aid)
                              (EM.lookup aid actorAspect)
           -- Regenerating actors never stop gaining HP, so we need to stop
           -- reporting it after they reach full HP for the first time.
           when (bhp b >= xM (aMaxHP ar) && bhp b - n < xM (aMaxHP ar)) $ do
             actorVerbMU aid bUI "recover your health fully"
             stopPlayBack
  UpdRefillCalm aid calmDelta ->
    when (calmDelta == minusM) $ do  -- lower deltas come from hits; obvious
      side <- getsClient sside
      fact <- getsState $ (EM.! side) . sfactionD
      body <- getsState $ getActorBody aid
      when (bfid body == side) $ do
        let closeFoe b =  -- mimics isHeardFoe
                     blid b == blid body
                     && chessDist (bpos b) (bpos body) <= 3  -- a bit costly
                     && not (waitedLastTurn b)  -- uncommon
                     && inline isAtWar fact (bfid b)  -- costly
        anyCloseFoes <- getsState $ any closeFoe . EM.elems . sactorD
        unless anyCloseFoes $ do  -- obvious where the feeling comes from
          aidVerbMU aid "hear something"
          duplicated <- msgDuplicateScrap
          unless duplicated stopPlayBack
  UpdTrajectory{} -> return ()  -- if projectile dies here, no display
  -- Change faction attributes.
  UpdQuitFaction fid _ toSt -> quitFactionUI fid toSt
  UpdLeadFaction fid (Just source) (Just target) -> do
    side <- getsClient sside
    when (fid == side) $ do
      fact <- getsState $ (EM.! side) . sfactionD
      lidV <- viewedLevelUI
      when (isAIFact fact) $ markDisplayNeeded lidV
      -- This faction can't run with multiple actors, so this is not
      -- a leader change while running, but rather server changing
      -- their leader, which the player should be alerted to.
      when (noRunWithMulti fact) stopPlayBack
      actorD <- getsState sactorD
      case EM.lookup source actorD of
        Just sb | bhp sb <= 0 -> assert (not $ bproj sb) $ do
          -- Regardless who the leader is, give proper names here, not 'you'.
          sbUI <- getsSession $ getActorUI source
          tbUI <- getsSession $ getActorUI target
          let subject = partActor tbUI
              object  = partActor sbUI
          msgAdd $ makeSentence [ MU.SubjectVerbSg subject "take command"
                                , "from", object ]
        _ -> return ()
  UpdLeadFaction{} -> return ()
  UpdDiplFaction fid1 fid2 _ toDipl -> do
    name1 <- getsState $ gname . (EM.! fid1) . sfactionD
    name2 <- getsState $ gname . (EM.! fid2) . sfactionD
    let showDipl Unknown = "unknown to each other"
        showDipl Neutral = "in neutral diplomatic relations"
        showDipl Alliance = "allied"
        showDipl War = "at war"
    msgAdd $ name1 <+> "and" <+> name2 <+> "are now" <+> showDipl toDipl <> "."
  UpdTacticFaction{} -> return ()
  UpdAutoFaction fid b -> do
    side <- getsClient sside
    lidV <- viewedLevelUI
    markDisplayNeeded lidV
    when (fid == side) $ setFrontAutoYes b
  UpdRecordKill{} -> return ()
  -- Alter map.
  UpdAlterTile lid _ _ _ -> markDisplayNeeded lid
  UpdAlterExplorable{} -> return ()
  UpdSearchTile aid p toTile -> do
    Kind.COps{cotile = cotile@Kind.Ops{okind}} <- getsState scops
    b <- getsState $ getActorBody aid
    lvl <- getLevel $ blid b
    subject <- partAidLeader aid
    let t = lvl `at` p
        fromTile = Tile.hideAs cotile toTile
        verb | t == toTile = "confirm"
             | otherwise = "reveal"
        subject2 = MU.Text $ TK.tname $ okind fromTile
        verb2 = "be"
        object = MU.Text $ TK.tname $ okind toTile
    let msg = makeSentence [ MU.SubjectVerbSg subject verb
                           , "that the"
                           , MU.SubjectVerbSg subject2 verb2
                           , MU.AW object ]
    unless (subject2 == object) $ msgAdd msg
  UpdHideTile{} -> return ()
  UpdSpotTile{} -> return ()
  UpdLoseTile{} -> return ()
  UpdAlterSmell{} -> return ()
  UpdSpotSmell{} -> return ()
  UpdLoseSmell{} -> return ()
  -- Assorted.
  UpdTimeItem{} -> return ()
  UpdAgeGame{} -> do
    sdisplayNeeded <- getsSession sdisplayNeeded
    when sdisplayNeeded $ do
      -- Push the frame depicting the current level to the frame queue.
      -- Only one line of the report is shown, as in animations,
      -- because it may not be our turn, so we can't clear the message
      -- to see what is underneath.
      lidV <- viewedLevelUI
      report <- getReportUI
      let truncRep = [renderReport report]
      frame <- drawOverlay ColorFull False truncRep lidV
      displayFrames lidV [Just frame]
  UpdUnAgeGame{} -> return ()
  UpdDiscover c iid _ _ -> discover c oldState iid
  UpdCover{} -> return ()  -- don't spam when doing undo
  UpdDiscoverKind c iid _ -> discover c oldState iid
  UpdCoverKind{} -> return ()  -- don't spam when doing undo
  UpdDiscoverSeed c iid _ -> discover c oldState iid
  UpdCoverSeed{} -> return ()  -- don't spam when doing undo
  UpdDiscoverServer{} -> error "server command leaked to client"
  UpdCoverServer{} -> error "server command leaked to client"
  UpdPerception{} -> return ()
  UpdRestart fid _ _ _ _ -> do
    sstart <- getsSession sstart
    when (sstart == 0) resetSessionStart
    history <- getsSession shistory
    when (lengthHistory history == 0) $ do
      Kind.COps{corule} <- getsState scops
      let title = rtitle $ Kind.stdRuleset corule
      msgAdd $ "Welcome to" <+> title <> "!"
      -- Generate initial history. Only for UI clients.
      sconfig <- getsSession sconfig
      shistory <- defaultHistory $ configHistoryMax sconfig
      modifySession $ \sess -> sess {shistory}
    mode <- getGameMode
    curChal <- getsClient scurChal
    fact <- getsState $ (EM.! fid) . sfactionD
    let loneMode = case ginitial fact of
          [] -> True
          [(_, 1, _)] -> True
          _ -> False
    msgAdd $ "New game started in" <+> mname mode <+> "mode." <+> mdesc mode
             <+> if cwolf curChal && not loneMode
                 then "Being a lone wolf, you start without companions."
                 else ""
    when (lengthHistory history > 1) $ fadeOutOrIn False
    setFrontAutoYes $ isAIFact fact
    when (isAIFact fact) $ do
      -- Prod the frontend to flush frames and start showing them continuously.
      slides <- reportToSlideshow []
      void $ getConfirms ColorFull [K.spaceKM, K.escKM] slides
  UpdRestartServer{} -> return ()
  UpdResume fid _ -> do
    resetSessionStart
    fact <- getsState $ (EM.! fid) . sfactionD
    setFrontAutoYes $ isAIFact fact
    unless (isAIFact fact) $ do
      mode <- getGameMode
      promptAdd $ "Continuing" <+> mname mode <> "." <+> mdesc mode
                  <+> "Are you up for the challenge?"
      slides <- reportToSlideshow [K.spaceKM, K.escKM]
      km <- getConfirms ColorFull [K.spaceKM, K.escKM] slides
      if km == K.escKM then addPressedEsc else promptAdd "Prove yourself!"
  UpdResumeServer{} -> return ()
  UpdKillExit{} -> frontendShutdown
  UpdWriteSave -> when verbose $ promptAdd "Saving backup."
  UpdMsgAll "SortSlots" -> do  -- hack
    side <- getsClient sside
    sortSlots side Nothing
  UpdMsgAll msg -> msgAdd msg

updateItemSlot :: MonadClientUI m
               => CStore -> Maybe ActorId -> ItemId -> m SlotChar
updateItemSlot store maid iid = do
  slots@(ItemSlots itemSlots organSlots) <- getsSession sslots
  let onlyOrgans = store == COrgan
      lSlots = if onlyOrgans then organSlots else itemSlots
      incrementPrefix m l iid2 = EM.insert l iid2 $
        case EM.lookup l m of
          Nothing -> m
          Just iidOld ->
            let lNew = SlotChar (slotPrefix l + 1) (slotChar l)
            in incrementPrefix m lNew iidOld
  case lookup iid $ map swap $ EM.assocs lSlots of
    Nothing -> do
      side <- getsClient sside
      item <- getsState $ getItemBody iid
      lastSlot <- getsSession slastSlot
      mb <- maybe (return Nothing) (fmap Just . getsState . getActorBody) maid
      l <- getsState $ assignSlot store item side mb slots lastSlot
      let newSlots | onlyOrgans = ItemSlots
                                    itemSlots
                                    (incrementPrefix organSlots l iid)
                   | otherwise = ItemSlots
                                   (incrementPrefix itemSlots l iid)
                                   organSlots
      modifySession $ \sess -> sess {sslots = newSlots}
      return l
    Just l -> return l  -- slot already assigned; a letter or a number

markDisplayNeeded :: MonadClientUI m => LevelId -> m ()
markDisplayNeeded lid = do
  lidV <- viewedLevelUI
  when (lidV == lid) $
     modifySession $ \sess -> sess {sdisplayNeeded = True}

updateItemSlotSide :: MonadClientUI m
                   => CStore -> ActorId -> ItemId -> m SlotChar
updateItemSlotSide store aid iid = do
  side <- getsClient sside
  b <- getsState $ getActorBody aid
  if bfid b == side
  then updateItemSlot store (Just aid) iid
  else updateItemSlot store Nothing iid

lookAtMove :: MonadClientUI m => ActorId -> m ()
lookAtMove aid = do
  body <- getsState $ getActorBody aid
  side <- getsClient sside
  aimMode <- getsSession saimMode
  when (not (bproj body)
        && bfid body == side
        && isNothing aimMode) $ do  -- aiming does a more extensive look
    lookMsg <- lookAt False "" True (bpos body) aid ""
    msgAdd lookMsg
  fact <- getsState $ (EM.! bfid body) . sfactionD
  adjacentAssocs <- getsState $ actorAdjacentAssocs body
  if not (bproj body) && side == bfid body then do
    let foe (_, b2) = isAtWar fact (bfid b2)
        adjFoes = filter foe adjacentAssocs
    unless (null adjFoes) stopPlayBack
  else when (isAtWar fact side) $ do
    let our (_, b2) = not (bproj b2) && bfid b2 == side
        adjOur = filter our adjacentAssocs
    unless (null adjOur) stopPlayBack

-- | Sentences such as \"Dog barks loudly.\".
actorVerbMU :: MonadClientUI m => ActorId -> ActorUI -> MU.Part -> m ()
actorVerbMU aid bUI verb = do
  subject <- partActorLeader aid bUI
  msgAdd $ makeSentence [MU.SubjectVerbSg subject verb]

aidVerbMU :: MonadClientUI m => ActorId -> MU.Part -> m ()
aidVerbMU aid verb = do
  bUI <- getsSession $ getActorUI aid
  actorVerbMU aid bUI verb

itemVerbMU :: MonadClientUI m
           => ItemId -> ItemQuant -> MU.Part -> Container -> m ()
itemVerbMU iid kit@(k, _) verb c = assert (k > 0) $ do
  lid <- getsState $ lidFromC c
  localTime <- getsState $ getLocalTime lid
  itemToF <- itemToFullClient
  side <- getsClient sside
  factionD <- getsState sfactionD
  let subject = partItemWs side factionD
                                k (storeFromC c) localTime (itemToF iid kit)
      msg | k > 1 = makeSentence [MU.SubjectVerb MU.PlEtc MU.Yes subject verb]
          | otherwise = makeSentence [MU.SubjectVerbSg subject verb]
  msgAdd msg

-- We assume the item is inside the specified container.
-- So, this function can't be used for, e.g., @UpdDestroyItem@.
itemAidVerbMU :: MonadClientUI m
              => ActorId -> MU.Part
              -> ItemId -> Either (Maybe Int) Int -> CStore
              -> m ()
itemAidVerbMU aid verb iid ek cstore = do
  body <- getsState $ getActorBody aid
  bag <- getsState $ getBodyStoreBag body cstore
  side <- getsClient sside
  factionD <- getsState sfactionD
  -- The item may no longer be in @c@, but it was
  case iid `EM.lookup` bag of
    Nothing -> error $ "" `showFailure` (aid, verb, iid, cstore)
    Just kit@(k, _) -> do
      itemToF <- itemToFullClient
      let lid = blid body
      localTime <- getsState $ getLocalTime lid
      subject <- partAidLeader aid
      let itemFull = itemToF iid kit
          object = case ek of
            Left (Just n) ->
              assert (n <= k `blame` (aid, verb, iid, cstore))
              $ partItemWs side factionD n cstore localTime itemFull
            Left Nothing ->
              let (_, _, name, stats) =
                    partItem side factionD cstore localTime itemFull
              in MU.Phrase [name, stats]
            Right n ->
              assert (n <= k `blame` (aid, verb, iid, cstore))
              $ let (_, _, name1, stats) =
                      partItemShort side factionD cstore localTime itemFull
                    name = if n == 1 then name1 else MU.CarWs n name1
                in MU.Phrase ["the", name, stats]
          msg = makeSentence [MU.SubjectVerbSg subject verb, object]
      msgAdd msg

msgDuplicateScrap :: MonadClientUI m => m Bool
msgDuplicateScrap = do
  report <- getsSession _sreport
  history <- getsSession shistory
  let (lastMsg, repRest) = lastMsgOfReport report
      repLast = lastReportOfHistory history
  case incrementInReport (== lastMsg) repRest of
    Just repIncr -> do
      modifySession $ \sess -> sess {_sreport = repIncr}
      return True
    Nothing -> case incrementInReport (== lastMsg) repLast of
      Just repIncr -> do
        let historyIncr = replaceLastReportOfHistory repIncr history
        modifySession $ \sess -> sess { _sreport = repRest
                                      , shistory = historyIncr }
        return True
      Nothing -> return False

createActorUI :: MonadClientUI m => Bool -> ActorId -> Actor -> m ()
createActorUI born aid body = do
  side <- getsClient sside
  fact <- getsState $ (EM.! bfid body) . sfactionD
  mbUI <- getsSession $ EM.lookup aid . sactorUI
  bUI <- case mbUI of
    Just bUI -> return bUI
    Nothing -> do
      trunk <- getsState $ getItemBody $ btrunk body
      Config{configHeroNames} <- getsSession sconfig
      let isBlast = actorTrunkIsBlast trunk
          baseColor = flavourToColor $ jflavour trunk
          basePronoun | not (bproj body) && fhasGender (gplayer fact) = "he"
                      | otherwise = "it"
          nameFromNumber fn k = if k == 0
                                then makePhrase [MU.Ws $ MU.Text fn, "Captain"]
                                else fn <+> tshow k
          heroNamePronoun k =
            if gcolor fact /= Color.BrWhite
            then (nameFromNumber (fname $ gplayer fact) k, "he")
            else fromMaybe (nameFromNumber (fname $ gplayer fact) k, "he")
                 $ lookup k configHeroNames
      (n, bsymbol) <-
        if | bproj body -> return (0, if isBlast then jsymbol trunk else '*')
           | baseColor /= Color.BrWhite -> return (0, jsymbol trunk)
           | otherwise -> do
             sactorUI <- getsSession sactorUI
             let hasNameK k bUI = bname bUI == fst (heroNamePronoun k)
                                  && bcolor bUI == gcolor fact
                 findHeroK k = isJust $ find (hasNameK k) (EM.elems sactorUI)
                 mhs = map findHeroK [0..]
                 n = fromJust $ elemIndex False mhs
             return (n, if 0 < n && n < 10 then Char.intToDigit n else '@')
      factionD <- getsState sfactionD
      localTime <- getsState $ getLocalTime $ blid body
      let (bname, bpronoun) =
            if | bproj body ->
                 let adj | length (btrajectory body) < 5 = "falling"
                         | otherwise = "flying"
                     -- Not much detail about a fast flying item.
                     (_, _, object1, object2) =
                       partItem (bfid body) factionD CInv localTime
                                (itemNoDisco (trunk, 1))
                 in ( makePhrase [MU.AW $ MU.Text adj, object1, object2]
                    , basePronoun )
               | baseColor /= Color.BrWhite -> (jname trunk, basePronoun)
               | otherwise -> heroNamePronoun n
          bcolor | bproj body = if isBlast then baseColor else Color.BrWhite
                 | baseColor == Color.BrWhite = gcolor fact
                 | otherwise = baseColor
          bUI = ActorUI{..}
      modifySession $ \sess ->
        sess {sactorUI = EM.insert aid bUI $ sactorUI sess}
      return bUI
  let verb = if born
             then MU.Text $ "appear"
                            <+> if bfid body == side then "" else "suddenly"
             else "be spotted"
  mapM_ (\(iid, store) -> void $ updateItemSlotSide store aid iid)
        (getCarriedIidCStore body)
  when (bfid body /= side) $ do
    when (not (bproj body) && isAtWar fact side) $
      -- Aim even if nobody can shoot at the enemy. Let's home in on him
      -- and then we can aim or melee. We set permit to False, because it's
      -- technically very hard to check aimability here, because we are
      -- in-between turns and, e.g., leader's move has not yet been taken
      -- into account.
      modifySession $ \sess -> sess {sxhair = TEnemy aid False}
    stopPlayBack
  -- Don't spam if the actor was already visible (but, e.g., on a tile that is
  -- invisible this turn (in that case move is broken down to lose+spot)
  -- or on a distant tile, via teleport while the observer teleported, too).
  lastLost <- getsSession slastLost
  if ES.member aid lastLost || bproj body then
    markDisplayNeeded (blid body)
  else do
    actorVerbMU aid bUI verb
    animate (blid body) $ actorX (bpos body)
  lookAtMove aid

destroyActorUI :: MonadClientUI m => Bool -> ActorId -> Actor -> m ()
destroyActorUI destroy aid b = do
  trunk <- getsState $ getItemBody $ btrunk b
  let baseColor = flavourToColor $ jflavour trunk
  unless (baseColor == Color.BrWhite) $  -- keep setup for heroes, etc.
    modifySession $ \sess -> sess {sactorUI = EM.delete aid $ sactorUI sess}
  let affect tgt = case tgt of
        TEnemy a permit | a == aid ->
          if destroy then
            -- If *really* nothing more interesting, the actor will
            -- go to last known location to perhaps find other foes.
            TPoint TAny (blid b) (bpos b)
          else
            -- If enemy only hides (or we stepped behind obstacle) find him.
            TPoint (TEnemyPos a permit) (blid b) (bpos b)
        _ -> tgt
  modifySession $ \sess -> sess {sxhair = affect $ sxhair sess}
  when (isNothing $ btrajectory b) $
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
        displayMore ColorBW "Alas!"
        mleader <- getsClient _sleader
        when (isJust mleader)
          -- This is especially handy when the dead actor was a leader
          -- on a different level than the new one:
          clearAimMode
    -- If pushed, animate spotting again, to draw attention to pushing.
    markDisplayNeeded (blid b)

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
  sb <- getsState $ getActorBody source
  sbUI <- getsSession $ getActorUI source
  tb <- getsState $ getActorBody target
  tbUI <- getsSession $ getActorUI target
  spart <- partActorLeader source sbUI
  tpart <- partActorLeader target tbUI
  let msg = makeSentence [MU.SubjectVerbSg spart "displace", tpart]
  msgAdd msg
  when (bfid sb /= bfid tb) $ do
    lookAtMove source
    lookAtMove target
  let ps = (bpos tb, bpos sb)
  animate (blid sb) $ swapPlaces ps

moveItemUI :: MonadClientUI m
           => ItemId -> Int -> ActorId -> CStore -> CStore
           -> m ()
moveItemUI iid k aid cstore1 cstore2 = do
  let verb = verbCStore cstore2
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  let underAI = isAIFact fact
  mleader <- getsClient _sleader
  ItemSlots itemSlots _ <- getsSession sslots
  case lookup iid $ map swap $ EM.assocs itemSlots of
    Just slastSlot -> do
      when (Just aid == mleader) $ modifySession $ \sess -> sess {slastSlot}
      if cstore1 == CGround && Just aid == mleader && not underAI then
        itemAidVerbMU aid (MU.Text verb) iid (Right k) cstore2
      else when (not (bproj b) && bhp b > 0) $  -- don't announce death drops
        itemAidVerbMU aid (MU.Text verb) iid (Left $ Just k) cstore2
    Nothing -> error $
      "" `showFailure` (iid, k, aid, cstore1, cstore2, itemSlots)

quitFactionUI :: MonadClientUI m => FactionId -> Maybe Status -> m ()
quitFactionUI fid toSt = do
  Kind.COps{coitem=Kind.Ops{okind, ouniqGroup}} <- getsState scops
  fact <- getsState $ (EM.! fid) . sfactionD
  let fidName = MU.Text $ gname fact
      person = if fhasGender $ gplayer fact then MU.PlEtc else MU.Sg3rd
      horror = isHorrorFact fact
  side <- getsClient sside
  when (side == fid && maybe False ((/= Camping) . stOutcome) toSt) $ do
    let won = case toSt of
          Just Status{stOutcome=Conquer} -> True
          Just Status{stOutcome=Escape} -> True
          _ -> False
    when won $ do
      gameModeId <- getsState sgameModeId
      scurChal <- getsClient scurChal
      let sing = M.singleton scurChal 1
          f = M.unionWith (+)
          g = EM.insertWith f gameModeId sing
      modifyClient $ \cli -> cli {svictories = g $ svictories cli}
    tellGameClipPS
    resetGameStart
  let msgIfSide _ | fid /= side = Nothing
      msgIfSide s = Just s
      (startingPart, partingPart) = case toSt of
        _ | horror ->
          -- Ignore summoned actors' factions.
          (Nothing, Nothing)
        Just Status{stOutcome=Killed} ->
          ( Just "be eliminated"
          , msgIfSide "Let's hope another party can save the day!" )
        Just Status{stOutcome=Defeated} ->
          ( Just "be decisively defeated"
          , msgIfSide "Let's hope your new overlords let you live." )
        Just Status{stOutcome=Camping} ->
          ( Just "order save and exit"
          , Just $ if fid == side
                   then "See you soon, stronger and braver!"
                   else "See you soon, stalwart warrior!" )
        Just Status{stOutcome=Conquer} ->
          ( Just "vanquish all foes"
          , msgIfSide "Can it be done in a better style, though?" )
        Just Status{stOutcome=Escape} ->
          ( Just "achieve victory"
          , msgIfSide "Can it be done better, though?" )
        Just Status{stOutcome=Restart, stNewGame=Just gn} ->
          ( Just $ MU.Text $ "order mission restart in" <+> tshow gn <+> "mode"
          , Just $ if fid == side
                   then "This time for real."
                   else "Somebody couldn't stand the heat." )
        Just Status{stOutcome=Restart, stNewGame=Nothing} ->
          error $ "" `showFailure` (fid, toSt)
        Nothing -> (Nothing, Nothing)  -- server wipes out Camping for savefile
  case startingPart of
    Nothing -> return ()
    Just sp -> msgAdd $ makeSentence [MU.SubjectVerb person MU.Yes fidName sp]
  case (toSt, partingPart) of
    (Just status, Just pp) -> do
      isNoConfirms <- isNoConfirmsGame
      go <- if isNoConfirms then return False else displaySpaceEsc ColorFull ""
      when (side == fid) recordHistory
        -- we are going to exit or restart, so record and clear, but only once
      when go $ do
        lidV <- viewedLevelUI
        Level{lxsize, lysize} <- getLevel lidV
        let store = CGround  -- only matters for UI details; all items shown
            currencyName = MU.Text $ IK.iname $ okind $ ouniqGroup "currency"
        arena <- getArenaUI
        (bag, itemSlides, total) <- do
          (bag, tot) <- getsState $ calculateTotal side
          if EM.null bag then return (EM.empty, emptySlideshow, 0)
          else do
            let spoilsMsg = makeSentence [ "Your spoils are worth"
                                         , MU.CarWs tot currencyName ]
            promptAdd spoilsMsg
            io <- itemOverlay store arena bag
            sli <- overlayToSlideshow (lysize + 1) [K.spaceKM, K.escKM] io
            return (bag, sli, tot)
        localTime <- getsState $ getLocalTime arena
        itemToF <- itemToFullClient
        ItemSlots lSlots _ <- getsSession sslots
        let keyOfEKM (Left km) = km
            keyOfEKM (Right SlotChar{slotChar}) = [K.mkChar slotChar]
            allOKX = concatMap snd $ slideshow itemSlides
            keys = [K.spaceKM, K.escKM] ++ concatMap (keyOfEKM . fst) allOKX
            examItem slot =
              case EM.lookup slot lSlots of
                Nothing -> error $ "" `showFailure` slot
                Just iid -> case EM.lookup iid bag of
                  Nothing -> error $ "" `showFailure` iid
                  Just kit@(k, _) -> do
                    factionD <- getsState sfactionD
                    let itemFull = itemToF iid kit
                        attrLine = itemDesc side factionD 0
                                            store localTime itemFull
                        ov = splitAttrLine lxsize attrLine
                        worth = itemPrice (itemBase itemFull, 1)
                        lootMsg = makeSentence $
                          ["This particular loot is worth"]
                          ++ (if k > 1 then [ MU.Cardinal k, "times"] else [])
                          ++ [MU.CarWs worth currencyName]
                    promptAdd lootMsg
                    slides <- overlayToSlideshow (lysize + 1)
                                                 [K.spaceKM, K.escKM]
                                                 (ov, [])
                    km <- getConfirms ColorFull [K.spaceKM, K.escKM] slides
                    return $! km == K.spaceKM
            viewItems pointer =
              if itemSlides == emptySlideshow then return True
              else do
                (ekm, pointer2) <- displayChoiceScreen ColorFull False pointer
                                                       itemSlides keys
                case ekm of
                  Left km | km == K.spaceKM -> return True
                  Left km | km == K.escKM -> return False
                  Left _ -> error $ "" `showFailure` ekm
                  Right slot -> do
                    go2 <- examItem slot
                    if go2 then viewItems pointer2 else return True
        go3 <- viewItems 2
        when go3 $ do
          -- Show score for any UI client after any kind of game exit,
          -- even though it is saved only for human UI clients at game over.
          scoreSlides <- scoreToSlideshow total status
          void $ getConfirms ColorFull [K.spaceKM, K.escKM] scoreSlides
          -- The last prompt stays onscreen during shutdown, etc.
          promptAdd pp
          partingSlide <- reportToSlideshow [K.spaceKM, K.escKM]
          void $ getConfirms ColorFull [K.spaceKM, K.escKM] partingSlide
      unless (fmap stOutcome toSt == Just Camping) $
        fadeOutOrIn True
    _ -> return ()

discover :: MonadClientUI m => Container -> State -> ItemId -> m ()
discover c oldState iid = do
  let oldDiscoKind = sdiscoKind oldState
      oldDiscoAspect = sdiscoAspect oldState
      cstore = storeFromC c
  lid <- getsState $ lidFromC c
  discoKind <- getsState sdiscoKind
  discoAspect <- getsState sdiscoAspect
  localTime <- getsState $ getLocalTime lid
  itemToF <- itemToFullClient
  bag <- getsState $ getContainerBag c
  side <- getsClient sside
  factionD <- getsState sfactionD
  (isOurOrgan, nameWhere) <- case c of
    CActor aidOwner storeOwner -> do
      bOwner <- getsState $ getActorBody aidOwner
      bOwnerUI <- getsSession $ getActorUI aidOwner
      let name = if bproj bOwner || bfid bOwner == side
                 then []
                 else ppCStoreWownW True storeOwner (partActor bOwnerUI)
      return (bfid bOwner == side && storeOwner == COrgan, name)
    _ -> return (False, [])
  let kit = EM.findWithDefault (1, []) iid bag
      itemFull = itemToF iid kit
      knownName = partItemMediumAW side factionD cstore localTime itemFull
      -- Wipe out the whole knowledge of the item to make sure the two names
      -- in the message differ even if, e.g., the item is described as
      -- "of many effects".
      itemSecret = itemNoDisco (itemBase itemFull, itemK itemFull)
      (_, _, secretName, secretAEText) =
        partItem side factionD cstore localTime itemSecret
      namePhrase = MU.Phrase $ [secretName, secretAEText] ++ nameWhere
      msg = makeSentence
        ["the", MU.SubjectVerbSg namePhrase "turn out to be", knownName]
      jix = jkindIx $ itemBase itemFull
      ik = itemKind $ fromJust $ itemDisco itemFull
  -- Compare descriptions of all aspects and effects to determine
  -- if the discovery was meaningful to the player.
  unless (isOurOrgan
          || (EM.member jix discoKind == EM.member jix oldDiscoKind
              && (EM.member iid discoAspect == EM.member iid oldDiscoAspect
                  || not (aspectsRandom ik)))) $
    msgAdd msg

-- * RespSfxAtomicUI

-- | Display special effects (text, animation) sent to the client.
displayRespSfxAtomicUI :: MonadClientUI m => Bool -> SfxAtomic -> m ()
{-# INLINE displayRespSfxAtomicUI #-}
displayRespSfxAtomicUI verbose sfx = case sfx of
  SfxStrike source target iid store ->
    strike False source target iid store
  SfxRecoil source target _ _ -> do
    spart <- partAidLeader source
    tpart <- partAidLeader target
    msgAdd $ makeSentence [MU.SubjectVerbSg spart "shrink away from", tpart]
  SfxSteal source target iid store ->
    strike True source target iid store
  SfxRelease source target _ _ -> do
    spart <- partAidLeader source
    tpart <- partAidLeader target
    msgAdd $ makeSentence [MU.SubjectVerbSg spart "release", tpart]
  SfxProject aid iid cstore -> do
    setLastSlot aid iid cstore
    itemAidVerbMU aid "fling" iid (Left $ Just 1) cstore
  SfxReceive aid iid cstore ->
    itemAidVerbMU aid "receive" iid (Left $ Just 1) cstore
  SfxApply aid iid cstore -> do
    setLastSlot aid iid cstore
    itemAidVerbMU aid "apply" iid (Left $ Just 1) cstore
  SfxCheck aid iid cstore ->
    itemAidVerbMU aid "deapply" iid (Left $ Just 1) cstore
  SfxTrigger aid _p ->
    -- So far triggering is visible, e.g., doors close, so no need for messages.
    when verbose $ aidVerbMU aid "trigger"
  SfxShun aid _p ->
    when verbose $ aidVerbMU aid "shun"
  SfxEffect fidSource aid effect hpDelta -> do
    b <- getsState $ getActorBody aid
    bUI <- getsSession $ getActorUI aid
    side <- getsClient sside
    let fid = bfid b
        isOurCharacter = fid == side && not (bproj b)
        isOurAlive = isOurCharacter && bhp b > 0
    case effect of
        IK.ELabel{} -> return ()
        IK.EqpSlot{} -> return ()
        IK.Burn{} -> do
          if isOurAlive
          then actorVerbMU aid bUI "feel burned"
          else actorVerbMU aid bUI "look burned"
          let ps = (bpos b, bpos b)
          animate (blid b) $ twirlSplash ps Color.BrRed Color.Brown
        IK.Explode{} -> return ()  -- lots of visual feedback
        IK.RefillHP p | p == 1 -> return ()  -- no spam from regeneration
        IK.RefillHP p | p == -1 -> return ()  -- no spam from poison
        IK.RefillHP{} | hpDelta > 0 -> do
          if isOurAlive then
            actorVerbMU aid bUI "feel healthier"
          else
            actorVerbMU aid bUI "look healthier"
          let ps = (bpos b, bpos b)
          animate (blid b) $ twirlSplash ps Color.BrGreen Color.Green
        IK.RefillHP{} -> do
          if isOurAlive then
            actorVerbMU aid bUI "feel wounded"
          else
            actorVerbMU aid bUI "look wounded"
          let ps = (bpos b, bpos b)
          animate (blid b) $ twirlSplash ps Color.BrRed Color.Red
        IK.RefillCalm p | p == 1 -> return ()  -- no spam from regen items
        IK.RefillCalm p | p > 0 ->
          if isOurAlive then
            actorVerbMU aid bUI "feel calmer"
          else
            actorVerbMU aid bUI "look calmer"
        IK.RefillCalm _ ->
          if isOurAlive then
            actorVerbMU aid bUI "feel agitated"
          else
            actorVerbMU aid bUI "look agitated"
        IK.Dominate -> do
          -- For subsequent messages use the proper name, never "you".
          let subject = partActor bUI
          if fid /= fidSource then do
            -- Before domination, possibly not seen if actor (yet) not ours.
            if | bcalm b == 0 ->  -- sometimes only a coincidence, but nm
                 aidVerbMU aid $ MU.Text "yield, under extreme pressure"
               | isOurAlive ->
                 aidVerbMU aid $ MU.Text "black out, dominated by foes"
               | otherwise ->
                 aidVerbMU aid $ MU.Text "decide abrubtly to switch allegiance"
            fidName <- getsState $ gname . (EM.! fid) . sfactionD
            let verb = "be no longer controlled by"
            msgAdd $ makeSentence
              [MU.SubjectVerbSg subject verb, MU.Text fidName]
            when isOurAlive $ displayMoreKeep ColorFull ""
          else do
            -- After domination, possibly not seen, if actor (already) not ours.
            fidSourceName <- getsState $ gname . (EM.! fidSource) . sfactionD
            let verb = "be now under"
            msgAdd $ makeSentence
              [MU.SubjectVerbSg subject verb, MU.Text fidSourceName, "control"]
          stopPlayBack
        IK.Impress -> actorVerbMU aid bUI $ "be awestruck"
        IK.Summon grp p -> do
          let verb = if bproj b then "lure" else "summon"
              object = if p == 1
                       then MU.Text $ tshow grp
                       else MU.Ws $ MU.Text $ tshow grp  -- avoid "1 + dl 3"
          actorVerbMU aid bUI $ MU.Phrase [verb, object]
        IK.Ascend True -> actorVerbMU aid bUI "find a way upstairs"
        IK.Ascend False -> actorVerbMU aid bUI "find a way downstairs"
        IK.Escape{} -> return ()
        IK.Paralyze{} -> actorVerbMU aid bUI "be paralyzed"
        IK.InsertMove{} -> actorVerbMU aid bUI "act with extreme speed"
        IK.Teleport t | Dice.maxDice t <= 9 -> actorVerbMU aid bUI "blink"
        IK.Teleport{} -> actorVerbMU aid bUI "teleport"
        IK.CreateItem{} -> return ()
        IK.DropItem _ _ COrgan _ -> return ()
        IK.DropItem{} -> actorVerbMU aid bUI "be stripped"
        IK.PolyItem -> do
          localTime <- getsState $ getLocalTime $ blid b
          allAssocs <- fullAssocsClient aid [CGround]
          case allAssocs of
            [] -> return ()  -- invisible items?
            (_, ItemFull{..}) : _ -> do
              subject <- partActorLeader aid bUI
              factionD <- getsState sfactionD
              let itemSecret = itemNoDisco (itemBase, itemK)
                  (_, _, secretName, secretAEText) =
                    partItem side factionD CGround localTime itemSecret
                  verb = "repurpose"
                  store = MU.Text $ ppCStoreIn CGround
              msgAdd $ makeSentence
                [ MU.SubjectVerbSg subject verb
                , "the", secretName, secretAEText, store ]
        IK.Identify -> do
          allAssocs <- fullAssocsClient aid [CGround]
          case allAssocs of
            [] -> return ()  -- invisible items?
            (_, ItemFull{..}) : _ -> do
              subject <- partActorLeader aid bUI
              let verb = "inspect"
                  store = MU.Text $ ppCStoreIn CGround
              msgAdd $ makeSentence
                [ MU.SubjectVerbSg subject verb
                , "an item", store ]
        IK.Detect{} -> do
          subject <- partActorLeader aid bUI
          let verb = "perceive nearby area"
          displayMore ColorFull $ makeSentence [MU.SubjectVerbSg subject verb]
        IK.DetectActor{} -> do
          subject <- partActorLeader aid bUI
          let verb = "detect nearby actors"
          displayMore ColorFull $ makeSentence [MU.SubjectVerbSg subject verb]
        IK.DetectItem{} -> do
          subject <- partActorLeader aid bUI
          let verb = "detect nearby items"
          displayMore ColorFull $ makeSentence [MU.SubjectVerbSg subject verb]
        IK.DetectExit{} -> do
          subject <- partActorLeader aid bUI
          let verb = "detect nearby exits"
          displayMore ColorFull $ makeSentence [MU.SubjectVerbSg subject verb]
        IK.DetectHidden{} -> do
          subject <- partActorLeader aid bUI
          let verb = "detect nearby secrets"
          displayMore ColorFull $ makeSentence [MU.SubjectVerbSg subject verb]
        IK.SendFlying{} -> actorVerbMU aid bUI "be sent flying"
        IK.PushActor{} -> actorVerbMU aid bUI "be pushed"
        IK.PullActor{} -> actorVerbMU aid bUI "be pulled"
        IK.DropBestWeapon -> actorVerbMU aid bUI "be disarmed"
        IK.ActivateInv{} -> return ()
        IK.ApplyPerfume ->
          msgAdd "The fragrance quells all scents in the vicinity."
        IK.OneOf{} -> return ()
        IK.OnSmash{} -> error $ "" `showFailure` sfx
        IK.Recharging{} -> error $ "" `showFailure` sfx
        IK.Temporary t -> actorVerbMU aid bUI $ MU.Text t
        IK.Unique -> error $ "" `showFailure` sfx
        IK.Periodic -> error $ "" `showFailure` sfx
  SfxMsgFid _ sfxMsg -> do
    mleader <- getsClient _sleader
    case mleader of
      Just{} -> return ()  -- will display stuff when leader moves
      Nothing -> do
        lidV <- viewedLevelUI
        markDisplayNeeded lidV
        recordHistory
    msg <- ppSfxMsg sfxMsg
    msgAdd msg

ppSfxMsg :: MonadClientUI m => SfxMsg -> m Text
ppSfxMsg sfxMsg = case sfxMsg of
  SfxUnexpected reqFailure -> return $!
    "Unexpected problem:" <+> showReqFailure reqFailure <> "."
  SfxLoudUpd local cmd -> do
    Kind.COps{coTileSpeedup} <- getsState scops
    let sound = case cmd of
          UpdDestroyActor{} -> "shriek"
          UpdCreateItem{} -> "clatter"
          UpdTrajectory{} ->
            -- Projectile hits an non-walkable tile on leader's level.
            "thud"
          UpdAlterTile _ _ fromTile _ ->
            if Tile.isDoor coTileSpeedup fromTile
            then "creaking sound"
            else "rumble"
          UpdAlterExplorable _ k -> if k > 0 then "grinding noise"
                                             else "fizzing noise"
          _ -> error $ "" `showFailure` cmd
        distant = if local then [] else ["distant"]
        msg = makeSentence [ "you hear"
                           , MU.AW $ MU.Phrase $ distant ++ [sound] ]
    return $! msg
  SfxLoudStrike local ik distance -> do
    Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
    let verb = IK.iverbHit $ okind ik
        adverb = if | distance < 5 -> "loudly"
                    | distance < 10 -> "distinctly"
                    | distance < 40 -> ""  -- most common
                    | distance < 45 -> "faintly"
                    | otherwise -> "barely"  -- 50 is the hearing limit
        distant = if local then [] else ["far away"]
        msg = makeSentence $
          [ "you", adverb, "hear something", verb, "someone"] ++ distant
    return $! msg
  SfxLoudSummon isProj grp p -> do
    let verb = if isProj then "something lure" else "somebody summon"
        object = if p == 1
                 then MU.Text $ tshow grp
                 else MU.Ws $ MU.Text $ tshow grp
    return $! makeSentence ["you hear", verb, object]
  SfxFizzles -> return "It flashes and fizzles."
  SfxNothingHappens -> return "Nothing happens."
  SfxVoidDetection -> return "Nothing new detected."
  SfxSummonLackCalm aid -> do
    msbUI <- getsSession $ EM.lookup aid . sactorUI
    case msbUI of
      Nothing -> return ""
      Just sbUI -> do
        let subject = partActor sbUI
            verb = "lack Calm to summon"
        return $! makeSentence [MU.SubjectVerbSg subject verb]
  SfxLevelNoMore -> return "No more levels in this direction."
  SfxLevelPushed -> return "You notice somebody pushed to another level."
  SfxBracedImmune aid -> do
    msbUI <- getsSession $ EM.lookup aid . sactorUI
    case msbUI of
      Nothing -> return ""
      Just sbUI -> do
        let subject = partActor sbUI
            verb = "be braced and so immune to translocation"
        return $! makeSentence [MU.SubjectVerbSg subject verb]
  SfxEscapeImpossible -> return "This faction doesn't want to escape outside."
  SfxTransImpossible -> return "Translocation not possible."
  SfxIdentifyNothing store -> return $!
    "Nothing to identify" <+> ppCStoreIn store <> "."
  SfxPurposeNothing store -> return $!
    "The purpose of repurpose cannot be availed without an item"
    <+> ppCStoreIn store <> "."
  SfxPurposeTooFew maxCount itemK -> return $!
    "The purpose of repurpose is served by" <+> tshow maxCount
    <+> "pieces of this item, not by" <+> tshow itemK <> "."
  SfxPurposeUnique -> return "Unique items can't be repurposed."
  SfxColdFish -> return "Healing attempt from another faction is thwarted by your cold fish attitude."
  SfxTimerExtended aid iid cstore -> do
    b <- getsState $ getActorBody aid
    bUI <- getsSession $ getActorUI aid
    aidPhrase <- partActorLeader aid bUI
    factionD <- getsState sfactionD
    localTime <- getsState $ getLocalTime (blid b)
    itemToF <- itemToFullClient
    let itemFull = itemToF iid (1, [])
        (_, _, name, stats) =
          partItem (bfid b) factionD cstore localTime itemFull
        storeOwn = ppCStoreWownW True cstore aidPhrase
        cond = if jsymbol (itemBase itemFull) == '+' then ["condition"] else []
    return $! makeSentence $
      ["the", name, stats] ++ cond ++ storeOwn ++ ["will now last longer"]

setLastSlot :: MonadClientUI m => ActorId -> ItemId -> CStore -> m ()
setLastSlot aid iid cstore = do
  mleader <- getsClient _sleader
  when (Just aid == mleader) $ do
    ItemSlots itemSlots _ <- getsSession sslots
    case lookup iid $ map swap $ EM.assocs itemSlots of
      Just slastSlot -> modifySession $ \sess -> sess {slastSlot}
      Nothing -> error $ "" `showFailure` (iid, cstore, aid)

strike :: MonadClientUI m
       => Bool -> ActorId -> ActorId -> ItemId -> CStore -> m ()
strike catch source target iid cstore = assert (source /= target) $ do
  actorAspect <- getsClient sactorAspect
  tb <- getsState $ getActorBody target
  tbUI <- getsSession $ getActorUI target
  sourceSeen <- getsState $ memActor source (blid tb)
  (ps, hurtMult) <-
   if sourceSeen then do
    hurtMult <- getsState $ armorHurtBonus actorAspect source target
    itemToF <- itemToFullClient
    sb <- getsState $ getActorBody source
    sbUI <- getsSession $ getActorUI source
    spart <- partActorLeader source sbUI
    tpart <- partActorLeader target tbUI
    spronoun <- partPronounLeader source sbUI
    localTime <- getsState $ getLocalTime (blid tb)
    bag <- getsState $ getBodyStoreBag sb cstore
    side <- getsClient sside
    factionD <- getsState sfactionD
    let kit = EM.findWithDefault (1, []) iid bag
        itemFull = itemToF iid kit
        verb = case itemDisco itemFull of
          _ | catch -> "catch"
          Nothing -> "hit"  -- not identified
          Just ItemDisco{itemKind} -> IK.iverbHit itemKind
        isOrgan = iid `EM.member` borgan sb
        partItemChoice =
          if isOrgan
          then partItemShortWownW side factionD spronoun COrgan localTime
          else partItemShortAW side factionD cstore localTime
        msg | bhp tb <= 0  -- incapacitated, so doesn't actively block
              || hurtMult > 90  -- at most minor armor
              || jdamage (itemBase itemFull) == 0 = makeSentence $
              [MU.SubjectVerbSg spart verb, tpart]
              ++ if bproj sb
                 then []
                 else ["with", partItemChoice itemFull]
            | otherwise =
          -- This sounds funny when the victim falls down immediately,
          -- but there is no easy way to prevent that. And it's consistent.
          -- If/when death blow instead sets HP to 1 and only the next below 1,
          -- we can check here for HP==1; also perhaps actors with HP 1 should
          -- not be able to block.
          let sActs = if bproj sb
                      then [ MU.SubjectVerbSg spart "connect" ]
                      else [ MU.SubjectVerbSg spart verb, tpart
                           , "with", partItemChoice itemFull ]
              actionPhrase =
                MU.SubjectVerbSg tpart
                $ if bproj sb
                  then if braced tb
                       then "deflect it"
                       else "fend it off"  -- ward it off
                  else if braced tb
                       then "block"  -- parry
                       else "dodge"  -- evade
              butEvenThough = if catch then ", even though" else ", but"
          in makeSentence
               [ MU.Phrase sActs <> butEvenThough
               , actionPhrase
               , if | hurtMult >= 50 ->  -- braced or big bonuses
                      "partly"
                    | hurtMult > 1 ->  -- braced and/or huge bonuses
                      if braced tb then "doggedly" else "nonchalantly"
                    | otherwise ->         -- 1% got through, which can
                      "almost completely"  -- still be deadly, if fast missile
               ]
    msgAdd msg
    return ((bpos tb, bpos sb), hurtMult)
   else return ((bpos tb, bpos tb), 100)
  let anim | hurtMult > 90 = twirlSplash ps Color.BrRed Color.Red
           | hurtMult > 1 = blockHit ps Color.BrRed Color.Red
           | otherwise = blockMiss ps
  animate (blid tb) anim
