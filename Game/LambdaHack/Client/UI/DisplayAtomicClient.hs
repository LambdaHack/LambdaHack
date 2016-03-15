-- | Display atomic commands received by the client.
module Game.LambdaHack.Client.UI.DisplayAtomicClient
  ( displayRespUpdAtomicUI, displayRespSfxAtomicUI
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List (delete)
import Data.Maybe
import Data.Monoid
import Data.Tuple
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.CommonClient
import Game.LambdaHack.Client.ItemSlot
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Client.UI.MsgClient
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Client.UI.WidgetClient
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Color as Color
import qualified Game.LambdaHack.Common.Dice as Dice
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemDescription
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Time
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind
import qualified Game.LambdaHack.Content.TileKind as TK

-- * RespUpdAtomicUI

-- TODO: let user configure which messages are not created, which are
-- slightly hidden, which are shown and which flash and center screen
-- and perhaps highligh the related location/actor. Perhaps even
-- switch to the actor, changing HP displayed on screen, etc.
-- but it's too short a clip to read the numbers, so probably
-- highlighing should be enough.
-- TODO: for a start, flesh out the verbose variant and then add
-- a single client debug option that flips verbosity
--
-- | Visualize atomic actions sent to the client. This is done
-- in the global state after the command is executed and after
-- the client state is modified by the command.
displayRespUpdAtomicUI :: MonadClientUI m
                       => Bool -> StateClient -> UpdAtomic -> m ()
displayRespUpdAtomicUI verbose oldStateClient cmd = case cmd of
  -- Create/destroy actors and items.
  UpdCreateActor aid body _ -> do
    side <- getsClient sside
    let verb = "appear" <+> if bfid body == side then "" else "suddenly"
    createActorUI aid body verbose (MU.Text verb)
  UpdDestroyActor aid body _ -> do
    destroyActorUI aid body "die" "be destroyed" verbose
    side <- getsClient sside
    when (bfid body == side && not (bproj body)) $
      void $ stopPlayBack
  UpdCreateItem iid _ kit c -> do
    case c of
      CActor aid store -> do
        l <- updateItemSlotSide store aid iid
        case store of
          COrgan -> do
            let verb =
                  MU.Text $ "become" <+> case fst kit of
                                           1 -> ""
                                           k -> tshow k <> "-fold"
            -- This describes all such items already among organs,
            -- which is useful, because it shows "charging".
            itemAidVerbMU aid verb iid (Left Nothing) COrgan
          _ -> do
            itemVerbMU iid kit (MU.Text $ "appear" <+> ppContainer c) c
            mleader <- getsClient _sleader
            when (Just aid == mleader) $ do
              lastStore <- getsClient slastStore
              let newStore = store : delete store lastStore
              modifyClient $ \cli -> cli { slastSlot = l
                                         , slastStore = newStore }
      CEmbed{} -> return ()
      CFloor{} -> do
        -- If you want an item to be assigned to @slastSlot@, create it
        -- in @CActor aid CGround@, not in @CFloor@.
        void $ updateItemSlot CGround Nothing iid
        itemVerbMU iid kit (MU.Text $ "appear" <+> ppContainer c) c
      CTrunk{} -> assert `failure` c
    void $ stopPlayBack
  UpdDestroyItem iid _ kit c -> itemVerbMU iid kit "disappear" c
  UpdSpotActor aid body _ -> createActorUI aid body verbose "be spotted"
  UpdLoseActor aid body _ ->
    destroyActorUI aid body "be missing in action" "be lost" verbose
  UpdSpotItem iid _ kit c -> do
    (itemSlots, _) <- getsClient sslots
    case lookup iid $ map swap $ EM.assocs itemSlots of
      Nothing ->  -- never seen or would have a slot
        case c of
          CActor aid store ->
            -- Enemy actor fetching an item from shared stash, most probably.
            void $ updateItemSlotSide store aid iid
          CEmbed{} -> return ()
          CFloor lid p -> do
            void $ updateItemSlot CGround Nothing iid
            scursorOld <- getsClient scursor
            case scursorOld of
              TEnemy{} -> return ()  -- probably too important to overwrite
              TEnemyPos{} -> return ()
              _ -> modifyClient $ \cli -> cli {scursor = TPoint lid p}
            itemVerbMU iid kit "be spotted" c
            void $ stopPlayBack
          CTrunk{} -> return ()
      _ -> return ()  -- seen already (has a slot assigned)
  UpdLoseItem{} -> return ()
  -- Move actors and items.
  UpdMoveActor aid source target -> moveActor aid source target
  UpdWaitActor aid _ -> when verbose $ aidVerbMU aid "wait"
  UpdDisplaceActor source target -> displaceActorUI source target
  UpdMoveItem iid k aid c1 c2 -> moveItemUI iid k aid c1 c2
  -- Change actor attributes.
  UpdAgeActor{} -> return ()
  UpdRefillHP _ 0 -> return ()
  UpdRefillHP aid n -> do
    when verbose $
      aidVerbMU aid $ MU.Text $ (if n > 0 then "heal" else "lose")
                                <+> tshow (abs $ n `divUp` oneM) <> "HP"
    mleader <- getsClient _sleader
    when (Just aid == mleader) $ do
      b <- getsState $ getActorBody aid
      hpMax <- sumOrganEqpClient IK.EqpSlotAddMaxHP aid
      when (bhp b >= xM hpMax && hpMax > 0
            && resCurrentTurn (bhpDelta b) > 0) $ do
        actorVerbMU aid b "recover your health fully"
        void $ stopPlayBack
  UpdRefillCalm aid calmDelta ->
    when (calmDelta == minusM) $ do  -- lower deltas come from hits; obvious
      side <- getsClient sside
      b <- getsState $ getActorBody aid
      when (bfid b == side) $ do
        fact <- getsState $ (EM.! bfid b) . sfactionD
        allFoes <- getsState $ actorRegularList (isAtWar fact) (blid b)
        let closeFoes = filter ((<= 3) . chessDist (bpos b) . bpos) allFoes
        when (null closeFoes) $ do  -- obvious where the feeling comes from
          aidVerbMU aid "hear something"
          msgDuplicateScrap
          void $ stopPlayBack
  UpdFidImpressedActor aid _fidOld fidNew -> do
    b <- getsState $ getActorBody aid
    actorVerbMU aid b $
      if | fidNew == bfid b ->
           "get calmed and refocused"
-- TODO: only show for liquids; for others say 'flash', etc.
--              "get refocused by the fragrant moisture"
         | fidNew == bfidOriginal b ->
           "remember forgone allegiance suddenly"
         | otherwise ->
           "experience anxiety that weakens resolve and erodes loyalty"
-- TODO     "inhale the sweet smell that weakens resolve and erodes loyalty"
  UpdTrajectory{} -> return ()
  UpdColorActor{} -> return ()
  -- Change faction attributes.
  UpdQuitFaction fid mbody _ toSt -> quitFactionUI fid mbody toSt
  UpdLeadFaction fid (Just (source, _)) (Just (target, _)) -> do
    side <- getsClient sside
    when (fid == side) $ do
      fact <- getsState $ (EM.! side) . sfactionD
      -- This faction can't run with multiple actors, so this is not
      -- a leader change while running, but rather server changing
      -- their leader, which the player should be alerted to.
      when (noRunWithMulti fact) $
        void $ stopPlayBack
      actorD <- getsState sactorD
      case EM.lookup source actorD of
        Just sb | bhp sb <= 0 -> assert (not $ bproj sb) $ do
          -- Regardless who the leader is, give proper names here, not 'you'.
          tb <- getsState $ getActorBody target
          let subject = partActor tb
              object  = partActor sb
          msgAdd $ makeSentence [ MU.SubjectVerbSg subject "take command"
                                , "from", object ]
        _ ->
          return ()
          -- TODO: report when server changes spawner's leader;
          -- perhaps don't switch _sleader in HandleAtomicClient,
          -- compare here and switch here? too hacky? fails for AI?
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
    when (fid == side) $ setFrontAutoYes b
  UpdRecordKill{} -> return ()
  -- Alter map.
  UpdAlterTile{} -> when verbose $ return ()  -- TODO: door opens
  UpdAlterClear _ k ->
    msgAdd $ if k > 0 then "You hear grinding noises."
                      else "You hear fizzing noises."
  UpdSearchTile aid p fromTile toTile -> do
    Kind.COps{cotile = Kind.Ops{okind}} <- getsState scops
    b <- getsState $ getActorBody aid
    lvl <- getLevel $ blid b
    subject <- partAidLeader aid
    let t = lvl `at` p
        verb | t == toTile = "confirm"
             | otherwise = "reveal"
        subject2 = MU.Text $ TK.tname $ okind fromTile
        verb2 = "be"
    let msg = makeSentence [ MU.SubjectVerbSg subject verb
                           , "that the"
                           , MU.SubjectVerbSg subject2 verb2
                           , "a hidden"
                           , MU.Text $ TK.tname $ okind toTile ]
    msgAdd msg
  UpdLearnSecrets{} -> return ()
  UpdSpotTile{} -> return ()
  UpdLoseTile{} -> return ()
  UpdAlterSmell{} -> return ()
  UpdSpotSmell{} -> return ()
  UpdLoseSmell{} -> return ()
  -- Assorted.
  UpdTimeItem{} -> return ()
  UpdAgeGame{} -> return ()
  UpdDiscover c iid _ _ _ -> discover c oldStateClient iid
  UpdCover{} -> return ()  -- don't spam when doing undo
  UpdDiscoverKind c iid _ -> discover c oldStateClient iid
  UpdCoverKind{} -> return ()  -- don't spam when doing undo
  UpdDiscoverSeed c iid _ _ -> discover c oldStateClient iid
  UpdCoverSeed{} -> return ()  -- don't spam when doing undo
  UpdPerception{} -> return ()
  UpdRestart fid _ _ _ _ _ -> do
    mode <- getGameMode
    msgAdd $ "New game started in" <+> mname mode <+> "mode." <+> mdesc mode
    -- TODO: use a vertical animation instead, e.g., roll down,
    -- and reveal the first frame of a new game, not blank screen.
    history <- getsSession shistory
    when (lengthHistory history > 1) $ fadeOutOrIn False
    fact <- getsState $ (EM.! fid) . sfactionD
    setFrontAutoYes $ isAIFact fact
  UpdRestartServer{} -> return ()
  UpdResume fid _ -> do
    fact <- getsState $ (EM.! fid) . sfactionD
    setFrontAutoYes $ isAIFact fact
  UpdResumeServer{} -> return ()
  UpdKillExit{} -> return ()
  UpdWriteSave -> when verbose $ msgAdd "Saving backup."
  UpdMsgAll msg -> msgAdd msg
  UpdRecordHistory _ -> recordHistory

updateItemSlotSide :: MonadClient m
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
  tgtMode <- getsSession stgtMode
  when (not (bproj body)
        && bfid body == side
        && isNothing tgtMode) $ do  -- targeting does a more extensive look
    lookMsg <- lookAt False "" True (bpos body) aid ""
    msgAdd lookMsg
  fact <- getsState $ (EM.! bfid body) . sfactionD
  if not (bproj body) && side == bfid body then do
    foes <- getsState $ actorList (isAtWar fact) (blid body)
    when (any (adjacent (bpos body) . bpos) foes) $
      void $ stopPlayBack
  else when (isAtWar fact side) $ do
    friends <- getsState $ actorRegularList (== side) (blid body)
    when (any (adjacent (bpos body) . bpos) friends) $
      void $ stopPlayBack

-- | Sentences such as \"Dog barks loudly.\".
actorVerbMU :: MonadClientUI m => ActorId -> Actor -> MU.Part -> m ()
actorVerbMU aid b verb = do
  subject <- partActorLeader aid b
  msgAdd $ makeSentence [MU.SubjectVerbSg subject verb]

aidVerbMU :: MonadClientUI m => ActorId -> MU.Part -> m ()
aidVerbMU aid verb = do
  b <- getsState $ getActorBody aid
  actorVerbMU aid b verb

itemVerbMU :: MonadClientUI m
           => ItemId -> ItemQuant -> MU.Part -> Container -> m ()
itemVerbMU iid kit@(k, _) verb c = assert (k > 0) $ do
  lid <- getsState $ lidFromC c
  localTime <- getsState $ getLocalTime lid
  itemToF <- itemToFullClient
  let subject = partItemWs k (storeFromC c) localTime (itemToF iid kit)
      msg | k > 1 = makeSentence [MU.SubjectVerb MU.PlEtc MU.Yes subject verb]
          | otherwise = makeSentence [MU.SubjectVerbSg subject verb]
  msgAdd msg

-- TODO: split into 3 parts wrt ek and reuse somehow, e.g., the secret part
-- We assume the item is inside the specified container.
-- So, this function can't be used for, e.g., @UpdDestroyItem@.
itemAidVerbMU :: MonadClientUI m
              => ActorId -> MU.Part
              -> ItemId -> Either (Maybe Int) Int -> CStore
              -> m ()
itemAidVerbMU aid verb iid ek cstore = do
  bag <- getsState $ getActorBag aid cstore
  -- The item may no longer be in @c@, but it was
  case iid `EM.lookup` bag of
    Nothing -> assert `failure` (aid, verb, iid, cstore)
    Just kit@(k, _) -> do
      itemToF <- itemToFullClient
      body <- getsState $ getActorBody aid
      let lid = blid body
      localTime <- getsState $ getLocalTime lid
      subject <- partAidLeader aid
      let itemFull = itemToF iid kit
          object = case ek of
            Left (Just n) ->
              assert (n <= k `blame` (aid, verb, iid, cstore))
              $ partItemWs n cstore localTime itemFull
            Left Nothing ->
              let (_, name, stats) = partItem cstore localTime itemFull
              in MU.Phrase [name, stats]
            Right n ->
              assert (n <= k `blame` (aid, verb, iid, cstore))
              $ let itemSecret = itemNoDisco (itemBase itemFull, n)
                    (_, secretName, secretAE) = partItem cstore localTime itemSecret
                    name = MU.Phrase [secretName, secretAE]
                    nameList = if n == 1
                               then ["the", name]
                               else ["the", MU.Text $ tshow n, MU.Ws name]
                in MU.Phrase nameList
          msg = makeSentence [MU.SubjectVerbSg subject verb, object]
      msgAdd msg

msgDuplicateScrap :: MonadClientUI m => m ()
msgDuplicateScrap = do
  report <- getsSession sreport
  history <- getsSession shistory
  let (lastMsg, repRest) = lastMsgOfReport report
      lastDup = isJust . findInReport (== lastMsg)
      lastDuplicated = lastDup repRest
                       || maybe False lastDup (lastReportOfHistory history)
  when lastDuplicated $
    modifySession $ \sess -> sess {sreport = repRest}

-- TODO: "XXX spots YYY"? or blink or show the changed cursor?
createActorUI :: MonadClientUI m
              => ActorId -> Actor -> Bool -> MU.Part -> m ()
createActorUI aid body verbose verb = do
  mapM_ (\(iid, store) -> void $ updateItemSlotSide store aid iid)
        (getCarriedIidCStore body)
  side <- getsClient sside
  when (bfid body /= side) $ do
    fact <- getsState $ (EM.! bfid body) . sfactionD
    when (not (bproj body) && isAtWar fact side) $
      -- Target even if nobody can aim at the enemy. Let's home in on him
      -- and then we can aim or melee. We set permit to False, because it's
      -- technically very hard to check aimability here, because we are
      -- in-between turns and, e.g., leader's move has not yet been taken
      -- into account.
      modifyClient $ \cli -> cli {scursor = TEnemy aid False}
    void $ stopPlayBack
  -- Don't spam if the actor was already visible (but, e.g., on a tile that is
  -- invisible this turn (in that case move is broken down to lose+spot)
  -- or on a distant tile, via teleport while the observer teleported, too).
  lastLost <- getsSession slastLost
  when (ES.notMember aid lastLost
        && (not (bproj body) || verbose)) $ do
    actorVerbMU aid body verb
    animFrs <- animate (blid body) $ actorX (bpos body)
    displayActorStart body animFrs
  lookAtMove aid

destroyActorUI :: MonadClientUI m
               => ActorId -> Actor -> MU.Part -> MU.Part -> Bool -> m ()
destroyActorUI aid body verb verboseVerb verbose = do
  Kind.COps{corule} <- getsState scops
  side <- getsClient sside
  when (bfid body == side) $ do
    let upd = ES.delete aid
    modifySession $ \sess -> sess {sselected = upd $ sselected sess}
  if bfid body == side && bhp body <= 0 && not (bproj body) then do
    when verbose $ actorVerbMU aid body verb
    let firstDeathEnds = rfirstDeathEnds $ Kind.stdRuleset corule
        fid = bfid body
    fact <- getsState $ (EM.! fid) . sfactionD
    actorsAlive <- anyActorsAlive fid (Just aid)
    -- TODO: deduplicate wrt Server
    -- TODO; actually show the --more- prompt, but not between fadeout frames
    unless (fneverEmpty (gplayer fact)
            && (not actorsAlive || firstDeathEnds)) $
      void $ displayMore ColorBW ""
  else when verbose $ actorVerbMU aid body verboseVerb
  -- If pushed, animate spotting again, to draw attention to pushing.
  when (isNothing $ btrajectory body) $
    modifySession $ \sess -> sess {slastLost = ES.insert aid $ slastLost sess}

-- TODO: deduplicate wrt Server
anyActorsAlive :: MonadClient m => FactionId -> Maybe ActorId -> m Bool
anyActorsAlive fid maid = do
  fact <- getsState $ (EM.! fid) . sfactionD
  if fleaderMode (gplayer fact) /= LeaderNull
    then return $! isJust $ gleader fact
    else do
      as <- getsState $ fidActorNotProjAssocs fid
      return $! not $ null $ maybe as (\aid -> filter ((/= aid) . fst) as) maid

moveActor :: MonadClientUI m => ActorId -> Point -> Point -> m ()
moveActor aid source target = do
  -- If source and target tile distant, assume it's a teleportation
  -- and display an animation. Note: jumps and pushes go through all
  -- intervening tiles, so won't be considered. Note: if source or target
  -- not seen, the (half of the) animation would be boring, just a delay,
  -- not really showing a transition, so we skip it (via 'breakUpdAtomic').
  -- The message about teleportation is sometimes shown anyway, just as the X.
  if adjacent source target
  then actorMoveDisplay aid
  else do
    body <- getsState $ getActorBody aid
    let ps = (source, target)
    animFrs <- animate (blid body) $ teleport ps
    displayActorStart body animFrs
    lookAtMove aid

displaceActorUI :: MonadClientUI m => ActorId -> ActorId -> m ()
displaceActorUI source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  spart <- partActorLeader source sb
  tpart <- partActorLeader target tb
  let msg = makeSentence [MU.SubjectVerbSg spart "displace", tpart]
  msgAdd msg
  when (bfid sb /= bfid tb) $ do
    lookAtMove source
    lookAtMove target
  let ps = (bpos tb, bpos sb)
  animFrs <- animate (blid sb) $ swapPlaces ps
  displayActorStart sb animFrs

moveItemUI :: MonadClientUI m
           => ItemId -> Int -> ActorId -> CStore -> CStore
           -> m ()
moveItemUI iid k aid cstore1 cstore2 = do
  let verb = verbCStore cstore2
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  let underAI = isAIFact fact
  mleader <- getsClient _sleader
  bag <- getsState $ getActorBag aid cstore2
  let kit = bag EM.! iid
  itemToF <- itemToFullClient
  (itemSlots, _) <- getsClient sslots
  let itemFull = itemToF iid kit
  case lookup iid $ map swap $ EM.assocs itemSlots of
    Just l -> do
      when (Just aid == mleader) $ do
        lastStore <- getsClient slastStore
        let newStore = cstore2 : cstore1
                       : delete cstore2 (delete cstore1 lastStore)
        modifyClient $ \cli -> cli { slastSlot = l
                                   , slastStore = newStore }
      if cstore1 == CGround && Just aid == mleader && not underAI then
        itemAidVerbMU aid (MU.Text verb) iid (Right k) cstore2
      else when (not (bproj b) && bhp b > 0) $  -- don't announce death drops
        itemAidVerbMU aid (MU.Text verb) iid (Left $ Just k) cstore2
    Nothing -> assert `failure` (iid, itemFull)

quitFactionUI :: MonadClientUI m
              => FactionId -> Maybe Actor -> Maybe Status -> m ()
quitFactionUI fid mbody toSt = do
  Kind.COps{coitem=Kind.Ops{okind, ouniqGroup}} <- getsState scops
  fact <- getsState $ (EM.! fid) . sfactionD
  let fidName = MU.Text $ gname fact
      horror = isHorrorFact fact
  side <- getsClient sside
  let msgIfSide _ | fid /= side = Nothing
      msgIfSide s = Just s
      (startingPart, partingPart) = case toSt of
        _ | horror ->
          (Nothing, Nothing)  -- Ignore summoned actors' factions.
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
          assert `failure` (fid, mbody, toSt)
        Nothing ->
          (Nothing, Nothing)  -- Wipe out the quit flag for the savegame files.
  case startingPart of
    Nothing -> return ()
    Just sp -> do
      let msg = makeSentence [MU.SubjectVerbSg fidName sp]
      msgAdd msg
  case (toSt, partingPart) of
    (Just status, Just pp) -> do
      startingSlide <- promptToSlideshow moreMsg
      recordHistory  -- we are going to exit or restart, so record
      let bodyToItemSlides b = do
            (bag, tot) <- getsState $ calculateTotal b
            let currencyName = MU.Text $ IK.iname $ okind
                               $ ouniqGroup "currency"
                itemMsg = makeSentence [ "Your loot is worth"
                                       , MU.CarWs tot currencyName ]
                          <+> moreMsg
            if EM.null bag then return (mempty, 0)
            else do
              io <- itemOverlay CGround (blid b) bag
              -- TODO: treat as menu and display item description
              sli <- overlayToSlideshow itemMsg $ fst io
              return (sli, tot)
      (itemSlides, total) <- case mbody of
        Just b | fid == side -> bodyToItemSlides b
        _ -> case gleader fact of
          Nothing -> return (mempty, 0)
          Just (aid, _) -> do
            b <- getsState $ getActorBody aid
            bodyToItemSlides b
      -- Show score for any UI client (except after ESC),
      -- even though it is saved only for human UI clients.
      scoreSlides <- scoreToSlideshow total status
      partingSlide <- promptToSlideshow $ pp <+> moreMsg
      -- TODO: First ESC cancels items display.
      void $ getConfirms ColorFull [K.spaceKM] [K.escKM]
           $ startingSlide <> itemSlides
      -- TODO: Second ESC cancels high score and parting message display.
      -- The last slide stays onscreen during shutdown, etc.
             <> scoreSlides <> partingSlide
      -- TODO: perhaps use a vertical animation instead, e.g., roll down
      -- and put it before item and score screens (on blank background)
      unless (fmap stOutcome toSt == Just Camping) $ do
        msgAdd pp  -- TODO: mark for non-inclusion in history
        fadeOutOrIn True
    _ -> return ()

discover :: MonadClientUI m
         => Container -> StateClient -> ItemId -> m ()
discover c oldcli iid = do
  let cstore = storeFromC c
  lid <- getsState $ lidFromC c
  cops <- getsState scops
  localTime <- getsState $ getLocalTime lid
  itemToF <- itemToFullClient
  bag <- getsState $ getCBag c
  let kit = EM.findWithDefault (1, []) iid bag
      itemFull = itemToF iid kit
      knownName = partItemMediumAW cstore localTime itemFull
      -- Wipe out the whole knowledge of the item to make sure the two names
      -- in the message differ even if, e.g., the item is described as
      -- "of many effects".
      itemSecret = itemNoDisco (itemBase itemFull, itemK itemFull)
      (_, secretName, secretAEText) = partItem cstore localTime itemSecret
      msg = makeSentence
        [ "the", MU.SubjectVerbSg (MU.Phrase [secretName, secretAEText])
                                  "turn out to be"
        , knownName ]
      oldItemFull =
        itemToFull cops (sdiscoKind oldcli) (sdiscoEffect oldcli)
                   iid (itemBase itemFull) (1, [])
  -- Compare descriptions of all aspects and effects to determine
  -- if the discovery was meaningful to the player.
  when (textAllAE 7 False cstore itemFull
        /= textAllAE 7 False cstore oldItemFull) $
    msgAdd msg

-- * RespSfxAtomicUI

-- | Display special effects (text, animation) sent to the client.
displayRespSfxAtomicUI :: MonadClientUI m => Bool -> SfxAtomic -> m ()
displayRespSfxAtomicUI verbose sfx = case sfx of
  SfxStrike source target iid cstore b -> strike source target iid cstore b
  SfxRecoil source target _ _ _ -> do
    spart <- partAidLeader source
    tpart <- partAidLeader target
    msgAdd $ makeSentence [MU.SubjectVerbSg spart "shrink away from", tpart]
  SfxProject aid iid cstore -> do
    setLastSlot aid iid cstore
    itemAidVerbMU aid "aim" iid (Left $ Just 1) cstore
  SfxCatch aid iid cstore ->
    itemAidVerbMU aid "catch" iid (Left $ Just 1) cstore
  SfxApply aid iid cstore -> do
    setLastSlot aid iid cstore
    itemAidVerbMU aid "apply" iid (Left $ Just 1) cstore
  SfxCheck aid iid cstore ->
    itemAidVerbMU aid "deapply" iid (Left $ Just 1) cstore
  SfxTrigger aid _p _feat ->
    -- TODO: when more triggers that are not visible on the map, add msgs
    when verbose $ aidVerbMU aid "trigger"  -- TODO: opens door, etc.
  SfxShun aid _p _ ->
    when verbose $ aidVerbMU aid "shun"  -- TODO: shuns stairs down
  SfxEffect fidSource aid effect -> do
    b <- getsState $ getActorBody aid
    side <- getsClient sside
    let fid = bfid b
    if bhp b <= 0 then do
      -- We assume the effect is the cause of incapacitation, but in case
      -- of projectile, to reduce spam, we verify with @canKill@.
      let firstFall | fid == side && bproj b = "fall apart"
                    | fid == side = "fall down"
                    | bproj b = "break up"
                    | otherwise = "collapse"
          hurtExtra | fid == side && bproj b = "be reduced to dust"
                    | fid == side = "be stomped flat"
                    | bproj b = "be shattered into little pieces"
                    | otherwise = "be reduced to a bloody pulp"
          -- Aspect bonuses ignored, so hurtExtra will add variety sometimes.
          deadPreviousTurn dp = bhp b <= dp
          harm2 dp = if deadPreviousTurn dp
                     then (True, Just hurtExtra)
                     else (False, Just firstFall)
          (deadBefore, mverbDie) =
            case effect of
              IK.Hurt p -> harm2 (- (xM $ Dice.maxDice p))
              IK.RefillHP p | p < 0 -> harm2 (xM p)
              IK.OverfillHP p | p < 0 -> harm2 (xM p)
              IK.Burn p -> harm2 (- (xM $ Dice.maxDice p))
              _ -> (False, Nothing)
      case mverbDie of
        Nothing -> return ()  -- only brutal effects work on dead/dying actor
        Just verbDie -> do
          subject <- partActorLeader aid b
          let msgDie = makeSentence [MU.SubjectVerbSg subject verbDie]
          msgAdd msgDie
          when (fid == side && not (bproj b)) $ do
            animDie <- animate (blid b) $
              if deadBefore
              then twirlSplash (bpos b, bpos b) Color.Red Color.Red
              else deathBody $ bpos b
            displayActorStart b animDie
    else case effect of
        IK.ELabel{} -> return ()
        IK.Hurt{} -> return ()  -- avoid spam; SfxStrike just sent
        IK.Burn{} -> do
          if fid == side then
            actorVerbMU aid b "feel burned"
          else
            actorVerbMU aid b "look burned"
          let ps = (bpos b, bpos b)
          animFrs <- animate (blid b) $ twirlSplash ps Color.BrRed Color.Red
          displayActorStart b animFrs
        IK.Explode{} -> return ()  -- lots of visual feedback
        IK.RefillHP p | p == 1 -> return ()  -- no spam from regeneration
        IK.RefillHP p | p > 0 -> do
          if fid == side then
            actorVerbMU aid b "feel healthier"
          else
            actorVerbMU aid b "look healthier"
          let ps = (bpos b, bpos b)
          animFrs <- animate (blid b) $ twirlSplash ps Color.BrBlue Color.Blue
          displayActorStart b animFrs
        IK.RefillHP p | p == -1 -> return ()  -- no spam from poison
        IK.RefillHP _ -> do
          if fid == side then
            actorVerbMU aid b "feel wounded"
          else
            actorVerbMU aid b "look wounded"
          let ps = (bpos b, bpos b)
          animFrs <- animate (blid b) $ twirlSplash ps Color.BrRed Color.Red
          displayActorStart b animFrs
        IK.OverfillHP p | p > 0 -> do
          if fid == side then
            actorVerbMU aid b "feel healthier"
          else
            actorVerbMU aid b "look healthier"
          let ps = (bpos b, bpos b)
          animFrs <- animate (blid b) $ twirlSplash ps Color.BrBlue Color.Blue
          displayActorStart b animFrs
        IK.OverfillHP _ -> do
          if fid == side then
            actorVerbMU aid b "feel wounded"
          else
            actorVerbMU aid b "look wounded"
          let ps = (bpos b, bpos b)
          animFrs <- animate (blid b) $ twirlSplash ps Color.BrRed Color.Red
          displayActorStart b animFrs
        IK.RefillCalm p | p == 1 -> return ()  -- no spam from regen items
        IK.RefillCalm p | p > 0 -> do
          if fid == side then
            actorVerbMU aid b "feel calmer"
          else
            actorVerbMU aid b "look calmer"
        IK.RefillCalm _ -> do
          if fid == side then
            actorVerbMU aid b "feel agitated"
          else
            actorVerbMU aid b "look agitated"
        IK.OverfillCalm p | p > 0 -> do
          if fid == side then
            actorVerbMU aid b "feel calmer"
          else
            actorVerbMU aid b "look calmer"
        IK.OverfillCalm _ -> do
          if fid == side then
            actorVerbMU aid b "feel agitated"
          else
            actorVerbMU aid b "look agitated"
        IK.Dominate -> do
          -- For subsequent messages use the proper name, never "you".
          let subject = partActor b
          if fid /= fidSource then do  -- before domination
            if | bcalm b == 0 ->  -- sometimes only a coincidence, but nm
                 aidVerbMU aid $ MU.Text "yield, under extreme pressure"
               | fid == side ->
                 aidVerbMU aid $ MU.Text "black out, dominated by foes"
               | otherwise ->
                 aidVerbMU aid $ MU.Text "decide abrubtly to switch allegiance"
            fidName <- getsState $ gname . (EM.! fid) . sfactionD
            let verb = "be no longer controlled by"
            msgAdd $ makeSentence
              [MU.SubjectVerbSg subject verb, MU.Text fidName]
            when (fid == side) $ void $ displayMore ColorFull ""
          else do
            fidSourceName <- getsState $ gname . (EM.! fidSource) . sfactionD
            let verb = "be now under"
            msgAdd $ makeSentence
              [MU.SubjectVerbSg subject verb, MU.Text fidSourceName, "control"]
          void $ stopPlayBack
        IK.Impress -> return ()
        IK.CallFriend{} -> do
          let verb = if bproj b then "attract" else "call forth"
          actorVerbMU aid b $ MU.Text $ verb <+> "friends"
        IK.Summon{} -> do  -- TODO: if a singleton, use the freq?
          let verb = if bproj b then "lure" else "summon"
          actorVerbMU aid b $ MU.Text $ verb <+> "nearby beasts"
        IK.Ascend k | k > 0 -> actorVerbMU aid b "find a way upstairs"
        IK.Ascend k | k < 0 -> actorVerbMU aid b "find a way downstairs"
        IK.Ascend{} -> assert `failure` sfx
        IK.Escape{} -> return ()
        IK.Paralyze{} -> actorVerbMU aid b "be paralyzed"
        IK.InsertMove{} -> actorVerbMU aid b "act with extreme speed"
        IK.Teleport t | t > 9 -> actorVerbMU aid b "teleport"
        IK.Teleport{} -> actorVerbMU aid b "blink"
        IK.CreateItem{} -> return ()
        IK.DropItem COrgan _ True -> return ()
        IK.DropItem _ _ False -> actorVerbMU aid b "be stripped"  -- TODO
        IK.DropItem _ _ True -> actorVerbMU aid b "be violently stripped"
        IK.PolyItem -> do
          localTime <- getsState $ getLocalTime $ blid b
          allAssocs <- fullAssocsClient aid [CGround]
          case allAssocs of
            [] -> return ()  -- invisible items?
            (_, ItemFull{..}) : _ -> do
              subject <- partActorLeader aid b
              let itemSecret = itemNoDisco (itemBase, itemK)
                  -- TODO: plural form of secretName? only when K > 1?
                  -- At this point we don't easily know how many consumed.
                  (_, secretName, secretAEText) = partItem CGround localTime itemSecret
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
              subject <- partActorLeader aid b
              let verb = "inspect"
                  store = MU.Text $ ppCStoreIn CGround
              msgAdd $ makeSentence
                [ MU.SubjectVerbSg subject verb
                , "an item", store ]
        IK.SendFlying{} -> actorVerbMU aid b "be sent flying"
        IK.PushActor{} -> actorVerbMU aid b "be pushed"
        IK.PullActor{} -> actorVerbMU aid b "be pulled"
        IK.DropBestWeapon -> actorVerbMU aid b "be disarmed"
        IK.ActivateInv{} -> return ()
        IK.ApplyPerfume ->
          msgAdd "The fragrance quells all scents in the vicinity."
        IK.OneOf{} -> return ()
        IK.OnSmash{} -> assert `failure` sfx
        IK.Recharging{} -> assert `failure` sfx
        IK.Temporary t -> actorVerbMU aid b $ MU.Text t
  SfxMsgFid _ msg -> msgAdd msg
  SfxMsgAll msg -> msgAdd msg
  SfxActorStart aid -> do
    -- This is always sent before the state-changing commands.
    arena <- getArenaUI
    b <- getsState $ getActorBody aid
    when (blid b == arena) $
      actorDisplay aid

actorDisplay :: MonadClientUI m => ActorId -> m ()
actorDisplay aid = do
  mleader <- getsClient _sleader
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  -- If key will be requested, don't show the frame, because during
  -- the request extra message may be shown, so the other frame is better.
  when (Just aid /= mleader || isAIFact fact) $ do
    -- Display the new game state.
    localTime <- getsState $ getLocalTime (blid b)
    displayPush ""
    -- Set display time to local time, which is quantized by the server.
    -- Consequently, time-based displays are performed always
    -- at server-determined moments, at most as often, as server ages time.
    -- Performing extra displays at other moments doesn't reset that.
    -- We could the skip next closest scheduled display, but more frames
    -- for extra events is better and also, we'd need more acurate timing.
    let ageDisp = EM.insert (blid b) localTime
    modifySession $ \sess -> sess {sdisplayed = ageDisp $ sdisplayed sess}

actorMoveDisplay :: MonadClientUI m => ActorId -> m ()
actorMoveDisplay aid = do
  -- TODO: handle projectiles and insert *delay* between the same actor's move
  arena <- getArenaUI
  b <- getsState $ getActorBody aid
  when (blid b == arena) $ do
    -- If any time has passed (server quantizes this)
    -- since any actor advanced @sdisplayed@ or if the actor is newborn,
    -- we end and display the frame early, before his next move.
    -- In the result, he moves at most once per frame and thanks to this,
    -- his multiple moves are not collapsed into one frame.
    -- The exception is when it moves faster than one meter per clip,
    -- which is 10 times normal speed, and then some move frames are not shown.
    -- Otherwise lots of extremely fast bullets would freeze the game.
    -- Note that if the actor just displayed an animation (e.g., via
    -- displacement animation), @sdisplayed@ is updated and so no display here.
    timeDisp <- getsSession $ EM.findWithDefault timeZero arena . sdisplayed
    localTime <- getsState $ getLocalTime (blid b)
    when (localTime > timeDisp || actorNewBorn b) $
      actorDisplay aid

setLastSlot :: MonadClientUI m => ActorId -> ItemId -> CStore -> m ()
setLastSlot aid iid cstore = do
  mleader <- getsClient _sleader
  when (Just aid == mleader) $ do
    (itemSlots, _) <- getsClient sslots
    lastStore <- getsClient slastStore
    let newStore = cstore : delete cstore lastStore
    case lookup iid $ map swap $ EM.assocs itemSlots of
      Just l -> modifyClient $ \cli -> cli { slastSlot = l
                                           , slastStore = newStore }
      Nothing -> assert `failure` (iid, cstore, aid)

strike :: MonadClientUI m
       => ActorId -> ActorId -> ItemId -> CStore -> HitAtomic -> m ()
strike source target iid cstore hitStatus = assert (source /= target) $ do
  itemToF <- itemToFullClient
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  spart <- partActorLeader source sb
  tpart <- partActorLeader target tb
  spronoun <- partPronounLeader source sb
  localTime <- getsState $ getLocalTime (blid sb)
  bag <- getsState $ getActorBag source cstore
  let kit = EM.findWithDefault (1, []) iid bag
      itemFull = itemToF iid kit
      verb = case itemDisco itemFull of
        Nothing -> "hit"  -- not identified
        Just ItemDisco{itemKind} -> IK.iverbHit itemKind
      isOrgan = iid `EM.member` borgan sb
      partItemChoice =
        if isOrgan
        then partItemWownW spronoun COrgan localTime
        else partItemAW cstore localTime
      msg HitClear = makeSentence $
        [MU.SubjectVerbSg spart verb, tpart]
        ++ if bproj sb
           then []
           else ["with", partItemChoice itemFull]
      msg (HitBlock n) =
        -- This sounds funny when the victim falls down immediately,
        -- but there is no easy way to prevent that. And it's consistent.
        -- If/when death blow instead sets HP to 1 and only the next below 1,
        -- we can check here for HP==1; also perhaps actors with HP 1 should
        -- not be able to block.
        let sActs =
              if bproj sb
              then [ MU.SubjectVerbSg spart "connect" ]
              else [ MU.SubjectVerbSg spart "swing"
                   , partItemChoice itemFull ]
        in makeSentence [ MU.Phrase sActs <> ", but"
                        , MU.SubjectVerbSg tpart "block"
                        , if n > 1 then "doggedly" else "partly"
                        ]
-- TODO: when other armor is in, etc.:
--      msg HitSluggish =
--        let adv = MU.Phrase ["sluggishly", verb]
--        in makeSentence $ [MU.SubjectVerbSg spart adv, tpart]
--                          ++ ["with", partItemChoice itemFull]
  msgAdd $ msg hitStatus
  let ps = (bpos tb, bpos sb)
      anim HitClear = twirlSplash ps Color.BrRed Color.Red
      anim (HitBlock 1) = blockHit ps Color.BrRed Color.Red
      anim (HitBlock _) = blockMiss ps
  animFrs <- animate (blid sb) $ anim hitStatus
  displayActorStart sb animFrs
