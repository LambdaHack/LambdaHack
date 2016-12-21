-- | Display atomic commands received by the client.
module Game.LambdaHack.Client.UI.DisplayAtomicM
  ( displayRespUpdAtomicUI, displayRespSfxAtomicUI
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.IntMap.Strict as IM
import Data.Tuple
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.CommonM
import Game.LambdaHack.Client.ItemSlot
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.FrameM
import Game.LambdaHack.Client.UI.HandleHelperM
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
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemDescription
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
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
{-# INLINE displayRespUpdAtomicUI #-}
displayRespUpdAtomicUI verbose oldCli cmd = case cmd of
  -- Create/destroy actors and items.
  UpdCreateActor aid body _ -> createActorUI True aid body
  UpdDestroyActor aid body _ -> do
    destroyActorUI True aid body
    side <- getsClient sside
    when (bfid body == side && not (bproj body)) $
      stopPlayBack
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
              !lastStore <- getsClient slastStore
              let newStore = store : delete store lastStore
              modifyClient $ \cli -> cli { slastSlot = l
                                         , slastStore = newStore }
      CEmbed lid _ -> markDisplayNeeded lid
      CFloor lid _ -> do
        -- If you want an item to be assigned to @slastSlot@, create it
        -- in @CActor aid CGround@, not in @CFloor@.
        void $ updateItemSlot CGround Nothing iid
        itemVerbMU iid kit (MU.Text $ "appear" <+> ppContainer c) c
        markDisplayNeeded lid
      CTrunk{} -> assert `failure` c
    stopPlayBack
  UpdDestroyItem iid _ kit c -> do
    itemVerbMU iid kit "disappear" c
    lid <- getsState $ lidFromC c
    markDisplayNeeded lid
  UpdSpotActor aid body _ -> createActorUI False aid body
  UpdLoseActor aid body _ -> destroyActorUI False aid body
  UpdSpotItem iid _ kit c -> do
    -- This is due to a move, or similar, which will be displayed,
    -- so no extra @markDisplayNeeded@ needed here and in similar places.
    ItemSlots itemSlots _ <- getsClient sslots
    case lookup iid $ map swap $ EM.assocs itemSlots of
      Nothing ->  -- never seen or would have a slot
        case c of
          CActor aid store ->
            -- Most probably an actor putting item in or out of shared stash.
            void $ updateItemSlotSide store aid iid
          CEmbed{} -> return ()
          CFloor lid p -> do
            void $ updateItemSlot CGround Nothing iid
            sxhairOld <- getsClient sxhair
            case sxhairOld of
              TEnemy{} -> return ()  -- probably too important to overwrite
              TEnemyPos{} -> return ()
              _ -> modifyClient $ \cli -> cli {sxhair = TPoint lid p}
            itemVerbMU iid kit "be spotted" c
            stopPlayBack
          CTrunk{} -> return ()
      _ -> return ()  -- seen already (has a slot assigned)
    case c of
      CActor aid store | store `elem` [CEqp, CInv] -> do
        -- Actor fetching an item from shared stash, most probably.
        b <- getsState $ getActorBody aid
        subject <- partActorLeader aid b
        let noun = MU.WownW subject (MU.Text $ snd $ ppCStore store)
            verb = MU.Text $ makePhrase ["be added to", noun]
        itemVerbMU iid kit verb c
      _ -> return ()
  UpdLoseItem iid _ kit c@(CActor aid store) | store /= CSha -> do
    -- Actor putting an item into shared stash, most probably.
    b <- getsState $ getActorBody aid
    subject <- partActorLeader aid b
    let noun = MU.WownW subject (MU.Text $ snd $ ppCStore store)
        verb = MU.Text $ makePhrase ["be removed from", noun]
    itemVerbMU iid kit verb c
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
                                <+> tshow (abs $ n `divUp` oneM) <> "HP"
    mleader <- getsClient _sleader
    when (Just aid == mleader) $ do
      b <- getsState $ getActorBody aid
      actorAspect <- getsClient sactorAspect
      let ar = case EM.lookup aid actorAspect of
            Just aspectRecord -> aspectRecord
            Nothing -> assert `failure` aid
      when (bhp b >= xM (aMaxHP ar) && aMaxHP ar > 0
            && resCurrentTurn (bhpDelta b) > 0) $ do
        actorVerbMU aid b "recover your health fully"
        stopPlayBack
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
          stopPlayBack
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
  UpdTrajectory{} -> return ()  -- if projectile dies here, no display
  UpdColorActor aid fromCol toCol -> do
    -- If color changed, make sure it's ever shown,
    -- e.g., before projectile dies.
    b <- getsState $ getActorBody aid
    arena <- getArenaUI
    -- Ignore actors on other levels.
    when (arena == blid b) $
      animate (blid b) $ blinkColorActor (bpos b) (bsymbol b) fromCol toCol
  -- Change faction attributes.
  UpdQuitFaction fid mbody _ toSt -> quitFactionUI fid mbody toSt
  UpdLeadFaction fid (Just source) (Just target) -> do
    side <- getsClient sside
    when (fid == side) $ do
      fact <- getsState $ (EM.! side) . sfactionD
      -- This faction can't run with multiple actors, so this is not
      -- a leader change while running, but rather server changing
      -- their leader, which the player should be alerted to.
      when (noRunWithMulti fact) $
        stopPlayBack
      actorD <- getsState sactorD
      case EM.lookup source actorD of
        Just sb | bhp sb <= 0 -> assert (not $ bproj sb) $ do
          -- Regardless who the leader is, give proper names here, not 'you'.
          tb <- getsState $ getActorBody target
          let subject = partActor tb
              object  = partActor sb
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
    when (fid == side) $ setFrontAutoYes b
  UpdRecordKill{} -> return ()
  -- Alter map.
  UpdAlterTile lid _ _ _ -> markDisplayNeeded lid
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
  UpdDiscover c iid _ _ _ -> discover c oldCli iid
  UpdCover{} -> return ()  -- don't spam when doing undo
  UpdDiscoverKind c iid _ -> discover c oldCli iid
  UpdCoverKind{} -> return ()  -- don't spam when doing undo
  UpdDiscoverSeed c iid _ _ -> discover c oldCli iid
  UpdCoverSeed{} -> return ()  -- don't spam when doing undo
  UpdPerception{} -> return ()
  UpdRestart fid _ _ _ _ _ -> do
    sstart <- getsSession sstart
    when (sstart == 0) $ resetSessionStart
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
    msgAdd $ "New game started in" <+> mname mode <+> "mode." <+> mdesc mode
    when (lengthHistory history > 1) $ fadeOutOrIn False
    fact <- getsState $ (EM.! fid) . sfactionD
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
      promptAdd $ mdesc mode <+> "Are you up for the challenge?"
      slides <- reportToSlideshow [K.spaceKM, K.escKM]
      km <- getConfirms ColorFull [K.spaceKM, K.escKM] slides
      if km == K.escKM then addPressedEsc else promptAdd "Prove yourself!"
  UpdResumeServer{} -> return ()
  UpdKillExit{} -> frontendShutdown
  UpdWriteSave -> when verbose $ promptAdd "Saving backup."
  UpdMsgAll msg -> msgAdd msg
  UpdRecordHistory _ -> recordHistory

markDisplayNeeded :: MonadClientUI m => LevelId -> m ()
{-# INLINABLE markDisplayNeeded #-}
markDisplayNeeded lid = do
  lidV <- viewedLevelUI
  when (lidV == lid) $
     modifySession $ \sess -> sess {sdisplayNeeded = True}

updateItemSlotSide :: MonadClient m
                   => CStore -> ActorId -> ItemId -> m SlotChar
{-# INLINABLE updateItemSlotSide #-}
updateItemSlotSide store aid iid = do
  side <- getsClient sside
  b <- getsState $ getActorBody aid
  if bfid b == side
  then updateItemSlot store (Just aid) iid
  else updateItemSlot store Nothing iid

lookAtMove :: MonadClientUI m => ActorId -> m ()
{-# INLINABLE lookAtMove #-}
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
  if not (bproj body) && side == bfid body then do
    foes <- getsState $ actorList (isAtWar fact) (blid body)
    when (any (adjacent (bpos body) . bpos) foes) $
      stopPlayBack
  else when (isAtWar fact side) $ do
    friends <- getsState $ actorRegularList (== side) (blid body)
    when (any (adjacent (bpos body) . bpos) friends) $
      stopPlayBack

-- | Sentences such as \"Dog barks loudly.\".
actorVerbMU :: MonadClientUI m => ActorId -> Actor -> MU.Part -> m ()
{-# INLINABLE actorVerbMU #-}
actorVerbMU aid b verb = do
  subject <- partActorLeader aid b
  msgAdd $ makeSentence [MU.SubjectVerbSg subject verb]

aidVerbMU :: MonadClientUI m => ActorId -> MU.Part -> m ()
{-# INLINABLE aidVerbMU #-}
aidVerbMU aid verb = do
  b <- getsState $ getActorBody aid
  actorVerbMU aid b verb

itemVerbMU :: MonadClientUI m
           => ItemId -> ItemQuant -> MU.Part -> Container -> m ()
{-# INLINABLE itemVerbMU #-}
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
{-# INLINABLE itemAidVerbMU #-}
itemAidVerbMU aid verb iid ek cstore = do
  body <- getsState $ getActorBody aid
  bag <- getsState $ getBodyStoreBag body cstore
  -- The item may no longer be in @c@, but it was
  case iid `EM.lookup` bag of
    Nothing -> assert `failure` (aid, verb, iid, cstore)
    Just kit@(k, _) -> do
      itemToF <- itemToFullClient
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
{-# INLINABLE msgDuplicateScrap #-}
msgDuplicateScrap = do
  report <- getsSession _sreport
  history <- getsSession shistory
  let (lastMsg, repRest) = lastMsgOfReport report
      lastDup = isJust . findInReport (== lastMsg)
      lastDuplicated = lastDup repRest
                       || lastDup (lastReportOfHistory history)
  when lastDuplicated $
    modifySession $ \sess -> sess {_sreport = repRest}

-- TODO: "XXX spots YYY"? or blink or show the changed xhair?
createActorUI :: MonadClientUI m => Bool -> ActorId -> Actor -> m ()
{-# INLINABLE createActorUI #-}
createActorUI born aid body = do
  side <- getsClient sside
  let verb = if born
             then MU.Text $ "appear"
                            <+> if bfid body == side then "" else "suddenly"
             else "be spotted"
  mapM_ (\(iid, store) -> void $ updateItemSlotSide store aid iid)
        (getCarriedIidCStore body)
  when (bfid body /= side) $ do
    fact <- getsState $ (EM.! bfid body) . sfactionD
    when (not (bproj body) && isAtWar fact side) $
      -- Aim even if nobody can shoot at the enemy. Let's home in on him
      -- and then we can aim or melee. We set permit to False, because it's
      -- technically very hard to check aimability here, because we are
      -- in-between turns and, e.g., leader's move has not yet been taken
      -- into account.
      modifyClient $ \cli -> cli {sxhair = TEnemy aid False}
    stopPlayBack
  -- Don't spam if the actor was already visible (but, e.g., on a tile that is
  -- invisible this turn (in that case move is broken down to lose+spot)
  -- or on a distant tile, via teleport while the observer teleported, too).
  lastLost <- getsSession slastLost
  if ES.member aid lastLost || bproj body then
    markDisplayNeeded (blid body)
  else do
    actorVerbMU aid body verb
    animate (blid body) $ actorX (bpos body)
    when (isNothing $ btrajectory body) $
      modifySession $ \sess -> sess {slastLost = ES.insert aid $ slastLost sess}
  lookAtMove aid

destroyActorUI :: MonadClientUI m => Bool -> ActorId -> Actor -> m ()
{-# INLINABLE destroyActorUI #-}
destroyActorUI died aid body = do
  Kind.COps{corule} <- getsState scops
  side <- getsClient sside
  when (bfid body == side) $ do
    let upd = ES.delete aid
    modifySession $ \sess -> sess {sselected = upd $ sselected sess}
  when (bfid body == side && died && not (bproj body)) $ do
    let firstDeathEnds = rfirstDeathEnds $ Kind.stdRuleset corule
        fid = bfid body
    fact <- getsState $ (EM.! fid) . sfactionD
    actorsAlive <- anyActorsAlive fid (Just aid)
    -- TODO: deduplicate wrt Server
    -- TODO; actually show the --more- prompt, but not between fadeout frames
    unless (fneverEmpty (gplayer fact)
            && (not actorsAlive || firstDeathEnds)) $
      displayMore ColorBW ""
  -- If pushed, animate spotting again, to draw attention to pushing.
  when (isNothing $ btrajectory body) $
    modifySession $ \sess -> sess {slastLost = ES.insert aid $ slastLost sess}
  markDisplayNeeded (blid body)

-- TODO: deduplicate wrt Server
anyActorsAlive :: MonadClient m => FactionId -> Maybe ActorId -> m Bool
{-# INLINABLE anyActorsAlive #-}
anyActorsAlive fid maid = do
  fact <- getsState $ (EM.! fid) . sfactionD
  if fleaderMode (gplayer fact) /= LeaderNull
    then -- Leader could be changed wrt the one in fact, but not removed.
         return $! isJust $ gleader fact
    else do
      as <- getsState $ fidActorNotProjAssocs fid
      return $! not $ null $ maybe as (\aid -> filter ((/= aid) . fst) as) maid

moveActor :: MonadClientUI m => ActorId -> Point -> Point -> m ()
{-# INLINABLE moveActor #-}
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
{-# INLINABLE displaceActorUI #-}
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
  animate (blid sb) $ swapPlaces ps

moveItemUI :: MonadClientUI m
           => ItemId -> Int -> ActorId -> CStore -> CStore
           -> m ()
{-# INLINABLE moveItemUI #-}
moveItemUI iid k aid cstore1 cstore2 = do
  let verb = verbCStore cstore2
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  let underAI = isAIFact fact
  mleader <- getsClient _sleader
  ItemSlots itemSlots _ <- getsClient sslots
  case lookup iid $ map swap $ EM.assocs itemSlots of
    Just l -> do
      when (Just aid == mleader) $ do
        !lastStore <- getsClient slastStore
        let newStore = cstore2 : cstore1
                       : delete cstore2 (delete cstore1 lastStore)
        modifyClient $ \cli -> cli { slastSlot = l
                                   , slastStore = newStore }
      if cstore1 == CGround && Just aid == mleader && not underAI then
        itemAidVerbMU aid (MU.Text verb) iid (Right k) cstore2
      else when (not (bproj b) && bhp b > 0) $  -- don't announce death drops
        itemAidVerbMU aid (MU.Text verb) iid (Left $ Just k) cstore2
    Nothing -> assert `failure` iid

-- TODO: split into many top-level functions; factor out common parts
quitFactionUI :: MonadClientUI m
              => FactionId -> Maybe Actor -> Maybe Status -> m ()
{-# INLINABLE quitFactionUI #-}
quitFactionUI fid mbody toSt = do
  Kind.COps{coitem=Kind.Ops{okind, ouniqGroup}} <- getsState scops
  fact <- getsState $ (EM.! fid) . sfactionD
  let fidName = MU.Text $ gname fact
      horror = isHorrorFact fact
  side <- getsClient sside
  when (side == fid && maybe False ((/= Camping) . stOutcome) toSt) $ do
    let won = case toSt of
          Just Status{stOutcome=Conquer} -> True
          Just Status{stOutcome=Escape} -> True
          _ -> False
    when won $ do
      gameModeId <- getsState sgameModeId
      scurDiff <- getsClient scurDiff
      let sing = IM.singleton scurDiff 1
          f = IM.unionWith (+)
          g svictoriesOld = EM.insertWith f gameModeId sing svictoriesOld
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
          assert `failure` (fid, mbody, toSt)
        Nothing -> (Nothing, Nothing)  -- server wipes out Camping for savefile
  case startingPart of
    Nothing -> return ()
    Just sp -> msgAdd $ makeSentence [MU.SubjectVerbSg fidName sp]
  case (toSt, partingPart) of
    (Just status, Just pp) -> do
      isNoConfirms <- isNoConfirmsGame
      go <- if isNoConfirms then return False else displaySpaceEsc ColorFull ""
      recordHistory  -- we are going to exit or restart, so record and clear
      when go $ do
        lidV <- viewedLevelUI
        Level{lxsize, lysize} <- getLevel lidV
        let store = CGround  -- only matters for UI details; all items shown
            currencyName = MU.Text $ IK.iname $ okind $ ouniqGroup "currency"
            bodyToItemSlides b = do
              (bag, tot) <- getsState $ calculateTotal b
              if EM.null bag then return (EM.empty, emptySlideshow, 0)
              else do
                let spoilsMsg = makeSentence [ "Your spoils are worth"
                                             , MU.CarWs tot currencyName ]
                promptAdd spoilsMsg
                io <- itemOverlay store (blid b) bag
                sli <- overlayToSlideshow (lysize + 1) [K.spaceKM, K.escKM] io
                return (bag, sli, tot)
        factOur <- getsState $ (EM.! side) . sfactionD
        (bag, itemSlides, total) <- case mbody of
          Just b | fid == side -> bodyToItemSlides b
          _ -> case gleader factOur of
            Nothing -> return (EM.empty, emptySlideshow, 0)
            Just aid -> do
              b <- getsState $ getActorBody aid
              bodyToItemSlides b
        arena <- getArenaUI
        localTime <- getsState $ getLocalTime arena
        itemToF <- itemToFullClient
        ItemSlots lSlots _ <- getsClient sslots
        let keyOfEKM (Left km) = km
            keyOfEKM (Right SlotChar{slotChar}) = [K.mkChar slotChar]
            allOKX = concatMap snd $ slideshow itemSlides
            keys = [K.spaceKM, K.escKM] ++ concatMap (keyOfEKM . fst) allOKX
            examItem slot =
              case EM.lookup slot lSlots of
                Nothing -> assert `failure` slot
                Just iid -> case EM.lookup iid bag of
                  Nothing -> assert `failure` iid
                  Just kit@(k, _) -> do
                    let itemFull = itemToF iid kit
                        attrLine = itemDesc store localTime itemFull
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
                  Left _ -> assert `failure` ekm
                  Right slot -> do
                    go2 <- examItem slot
                    if go2 then viewItems pointer2 else return True
        go3 <- viewItems 2
        when go3 $ do
          -- Show score for any UI client after any kind of game exit,
          -- even though it is saved only for human UI clients at game over.
          -- TODO: click on score and get more details and highlight
          -- current score
          scoreSlides <- scoreToSlideshow total status
          void $ getConfirms ColorFull [K.spaceKM, K.escKM] scoreSlides
          -- The last prompt stays onscreen during shutdown, etc.
          promptAdd pp
          partingSlide <- reportToSlideshow [K.spaceKM, K.escKM]
          void $ getConfirms ColorFull [K.spaceKM, K.escKM] partingSlide
          -- TODO: perhaps use a vertical animation instead, e.g., roll down
          -- and put it before item and score screens (on blank background)
      unless (fmap stOutcome toSt == Just Camping) $
        fadeOutOrIn True
    _ -> return ()

discover :: MonadClientUI m => Container -> StateClient -> ItemId -> m ()
{-# INLINABLE discover #-}
discover c oldCli iid = do
  let StateClient{sdiscoKind=oldDiscoKind, sdiscoAspect=oldDiscoAspect} = oldCli
      cstore = storeFromC c
  lid <- getsState $ lidFromC c
  discoKind <- getsClient sdiscoKind
  localTime <- getsState $ getLocalTime lid
  itemToF <- itemToFullClient
  bag <- getsState $ getContainerBag c
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
      jix = jkindIx $ itemBase itemFull
  -- Compare descriptions of all aspects and effects to determine
  -- if the discovery was meaningful to the player.
  when (EM.member jix discoKind /= EM.member jix oldDiscoKind
        || case EM.lookup iid oldDiscoAspect of
          Just _ -> False
          Nothing -> case itemDisco itemFull of
            Just ItemDisco{itemAspectMean, itemAspect=Just ar} ->
              ar /= itemAspectMean
            _ -> False) $
    msgAdd msg

-- * RespSfxAtomicUI

-- | Display special effects (text, animation) sent to the client.
displayRespSfxAtomicUI :: MonadClientUI m => Bool -> SfxAtomic -> m ()
{-# INLINE displayRespSfxAtomicUI #-}
displayRespSfxAtomicUI verbose sfx = case sfx of
  SfxStrike source target iid cstore b -> strike source target iid cstore b
  SfxRecoil source target _ _ _ -> do
    spart <- partAidLeader source
    tpart <- partAidLeader target
    msgAdd $ makeSentence [MU.SubjectVerbSg spart "shrink away from", tpart]
  SfxProject aid iid cstore -> do
    setLastSlot aid iid cstore
    itemAidVerbMU aid "fling" iid (Left $ Just 1) cstore
  SfxCatch aid iid cstore ->
    itemAidVerbMU aid "catch" iid (Left $ Just 1) cstore
  SfxApply aid iid cstore -> do
    setLastSlot aid iid cstore
    itemAidVerbMU aid "apply" iid (Left $ Just 1) cstore
  SfxCheck aid iid cstore ->
    itemAidVerbMU aid "deapply" iid (Left $ Just 1) cstore
  SfxTrigger aid _p _feat ->
    -- TODO: when more triggers that are not visible on the map, add msgs
    when verbose $ aidVerbMU aid "trigger"
  SfxShun aid _p _ ->
    when verbose $ aidVerbMU aid "shun"  -- TODO: shuns stairs down
  SfxEffect fidSource aid effect hpDelta -> do
    b <- getsState $ getActorBody aid
    arena <- getArenaUI
    side <- getsClient sside
    let fid = bfid b
        isOurCharacter = fid == side && not (bproj b)
        isOurAlive = isOurCharacter && bhp b > 0
    if bhp b <= 0 && hpDelta < 0 && (isOurCharacter || arena == blid b) then do
      -- We assume each non-projectile actor incapacitation is caused
      -- by some effect, and so we animate the event only here.
      let (firstFall, hurtExtra) = case (fid == side, bproj b) of
            (True, True) -> ("fall apart", "be reduced to dust")
            (True, False) -> ("fall down", "be stomped flat")
            (False, True) -> ("break up", "be shattered into little pieces")
            (False, False) -> ("collapse", "be reduced to a bloody pulp")
          verbDie = if alreadyDeadBefore then hurtExtra else firstFall
          alreadyDeadBefore = bhp b - hpDelta <= 0
      subject <- partActorLeader aid b
      let msgDie = makeSentence [MU.SubjectVerbSg subject verbDie]
      msgAdd msgDie
      let deathAct = if isOurCharacter
                     then if alreadyDeadBefore
                          then twirlSplash (bpos b, bpos b) Color.Red Color.Red
                          else deathBody (bpos b)
                     else pushAndDelay  -- if boring, at least display
      animate (blid b) deathAct
    else case effect of
        IK.ELabel{} -> return ()
        IK.EqpSlot{} -> return ()
        IK.Hurt{} -> return ()  -- avoid spam; SfxStrike just sent
        IK.Burn{} -> do
          if isOurAlive
          then actorVerbMU aid b "feel burned"
          else actorVerbMU aid b "look burned"
          let ps = (bpos b, bpos b)
          animate (blid b) $ twirlSplash ps Color.BrRed Color.Red
        IK.Explode{} -> return ()  -- lots of visual feedback
        IK.RefillHP p | p == 1 -> return ()  -- no spam from regeneration
        IK.RefillHP p | p == -1 -> return ()  -- no spam from poison
        IK.RefillHP{} | hpDelta > 0 -> do
          if isOurAlive then
            actorVerbMU aid b "feel healthier"
          else
            actorVerbMU aid b "look healthier"
          let ps = (bpos b, bpos b)
          animate (blid b) $ twirlSplash ps Color.BrBlue Color.Blue
        IK.RefillHP{} -> do
          if isOurAlive then
            actorVerbMU aid b "feel wounded"
          else
            actorVerbMU aid b "look wounded"
          let ps = (bpos b, bpos b)
          animate (blid b) $ twirlSplash ps Color.BrRed Color.Red
        IK.OverfillHP{} | hpDelta > 0 -> do
          if isOurAlive then
            actorVerbMU aid b "feel healthier"
          else
            actorVerbMU aid b "look healthier"
          let ps = (bpos b, bpos b)
          animate (blid b) $ twirlSplash ps Color.BrBlue Color.Blue
        IK.OverfillHP{} -> do
          if isOurAlive then
            actorVerbMU aid b "feel wounded"
          else
            actorVerbMU aid b "look wounded"
          let ps = (bpos b, bpos b)
          animate (blid b) $ twirlSplash ps Color.BrRed Color.Red
        IK.RefillCalm p | p == 1 -> return ()  -- no spam from regen items
        IK.RefillCalm p | p > 0 -> do
          if isOurAlive then
            actorVerbMU aid b "feel calmer"
          else
            actorVerbMU aid b "look calmer"
        IK.RefillCalm _ -> do
          if isOurAlive then
            actorVerbMU aid b "feel agitated"
          else
            actorVerbMU aid b "look agitated"
        IK.OverfillCalm p | p > 0 -> do
          if isOurAlive then
            actorVerbMU aid b "feel calmer"
          else
            actorVerbMU aid b "look calmer"
        IK.OverfillCalm _ -> do
          if isOurAlive then
            actorVerbMU aid b "feel agitated"
          else
            actorVerbMU aid b "look agitated"
        IK.Dominate -> do
          -- For subsequent messages use the proper name, never "you".
          let subject = partActor b
          if fid /= fidSource then do  -- before domination
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
            when isOurAlive $ displayMore ColorFull ""
          else do
            fidSourceName <- getsState $ gname . (EM.! fidSource) . sfactionD
            let verb = "be now under"
            msgAdd $ makeSentence
              [MU.SubjectVerbSg subject verb, MU.Text fidSourceName, "control"]
          stopPlayBack
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
        IK.DropItem COrgan _ -> return ()
        IK.DropItem _ _ -> actorVerbMU aid b "be stripped"  -- TODO
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
        IK.Unique -> assert `failure` sfx
        IK.Periodic -> assert `failure` sfx
  SfxMsgFid _ msg -> msgAdd msg
  SfxMsgAll msg -> msgAdd msg

setLastSlot :: MonadClientUI m => ActorId -> ItemId -> CStore -> m ()
{-# INLINABLE setLastSlot #-}
setLastSlot aid iid cstore = do
  mleader <- getsClient _sleader
  when (Just aid == mleader) $ do
    ItemSlots itemSlots _ <- getsClient sslots
    !lastStore <- getsClient slastStore
    let newStore = cstore : delete cstore lastStore
    case lookup iid $ map swap $ EM.assocs itemSlots of
      Just l -> modifyClient $ \cli -> cli { slastSlot = l
                                           , slastStore = newStore }
      Nothing -> assert `failure` (iid, cstore, aid)

strike :: MonadClientUI m
       => ActorId -> ActorId -> ItemId -> CStore -> HitAtomic -> m ()
{-# INLINABLE strike #-}
strike source target iid cstore hitStatus = assert (source /= target) $ do
  tb <- getsState $ getActorBody target
  sourceSeen <- getsState $ memActor source (blid tb)
  ps <- if sourceSeen then do
    itemToF <- itemToFullClient
    sb <- getsState $ getActorBody source
    spart <- partActorLeader source sb
    tpart <- partActorLeader target tb
    spronoun <- partPronounLeader source sb
    localTime <- getsState $ getLocalTime (blid tb)
    bag <- getsState $ getBodyStoreBag sb cstore
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
                else [ MU.SubjectVerbSg spart verb, tpart
                     , "with", partItemChoice itemFull ]
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
    return (bpos tb, bpos sb)
  else return (bpos tb, bpos tb)
  let anim HitClear = twirlSplash ps Color.BrRed Color.Red
      anim (HitBlock 1) = blockHit ps Color.BrRed Color.Red
      anim (HitBlock _) = blockMiss ps
  animate (blid tb) $ anim hitStatus
