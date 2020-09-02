{-# LANGUAGE TupleSections #-}
-- | Display atomic commands received by the client.
module Game.LambdaHack.Client.UI.DisplayAtomicM
  ( displayRespUpdAtomicUI, displayRespSfxAtomicUI
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , ppHearMsg, ppHearDistanceAdjective, ppHearDistanceAdverb
  , updateItemSlot, markDisplayNeeded, lookAtMove
  , aidVerbMU, aidVerbMU0, aidVerbDuplicateMU
  , itemVerbMU, itemAidVerbMU, manyItemsAidVerbMU
  , createActorUI, destroyActorUI, spotItemBag, moveActor, displaceActorUI
  , moveItemUI, quitFactionUI
  , displayGameOverLoot, displayGameOverAnalytics
  , discover, ppSfxMsg, strike
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
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
import           Game.LambdaHack.Content.RuleKind
import qualified Game.LambdaHack.Content.TileKind as TK
import qualified Game.LambdaHack.Core.Dice as Dice
import           Game.LambdaHack.Core.Frequency
import           Game.LambdaHack.Core.Random
import qualified Game.LambdaHack.Definition.Ability as Ability
import qualified Game.LambdaHack.Definition.Color as Color
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.Flavour

-- * RespUpdAtomicUI

-- | Visualize atomic updates sent to the client. This is done
-- in the global state after the command is executed and after
-- the client state is modified by the command.
-- Don't modify client state (except a few fields), but only client
-- session (e.g., by displaying messages). This is enforced by types.
displayRespUpdAtomicUI :: MonadClientUI m => UpdAtomic -> m ()
{-# INLINE displayRespUpdAtomicUI #-}
displayRespUpdAtomicUI cmd = case cmd of
  -- Create/destroy actors and items.
  UpdRegisterItems{} -> return ()
  UpdCreateActor aid body _ -> createActorUI True aid body
  UpdDestroyActor aid body _ -> destroyActorUI True aid body
  UpdCreateItem verbose iid _ kit@(kAdd, _) c -> do
    recordItemLid iid c
    updateItemSlot c iid
    if verbose then case c of
      CActor aid store -> do
        b <- getsState $ getActorBody aid
        case store of
          _ | bproj b ->
            itemVerbMU MsgItemCreation iid kit "appear" c
          COrgan -> do
            localTime <- getsState $ getLocalTime (blid b)
            arItem <- getsState $ aspectRecordFromIid iid
            if | IA.checkFlag Ability.Blast arItem -> return ()
               | IA.checkFlag Ability.Condition arItem -> do
                 side <- getsClient sside
                 discoBenefit <- getsClient sdiscoBenefit
                 bag <- getsState $ getContainerBag c
                 itemKind <- getsState $ getIidKind iid
                 let more = case EM.lookup iid bag of
                       Just (kTotal, _) | kTotal /= kAdd -> Just kTotal
                       _ -> Nothing
                     verb = MU.Text $
                       "become"
                       <+> case kit of
                         (1, t:_) ->  -- only exceptionally not singleton list
                                      -- or even more than one copy total
                           let total = deltaOfItemTimer localTime t
                           in timeDeltaInSecondsText total
                         (1, []) | isNothing more -> ""
                         (k, _) ->  -- usually the list empty; ignore anyway
                           (if isJust more then "additionally" else "")
                           <+> tshow k <> "-fold"
                           <+> case more of
                                 Nothing -> ""
                                 Just kTotal ->
                                   "(total:" <+> tshow kTotal <> "-fold)"
                     msgClass = case lookup IK.S_ASLEEP $ IK.ifreq itemKind of
                       Just n | n > 0 -> MsgBecomeSleep
                       _ -> if | bfid b /= side -> MsgBecome
                               | benInEqp (discoBenefit EM.! iid) ->
                                   MsgBecomeBeneficialUs
                               | otherwise -> MsgBecomeHarmfulUs
                 -- This describes all such items already among organs,
                 -- which is useful, because it shows "charging".
                 itemAidVerbMU msgClass aid verb iid (Left Nothing)
               | otherwise -> do
                 wown <- ppContainerWownW partActorLeader True c
                 itemVerbMU MsgItemCreation iid kit
                            (MU.Text $ makePhrase $ "grow" : wown) c
          _ -> do
            wown <- ppContainerWownW partActorLeader True c
            itemVerbMU MsgItemCreation iid kit
                       (MU.Text $ makePhrase $ "appear" : wown) c
      CEmbed lid _ -> markDisplayNeeded lid
      CFloor lid _ -> do
        factionD <- getsState sfactionD
        itemVerbMU MsgItemCreation iid kit
                   (MU.Text $ "appear" <+> ppContainer factionD c) c
        markDisplayNeeded lid
      CTrunk{} -> return ()
    else do
      lid <- getsState $ lidFromC c
      markDisplayNeeded lid
  UpdDestroyItem verbose iid _ kit c ->
    if verbose then case c of
      CActor aid _  -> do
        b <- getsState $ getActorBody aid
        if bproj b then
          itemVerbMUShort MsgItemDestruction iid kit "break" c
        else do
          ownW <- ppContainerWownW partActorLeader False c
          let verb = MU.Text $ makePhrase $ "vanish from" : ownW
          itemVerbMUShort MsgItemDestruction iid kit verb c
      CEmbed lid _ -> markDisplayNeeded lid
      CFloor lid _ -> do
        factionD <- getsState sfactionD
        itemVerbMUShort MsgItemDestruction iid kit
                        (MU.Text $ "break" <+> ppContainer factionD c) c
        markDisplayNeeded lid
      CTrunk{} -> return ()
    else do
      lid <- getsState $ lidFromC c
      markDisplayNeeded lid
  UpdSpotActor aid body -> createActorUI False aid body
  UpdLoseActor aid body -> destroyActorUI False aid body
  UpdSpotItem verbose iid kit c -> spotItemBag verbose c $ EM.singleton iid kit
  UpdLoseItem True iid kit c@(CActor aid _) -> do
    b <- getsState $ getActorBody aid
    when (not (bproj b) && bhp b > 0) $ do  -- don't spam
      ownW <- ppContainerWownW partActorLeader False c
      let verb = MU.Text $ makePhrase $ "be removed from" : ownW
      itemVerbMUShort MsgItemMove iid kit verb c
  UpdLoseItem{} -> return ()
  UpdSpotItemBag verbose c bag -> spotItemBag verbose c bag
  UpdLoseItemBag{} -> return ()  -- rarely interesting and can be very long
  -- Move actors and items.
  UpdMoveActor aid source target -> moveActor aid source target
  UpdWaitActor aid WSleep _ -> aidVerbMU MsgNoLongerSleep aid "wake up"
  UpdWaitActor{} -> return ()  -- falling asleep handled uniformly elsewhere
  UpdDisplaceActor source target -> displaceActorUI source target
  UpdMoveItem iid k aid c1 c2 -> moveItemUI iid k aid c1 c2
  -- Change actor attributes.
  UpdRefillHP _ 0 -> return ()
  UpdRefillHP aid hpDelta -> do
    let coarseDelta = abs hpDelta `div` oneM
        tDelta = if coarseDelta == 0
                 then if hpDelta > 0 then "a little" else "a fraction of an HP"
                 else tshow coarseDelta <+> "HP"
    aidVerbMU MsgNumeric aid $ MU.Text
                             $ (if hpDelta > 0 then "heal" else "lose")
                               <+> tDelta
    b <- getsState $ getActorBody aid
    arena <- getArenaUI
    side <- getsClient sside
    if | bproj b && (EM.null (beqp b) || isNothing (btrajectory b)) ->
           return ()  -- ignore caught proj or one hitting a wall
       | bhp b <= 0 && hpDelta < 0
         && (bfid b == side && not (bproj b) || arena == blid b) -> do
         let (firstFall, hurtExtra) = case (bfid b == side, bproj b) of
               (True, True) -> ("drop down", "tumble down")
               (True, False) -> ("fall down", "suffer woeful mutilation")
               (False, True) -> ("plummet", "crash")
               (False, False) -> ("collapse", "be reduced to a bloody pulp")
             verbDie = if alreadyDeadBefore then hurtExtra else firstFall
             alreadyDeadBefore = bhp b - hpDelta <= 0
         tfact <- getsState $ (EM.! bfid b) . sfactionD
         bUI <- getsSession $ getActorUI aid
         subjectRaw <- partActorLeader aid
         let subject = if alreadyDeadBefore || subjectRaw == "you"
                       then subjectRaw
                       else partActor bUI  -- avoid "fallen"
             msgDie = makeSentence [MU.SubjectVerbSg subject verbDie]
             targetIsFoe = isFoe (bfid b) tfact side
             targetIsFriend = isFriend (bfid b) tfact side
             msgClass | bproj b = MsgDeathBoring
                      | targetIsFoe = MsgDeathGood
                      | targetIsFriend = MsgDeathBad
                      | otherwise = MsgDeathBoring
         if | bproj b -> msgAdd msgClass msgDie
            | bfid b == side -> do
              msgLnAdd msgClass $ msgDie <+> "Alas!"
              displayMore ColorBW ""
            | otherwise -> msgLnAdd msgClass msgDie
         -- We show death anims only if not dead already before this refill.
         let deathAct
               | alreadyDeadBefore =
                 twirlSplash (bpos b, bpos b) Color.Red Color.Red
               | bfid b == side = deathBody (bpos b)
               | otherwise = shortDeathBody (bpos b)
         unless (bproj b) $ animate (blid b) deathAct
       | otherwise -> do
         when (hpDelta >= bhp b && bhp b > 0) $
           aidVerbMU MsgWarning aid "return from the brink of death"
         mleader <- getsClient sleader
         when (Just aid == mleader) $ do
           actorMaxSk <- getsState $ getActorMaxSkills aid
           -- Regenerating actors never stop gaining HP, so we need to stop
           -- reporting it after they reach full HP for the first time.
           -- Also, no spam for non-leaders.
           when (bhp b >= xM (Ability.getSk Ability.SkMaxHP actorMaxSk)
                 && bhp b - hpDelta < xM (Ability.getSk Ability.SkMaxHP
                                                  actorMaxSk)) $
             msgAdd MsgNeutralEventRare "You recover your health fully. Any further gains will be transient."
         when (bfid b == side && not (bproj b)) $ do
           markDisplayNeeded (blid b)
           when (hpDelta < 0) $ do
             sUIOptions <- getsSession sUIOptions
             currentWarning <-
               getsState $ checkWarningHP sUIOptions aid (bhp b)
             when currentWarning $ do
               previousWarning <-
                 getsState $ checkWarningHP sUIOptions aid (bhp b - hpDelta)
               unless previousWarning $
                 aidVerbMU0 MsgNearDeath aid
                            "be down to a dangerous health level"
  UpdRefillCalm _ 0 -> return ()
  UpdRefillCalm aid calmDelta -> do
    side <- getsClient sside
    b <- getsState $ getActorBody aid
    when (bfid b == side && not (bproj b)) $ do
      if | calmDelta > 0 -> do  -- regeneration or effect
           mleader <- getsClient sleader
           when (Just aid == mleader) $ do
             actorMaxSk <- getsState $ getActorMaxSkills aid
             let bPrev = b {bcalm = bcalm b - calmDelta}
             when (calmEnough b actorMaxSk
                   && not (calmEnough bPrev actorMaxSk)) $
               msgAdd MsgNeutralEvent "You are again calm enough to manage your equipment outfit."
           markDisplayNeeded (blid b)
         | calmDelta == minusM1 -> do
           fact <- getsState $ (EM.! side) . sfactionD
           s <- getState
           let closeFoe (!p, aid2) =  -- mimics isHeardFoe
                 let b2 = getActorBody aid2 s
                 in inline chessDist p (bpos b) <= 3
                    && not (actorWaitsOrSleeps b2)  -- uncommon
                    && inline isFoe side fact (bfid b2)  -- costly
               anyCloseFoes = any closeFoe $ EM.assocs $ lbig
                                           $ sdungeon s EM.! blid b
           unless anyCloseFoes $ do  -- obvious where the feeling comes from
             duplicated <- aidVerbDuplicateMU MsgHeardClose aid "hear something"
             unless duplicated stopPlayBack
         | otherwise ->  -- low deltas from hits; displayed elsewhere
           return ()
      when (calmDelta < 0) $ do
        sUIOptions <- getsSession sUIOptions
        currentWarning <-
          getsState $ checkWarningCalm sUIOptions aid (bcalm b)
        when currentWarning $ do
          previousWarning <-
            getsState $ checkWarningCalm sUIOptions aid (bcalm b - calmDelta)
          unless previousWarning $
            -- This messages is not shown if impression happens after
            -- Calm is low enough. However, this is rare and HUD shows the red.
            aidVerbMU0 MsgNearDeath aid
                       "have grown agitated and impressed enough to be in danger of defecting"
  UpdTrajectory _ _ mt ->  -- if projectile dies just after, force one frame
    when (isNothing mt) pushFrame
  -- Change faction attributes.
  UpdQuitFaction fid _ toSt manalytics -> quitFactionUI fid toSt manalytics
  UpdSpotStashFaction verbose fid lid pos -> do
    when verbose $ do
      side <- getsClient sside
      if fid == side then
        msgLnAdd MsgDiplomacy
                 "You set up the shared inventory stash of your team."
      else do
        fact <- getsState $ (EM.! fid) . sfactionD
        let fidName = MU.Text $ gname fact
        msgAdd MsgDiplomacy $
          makeSentence [ "you have found the current"
                       , MU.WownW fidName "hoard location" ]
    animate lid $ actorX pos
  UpdLoseStashFaction verbose fid lid pos -> do
    when verbose $ do
      side <- getsClient sside
      if fid == side then
        msgAdd MsgDiplomacy "You've lost access to your shared inventory stash!"
      else do
        fact <- getsState $ (EM.! fid) . sfactionD
        let fidName = MU.Text $ gname fact
        msgAdd MsgDiplomacy $
          makeSentence [fidName, "no longer control their hoard"]
    animate lid $ vanish pos
  UpdLeadFaction fid (Just source) mtgt@(Just target) -> do
    mleader <- getsClient sleader
    when (mtgt /= mleader) $ do
      fact <- getsState $ (EM.! fid) . sfactionD
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
          msgAdd MsgLeader $
            makeSentence [ MU.SubjectVerbSg subject "take command"
                         , "from", object ]
        _ -> return ()
      lookAtMove target
  UpdLeadFaction _ Nothing mtgt@(Just target) -> do
    mleader <- getsClient sleader
    when (mtgt /= mleader) $
      lookAtMove target
  UpdLeadFaction{} -> return ()
  UpdDiplFaction fid1 fid2 _ toDipl -> do
    name1 <- getsState $ gname . (EM.! fid1) . sfactionD
    name2 <- getsState $ gname . (EM.! fid2) . sfactionD
    let showDipl Unknown = "unknown to each other"
        showDipl Neutral = "in neutral diplomatic relations"
        showDipl Alliance = "allied"
        showDipl War = "at war"
    msgAdd MsgDiplomacy $
      name1 <+> "and" <+> name2 <+> "are now" <+> showDipl toDipl <> "."
  UpdDoctrineFaction{} -> return ()
  UpdAutoFaction fid b -> do
    side <- getsClient sside
    lidV <- viewedLevelUI
    markDisplayNeeded lidV
    when (fid == side) $ do
      unless b addPressedControlEsc  -- sets @swasAutomated@, enters main menu
      setFrontAutoYes b  -- now can stop auto-accepting prompts
  UpdRecordKill{} -> return ()
  -- Alter map.
  UpdAlterTile lid p fromTile toTile -> do
    COps{cotile} <- getsState scops
    markDisplayNeeded lid
    let feats = TK.tfeature $ okind cotile fromTile
        toAlter feat =
          case feat of
            TK.OpenTo tgroup -> Just tgroup
            TK.CloseTo tgroup -> Just tgroup
            TK.ChangeTo tgroup -> Just tgroup
            TK.OpenWith _ _ tgroup -> Just tgroup
            TK.CloseWith _ _ tgroup -> Just tgroup
            TK.ChangeWith _ _ tgroup -> Just tgroup
            _ -> Nothing
        groupsToAlterTo = mapMaybe toAlter feats
        freq = map fst $ filter (\(_, q) -> q > 0)
               $ TK.tfreq $ okind cotile toTile
        unexpected = null $ intersect freq groupsToAlterTo
    mactorAtPos <- getsState $ posToBig p lid
    mleader <- getsClient sleader
    when (unexpected || isJust mactorAtPos && mactorAtPos /= mleader) $ do
      -- Player notices @fromTile can't be altered into @toTIle@,
      -- which is uncanny, so we produce a message.
      -- This happens when the player missed an earlier search of the tile
      -- performed by another faction.
      let subject = ""  -- a hack, because we don't handle adverbs well
          verb = "turn into"
          msg = makeSentence $
            [ "the", MU.Text $ TK.tname $ okind cotile fromTile
            , "at position", MU.Text $ tshow p ]
            ++ ["suddenly" | unexpected]  -- adverb
            ++ [ MU.SubjectVerbSg subject verb
               , MU.AW $ MU.Text $ TK.tname $ okind cotile toTile ]
      msgAdd (if unexpected then MsgNeutralEventRare else MsgNeutralEvent) msg
  UpdAlterExplorable lid _ -> markDisplayNeeded lid
  UpdAlterGold{} -> return ()  -- not displayed on HUD
  UpdSearchTile aid _p toTile -> do
    COps{cotile} <- getsState scops
    subject <- partActorLeader aid
    let fromTile = fromMaybe (error $ show toTile) $ Tile.hideAs cotile toTile
        subject2 = MU.Text $ TK.tname $ okind cotile fromTile
        object = MU.Text $ TK.tname $ okind cotile toTile
    let msg = makeSentence [ MU.SubjectVerbSg subject "reveal"
                           , "that the"
                           , MU.SubjectVerbSg subject2 "be"
                           , MU.AW object ]
    unless (subject2 == object) $ msgAdd MsgDiscoTile msg
  UpdHideTile{} -> return ()
  UpdSpotTile{} -> return ()
  UpdLoseTile{} -> return ()
  UpdSpotEntry{} -> return ()
  UpdLoseEntry{} -> return ()
  UpdAlterSmell{} -> return ()
  UpdSpotSmell{} -> return ()
  UpdLoseSmell{} -> return ()
  -- Assorted.
  UpdTimeItem{} -> return ()
  UpdAgeGame{} -> do
    sdisplayNeeded <- getsSession sdisplayNeeded
    sturnDisplayed <- getsSession sturnDisplayed
    time <- getsState stime
    let clipN = time `timeFit` timeClip
        clipMod = clipN `mod` clipsInTurn
        turnPing = clipMod == 0  -- e.g., to see resting counter
    when (sdisplayNeeded || turnPing && not sturnDisplayed) pushFrame
    when turnPing $
      modifySession $ \sess -> sess {sturnDisplayed = False}
  UpdUnAgeGame{} -> return ()
  UpdDiscover c iid _ _ -> discover c iid
  UpdCover{} -> return ()  -- don't spam when doing undo
  UpdDiscoverKind{} -> return ()  -- don't spam when server tweaks stuff
  UpdCoverKind{} -> return ()  -- don't spam when doing undo
  UpdDiscoverAspect{} -> return ()  -- don't spam when server tweaks stuff
  UpdCoverAspect{} -> return ()  -- don't spam when doing undo
  UpdDiscoverServer{} -> error "server command leaked to client"
  UpdCoverServer{} -> error "server command leaked to client"
  UpdPerception{} -> return ()
  UpdRestart fid _ _ _ _ srandom -> do
    -- Start or take over the frontend.
    CCUI{coscreen} <- getsSession sccui
    soptions <- getsClient soptions
    schanF <- chanFrontend coscreen soptions
    modifySession $ \sess -> sess {schanF, srandomUI = srandom}
    COps{cocave, corule} <- getsState scops
    sstart <- getsSession sstart
    when (sstart == 0) resetSessionStart
    history <- getsSession shistory
    when (lengthHistory history == 0) $ do
      let title = T.pack $ rtitle corule
      msgAdd MsgAdmin $ "Welcome to" <+> title <> "!"
      -- Generate initial history. Only for UI clients.
      shistory <- defaultHistory
      modifySession $ \sess -> sess {shistory}
    recordHistory  -- to ensure EOL even at creation of history
    lid <- getArenaUI
    lvl <- getLevel lid
    gameMode <- getGameMode
    curChal <- getsClient scurChal
    fact <- getsState $ (EM.! fid) . sfactionD
    let loneMode = case ginitial fact of
          [] -> True
          [(_, 1, _)] -> True
          _ -> False
    msgAdd MsgAdmin "-------------------------------------------------"
    recordHistory
    msgAdd MsgWarning $ "New game started in" <+> mname gameMode
                        <+> "mode. Press '?' for details."
    msgAdd MsgPlot $ mdesc gameMode
    let desc = cdesc $ okind cocave $ lkind lvl
    unless (T.null desc) $ do
      msgLnAdd MsgFocus "You take in your surroundings."
      msgAdd MsgLandscape desc
    -- We can fool the player only once (per scenario), but let's not do it
    -- in the same way each time. TODO: PCG
    blurb <- rndToActionUI $ oneOf
      [ "You think you saw movement."
      , "Something catches your peripherial vision."
      , "You think you felt a tremor under your feet."
      , "A whiff of chilly air passes around you."
      , "You notice a draft just when it dies down."
      , "The ground nearby is stained along some faint lines."
      , "Scarce black motes slowly settle on the ground."
      , "The ground in the immediate area is empty, as if just swiped."
      ]
    msgLnAdd MsgBecomeHarmfulUs blurb  -- nice colour; being here is harmful
    when (cwolf curChal && not loneMode) $
      msgAdd MsgWarning "Being a lone wolf, you begin without companions."
    when (lengthHistory history > 1) $ fadeOutOrIn False
    setFrontAutoYes $ isAIFact fact
    -- Forget the furious keypresses when dying in the previous game.
    resetPressedKeys
    -- Help newbies when actors obscured by text and no obvious key to press:
    displayMore ColorFull "\nAre you up for the challenge?"
    promptAdd0 "A grand story starts right here!"
  UpdRestartServer{} -> return ()
  UpdResume fid _ -> do
    COps{cocave} <- getsState scops
    -- Start or take over the frontend.
    CCUI{coscreen} <- getsSession sccui
    soptions <- getsClient soptions
    schanF <- chanFrontend coscreen soptions
    modifySession $ \sess -> sess {schanF}
    resetSessionStart
    fact <- getsState $ (EM.! fid) . sfactionD
    setFrontAutoYes $ isAIFact fact
    unless (isAIFact fact) $ do
      lid <- getArenaUI
      lvl <- getLevel lid
      gameMode <- getGameMode
      msgAdd MsgAlert $ "Continuing" <+> mname gameMode
                        <> ". Press '?' for details."
      msgAdd0 MsgPrompt $ mdesc gameMode
      let desc = cdesc $ okind cocave $ lkind lvl
      unless (T.null desc) $ do
        msgLnAdd0 MsgPromptFocus "You remember your surroundings."
        msgAdd0 MsgPrompt desc
      displayMore ColorFull "\nAre you up for the challenge?"
      promptAdd0 "Prove yourself!"
  UpdResumeServer{} -> return ()
  UpdKillExit{} -> do
    side <- getsClient sside
    factionD <- getsState sfactionD
    when (side `EM.member` factionD) $  -- the active UI client
      frontendShutdown
  UpdWriteSave -> msgAdd MsgSpam "Saving backup."
  UpdHearFid _ distance hearMsg -> do
    mleader <- getsClient sleader
    case mleader of
      Just{} -> return ()  -- will display stuff when leader moves
      Nothing -> do
        lidV <- viewedLevelUI
        markDisplayNeeded lidV
        recordHistory
    msg <- ppHearMsg distance hearMsg
    let msgClass = case distance of
          Nothing -> MsgHeardElsewhere
          Just 0 -> MsgHeardClose
          Just _ -> MsgHeard
    msgAdd msgClass msg

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
    itemsBlurb <- lookAtItems True (bpos body) aid
    let msgClass = if Just aid == mleader then MsgAtFeetMajor else MsgAtFeet
    msgAdd msgClass $ stashBlurb <+> itemsBlurb
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

aidVerbMU :: MonadClientUI m => MsgClass -> ActorId -> MU.Part -> m ()
aidVerbMU msgClass aid verb = do
  subject <- partActorLeader aid
  msgAdd msgClass $ makeSentence [MU.SubjectVerbSg subject verb]

aidVerbMU0 :: MonadClientUI m => MsgClass -> ActorId -> MU.Part -> m ()
aidVerbMU0 msgClass aid verb = do
  subject <- partActorLeader aid
  msgAdd0 msgClass $ makeSentence [MU.SubjectVerbSg subject verb]

aidVerbDuplicateMU :: MonadClientUI m
                   => MsgClass -> ActorId -> MU.Part -> m Bool
aidVerbDuplicateMU msgClass aid verb = do
  subject <- partActorLeader aid
  msgAddDuplicate (makeSentence [MU.SubjectVerbSg subject verb]) msgClass 1

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

itemVerbMU :: MonadClientUI m
           => MsgClass -> ItemId -> ItemQuant -> MU.Part -> Container -> m ()
itemVerbMU msgClass iid kit verb c = do
  msg <- itemVerbMUGeneral True iid kit verb c
  msgAdd msgClass msg

itemVerbMUShort :: MonadClientUI m
                => MsgClass -> ItemId -> ItemQuant -> MU.Part -> Container
                -> m ()
itemVerbMUShort msgClass iid kit verb c = do
  msg <- itemVerbMUGeneral False iid kit verb c
  msgAdd msgClass msg

itemAidVerbMU :: MonadClientUI m
              => MsgClass -> ActorId -> MU.Part
              -> ItemId -> Either (Maybe Int) Int
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
      msg = makeSentence [MU.SubjectVerbSg subject verb, object]
  msgAdd msgClass msg

manyItemsAidVerbMU :: MonadClientUI m
                   => MsgClass -> ActorId -> MU.Part
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

createActorUI :: MonadClientUI m => Bool -> ActorId -> Actor -> m ()
createActorUI born aid body = do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  side <- getsClient sside
  when (bfid body == side && not (bproj body)) $ do
    let upd = ES.insert aid
    modifySession $ \sess -> sess {sselected = upd $ sselected sess}
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
    (n, bsymbol) <-
      if | bproj body -> return (0, if IA.checkFlag Ability.Blast arItem
                                    then IK.isymbol itemKind
                                    else '*')
         | baseColor /= Color.BrWhite -> return (0, IK.isymbol itemKind)
         | otherwise -> do
           let hasNameK k bUI = bname bUI == fst (heroNamePronoun k)
                                && bcolor bUI == gcolor fact
               findHeroK k = isJust $ find (hasNameK k) (EM.elems actorUI)
               mhs = map findHeroK [0..]
               n = fromMaybe (error $ show mhs) $ elemIndex False mhs
           return (n, if 0 < n && n < 10 then Char.intToDigit n else '@')
    let (object1, object2) =
          partItemShortest rwidth (bfid body) factionD localTime
                           itemFull quantSingle
        (bname, bpronoun) =
          if | bproj body ->
               let adj = case btrajectory body of
                     Just (tra, _) | length tra < 5 -> "falling"
                     _ -> "flying"
               in (makePhrase [adj, object1, object2], basePronoun)
             | baseColor /= Color.BrWhite ->
               let name = makePhrase [object1, object2]
               in ( if IA.checkFlag Ability.Unique arItem
                    then makePhrase [MU.Capitalize $ MU.Text $ "the" <+> name]
                    else name
                  , basePronoun )
             | otherwise -> heroNamePronoun n
        bcolor | bproj body = if IA.checkFlag Ability.Blast arItem
                              then baseColor
                              else Color.BrWhite
               | baseColor == Color.BrWhite = gcolor fact
               | otherwise = baseColor
        bUI = ActorUI{..}
    modifySession $ \sess ->
      sess {sactorUI = EM.insert aid bUI actorUI}
  let verb = MU.Text $
        if born
        then if bfid body == side then "join you" else "appear suddenly"
        else "be spotted"
  mapM_ (\(iid, store) -> do
           let c = if not (bproj body) && iid == btrunk body
                   then CTrunk (bfid body) (blid body) (bpos body)
                   else CActor aid store
           updateItemSlot c iid
           recordItemLid iid c)
        ((btrunk body, CEqp)  -- store will be overwritten, unless projectile
         : filter ((/= btrunk body) . fst) (getCarriedIidCStore body))
  -- Don't spam if the actor was already visible (but, e.g., on a tile that is
  -- invisible this turn (in that case move is broken down to lose+spot)
  -- or on a distant tile, via teleport while the observer teleported, too).
  lastLost <- getsSession slastLost
  when (bfid body /= side) $ do
    when (not (bproj body) && isFoe (bfid body) fact side) $ do
      -- Aim even if nobody can shoot at the enemy. Let's home in on him
      -- and then we can aim or melee. We set permit to False, because it's
      -- technically very hard to check aimability here, because we are
      -- in-between turns and, e.g., leader's move has not yet been taken
      -- into account.
      xhair <- getsSession sxhair
      case xhair of
        Just (TVector _) -> return ()  -- explicitly set; keep it
        _ -> modifySession $ \sess ->
               sess { sxhair = Just $ TEnemy aid
                    , sitemSel = Nothing } -- reset flinging totally
      foes <- getsState $ foeRegularList side (blid body)
      itemsSize <- getsState $ guardItemSize body
      unless (ES.member aid lastLost) $
        if length foes > 1
        then when (itemsSize > 0) $
          msgAdd0 MsgSpotThreat "Another armed threat!"
        else if itemsSize > 0
             then msgAdd0 MsgSpotThreat "Armed intrusion ahead!"
             else msgAdd0 MsgSpotThreat "You are not alone!"
    stopPlayBack
  if | EM.null actorUI && bfid body == side ->
       return ()  -- don't speak about yourself in 3rd person
     | born && bproj body -> pushFrame  -- make sure first position displayed
     | ES.member aid lastLost || bproj body -> markDisplayNeeded (blid body)
     | otherwise -> do
       aidVerbMU MsgSpotActor aid verb
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
  unless (bproj b) $
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

spotItemBag :: forall m. MonadClientUI m => Bool -> Container -> ItemBag -> m ()
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
      sortItems iis = sortOn (getKind . fst) iis
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
        msgAdd MsgItemMoveNoLog $ msgShort <> dotsIfShorter
        msgAdd MsgItemMoveLog $ msgLong
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
        manyItemsAidVerbMU MsgItemMove aid verb sortedAssocs Right
      else when (not (bproj b) && bhp b > 0) $  -- don't announce death drops
        manyItemsAidVerbMU MsgItemMove aid verb sortedAssocs (Left . Just)
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
                 then MsgAction  -- to avoid run after displace; configurable
                 else MsgActionMinor
      msg = makeSentence [MU.SubjectVerbSg spart "displace", tpart]
  msgAdd msgClass msg
  when (bfid sb /= bfid tb) $ do
    lookAtMove source
    lookAtMove target
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
        itemAidVerbMU MsgItemMove aid verb iid (Right k)
      else when (not (bproj b) && bhp b > 0) $  -- don't announce death drops
        itemAidVerbMU MsgItemMove aid verb iid (Left $ Just k)
    Nothing -> error $
      "" `showFailure` (iid, k, aid, cstore1, cstore2)

quitFactionUI :: MonadClientUI m
              => FactionId -> Maybe Status
              -> Maybe (FactionAnalytics, GenerationAnalytics)
              -> m ()
quitFactionUI fid toSt manalytics = do
  ClientOptions{sexposeItems} <- getsClient soptions
  fact <- getsState $ (EM.! fid) . sfactionD
  let fidName = MU.Text $ gname fact
      person = if fhasGender $ gplayer fact then MU.PlEtc else MU.Sg3rd
      horror = isHorrorFact fact
      camping = maybe True ((== Camping) . stOutcome) toSt
  side <- getsClient sside
  when (fid == side && not camping) $ do
    tellGameClipPS
    resetGameStart
  gameMode <- getGameMode
  allNframes <- getsSession sallNframes
  let startingPart = case toSt of
        _ | horror -> Nothing  -- Ignore summoned actors' factions.
        Just Status{stOutcome=Killed} -> Just "be eliminated"
        Just Status{stOutcome=Defeated} -> Just "be decisively defeated"
        Just Status{stOutcome=Camping} -> Just "order save and exit"
        Just Status{stOutcome=Conquer} -> Just "vanquish all foes"
        Just Status{stOutcome=Escape} -> Just "achieve victory"
        Just Status{stOutcome=Restart, stNewGame=Just gn} ->
          Just $ MU.Text $ "order mission restart in"
                           <+> fromGroupName gn <+> "mode"
        Just Status{stOutcome=Restart, stNewGame=Nothing} ->
          error $ "" `showFailure` (fid, toSt)
        Nothing -> Nothing  -- server wipes out Camping for savefile
      middlePart = case toSt of
        _ | fid /= side -> Nothing
        Just Status{stOutcome} -> lookup stOutcome $ mendMsg gameMode
        Nothing -> Nothing
      partingPart = case toSt of
        _ | fid /= side || allNframes == -1 -> Nothing
        Just Status{stOutcome} -> lookup stOutcome genericEndMessages
        Nothing -> Nothing
  case startingPart of
    Nothing -> return ()
    Just sp ->
      let blurb = makeSentence [MU.SubjectVerb person MU.Yes fidName sp]
      in msgLnAdd MsgOutcome blurb
  case (toSt, partingPart) of
    (Just status, Just pp) -> do
      noConfirmsGame <- isNoConfirmsGame
      go <- if noConfirmsGame
            then return False
            else displaySpaceEsc ColorFull ""  -- short, just @startingPart@
      recordHistory
        -- we are going to exit or restart, so record and clear, but only once
      (itemBag, total) <- getsState $ calculateTotal side
      when go $ do
        case middlePart of
          Nothing -> return ()
          Just sp1 -> do
            factionD <- getsState sfactionD
            itemToF <- getsState $ flip itemToFull
            let getTrunkFull (aid, b) = (aid, itemToF $ btrunk b)
            ourTrunks <- getsState $ map getTrunkFull
                                     . fidActorNotProjGlobalAssocs side
            let smartFaction fact2 = fleaderMode (gplayer fact2) /= LeaderNull
                canBeSmart = any (smartFaction . snd)
                canBeOurFaction = any (\(fid2, _) -> fid2 == side)
                smartEnemy trunkFull =
                  let possible =
                        possibleActorFactions (itemKind trunkFull) factionD
                  in not (canBeOurFaction possible) && canBeSmart possible
                smartEnemiesOurs = filter (smartEnemy . snd) ourTrunks
                uniqueActor trunkFull = IA.checkFlag Ability.Unique
                                        $ aspectRecordFull trunkFull
                uniqueEnemiesOurs = filter (uniqueActor . snd) smartEnemiesOurs
                smartUniqueEnemyCaptured = not $ null uniqueEnemiesOurs
                smartEnemyCaptured = not $ null smartEnemiesOurs
            smartEnemySentence <- case uniqueEnemiesOurs ++ smartEnemiesOurs of
              [] -> return ""
              (enemyAid, _) : _ -> do
                bUI <- getsSession $ getActorUI enemyAid
                return $! makePhrase [MU.Capitalize (partActor bUI)] <> "?"
            let won = maybe False ((`elem` victoryOutcomes) . stOutcome) toSt
                (sp2, escPrompt) =
                  if | not won -> ("", "Accept the unacceptable?")
                     | smartUniqueEnemyCaptured ->
                       ( "\nOh, wait, who is this, towering behind your escaping crew?" <+> smartEnemySentence <+> "This changes everything. For everybody. Everywhere. Forever. Did you plan for this? Are you sure it was your idea?"
                       , "What happens now?" )
                     | smartEnemyCaptured ->
                       ( "\nOh, wait, who is this, hunched among your escaping crew?" <+> smartEnemySentence <+> "Suddenly, this makes your crazy story credible. Suddenly, the door of knowledge opens again."
                       , "How will you play that move?" )
                     | otherwise -> ("", "Let's see what we've got here.")
            msgAdd MsgPlot $ sp1 <> sp2
            void $ displaySpaceEsc ColorFull escPrompt
        case manalytics of
          Nothing -> return ()
          Just (factionAn, generationAn) ->
            cycleLore []
              [ displayGameOverLoot (itemBag, total) generationAn
              , displayGameOverLore SOrgan True generationAn
              , displayGameOverAnalytics factionAn generationAn
              , displayGameOverLore SCondition sexposeItems generationAn
              , displayGameOverLore SBlast True generationAn
              , displayGameOverLore SEmbed True generationAn ]
      unless noConfirmsGame $ do
        -- Show score for any UI client after any kind of game exit,
        -- even though it's saved only for human UI clients at game over
        -- (that is not a noConfirms or benchmark game).
        scoreSlides <- scoreToSlideshow total status
        void $ getConfirms ColorFull [K.spaceKM, K.escKM] scoreSlides
      -- The last prompt stays onscreen during shutdown, etc.
      when (not noConfirmsGame || camping) $
        void $ displaySpaceEsc ColorFull pp  -- these are short
    _ -> return ()

displayGameOverLoot :: MonadClientUI m
                    => (ItemBag, Int) -> GenerationAnalytics -> m K.KM
displayGameOverLoot (heldBag, total) generationAn = do
  ClientOptions{sexposeItems} <- getsClient soptions
  COps{coitem} <- getsState scops
  ItemSlots itemSlots <- getsSession sslots
  -- We assume "gold grain", not "grain" with label "of gold":
  let currencyName = IK.iname $ okind coitem $ ouniqGroup coitem IK.S_CURRENCY
      lSlotsRaw = EM.filter (`EM.member` heldBag) $ itemSlots EM.! SItem
      generationItem = generationAn EM.! SItem
      (itemBag, lSlots) =
        if sexposeItems
        then let generationBag = EM.map (\k -> (-k, [])) generationItem
                 bag = heldBag `EM.union` generationBag
                 slots = EM.fromDistinctAscList $ zip allSlots $ EM.keys bag
             in (bag, slots)
        else (heldBag, lSlotsRaw)
      promptFun iid itemFull2 k =
        let worth = itemPrice 1 $ itemKind itemFull2
            lootMsg = if worth == 0 then "" else
              let pile = if k == 1 then "exemplar" else "hoard"
              in makeSentence $
                   ["this treasure", pile, "is worth"]
                   ++ (if k > 1 then [ MU.Cardinal k, "times"] else [])
                   ++ [MU.CarWs worth $ MU.Text currencyName]
            holdsMsg =
              let n = generationItem EM.! iid
              in if | max 0 k == 1 && n == 1 ->
                      "You keep the only specimen extant:"
                    | max 0 k == 0 && n == 1 ->
                      "You don't have the only hypothesized specimen:"
                    | max 0 k == 0 && n == 0 ->
                      "No such specimen was recorded:"
                    | otherwise ->
                        makePhrase [ "You hold"
                                   , if k == n
                                     then "all pieces"
                                     else MU.CardinalAWs (max 0 k) "piece"
                                   , "out of"
                                   , MU.Car n
                                   , "scattered:" ]
        in lootMsg <+> holdsMsg
  dungeonTotal <- getsState sgold
  let promptGold = spoilsBlurb currencyName total dungeonTotal
      -- Total number of items is meaningless in the presence of so much junk.
      prompt = promptGold
               <+> (if sexposeItems
                    then "Non-positive count means none held but this many generated."
                    else "")
      examItem = displayItemLore itemBag 0 promptFun
  viewLoreItems "GameOverLoot" lSlots itemBag prompt examItem True

displayGameOverAnalytics :: MonadClientUI m
                         => FactionAnalytics -> GenerationAnalytics
                         -> m K.KM
displayGameOverAnalytics factionAn generationAn = do
  ClientOptions{sexposeActors} <- getsClient soptions
  side <- getsClient sside
  ItemSlots itemSlots <- getsSession sslots
  let ourAn = akillCounts
              $ EM.findWithDefault emptyAnalytics side factionAn
      foesAn = EM.unionsWith (+)
               $ concatMap EM.elems $ catMaybes
               $ map (`EM.lookup` ourAn) [KillKineticMelee .. KillOtherPush]
      trunkBagRaw = EM.map (, []) foesAn
      lSlotsRaw = EM.filter (`EM.member` trunkBagRaw) $ itemSlots EM.! STrunk
      killedBag = EM.fromList $ map (\iid -> (iid, trunkBagRaw EM.! iid))
                                    (EM.elems lSlotsRaw)
      generationTrunk = generationAn EM.! STrunk
      (trunkBag, lSlots) =
        if sexposeActors
        then let generationBag = EM.map (\k -> (-k, [])) generationTrunk
                 bag = killedBag `EM.union` generationBag
                 slots = EM.fromDistinctAscList $ zip allSlots $ EM.keys bag
             in (bag, slots)
        else (killedBag, lSlotsRaw)
      total = sum $ filter (> 0) $ map fst $ EM.elems trunkBag
      -- Not just "killed 1 out of 4", because it's sometimes "2 out of 1",
      -- if an enemy was revived.
      promptFun :: ItemId -> ItemFull-> Int -> Text
      promptFun iid _ k =
        let n = generationTrunk EM.! iid
        in makePhrase [ "You recall the adversary, which you killed on"
                      , MU.CarWs (max 0 k) "occasion", "while reports mention"
                      , MU.CarWs n "individual", "in total:" ]
      prompt = makeSentence ["your team vanquished", MU.CarWs total "adversary"]
                 -- total reported would include our own, so not given
               <+> (if sexposeActors
                    then "Non-positive count means none killed but this many reported."
                    else "")
      examItem = displayItemLore trunkBag 0 promptFun
  viewLoreItems "GameOverAnalytics" lSlots trunkBag prompt examItem False

displayGameOverLore :: MonadClientUI m
                    => SLore -> Bool -> GenerationAnalytics -> m K.KM
displayGameOverLore slore exposeCount generationAn = do
  let generationLore = generationAn EM.! slore
      generationBag = EM.map (\k -> (if exposeCount then k else 1, []))
                             generationLore
      total = sum $ map fst $ EM.elems generationBag
      slots = EM.fromDistinctAscList $ zip allSlots $ EM.keys generationBag
      promptFun :: ItemId -> ItemFull-> Int -> Text
      promptFun _ _ k =
        makeSentence
          [ "this", MU.Text (ppSLore slore), "manifested during your quest"
          , MU.CarWs k "time" ]
      prompt | total == 0 =
               makeSentence [ "you didn't experience any"
                            , MU.Ws $ MU.Text (headingSLore slore)
                            , "this time" ]
             | otherwise =
               makeSentence [ "you experienced the following variety of"
                            , MU.CarWs total $ MU.Text (headingSLore slore) ]
      examItem = displayItemLore generationBag 0 promptFun
      displayRanged = slore `notElem` [SOrgan, STrunk]
  viewLoreItems ("GameOverLore" ++ show slore)
                slots generationBag prompt examItem displayRanged

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
      let isOurOrgan = bfid bOwner == side && storeOwner == COrgan
            -- assume own faction organs known intuitively
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
    msgAdd MsgItemDisco msg

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
        adverb = MU.Text $ ppHearDistanceAdverb distance
        object = if p == 1  -- works, because exact number sent, not dice
                 then MU.Text $ fromGroupName grp
                 else MU.Ws $ MU.Text $ fromGroupName grp
    return $! makeSentence ["you", adverb, "hear", verb, object]
  HearCollideTile -> do
    let adverb = MU.Text $ ppHearDistanceAdverb distance
    return $! makeSentence ["you", adverb, "hear someone crash into something"]
  HearTaunt t -> do
    let adverb = MU.Text $ ppHearDistanceAdverb distance
    return $! makeSentence ["you", adverb, "overhear", MU.Text t]

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

-- * RespSfxAtomicUI

-- | Display special effects (text, animation) sent to the client.
-- Don't modify client state (except a few fields), but only client
-- session (e.g., by displaying messages). This is enforced by types.
displayRespSfxAtomicUI :: MonadClientUI m => SfxAtomic -> m ()
{-# INLINE displayRespSfxAtomicUI #-}
displayRespSfxAtomicUI sfx = case sfx of
  SfxStrike source target iid ->
    strike False source target iid
  SfxRecoil source target _ -> do
    spart <- partActorLeader source
    tpart <- partActorLeader target
    msgAdd MsgAction $
      makeSentence [MU.SubjectVerbSg spart "shrink away from", tpart]
  SfxSteal source target iid ->
    strike True source target iid
  SfxRelease source target _ -> do
    spart <- partActorLeader source
    tpart <- partActorLeader target
    msgAdd MsgAction $ makeSentence [MU.SubjectVerbSg spart "release", tpart]
  SfxProject aid iid ->
    itemAidVerbMU MsgAction aid "fling" iid (Left $ Just 1)
  SfxReceive aid iid ->
    itemAidVerbMU MsgAction aid "receive" iid (Left $ Just 1)
  SfxApply aid iid -> do
    CCUI{coscreen=ScreenContent{rapplyVerbMap}} <- getsSession sccui
    ItemFull{itemKind} <- getsState $ itemToFull iid
    let actionPart = case EM.lookup (IK.isymbol itemKind) rapplyVerbMap of
          Just verb -> MU.Text verb
          Nothing -> "trigger"
    itemAidVerbMU MsgAction aid actionPart iid (Left $ Just 1)
  SfxCheck aid iid ->
    itemAidVerbMU MsgAction aid "recover" iid (Left $ Just 1)
  SfxTrigger _ _ _ fromTile -> do
    COps{cotile} <- getsState scops
    let subject = MU.Text $ TK.tname $ okind cotile fromTile
        verb = "shake"
        msg = makeSentence ["the", MU.SubjectVerbSg subject verb]
    msgAdd MsgNeutralEvent msg
  SfxShun aid _ _ _ ->
    aidVerbMU MsgAction aid "shun it"
  SfxEffect fidSource aid effect hpDelta -> do
    b <- getsState $ getActorBody aid
    bUI <- getsSession $ getActorUI aid
    side <- getsClient sside
    mleader <- getsClient sleader
    let fid = bfid b
        isOurCharacter = fid == side && not (bproj b)
        isOurAlive = isOurCharacter && bhp b > 0
        isOurLeader = Just aid == mleader
        feelLookHP = feelLook MsgEffect
        feelLookCalm bigAdj projAdj =
          when (bhp b > 0) $ feelLook MsgEffectMinor bigAdj projAdj
        feelLook msgClass bigAdj projAdj =
          let (verb, adjective) =
                if bproj b
                then ("get", projAdj)
                else (if isOurCharacter then "feel" else "look", bigAdj)
          in aidVerbMU msgClass aid $ MU.Text $ verb <+> adjective
    case effect of
        IK.Burn{} -> do
          feelLookHP "burned" "scorched"
          let ps = (bpos b, bpos b)
          animate (blid b) $ twirlSplash ps Color.BrRed Color.Brown
        IK.Explode{} -> return ()  -- lots of visual feedback
        IK.RefillHP p | p == 1 -> return ()  -- no spam from regeneration
        IK.RefillHP p | p == -1 -> return ()  -- no spam from poison
        IK.RefillHP{} | hpDelta > 0 -> do
          feelLookHP "healthier" "mended"
          let ps = (bpos b, bpos b)
          animate (blid b) $ twirlSplash ps Color.BrGreen Color.Green
        IK.RefillHP{} -> do
          feelLookHP "wounded" "broken"
          let ps = (bpos b, bpos b)
          animate (blid b) $ twirlSplash ps Color.BrRed Color.Red
        IK.RefillCalm{} | bproj b -> return ()
        IK.RefillCalm p | p == 1 -> return ()  -- no spam from regen items
        IK.RefillCalm p | p > 0 -> feelLookCalm "calmer" "stabilized"
        IK.RefillCalm _ -> feelLookCalm "agitated" "wobbly"
        IK.Dominate -> do
          -- For subsequent messages use the proper name, never "you".
          let subject = partActor bUI
          if fid /= fidSource then do
            -- Before domination, possibly not seen if actor (yet) not ours.
            if | bcalm b == 0 ->  -- sometimes only a coincidence, but nm
                 aidVerbMU MsgEffectMinor aid
                 $ MU.Text "yield, under extreme pressure"
               | isOurAlive ->
                 aidVerbMU MsgEffectMinor aid
                 $ MU.Text "black out, dominated by foes"
               | otherwise ->
                 aidVerbMU MsgEffectMinor aid
                 $ MU.Text "decide abruptly to switch allegiance"
            fidNameRaw <- getsState $ gname . (EM.! fid) . sfactionD
            -- Avoid "controlled by Controlled foo".
            let fidName = T.unwords $ tail $ T.words fidNameRaw
                verb = "be no longer controlled by"
            msgLnAdd MsgEffectMajor $ makeSentence
              [MU.SubjectVerbSg subject verb, MU.Text fidName]
            when isOurAlive $ displayMoreKeep ColorFull ""  -- Ln makes it short
          else do
            -- After domination, possibly not seen, if actor (already) not ours.
            fidSourceNameRaw <- getsState $ gname . (EM.! fidSource) . sfactionD
            -- Avoid "Controlled control".
            let fidSourceName = T.unwords $ tail $ T.words fidSourceNameRaw
                verb = "be now under"
            msgAdd MsgEffectMajor $ makeSentence
              [MU.SubjectVerbSg subject verb, MU.Text fidSourceName, "control"]
        IK.Impress -> aidVerbMU MsgEffectMinor aid "be awestruck"
        IK.PutToSleep -> aidVerbMU MsgEffectMajor aid "be put to sleep"
        IK.Yell -> aidVerbMU MsgMisc aid "start"
        IK.Summon grp p -> do
          let verb = if bproj b then "lure" else "summon"
              object = (if p == 1  -- works, because exact number sent, not dice
                        then MU.AW
                        else MU.Ws) $ MU.Text $ fromGroupName grp
          aidVerbMU MsgEffectMajor aid $ MU.Phrase [verb, object]
        IK.Ascend up -> do
          COps{cocave} <- getsState scops
          aidVerbMU MsgEffectMajor aid $ MU.Text $
            "find a way" <+> if up then "upstairs" else "downstairs"
          when isOurLeader $ do
            destinations <- getsState $ whereTo (blid b) (bpos b) up
                                        . sdungeon
            case destinations of
              (lid, _) : _ -> do  -- only works until different levels possible
                lvl <- getLevel lid
                let desc = cdesc $ okind cocave $ lkind lvl
                unless (T.null desc) $
                  msgAdd MsgLandscape $ desc <> "\n"
              [] -> return ()  -- spell fizzles; normally should not be sent
        IK.Escape{} | isOurCharacter -> do
          ours <- getsState $ fidActorNotProjGlobalAssocs side
          when (length ours > 1) $ do
            let object = partActor bUI
            msgAdd MsgOutcome $
              "The team joins" <+> makePhrase [object]
              <> ", forms a perimeter, repacks its belongings and leaves triumphant."
        IK.Escape{} -> return ()
        IK.Paralyze{} -> aidVerbMU MsgEffect aid "be paralyzed"
        IK.ParalyzeInWater{} ->
          aidVerbMU MsgEffectMinor aid "move with difficulty"
        IK.InsertMove d ->
          if Dice.supDice d >= 10
          then aidVerbMU MsgEffect aid "act with extreme speed"
          else aidVerbMU MsgEffectMinor aid "move swiftly"
        IK.Teleport t | Dice.supDice t <= 9 ->
          aidVerbMU MsgEffectMinor aid "blink"
        IK.Teleport{} -> aidVerbMU MsgEffect aid "teleport"
        IK.CreateItem{} -> return ()
        IK.DestroyItem{} -> return ()
        IK.ConsumeItems{} -> return ()
        IK.DropItem _ _ COrgan _ -> return ()
        IK.DropItem{} -> aidVerbMU MsgEffect aid "be stripped"
        IK.Recharge{} -> aidVerbMU MsgEffect aid "heat up"
        IK.Discharge{} -> aidVerbMU MsgEffect aid "cool down"
        IK.PolyItem -> do
          subject <- partActorLeader aid
          let ppstore = MU.Text $ ppCStoreIn CGround
          msgAdd MsgEffect $ makeSentence
            [ MU.SubjectVerbSg subject "repurpose", "what lies", ppstore
            , "to a common item of the current level" ]
        IK.RerollItem -> do
          subject <- partActorLeader aid
          let ppstore = MU.Text $ ppCStoreIn CGround
          msgAdd MsgEffect $ makeSentence
            [ MU.SubjectVerbSg subject "reshape", "what lies", ppstore
            , "striving for the highest possible standards" ]
        IK.DupItem -> do
          subject <- partActorLeader aid
          let ppstore = MU.Text $ ppCStoreIn CGround
          msgAdd MsgEffect $ makeSentence
            [MU.SubjectVerbSg subject "multiply", "what lies", ppstore]
        IK.Identify -> do
          subject <- partActorLeader aid
          pronoun <- partPronounLeader aid
          msgAdd MsgEffectMinor $ makeSentence
            [ MU.SubjectVerbSg subject "look at"
            , MU.WownW pronoun $ MU.Text "inventory"
            , "intensely" ]
        IK.Detect d _ -> do
          subject <- partActorLeader aid
          let verb = MU.Text $ detectToVerb d
              object = MU.Ws $ MU.Text $ detectToObject d
          msgAdd MsgEffectMinor $
            makeSentence [MU.SubjectVerbSg subject verb, object]
          -- Don't make it modal if all info remains after no longer seen.
          unless (d `elem` [IK.DetectHidden, IK.DetectExit]) $
            displayMore ColorFull ""  -- the sentence short
        IK.SendFlying{} -> aidVerbMU MsgEffect aid "be sent flying"
        IK.PushActor{} -> aidVerbMU MsgEffect aid "be pushed"
        IK.PullActor{} -> aidVerbMU MsgEffect aid "be pulled"
        IK.ApplyPerfume ->
          msgAdd MsgEffectMinor
                 "The fragrance quells all scents in the vicinity."
        IK.OneOf{} -> return ()
        IK.OnSmash{} -> error $ "" `showFailure` sfx
        IK.OnCombine{} -> error $ "" `showFailure` sfx
        IK.OnUser{} -> error $ "" `showFailure` sfx
        IK.AndEffect{} -> error $ "" `showFailure` sfx
        IK.OrEffect{} -> error $ "" `showFailure` sfx
        IK.SeqEffect{} -> error $ "" `showFailure` sfx
        IK.VerbNoLonger t -> do
          let msgClass = if bfid b == side then MsgNoLongerUs else MsgNoLonger
          aidVerbMU msgClass aid $ MU.Text t
        IK.VerbMsg t -> aidVerbMU MsgEffectMinor aid $ MU.Text t
        IK.VerbMsgFail t -> aidVerbMU MsgWarning aid $ MU.Text t
  SfxMsgFid _ sfxMsg -> do
    mleader <- getsClient sleader
    case mleader of
      Just{} -> return ()  -- will display stuff when leader moves
      Nothing -> do
        lidV <- viewedLevelUI
        markDisplayNeeded lidV
        recordHistory
    mmsg <- ppSfxMsg sfxMsg
    case mmsg of
      Just (msgClass, msg) -> msgAdd msgClass msg
      Nothing -> return ()
  SfxRestart -> do
    fadeOutOrIn True
    relinquishFrontend  -- make sure the fadeout is not interspersed with fadein
  SfxCollideTile source pos -> do
    COps{cotile} <- getsState scops
    sb <- getsState $ getActorBody source
    lvl <- getLevel $ blid sb
    spart <- partActorLeader source
    let object = MU.AW $ MU.Text $ TK.tname $ okind cotile $ lvl `at` pos
    -- Neutral message, because minor damage and we don't say, which faction.
    msgAdd MsgNeutralEvent $! makeSentence
      [MU.SubjectVerbSg spart "collide", "painfully with", object]
  SfxTaunt voluntary aid -> do
    side <- getsClient sside
    b <- getsState $ getActorBody aid
    unless (bproj b && bfid b == side) $ do  -- don't spam
      spart <- partActorLeader aid
      (_heardSubject, verb) <- displayTaunt voluntary rndToActionUI aid
      msgAdd MsgMisc $! makeSentence [MU.SubjectVerbSg spart (MU.Text verb)]

ppSfxMsg :: MonadClientUI m => SfxMsg -> m (Maybe (MsgClass, Text))
ppSfxMsg sfxMsg = case sfxMsg of
  SfxUnexpected reqFailure -> return $
    Just ( MsgWarning
         , "Unexpected problem:" <+> showReqFailure reqFailure <> "." )
  SfxExpected itemName reqFailure -> return $
    Just ( MsgWarning
         , "The" <+> itemName <+> "is not triggered:"
           <+> showReqFailure reqFailure <> "." )
  SfxExpectedEmbed iid lid reqFailure -> do
    iidSeen <- getsState $ EM.member iid . sitemD
    if iidSeen then do
      itemFull <- getsState $ itemToFull iid
      side <- getsClient sside
      factionD <- getsState sfactionD
      localTime <- getsState $ getLocalTime lid
      let (object1, object2) =
            partItemShortest maxBound side factionD localTime
                             itemFull quantSingle
          name = makePhrase [object1, object2]
      return $
        Just ( MsgWarning
             , "The" <+> "embedded" <+> name <+> "is not activated:"
               <+> showReqFailure reqFailure <> "." )
    else return Nothing
  SfxFizzles -> return $ Just (MsgWarning, "It didn't work.")
  SfxNothingHappens -> return $ Just (MsgMisc, "Nothing happens.")
  SfxNoItemsForTile toolsToAlterWith -> do
    revCmd <- revCmdMap
    let km = revCmd HumanCmd.AlterDir
        tItems = describeToolsAlternative toolsToAlterWith
    return $ Just ( MsgWarning
                  , "To transform the terrain, prepare the following items on the ground or in equipment:"
                    <+> tItems
                    <+> "and use the <"
                    <> T.pack (K.showKM km)
                    <> "> terrain modification command."
                  )
  SfxVoidDetection d -> return $
    Just ( MsgMisc
         , makeSentence ["no new", MU.Text $ detectToObject d, "detected"] )
  SfxUnimpressed aid -> do
    msbUI <- getsSession $ EM.lookup aid . sactorUI
    case msbUI of
      Nothing -> return Nothing
      Just sbUI -> do
        let subject = partActor sbUI
            verb = "be unimpressed"
        return $ Just (MsgWarning, makeSentence [MU.SubjectVerbSg subject verb])
  SfxSummonLackCalm aid -> do
    msbUI <- getsSession $ EM.lookup aid . sactorUI
    case msbUI of
      Nothing -> return Nothing
      Just sbUI -> do
        let subject = partActor sbUI
            verb = "lack Calm to summon"
        return $ Just (MsgWarning, makeSentence [MU.SubjectVerbSg subject verb])
  SfxSummonTooManyOwn aid -> do
    msbUI <- getsSession $ EM.lookup aid . sactorUI
    case msbUI of
      Nothing -> return Nothing
      Just sbUI -> do
        let subject = partActor sbUI
            verb = "can't keep track of their numerous friends, let alone summon any more"
        return $ Just (MsgWarning, makeSentence [subject, verb])
  SfxSummonTooManyAll aid -> do
    msbUI <- getsSession $ EM.lookup aid . sactorUI
    case msbUI of
      Nothing -> return Nothing
      Just sbUI -> do
        let subject = partActor sbUI
            verb = "can't keep track of everybody around, let alone summon anyone else"
        return $ Just (MsgWarning, makeSentence [subject, verb])
  SfxSummonFailure aid -> do
    msbUI <- getsSession $ EM.lookup aid . sactorUI
    case msbUI of
      Nothing -> return Nothing
      Just sbUI -> do
        let subject = partActor sbUI
            verb = "fail to summon anything"
        return $ Just (MsgWarning, makeSentence [MU.SubjectVerbSg subject verb])
  SfxLevelNoMore ->
    return $ Just (MsgWarning, "No more levels in this direction.")
  SfxLevelPushed ->
    return $ Just (MsgWarning, "You notice somebody pushed to another level.")
  SfxBracedImmune aid -> do
    msbUI <- getsSession $ EM.lookup aid . sactorUI
    case msbUI of
      Nothing -> return Nothing
      Just sbUI -> do
        let subject = partActor sbUI
            verb = "be braced and so immune to translocation"
        return $ Just (MsgMisc, makeSentence [MU.SubjectVerbSg subject verb])
                         -- too common
  SfxEscapeImpossible -> return $
    Just ( MsgWarning
         , "Escaping outside is unthinkable for members of this faction." )
  SfxStasisProtects -> return $
    Just ( MsgMisc  -- too common
         , "Paralysis and speed surge require recovery time." )
  SfxWaterParalysisResisted -> return Nothing  -- don't spam
  SfxTransImpossible -> return $
    Just (MsgWarning, "Translocation not possible.")
  SfxIdentifyNothing -> return $ Just (MsgWarning, "Nothing to identify.")
  SfxPurposeNothing -> return $
    Just ( MsgWarning
         , "The purpose of repurpose cannot be availed without an item"
           <+> ppCStoreIn CGround <> "." )
  SfxPurposeTooFew maxCount itemK -> return $
    Just ( MsgWarning
         , "The purpose of repurpose is served by" <+> tshow maxCount
           <+> "pieces of this item, not by" <+> tshow itemK <> "." )
  SfxPurposeUnique -> return $
    Just (MsgWarning, "Unique items can't be repurposed.")
  SfxPurposeNotCommon -> return $
    Just (MsgWarning, "Only ordinary common items can be repurposed.")
  SfxRerollNothing -> return $
    Just ( MsgWarning
         , "The shape of reshape cannot be assumed without an item"
           <+> ppCStoreIn CGround <> "." )
  SfxRerollNotRandom -> return $
    Just (MsgWarning, "Only items of variable shape can be reshaped.")
  SfxDupNothing -> return $
    Just ( MsgWarning
         , "Mutliplicity won't rise above zero without an item"
           <+> ppCStoreIn CGround <> "." )
  SfxDupUnique -> return $
    Just (MsgWarning, "Unique items can't be multiplied.")
  SfxDupValuable -> return $
    Just (MsgWarning, "Valuable items can't be multiplied.")
  SfxColdFish -> return $
    Just ( MsgMisc  -- repeatable
         , "Healing attempt from another faction is thwarted by your cold fish attitude." )
  SfxTimerExtended aid iid cstore delta -> do
    CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
    aidSeen <- getsState $ EM.member aid . sactorD
    iidSeen <- getsState $ EM.member iid . sitemD
    if aidSeen && iidSeen then do
      b <- getsState $ getActorBody aid
      bUI <- getsSession $ getActorUI aid
        -- assume almost always a prior message mentions the object
      factionD <- getsState sfactionD
      localTime <- getsState $ getLocalTime (blid b)
      itemFull <- getsState $ itemToFull iid
      side <- getsClient sside
      bag <- getsState $ getBodyStoreBag b cstore
      let (name, powers) =
            partItem rwidth (bfid b) factionD localTime itemFull quantSingle
          total = case bag EM.! iid of
            (_, []) -> error $ "" `showFailure` (bag, iid, aid, cstore, delta)
            (_, t:_) -> deltaOfItemTimer localTime t
              -- only exceptionally not singleton list
      storeOwn <- ppContainerWownW partPronounLeader True (CActor aid cstore)
      let cond = [ "condition"
                 | IA.checkFlag Ability.Condition $ aspectRecordFull itemFull ]
          arItem = aspectRecordFull itemFull
          isBlast = IA.checkFlag Ability.Blast arItem
          -- Note that when enemy actor causes the extension to himself,
          -- the player is not notified at all. So the shorter blurb below
          -- is the middle ground.
          (msgClass, parts) | bfid b == side && not isBlast =
            ( MsgLongerUs
            , ["the", name, powers] ++ cond ++ storeOwn ++ ["will now last"]
              ++ [MU.Text $ timeDeltaInSecondsText delta <+> "longer"]
              ++ [MU.Text $ "(total:" <+> timeDeltaInSecondsText total <> ")"] )
                            | otherwise =
            -- Avoid TMI for not our actors and for explosions, for which
            -- the totals defeat merging of similar messages.
            --
            -- Ideally we'd use a pronoun here, but the action (e.g., hit)
            -- that caused this extension can be invisible to some onlookers.
            -- So their narrative context needs to be taken into account.
            ( MsgLonger
            , [partItemShortWownW rwidth side factionD (partActor bUI) localTime
                                  itemFull quantSingle]
              ++ cond ++ ["is extended"] )
      return $ Just (msgClass, makeSentence parts)
    else return Nothing
  SfxCollideActor source target -> do
    sourceSeen <- getsState $ EM.member source . sactorD
    targetSeen <- getsState $ EM.member target . sactorD
    if sourceSeen && targetSeen then do
      spart <- partActorLeader source
      tpart <- partActorLeader target
      -- Neutral message, because minor damage and we don't say, which faction.
      -- And the collision may even be intentional.
      return $
        Just ( MsgNeutralEventRare
             , makeSentence
                 [MU.SubjectVerbSg spart "collide", "awkwardly with", tpart] )
    else return Nothing
  SfxItemYield iid k lid -> do
    iidSeen <- getsState $ EM.member iid . sitemD
    if iidSeen then do
      let fakeKit = quantSingle
          fakeC = CFloor lid originPoint
          verb = MU.Text $ "yield" <+> makePhrase [MU.CardinalAWs k "item"]
      msg <- itemVerbMUGeneral False iid fakeKit verb fakeC
      return $ Just (MsgItemCreation, msg)
    else return Nothing

strike :: MonadClientUI m => Bool -> ActorId -> ActorId -> ItemId -> m ()
strike catch source target iid = assert (source /= target) $ do
  CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
  tb <- getsState $ getActorBody target
  sourceSeen <- getsState $ EM.member source . sactorD
  if not sourceSeen then
    animate (blid tb) $ blockMiss (bpos tb, bpos tb)
  else do
    hurtMult <- getsState $ armorHurtBonus source target
    sb <- getsState $ getActorBody source
    sMaxSk <- getsState $ getActorMaxSkills source
    spart <- partActorLeader source
    tpart <- partActorLeader target
    spronoun <- partPronounLeader source
    tpronoun <- partPronounLeader target
    tbUI <- getsSession $ getActorUI target
    localTime <- getsState $ getLocalTime (blid tb)
    itemFullWeapon <- getsState $ itemToFull iid
    let kitWeapon = quantSingle
    side <- getsClient sside
    factionD <- getsState sfactionD
    tfact <- getsState $ (EM.! bfid tb) . sfactionD
    eqpOrgKit <- getsState $ kitAssocs target [CEqp, COrgan]
    orgKit <- getsState $ kitAssocs target [COrgan]
    let notCond (_, (itemFullArmor, _)) =
          not $ IA.checkFlag Ability.Condition $ aspectRecordFull itemFullArmor
        isOrdinaryCond (_, (itemFullArmor, _)) =
          isJust $ lookup IK.CONDITION $ IK.ifreq $ itemKind itemFullArmor
        relevantSkArmor =
          if bproj sb then Ability.SkArmorRanged else Ability.SkArmorMelee
        rateArmor (iidArmor, (itemFullArmor, (k, _))) =
          ( k * IA.getSkill relevantSkArmor (aspectRecordFull itemFullArmor)
          , ( iidArmor
            , itemFullArmor ) )
        abs15 (v, _) = abs v >= 15
        condArmor = filter abs15 $ map rateArmor $ filter isOrdinaryCond orgKit
        fstGt0 (v, _) = v > 0
        wornArmor = filter fstGt0 $ map rateArmor $ filter notCond eqpOrgKit
    mblockArmor <- case wornArmor of
      [] -> return Nothing
      _ -> Just
           <$> rndToActionUI (frequency $ toFreq "msg armor" wornArmor)
    let (blockWithWhat, blockWithWeapon) = case mblockArmor of
          Just (iidArmor, itemFullArmor) | iidArmor /= btrunk tb ->
            let (object1, object2) =
                  partItemShortest rwidth (bfid tb) factionD localTime
                                   itemFullArmor quantSingle
                name = MU.Phrase [object1, object2]
            in ( ["with", MU.WownW tpronoun name]
               , Dice.supDice (IK.idamage $ itemKind itemFullArmor) > 0 )
          _ -> ([], False)
        verb = MU.Text $ IK.iverbHit $ itemKind itemFullWeapon
        partItemChoice =
          if iid `EM.member` borgan sb
          then partItemShortWownW rwidth side factionD spronoun localTime
          else partItemShortAW rwidth side factionD localTime
        weaponNameWith = if iid == btrunk tb
                         then []
                         else ["with", partItemChoice itemFullWeapon kitWeapon]
        sleepy = if bwatch tb `elem` [WSleep, WWake]
                    && tpart /= "you"
                    && bhp tb > 0
                 then "the sleepy"
                 else ""
        unBurn (IK.Burn d) = Just d
        unBurn _ = Nothing
        unRefillHP (IK.RefillHP n) | n < 0 = Just (-n)
        unRefillHP _ = Nothing
        kineticDmg =
          let dmg = Dice.supDice $ IK.idamage $ itemKind itemFullWeapon
              rawDeltaHP = intCast sHurt * xM dmg `divUp` 100
          in case btrajectory sb of
            Just (_, speed) | bproj sb ->
              - modifyDamageBySpeed rawDeltaHP speed
            _ -> - rawDeltaHP
        burnDmg = - (sum $ map Dice.supDice
                     $ mapMaybe unBurn $ IK.ieffects $ itemKind itemFullWeapon)
        hpDmg =
          - (sum $ mapMaybe unRefillHP $ IK.ieffects $ itemKind itemFullWeapon)
        -- For variety, attack adverb is based on attacker's and weapon's
        -- damage potential as compared to victim's current HP.
        -- We are not taking into account victim's armor yet.
        sHurt = armorHurtCalculation (bproj sb) sMaxSk Ability.zeroSkills
        sDamage = min 0 $ kineticDmg + xM (burnDmg + hpDmg)
        deadliness = 1000 * (- sDamage) `div` max 1 (bhp tb)
        strongly
          | deadliness >= 10000 = "artfully"
          | deadliness >= 5000 = "madly"
          | deadliness >= 2000 = "mercilessly"
          | deadliness >= 1000 = "murderously"  -- one blow can wipe out all HP
          | deadliness >= 700 = "devastatingly"
          | deadliness >= 500 = "vehemently"
          | deadliness >= 400 = "forcefully"
          | deadliness >= 350 = "sturdily"
          | deadliness >= 300 = "accurately"
          | deadliness >= 20 = ""  -- common, terse case, between 2% and 30%
          | deadliness >= 10 = "cautiously"
          | deadliness >= 5 = "guardedly"
          | deadliness >= 3 = "hesitantly"
          | deadliness >= 2 = "clumsily"
          | deadliness >= 1 = "haltingly"
          | otherwise = "feebly"
        -- Here we take into account armor, so we look at @hurtMult@,
        -- so we finally convey the full info about effectiveness of the strike.
        blockHowWell  -- under some conditions, the message not shown at all
          | hurtMult > 90 = "incompetently"
          | hurtMult > 80 = "too late"
          | hurtMult > 70 = "too slowly"
          | hurtMult > 20 = if | deadliness >= 2000 -> "marginally"
                               | deadliness >= 1000 -> "partially"
                               | deadliness >= 100 -> "partly"  -- common
                               | deadliness >= 50 -> "to an extent"
                               | deadliness >= 20 -> "to a large extent"
                               | deadliness >= 5 -> "for the major part"
                               | otherwise -> "for the most part"
          | hurtMult > 1 = if | actorWaits tb -> "doggedly"
                              | hurtMult > 10 -> "nonchalantly"
                              | otherwise -> "bemusedly"
          | otherwise = "almost completely"
              -- a fraction gets through, but if fast missile, can be deadly
        blockPhrase =
          let (subjectBlock, verbBlock) =
                if | not $ bproj sb ->
                     (tpronoun, if blockWithWeapon
                                then "parry"
                                else "block")
                   | tpronoun == "it"
                     || projectileHitsWeakly && tpronoun /= "you" ->
                     -- Avoid ambiguity.
                     (partActor tbUI, if actorWaits tb
                                      then "deflect it"
                                      else "fend it off")
                   | otherwise ->
                     (tpronoun, if actorWaits tb
                                then "avert it"
                                else "ward it off")
          in MU.SubjectVerbSg subjectBlock verbBlock
        surprisinglyGoodDefense = deadliness >= 20 && hurtMult <= 70
        surprisinglyBadDefense = deadliness < 20 && hurtMult > 70
        yetButAnd
          | surprisinglyGoodDefense = ", but"
          | surprisinglyBadDefense = ", yet"
          | otherwise = " and"  -- no surprises
        projectileHitsWeakly = bproj sb && deadliness < 20
        msgArmor = if not projectileHitsWeakly
                        -- ensures if attack msg terse, armor message
                        -- mentions object, so we know who is hit
                      && hurtMult > 90
                        -- at most minor armor relatively to skill of the hit
                      && (null condArmor || deadliness < 100)
                      || null blockWithWhat
                      || kineticDmg >= -1000  -- -1/1000 HP
                   then ""
                   else yetButAnd
                        <+> makePhrase ([blockPhrase, blockHowWell]
                                        ++ blockWithWhat)
        ps = (bpos tb, bpos sb)
        basicAnim
          | hurtMult > 70 = twirlSplash ps Color.BrRed Color.Red
          | hurtMult > 1 = if burnDmg >= 0 && hpDmg >= 0  -- no extra anim
                           then blockHit ps Color.BrRed Color.Red
                           else blockMiss ps
          | otherwise = blockMiss ps
        targetIsFoe = bfid sb == side  -- no big news if others hit our foes
                      && isFoe (bfid tb) tfact side
        targetIsFriend = isFriend (bfid tb) tfact side
                           -- warning if anybody hits our friends
        msgClassMelee = if targetIsFriend then MsgMeleeUs else MsgMelee
        msgClassRanged = if targetIsFriend then MsgRangedUs else MsgRanged
    -- The messages about parrying and immediately afterwards dying
    -- sound goofy, but there is no easy way to prevent that.
    -- And it's consistent.
    -- If/when death blow instead sets HP to 1 and only the next below 1,
    -- we can check here for HP==1; also perhaps actors with HP 1 should
    -- not be able to block.
    if | catch -> do  -- charge not needed when catching
         let msg = makeSentence
                     [MU.SubjectVerbSg spart "catch", tpart, "skillfully"]
         msgAdd MsgNeutralEventRare msg
         animate (blid tb) $ blockHit ps Color.BrGreen Color.Green
       | not (hasCharge localTime kitWeapon) -> do
         -- Can easily happen with a thrown discharged item.
         -- Much less plausible with a wielded weapon.
         -- Theoretically possible if the weapon not identified
         -- (then timeout is a mean estimate), but they usually should be,
         -- even in foes' possession.
         let msg = if bproj sb
                   then makePhrase
                          [MU.Capitalize $ MU.SubjectVerbSg spart "connect"]
                        <> ", but it may be completely discharged."
                   else makePhrase
                          ([ MU.Capitalize $ MU.SubjectVerbSg spart "try"
                           , "to"
                           , verb
                           , tpart ]
                           ++ weaponNameWith)
                        <> if null weaponNameWith
                           then ", but there are no charges left."
                           else ", but it may be not readied yet."
         msgAdd MsgNeutralEventRare msg  -- and no animation
       | bproj sb && bproj tb -> do  -- server sends unless both are blasts
         -- Short message.
         msgAdd MsgNeutralEventRare $
           makeSentence [MU.SubjectVerbSg spart "intercept", tpart]
         -- Basic non-bloody animation regardless of stats.
         animate (blid tb) $ blockHit ps Color.BrBlue Color.Blue
       | kineticDmg >= -1000  -- -1/1000 HP
         -- We ignore nested effects, because they are, in general, avoidable.
         && burnDmg >= 0 && hpDmg >= 0 -> do
         let adverb | itemSuspect itemFullWeapon && bfid sb == side =
                        "tentatively"  -- we didn't identify the weapon before
                    | bproj sb = "lightly"
                    | otherwise = "delicately"
             msg = makeSentence $
               [MU.SubjectVerbSg spart verb, tpart, adverb]
               ++ if bproj sb then [] else weaponNameWith
         msgAdd msgClassMelee msg  -- too common for color
         animate (blid tb) $ subtleHit ps
       | bproj sb -> do  -- more terse than melee, because sometimes very spammy
         let msgRangedPowerful | targetIsFoe = MsgRangedPowerfulWe
                               | targetIsFriend = MsgRangedPowerfulUs
                               | otherwise = msgClassRanged
             (attackParts, msgRanged)
               | projectileHitsWeakly =
                 ( [MU.SubjectVerbSg spart "connect"]  -- weak, so terse
                 , msgClassRanged )
               | deadliness >= 300 =
                 ( [MU.SubjectVerbSg spart verb, tpart, "powerfully"]
                 , if targetIsFriend || deadliness >= 700
                   then msgRangedPowerful
                   else msgClassRanged )
               | otherwise =
                 ( [MU.SubjectVerbSg spart verb, tpart]  -- strong, for a proj
                 , msgClassRanged )
         msgAdd msgRanged $ makePhrase [MU.Capitalize $ MU.Phrase attackParts]
                            <> msgArmor <> "."
         animate (blid tb) basicAnim
       | bproj tb -> do  -- much less emotion and the victim not active.
         let attackParts =
               [MU.SubjectVerbSg spart verb, tpart] ++ weaponNameWith
         msgAdd MsgMelee $ makeSentence attackParts
         animate (blid tb) basicAnim
       | otherwise -> do  -- ordinary melee
         let msgMeleeInteresting | targetIsFoe = MsgMeleeInterestingWe
                                 | targetIsFriend = MsgMeleeInterestingUs
                                 | otherwise = msgClassMelee
             msgMeleePowerful | targetIsFoe = MsgMeleePowerfulWe
                              | targetIsFriend = MsgMeleePowerfulUs
                              | otherwise = msgClassMelee
             attackParts =
               [MU.SubjectVerbSg spart verb, sleepy, tpart, strongly]
               ++ weaponNameWith
             (tmpInfluenceBlurb, msgClassInfluence) =
               if null condArmor || T.null msgArmor
               then ("", msgClassMelee)
               else
                 let (armor, (_, itemFullArmor)) =
                       maximumBy (comparing $ abs . fst) condArmor
                     (object1, object2) =
                       partItemShortest rwidth (bfid tb) factionD localTime
                                        itemFullArmor quantSingle
                     name = makePhrase [object1, object2]
                     msgText =
                       if hurtMult > 20 && not surprisinglyGoodDefense
                          || surprisinglyBadDefense
                       then (if armor <= -15
                             then ", due to being"
                             else assert (armor >= 15) ", regardless of being")
                            <+> name
                       else (if armor >= 15
                             then ", thanks to being"
                             else assert (armor <= -15) ", despite being")
                            <+> name
                 in (msgText, msgMeleeInteresting)
             msgClass = if targetIsFriend && deadliness >= 300
                           || deadliness >= 2000
                        then msgMeleePowerful
                        else msgClassInfluence
         msgAdd msgClass $ makePhrase [MU.Capitalize $ MU.Phrase attackParts]
                           <> msgArmor <> tmpInfluenceBlurb <> "."
         animate (blid tb) basicAnim
