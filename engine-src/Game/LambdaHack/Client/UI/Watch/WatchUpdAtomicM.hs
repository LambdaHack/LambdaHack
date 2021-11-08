-- | Display atomic update commands received by the client.
module Game.LambdaHack.Client.UI.Watch.WatchUpdAtomicM
  ( watchRespUpdAtomicUI
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , assignItemRole, Threat, createActorUI, destroyActorUI, spotItemBag
  , recordItemLid, moveActor, displaceActorUI, moveItemUI
  , discover, ppHearMsg, ppHearDistanceAdjective, ppHearDistanceAdverb
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Concurrent (threadDelay)
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M
import qualified Data.Text as T
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
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.FrameM
import           Game.LambdaHack.Client.UI.HandleHelperM
import           Game.LambdaHack.Client.UI.ItemDescription
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.MsgM
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.SlideshowM
import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Client.UI.Watch.WatchCommonM
import           Game.LambdaHack.Client.UI.Watch.WatchQuitM
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Content.CaveKind (cdesc)
import           Game.LambdaHack.Content.FactionKind
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Content.ModeKind as MK
import           Game.LambdaHack.Content.RuleKind
import qualified Game.LambdaHack.Content.TileKind as TK
import           Game.LambdaHack.Core.Random
import qualified Game.LambdaHack.Definition.Ability as Ability
import qualified Game.LambdaHack.Definition.Color as Color
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.Flavour

-- | Visualize atomic updates sent to the client. This is done
-- in the global state after the command is executed and after
-- the client state is modified by the command.
-- Doesn't modify client state (except a few fields), but only client
-- session (e.g., by displaying messages). This is enforced by types.
watchRespUpdAtomicUI :: MonadClientUI m => UpdAtomic -> m ()
{-# INLINE watchRespUpdAtomicUI #-}
watchRespUpdAtomicUI cmd = case cmd of
  -- Create/destroy actors and items.
  UpdRegisterItems{} -> return ()
  UpdCreateActor aid body _ -> createActorUI True aid body
  UpdDestroyActor aid body _ -> destroyActorUI True aid body
  UpdCreateItem verbose iid _ kit@(kAdd, _) c -> do
    recordItemLid iid c
    assignItemRole c iid
    when verbose $ case c of
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
                     verbShow = MU.Text $
                       "become"
                       <+> case kit of
                         (1, _ : _) -> "somewhat"
                         (1, []) | isNothing more -> ""
                         _ | isNothing more -> "many-fold"
                         _ -> "additionally"
                     verbSave = MU.Text $
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
                     good = benInEqp (discoBenefit EM.! iid)
                     msgClass = case lookup IK.S_ASLEEP $ IK.ifreq itemKind of
                       Just n | n > 0 -> MsgStatusSleep
                       _ -> if | bfid b /= side -> MsgStatusOthers
                               | good -> MsgStatusGoodUs
                               | otherwise -> MsgStatusBadUs
                 -- This describes all such items already among organs,
                 -- which is useful, because it shows "charging".
                 itemAidDistinctMU msgClass aid verbShow verbSave iid
                 when (bfid b == side && not good) $
                   -- Others get conditions too often and good ones are not
                   -- dire enough and also too common.
                   msgAdd MsgTutorialHint "Temporary conditions, especially the bad ones, pass quickly, usually after just a few turns. While active, they are listed in the '@' organ menu and the effects of most of them are seen in the '#' skill menu."
               | otherwise -> do
                 wown <- ppContainerWownW partActorLeader True c
                 itemVerbMU MsgItemCreation iid kit
                            (MU.Text $ makePhrase $ "grow" : wown) c
          _ -> do
            wown <- ppContainerWownW partActorLeader True c
            itemVerbMU MsgItemCreation iid kit
                       (MU.Text $ makePhrase $ "appear" : wown) c
      CEmbed{} -> return ()  -- not visible so can't delay even if important
      CFloor lid _ -> do
        factionD <- getsState sfactionD
        itemVerbMU MsgItemCreation iid kit
                   (MU.Text $ "appear" <+> ppContainer factionD c) c
        markDisplayNeeded lid
      CTrunk{} -> return ()
  UpdDestroyItem verbose iid _ kit c ->
    when verbose $ case c of
      CActor aid _  -> do
        b <- getsState $ getActorBody aid
        if bproj b then
          itemVerbMUShort MsgItemRuination iid kit "break" c
        else do
          ownW <- ppContainerWownW partActorLeader False c
          let verb = MU.Text $ makePhrase $ "vanish from" : ownW
          itemVerbMUShort MsgItemRuination iid kit verb c
      CEmbed{} -> return ()  -- not visible so can't delay even if important
      CFloor lid _ -> do
        factionD <- getsState sfactionD
        itemVerbMUShort MsgItemRuination iid kit
                        (MU.Text $ "break" <+> ppContainer factionD c) c
        markDisplayNeeded lid
      CTrunk{} -> return ()
  UpdSpotActor aid body -> createActorUI False aid body
  UpdLoseActor aid body -> destroyActorUI False aid body
  UpdSpotItem verbose iid kit c -> spotItemBag verbose c $ EM.singleton iid kit
  UpdLoseItem True iid kit c@(CActor aid _) -> do
    b <- getsState $ getActorBody aid
    when (not (bproj b) && bhp b > 0) $ do  -- don't spam
      ownW <- ppContainerWownW partActorLeader False c
      let verb = MU.Text $ makePhrase $ "be removed from" : ownW
      itemVerbMUShort MsgItemMovement iid kit verb c
  UpdLoseItem{} -> return ()
  UpdSpotItemBag verbose c bag -> spotItemBag verbose c bag
  UpdLoseItemBag{} -> return ()  -- rarely interesting and can be very long
  -- Move actors and items.
  UpdMoveActor aid source target -> moveActor aid source target
  UpdWaitActor aid WSleep _ -> do
    aidVerbMU MsgStatusWakeup aid "wake up"
    msgAdd MsgTutorialHint "Woken up actors regain stats and skills, including sight radius and melee armor, over several turns."
  UpdWaitActor aid WWake _ -> do
    side <- getsClient sside
    b <- getsState $ getActorBody aid
    unless (bfid b == side) $
      msgAdd MsgTutorialHint "To avoid waking enemies up, make sure they don't lose HP nor too much Calm through noises, particularly close ones. Beware, however, that they slowly regenerate HP as they sleep and eventually wake up at full HP."
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
    b <- getsState $ getActorBody aid
    unless (bproj b) $
      aidVerbMU MsgNumericReport aid $ MU.Text
                ((if hpDelta > 0 then "heal" else "lose") <+> tDelta)
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
             -- Rarely, this is wrong, because 2 other actors hit the victim
             -- at exactly the same time. No big problem. Doubled "dies"
             -- messages appears instead of "dies; is mutilated".
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
                      | targetIsFoe = MsgDeathVictory
                      | targetIsFriend = MsgDeathDeafeat
                      | otherwise = MsgDeathBoring
         if | bproj b -> msgAdd msgClass msgDie
            | bfid b == side -> do
              msgLnAdd msgClass $ msgDie <+> "Alas!"
              displayMore ColorBW ""
            | otherwise -> msgLnAdd msgClass msgDie
         -- We show death anims only if not dead already before this refill.
         let deathAct = if bfid b == side
                        then deathBody (bpos b)
                        else shortDeathBody (bpos b)
         unless (bproj b || alreadyDeadBefore) $ animate (blid b) deathAct
       | otherwise -> do
         when (hpDelta >= bhp b && bhp b > 0) $
           aidVerbMU MsgActionWarning aid "return from the brink of death"
         mleader <- getsClient sleader
         when (Just aid == mleader) $ do
           actorMaxSk <- getsState $ getActorMaxSkills aid
           -- Regenerating actors never stop gaining HP, so we need to stop
           -- reporting it after they reach full HP for the first time.
           -- Also, no spam for non-leaders.
           when (bhp b >= xM (Ability.getSk Ability.SkMaxHP actorMaxSk)
                 && bhp b - hpDelta < xM (Ability.getSk Ability.SkMaxHP
                                                  actorMaxSk)) $
             msgAdd MsgSpecialEvent "You recover your health fully. Any further gains will be transient."
         when (bfid b == side && not (bproj b)) $ do
           when (abs hpDelta >= oneM) $ markDisplayNeeded (blid b)
           when (hpDelta < 0) $ do
             when (hpDelta <= xM (-3)) $ msgAdd MsgTutorialHint "You took a lot of damage from one source. If the danger persists, consider retreating towards your teammates or buffing up or an instant escape, if consumables permit."
             sUIOptions <- getsSession sUIOptions
             currentWarning <-
               getsState $ checkWarningHP sUIOptions aid (bhp b)
             when currentWarning $ do
               previousWarning <-
                 getsState $ checkWarningHP sUIOptions aid (bhp b - hpDelta)
               unless previousWarning $
                 aidVerbMU MsgRiskOfDeath aid
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
           -- If the leader regenerates Calm more often than once per
           -- standard game turn, this will not be reflected, for smoother
           -- and faster display. However, every halt for keypress
           -- shows Calm, so this only matters for macros, where speed is good.
           when (abs calmDelta > oneM) $ markDisplayNeeded (blid b)
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
             duplicated <- aidVerbDuplicateMU MsgHeardNearby aid
                                              "hear something"
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
            aidVerbMU MsgRiskOfDeath aid
                      "have grown agitated and impressed enough to be in danger of defecting"
  UpdTrajectory _ _ mt ->  -- if projectile dies just after, force one frame
    when (isNothing mt) $ pushFrame False
  -- Change faction attributes.
  UpdQuitFaction fid _ toSt manalytics -> quitFactionUI fid toSt manalytics
  UpdSpotStashFaction verbose fid lid pos -> do
    side <- getsClient sside
    when verbose $ do
      if fid == side then
        msgLnAdd MsgFactionIntel
                 "You set up the shared inventory stash of your team."
      else do
        fact <- getsState $ (EM.! fid) . sfactionD
        let fidName = MU.Text $ gname fact
        msgAdd MsgFactionIntel $
          makeSentence [ "you have found the current"
                       , MU.WownW fidName "hoard location" ]
    unless (fid == side) $
      animate lid $ actorX pos
  UpdLoseStashFaction verbose fid lid pos -> do
    when verbose $ do
      side <- getsClient sside
      if fid == side then
        msgAdd MsgFactionIntel
               "You've lost access to your shared inventory stash!"
      else do
        fact <- getsState $ (EM.! fid) . sfactionD
        let fidName = MU.Text $ gname fact
        msgAdd MsgFactionIntel $
          makeSentence [fidName, "no longer control their hoard"]
    animate lid $ vanish pos
  UpdLeadFaction fid (Just source) mtgt@(Just target) -> do
    mleader <- getsClient sleader
    when (mtgt /= mleader) $ do
      fact <- getsState $ (EM.! fid) . sfactionD
      lidV <- viewedLevelUI
      when (gunderAI fact) $ markDisplayNeeded lidV
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
          msgAdd MsgPointmanSwap $
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
    msgAdd MsgFactionIntel $
      name1 <+> "and" <+> name2 <+> "are now" <+> showDipl toDipl <> "."
  UpdDoctrineFaction{} -> return ()
  UpdAutoFaction fid b -> do
    side <- getsClient sside
    lidV <- viewedLevelUI
    markDisplayNeeded lidV
    when (fid == side) $ do
      unless b $
        -- Clear macros and invoke a special main menu entrance macro
        -- that sets @swasAutomated@, preparing for AI control at exit.
        modifySession $ \sess ->
          sess { smacroFrame =
                   emptyMacroFrame {keyPending = KeyMacro [K.controlEscKM]}
               , smacroStack = [] }
      setFrontAutoYes b  -- now can start/stop auto-accepting prompts
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
      -- Faction notices @fromTile@ can't be altered into @toTIle@,
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
      msgAdd (if unexpected then MsgSpecialEvent else MsgNeutralEvent) msg
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
    unless (subject2 == object) $ do
      msgAdd MsgTerrainReveal msg
      msgAdd MsgTutorialHint "Solid terrain drawn in pink is not fully known until searched. This is usually done by bumping into it, which also triggers effects and transformations the terrain is capable of. Once revealed, the terrain can be inspected in aiming mode started with the '*' key or with mouse."
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
    if | sdisplayNeeded -> pushFrame True
           -- adds delay, because it's not an extra animation-like frame,
           -- but showing some real information accumulated up to this point
       | turnPing && not sturnDisplayed -> pushFrame False
       | otherwise -> return ()
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
    cops@COps{cocave, comode, corule} <- getsState scops
    oldSess <- getSession
    snxtChal <- getsClient snxtChal
    noConfirmsGame <- isNoConfirmsGame
    let uiOptions = sUIOptions oldSess
        f !acc _p !i _a = i : acc
        modes = zip [0..] $ ofoldlGroup' comode CAMPAIGN_SCENARIO f []
        g :: (Int, ContentId ModeKind) -> Int
        g (_, mode) = case EM.lookup mode (svictories oldSess) of
          Nothing -> 0
          Just cm -> fromMaybe 0 (M.lookup snxtChal cm)
        (snxtScenario, _) = minimumBy (comparing g) modes
        nxtGameTutorial = MK.mtutorial $ snd $ nxtGameMode cops snxtScenario
    putSession $
      (emptySessionUI uiOptions)
        { schanF = schanF oldSess
        , sccui = sccui oldSess
        , shistory = shistory oldSess
        , svictories = svictories oldSess
        , scampings = scampings oldSess
        , srestarts = srestarts oldSess
        , smarkVision = smarkVision oldSess
        , smarkSmell = smarkSmell oldSess
        , snxtScenario
        , scurTutorial = noConfirmsGame || snxtTutorial oldSess
            -- make sure a newbie interrupting a screensaver has ample help
        , snxtTutorial = nxtGameTutorial
        , soverrideTut = soverrideTut oldSess
        , sstart = sstart oldSess
        , sgstart = sgstart oldSess
        , sallTime = sallTime oldSess
        , snframes = snframes oldSess
        , sallNframes = sallNframes oldSess
        , srandomUI = srandom
        }
    when (sstart oldSess == 0) resetSessionStart
    when (lengthHistory (shistory oldSess) == 0) $ do
      -- Generate initial history. Only for UI clients.
      shistory <- defaultHistory
      modifySession $ \sess -> sess {shistory}
      let title = T.pack $ rtitle corule
      msgAdd MsgBookKeeping $ "Welcome to" <+> title <> "!"
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
    msgAdd MsgBookKeeping "-------------------------------------------------"
    recordHistory
    msgAdd MsgPromptGeneric
           "A grand story starts right here! (Press '?' for mode description and help.)"
    if lengthHistory (shistory oldSess) > 1
      then fadeOutOrIn False
      else pushReportFrame  -- show anything ASAP
    msgAdd MsgActionWarning
           ("New game started in" <+> mname gameMode <+> "mode.")
    let desc = cdesc $ okind cocave $ lkind lvl
    unless (T.null desc) $ do
      msgLnAdd MsgBackdropFocus "You take in your surroundings."
      msgAdd MsgBackdropInfo desc
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
    msgLnAdd MsgBadMiscEvent blurb  -- being here is a bad turn of events
    when (cwolf curChal && not loneMode) $
      msgAdd MsgActionWarning "Being a lone wolf, you begin without companions."
    setFrontAutoYes $ gunderAI fact
    -- Forget the furious keypresses when dying in the previous game.
    resetPressedKeys
  UpdRestartServer{} -> return ()
  UpdResume fid _ -> do
    COps{cocave} <- getsState scops
    resetSessionStart
    fact <- getsState $ (EM.! fid) . sfactionD
    setFrontAutoYes $ gunderAI fact
    unless (gunderAI fact) $ do
      lid <- getArenaUI
      lvl <- getLevel lid
      gameMode <- getGameMode
      msgAdd MsgPromptGeneric
             "Welcome back! (Press '?' for mode description and help.)"
      pushReportFrame  -- show anything ASAP
      msgAdd MsgActionAlert $ "Continuing" <+> mname gameMode <+> "mode."
      let desc = cdesc $ okind cocave $ lkind lvl
      unless (T.null desc) $ do
        msgLnAdd MsgPromptFocus "You remember your surroundings."
        msgAdd MsgPromptGeneric desc
  UpdResumeServer{} -> return ()
  UpdKillExit{} -> do
#ifdef USE_JSFILE
      -- Some browsers seem to trash Local Storage when page reloaded or closed
      -- or the browser closed, while they still internally finish the saving
      -- in the background, so wait 2s. If the exit is without a save,
      -- the wait is spurious, but it's not supposed to be common.
      -- TODO: replace the @liftIO@ with a @MonadClientUI@ delay function.
    liftIO $ threadDelay 2000000
#else
    liftIO $ threadDelay 200000
#endif
    -- The prompt is necessary to force frontend to show this before exiting.
    void $ displayMore ColorBW "Done."  -- in case it follows "Saving..."
    side <- getsClient sside
    debugPossiblyPrintUI $ "Client" <+> tshow side <+> "closing frontend."
    frontendShutdown
    debugPossiblyPrintUI $ "Client" <+> tshow side <+> "closed frontend."
  UpdWriteSave -> msgAdd MsgInnerWorkSpam "Saving backup."
  UpdHearFid _ distance hearMsg -> do
    mleader <- getsClient sleader
    case mleader of
      Just{} -> return ()  -- will flush messages when leader moves
      Nothing -> do
        lidV <- viewedLevelUI
        markDisplayNeeded lidV
        recordHistory
    msg <- ppHearMsg distance hearMsg
    let msgClass = case distance of
          Nothing -> MsgHeardOutside
          Just 0 -> MsgHeardNearby
          Just _ -> MsgHeardFaraway
    msgAdd msgClass msg
    case hearMsg of
      HearUpd UpdDestroyActor{} ->
        msgAdd MsgTutorialHint "Events out of your sight radius (as listed in the '#' skill menu) can sometimes be heard, depending on your hearing radius skill. Some, such as death shrieks, can always be heard regardless of skill and distance, including when they come from a different floor."
      HearTaunt{} -> do
        globalTime <- getsState stime
        when (globalTime > timeTurn) $  -- avoid too many hints at the start
          msgAdd MsgTutorialHint "Enemies you can't see are sometimes heard yelling and emitting other noises. Whether you can hear them, depends on their distance and your hearing radius, as listed in the '#' skill menu."
      _ -> return ()
  UpdMuteMessages _ smuteMessages ->
    modifySession $ \sess -> sess {smuteMessages}

assignItemRole :: MonadClientUI m => Container -> ItemId -> m ()
assignItemRole c iid = do
  arItem <- getsState $ aspectRecordFromIid iid
  let assignSingleRole lore = do
        ItemRoles itemRoles <- getsSession sroles
        let itemRole = itemRoles EM.! lore
        unless (iid `ES.member` itemRole) $ do
          let newRoles = ItemRoles $ EM.adjust (ES.insert iid) lore itemRoles
          modifySession $ \sess -> sess {sroles = newRoles}
      slore = IA.loreFromContainer arItem c
  assignSingleRole slore
  when (slore `elem` [SOrgan, STrunk, SCondition]) $
    assignSingleRole SBody

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
                      && fhasGender (gkind fact) = "he"
                    | otherwise = "it"
        nameFromNumber fn k = if k == 0
                              then makePhrase [MU.Ws $ MU.Text fn, "Captain"]
                              else fn <+> tshow k
        heroNamePronoun k =
          if gcolor fact /= Color.BrWhite
          then (nameFromNumber (fname $ gkind fact) k, "he")
          else fromMaybe (nameFromNumber (fname $ gkind fact) k, "he")
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
           assignItemRole c iid
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
             msgAdd MsgTutorialHint "Enemies are normally dealt with using melee (by bumping when adjacent) or ranged combat (by 'f'linging items at them)."
           ThreatArmed ->
             msgAdd MsgTutorialHint "Enemies can be dealt with not only via combat, but also with clever use of terrain effects, stealth (not emitting nor reflecting light) or hasty retreat (particularly when foes are asleep or drowsy)."
           _ | length friendAssocs <= 1 -> return ()  -- one member on level
           ThreatAnotherUnarmed ->
             msgAdd MsgTutorialHint "When dealing with groups of enemies, remember than you fight as a team. Switch the pointman (marked on the map with the yellow box) using the Tab key until you move each teammate to a tactically advantageous position. Avoid meleeing alone."
           ThreatAnotherArmed ->
             msgAdd MsgTutorialHint "When dealing with groups of armed enemies, remember than you fight as a team. Switch the pointman (marked on the map with the yellow box) using the Tab key until you move each teammate to a tactically advantageous position. Retreat, if necessary to form a front line. Soften the foes with missiles, especially of exploding kind."
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
  ItemRoles itemRoles <- getsSession sroles
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
        if iid `ES.member` (itemRoles EM.! slore)
        then return Nothing  -- this item or another with the same @iid@
                             -- seen already (has a role assigned); old news
        else do  -- never seen or would have a role
          assignItemRole c iid
          case c of
            CFloor{} -> do
              let subjectShort = partItemWsShortest rwidth side factionD k
                                                    localTime itemFull kit
                  subjectLong = partItemWsLong rwidth side factionD k
                                               localTime itemFull kit
              return $ Just (k, subjectShort, subjectLong)
            _ -> return Nothing
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
      mleader <- getsClient sleader
      if Just aid == mleader && not (gunderAI fact) then
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
  stopAtMove aid

displaceActorUI :: MonadClientUI m => ActorId -> ActorId -> m ()
displaceActorUI source target = do
  mleader <- getsClient sleader
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  spart <- partActorLeader source
  tpart <- partActorLeader target
  let msgClass = if mleader `elem` map Just [source, target]
                 then MsgActionMajor  -- to interrupt run after a displace;
                 else MsgActionMinor  -- configurable, animation is feedback
      msg = makeSentence [MU.SubjectVerbSg spart "displace", tpart]
  msgAdd msgClass msg
  lookAtMove source
  stopAtMove source
  when (bfid sb /= bfid tb) $ do
    lookAtMove target  -- in case only this one is ours
    stopAtMove target
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
  mleader <- getsClient sleader
  ItemRoles itemRoles <- getsSession sroles
  if iid `ES.member` (itemRoles EM.! SItem) then
    -- So far organs can't be put into stash, so no need to call
    -- @assignItemRole@ to add or reassign lore category.
    if cstore1 == CGround && Just aid == mleader && not (gunderAI fact) then
      itemAidVerbMU MsgActionMajor aid verb iid (Right k)
    else when (not (bproj b) && bhp b > 0) $  -- don't announce death drops
      itemAidVerbMU MsgActionMajor aid verb iid (Left k)
  else error $ "" `showFailure` (iid, k, aid, cstore1, cstore2)

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
