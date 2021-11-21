-- | Display atomic SFX commands received by the client.
module Game.LambdaHack.Client.UI.Watch.WatchSfxAtomicM
  ( watchRespSfxAtomicUI
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , returnJustLeft, ppSfxMsg, strike
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import           Data.Int (Int64)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import           Game.LambdaHack.Atomic
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.ActorUI
import           Game.LambdaHack.Client.UI.Animation
import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Client.UI.EffectDescription
import           Game.LambdaHack.Client.UI.Frame
import           Game.LambdaHack.Client.UI.FrameM
import           Game.LambdaHack.Client.UI.HandleHelperM
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import           Game.LambdaHack.Client.UI.ItemDescription
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.MonadClientUI
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.MsgM
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.SlideshowM
import           Game.LambdaHack.Client.UI.Watch.WatchCommonM
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
import           Game.LambdaHack.Common.ReqFailure
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Content.CaveKind (cdesc)
import qualified Game.LambdaHack.Content.ItemKind as IK
import qualified Game.LambdaHack.Content.TileKind as TK
import qualified Game.LambdaHack.Core.Dice as Dice
import           Game.LambdaHack.Core.Frequency
import           Game.LambdaHack.Core.Random
import qualified Game.LambdaHack.Definition.Ability as Ability
import qualified Game.LambdaHack.Definition.Color as Color
import           Game.LambdaHack.Definition.Defs

-- | Display special effects (text, animation) sent to the client.
-- Don't modify client state (except a few fields), but only client
-- session (e.g., by displaying messages). This is enforced by types.
watchRespSfxAtomicUI :: MonadClientUI m => SfxAtomic -> m ()
{-# INLINE watchRespSfxAtomicUI #-}
watchRespSfxAtomicUI sfx = case sfx of
  SfxStrike source target iid ->
    strike False source target iid
  SfxRecoil source target iid -> do
    sourceSeen <- getsState $ EM.member source . sactorD
    if not sourceSeen then do
      tb <- getsState $ getActorBody target
      animate (blid tb) $ blockMiss (bpos tb, bpos tb)
    else do
      CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
      sb <- getsState $ getActorBody source
      tb <- getsState $ getActorBody source
      spart <- partActorLeader source
      tpart <- partActorLeader target
      side <- getsClient sside
      factionD <- getsState sfactionD
      localTime <- getsState $ getLocalTime (blid tb)
      itemFullWeapon <- getsState $ itemToFull iid
      let kitWeapon = quantSingle
          (weaponName, _) = partItemShort rwidth side factionD
                                          localTime itemFullWeapon kitWeapon
          weaponNameOwn = partItemShortWownW rwidth side factionD spart
                                             localTime itemFullWeapon kitWeapon
          verb = if bproj sb then "deflect" else "fend off"
          objects | iid == btrunk sb = ["the", spart]
                  | iid `EM.member` borgan sb =  ["the", weaponNameOwn]
                  | otherwise = ["the", weaponName, "of", spart]
      msgAdd MsgActionMajor $
        makeSentence $ MU.SubjectVerbSg tpart verb : objects
      animate (blid tb) $ blockMiss (bpos tb, bpos sb)
  SfxSteal source target iid ->
    strike True source target iid
  SfxRelease source target _ -> do
    spart <- partActorLeader source
    tpart <- partActorLeader target
    msgAdd MsgActionMajor $
      makeSentence [MU.SubjectVerbSg spart "release", tpart]
  SfxProject aid iid ->
    itemAidVerbMU MsgActionMajor aid "fling" iid (Left 1)
  SfxReceive aid iid ->
    itemAidVerbMU MsgActionMajor aid "receive" iid (Left 1)
  SfxApply aid iid -> do
    CCUI{coscreen=ScreenContent{rapplyVerbMap}} <- getsSession sccui
    ItemFull{itemKind} <- getsState $ itemToFull iid
    let actionPart =
          maybe "trigger"
                MU.Text
                (EM.lookup (IK.isymbol itemKind) rapplyVerbMap)
    itemAidVerbMU MsgActionMajor aid actionPart iid (Left 1)
  SfxCheck aid iid ->
    itemAidVerbMU MsgActionMajor aid "recover" iid (Left 1)
  SfxTrigger _ _ _ fromTile -> do
    COps{cotile} <- getsState scops
    let subject = MU.Text $ TK.tname $ okind cotile fromTile
        verb = "shake"
        msg = makeSentence ["the", MU.SubjectVerbSg subject verb]
    msgAdd MsgNeutralEvent msg
  SfxShun aid _ _ _ ->
    aidVerbMU MsgActionMajor aid "shun it"
  SfxEffect fidSource aid iid effect hpDelta -> do
    -- In most messages below @iid@ is ignored, because it's too common,
    -- e.g., caused by some combat hits, or rather obvious,
    -- e.g., in case of embedded items, or would be counterintuitive,
    -- e.g., when actor is said to be intimidated by a particle, not explosion.
    CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
    b <- getsState $ getActorBody aid
    bUI <- getsSession $ getActorUI aid
    side <- getsClient sside
    mleader <- getsClient sleader
    itemD <- getsState sitemD
    actorMaxSk <- getsState $ getActorMaxSkills aid
    let fid = bfid b
        isOurCharacter = fid == side && not (bproj b)
        isAlive = bhp b > 0
        isOurAlive = isOurCharacter && isAlive
        isOurLeader = Just aid == mleader
        -- The message classes are close enough. It's melee or similar.
        feelLookHPBad bigAdj projAdj = do
          feelLook MsgBadMiscEvent MsgGoodMiscEvent bigAdj projAdj
          -- We can't know here if the hit was in melee, ranged or
          -- even triggering a harmful item. However, let's not talk
          -- about armor before the player has the most basic one.
          -- for melee. Most of the time the first hit in the game is,
          -- in fact, from melee, so that's a sensible default.
          --
          -- Note that the @idamage@ is called piercing (or edged) damage,
          -- even though the distinction from impact damage is fleshed
          -- out only in Allure.
          when (isOurCharacter
                && Ability.getSk Ability.SkArmorMelee actorMaxSk > 0) $
            msgAdd MsgTutorialHint "You took damage of a different kind than the normal piercing hit, which means your armor couldn't block any part of it. Normally, your HP (hit points, health) do not regenerate, so losing them is a big deal. Apply healing concoctions or take a long sleep to replenish your HP (but in this hectic environment not even uninterrupted resting that leads to sleep is easy)."
        feelLookHPGood = feelLook MsgGoodMiscEvent MsgBadMiscEvent
        feelLookCalm bigAdj projAdj = when isAlive $
          feelLook MsgEffectMinor MsgEffectMinor bigAdj projAdj
        -- Ignore @iid@, because it's usually obvious what item caused that
        -- and because the effects are not particularly disortienting.
        feelLook msgClassOur msgClassTheir bigAdj projAdj =
          let (verb, adjective) =
                if bproj b
                then ("get", projAdj)
                else ( if isOurCharacter then "feel" else "look"
                     , if isAlive then bigAdj else projAdj )
                         -- dead body is an item, not a person
              msgClass = if | bproj b -> MsgEffectMinor
                            | isOurCharacter -> msgClassOur
                            | otherwise -> msgClassTheir
          in aidVerbMU msgClass aid $ MU.Text $ verb <+> adjective
    case effect of
      IK.Burn{} -> do
        feelLookHPBad "burned" "scorched"
        let ps = (bpos b, bpos b)
        animate (blid b) $ twirlSplash ps Color.BrRed Color.Brown
      IK.Explode{} -> return ()  -- lots of visual feedback
      IK.RefillHP p | p == 1 -> return ()  -- no spam from regeneration
      IK.RefillHP p | p == -1 -> return ()  -- no spam from poison
      IK.RefillHP{} | hpDelta > 0 -> do
        feelLookHPGood "healthier" "mended"
        let ps = (bpos b, bpos b)
        animate (blid b) $ twirlSplash ps Color.BrGreen Color.Green
      IK.RefillHP{} -> do
        feelLookHPBad "wounded" "broken"
        let ps = (bpos b, bpos b)
        animate (blid b) $ twirlSplash ps Color.BrRed Color.Red
      IK.RefillCalm{} | not isAlive -> return ()
      IK.RefillCalm{} | bproj b -> return ()
      IK.RefillCalm p | p == 1 -> return ()  -- no spam from regen items
      IK.RefillCalm p | p > 0 -> feelLookCalm "calmer" "stabilized"
      IK.RefillCalm _ -> feelLookCalm "agitated" "wobbly"
      IK.Dominate | not isAlive -> return ()
      IK.Dominate -> do
        -- For subsequent messages use the proper name, never "you".
        let subject = partActor bUI
        if fid /= fidSource then do
          -- Before domination, possibly not seen if actor (yet) not ours.
          if bcalm b == 0  -- sometimes only a coincidence, but nm
          then aidVerbMU MsgEffectMedium aid "yield, under extreme pressure"
          else do
            let verb = if isOurAlive
                       then "black out, dominated by foes"
                       else "decide abruptly to switch allegiance"
                -- Faction is being switched, so item that caused domination
                -- and vanished may not be known to the new faction.
                msuffix = if iid == btrunk b || iid `EM.notMember` itemD
                          then Nothing
                          else Just $ if isOurAlive
                                      then "through"
                                      else "under the influence of"
            mitemAidVerbMU MsgEffectMedium aid verb iid msuffix
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
      IK.Impress | not isAlive -> return ()
      IK.Impress -> aidVerbMU MsgEffectMinor aid "be awestruck"
      IK.PutToSleep | not isAlive -> return ()
      IK.PutToSleep -> do
        let verb = "be put to sleep"
            msuffix = Just $ if fidSource == bfid b then "due to" else "by"
        mitemAidVerbMU MsgEffectMajor aid verb iid msuffix
      IK.Yell | not isAlive -> return ()
      IK.Yell -> aidVerbMU MsgMiscellanous aid "start"
      IK.Summon grp p -> do
        let verbBase = if bproj b then "lure" else "summon"
            part = MU.Text $ displayGroupName grp
            object = if p == 1  -- works, because exact number sent, not dice
                     then MU.AW part
                     else MU.Ws part
            verb = MU.Phrase [verbBase, object]
            msuffix = Just "with"
        mitemAidVerbMU MsgEffectMajor aid verb iid msuffix
      IK.Ascend{} | not isAlive -> return ()
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
                msgAdd MsgBackdropInfo $ desc <> "\n"
              msgAdd MsgTutorialHint "New floor is new opportunities, though the old level is still there and others may roam it after you left. Viewing all floors, without moving between them, can be done using the '<' and '>' keys."
            [] -> return ()  -- spell fizzles; normally should not be sent
      IK.Escape{} | isOurCharacter -> do
        ours <- getsState $ fidActorNotProjGlobalAssocs side
        when (length ours > 1) $ do
          (_, total) <- getsState $ calculateTotal side
          if total == 0
          then msgAdd MsgFactionIntel $
                 "The team joins" <+> makePhrase [partActor bUI]
                 <> ", forms a perimeter and leaves triumphant."
          else msgAdd MsgItemCreation $
                 "The team joins" <+> makePhrase [partActor bUI]
                 <> ", forms a perimeter, repacks its belongings and leaves triumphant."
      IK.Escape{} -> return ()
      IK.Paralyze{} | not isAlive -> return ()
      IK.Paralyze{} ->
        mitemAidVerbMU MsgEffectMedium aid "be paralyzed" iid (Just "with")
      IK.ParalyzeInWater{} | not isAlive -> return ()
      IK.ParalyzeInWater{} ->
        aidVerbMU MsgEffectMinor aid "move with difficulty"
      IK.InsertMove{} | not isAlive -> return ()
      IK.InsertMove d ->
        -- Usually self-inflicted of from embeds, so obvious, so no @iid@.
        if Dice.supDice d >= 10
        then aidVerbMU MsgEffectMedium aid "act with extreme speed"
        else do
          let msgClass = if isOurCharacter
                         then MsgEffectMedium
                         else MsgEffectMinor
          aidVerbMU msgClass aid "move swiftly"
      IK.Teleport t | Dice.supDice t <= 9 -> do
        -- Actor may be sent away before noticing the item that did it.
        let msuffix = if iid `EM.notMember` itemD
                      then Nothing
                      else Just "due to"
            msgClass = if isOurCharacter
                       then MsgEffectMedium
                       else MsgEffectMinor
        mitemAidVerbMU msgClass aid "blink" iid msuffix
      IK.Teleport{} -> do
        -- Actor may be sent away before noticing the item that did it.
        let msuffix = if iid `EM.notMember` itemD
                      then Nothing
                      else Just "by the power of"
        mitemAidVerbMU MsgEffectMedium aid "teleport" iid msuffix
      IK.CreateItem{} -> return ()
      IK.DestroyItem{} -> return ()
      IK.ConsumeItems{} -> return ()
      IK.DropItem _ _ COrgan _ -> return ()
      IK.DropItem{} ->  -- rare enough
        mitemAidVerbMU MsgEffectMedium aid "be stripped" iid (Just "with")
      IK.Recharge{} | not isAlive -> return ()
      IK.Recharge{} -> aidVerbMU MsgEffectMedium aid "heat up"
      IK.Discharge{} | not isAlive -> return ()
      IK.Discharge{} -> aidVerbMU MsgEffectMedium aid "cool down"
      IK.PolyItem -> do
        subject <- partActorLeader aid
        let ppstore = MU.Text $ ppCStoreIn CGround
        msgAdd MsgEffectMedium $ makeSentence
          [ MU.SubjectVerbSg subject "repurpose", "what lies", ppstore
          , "to a common item of the current level" ]
      IK.RerollItem -> do
        subject <- partActorLeader aid
        let ppstore = MU.Text $ ppCStoreIn CGround
        msgAdd MsgEffectMedium $ makeSentence
          [ MU.SubjectVerbSg subject "reshape", "what lies", ppstore
          , "striving for the highest possible standards" ]
      IK.DupItem -> do
        subject <- partActorLeader aid
        let ppstore = MU.Text $ ppCStoreIn CGround
        msgAdd MsgEffectMedium $ makeSentence
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
        factionD <- getsState sfactionD
        localTime <- getsState $ getLocalTime $ blid b
        let verb = MU.Text $ detectToVerb d
            object = MU.Ws $ MU.Text $ detectToObject d
        (periodic, itemFull) <-
          if iid `EM.member` itemD then do
            itemFull <- getsState $ itemToFull iid
            let arItem = aspectRecordFull itemFull
            return (IA.checkFlag Ability.Periodic arItem, itemFull)
          else do
#ifdef WITH_EXPENSIVE_ASSERTIONS
            -- It's not actually expensive, but it's particularly likely
            -- to fail with wild content, indicating server game rules logic
            -- needs to be fixed/extended:
            -- Observer from another faction may receive the effect information
            -- from the server, because the affected actor is visible,
            -- but the position of the item may be out of FOV. This is fine;
            -- the message is then shorter, because only the effect was seen,
            -- while the cause remains misterious.
            let !_A = if fid /= side  -- not from affected faction; observing
                      then ()
                      else error $ "item never seen by the affected actor"
                                   `showFailure` (aid, b, bUI, verb, iid, sfx)
#endif
            return (False, undefined)
        let iidDesc =
              let (name1, powers) = partItemShort rwidth side factionD localTime
                                                  itemFull quantSingle
              in makePhrase ["the", name1, powers]
            -- If item not periodic, most likely intentional, so don't spam.
            means = [MU.Text $ "(via" <+> iidDesc <> ")" | periodic]
        msgAdd MsgEffectMinor $
          makeSentence $ [MU.SubjectVerbSg subject verb] ++ [object] ++ means
        -- Don't make it modal if all info remains after no longer seen.
        unless (fid /= side || d `elem` [IK.DetectHidden, IK.DetectExit]) $
          displayMore ColorFull ""  -- the sentence short
      IK.SendFlying{} | not isAlive -> return ()
      IK.SendFlying{} -> aidVerbMU MsgEffectMedium aid "be sent flying"
      IK.PushActor{} | not isAlive -> return ()
      IK.PushActor{} -> aidVerbMU MsgEffectMedium aid "be pushed"
      IK.PullActor{} | not isAlive -> return ()
      IK.PullActor{} -> aidVerbMU MsgEffectMedium aid "be pulled"
      IK.ApplyPerfume ->
        msgAdd MsgEffectMinor
               "The fragrance quells all scents in the vicinity."
      IK.AtMostOneOf{} -> return ()
      IK.OneOf{} -> return ()
      IK.OnSmash{} -> error $ "" `showFailure` sfx
      IK.OnCombine{} -> error $ "" `showFailure` sfx
      IK.OnUser{} -> error $ "" `showFailure` sfx
      IK.NopEffect -> error $ "" `showFailure` sfx
      IK.AndEffect{} -> error $ "" `showFailure` sfx
      IK.OrEffect{} -> error $ "" `showFailure` sfx
      IK.SeqEffect{} -> error $ "" `showFailure` sfx
      IK.When{} -> error $ "" `showFailure` sfx
      IK.Unless{} -> error $ "" `showFailure` sfx
      IK.IfThenElse{} -> error $ "" `showFailure` sfx
      IK.VerbNoLonger{} | not isAlive -> return ()
      IK.VerbNoLonger verb ending -> do
        let msgClass = if fid == side
                       then MsgStatusStopUs
                       else MsgStatusStopThem
        subject <- partActorLeader aid
        msgAdd msgClass $
          makePhrase [MU.Capitalize $ MU.SubjectVerbSg subject $ MU.Text verb]
          <> ending
      IK.VerbMsg verb ending -> do
        subject <- partActorLeader aid
        msgAdd MsgEffectMedium $
          makePhrase [MU.Capitalize $ MU.SubjectVerbSg subject $ MU.Text verb]
          <> ending
      IK.VerbMsgFail verb ending -> do
        subject <- partActorLeader aid
        msgAdd MsgActionWarning $
          makePhrase [MU.Capitalize $ MU.SubjectVerbSg subject $ MU.Text verb]
          <> ending
  SfxItemApplied verbose iid c -> do
    if verbose
    then itemVerbMU MsgActionMinor iid (1, []) "have got activated" c
    else itemVerbMU MsgInnerWorkSpam iid (1, []) "have been triggered" c
  SfxMsgFid _ sfxMsg -> do
    mleader <- getsClient sleader
    case mleader of
      Just{} -> return ()  -- will flush messages when leader moves
      Nothing -> do
        lidV <- viewedLevelUI
        markDisplayNeeded lidV
        recordHistory
    mmsg <- ppSfxMsg sfxMsg
    case mmsg of
      Just (Left (msgClass, msg)) -> msgAdd msgClass msg
      Just (Right (msgClass, (t1, t2))) -> do
        let dotsIfShorter = if t1 == t2 then "" else ".."
        msgAddDistinct msgClass  (t1 <> dotsIfShorter, t2)
      Nothing -> return ()
  SfxRestart -> fadeOutOrIn True
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
      let msgClass = if voluntary && bfid b == side
                     then MsgActionComplete  -- give feedback after keypress
                     else MsgMiscellanous
      msgAdd msgClass $! makeSentence [MU.SubjectVerbSg spart (MU.Text verb)]

returnJustLeft :: MonadClientUI m
               => (MsgClassShowAndSave, Text)
               -> m (Maybe (Either (MsgClassShowAndSave, Text)
                                   (MsgClassDistinct, (Text, Text))))
returnJustLeft = return . Just . Left

ppSfxMsg :: MonadClientUI m
         => SfxMsg -> m (Maybe (Either (MsgClassShowAndSave, Text)
                                       (MsgClassDistinct, (Text, Text))))
ppSfxMsg sfxMsg = case sfxMsg of
  SfxUnexpected reqFailure -> returnJustLeft
    ( MsgActionWarning
    , "Unexpected problem:" <+> showReqFailure reqFailure <> "." )
  SfxExpected itemName reqFailure -> returnJustLeft
    ( MsgActionWarning
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
      returnJustLeft
        ( MsgActionWarning
        , "The" <+> "embedded" <+> name <+> "is not activated:"
          <+> showReqFailure reqFailure <> "." )
    else return Nothing
  SfxFizzles iid c -> do
    msg <- itemVerbMUGeneral True iid (1, []) "do not work" c
    return $ Just $ Right (MsgStatusWarning, ("It didn't work.", msg))
  SfxNothingHappens iid c -> do
    msg <- itemVerbMUGeneral True iid (1, []) "do nothing, predictably" c
    return $ Just $ Right (MsgStatusBenign, ("Nothing happens.", msg))
  SfxNoItemsForTile toolsToAlterWith -> do
    revCmd <- revCmdMap
    let km = revCmd HumanCmd.AlterDir
        tItems = describeToolsAlternative toolsToAlterWith
    returnJustLeft ( MsgActionWarning
                   , "To transform the terrain, prepare the following items on the ground or in equipment:"
                     <+> tItems
                     <+> "and use the '"
                     <> T.pack (K.showKM km)
                     <> "' terrain modification command."
                   )
  SfxVoidDetection d -> returnJustLeft
    ( MsgMiscellanous
    , makeSentence ["no new", MU.Text $ detectToObject d, "detected"] )
  SfxUnimpressed aid -> do
    msbUI <- getsSession $ EM.lookup aid . sactorUI
    case msbUI of
      Nothing -> return Nothing
      Just sbUI -> do
        let subject = partActor sbUI
            verb = "be unimpressed"
        returnJustLeft ( MsgActionWarning
                       , makeSentence [MU.SubjectVerbSg subject verb] )
  SfxSummonLackCalm aid -> do
    msbUI <- getsSession $ EM.lookup aid . sactorUI
    case msbUI of
      Nothing -> return Nothing
      Just sbUI -> do
        let subject = partActor sbUI
            verb = "lack Calm to summon"
        returnJustLeft ( MsgActionWarning
                       , makeSentence [MU.SubjectVerbSg subject verb] )
  SfxSummonTooManyOwn aid -> do
    msbUI <- getsSession $ EM.lookup aid . sactorUI
    case msbUI of
      Nothing -> return Nothing
      Just sbUI -> do
        let subject = partActor sbUI
            verb = "can't keep track of their numerous friends, let alone summon any more"
        returnJustLeft (MsgActionWarning, makeSentence [subject, verb])
  SfxSummonTooManyAll aid -> do
    msbUI <- getsSession $ EM.lookup aid . sactorUI
    case msbUI of
      Nothing -> return Nothing
      Just sbUI -> do
        let subject = partActor sbUI
            verb = "can't keep track of everybody around, let alone summon anyone else"
        returnJustLeft (MsgActionWarning, makeSentence [subject, verb])
  SfxSummonFailure aid -> do
    msbUI <- getsSession $ EM.lookup aid . sactorUI
    case msbUI of
      Nothing -> return Nothing
      Just sbUI -> do
        let subject = partActor sbUI
            verb = "fail to summon anything"
        returnJustLeft ( MsgActionWarning
                       , makeSentence [MU.SubjectVerbSg subject verb] )
  SfxLevelNoMore -> returnJustLeft
    (MsgActionWarning, "No more levels in this direction.")
  SfxLevelPushed -> returnJustLeft
    (MsgActionWarning, "You notice somebody pushed to another level.")
  SfxBracedImmune aid -> do
    msbUI <- getsSession $ EM.lookup aid . sactorUI
    case msbUI of
      Nothing -> return Nothing
      Just sbUI -> do
        let subject = partActor sbUI
            verb = "be braced and so immune to translocation"
        returnJustLeft ( MsgMiscellanous
                       , makeSentence [MU.SubjectVerbSg subject verb] )
                         -- too common
  SfxEscapeImpossible -> returnJustLeft
    ( MsgActionWarning
    , "Escaping outside is unthinkable for members of this faction." )
  SfxStasisProtects -> returnJustLeft
    ( MsgMiscellanous  -- too common
    , "Paralysis and speed surge require recovery time." )
  SfxWaterParalysisResisted -> return Nothing  -- don't spam
  SfxTransImpossible -> returnJustLeft
    (MsgActionWarning, "Translocation not possible.")
  SfxIdentifyNothing -> returnJustLeft
    (MsgActionWarning, "Nothing to identify.")
  SfxPurposeNothing -> returnJustLeft
    ( MsgActionWarning
    , "The purpose of repurpose cannot be availed without an item"
      <+> ppCStoreIn CGround <> "." )
  SfxPurposeTooFew maxCount itemK -> returnJustLeft
    ( MsgActionWarning
    , "The purpose of repurpose is served by" <+> tshow maxCount
      <+> "pieces of this item, not by" <+> tshow itemK <> "." )
  SfxPurposeUnique -> returnJustLeft
    (MsgActionWarning, "Unique items can't be repurposed.")
  SfxPurposeNotCommon -> returnJustLeft
    (MsgActionWarning, "Only ordinary common items can be repurposed.")
  SfxRerollNothing -> returnJustLeft
    ( MsgActionWarning
    , "The shape of reshape cannot be assumed without an item"
      <+> ppCStoreIn CGround <> "." )
  SfxRerollNotRandom -> returnJustLeft
    (MsgActionWarning, "Only items of variable shape can be reshaped.")
  SfxDupNothing -> returnJustLeft
    ( MsgActionWarning
    , "Mutliplicity won't rise above zero without an item"
      <+> ppCStoreIn CGround <> "." )
  SfxDupUnique -> returnJustLeft
    (MsgActionWarning, "Unique items can't be multiplied.")
  SfxDupValuable -> returnJustLeft
    (MsgActionWarning, "Valuable items can't be multiplied.")
  SfxColdFish -> returnJustLeft
    ( MsgMiscellanous  -- repeatable
    , "Healing attempt from another faction is thwarted by your cold fish attitude." )
  SfxReadyGoods -> returnJustLeft
    ( MsgMiscellanous  -- repeatable
    , "Crafting is alien to you, accustomed to buying ready goods all your life." )
  SfxTimerExtended aid iid cstore delta -> do
    CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
    aidSeen <- getsState $ EM.member aid . sactorD
    iidSeen <- getsState $ EM.member iid . sitemD
    if aidSeen && iidSeen then do
      b <- getsState $ getActorBody aid
      bUI <- getsSession $ getActorUI aid
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
      -- Ideally we'd use a pronoun here, but the action (e.g., hit)
      -- that caused this extension can be invisible to some onlookers.
      -- So their narrative context needs to be taken into account.
      -- The upside is that the messages do not bind pronouns
      -- and so commute and so repetitions can be squashed.
      let cond = [ "condition"
                 | IA.checkFlag Ability.Condition $ aspectRecordFull itemFull ]
          usShow =
            ["the", name, powers] ++ cond
            ++ storeOwn ++ ["will now last longer"]
          usSave =
            ["the", name, powers] ++ cond
            ++ storeOwn ++ ["will now last"]
            ++ [MU.Text $ timeDeltaInSecondsText delta <+> "longer"]
            ++ [MU.Text $ "(total:" <+> timeDeltaInSecondsText total <> ")"]
          -- Note that when enemy actor causes the extension to himself,
          -- the player is not notified at all. So the shorter blurb
          -- displayed on the screen is middle ground and full is in history.
          themShow =
            [partItemShortWownW rwidth side factionD (partActor bUI) localTime
                                itemFull quantSingle]
            ++ cond ++ ["is extended"]
          themSave =
            [partItemShortWownW rwidth side factionD (partActor bUI) localTime
                                itemFull quantSingle]
            ++ cond ++ ["is extended by"]
            ++ [MU.Text $ timeDeltaInSecondsText delta]
            ++ [MU.Text $ "(total:" <+> timeDeltaInSecondsText total <> ")"]
          (msgClass, parts1, parts2) =
            if bfid b == side
            then (MsgStatusLongerUs, usShow, usSave)
            else (MsgStatusLongThem, themShow, themSave)
      return $ Just $ Right
        (msgClass, (makeSentence parts1, makeSentence parts2))
    else return Nothing
  SfxCollideActor source target -> do
    sourceSeen <- getsState $ EM.member source . sactorD
    targetSeen <- getsState $ EM.member target . sactorD
    if sourceSeen && targetSeen then do
      spart <- partActorLeader source
      tpart <- partActorLeader target
      -- Neutral message, because minor damage and we don't say, which faction.
      -- And the collision may even be intentional.
      returnJustLeft
        ( MsgSpecialEvent
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
      returnJustLeft (MsgSpecialEvent, msg)  -- differentiate wrt item creation
    else return Nothing

strike :: MonadClientUI m => Bool -> ActorId -> ActorId -> ItemId -> m ()
strike catch source target iid = assert (source /= target) $ do
  sourceSeen <- getsState $ EM.member source . sactorD
  if not sourceSeen then do
    tb <- getsState $ getActorBody target
    animate (blid tb) $ blockMiss (bpos tb, bpos tb)
  else do
    CCUI{coscreen=ScreenContent{rwidth}} <- getsSession sccui
    tb <- getsState $ getActorBody target
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
    let isCond (_, (itemFullArmor, _)) =
          IA.checkFlag Ability.Condition $ aspectRecordFull itemFullArmor
        -- We exclude genetic flaws, backstory items, etc., because they
        -- can't be easily taken off, so no point spamming the player.
        isOrdinaryCond ikit@(_, (itemFullArmor, _)) =
          not (IA.checkFlag Ability.MetaGame (aspectRecordFull itemFullArmor))
          && isCond ikit
        relevantSkArmor =
          if bproj sb then Ability.SkArmorRanged else Ability.SkArmorMelee
        rateArmor (iidArmor, (itemFullArmor, (k, _))) =
          ( k * IA.getSkill relevantSkArmor (aspectRecordFull itemFullArmor)
          , ( iidArmor
            , itemFullArmor ) )
        abs15 (v, _) = abs v >= 15
        condArmor = filter abs15 $ map rateArmor $ filter isOrdinaryCond orgKit
        fstGt0 (v, _) = v > 0
        wornArmor =
          filter fstGt0 $ map rateArmor $ filter (not . isCond) eqpOrgKit
    mblockArmor <- case wornArmor of
      [] -> return Nothing
      _ -> Just
           <$> rndToActionUI (frequency $ toFreq "msg armor" wornArmor)
    actorMaxSkills <- getsState sactorMaxSkills
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
        weaponNameWith = if iid == btrunk sb
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
              rawDeltaHP = into @Int64 sHurt * xM dmg `divUp` 100
          in case btrajectory sb of
            Just (_, speed) | bproj sb ->
              - modifyDamageBySpeed rawDeltaHP speed
            _ -> - rawDeltaHP
        burnDmg = - (sum $ map Dice.supDice
                     $ mapMaybe unBurn $ IK.ieffects $ itemKind itemFullWeapon)
        fillDmg =
          - (sum $ mapMaybe unRefillHP $ IK.ieffects $ itemKind itemFullWeapon)
        -- For variety, attack adverb is based on attacker's and weapon's
        -- damage potential as compared to victim's current HP.
        -- We are not taking into account victim's armor yet.
        sHurt = armorHurtCalculation (bproj sb) sMaxSk Ability.zeroSkills
        nonPiercingDmg = burnDmg + fillDmg
        sDamage = min 0 $ kineticDmg + xM nonPiercingDmg
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
          | hurtMult > 20 || nonPiercingDmg < 0 =
                            if | deadliness >= 2000 -> "marginally"
                               | deadliness >= 1000 -> "partially"
                               | deadliness >= 100 -> "partly"  -- common
                               | deadliness >= 50 -> "to an extent"
                               | deadliness >= 20 -> "to a large extent"
                               | deadliness >= 5 -> "for the major part"
                               | otherwise -> "for the most part"
          | hurtMult > 1 = if | actorWaits tb -> "doggedly"
                              | deadliness >= 50 -> "easily"  -- common
                              | deadliness >= 20 -> "effortlessly"
                              | deadliness >= 5 -> "nonchalantly"
                              | otherwise -> "bemusedly"
          | otherwise = "almost completely"
              -- a fraction gets through, but if fast missile, can be deadly
        avertVerb = if actorWaits tb then "avert it" else "ward it off"
        blockPhrase =
          let (subjectBlock, verbBlock) =
                if | not $ bproj sb ->
                     (tpronoun, if blockWithWeapon
                                then "parry"
                                else "block")
                   | tpronoun == "it"
                     || projectileHitsWeakly && tpronoun /= "you" ->
                     -- Avoid ambiguity.
                     (partActor tbUI, avertVerb)
                   | otherwise -> (tpronoun, avertVerb)
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
          | hurtMult > 1 = if nonPiercingDmg >= 0  -- no extra anim
                           then blockHit ps Color.BrRed Color.Red
                           else blockMiss ps
          | otherwise = blockMiss ps
        targetIsFoe = bfid sb == side  -- no big news if others hit our foes
                      && isFoe (bfid tb) tfact side
        targetIsFriend = isFriend (bfid tb) tfact side
                           -- warning if anybody hits our friends
        msgClassMelee =
          if targetIsFriend then MsgMeleeNormalUs else MsgMeleeOthers
        msgClassRanged =
          if targetIsFriend then MsgRangedNormalUs else MsgRangedOthers
        animateAlive lid anim =
          if bhp tb > 0
          then animate lid anim
          else animate lid $ twirlSplashShort ps Color.BrRed Color.Red
        tutorialHintBenignFoe =
          when (bfid sb == side
                && not (actorCanMeleeToHarm actorMaxSkills target tb)) $
            msgAdd MsgTutorialHint "This enemy can't harm you in melee. Left alone could it possibly be of some use?"
    -- The messages about parrying and immediately afterwards dying
    -- sound goofy, but there is no easy way to prevent that.
    -- And it's consistent.
    -- If/when death blow instead sets HP to 1 and only the next below 1,
    -- we can check here for HP==1; also perhaps actors with HP 1 should
    -- not be able to block.
    if | catch -> do  -- charge not needed when catching
         let msg = makeSentence
                     [MU.SubjectVerbSg spart "catch", tpart, "skillfully"]
         msgAdd MsgSpecialEvent msg
         when (bfid sb == side) $
           msgAdd MsgTutorialHint "You managed to catch a projectile, thanks to being braced and hitting it exactly when it was at arm's reach. The obtained item has been put into the shared stash of your party."
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
         msgAdd MsgSpecialEvent msg  -- and no animation
       | bproj sb && bproj tb -> do  -- server sends unless both are blasts
         -- Short message.
         msgAdd MsgSpecialEvent $
           makeSentence [MU.SubjectVerbSg spart "intercept", tpart]
         -- Basic non-bloody animation regardless of stats.
         animateAlive (blid tb) $ blockHit ps Color.BrBlue Color.Blue
       | kineticDmg >= -1000  -- -1/1000 HP
         -- We ignore nested effects, because they are, in general, avoidable.
         && nonPiercingDmg >= 0 -> do
         let adverb | itemSuspect itemFullWeapon && bfid sb == side =
                        "tentatively"  -- we didn't identify the weapon before
                    | bproj sb = "lightly"
                    | otherwise = "delicately"
             msg = makeSentence $
               [MU.SubjectVerbSg spart verb, tpart, adverb]
               ++ if bproj sb then [] else weaponNameWith
         msgAdd msgClassMelee msg  -- too common for color
         when (bfid sb == side || bfid tb == side) $
           msgAdd MsgTutorialHint "Some hits don't cause piercing, impact, burning nor any other direct damage. However, they can have other effects, bad, good or both."
         animate (blid tb) $ subtleHit ps
       | bproj sb -> do  -- more terse than melee, because sometimes very spammy
         let msgRangedPowerful | targetIsFoe = MsgRangedMightyWe
                               | targetIsFriend = MsgRangedMightyUs
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
         tutorialHintBenignFoe
         animateAlive (blid tb) basicAnim
       | bproj tb -> do  -- much less emotion and the victim not active.
         let attackParts =
               [MU.SubjectVerbSg spart verb, tpart] ++ weaponNameWith
         msgAdd MsgMeleeOthers $ makeSentence attackParts
         animateAlive (blid tb) basicAnim
       | otherwise -> do  -- ordinary melee
         let msgMeleeInteresting | targetIsFoe = MsgMeleeComplexWe
                                 | targetIsFriend = MsgMeleeComplexUs
                                 | otherwise = msgClassMelee
             msgMeleePowerful | targetIsFoe = MsgMeleeMightyWe
                              | targetIsFriend = MsgMeleeMightyUs
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
                       if hurtMult > 70
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
         tutorialHintBenignFoe
         animateAlive (blid tb) basicAnim
