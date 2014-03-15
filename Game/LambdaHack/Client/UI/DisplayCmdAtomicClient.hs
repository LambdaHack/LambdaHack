-- | Display atomic commands received by the client.
module Game.LambdaHack.Client.UI.DisplayCmdAtomicClient
  ( displayRespUpdAtomicUI, displayRespSfxAtomicUI
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import Data.Monoid
import Data.Tuple
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Atomic
import Game.LambdaHack.Client.CommonClient
import Game.LambdaHack.Client.ItemSlot
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Animation
import qualified Game.LambdaHack.Common.Color as Color
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.TileKind

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
-- | Visualization of atomic actions for the client is perfomed
-- in the global state after the command is executed and after
-- the client state is modified by the command.
displayRespUpdAtomicUI :: MonadClientUI m => Bool -> UpdAtomic -> m ()
displayRespUpdAtomicUI verbose cmd = case cmd of
  UpdCreateActor aid body _ -> createActorUI aid body verbose "appear"
  UpdDestroyActor aid body _ -> do
    destroyActorUI aid body "die" "be destroyed" verbose
    side <- getsClient sside
    when (bfid body == side && not (bproj body)) stopPlayBack
  UpdCreateItem _ item k _ -> itemVerbMU item k "drop to the ground"
  UpdDestroyItem _ item k _ -> itemVerbMU item k "disappear"
  UpdSpotActor aid body _ -> createActorUI aid body verbose "be spotted"
  UpdLoseActor aid body _ ->
    destroyActorUI aid body "be missing in action" "be lost" verbose
  UpdSpotItem _ item k c -> do
    scursorOld <- getsClient scursor
    case scursorOld of
      TEnemy{} -> return ()  -- probably too important to overwrite
      TEnemyPos{} -> return ()
      _ -> do
        (lid, p) <- posOfContainer c
        modifyClient $ \cli -> cli {scursor = TPoint lid p}
        stopPlayBack
        -- TODO: perhaps don't spam for already seen items; very hard to do
        itemVerbMU item k "be spotted"
  UpdMoveActor aid _ _ -> lookAtMove aid
  UpdWaitActor aid _ _| verbose -> aVerbMU aid "wait"
  UpdDisplaceActor source target -> displaceActorUI source target
  UpdMoveItem iid k c1 c2 -> moveItemUI verbose iid k c1 c2
  UpdHealActor aid n -> do
    when verbose $
      aVerbMU aid $ MU.Text $ (if n > 0 then "heal" else "lose")
                              <+> tshow (abs n) <> "HP"
    mleader <- getsClient _sleader
    when (Just aid == mleader) $ do
      Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
      b <- getsState $ getActorBody aid
      let ActorKind{ahp, acalm} = okind $ bkind b
      -- TODO: if one of these does not regenerate, ignore it
      when (bhp b == maxDice ahp && bcalm b == maxDice acalm) $ do
        actorVerbMU aid b "recover fully"
        stopPlayBack
  UpdHasteActor aid delta ->
    aVerbMU aid $ if delta > speedZero
                  then "speed up"
                  else "slow down"
  UpdLeadFaction fid (Just source) (Just target) -> do
    side <- getsClient sside
    when (fid == side) $ do
      actorD <- getsState sactorD
      case EM.lookup source actorD of
        Just sb | bhp sb <= 0 -> assert (not $ bproj sb) $ do
          -- Regardless who the leader is, give proper names here, not 'you'.
          tb <- getsState $ getActorBody target
          let subject = partActor tb
              object  = partActor sb
          msgAdd $ makeSentence [ MU.SubjectVerbSg subject "take command"
                                , "from", object ]
        _ -> skip
  UpdDiplFaction fid1 fid2 _ toDipl -> do
    name1 <- getsState $ gname . (EM.! fid1) . sfactionD
    name2 <- getsState $ gname . (EM.! fid2) . sfactionD
    let showDipl Unknown = "unknown to each other"
        showDipl Neutral = "in neutral diplomatic relations"
        showDipl Alliance = "allied"
        showDipl War = "at war"
    msgAdd $ name1 <+> "and" <+> name2 <+> "are now" <+> showDipl toDipl <> "."
  UpdQuitFaction fid mbody _ toSt -> quitFactionUI fid mbody toSt
  UpdAlterTile{} | verbose ->
    return ()  -- TODO: door opens
  UpdSearchTile aid p fromTile toTile -> do
    Kind.COps{cotile = Kind.Ops{okind}} <- getsState scops
    b <- getsState $ getActorBody aid
    lvl <- getLevel $ blid b
    subject <- partAidLeader aid
    let t = lvl `at` p
        verb | t == toTile = "confirm"
             | otherwise = "reveal"
        subject2 = MU.Text $ tname $ okind fromTile
        verb2 = "be"
    let msg = makeSentence [ MU.SubjectVerbSg subject verb
                           , "that the"
                           , MU.SubjectVerbSg subject2 verb2
                           , "a hidden"
                           , MU.Text $ tname $ okind toTile ]
    msgAdd msg
  UpdAgeGame t -> do
    when (t > timeClip) $ displayFrames [Nothing]  -- show delay
    -- TODO: shows messages on leader level, instead of recently shown
    -- level (e.g., between animations); perhaps display messages separately
    -- from level (but on the same text window) or keep last level frame
    -- and only overlay messages on it when needed; or store the level
    -- of last shown
    displayPush  -- TODO: is this really needed? write why
  UpdDiscover _ _ iid _ -> do
    disco <- getsClient sdisco
    item <- getsState $ getItemBody iid
    let ix = jkindIx item
    Kind.COps{coitem} <- getsState scops
    let discoUnknown = EM.delete ix disco
        (objUnkown1, objUnkown2) = partItem coitem discoUnknown item
        msg = makeSentence
          [ "the", MU.SubjectVerbSg (MU.Phrase [objUnkown1, objUnkown2])
                                    "turn out to be"
          , partItemAW coitem disco item ]
    msgAdd msg
  UpdCover _ _ iid ik -> do
    discoUnknown <- getsClient sdisco
    item <- getsState $ getItemBody iid
    let ix = jkindIx item
    Kind.COps{coitem} <- getsState scops
    let disco = EM.insert ix ik discoUnknown
        (objUnkown1, objUnkown2) = partItem coitem discoUnknown item
        (obj1, obj2) = partItem coitem disco item
        msg = makeSentence
          [ "the", MU.SubjectVerbSg (MU.Phrase [obj1, obj2])
                                    "look like an ordinary"
          , objUnkown1, objUnkown2 ]
    msgAdd msg
  UpdRestart _ _ _ _ _ t -> msgAdd $ "New game started in" <+> t <+> "mode."
  UpdSaveBkp | verbose -> msgAdd "Saving backup."
  UpdMsgAll msg -> msgAdd msg
  _ -> return ()

lookAtMove :: MonadClientUI m => ActorId -> m ()
lookAtMove aid = do
  body <- getsState $ getActorBody aid
  side <- getsClient sside
  tgtMode <- getsClient stgtMode
  when (not (bproj body)
        && bfid body == side
        && isNothing tgtMode) $ do  -- targeting does a more extensive look
    lookMsg <- lookAt False "" True (bpos body) aid ""
    msgAdd lookMsg
  fact <- getsState $ (EM.! bfid body) . sfactionD
  Level{lxsize, lysize} <- getsState $ (EM.! blid body) . sdungeon
  if side == bfid body then do
    foes <- getsState $ actorList (isAtWar fact) (blid body)
    when (foesAdjacent lxsize lysize (bpos body) foes) stopPlayBack
  else when (isAtWar fact side) $ do
    foes <- getsState $ actorNotProjList (== side) (blid body)
    when (foesAdjacent lxsize lysize (bpos body) foes) stopPlayBack

-- | Sentences such as \"Dog barks loudly.\".
actorVerbMU :: MonadClientUI m => ActorId -> Actor -> MU.Part -> m ()
actorVerbMU aid b verb = do
  subject <- partActorLeader aid b
  msgAdd $ makeSentence [MU.SubjectVerbSg subject verb]

aVerbMU :: MonadClientUI m => ActorId -> MU.Part -> m ()
aVerbMU aid verb = do
  b <- getsState $ getActorBody aid
  actorVerbMU aid b verb

itemVerbMU :: MonadClientUI m => Item -> Int -> MU.Part -> m ()
itemVerbMU item k verb = assert (k > 0) $ do
  Kind.COps{coitem} <- getsState scops
  disco <- getsClient sdisco
  let subject = partItemWs coitem disco k item
      msg | k > 1 = makeSentence [MU.SubjectVerb MU.PlEtc MU.Yes subject verb]
          | otherwise = makeSentence [MU.SubjectVerbSg subject verb]
  msgAdd msg

_iVerbMU :: MonadClientUI m => ItemId -> Int -> MU.Part -> m ()
_iVerbMU iid k verb = do
  item <- getsState $ getItemBody iid
  itemVerbMU item k verb

aiVerbMU :: MonadClientUI m => ActorId -> MU.Part -> ItemId -> Int -> m ()
aiVerbMU aid verb iid k = do
  Kind.COps{coitem} <- getsState scops
  disco <- getsClient sdisco
  item <- getsState $ getItemBody iid
  subject <- partAidLeader aid
  let msg = makeSentence [ MU.SubjectVerbSg subject verb
                         , partItemWs coitem disco k item ]
  msgAdd msg

-- TODO: "XXX spots YYY"? or blink or show the changed cursor?
createActorUI :: MonadClientUI m => ActorId -> Actor -> Bool -> MU.Part -> m ()
createActorUI aid body verbose verb = do
  side <- getsClient sside
  when (verbose || bfid body /= side) $ actorVerbMU aid body verb
  when (bfid body /= side) $ do
    fact <- getsState $ (EM.! bfid body) . sfactionD
    when (not (bproj body) && isAtWar fact side) $ do
      -- Target even if nobody can aim at the enemy. Let's home in on him
      -- and then we can aim or melee. We set permit to False, because it's
      -- technically very hard to check aimability here, because we are
      -- in-between turns and, e.g., leader's move has not yet been taken
      -- into account.
      modifyClient $ \cli -> cli {scursor = TEnemy aid False}
    stopPlayBack
  lookAtMove aid

destroyActorUI :: MonadClientUI m
               => ActorId -> Actor -> MU.Part -> MU.Part -> Bool -> m ()
destroyActorUI aid body verb verboseVerb verbose = do
  side <- getsClient sside
  if (bfid body == side && bhp body <= 0 && not (bproj body)) then do
    actorVerbMU aid body verb
    void $ displayMore ColorBW ""
  else when verbose $ actorVerbMU aid body verboseVerb

moveItemUI :: MonadClientUI m
           => Bool -> ItemId -> Int -> Container -> Container -> m ()
moveItemUI verbose iid k c1 c2 = do
  Kind.COps{coitem} <- getsState scops
  case (c1, c2) of
    (CActor _ CGround, CActor aid cstore) -> do
      b <- getsState $ getActorBody aid
      unless (bproj b) $ do
        side <- getsClient sside
        if bfid b == side then do
          item <- getsState $ getItemBody iid
          disco <- getsClient sdisco
          slots <- getsClient sslots
          bag <- getsState $ getCBag $ CActor aid cstore
          let n = bag EM.! iid
          case lookup iid $ map swap $ EM.assocs slots of
            Just l -> msgAdd $ makePhrase
                        [ slotLabel l
                        , partItemWs coitem disco n item
                        , "\n" ]
            Nothing -> assert `failure` (aid, b, iid, slots)
        else aiVerbMU aid "get" iid k
    (CActor aid _, CActor _ CGround) | verbose ->
      aiVerbMU aid "drop" iid k
    _ -> return ()

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
  displayFrames $ Nothing : animFrs

quitFactionUI :: MonadClientUI m
              => FactionId -> Maybe Actor -> Maybe Status -> m ()
quitFactionUI fid mbody toSt = do
  cops@Kind.COps{coitem=Kind.Ops{okind, ouniqGroup}} <- getsState scops
  fact <- getsState $ (EM.! fid) . sfactionD
  let fidName = MU.Text $ gname fact
      horror = isHorrorFact cops fact
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
        Just Status{stOutcome=Restart, stInfo} ->
          ( Just $ MU.Text $ "order mission restart in" <+> stInfo <+> "mode"
          , Just $ if fid == side
                   then "This time for real."
                   else "Somebody couldn't stand the heat." )
        Nothing ->
          (Nothing, Nothing)  -- Wipe out the quit flag for the savegame files.
  case startingPart of
    Nothing -> return ()
    Just sp -> do
      let msg = makeSentence [MU.SubjectVerbSg fidName sp]
      msgAdd msg
  case (toSt, partingPart) of
    (Just status, Just pp) -> do
      (bag, total) <- case mbody of
        Just body | fid == side -> getsState $ calculateTotal body
        _ -> case gleader fact of
          Nothing -> return (EM.empty, 0)
          Just aid -> do
            b <- getsState $ getActorBody aid
            getsState $ calculateTotal b
      let currencyName = MU.Text $ iname $ okind $ ouniqGroup "currency"
          itemMsg = makeSentence [ "Your loot is worth"
                                 , MU.CarWs total currencyName ]
                    <+> moreMsg
      startingSlide <- promptToSlideshow moreMsg
      recordHistory  -- we are going to exit or restart, so record
      itemSlides <-
        if EM.null bag then return mempty
        else do
          slots <- getsClient sslots
          let sl = EM.filter (`EM.member` bag) slots
          io <- itemOverlay bag sl
          overlayToSlideshow itemMsg io
      -- Show score for any UI client, even though it is saved only
      -- for human UI clients.
      scoreSlides <- scoreToSlideshow total status
      partingSlide <- promptToSlideshow $ pp <+> moreMsg
      shutdownSlide <- promptToSlideshow pp
      -- TODO: First ESC cancels items display.
      void $ getInitConfirms ColorFull []
           $ startingSlide <> itemSlides
      -- TODO: Second ESC cancels high score and parting message display.
      -- The last slide stays onscreen during shutdown, etc.
          <> scoreSlides <> partingSlide <> shutdownSlide
    _ -> return ()

-- * RespSfxAtomicUI

displayRespSfxAtomicUI :: MonadClientUI m => Bool -> SfxAtomic -> m ()
displayRespSfxAtomicUI verbose sfx = case sfx of
  SfxStrike source target item b -> strike source target item b
  SfxRecoil source target _ _ -> do
    spart <- partAidLeader source
    tpart <- partAidLeader target
    msgAdd $ makeSentence [MU.SubjectVerbSg spart "shrink away from", tpart]
  SfxProject aid iid -> aiVerbMU aid "aim" iid 1
  SfxCatch aid iid -> aiVerbMU aid "catch" iid 1
  SfxActivate aid iid -> aiVerbMU aid "activate"{-TODO-} iid 1
  SfxCheck aid iid -> aiVerbMU aid "check" iid 1
  SfxTrigger aid _p _feat | verbose ->
    aVerbMU aid "trigger"  -- TODO: opens door
  SfxShun aid _p _ | verbose ->
    aVerbMU aid "shun"  -- TODO: shuns stairs down
  SfxEffect aid effect -> do
    b <- getsState $ getActorBody aid
    side <- getsClient sside
    let fid = bfid b
    if bhp b <= 0 && not (bproj b) || bhp b < 0 then do
      -- We assume the effect is the cause of incapacitation.
      let firstFall | fid == side && bproj b = "fall apart"
                    | fid == side = "fall down"
                    | bproj b = "break up"
                    | otherwise = "collapse"
          hurtExtra | fid == side && bproj b = "be stomped flat"
                    | fid == side = "be ground into the floor"
                    | bproj b = "be shattered into little pieces"
                    | otherwise = "be reduced to a bloody pulp"
      subject <- partActorLeader aid b
      let deadPreviousTurn p = p < 0
                               && (bhp b <= p && not (bproj b)
                                   || bhp b < p)
          (deadBefore, verbDie) =
            case effect of
              Effect.Hurt _ p | deadPreviousTurn p -> (True, hurtExtra)
              Effect.Heal p | deadPreviousTurn p -> (True, hurtExtra)
              _ -> (False, firstFall)
          msgDie = makeSentence [MU.SubjectVerbSg subject verbDie]
      msgAdd msgDie
      when (fid == side && not (bproj b)) $ do
        animDie <- if deadBefore
                   then animate (blid b)
                        $ twirlSplash (bpos b, bpos b) Color.Red Color.Red
                   else animate (blid b) $ deathBody $ bpos b
        displayFrames animDie
    else case effect of
        Effect.NoEffect -> msgAdd "Nothing happens."
        Effect.Heal p | p > 0 -> do
          if fid == side then
            actorVerbMU aid b "feel healthier"
          else
            actorVerbMU aid b "look healthier"
          let ps = (bpos b, bpos b)
          animFrs <- animate (blid b) $ twirlSplash ps Color.BrBlue Color.Blue
          displayFrames $ Nothing : animFrs
        Effect.Heal _ -> do
          if fid == side then
            actorVerbMU aid b "feel wounded"
          else
            actorVerbMU aid b "look wounded"
          let ps = (bpos b, bpos b)
          animFrs <- animate (blid b) $ twirlSplash ps Color.BrRed Color.Red
          displayFrames $ Nothing : animFrs
        Effect.Mindprobe nEnemy -> do
          let msg = makeSentence
                [MU.CardinalWs nEnemy "howl", "of anger", "can be heard"]
          msgAdd msg
        Effect.Dominate -> do
          if fid == side then do
            aVerbMU aid $ MU.Text "black out, dominated by foes"
            void $ displayMore ColorFull ""
          else do
            fidName <- getsState $ gname . (EM.! fid) . sfactionD
            aVerbMU aid $ MU.Text $ "be no longer controlled by" <+> fidName
        Effect.ApplyPerfume ->
          msgAdd "The fragrance quells all scents in the vicinity."
        Effect.Searching{} -> do
          subject <- partActorLeader aid b
          let msg = makeSentence
                [ "It gets lost and"
                , MU.SubjectVerbSg subject "search in vain" ]
          msgAdd msg
        Effect.Ascend k | k > 0 -> actorVerbMU aid b "find a way upstairs"
        Effect.Ascend k | k < 0 -> actorVerbMU aid b "find a way downstairs"
        Effect.Ascend{} -> assert `failure` sfx
        _ -> return ()
  SfxMsgFid _ msg -> msgAdd msg
  SfxMsgAll msg -> msgAdd msg
  SfxDisplayPush _ ->
    -- TODO: shows messages on leader level, instead of recently shown
    -- level (e.g., between animations); perhaps display messages separately
    -- from level (but on the same text window) or keep last level frame
    -- and only overlay messages on it when needed; or store the level
    -- of last shown
    displayPush
  SfxDisplayDelay _ -> displayFrames [Nothing]
  SfxRecordHistory _ -> recordHistory
  _ -> return ()

strike :: MonadClientUI m
        => ActorId -> ActorId -> Item -> HitAtomic -> m ()
strike source target item b = assert (source /= target) $ do
  Kind.COps{coitem=coitem@Kind.Ops{okind}} <- getsState scops
  disco <- getsClient sdisco
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  spart <- partActorLeader source sb
  tpart <- partActorLeader target tb
  let (verb, withWhat) | bproj sb = ("hit", False)
                       | otherwise =
        case jkind disco item of
          Nothing -> ("hit", False)  -- not identified
          Just ik -> let kind = okind ik
                     in ( iverbApply kind
                        , isNothing $ lookup "hth" $ ifreq kind )
      msg MissBlock =
        let (partBlock1, partBlock2) =
              if withWhat
              then ("swing", partItemAW coitem disco item)
              else ("try to", verb)
        in makeSentence
          [ MU.SubjectVerbSg spart partBlock1
          , partBlock2 MU.:> ", but"
          , MU.SubjectVerbSg tpart "block"
          ]
      msg _ = makeSentence $
        [MU.SubjectVerbSg spart verb, tpart]
        ++ if withWhat
           then ["with", partItemAW coitem disco item]
           else []
  msgAdd $ msg b
  let ps = (bpos tb, bpos sb)
      anim Hit = twirlSplash ps Color.BrRed Color.Red
      anim HitBlock = blockHit ps Color.BrRed Color.Red
      anim MissBlock = blockMiss ps
  animFrs <- animate (blid sb) $ anim b
  displayFrames $ Nothing : animFrs
