-- | Semantics of client UI response to atomic commands.
-- See
-- <https://github.com/kosmikus/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Client.AtomicSemCli
  ( cmdAtomicSem, cmdAtomicSemCli, cmdAtomicFilterCli
  , drawCmdAtomicUI, drawSfxAtomicUI
  ) where

import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Maybe
import qualified Data.Monoid as Monoid
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.Draw
import Game.LambdaHack.Client.HumanLocal
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Animation
import Game.LambdaHack.Common.AtomicCmd
import Game.LambdaHack.Common.AtomicPos
import Game.LambdaHack.Common.AtomicSem
import qualified Game.LambdaHack.Common.Color as Color
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Utils.Assert

-- * CmdAtomicAI

-- | Clients keep a subset of atomic commands sent by the server
-- and add some of their own. The result of this function is the list
-- of commands kept for each command received.
cmdAtomicFilterCli :: MonadClient m => CmdAtomic -> m [CmdAtomic]
cmdAtomicFilterCli cmd = case cmd of
  AlterTileA lid p fromTile toTile -> do
    Kind.COps{cotile = Kind.Ops{oname}} <- getsState scops
    lvl@Level{lxsize} <- getLevel lid
    let t = lvl `at` p
    if t == fromTile
      then return [cmd]
      else do
        -- From alterTileA@ we know @t == freshClientTile@,
        -- which is uncanny, so we produce a message.
        -- It happens when a client thinks the tile is @t@,
        -- but it's @fromTile@, and @AlterTileA@ changes it
        -- to @toTile@. See @alterTileA@.
        let subject = ""  -- a hack, we we don't handle adverbs well
            verb = "turn into"
            msg = makeSentence [ "the", MU.Text $ oname t
                               , "at position", MU.Text $ showPoint lxsize p
                               , "suddenly"  -- adverb
                               , MU.SubjectVerbSg subject verb
                               , MU.AW $ MU.Text $ oname toTile ]
        return [ cmd  -- reveal the tile
               , MsgAllA msg  -- show the message
               ]
  SearchTileA aid p fromTile toTile -> do
    b <- getsState $ getActorBody aid
    lvl <- getLevel $ blid b
    let t = lvl `at` p
    if t == toTile
      then -- Already knows the tile fully.
           return []
      else if t == fromTile
           then -- Fully ignorant. (No intermediate knowledge possible.)
                return [ AlterTileA (blid b) p fromTile toTile  -- reveal tile
                       , cmd  -- show the message
                       ]
           else -- Misguided.
                assert `failure` "LoseTile fails to reset memory"
                       `with` (aid, p, fromTile, toTile, b, t, cmd)
  DiscoverA _ _ iid _ -> do
    disco <- getsClient sdisco
    item <- getsState $ getItemBody iid
    if jkindIx item `EM.member` disco
      then return []
      else return [cmd]
  CoverA _ _ iid _ -> do
    disco <- getsClient sdisco
    item <- getsState $ getItemBody iid
    if jkindIx item `EM.notMember` disco
      then return []
      else return [cmd]
  PerceptionA lid outPA inPA -> do
    -- Here we cheat by setting a new perception outright instead of
    -- in @cmdAtomicSemCli@, to avoid computing perception twice.
    -- TODO: try to assert similar things as for @atomicRemember@:
    -- that posCmdAtomic of all the Lose* commands was visible in old Per,
    -- but is not visible any more.
    perOld <- getPerFid lid
    perceptionA lid outPA inPA
    perNew <- getPerFid lid
    s <- getState
    fid <- getsClient sside
    -- Wipe out actors that just became invisible due to changed FOV.
    -- TODO: perhaps instead create LoseActorA for all actors in lprio,
    -- and keep only those where seenAtomicCli is True; this is even
    -- cheaper than repeated posToActor (until it's optimized).
    let outFov = totalVisible perOld ES.\\ totalVisible perNew
        outPrio = mapMaybe (\p -> posToActor p lid s) $ ES.elems outFov
        fActor aid =
          let b = getActorBody aid s
          in if bfid b == fid  -- optimization; precludes DominateActorA
             then Nothing
             else Just $ LoseActorA aid b (getActorItem aid s)
        outActor = mapMaybe fActor outPrio
    -- Wipe out remembered items on tiles that now came into view.
    Level{lfloor, lsmell} <- getLevel lid
    let inFov = totalVisible perNew ES.\\ totalVisible perOld
        pMaybe p = maybe Nothing (\x -> Just (p, x))
        inFloor = mapMaybe (\p -> pMaybe p $ EM.lookup p lfloor)
                           (ES.elems inFov)
        fItem p (iid, k) = LoseItemA iid (getItemBody iid s) k (CFloor lid p)
        fBag (p, bag) = map (fItem p) $ EM.assocs bag
        inItem = concatMap fBag inFloor
    -- Remembered map tiles not wiped out, due to optimization in @spotTileA@.
    -- Wipe out remembered smell on tiles that now came into smell Fov.
    let inSmellFov = smellVisible perNew ES.\\ smellVisible perOld
        inSm = mapMaybe (\p -> pMaybe p $ EM.lookup p lsmell)
                        (ES.elems inSmellFov)
        inSmell = if null inSm then [] else [LoseSmellA lid inSm]
    let seenNew = seenAtomicCli False fid perNew
        seenOld = seenAtomicCli False fid perOld
    -- TODO: these assertions are probably expensive
    psActor <- mapM posCmdAtomic outActor
    -- Verify that we forget only previously seen actors.
    assert (allB seenOld psActor) skip
    -- Verify that we forget only currently invisible actors.
    assert (allB (not . seenNew) psActor) skip
    psItemSmell <- mapM posCmdAtomic $ inItem ++ inSmell
    -- Verify that we forget only previously invisible items and smell.
    assert (allB (not . seenOld) psItemSmell) skip
    -- Verify that we forget only currently seen items and smell.
    assert (allB seenNew psItemSmell) skip
    return $ cmd : outActor ++ inItem ++ inSmell
  _ -> return [cmd]

-- | Effect of atomic actions on client state is calculated
-- in the global state before the command is executed.
-- Clients keep a subset of atomic commands sent by the server
-- and add their own. The result of this function is the list of commands
-- kept for each command received.
cmdAtomicSemCli :: MonadClient m => CmdAtomic -> m ()
cmdAtomicSemCli cmd = case cmd of
  LeadFactionA fid source target -> do
    side <- getsClient sside
    when (side == fid) $ do
      mleader <- getsClient _sleader
      assert (mleader == source     -- somebody changed the leader for us
              || mleader == target  -- we changed the leader originally
              `blame` (cmd, mleader)) skip
      modifyClient $ \cli -> cli {_sleader = target}
  DiscoverA lid p iid ik -> discoverA lid p iid ik
  CoverA lid p iid ik -> coverA lid p iid ik
  PerceptionA lid outPA inPA -> perceptionA lid outPA inPA
  RestartA _ sdisco sfper s sdebugCli _ -> do
    side <- getsClient sside
    let fact = sfactionD s EM.! side
    shistory <- getsClient shistory
    sconfigUI <- getsClient sconfigUI
    isAI <- getsClient sisAI
    let cli = defStateClient shistory sconfigUI side isAI
    putClient cli { sdisco
                  , sfper
                  , _sleader = gleader fact
                  , sundo = [CmdAtomic cmd]
                  , sdebugCli}
  ResumeA _fid sfper -> modifyClient $ \cli -> cli {sfper}
  KillExitA _fid -> killExitA
  SaveBkpA -> saveClient
  _ -> return ()

perceptionA :: MonadClient m => LevelId -> PerActor -> PerActor -> m ()
perceptionA lid outPA inPA = do
  cops <- getsState scops
  s <- getState
  -- Clients can't compute FOV on their own, because they don't know
  -- if unknown tiles are clear or not. Server would need to send
  -- info about properties of unknown tiles, which complicates
  -- and makes heavier the most bulky data set in the game: tile maps.
  -- Note we assume, but do not check that @outPA@ is contained
  -- in current perception and @inPA@ has no common part with it.
  -- It would make the already very costly operation even more expensive.
  perOld <- getPerFid lid
  -- Check if new perception is already set in @cmdAtomicFilterCli@
  -- or if we are doing undo/redo, which does not involve filtering.
  -- The data structure is strict, so the cheap check can't be any simpler.
  let interHead [] = Nothing
      interHead ((aid, vis) : _) =
        Just $ pvisible vis `ES.intersection`
                 maybe ES.empty pvisible (EM.lookup aid (perActor perOld))
      unset = maybe False ES.null (interHead (EM.assocs inPA))
              || maybe False (not . ES.null) (interHead (EM.assocs outPA))
  when unset $ do
    let dummyToPer Perception{perActor} = Perception
          { perActor
          , ptotal = PerceptionVisible
                     $ ES.unions $ map pvisible $ EM.elems perActor
          , psmell = smellFromActors cops s perActor }
        paToDummy perActor = Perception
          { perActor
          , ptotal = PerceptionVisible ES.empty
          , psmell = PerceptionVisible ES.empty }
        outPer = paToDummy outPA
        inPer = paToDummy inPA
        adj Nothing = assert `failure` "no perception to alter" `with` lid
        adj (Just per) = Just $ dummyToPer $ addPer (diffPer per outPer) inPer
        f = EM.alter adj lid
    modifyClient $ \cli -> cli {sfper = f (sfper cli)}

discoverA :: MonadClient m
          => LevelId -> Point -> ItemId -> Kind.Id ItemKind -> m ()
discoverA lid p iid ik = do
  item <- getsState $ getItemBody iid
  let f Nothing = Just ik
      f (Just ik2) = assert `failure` "already discovered"
                            `with` (lid, p, iid, ik, ik2)
  modifyClient $ \cli -> cli {sdisco = EM.alter f (jkindIx item) (sdisco cli)}

coverA :: MonadClient m
       => LevelId -> Point -> ItemId -> Kind.Id ItemKind -> m ()
coverA lid p iid ik = do
  item <- getsState $ getItemBody iid
  let f Nothing = assert `failure` "already covered" `with` (lid, p, iid, ik)
      f (Just ik2) = assert (ik == ik2 `blame` (ik, ik2)) Nothing
  modifyClient $ \cli -> cli {sdisco = EM.alter f (jkindIx item) (sdisco cli)}

killExitA :: MonadClient m => m ()
killExitA = modifyClient $ \cli -> cli {squit = True}

-- * CmdAtomicUI

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
drawCmdAtomicUI :: MonadClientUI m => Bool -> CmdAtomic -> m ()
drawCmdAtomicUI verbose cmd = case cmd of
  CreateActorA aid body _ -> do
    when verbose $ actorVerbMU aid body "appear"
    lookAtMove aid
  DestroyActorA aid body _ -> do
    side <- getsClient sside
    if bhp body <= 0 && not (bproj body) && bfid body == side then do
      actorVerbMU aid body "die"
      void $ displayMore ColorBW ""
    else when verbose $ actorVerbMU aid body "disappear"
  CreateItemA _ item k _ | verbose -> itemVerbMU item k "appear"
  DestroyItemA _ item k _ | verbose -> itemVerbMU item k "disappear"
  LoseActorA aid body _ -> do
    side <- getsClient sside
    -- If no other faction actor is looking, death is invisible and
    -- so is domination, time-freeze, etc. Then, this command appears instead.
    when (bfid body == side && bhp body <= 0 && not (bproj body)) $ do
      actorVerbMU aid body "be missing in action"
      void $ displayMore ColorFull ""
  MoveActorA aid _ _ -> lookAtMove aid
  WaitActorA aid _ _| verbose -> aVerbMU aid "wait"
  DisplaceActorA source target -> displaceActorUI source target
  MoveItemA iid k c1 c2 -> moveItemUI verbose iid k c1 c2
  HealActorA aid n | verbose ->
    aVerbMU aid $ MU.Text $ if n > 0
                            then "heal"  <+> showT n <> "HP"
                            else "be about to lose" <+> showT n <> "HP"
  HasteActorA aid delta ->
    aVerbMU aid $ if delta > speedZero
                  then "speed up"
                  else "slow down"
  LeadFactionA fid (Just source) (Just target) -> do
    Kind.COps{coactor} <- getsState scops
    side <- getsClient sside
    when (fid == side) $ do
      actorD <- getsState sactorD
      case EM.lookup source actorD of
        Just sb | bhp sb <= 0 -> assert (not $ bproj sb) $ do
          -- Regardless who the leader is, give proper names here, not 'you'.
          tb <- getsState $ getActorBody target
          let subject = partActor coactor tb
              object  = partActor coactor sb
          msgAdd $ makeSentence [ MU.SubjectVerbSg subject "take command"
                                , "from", object ]
        _ -> skip
  DiplFactionA fid1 fid2 _ toDipl -> do
    name1 <- getsState $ gname . (EM.! fid1) . sfactionD
    name2 <- getsState $ gname . (EM.! fid2) . sfactionD
    let showDipl Unknown = "unknown to each other"
        showDipl Neutral = "in neutral diplomatic relations"
        showDipl Alliance = "allied"
        showDipl War = "at war"
    msgAdd $ name1 <+> "and" <+> name2 <+> "are now" <+> showDipl toDipl <> "."
  QuitFactionA fid mbody _ toSt -> quitFactionUI fid mbody toSt
  AlterTileA{} | verbose ->
    return ()  -- TODO: door opens
  SearchTileA aid _ fromTile toTile -> do
    Kind.COps{cotile = Kind.Ops{oname}} <- getsState scops
    subject <- partAidLeader aid
    let verb = "reveal that the"
        subject2 = MU.Text $ oname fromTile
        verb2 = "be"
    let msg = makeSentence [ MU.SubjectVerbSg subject verb
                           , MU.SubjectVerbSg subject2 verb2
                           , "a hidden"
                           , MU.Text $ oname toTile ]
    msgAdd msg
  AgeGameA t -> do
    when (t > timeClip) $ displayFrames [Nothing]  -- show delay
    -- TODO: shows messages on leader level, instead of recently shown
    -- level (e.g., between animations); perhaps draw messages separately
    -- from level (but on the same text window) or keep last level frame
    -- and only overlay messages on it when needed; or store the level
    -- of last shown
    displayPush  -- TODO: is this really needed? write why
  DiscoverA _ _ iid _ -> do
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
  CoverA _ _ iid ik -> do
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
  RestartA _ _ _ _ _ t ->
    msgAdd $ "New game started in" <+> t <+> "mode."
  SaveBkpA | verbose -> msgAdd "Saving backup."
  MsgAllA msg -> msgAdd msg
  _ -> return ()

lookAtMove :: MonadClientUI m => ActorId -> m ()
lookAtMove aid = do
  body <- getsState $ getActorBody aid
  side <- getsClient sside
  tgtMode <- getsClient stgtMode
  when (not (bproj body)
        && bfid body == side
        && isNothing tgtMode) $ do  -- targeting does a more extensive look
    lookMsg <- lookAt False True (bpos body) aid ""
    msgAdd lookMsg

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
itemVerbMU item k verb = do
  Kind.COps{coitem} <- getsState scops
  disco <- getsClient sdisco
  let msg =
        makeSentence [MU.SubjectVerbSg (partItemWs coitem disco k item) verb]
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

moveItemUI :: MonadClientUI m
          => Bool -> ItemId -> Int -> Container -> Container -> m ()
moveItemUI verbose iid k c1 c2 = do
  Kind.COps{coitem} <- getsState scops
  item <- getsState $ getItemBody iid
  disco <- getsClient sdisco
  case (c1, c2) of
    (CFloor _ _, CActor aid l) -> do
      b <- getsState $ getActorBody aid
      unless (bproj b) $ do
        let n = bbag b EM.! iid
        side <- getsClient sside
        if bfid b == side then
          msgAdd $ makePhrase [ letterLabel l
                              , partItemWs coitem disco n item
                              , "\n" ]
        else aiVerbMU aid "pick up" iid k
    (CActor aid _, CFloor _ _) | verbose ->
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
  Kind.COps{coitem=Kind.Ops{oname, ouniqGroup}} <- getsState scops
  factionD <- getsState sfactionD
  let fact = factionD EM.! fid
      fidName = MU.Text $ gname fact
  side <- getsClient sside
  spawn <- getsState $ isSpawnFaction fid
  summon <- getsState $ isSummonFaction fid
  let msgIfSide _ | fid /= side = Nothing
      msgIfSide s = Just s
      (startingPart, partingPart) = case toSt of
        _ | summon && not spawn ->
          (Nothing, Nothing)  -- Ignore summoned actors factions.
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
      let currencyName = MU.Text $ oname $ ouniqGroup "currency"
          itemMsg = makeSentence [ "Your loot is worth"
                                 , MU.CarWs total currencyName ]
                    <+> moreMsg
      startingSlide <- promptToSlideshow moreMsg
      recordHistory  -- we are going to exit or restart, so record
      itemSlides <-
        if EM.null bag then return Monoid.mempty
        else do
          io <- floorItemOverlay bag
          overlayToSlideshow itemMsg io
      scoreSlides <- scoreToSlideshow total status
      partingSlide <- promptToSlideshow $ pp <+> moreMsg
      shutdownSlide <- promptToSlideshow pp
      -- TODO: First ESC cancels items display.
      void $ getInitConfirms []
        $ startingSlide Monoid.<> itemSlides
      -- TODO: Second ESC cancels high score and parting message display.
      -- The last slide stays onscreen during shutdown, etc.
          Monoid.<> scoreSlides Monoid.<> partingSlide Monoid.<> shutdownSlide
    _ -> return ()

-- * SfxAtomicUI

drawSfxAtomicUI :: MonadClientUI m => Bool -> SfxAtomic -> m ()
drawSfxAtomicUI verbose sfx = case sfx of
  StrikeD source target item b -> strikeD source target item b
  RecoilD source target _ _ -> do
    spart <- partAidLeader source
    tpart <- partAidLeader target
    msgAdd $ makeSentence [MU.SubjectVerbSg spart "shrink away from", tpart]
  ProjectD aid iid -> aiVerbMU aid "aim" iid 1
  CatchD aid iid -> aiVerbMU aid "catch" iid 1
  ActivateD aid iid -> aiVerbMU aid "activate"{-TODO-} iid 1
  CheckD aid iid -> aiVerbMU aid "check" iid 1
  TriggerD aid _p _feat | verbose ->
    aVerbMU aid "trigger"  -- TODO: opens door
  ShunD aid _p _ | verbose ->
    aVerbMU aid "shun"  -- TODO: shuns stairs down
  EffectD aid effect -> do
    b <- getsState $ getActorBody aid
    side <- getsClient sside
    let fid = bfid b
    if bhp b <= 0 && not (bproj b) || bhp b < 0 then do
      -- We assume the effect is the cause of incapacitation.
      let firstFall | fid == side && bproj b = "fall apart"
                    | fid == side =  "fall down"
                    | bproj b = "break up"
                    | otherwise =  "collapse"
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
        Effect.Dominate ->
          if fid == side then lookAtMove aid
          else do
            fidName <- getsState $ gname . (EM.! fid) . sfactionD
            aVerbMU aid $ MU.Text $ "fall under the influence of" <+> fidName
        Effect.ApplyPerfume ->
          msgAdd "The fragrance quells all scents in the vicinity."
        Effect.Searching{} -> do
          subject <- partActorLeader aid b
          let msg = makeSentence
                [ "It gets lost and"
                , MU.SubjectVerbSg subject "search in vain" ]
          msgAdd msg
        Effect.Ascend{} -> actorVerbMU aid b "find a way upstairs"
        Effect.Descend{} -> actorVerbMU aid b "find a way downstairs"
        _ -> return ()
  MsgFidD _ msg -> msgAdd msg
  MsgAllD msg -> msgAdd msg
  DisplayPushD _ ->
    -- TODO: shows messages on leader level, instead of recently shown
    -- level (e.g., between animations); perhaps draw messages separately
    -- from level (but on the same text window) or keep last level frame
    -- and only overlay messages on it when needed; or store the level
    -- of last shown
    displayPush
  DisplayDelayD _ -> displayFrames [Nothing]
  RecordHistoryD _ -> recordHistory
  _ -> return ()

strikeD :: MonadClientUI m
        => ActorId -> ActorId -> Item -> HitAtomic -> m ()
strikeD source target item b = assert (source /= target) $ do
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
      msg MissBlockD =
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
      anim HitD = twirlSplash ps Color.BrRed Color.Red
      anim HitBlockD = blockHit ps Color.BrRed Color.Red
      anim MissBlockD = blockMiss ps
  animFrs <- animate (blid sb) $ anim b
  displayFrames $ Nothing : animFrs
