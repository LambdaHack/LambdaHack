{-# LANGUAGE OverloadedStrings #-}
-- | Semantics of client UI response to atomic commands.
module Game.LambdaHack.Client.CmdAtomicUI
  ( cmdAtomicSem, cmdAtomicSemCli, cmdAtomicFilterCli
  , drawCmdAtomicUI, drawDescAtomicUI
  ) where

import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Maybe
import Data.Text (Text)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.Animation
import Game.LambdaHack.Client.Draw
import Game.LambdaHack.Client.State
import Game.LambdaHack.CmdAtomic
import Game.LambdaHack.CmdAtomicSem
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Content.ItemKind
import qualified Game.LambdaHack.Effect as Effect
import Game.LambdaHack.Faction
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.State
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert

-- * CmdAtomicCli

-- | Clients keep a subset of atomic commands sent by the server
-- and add some of their own. The result of this function is the list
-- of commands kept for each command received.
cmdAtomicFilterCli :: MonadClient m => CmdAtomic -> m [CmdAtomic]
cmdAtomicFilterCli cmd = case cmd of
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
    perOld <- getPerFid lid
    perceptionA lid outPA inPA
    perNew <- getPerFid lid
    -- Wipe out remembered items on tiles that now came into view.
    lfloor <- getsLevel lid lfloor
    s <- getState
    let inFov = pvisible (ptotal perNew) ES.\\ pvisible (ptotal perOld)
        pMaybe p = maybe Nothing (\x -> Just (p, x))
        inFloor = mapMaybe (\p -> pMaybe p $ EM.lookup p lfloor)
                           (ES.elems inFov)
        fItem p (iid, k) = LoseItemA iid (getItemBody iid s) k (CFloor lid p)
        fBag (p, bag) = map (fItem p) $ EM.assocs bag
        inItem = concatMap fBag inFloor
    -- Wipe out actors that just became invisible due to changed FOV.
    let outFov = pvisible (ptotal perOld) ES.\\ pvisible (ptotal perNew)
        outPrio = mapMaybe (\p -> posToActor p lid s) $ ES.elems outFov
        fActor aid = LoseActorA aid (getActorBody aid s)
        outActor = map fActor outPrio
    return $ cmd : inItem ++ outActor
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
      mleader <- getsClient sleader
      assert (mleader == source     -- somebody changed the leader for us
              || mleader == target  -- we changed the leader originally
              `blame` (cmd, mleader)) skip
      modifyClient $ \cli -> cli {_sleader = target}
  DiscoverA lid p iid ik -> discoverA lid p iid ik
  CoverA lid p iid ik -> coverA lid p iid ik
  PerceptionA lid outPA inPA -> perceptionA lid outPA inPA
  RestartA _ sdisco sfper s -> do
    -- TODO: here or elsewhere re-read RNG seed from config file
    -- TODO: stop running? or is it done elsewhere?
    side <- getsClient sside
    let fac = sfaction s EM.! side
    shistory <- getsClient shistory
    sconfigUI <- getsClient sconfigUI
    isAI <- getsClient sisAI
    let cli = defStateClient shistory sconfigUI side isAI
    putClient cli {sdisco, sfper, _sleader = gleader fac}
    -- TODO: Save ASAP in case of crashes and disconnects.
  _ -> return ()

perceptionA :: MonadClient m => LevelId -> PerActor -> PerActor -> m ()
perceptionA lid outPA inPA = do
  -- Clients can't compute FOV on their own, because they don't know
  -- if unknown tiles are clear or not. Server would need to send
  -- info about properties of unknown tiles, which complicates
  -- and makes heavier the most bulky data set in the game: tile maps.
  -- Note we assume, but do not check that @outPA@ is contained
  -- in current perseption and @inPA@ has no common part with it.
  -- It would make the already very costly operation even more expensive.
  perOld <- getPerFid lid
  -- Check if new perception already set in @cmdAtomicFilterCli@
  -- or if we are doing undo/redo, which does not involve filtering.
  -- The data structure is strict, so the cheap check can't be simpler.
  let interHead [] = Nothing
      interHead ((a, vis) : _) =
        Just $ pvisible vis `ES.intersection`
                 maybe ES.empty pvisible (EM.lookup a (perActor perOld))
      unset = maybe False ES.null (interHead (EM.assocs inPA))
              || maybe False (not . ES.null) (interHead (EM.assocs outPA))
  when unset $ do
    let dummyToPer per = Perception
          { perActor = perActor per
          , ptotal = PerceptionVisible
                     $ ES.unions $ map pvisible $ EM.elems $ perActor per }
        paToDummy pa = Perception
          { perActor = pa
          , ptotal = PerceptionVisible ES.empty }
        outPer = paToDummy outPA
        inPer = paToDummy inPA
        adj Nothing = assert `failure` lid
        adj (Just per) = Just $ dummyToPer $ addPer (diffPer per outPer) inPer
        f sfper = EM.alter adj lid sfper
    modifyClient $ \cli -> cli {sfper = f (sfper cli)}

discoverA :: MonadClient m
          => LevelId -> Point -> ItemId -> (Kind.Id ItemKind) -> m ()
discoverA lid p iid ik = do
  item <- getsState $ getItemBody iid
  let f Nothing = Just ik
      f (Just ik2) = assert `failure` (lid, p, iid, ik, ik2)
  modifyClient $ \cli -> cli {sdisco = EM.alter f (jkindIx item) (sdisco cli)}

coverA :: MonadClient m
       => LevelId -> Point -> ItemId -> (Kind.Id ItemKind) -> m ()
coverA lid p iid ik = do
  item <- getsState $ getItemBody iid
  let f Nothing = assert `failure` (lid, p, iid, ik)
      f (Just ik2) = assert (ik == ik2 `blame` (ik, ik2)) Nothing
  modifyClient $ \cli -> cli {sdisco = EM.alter f (jkindIx item) (sdisco cli)}

-- * CmdAtomicUI

-- TODO: let user configure which messages are not created, which are
-- slightly hidden, which are shown and which flash and center screen.
-- | Visualizationi of atomic actions for the client is perfomed
-- in the global state after the command is executed and after
-- the client state is modified by the command.
drawCmdAtomicUI :: MonadClientUI m => Bool -> CmdAtomic -> m ()
drawCmdAtomicUI verbose cmd = case cmd of
  CreateActorA _ body | verbose -> actorVerbMU body "appear"
  DestroyActorA _ body -> do
    side <- getsClient sside
    if bhp body <= 0 && not (bproj body) && bfaction body == side then do
      actorVerbMU body "die"
      --  Config{configFirstDeathEnds} <- getsServer sconfig
      -- if isHuman then sendQueryUI fid CarryOnCli else return False
      go <- displayMore ColorBW ""
      when go $ do
        actorD <- getsState sactorD
        let alive (_, b) = bfaction b == side && not (bproj b)
            party = filter alive $ EM.assocs actorD
        when (not $ null $ party) $ msgAdd "The survivors carry on."
    else when verbose $ actorVerbMU body "disappear"
  CreateItemA _ item k _ | verbose -> itemVerbMU item k "appear"
  DestroyItemA _ item k _ | verbose -> itemVerbMU item k "disappear"
  MoveActorA _ _ _ -> return ()  -- too boring even for verbose mode
  WaitActorA aid _ _| verbose -> aVerbMU aid "wait"
  DisplaceActorA source target -> displaceActorA source target
  MoveItemA iid k c1 c2 -> moveItemA verbose iid k c1 c2
  HealActorA aid n | verbose ->
    if n > 0
    then aVerbMU aid $ MU.Text $ "heal"  <+> showT n <> "HP"
    else aVerbMU aid $ MU.Text $ "be about to lose" <+> showT n <> "HP"
  HasteActorA aid delta | verbose ->
    if delta > speedZero
    then aVerbMU aid "speeds up"
    else aVerbMU aid "slows down"
  DominateActorA target _ toFid -> do
    side <- getsClient sside
    if toFid == side then do
      return ()
--    -- Display status line and FOV for the new actor.
--    sli <- promptToSlideshow ""
--    fr <- drawOverlay ColorBW $ head $ runSlideshow sli
--    displayFramesPush [Nothing, Just fr, Nothing]
    else do
      fidName <- getsState $ gname . (EM.! toFid) . sfaction
      aVerbMU target $ MU.Text $ "fall under the influence of" <+> fidName
  QuitFactionA fid _ toSt -> quitFactionA fid toSt
  AlterTileA _ _ _ _ | verbose ->
    return ()  -- TODO: door opens
  AlterSecretA _ _ ->
    assert `failure` ("client learns secrets" :: Text, cmd)
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
  RestartA{} -> msgAdd "This time for real."
  _ -> return ()

-- | Sentences such as \"Dog barks loudly.\".
actorVerbMU :: MonadClientUI m => Actor -> MU.Part -> m ()
actorVerbMU body verb = do
  Kind.COps{coactor} <- getsState scops
  let msg = makeSentence [MU.SubjectVerbSg (partActor coactor body) verb]
  msgAdd msg

aVerbMU :: MonadClientUI m => ActorId -> MU.Part -> m ()
aVerbMU aid verb = do
  body <- getsState $ getActorBody aid
  actorVerbMU body verb

itemVerbMU :: MonadClientUI m => Item -> Int -> MU.Part -> m ()
itemVerbMU item k verb = do
  Kind.COps{coitem} <- getsState scops
  disco <- getsClient sdisco
  let msg =
        makeSentence [MU.SubjectVerbSg (partItemNWs coitem disco k item) verb]
  msgAdd msg

_iVerbMU :: MonadClientUI m => ItemId -> Int -> MU.Part -> m ()
_iVerbMU iid k verb = do
  item <- getsState $ getItemBody iid
  itemVerbMU item k verb

aiVerbMU :: MonadClientUI m => ActorId -> MU.Part -> ItemId -> Int -> m ()
aiVerbMU aid verb iid k = do
  Kind.COps{coactor, coitem} <- getsState scops
  disco <- getsClient sdisco
  body <- getsState $ getActorBody aid
  item <- getsState $ getItemBody iid
  -- TODO: "you" instead of partActor? but keep partActor for other sides
  -- TODO: perhaps automate you/partActor depending on side
  let msg = makeSentence
        [ MU.SubjectVerbSg (partActor coactor body) verb
        , partItemNWs coitem disco k item ]
  msgAdd msg

moveItemA :: MonadClientUI m
          => Bool -> ItemId -> Int -> Container -> Container -> m ()
moveItemA verbose iid k c1 c2 = do
  Kind.COps{coitem} <- getsState scops
  item <- getsState $ getItemBody iid
  disco <- getsClient sdisco
  case (c1, c2) of
    (CFloor _ _, CActor aid l) -> do
      b <- getsState $ getActorBody aid
      let n = bbag b EM.! iid
      side <- getsClient sside
      if bfaction b == side then
        msgAdd $ makePhrase [ letterLabel l
                            , partItemNWs coitem disco n item
                            , "\n" ]
      else aiVerbMU aid "pick up" iid k
    (CActor aid _, CFloor _ _) | verbose -> do
      aiVerbMU aid "drop" iid k
    _ -> return ()

displaceActorA :: MonadClientUI m => ActorId -> ActorId -> m ()
displaceActorA source target = do
  Kind.COps{coactor} <- getsState scops
  sm <- getsState (getActorBody source)
  tm <- getsState (getActorBody target)
  per <- getPerFid (blid sm)
  let msg = makeSentence
        [ MU.SubjectVerbSg (partActor coactor sm) "displace"
        , partActor coactor tm ]
  msgAdd msg
  cli <- getClient
  loc <- getState
  let ps = (bpos tm, bpos sm)
      animFrs = animate cli loc per $ swapPlaces ps
  displayFramesPush $ Nothing : animFrs

quitFactionA :: MonadClientUI m => FactionId -> Maybe (Bool, Status) -> m ()
quitFactionA fid toSt = do
  fidName <- getsState $ MU.Text . gname . (EM.! fid) . sfaction
  case toSt of
    Just (_, Killed _) -> do
      let msg = makeSentence
            [MU.SubjectVerbSg fidName "be decisively defeated"]
      msgAdd msg
    Just (_, Camping) -> do
      let msg = makeSentence
            [MU.SubjectVerbSg fidName "saved his game and left"]
      msgAdd msg
    Just (_, Victor) -> do
      let msg = makeSentence
            [MU.SubjectVerbSg fidName "complete the mission"]
      msgAdd msg
    Just (_, Restart) -> do
      let msg = makeSentence
            [MU.SubjectVerbSg fidName "request a mission restart"]
      msgAdd msg
    Nothing -> do
      let msg = makeSentence
            [MU.SubjectVerbSg fidName "cancel all requests"]
      msgAdd msg

-- * DescAtomicUI

drawDescAtomicUI :: MonadClientUI m => Bool -> DescAtomic -> m ()
drawDescAtomicUI verbose desc = case desc of
  StrikeD source target item b -> strikeD source target item b
  RecoilD source target _ _ -> do
    Kind.COps{coactor} <- getsState scops
    sb <- getsState $ getActorBody source
    tb <- getsState $ getActorBody target
    let msg = makeSentence
          [ MU.SubjectVerbSg (partActor coactor sb) "shrink back from"
          , partActor coactor tb ]
    msgAdd msg
  ProjectD aid iid -> aiVerbMU aid "aim" iid 1
  CatchD aid iid -> aiVerbMU aid "catch" iid 1
  ActivateD aid iid -> aiVerbMU aid "activate"{-TODO-} iid 1
  CheckD aid iid -> aiVerbMU aid "check" iid 1
  TriggerD aid _p _feat _ | verbose ->
    aVerbMU aid $ "trigger"  -- TODO: opens door
  ShunD aid _p _ _ | verbose ->
    aVerbMU aid $ "shun"  -- TODO: shuns stairs down
  EffectD aid effect -> do
    b <- getsState $ getActorBody aid
    if bhp b > 0
      then case effect of
        Effect.NoEffect -> msgAdd "Nothing happens."
        Effect.Heal -> do
          cli <- getClient
          loc <- getState
          per <- getPerFid $ blid b
          aVerbMU aid "feel better"
          let ps = (bpos b, bpos b)
              animFrs = animate cli loc per
                        $ twirlSplash ps Color.BrBlue Color.Blue
          displayFramesPush $ Nothing : animFrs
        Effect.Wound _ -> aVerbMU aid "feel wounded"
        Effect.Mindprobe nEnemy -> do
          -- TODO: NWs with spelled cardinal would be handy
          let msg = makeSentence
                [MU.NWs nEnemy "howl", "of anger", "can be heard"]
          msgAdd msg
        Effect.Dominate | verbose -> aVerbMU aid "be dominated"
        Effect.ApplyPerfume ->
          msgAdd "The fragrance quells all scents in the vicinity."
        Effect.Searching -> do
          Kind.COps{coactor} <- getsState scops
          let msg = makeSentence
                [ "It gets lost and"
                , MU.SubjectVerbSg (partActor coactor b) "search in vain" ]
          msgAdd msg
        Effect.Ascend -> aVerbMU aid "find a way upstairs"
        Effect.Descend -> aVerbMU aid "find a way downstairs"
        _ -> return ()
      else case effect of
        Effect.Wound _ -> do
          -- Presumably the cause of death.
          let verbD = if bproj b then "break up" else "collapse"
          aVerbMU aid verbD
        _ -> return ()
  FailureD fid msg -> do
    side <- getsClient sside
    assert (fid == side) skip
    msgAdd msg
  BroadcastD msg -> msgAdd msg
  DisplayPushD _ -> displayPush
  DisplayDelayD _ -> displayFramesPush [Nothing]
  FlushFramesD _ -> do
    srunning <- getsClient srunning
    case srunning of
      Just (_, k) | k > 1 -> return ()
      _ -> do
        displayPush
        flushFrames
  _ -> return ()

strikeD :: MonadClientUI m
        => ActorId -> ActorId -> Item -> HitAtomic -> m ()
strikeD source target item b = assert (source /= target) $ do
  Kind.COps{coactor, coitem=coitem@Kind.Ops{okind}} <- getsState scops
  disco <- getsClient sdisco
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
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
          [ MU.SubjectVerbSg (partActor coactor sb) partBlock1
          , partBlock2 MU.:> ", but"
          , MU.SubjectVerbSg (partActor coactor tb) "block"
          ]
      msg _ = makeSentence $
        [ MU.SubjectVerbSg (partActor coactor sb) verb
        , partActor coactor tb ]
        ++ if withWhat
           then ["with", partItemAW coitem disco item]
           else []
  cli <- getClient
  loc <- getState
  per <- getPerFid $ blid sb
  side <- getsClient sside
  let (msgDie, animDie) | bhp tb > 0 = ("", [])
                        | otherwise =
        let verbD = if bproj tb then "drop down" else "fall down"
            msgD = makeSentence
                   [MU.SubjectVerbSg (partActor coactor tb) verbD]
            animD = animate cli loc per $ deathBody $ bpos tb
        in (msgD, animD)
  msgAdd $ msg b <+> msgDie
  let ps = (bpos tb, bpos sb)
      anim HitD = twirlSplash ps Color.BrRed Color.Red
      anim HitBlockD = blockHit ps Color.BrRed Color.Red
      anim MissBlockD = blockMiss ps
      animFrs = animate cli loc per (anim b)
  displayFramesPush $ Nothing : animFrs
  -- Animate only when the client's own actor dies.
  when (bfaction tb == side && not (bproj tb)) $ displayFramesPush animDie
