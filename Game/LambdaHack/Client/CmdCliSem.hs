{-# LANGUAGE DeriveDataTypeable, GADTs, OverloadedStrings, StandaloneDeriving
             #-}
-- | Semantics of 'CmdCli' client commands.
module Game.LambdaHack.Client.CmdCliSem where

import Control.Monad
import Control.Monad.Writer.Strict (WriterT, runWriterT)
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.Animation
import Game.LambdaHack.Client.Binding
import Game.LambdaHack.Client.CmdHuman
import Game.LambdaHack.Client.CmdHumanSem
import Game.LambdaHack.Client.Draw
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.LocalAction
import Game.LambdaHack.Client.RunAction
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.Strategy
import Game.LambdaHack.Client.StrategyAction
import Game.LambdaHack.CmdAtomic
import Game.LambdaHack.CmdAtomicSem
import Game.LambdaHack.CmdSer
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.StrategyKind
import qualified Game.LambdaHack.Effect as Effect
import Game.LambdaHack.Faction
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.Random
import Game.LambdaHack.State
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

-- * cmdAtomicCli

cmdAtomicCli :: (MonadAction m, MonadClient m) => CmdAtomic -> m ()
cmdAtomicCli cmd = do
  cmdAtomicSem cmd
  cmdAtomicSemCli cmd

cmdAtomicSemCli :: MonadClient m => CmdAtomic -> m ()
cmdAtomicSemCli cmd = case cmd of
  LeadFactionA fid _ target -> do
    side <- getsClient sside
    when (side == fid) $
      modifyClient $ \cli -> cli {_sleader = target}
  _ -> return ()

-- * cmdAtomicUI

cmdAtomicUI :: (MonadAction m, MonadClientUI m) => CmdAtomic -> m ()
cmdAtomicUI cmd = do
  cmdAtomicSem cmd
  cmdAtomicSemCli cmd
  drawCmdAtomicUI False cmd

-- TODO: let user configure which messages are not created, which are
-- slightly hidden, which are shown and which flash and center screen.
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
        let party = filter (\(_, b) ->
                      bfaction b == side && not (bproj b)) $ EM.assocs actorD
        when (not $ null $ party) $ msgAdd "The survivors carry on."
    else when verbose $ actorVerbMU body "disappear"
  CreateItemA _ _ item k _ -> itemVerbMU item k "appear"
  DestroyItemA _ _ item k _ -> itemVerbMU item k "disappear"
  MoveActorA _ _ _ -> return ()  -- too boring even for verbose mode
  WaitActorA aid _ _| verbose -> aVerbMU aid "wait"
  DisplaceActorA source target -> displaceActorA source target
  MoveItemA _ iid k c1 c2 -> moveItemA verbose iid k c1 c2
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
  LeadFactionA _ _ _ -> return ()  -- boring; display enemy leaders instead
  AlterTileA _lid _p _fromTile _toTile | verbose ->
    return ()  -- TODO: door opens
  AlterSecretA _ _ ->
    assert `failure` ("client learns secrets" :: Text, cmd)
  _ -> return ()

-- | Sentences such as \"Dog barks loudly.\".
actorVerbMU :: MonadClient m => Actor -> MU.Part -> m ()
actorVerbMU body verb = do
  Kind.COps{coactor} <- getsState scops
  let msg = makeSentence [MU.SubjectVerbSg (partActor coactor body) verb]
  msgAdd msg

aVerbMU :: MonadClient m => ActorId -> MU.Part -> m ()
aVerbMU aid verb = do
  body <- getsState $ getActorBody aid
  actorVerbMU body verb

itemVerbMU :: MonadClient m => Item -> Int -> MU.Part -> m ()
itemVerbMU item k verb = do
  Kind.COps{coitem} <- getsState scops
  disco <- getsState sdisco
  let msg =
        makeSentence [MU.SubjectVerbSg (partItemNWs coitem disco k item) verb]
  msgAdd msg

iVerbMU :: MonadClient m => ItemId -> Int -> MU.Part -> m ()
iVerbMU iid k verb = do
  item <- getsState $ getItemBody iid
  itemVerbMU item k verb

aiVerbMU :: MonadClient m => ActorId -> MU.Part -> ItemId -> Int -> m ()
aiVerbMU aid verb iid k = do
  Kind.COps{coactor, coitem} <- getsState scops
  disco <- getsState sdisco
  body <- getsState $ getActorBody aid
  item <- getsState $ getItemBody iid
  -- TODO: "you" instead of partActor? but keep partActor for other sides
  -- TODO: perhaps automate you/partActor depending on side
  let msg = makeSentence
        [ MU.SubjectVerbSg (partActor coactor body) verb
        , partItemNWs coitem disco k item ]
  msgAdd msg

moveItemA :: MonadClient m
          => Bool -> ItemId -> Int -> Container -> Container -> m ()
moveItemA verbose iid k c1 c2 = do
  Kind.COps{coitem} <- getsState scops
  item <- getsState $ getItemBody iid
  disco <- getsState sdisco
  case (c1, c2) of
    (CFloor _, CActor aid l) -> do
      b <- getsState $ getActorBody aid
      side <- getsClient sside
      if bfaction b == side then
        msgAdd $ makePhrase [ letterLabel l
                            , partItemNWs coitem disco k item
                            , "\n" ]
      else aiVerbMU aid "pick up" iid k
    (CActor aid _, CFloor _) | verbose -> do
      aiVerbMU aid "drop" iid k
    _ -> return ()

displaceActorA :: MonadClientUI m => ActorId -> ActorId -> m ()
displaceActorA source target = do
  Kind.COps{coactor} <- getsState scops
  per <- askPerception
  sm <- getsState (getActorBody source)
  tm <- getsState (getActorBody target)
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
    Just (_, Restart) -> do
      let msg = makeSentence
            [MU.SubjectVerbSg fidName "request mission restart"]
      msgAdd msg
    Just (_, Victor) -> do
      let msg = makeSentence
            [MU.SubjectVerbSg fidName "complete the mission"]
      msgAdd msg
    Nothing -> do
      let msg = makeSentence
            [MU.SubjectVerbSg fidName "cancel all requests"]
      msgAdd msg
    _ -> return ()

-- * descAtomicUI

descAtomicUI :: MonadClientUI m => DescAtomic -> m ()
descAtomicUI desc = drawDescAtomicUI False desc

drawDescAtomicUI :: MonadClientUI m => Bool -> DescAtomic -> m ()
drawDescAtomicUI verbose desc = case desc of
  StrikeA source target item b -> strikeA source target item b
  RecoilA source target _ _ -> do
    Kind.COps{coactor} <- getsState scops
    sb <- getsState $ getActorBody source
    tb <- getsState $ getActorBody target
    let msg = makeSentence
          [ MU.SubjectVerbSg (partActor coactor sb) "shrink back from"
          , partActor coactor tb ]
    msgAdd msg
  ProjectA aid iid -> aiVerbMU aid "aim" iid 1
  CatchA aid iid -> aiVerbMU aid "catch" iid 1
  ActivateA aid iid -> aiVerbMU aid "activate"{-TODO-} iid 1
  CheckA aid iid -> aiVerbMU aid "check" iid 1
  TriggerA aid _p _feat _ | verbose ->
    aVerbMU aid $ "trigger"  -- TODO: opens door
  ShunA aid _p _ _ | verbose ->
    aVerbMU aid $ "shun"  -- TODO: shuns stairs down
  EffectA aid effect -> do
    b <- getsState $ getActorBody aid
    if bhp b > 0
      then case effect of
        Effect.NoEffect -> msgAdd "Nothing happens."
        Effect.Heal -> aVerbMU aid "feel better"
        Effect.Wound _ -> aVerbMU aid "feel wounded"
        Effect.Mindprobe nEnemy -> do
          -- TODO: NWs with spelled cardinal would be handy
          let msg = makeSentence
                [MU.SubjectVerbSg (MU.NWs nEnemy "howl of anger") "be heard"]
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
        _ ->  return ()
      else case effect of
        Effect.Wound _ -> do
          -- Presumably the cause of death.
          let verbD = if bproj b then "break up" else "collapse"
          aVerbMU aid verbD
        _ ->  return ()

  _ -> return ()

strikeA :: MonadClientUI m
        => ActorId -> ActorId -> Item -> HitAtomic -> m ()
strikeA source target item b = do
  Kind.COps{coactor, coitem=coitem@Kind.Ops{okind}} <- getsState scops
  disco <- getsState sdisco
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  let (verb, withWhat) | bproj sb = ("hit", False)
                       | otherwise =
        case jkind disco item of
          Nothing -> ("hit", False)  -- not identified
          Just ik -> let kind = okind ik
                     in ( iverbApply kind
                        , isNothing $ lookup "hth" $ ifreq kind )
      msg MissBlockA =
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
  per <- askPerception
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
      anim HitA = twirlSplash ps Color.BrRed Color.Red
      anim HitBlockA = blockHit ps Color.BrRed Color.Red
      anim MissBlockA = blockMiss ps
      animFrs = animate cli loc per (anim b)
  displayFramesPush $ Nothing : animFrs
  -- Animate only when the client's own actor dies.
  when (bfaction tb == side && not (bproj tb)) $ displayFramesPush animDie

-- * cmdUpdateCli

remCli :: MonadAction m => ES.EnumSet Point -> Level -> LevelId -> m ()
remCli vis lvl arena = do
  cops <- getsState scops
  actorD <- getsState sactorD
  let updArena dng =
        let clvl = fromMaybe (assert `failure` arena) $ EM.lookup arena dng
            nlvl = rememberLevel cops actorD vis lvl clvl
        in EM.insert arena nlvl dng
  modifyState $ updateDungeon updArena

rememberCli :: (MonadAction m, MonadClient m)
            => Level -> LevelId -> ActorDict -> ItemDict -> FactionDict -> m ()
rememberCli lvl lid actorD itemD faction = do
  per <- askPerception
  -- TODO: instead gather info about factions when first encountered
  -- and update when they are killed
  modifyState $ updateFaction (const faction)
  modifyState $ updateActorD (const actorD)
  -- TODO: only add new visible items
  modifyState $ updateItemD (const itemD)
  remCli (totalVisible per) lvl lid

rememberPerCli :: (MonadAction m, MonadClient m)
               => Perception -> Level -> LevelId
               -> ActorDict -> ItemDict -> FactionDict
               -> m ()
rememberPerCli per lvl lid actorD itemD faction = do
  modifyClient $ \cli -> cli {sper = EM.insert lid per (sper cli)}
  rememberCli lvl lid actorD itemD faction

-- TODO: here or elsewhere re-read RNG seed from config file
restartCli :: (MonadAction m, MonadClient m) => FactionPers -> State -> m ()
restartCli sper loc = do
  shistory <- getsClient shistory
  sconfigUI <- getsClient sconfigUI
  side <- getsClient sside
  isAI <- getsClient sisAI
  let cli = defStateClient shistory sconfigUI side isAI
  putClient cli {sper}
  putState loc
  -- Save ASAP in case of crashes and disconnects.
  --TODO

-- * cmdQueryCli

handleAI :: MonadClient m => ActorId -> m CmdSer
handleAI actor = do
  body <- getsState $ getActorBody actor
  side <- getsClient sside
  assert (bfaction body == side `blame` (actor, bfaction body, side)) $ do
    Kind.COps{costrat=Kind.Ops{okind}} <- getsState scops
    leader <- getsClient sleader
    fact <- getsState $ (EM.! bfaction body) . sfaction
    let factionAI | Just actor /= leader = gAiMember fact
                  | otherwise = fromJust $ gAiLeader fact
        factionAbilities = sabilities (okind factionAI)
    stratTarget <- targetStrategy actor factionAbilities
    -- Choose a target from those proposed by AI for the actor.
    btarget <- rndToAction $ frequency $ bestVariant stratTarget
    modifyClient $ updateTarget actor (const btarget)
    stratAction <- actionStrategy actor factionAbilities
    let _debug = T.unpack
          $ "HandleAI abilities:" <+> showT factionAbilities
          <>          ", symbol:" <+> showT (bsymbol body)
          <>          ", loc:"    <+> showT (bpos body)
          <> "\nHandleAI target:" <+> showT stratTarget
          <> "\nHandleAI move:"   <+> showT stratAction
    -- trace _debug $ return ()
    -- Run the AI: chose an action from those given by the AI strategy.
    rndToAction $ frequency $ bestVariant $ stratAction

-- * cmdQueryUI

-- | Handle the move of the hero.
handleHuman :: MonadClientUI m => m [CmdSer]
handleHuman = do
  -- When running, stop if aborted by a disturbance. Otherwise let
  -- the human player issue commands, until any of them takes time.
  -- First time, just after pushing frames, ask for commands in Push mode.
  Just leader <- getsClient sleader
  let inputHumanCmd msg = do
        stopRunning
        humanCommand msg
  cmdS <- tryWith inputHumanCmd $ do
    srunning <- getsClient srunning
    maybe abort (continueRun leader) srunning
--  addSmell leader  -- TODO: instead do for all non-spawning factions
  Just leaderNew <- getsClient sleader
  if leaderNew == leader then
    return [cmdS]
  else do
    fid <- getsClient sside
    return [LeaderSer fid leaderNew, cmdS]

-- | Continue running in the given direction.
continueRun :: MonadClient m => ActorId -> (Vector, Int) -> m CmdSer
continueRun leader dd = do
  (dir, distNew) <- continueRunDir leader dd
  modifyClient $ \cli -> cli {srunning = Just (dir, distNew)}
  -- Attacks and opening doors disallowed when continuing to run.
  return $ RunSer leader dir

-- | Determine and process the next human player command. The argument is
-- the last abort message due to running, if any.
humanCommand :: forall m. MonadClientUI m
             => Msg
             -> m CmdSer
humanCommand msgRunAbort = do
  -- The frame state is now Push.
  kmPush <- case msgRunAbort of
    "" -> getKeyCommand (Just True)
    _  -> do
      slides <- promptToSlideshow msgRunAbort
      getKeyOverlayCommand $ head $ runSlideshow slides
  -- The frame state is now None and remains so between each pair
  -- of lines of @loop@ (but can change within called actions).
  let loop :: K.KM -> m CmdSer
      loop km = do
        -- Messages shown, so update history and reset current report.
        recordHistory
        -- On abort, just reset state and call loop again below.
        -- Each abort that gets this far generates a slide to be shown.
        (mcmdS, slides) <- runWriterT $ tryWithSlide (return Nothing) $ do
          -- Look up the key.
          Binding{kcmd} <- askBinding
          case M.lookup km kcmd of
            Just (_, _, cmd) -> do
              -- Query and clear the last command key.
              lastKey <- getsClient slastKey
              -- TODO: perhaps replace slastKey
              -- with test 'kmNext == km'
              -- or an extra arg to 'loop'.
              -- Depends on whether slastKey
              -- is needed in other parts of code.
              modifyClient (\st -> st {slastKey = Just km})
              if (Just km == lastKey)
                then cmdHumanSem Clear
                else cmdHumanSem cmd
            Nothing -> let msgKey = "unknown command <" <> K.showKM km <> ">"
                       in abortWith msgKey
        -- The command was aborted or successful and if the latter,
        -- possibly took some time.
        case mcmdS of
          Just cmdS -> assert (null (runSlideshow slides) `blame` slides) $ do
            -- Exit the loop and let other actors act. No next key needed
            -- and no slides could have been generated.
            modifyClient (\st -> st {slastKey = Nothing})
            return cmdS
          Nothing ->
            -- If no time taken, rinse and repeat.
            -- Analyse the obtained slides.
            case reverse (runSlideshow slides) of
              [] -> do
                -- Nothing special to be shown; by default draw current state.
                modifyClient (\st -> st {slastKey = Nothing})
                sli <- promptToSlideshow ""
                kmNext <- getKeyOverlayCommand $ head $ runSlideshow sli
                loop kmNext
              sLast : sls -> do
                -- Show, one by one, all but the last slide.
                -- Note: the code that generates the slides is responsible
                -- for inserting the @more@ prompt.
                b <- getManyConfirms [km] $ toSlideshow $ reverse sls
                -- Display the last slide while waiting for the next key,
                -- or display current state if slideshow interrupted.
                kmNext <- if b
                          then getKeyOverlayCommand sLast
                          else do
                            modifyClient (\st -> st {slastKey = Nothing})
                            sli <- promptToSlideshow ""
                            getKeyOverlayCommand $ head $ runSlideshow sli
                -- Look up and perform the next command.
                loop kmNext
  loop kmPush
