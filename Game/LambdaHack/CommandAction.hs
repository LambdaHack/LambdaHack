{-# LANGUAGE GADTs, OverloadedStrings #-}
-- | Semantics of player commands.
module Game.LambdaHack.CommandAction
  ( cmdSemantics, timedCmd, cmdSer, cmdUpdateCli, cmdQueryCli
  ) where

import Control.Monad
import Control.Monad.Writer.Strict (WriterT, lift)
import Control.Monad.Writer.Strict (WriterT, runWriterT)
import qualified Data.IntSet as IS
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (mempty)
import Data.Text (Text)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Animation
import Game.LambdaHack.Binding
import Game.LambdaHack.ClientAction
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Command
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Draw
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Key as K
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.MixedAction
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Running
import Game.LambdaHack.ServerAction
import Game.LambdaHack.State
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

-- | The basic action for a command and whether it takes time.
cmdAction :: MonadClientChan m => StateClient -> State -> Cmd
          -> WriterT Slideshow m (Maybe CmdSer)
cmdAction cli s cmd =
  let tgtMode = stgtMode cli
      leader = fromJust $ getLeader cli
      arena = sarena s
      sm = getActorBody leader s
      ppos = bpos sm
      tgtLoc = targetToPos cli s
      Level{lxsize} =
        maybe (getArena s) ((sdungeon s M.!) . tgtLevelId) tgtMode
  in case cmd of
    Apply{..} -> lift $ fmap Just $ playerApplyGroupItem verb object syms
    Project{} | isNothing tgtLoc -> retarget >> return Nothing
    Project{..} -> lift $ fmap Just $ playerProjectGroupItem verb object syms
    TriggerDir{..} -> lift $ fmap Just $ playerTriggerDir feature verb
    TriggerTile{..} -> lift $ fmap Just $ playerTriggerTile feature
    Pickup -> lift $ fmap Just $ pickupItem
    Drop   -> lift $ fmap Just $ dropItem
    Wait   -> lift $ fmap Just $ waitBlock
    Move v | isJust tgtMode ->
      let dir = toDir lxsize v
      in moveCursor dir 1 >> return Nothing
    Move v ->
      let dir = toDir lxsize v
          tpos = ppos `shift` dir
          -- We always see actors from our own faction.
          tgt = posToActor tpos s
      in case tgt of
        Just target | bfaction (getActorBody target s) == sside s
                      && not (bproj (getActorBody target s)) ->
          -- Select adjacent actor by bumping into him. Takes no time.
          selectLeader target arena
            >>= assert `trueM`
                  (leader, target, "leader bumps into himself" :: Text)
            >> return Nothing
        _ -> lift $ fmap Just $ movePl dir
    Run v | isJust tgtMode ->
      let dir = toDir lxsize v
      in moveCursor dir 10 >> return Nothing
    Run v ->
      let dir = toDir lxsize v
      in lift $ fmap Just $ runPl dir

    GameExit    -> lift $ fmap Just $ gameExit
    GameRestart -> lift $ fmap Just $ gameRestart
    GameSave    -> lift $ fmap Just $ gameSave
    CfgDump     -> lift $ fmap Just $ dumpConfig
    Inventory   -> inventory >> return Nothing
    TgtFloor    -> (targetFloor   $ TgtExplicit arena) >> return Nothing
    TgtEnemy    -> (targetMonster $ TgtExplicit arena) >> return Nothing
    TgtAscend k -> tgtAscend k >> return Nothing
    EpsIncr b   -> lift $ epsIncr b >> return Nothing
    Cancel      -> cancelCurrent displayMainMenu >> return Nothing
    Accept      -> acceptCurrent displayHelp >> return Nothing
    Clear       -> lift $ clearCurrent >> return Nothing
    History     -> displayHistory >> return Nothing
    HeroCycle   -> lift $ cycleHero >> return Nothing
    HeroBack    -> lift $ backCycleHero >> return Nothing
    Help        -> displayHelp >> return Nothing
    SelectHero k -> lift $ selectHero k >> return Nothing
    DebugArea   -> modifyClient toggleMarkVision >> return Nothing
    DebugOmni   -> modifyClient toggleOmniscient >> return Nothing  -- TODO: Server
    DebugSmell  -> modifyClient toggleMarkSmell >> return Nothing
    DebugVision -> undefined {-modifyServer cycleTryFov-}

-- | The semantics of player commands in terms of the @Action@ monad.
-- Decides if the action takes time and what action to perform.
-- Time cosuming commands are marked as such in help and cannot be
-- invoked in targeting mode on a remote level (level different than
-- the level of the selected hero).
cmdSemantics :: MonadClientChan m => Cmd -> WriterT Slideshow m (Maybe CmdSer)
cmdSemantics cmd = do
  Just leaderOld <- getsClient getLeader
  arenaOld <- getsLocal sarena
  posOld <- getsLocal (bpos . getActorBody leaderOld)
  cli <- getClient
  loc <- getLocal
  let sem = cmdAction cli loc cmd
  mcmdS <- if noRemoteCmd cmd
           then checkCursor sem
           else sem
  arena <- getsLocal sarena
  leaderNew <- getsClient getLeader
  case leaderNew of
    Nothing -> return ()
    Just leader -> do
      pos <- getsLocal (bpos . getActorBody leader)
      tgtMode <- getsClient stgtMode
      when (isNothing tgtMode  -- targeting performs a more extensive look
            && (posOld /= pos
                || arenaOld /= arena)) $ do
        lookMsg <- lookAt False True pos ""
        msgAdd lookMsg
  return mcmdS

-- | If in targeting mode, check if the current level is the same
-- as player level and refuse performing the action otherwise.
checkCursor :: MonadClientRO m
            => WriterT Slideshow m (Maybe CmdSer)
            -> WriterT Slideshow m (Maybe CmdSer)
checkCursor h = do
  arena <- getsLocal sarena
  (lid, _) <- viewedLevel
  if arena == lid
    then h
    else abortWith "[targeting] command disabled on a remote level, press ESC to switch back"

timedCmd :: CmdSer -> Bool
timedCmd cmd = case cmd of
  GameExitSer -> False
  GameRestartSer -> False
  GameSaveSer -> False
  CfgDumpSer -> False
  _ -> True

-- TODO: make it MonadServer
-- | The semantics of server commands.
cmdSer :: MonadServerChan m => CmdSer -> m ()
cmdSer cmd = case cmd of
  ApplySer aid v item -> applySer aid v item
  ProjectSer aid p v i -> projectSer aid p v i
  TriggerSer aid p -> triggerSer aid p
  PickupSer aid i l -> pickupSer aid i l
  DropSer aid item -> dropSer aid item
  WaitSer aid -> waitSer aid
  MoveSer aid dir -> moveSer aid dir
  RunSer aid dir -> runSer aid dir
  GameExitSer -> gameExitSer
  GameRestartSer -> gameRestartSer
  GameSaveSer -> gameSaveSer
  CfgDumpSer -> cfgDumpSer

cmdUpdateCli :: MonadClient m => CmdUpdateCli -> m ()
cmdUpdateCli cmd = case cmd of
  PickupCli aid i ni -> pickupCli aid i ni
  ApplyCli actor verb item -> do
    Kind.COps{coactor, coitem} <- getsLocal scops
    disco <- getsLocal sdisco
    body <- getsLocal (getActorBody actor)
    let msg = makeSentence
          [ MU.SubjectVerbSg (partActor coactor body) verb
          , partItemNWs coitem disco item ]
    msgAdd msg
  ShowItemsCli discoS msg items -> do
    io <- itemOverlay discoS True items
    slides <- overlayToSlideshow msg io
    void $ getManyConfirms [] slides
  ShowMsgCli msg ->
    msgAdd msg
  AnimateDeathCli aid -> animateDeathCli aid
  InvalidateArenaCli lid -> void $ invalidateArenaCli lid
  DiscoverCli ik i -> discoverCli ik i
  RememberCli arena vis lvl -> do
    cops <- getsLocal scops
    let updArena loc =
          let clvl = sdungeon loc M.! arena
              nlvl = rememberLevel cops vis lvl clvl
          in updateDungeon (M.insert arena nlvl) loc
    modifyLocal updArena
  RememberPerCli arena per lvl faction -> do
    void $ cmdUpdateCli $ RememberCli arena (totalVisible per) lvl
    modifyClient $ \cli -> cli {sper = M.insert arena per (sper cli)}
    modifyLocal $ updateFaction (const faction)
  SwitchLevelCli aid arena pbody items -> do
    arenaOld <- getsLocal sarena
    assert (arenaOld /= arena) $ do
      modifyClient $ invalidateSelectedLeader
      modifyLocal $ updateSelectedArena arena
      modifyLocal (insertActor aid pbody)
      modifyLocal (updateActorItem aid (const items))
      loc <- getLocal
      modifyClient $ updateSelectedLeader aid loc
  EffectCli msg poss deltaHP block -> do
    msgAdd msg
    cli <- getClient
    loc <- getLocal
    per <- askPerception
    -- Try to show an animation. Sometimes, e.g., when HP is unchaged,
    -- the animation will not be shown, but a single frame with @msg@ will.
    let anim | deltaHP > 0 =
          twirlSplash poss Color.BrBlue Color.Blue
             | deltaHP < 0 && block =
          blockHit    poss Color.BrRed  Color.Red
             | deltaHP < 0 && not block =
          twirlSplash poss Color.BrRed  Color.Red
             | otherwise = mempty
        animFrs = animate cli loc per anim
    displayFramesPush $ Nothing : animFrs
  ProjectCli spos source consumed -> do
    Kind.COps{coactor, coitem} <- getsLocal scops
    per <- askPerception
    disco <- getsLocal sdisco
    sm <- getsLocal (getActorBody source)
    let svisible = spos `IS.member` totalVisible per
        subject =
          if svisible
          then sm
          else sm {bname = Just "somebody"}
        msg = makeSentence
              [ MU.SubjectVerbSg (partActor coactor subject) "aim"
              , partItemNWs coitem disco consumed ]
    msgAdd msg
  ShowAttackCli source target verb stack say -> do
    Kind.COps{ coactor, coitem } <- getsLocal scops
    per <- askPerception
    disco <- getsLocal sdisco
    smRaw <- getsLocal (getActorBody source)
    tmRaw <- getsLocal (getActorBody target)
    let spos = bpos smRaw
        tpos = bpos tmRaw
        svisible = spos `IS.member` totalVisible per
        tvisible = tpos `IS.member` totalVisible per
        sm | svisible  = smRaw
           | otherwise = smRaw {bname = Just "somebody"}
        tm | tvisible  = tmRaw
           | otherwise = tmRaw {bname = Just "somebody"}
    -- The msg describes the source part of the action.
    -- TODO: right now it also describes the victim and weapon;
    -- perhaps, when a weapon is equipped, just say "you hit"
    -- or "you miss" and then "nose dies" or "nose yells in pain".
    let msg = makeSentence $
          [ MU.SubjectVerbSg (partActor coactor sm) verb
          , partActor coactor tm ]
          ++ if say
             then ["with", partItemAW coitem disco stack]
             else []
    msgAdd msg
  AnimateBlockCli source target verb -> do
    Kind.COps{coactor} <- getsLocal scops
    per <- askPerception
    smRaw <- getsLocal (getActorBody source)
    tmRaw <- getsLocal (getActorBody target)
    let spos = bpos smRaw
        tpos = bpos tmRaw
        svisible = spos `IS.member` totalVisible per
        tvisible = tpos `IS.member` totalVisible per
        sm | svisible  = smRaw
           | otherwise = smRaw {bname = Just "somebody"}
        tm | tvisible  = tmRaw
           | otherwise = tmRaw {bname = Just "somebody"}
        msgMiss = makeSentence
          [ MU.SubjectVerbSg (partActor coactor sm) "try to"
          , verb MU.:> ", but"
          , MU.SubjectVerbSg (partActor coactor tm) "block"
          ]
    msgAdd msgMiss
    cli <- getClient
    loc <- getLocal
    let poss = (tpos, spos)
        anim = blockMiss poss
        animFrs = animate cli loc per anim
    displayFramesPush $ Nothing : animFrs
  DisplaceCli source target -> do
    Kind.COps{coactor} <- getsLocal scops
    per <- askPerception
    sm <- getsLocal (getActorBody source)
    tm <- getsLocal (getActorBody target)
    let spos = bpos sm
        tpos = bpos tm
        msg = makeSentence
          [ MU.SubjectVerbSg (partActor coactor sm) "displace"
          , partActor coactor tm ]
    msgAdd msg
    cli <- getClient
    loc <- getLocal
    let poss = (tpos, spos)
        animFrs = animate cli loc per $ swapPlaces poss
    displayFramesPush $ Nothing : animFrs
  DisplayPushCli -> displayPush
  DisplayFramesPushCli frames -> displayFramesPush frames
  MoreBWCli msg -> do
    void $ displayMore ColorBW msg
    recordHistory  -- Prevent repeating the ending msgs.
  MoreFullCli msg -> do
    void $ displayMore ColorFull msg
    recordHistory  -- Prevent repeating the ending msgs.
  RestartCli cli loc -> do
    shistory <- getsClient shistory
    putClient cli {shistory}
    putLocal loc

cmdQueryCli :: MonadClientChan m => CmdQueryCli a -> m a
cmdQueryCli cmd = case cmd of
  ShowSlidesCli slides -> do
    go <- getManyConfirms [] slides
    return go
  CarryOnCli -> carryOnCli
  ConfirmShowItemsCli discoS msg items -> do
    io <- itemOverlay discoS True items
    slides <- overlayToSlideshow msg io
    go <- getManyConfirms [] slides
    return go
  SelectLeaderCli aid lid -> do
    go <- selectLeader aid lid
    return go
  ConfirmYesNoCli msg -> do
    go <- displayYesNo msg
    recordHistory  -- Prevent repeating the ending msgs.
    return go
  ConfirmMoreBWCli msg -> do
    go <- displayMore ColorBW msg
    recordHistory  -- Prevent repeating the ending msgs.
    return go
  ConfirmMoreFullCli msg -> do
    go <- displayMore ColorFull msg
    recordHistory  -- Prevent repeating the ending msgs.
    return go
  NullReportCli -> do
    StateClient{sreport} <- getClient
    return $! nullReport sreport
  SetArenaLeaderCli arena actor -> do
    arenaOld <- getsLocal sarena
    leaderOld <- getsClient getLeader
    -- Old leader may have been killed by enemies since @side@ last moved
    -- or local arena changed and the side has not elected a new leader yet
    -- or global arena changed the old leader is on the old arena.
    leader <- if arenaOld /= arena
              then do
                modifyClient invalidateSelectedLeader
                modifyLocal $ updateSelectedArena arena
                return actor
              else return $! fromMaybe actor leaderOld
    loc <- getLocal
    modifyClient $ updateSelectedLeader leader loc
    return leader
  GameSaveCli -> do
    cli <- getClient
    loc <- getLocal
    return (cli, loc)
  HandlePlayerCli leader -> handlePlayer leader

-- | Continue running in the given direction.
continueRun :: MonadClientChan m => ActorId -> (Vector, Int) -> m CmdSer
continueRun leader dd = do
  dir <- continueRunDir leader dd
  -- Attacks and opening doors disallowed when continuing to run.
  return $ RunSer leader dir

-- | Handle the move of the hero.
handlePlayer :: MonadClientChan m
             => ActorId
             -> m (CmdSer, Maybe ActorId, LevelId)
handlePlayer leader = do
  -- When running, stop if aborted by a disturbance.
  -- Otherwise let the player issue commands, until any of them takes time.
  -- First time, just after pushing frames, ask for commands in Push mode.
  cmdS <- tryWith (\msg -> stopRunning >> playerCommand msg) $ do
    srunning <- getsClient srunning
    maybe abort (continueRun leader) srunning
--  addSmell leader
  leaderNew <- getsClient getLeader
  arenaNew <- getsLocal sarena
  return (cmdS, leaderNew, arenaNew)

-- | Determine and process the next player command. The argument is the last
-- abort message due to running, if any.
playerCommand :: forall m. MonadClientChan m
              => Msg
              -> m CmdSer
playerCommand msgRunAbort = do
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
                then cmdSemantics Clear
                else cmdSemantics cmd
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

pickupCli :: MonadClient m => ActorId -> Item -> Item -> m ()
pickupCli aid i ni = do
  Kind.COps{coactor, coitem} <- getsLocal scops
  body <- getsLocal (getActorBody aid)
  side <- getsLocal sside
  disco <- getsLocal sdisco
  if bfaction body == side
    then msgAdd $ makePhrase [ letterLabel (jletter ni)
                             , partItemNWs coitem disco ni ]
    else msgAdd $ makeSentence
           [ MU.SubjectVerbSg (partActor coactor body) "pick up"
           , partItemNWs coitem disco i ]  -- single, not 'ni'

animateDeathCli :: MonadClient m => ActorId -> m ()
animateDeathCli target = do
  Kind.COps{coactor} <- getsLocal scops
  pbody <- getsLocal $ getActorBody target
  msgAdd $ makeSentence [MU.SubjectVerbSg (partActor coactor pbody) "die"]
  recordHistory  -- Prevent repeating the "die" msgs.
  cli <- getClient
  loc <- getLocal
  per <- askPerception
  let animFrs = animate cli loc per $ deathBody (bpos pbody)
  displayFramesPush animFrs

carryOnCli :: MonadClientChan m => m Bool
carryOnCli = do
  go <- displayMore ColorBW ""
  msgAdd "The survivors carry on."  -- TODO: reset messages at game over not to display it if there are no survivors.
  return go

-- | Make the item known to the player.
discoverCli :: MonadClient m => Kind.Id ItemKind -> Item -> m ()
discoverCli ik i = do
  Kind.COps{coitem} <- getsLocal scops
  oldDisco <- getsLocal sdisco
  let ix = jkindIx i
  unless (ix `M.member` oldDisco) $ do
    modifyLocal (updateDisco (M.insert ix ik))
    disco <- getsLocal sdisco
    let (object1, object2) = partItem coitem oldDisco i
        msg = makeSentence
          [ "the", MU.SubjectVerbSg (MU.Phrase [object1, object2])
                                    "turn out to be"
          , partItemAW coitem disco i ]
    msgAdd msg

invalidateArenaCli :: MonadClient m => LevelId -> m Bool
invalidateArenaCli arena = do
  arenaOld <- getsLocal sarena
  if arenaOld == arena
    then return False
    else do
      modifyClient invalidateSelectedLeader
      modifyLocal $ updateSelectedArena arena
      return True
