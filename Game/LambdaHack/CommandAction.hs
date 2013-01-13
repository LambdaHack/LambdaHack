{-# LANGUAGE OverloadedStrings #-}
-- | Semantics of player commands.
module Game.LambdaHack.CommandAction
  ( cmdSemantics, cmdSer, cmdCli
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Writer.Strict (WriterT, lift)
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
import Game.LambdaHack.ClientAction
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Command
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Draw
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.MixedAction
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.ServerAction
import Game.LambdaHack.State
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

-- | The basic action for a command and whether it takes time.
cmdAction :: MonadClient m => StateClient -> State -> Cmd
          -> (Bool, WriterT Slideshow m ())
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
    Apply{..} -> (True, cmdSerAction $ playerApplyGroupItem verb object syms)
    Project{} | isNothing tgtLoc -> (False, retarget)
    Project{..} ->
      (True, cmdSerAction $ playerProjectGroupItem verb object syms)
    TriggerDir{..} -> (True, cmdSerAction $ playerTriggerDir feature verb)
    TriggerTile{..} -> (True, cmdSerAction $ playerTriggerTile feature)
    Pickup -> (True, cmdSerAction $ pickupItem)
    Drop   -> (True, cmdSerAction $ dropItem)
    Wait   -> (True, cmdSerAction $ waitBlock)
    Move v | isJust tgtMode ->
      let dir = toDir lxsize v
      in (False, moveCursor dir 1)
    Move v ->
      let dir = toDir lxsize v
          tpos = ppos `shift` dir
          -- We always see actors from our own faction.
          tgt = posToActor tpos s
      in case tgt of
        Just target | bfaction (getActorBody target s) == sside s
                      && not (bproj (getActorBody target s)) ->
          -- Select adjacent actor by bumping into him. Takes no time.
          (False,
           selectLeader target arena
             >>= assert `trueM`
                   (leader, target, "leader bumps into himself" :: Text))
        _ -> (True, cmdSerAction $ movePl dir)
    Run v | isJust tgtMode ->
      let dir = toDir lxsize v
      in (False, moveCursor dir 10)
    Run v ->
      let dir = toDir lxsize v
      in (True, cmdSerAction $ runPl dir)
    GameExit    -> (True, cmdSerAction $ gameExit)     -- rewinds time
    GameRestart -> (True, cmdSerAction $ gameRestart)  -- resets state

    GameSave    -> (False, cmdSerAction $ gameSave)
    Inventory   -> (False, inventory)
    TgtFloor    -> (False, targetFloor   $ TgtExplicit arena)
    TgtEnemy    -> (False, targetMonster $ TgtExplicit arena)
    TgtAscend k -> (False, tgtAscend k)
    EpsIncr b   -> (False, lift $ epsIncr b)
    Cancel      -> (False, cancelCurrent displayMainMenu)
    Accept      -> (False, acceptCurrent displayHelp)
    Clear       -> (False, lift $ clearCurrent)
    History     -> (False, displayHistory)
    CfgDump     -> (False, cmdSerAction $ dumpConfig)
    HeroCycle   -> (False, lift $ cycleHero)
    HeroBack    -> (False, lift $ backCycleHero)
    Help        -> (False, displayHelp)
    SelectHero k -> (False, lift $ selectHero k)
    DebugArea   -> (False, modifyClient toggleMarkVision)
    DebugOmni   -> (False, modifyClient toggleOmniscient)  -- TODO: Server
    DebugSmell  -> (False, modifyClient toggleMarkSmell)
    DebugVision -> (False, undefined {-modifyServer cycleTryFov-})

-- | The semantics of player commands in terms of the @Action@ monad.
-- Decides if the action takes time and what action to perform.
-- Time cosuming commands are marked as such in help and cannot be
-- invoked in targeting mode on a remote level (level different than
-- the level of the selected hero).
cmdSemantics :: MonadClient m => Cmd -> WriterT Slideshow m (Bool, LevelId)
cmdSemantics cmd = do
  Just leaderOld <- getsClient getLeader
  arenaOld <- getsLocal sarena
  posOld <- getsLocal (bpos . getActorBody leaderOld)
  cli <- getClient
  loc <- getLocal
  let (timed, sem) = cmdAction cli loc cmd
  if timed
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
  return (timed, arena)

-- | If in targeting mode, check if the current level is the same
-- as player level and refuse performing the action otherwise.
checkCursor :: MonadClientRO m
            => WriterT Slideshow m ()
            -> WriterT Slideshow m ()
checkCursor h = do
  arena <- getsLocal sarena
  (lid, _) <- viewedLevel
  if arena == lid
    then h
    else abortWith "[targeting] command disabled on a remote level, press ESC to switch back"

-- TODO: make it MonadServer
-- | The semantics of server commands.
cmdSer :: MonadAction m => CmdSer -> m ()
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
  ResponseSer _ -> undefined

cmdSerAction :: MonadClient m => m CmdSer -> WriterT Slideshow m ()
cmdSerAction m = undefined -- lift $ m >>= cmdSer

-- | The semantics of client commands.
cmdCli :: MonadClient m => CmdCli -> m ()
cmdCli cmd = case cmd of
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
  ShowSlidesCli slides -> do
    go <- getManyConfirms [] slides
    ClientChan {toServer} <- getsClient schan
    liftIO $ writeChan toServer $ ResponseSer go
  AnimateDeathCli aid -> animateDeathCli aid
  CarryOnCli -> carryOnCli
  SelectLeaderCli aid lid -> do
    b <- selectLeader aid lid
    ClientChan {toServer} <- getsClient schan
    liftIO $ writeChan toServer $ ResponseSer b
  InvalidateArenaCli lid -> void $ invalidateArenaCli lid
  DiscoverCli ik i -> discoverCli ik i
  ConfirmYesNoCli msg -> do
    go <- displayYesNo msg
    recordHistory  -- Prevent repeating the ending msgs.
    ClientChan {toServer} <- getsClient schan
    liftIO $ writeChan toServer $ ResponseSer go
  ConfirmMoreBWCli msg -> do
    go <- displayMore ColorBW msg
    recordHistory  -- Prevent repeating the ending msgs.
    ClientChan {toServer} <- getsClient schan
    liftIO $ writeChan toServer $ ResponseSer go
  ConfirmMoreFullCli msg -> do
    go <- displayMore ColorFull msg
    recordHistory  -- Prevent repeating the ending msgs.
    ClientChan {toServer} <- getsClient schan
    liftIO $ writeChan toServer $ ResponseSer go
  RememberCli arena vis lvl -> do
    cops <- getsLocal scops
    let updArena loc =
          let clvl = sdungeon loc M.! arena
              nlvl = rememberLevel cops vis lvl clvl
          in updateDungeon (M.insert arena nlvl) loc
    modifyLocal updArena
  RememberPerCli arena per lvl faction -> do
    void $ cmdCli $ RememberCli arena (totalVisible per) lvl
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
  NullReportCli -> do
    StateClient{sreport} <- getClient
    ClientChan {toServer} <- getsClient schan
    liftIO $ writeChan toServer $ ResponseSer $ nullReport sreport


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

carryOnCli :: MonadClient m => m ()
carryOnCli = do
  go <- displayMore ColorBW ""
  msgAdd "The survivors carry on."  -- TODO: reset messages at game over not to display it if there are no survivors.
  ClientChan {toServer} <- getsClient schan
  liftIO $ writeChan toServer $ ResponseSer go

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
