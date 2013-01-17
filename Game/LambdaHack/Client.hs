{-# LANGUAGE DeriveDataTypeable, GADTs, OverloadedStrings, StandaloneDeriving
             #-}
-- | Semantics of client commands.
module Game.LambdaHack.Client
  ( cmdUpdateCli, cmdQueryCli
  ) where

import Control.Monad
import Control.Monad.Writer.Strict (WriterT, runWriterT)
import qualified Data.IntSet as IS
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (mempty)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.Animation
import Game.LambdaHack.Client.Binding
import Game.LambdaHack.Client.CmdPlayer
import Game.LambdaHack.Client.CmdPlayerAction
import Game.LambdaHack.Client.Draw
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.LocalAction
import Game.LambdaHack.Client.RunAction
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.Strategy
import Game.LambdaHack.Client.StrategyAction
import Game.LambdaHack.CmdCli
import Game.LambdaHack.CmdSer
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Random
import Game.LambdaHack.State
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

cmdUpdateCli :: MonadClient m => CmdUpdateCli -> m ()
cmdUpdateCli cmd = case cmd of
  PickupCli aid i ni -> pickupCli aid i ni
  ApplyCli actor verb item -> do
    Kind.COps{coactor, coitem} <- getsState scops
    disco <- getsState sdisco
    body <- getsState (getActorBody actor)
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
    cops <- getsState scops
    let updArena loc =
          let clvl = sdungeon loc M.! arena
              nlvl = rememberLevel cops vis lvl clvl
          in updateDungeon (M.insert arena nlvl) loc
    modifyState updArena
  RememberPerCli arena per lvl faction -> do
    -- TODO: remove if clients are guaranteed to be on good arena:
    arenaOld <- getsState sarena
    when (arenaOld /= arena) $ do
      modifyClient $ invalidateSelectedLeader
      modifyState $ updateSelectedArena arena
    void $ cmdUpdateCli $ RememberCli arena (totalVisible per) lvl
    modifyClient $ \cli -> cli {sper = M.insert arena per (sper cli)}
    modifyState $ updateFaction (const faction)
  SwitchLevelCli aid arena pbody items -> do
    arenaOld <- getsState sarena
    assert (arenaOld /= arena) $ do
      modifyClient $ invalidateSelectedLeader
      modifyState $ updateSelectedArena arena
      modifyState (insertActor aid pbody)
      modifyState (updateActorItem aid (const items))
      loc <- getState
      modifyClient $ updateSelectedLeader aid loc
  EffectCli msg poss deltaHP block -> do
    msgAdd msg
    cli <- getClient
    loc <- getState
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
    Kind.COps{coactor, coitem} <- getsState scops
    per <- askPerception
    disco <- getsState sdisco
    sm <- getsState (getActorBody source)
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
    Kind.COps{ coactor, coitem } <- getsState scops
    per <- askPerception
    disco <- getsState sdisco
    smRaw <- getsState (getActorBody source)
    tmRaw <- getsState (getActorBody target)
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
    Kind.COps{coactor} <- getsState scops
    per <- askPerception
    smRaw <- getsState (getActorBody source)
    tmRaw <- getsState (getActorBody target)
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
    loc <- getState
    let poss = (tpos, spos)
        anim = blockMiss poss
        animFrs = animate cli loc per anim
    displayFramesPush $ Nothing : animFrs
  DisplaceCli source target -> do
    Kind.COps{coactor} <- getsState scops
    per <- askPerception
    sm <- getsState (getActorBody source)
    tm <- getsState (getActorBody target)
    let spos = bpos sm
        tpos = bpos tm
        msg = makeSentence
          [ MU.SubjectVerbSg (partActor coactor sm) "displace"
          , partActor coactor tm ]
    msgAdd msg
    cli <- getClient
    loc <- getState
    let poss = (tpos, spos)
        animFrs = animate cli loc per $ swapPlaces poss
    displayFramesPush $ Nothing : animFrs
  DisplayPushCli -> displayPush
  DisplayDelayCli -> displayFramesPush [Nothing]
  MoreBWCli msg -> do
    void $ displayMore ColorBW msg
    recordHistory  -- Prevent repeating the ending msgs.
  MoreFullCli msg -> do
    void $ displayMore ColorFull msg
    recordHistory  -- Prevent repeating the ending msgs.
  RestartCli sper loc -> do
    shistory <- getsClient shistory
    let cli = defStateClient shistory sper
    putClient cli
    putState loc
  GameSaveCli toBkp -> clientGameSave toBkp

cmdQueryCli :: MonadClient m => CmdQueryCli a -> m a
cmdQueryCli cmd = case cmd of
  ShowSlidesCli slides -> getManyConfirms [] slides
  CarryOnCli -> carryOnCli
  ConfirmShowItemsCli discoS msg items -> do
    io <- itemOverlay discoS True items
    slides <- overlayToSlideshow msg io
    getManyConfirms [] slides
  SelectLeaderCli aid lid -> selectLeader aid lid
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
    arenaOld <- getsState sarena
    leaderOld <- getsClient getLeader
    -- Old leader may have been killed by enemies since @side@ last moved
    -- or local arena changed and the side has not elected a new leader yet
    -- or global arena changed the old leader is on the old arena.
    leader <- if arenaOld /= arena
              then do
                modifyClient invalidateSelectedLeader
                modifyState $ updateSelectedArena arena
                return actor
              else return $! fromMaybe actor leaderOld
    loc <- getState
    modifyClient $ updateSelectedLeader leader loc
    return leader
  HandlePlayerCli leader -> handlePlayer leader
  HandleAI actor -> do
    stratTarget <- targetStrategy actor
    -- Choose a target from those proposed by AI for the actor.
    btarget <- rndToAction $ frequency $ bestVariant stratTarget
    modifyClient $ updateTarget actor (const btarget)
    stratAction <- actionStrategy actor
    -- Run the AI: chose an action from those given by the AI strategy.
    rndToAction $ frequency $ bestVariant $ stratAction

-- | Continue running in the given direction.
continueRun :: MonadClient m => ActorId -> (Vector, Int) -> m CmdSer
continueRun leader dd = do
  dir <- continueRunDir leader dd
  -- Attacks and opening doors disallowed when continuing to run.
  return $ RunSer leader dir

-- | Handle the move of the hero.
handlePlayer :: MonadClient m
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
  arenaNew <- getsState sarena
  return (cmdS, leaderNew, arenaNew)

-- | Determine and process the next player command. The argument is the last
-- abort message due to running, if any.
playerCommand :: forall m. MonadClient m
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
  Kind.COps{coactor, coitem} <- getsState scops
  body <- getsState (getActorBody aid)
  side <- getsState sside
  disco <- getsState sdisco
  if bfaction body == side
    then msgAdd $ makePhrase [ letterLabel (jletter ni)
                             , partItemNWs coitem disco ni ]
    else msgAdd $ makeSentence
           [ MU.SubjectVerbSg (partActor coactor body) "pick up"
           , partItemNWs coitem disco i ]  -- single, not 'ni'

animateDeathCli :: MonadClient m => ActorId -> m ()
animateDeathCli target = do
  Kind.COps{coactor} <- getsState scops
  pbody <- getsState $ getActorBody target
  msgAdd $ makeSentence [MU.SubjectVerbSg (partActor coactor pbody) "die"]
  recordHistory  -- Prevent repeating the "die" msgs.
  cli <- getClient
  loc <- getState
  per <- askPerception
  let animFrs = animate cli loc per $ deathBody (bpos pbody)
  displayFramesPush animFrs

carryOnCli :: MonadClient m => m Bool
carryOnCli = do
  go <- displayMore ColorBW ""
  msgAdd "The survivors carry on."  -- TODO: reset messages at game over not to display it if there are no survivors.
  return go

-- | Make the item known to the player.
discoverCli :: MonadClient m => Kind.Id ItemKind -> Item -> m ()
discoverCli ik i = do
  Kind.COps{coitem} <- getsState scops
  oldDisco <- getsState sdisco
  let ix = jkindIx i
  unless (ix `M.member` oldDisco) $ do
    modifyState (updateDisco (M.insert ix ik))
    disco <- getsState sdisco
    let (object1, object2) = partItem coitem oldDisco i
        msg = makeSentence
          [ "the", MU.SubjectVerbSg (MU.Phrase [object1, object2])
                                    "turn out to be"
          , partItemAW coitem disco i ]
    msgAdd msg

invalidateArenaCli :: MonadClient m => LevelId -> m Bool
invalidateArenaCli arena = do
  arenaOld <- getsState sarena
  if arenaOld == arena
    then return False
    else do
      modifyClient invalidateSelectedLeader
      modifyState $ updateSelectedArena arena
      return True
