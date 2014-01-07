-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
-- Does not export the @liftIO@ operation nor a few other implementation
-- details.
module Game.LambdaHack.Client.Action
  ( -- * Action monads
    MonadClient( getClient, getsClient, putClient, modifyClient, saveClient )
  , MonadClientUI
  , MonadClientReadServer(..), MonadClientWriteServer(..)
  , SessionUI(..), ConnFrontend(..), connFrontend
    -- * Executing actions
  , mkConfigUI
    -- * Accessors to the game session Reader and the Perception Reader(-like)
  , askBinding, getPerFid
    -- * History and report
  , msgAdd, msgReset, recordHistory
    -- * Key input
  , getKeyOverlayCommand, getInitConfirms, stopPlayBack
    -- * Display and key input
  , displayFrames, displayMore, displayYesNo, displayChoiceUI
    -- * Generate slideshows
  , promptToSlideshow, overlayToSlideshow
    -- * Draw frames
  , drawOverlay, animate
    -- * Assorted primitives
  , restoreGame, removeServerSave, displayPush, scoreToSlideshow
  , rndToAction, getArenaUI, getLeaderUI
  , targetToPos, partAidLeader, partActorLeader
  , debugPrint
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.EnumMap.Strict as EM
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Monoid as Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU
import System.Directory
import System.FilePath
import System.Time

import Game.LambdaHack.Client.Action.ActionClass
import Game.LambdaHack.Client.Binding
import Game.LambdaHack.Client.Config
import Game.LambdaHack.Client.Draw
import Game.LambdaHack.Client.HumanCmd
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Animation
import qualified Game.LambdaHack.Common.ConfigIO as ConfigIO
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.HighScore as HighScore
import qualified Game.LambdaHack.Common.Key as K
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import qualified Game.LambdaHack.Common.Save as Save
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind
import qualified Game.LambdaHack.Frontend as Frontend

debugPrint :: MonadClient m => Text -> m ()
debugPrint t = do
  sdbgMsgCli <- getsClient $ sdbgMsgCli . sdebugCli
  when sdbgMsgCli $ liftIO $ Save.delayPrint t

connFrontend :: FactionId -> Frontend.ChanFrontend -> ConnFrontend
connFrontend fid fromF = ConnFrontend
  { readConnFrontend =
      liftIO $ atomically $ readTQueue fromF
  , writeConnFrontend = \efr -> do
      let toF = Frontend.toMulti Frontend.connMulti
      liftIO $ atomically $ writeTQueue toF (fid, efr)
  }

displayFrame :: MonadClientUI m => Bool -> Maybe SingleFrame -> m ()
displayFrame isRunning mf = do
  ConnFrontend{writeConnFrontend} <- getsSession sfconn
  let frame = case mf of
        Nothing -> AcDelay
        Just fr | isRunning -> AcRunning fr
        Just fr -> AcNormal fr
  writeConnFrontend $ Frontend.FrontFrame frame

promptGetKey :: MonadClientUI m => [K.KM] -> SingleFrame -> m K.KM
promptGetKey frontKM frontFr = do
  lastSeq <- getsClient slastSeq
  case lastSeq of
    LPlayBack (km : kms) macro n | null frontKM || km `elem` frontKM -> do
      displayFrame False $ Just frontFr
      let slastSeq = LPlayBack kms macro n
      modifyClient $ \cli -> cli {slastSeq}
      return km
    LPlayBack{} -> do  -- something went wrong, stop repeating
      displayFrame False $ Just frontFr
      stopPlayBack
      return K.escKey
    LRecord seqCurrent seqPrevious k -> do
      ConnFrontend{..} <- getsSession sfconn
      writeConnFrontend Frontend.FrontKey {..}
      km <- readConnFrontend
      let slastSeq = LRecord (km : seqCurrent) seqPrevious k
      modifyClient $ \cli -> cli {slastSeq}
      return km

stopPlayBack :: MonadClient m => m ()
stopPlayBack = do
  lastSeq <- getsClient slastSeq
  case lastSeq of
    LPlayBack _ macro _ -> do
      let slastSeq = LRecord macro [] 0
      modifyClient $ \cli -> cli {slastSeq}
    LRecord{} -> return ()

-- | Display a slideshow, awaiting confirmation for each slide except the last.
getInitConfirms :: MonadClientUI m
                => ColorMode -> [K.KM] -> Slideshow -> m Bool
getInitConfirms dm frontClear slides = do
  ConnFrontend{..} <- getsSession sfconn
  frontSlides <- mapM (drawOverlay dm) $ slideshow slides
  -- The first two cases are optimizations:
  case frontSlides of
    [] -> return True
    [x] -> do
      displayFrame False $ Just x
      return True
    _ -> do
      writeConnFrontend Frontend.FrontSlides{..}
      km <- readConnFrontend
      return $! km /= K.escKey

getLeaderUI :: MonadClientUI m => m ActorId
getLeaderUI = do
  cli <- getClient
  case _sleader cli of
    Nothing -> assert `failure` "leader expected but not found" `twith` cli
    Just leader -> return leader

getArenaUI :: MonadClientUI m => m LevelId
getArenaUI = do
  mleader <- getsClient _sleader
  case mleader of
    Just leader -> getsState $ blid . getActorBody leader
    Nothing -> do
      side <- getsClient sside
      factionD <- getsState sfactionD
      let fact = factionD EM.! side
      case gquit fact of
        Just Status{stDepth} -> return $ toEnum stDepth
        Nothing -> do
          dungeon <- getsState sdungeon
          let (minD, maxD) =
                case (EM.minViewWithKey dungeon, EM.maxViewWithKey dungeon) of
                  (Just ((s, _), _), Just ((e, _), _)) -> (s, e)
                  _ -> assert `failure` "empty dungeon" `twith` dungeon
          return $ max minD $ min maxD $ playerEntry $ gplayer fact

-- | Calculate the position of leader's target.
targetToPos :: MonadClientUI m => m (Maybe Point)
targetToPos = do
  mleader <- getsClient _sleader
  case mleader of
    Nothing -> return Nothing
    Just leader -> do
      scursor <- getsClient scursor
      lid <- getsState $ blid . getActorBody leader
      target <- getsClient $ getTarget leader
      case target of
        Just (TPos pos) -> return $ Just pos
        Just (TEnemy a _ll) -> do
          mem <- getsState $ memActor a lid  -- alive and visible?
          if mem then do
            pos <- getsState $ bpos . getActorBody a
            return $ Just pos
          else return Nothing
        Nothing -> return scursor

-- | Get the key binding.
askBinding :: MonadClientUI m => m Binding
askBinding = getsSession sbinding

-- | Add a message to the current report.
msgAdd :: MonadClientUI m => Msg -> m ()
msgAdd msg = modifyClient $ \d -> d {sreport = addMsg (sreport d) msg}

-- | Wipe out and set a new value for the current report.
msgReset :: MonadClient m => Msg -> m ()
msgReset msg = modifyClient $ \d -> d {sreport = singletonReport msg}

-- | Store current report in the history and reset report.
recordHistory :: MonadClient m => m ()
recordHistory = do
  StateClient{sreport, shistory} <- getClient
  unless (nullReport sreport) $ do
    ConfigUI{configHistoryMax} <- getsClient sconfigUI
    msgReset ""
    let nhistory = takeHistory configHistoryMax $! addReport sreport shistory
    modifyClient $ \cli -> cli {shistory = nhistory}

-- | Get the current perception of a client.
getPerFid :: MonadClient m => LevelId -> m Perception
getPerFid lid = do
  fper <- getsClient sfper
  return $! fromMaybe (assert `failure` "no perception at given level"
                              `twith` (lid, fper))
                      $ EM.lookup lid fper

-- | Display an overlay and wait for a human player command.
getKeyOverlayCommand :: MonadClientUI m => Overlay -> m K.KM
getKeyOverlayCommand overlay = do
  frame <- drawOverlay ColorFull overlay
  keyb <- askBinding
  -- Give the previous client time to display his frames.
  liftIO $ threadDelay 1000
  km <- promptGetKey [] frame
  return $! fromMaybe km $ M.lookup km $ kmacro keyb

-- | Push frames or delays to the frame queue.
displayFrames :: MonadClientUI m => Frames -> m ()
displayFrames = mapM_ (displayFrame False)

-- | A yes-no confirmation.
getYesNo :: MonadClientUI m => SingleFrame -> m Bool
getYesNo frame = do
  let keys = [ K.KM {key=K.Char 'y', modifier=K.NoModifier}
             , K.KM {key=K.Char 'n', modifier=K.NoModifier}
             , K.escKey
             ]
  K.KM {key} <- promptGetKey keys frame
  case key of
    K.Char 'y' -> return True
    _          -> return False

-- | Display a msg with a @more@ prompt. Return value indicates if the player
-- tried to cancel/escape.
displayMore :: MonadClientUI m => ColorMode -> Msg -> m Bool
displayMore dm prompt = do
  slides <- promptToSlideshow $ prompt <+> moreMsg
  -- Two frames drawn total (unless 'prompt' very long).
  getInitConfirms dm [] $ slides Monoid.<> toSlideshow [[]]

-- | Print a yes/no question and return the player's answer. Use black
-- and white colours to turn player's attention to the choice.
displayYesNo :: MonadClientUI m => ColorMode -> Msg -> m Bool
displayYesNo dm prompt = do
  sli <- promptToSlideshow $ prompt <+> yesnoMsg
  frame <- drawOverlay dm $ head $ slideshow sli
  getYesNo frame

-- TODO: generalize getInitConfirms and displayChoiceUI to a single op
-- | Print a prompt and an overlay and wait for a player keypress.
-- If many overlays, scroll screenfuls with SPACE. Do not wrap screenfuls
-- (in some menus @?@ cycles views, so the user can restart from the top).
displayChoiceUI :: MonadClientUI m
                => Msg -> Overlay -> [K.KM] -> m (Either Slideshow K.KM)
displayChoiceUI prompt ov keys = do
  slides <- fmap slideshow $ overlayToSlideshow (prompt <> ", ESC]") ov
  let legalKeys =
        [ K.KM {key=K.Space, modifier=K.NoModifier}
        , K.escKey ]
        ++ keys
      loop [] = fmap Left $ promptToSlideshow "never mind"
      loop (x : xs) = do
        frame <- drawOverlay ColorFull x
        km@K.KM {..} <- promptGetKey legalKeys frame
        case key of
          K.Esc -> fmap Left $ promptToSlideshow "never mind"
          K.Space -> loop xs
          _ -> return $ Right km
  loop slides

-- | The prompt is shown after the current message, but not added to history.
-- This is useful, e.g., in targeting mode, not to spam history.
promptToSlideshow :: MonadClientUI m => Msg -> m Slideshow
promptToSlideshow prompt = overlayToSlideshow prompt emptyOverlay

-- | The prompt is shown after the current message at the top of each slide.
-- Together they may take more than one line. The prompt is not added
-- to history. The portions of overlay that fit on the the rest
-- of the screen are displayed below. As many slides as needed are shown.
overlayToSlideshow :: MonadClientUI m => Msg -> Overlay -> m Slideshow
overlayToSlideshow prompt overlay = do
  side <- getsClient sside
  lid <- getArenaUI
  Level{lxsize, lysize} <- getLevel lid  -- TODO: screen length or viewLevel
  sreport <- getsClient sreport
  ours <- getsState $ actorNotProjList (== side) lid
  let leftForMsg = lxsize - length ours - 1
      (firstLineSize, promptPadded)
        | overlay /= emptyOverlay = (lxsize, T.justifyLeft lxsize ' ' prompt)
        | leftForMsg < lxsize `div` 3 = (0, prompt)
        | otherwise = (leftForMsg, prompt)
      msg = splitReport firstLineSize lxsize (addMsg sreport promptPadded)
  return $! splitOverlay lysize msg overlay

-- | Draw the current level with the overlay on top.
drawOverlay :: MonadClientUI m => ColorMode -> Overlay -> m SingleFrame
drawOverlay dm over = do
  cops <- getsState scops
  stgtMode <- getsClient stgtMode
  arena <- getArenaUI
  let lid = maybe arena tgtLevelId stgtMode
  mleader <- getsClient _sleader
  s <- getState
  cli <- getClient
  per <- getPerFid lid
  return $! draw dm cops per lid mleader cli s over

-- TODO: if more slides, don't take head, but do as in getInitConfirms,
-- but then we have to clear the messages or they get redisplayed
-- each time screen is refreshed.
-- | Push the frame depicting the current level to the frame queue.
-- Only one screenful of the report is shown, the rest is ignored.
displayPush :: MonadClientUI m => m ()
displayPush = do
  sls <- promptToSlideshow ""
  let slide = head $ slideshow sls
  frame <- drawOverlay ColorFull slide
  -- Visually speed up (by remving all empty frames) the show of the sequence
  -- of the move frames if the player is running.
  srunning <- getsClient srunning
  displayFrame (isJust srunning) $ Just frame

scoreToSlideshow :: MonadClientUI m => Int -> Status -> m Slideshow
scoreToSlideshow total status = do
  fid <- getsClient sside
  fact <- getsState $ (EM.! fid) . sfactionD
  table <- getsState shigh
  time <- getsState stime
  date <- liftIO getClockTime
  sdifficulty <- getsClient sdifficulty
  let showScore (ntable, pos) = HighScore.highSlideshow ntable pos status
      diff | not $ playerUI $ gplayer fact = 0
           | otherwise = sdifficulty
  return $! maybe Monoid.mempty showScore
            $ HighScore.register table total time status date diff

restoreGame :: MonadClient m => m (Maybe (State, StateClient))
restoreGame = do
  Kind.COps{corule} <- getsState scops
  let pathsDataFile = rpathsDataFile $ Kind.stdRuleset corule
  side <- getsClient sside
  isAI <- getsClient sisAI
  prefix <- getsClient $ ssavePrefixCli . sdebugCli
  ConfigUI{ configAppDataDir
          , configUICfgFile } <- getsClient sconfigUI
  let copies = [(configUICfgFile <.> ".default", configUICfgFile <.> ".ini")]
      name = fromMaybe "save" prefix <.> saveName side isAI
  liftIO $ Save.restoreGame name configAppDataDir copies pathsDataFile

-- | Assuming the client runs on the same machine and for the same
-- user as the server, move the server savegame out of the way.
removeServerSave :: MonadClient m => m ()
removeServerSave = do
  prefix <- getsClient $ ssavePrefixCli . sdebugCli  -- hack: assume the same
  ConfigUI{configAppDataDir} <- getsClient sconfigUI
  let serverSaveFile = configAppDataDir
                       </> fromMaybe "save" prefix
                       <.> serverSaveName
  bSer <- liftIO $ doesFileExist serverSaveFile
  when bSer $ liftIO $ renameFile serverSaveFile (serverSaveFile ++ ".bkp")

-- | Invoke pseudo-random computation with the generator kept in the state.
rndToAction :: MonadClient m => Rnd a -> m a
rndToAction r = do
  g <- getsClient srandom
  let (a, ng) = St.runState r g
  modifyClient $ \cli -> cli {srandom = ng}
  return a

-- TODO: restrict the animation to 'per' before drawing.
-- | Render animations on top of the current screen frame.
animate :: MonadClientUI m => LevelId -> Animation -> m Frames
animate arena anim = do
  cops <- getsState scops
  sreport <- getsClient sreport
  mleader <- getsClient _sleader
  Level{lxsize, lysize} <- getLevel arena
  cli <- getClient
  s <- getState
  per <- getPerFid arena
  let over = renderReport sreport
      topLineOnly = truncateToOverlay lxsize over
      basicFrame = draw ColorFull cops per arena mleader cli s topLineOnly
  snoAnim <- getsClient $ snoAnim . sdebugCli
  return $ if fromMaybe False snoAnim
           then [Just basicFrame]
           else renderAnim lxsize lysize basicFrame anim

-- | The part of speech describing the actor or a special name if a leader
-- of the observer's faction. The actor may not be present in the dungeon.
partActorLeader :: MonadClient m => ActorId -> Actor -> m MU.Part
partActorLeader aid b = do
  mleader <- getsClient _sleader
  return $! case mleader of
    Just leader | aid == leader -> "you"
    _ -> partActor b

-- | The part of speech describing the actor (designated by actor id
-- and present in the dungeon) or a special name if a leader
-- of the observer's faction.
partAidLeader :: MonadClient m => ActorId -> m MU.Part
partAidLeader aid = do
  b <- getsState $ getActorBody aid
  partActorLeader aid b

parseConfigUI :: FilePath -> ConfigIO.CP -> ConfigUI
parseConfigUI dataDir cp =
  let mkKey s =
        case K.keyTranslate s of
          K.Unknown _ ->
            assert `failure` "unknown config file key" `twith` (s, cp)
          key -> key
      mkKM ('C':'T':'R':'L':'-':s) = K.KM {key=mkKey s, modifier=K.Control}
      mkKM s = K.KM {key=mkKey s, modifier=K.NoModifier}
      configCommands =
        let mkCommand (key, def) = (mkKM key, read def :: HumanCmd)
            section = ConfigIO.getItems cp "commands"
        in map mkCommand section
      configAppDataDir = dataDir
      configUICfgFile = "config.ui"
      configSavePrefix = ConfigIO.get cp "file" "savePrefix"
      configMacros =
        let trMacro (from, to) =
              let fromTr = mkKM from
                  toTr  = mkKM to
              in if fromTr == toTr
                 then assert `failure` "degenerate alias" `twith` toTr
                 else (fromTr, toTr)
            section = ConfigIO.getItems cp "macros"
        in map trMacro section
      configFont = ConfigIO.get cp "ui" "font"
      configHistoryMax = ConfigIO.get cp "ui" "historyMax"
      configMaxFps = ConfigIO.get cp "ui" "maxFps"
      configNoAnim = ConfigIO.get cp "ui" "noAnim"
      configRunStopMsgs = ConfigIO.get cp "ui" "runStopMsgs"
  in ConfigUI{..}

-- | Read and parse UI config file.
mkConfigUI :: Kind.Ops RuleKind -> IO ConfigUI
mkConfigUI corule = do
  let cpUIDefault = rcfgUIDefault $ Kind.stdRuleset corule
  dataDir <- ConfigIO.appDataDir
  cpUI <- ConfigIO.mkConfig cpUIDefault $ dataDir </> "config.ui.ini"
  let conf = parseConfigUI dataDir cpUI
  -- Catch syntax errors ASAP,
  return $! deepseq conf conf
