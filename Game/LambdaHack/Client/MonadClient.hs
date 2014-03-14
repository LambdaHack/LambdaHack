{-# LANGUAGE FlexibleContexts, FunctionalDependencies, RankNTypes, TupleSections
             #-}
-- | Game action monads and basic building blocks for human and computer
-- player actions. Has no access to the the main action type.
-- Does not export the @liftIO@ operation nor a few other implementation
-- details.
module Game.LambdaHack.Client.MonadClient
  ( -- * Action monads
    MonadClient( getClient, getsClient, modifyClient, putClient, saveClient
               , liftIO  -- ^ exposed only to be implemented, not used
               )
  , MonadClientUI( getsSession  -- ^ exposed only to be implemented, not used
                 )
  , MonadClientReadServer(..), MonadClientWriteServer(..)
  , SessionUI(..), ConnFrontend(..), connFrontend
    -- * Executing actions
  , mkConfigUI
    -- * Accessors to the game session Reader and the Perception Reader(-like)
  , askBinding, getPerFid
    -- * History and report
  , msgAdd, msgReset, recordHistory
    -- * Key input
  , getKeyOverlayCommand, getInitConfirms, stopPlayBack, stopRunning
    -- * Display and key input
  , displayFrames, displayMore, displayYesNo, displayChoiceUI
    -- * Generate slideshows
  , promptToSlideshow, overlayToSlideshow, overlayToBlankSlideshow
    -- * Draw frames
  , drawOverlay, animate
    -- * Assorted primitives
  , restoreGame, removeServerSave, displayPush, scoreToSlideshow
  , rndToAction, getArenaUI, getLeaderUI, targetDescLeader, viewedLevel
  , aidTgtToPos, aidTgtAims, makeLine, saveName
  , leaderTgtToPos, leaderTgtAims, cursorToPos
  , partAidLeader, partActorLeader, unexploredDepth
  , getCacheBfsAndPath, getCacheBfs, accessCacheBfs
  , closestUnknown, closestSmell, furthestKnown, closestTriggers
  , closestItems, closestFoes, actorAbilities, pongUI, syncFrames
  , debugPrint
  , SlideOrCmd, failWith, failSlides, failSer
  ) where

import Control.Arrow ((&&&))
import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Ini as Ini
import qualified Data.Ini.Reader as Ini
import qualified Data.Ini.Types as Ini
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU
import System.Directory
import System.FilePath
import System.Time
import Text.Read

import Game.LambdaHack.Client.Binding
import Game.LambdaHack.Client.ConfigUI
import Game.LambdaHack.Client.Draw
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Ability (Ability)
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Animation
import Game.LambdaHack.Common.AtomicCmd
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.HighScore as HighScore
import Game.LambdaHack.Common.HumanCmd
import qualified Game.LambdaHack.Common.Key as K
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Request
import qualified Game.LambdaHack.Common.Save as Save
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Frontend as Frontend
import Game.LambdaHack.Utils.File

-- | The information that is constant across a client playing session,
-- including many consecutive games in a single session,
-- but is completely disregarded and reset when a new playing session starts.
-- Auxiliary AI and computer player clients have no @sfs@ nor @sbinding@.
data SessionUI = SessionUI
  { sfconn   :: !ConnFrontend  -- ^ connection with the frontend
  , sbinding :: !Binding       -- ^ binding of keys to commands
  , sescMVar :: !(Maybe (MVar ()))
  }

-- | Connection method between a client and a frontend.
data ConnFrontend = ConnFrontend
  { readConnFrontend  :: MonadClientUI m => m K.KM
      -- ^ read a keystroke received from the frontend
  , writeConnFrontend :: MonadClientUI m => Frontend.FrontReq -> m ()
      -- ^ write a UI request to the frontend
  }

class MonadActionRO m => MonadClient m where
  getClient    :: m StateClient
  getsClient   :: (StateClient -> a) -> m a
  modifyClient :: (StateClient -> StateClient) -> m ()
  putClient    :: StateClient -> m ()
  -- We do not provide a MonadIO instance, so that outside of Action/
  -- nobody can subvert the action monads by invoking arbitrary IO.
  liftIO       :: IO a -> m a
  saveClient   :: m ()

class MonadClient m => MonadClientUI m where
  getsSession  :: (SessionUI -> a) -> m a

class MonadClient m => MonadClientReadServer c m | m -> c where
  readServer  :: m c

class MonadClient m => MonadClientWriteServer d m | m -> d where
  writeServer  :: d -> m ()

saveName :: FactionId -> Bool -> String
saveName side isAI =
  let n = fromEnum side  -- we depend on the numbering hack to number saves
  in (if n > 0
      then "human_" ++ show n
      else "computer_" ++ show (-n))
     ++ if isAI then ".ai.sav" else ".ui.sav"

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
  escPressed <- tryTakeMVarSescMVar  -- this also clears the ESC marker
  lastPlayOld <- getsClient slastPlay
  km <- case lastPlayOld of
    km : kms | not escPressed && (null frontKM || km `elem` frontKM) -> do
      displayFrame False $ Just frontFr
      modifyClient $ \cli -> cli {slastPlay = kms}
      return km
    _ -> do
      unless (null lastPlayOld) stopPlayBack  -- we can't continue playback
      ConnFrontend{..} <- getsSession sfconn
      writeConnFrontend Frontend.FrontKey {..}
      readConnFrontend
  (seqCurrent, seqPrevious, k) <- getsClient slastRecord
  let slastRecord = (km : seqCurrent, seqPrevious, k)
  modifyClient $ \cli -> cli {slastRecord}
  return km

stopPlayBack :: MonadClientUI m => m ()
stopPlayBack = do
  modifyClient $ \cli -> cli
    { slastPlay = []
    , slastRecord = let (seqCurrent, seqPrevious, _) = slastRecord cli
                    in (seqCurrent, seqPrevious, 0)
    , swaitTimes = - swaitTimes cli
    }
  stopRunning

stopRunning :: MonadClientUI m => m ()
stopRunning = do
  srunning <- getsClient srunning
  case srunning of
    Nothing -> return ()
    Just RunParams{runLeader} -> do
      -- Switch to the original leader, from before the run start, unless dead.
      side <- getsClient sside
      fact <- getsState $ (EM.! side) . sfactionD
      arena <- getArenaUI
      s <- getState
      when (memActor runLeader arena s && not (isSpawnFact fact)) $
        modifyClient $ updateLeader runLeader s
      modifyClient (\cli -> cli { srunning = Nothing })

-- | Display a slideshow, awaiting confirmation for each slide except the last.
getInitConfirms :: MonadClientUI m
                => ColorMode -> [K.KM] -> Slideshow -> m Bool
getInitConfirms dm frontClear slides = do
  ConnFrontend{..} <- getsSession sfconn
  let (onBlank, ovs) = slideshow slides
  frontSlides <- mapM (drawOverlay onBlank dm) ovs
  -- The first two cases are optimizations:
  case frontSlides of
    [] -> return True
    [x] -> do
      displayFrame False $ Just x
      return True
    _ -> do
      writeConnFrontend Frontend.FrontSlides{..}
      km <- readConnFrontend
      -- Don't clear ESC marker here, because the wait for confirms may
      -- block a ping and the ping would not see the ESC.
      return $! km /= K.escKey

-- | Sync frames display with the frontend.
syncFrames :: MonadClientUI m => m ()
syncFrames = do
  ConnFrontend{..} <- getsSession sfconn
  -- Hack.
  writeConnFrontend Frontend.FrontSlides{frontClear=[], frontSlides=[]}
  km <- readConnFrontend
  assert (km == K.spaceKey) skip

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
getKeyOverlayCommand :: MonadClientUI m => Bool -> Overlay -> m K.KM
getKeyOverlayCommand onBlank overlay = do
  frame <- drawOverlay onBlank ColorFull overlay
  -- Give the previous client time to display his frames.
  liftIO $ threadDelay 1000
  promptGetKey [] frame

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
  getInitConfirms dm [] $ slides <> toSlideshow False [[]]

-- | Print a yes/no question and return the player's answer. Use black
-- and white colours to turn player's attention to the choice.
displayYesNo :: MonadClientUI m => ColorMode -> Msg -> m Bool
displayYesNo dm prompt = do
  sli <- promptToSlideshow $ prompt <+> yesnoMsg
  frame <- drawOverlay False dm $ head . snd $ slideshow sli
  getYesNo frame

-- TODO: generalize getInitConfirms and displayChoiceUI to a single op
-- | Print a prompt and an overlay and wait for a player keypress.
-- If many overlays, scroll screenfuls with SPACE. Do not wrap screenfuls
-- (in some menus @?@ cycles views, so the user can restart from the top).
displayChoiceUI :: MonadClientUI m
                => Msg -> Overlay -> [K.KM] -> m (Either Slideshow K.KM)
displayChoiceUI prompt ov keys = do
  (_, ovs) <- fmap slideshow $ overlayToSlideshow (prompt <> ", ESC]") ov
  let legalKeys = [K.spaceKey, K.escKey]
        ++ keys
      loop [] = fmap Left $ promptToSlideshow "never mind"
      loop (x : xs) = do
        frame <- drawOverlay False ColorFull x
        km@K.KM {..} <- promptGetKey legalKeys frame
        case key of
          K.Esc -> fmap Left $ promptToSlideshow "never mind"
          K.Space -> loop xs
          _ -> return $ Right km
  loop ovs

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
  lid <- getArenaUI
  Level{lxsize, lysize} <- getLevel lid  -- TODO: screen length or viewLevel
  sreport <- getsClient sreport
  let msg = splitReport lxsize (addMsg sreport prompt)
  return $! splitOverlay False (lysize + 1) msg overlay

overlayToBlankSlideshow :: MonadClientUI m => Msg -> Overlay -> m Slideshow
overlayToBlankSlideshow prompt overlay = do
  lid <- getArenaUI
  Level{lysize} <- getLevel lid  -- TODO: screen length or viewLevel
  return $! splitOverlay True (lysize + 3) (toOverlay [prompt]) overlay

-- | Draw the current level with the overlay on top.
drawOverlay :: MonadClientUI m => Bool -> ColorMode -> Overlay -> m SingleFrame
drawOverlay onBlank dm over = do
  cops <- getsState scops
  lid <- viewedLevel
  mleader <- getsClient _sleader
  s <- getState
  cli <- getClient
  per <- getPerFid lid
  tgtPos <- leaderTgtToPos
  cursorPos <- cursorToPos
  let anyPos = fromMaybe (Point 0 0) cursorPos
      pathFromLeader leader = fmap Just $ getCacheBfsAndPath leader anyPos
  bfsmpath <- maybe (return Nothing) pathFromLeader mleader
  tgtDesc <- maybe (return "------") targetDescLeader mleader
  cursorDesc <- targetDescCursor
  return $! draw onBlank dm cops per lid mleader cursorPos tgtPos
                 bfsmpath cli s cursorDesc tgtDesc over

-- TODO: if more slides, don't take head, but do as in getInitConfirms,
-- but then we have to clear the messages or they get redisplayed
-- each time screen is refreshed.
-- | Push the frame depicting the current level to the frame queue.
-- Only one screenful of the report is shown, the rest is ignored.
displayPush :: MonadClientUI m => m ()
displayPush = do
  sls <- promptToSlideshow ""
  let slide = head . snd $ slideshow sls
  frame <- drawOverlay False ColorFull slide
  -- Visually speed up (by remving all empty frames) the show of the sequence
  -- of the move frames if the player is running.
  srunning <- getsClient srunning
  displayFrame (isJust srunning) $ Just frame

scoreToSlideshow :: MonadClientUI m => Int -> Status -> m Slideshow
scoreToSlideshow total status = do
  cops <- getsState scops
  fid <- getsClient sside
  fact <- getsState $ (EM.! fid) . sfactionD
  -- TODO: Re-read the table in case it's changed by a concurrent game.
  -- TODO: we should do this, and make sure we do that after server
  -- saved the updated score table, and not register, but read from it.
  -- Otherwise the score is not accurate, e.g., the number of victims.
  table <- getsState shigh
  time <- getsState stime
  date <- liftIO getClockTime
  scurDifficulty <- getsClient scurDifficulty
  factionD <- getsState sfactionD
  dungeon <- getsState sdungeon
  let showScore (ntable, pos) = HighScore.highSlideshow ntable pos
      diff | not $ playerUI $ gplayer fact = difficultyDefault
           | otherwise = scurDifficulty
      theirVic (fi, fa) | isAtWar fact fi
                          && not (isHorrorFact cops fa) = Just $ gvictims fa
                        | otherwise = Nothing
      theirVictims = EM.unionsWith (+) $ mapMaybe theirVic $ EM.assocs factionD
      ourVic (fi, fa) | isAllied fact fi || fi == fid = Just $ gvictims fa
                      | otherwise = Nothing
      ourVictims = EM.unionsWith (+) $ mapMaybe ourVic $ EM.assocs factionD
      fightsAgainstSpawners =
        let escape = any lescape $ EM.elems dungeon
            isSpawner = isSpawnFact fact
        in escape && not isSpawner
      (worthMentioning, rScore) =
        HighScore.register table total time status date diff
                           (playerName $ gplayer fact)
                           ourVictims theirVictims fightsAgainstSpawners
  return $! if worthMentioning then showScore rScore else mempty

restoreGame :: MonadClient m => m (Maybe (State, StateClient))
restoreGame = do
  Kind.COps{corule} <- getsState scops
  let stdRuleset = Kind.stdRuleset corule
      pathsDataFile = rpathsDataFile stdRuleset
      cfgUIName = rcfgUIName stdRuleset
  side <- getsClient sside
  isAI <- getsClient sisAI
  prefix <- getsClient $ ssavePrefixCli . sdebugCli
  let copies = [( "GameDefinition" </> cfgUIName <.> "default"
                , cfgUIName <.> "ini" )]
      name = fromMaybe "save" prefix <.> saveName side isAI
  liftIO $ Save.restoreGame name copies pathsDataFile

-- | Assuming the client runs on the same machine and for the same
-- user as the server, move the server savegame out of the way.
removeServerSave :: MonadClient m => m ()
removeServerSave = do
  prefix <- getsClient $ ssavePrefixCli . sdebugCli  -- hack: assume the same
  dataDir <- liftIO appDataDir
  let serverSaveFile = dataDir
                       </> fromMaybe "save" prefix
                       <.> serverSaveName
  bSer <- liftIO $ doesFileExist serverSaveFile
  when bSer $ liftIO $ renameFile serverSaveFile (serverSaveFile <.> "bkp")

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
  tgtPos <- leaderTgtToPos
  cursorPos <- cursorToPos
  let anyPos = fromMaybe (Point 0 0) cursorPos
      pathFromLeader leader = fmap Just $ getCacheBfsAndPath leader anyPos
  bfsmpath <- maybe (return Nothing) pathFromLeader mleader
  tgtDesc <- maybe (return "------") targetDescLeader mleader
  cursorDesc <- targetDescCursor
  let over = renderReport sreport
      topLineOnly = truncateToOverlay lxsize over
      basicFrame =
        draw False ColorFull cops per arena mleader
             cursorPos tgtPos bfsmpath cli s cursorDesc tgtDesc topLineOnly
  snoAnim <- getsClient $ snoAnim . sdebugCli
  return $! if fromMaybe False snoAnim
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

parseConfigUI :: Ini.Config -> ConfigUI
parseConfigUI cfg =
  let configCommands =
        let mkCommand (ident, keydef) =
              case stripPrefix "Macro_" ident of
                Just _ ->
                  let (key, def) = read keydef
                  in (K.mkKM key, def :: (CmdCategory, HumanCmd))
                Nothing -> assert `failure` "wrong macro id" `twith` ident
            section = Ini.allItems "extra_commands" cfg
        in map mkCommand section
      configHeroNames =
        let toNumber (ident, name) =
              case stripPrefix "HeroName_" ident of
                Just n -> (read n, T.pack name)
                Nothing -> assert `failure` "wrong hero name id" `twith` ident
            section = Ini.allItems "hero_names" cfg
        in map toNumber section
      getOption :: forall a. Read a => String -> a
      getOption optionName =
        let lookupFail :: forall b. String -> b
            lookupFail err =
              assert `failure` ("config file access failed:" <+> T.pack err)
                     `twith` (optionName, cfg)
            s = fromMaybe (lookupFail "") $ Ini.getOption "ui" optionName cfg
        in either lookupFail id $ readEither s
      configVi = getOption "movementViKeys_hjklyubn"
      -- The option for Vi keys takes precendence,
      -- because the laptop keys are the default.
      configLaptop = not configVi && getOption "movementLaptopKeys_uk8o79jl"
      configFont = getOption "font"
      configHistoryMax = getOption "historyMax"
      configMaxFps = getOption "maxFps"
      configNoAnim = getOption "noAnim"
      configRunStopMsgs = getOption "runStopMsgs"
  in ConfigUI{..}

-- | Read and parse UI config file.
mkConfigUI :: Kind.Ops RuleKind -> IO ConfigUI
mkConfigUI corule = do
  let stdRuleset = Kind.stdRuleset corule
      cfgUIName = rcfgUIName stdRuleset
      commentsUIDefault = init $ map (drop 2) $ lines $ rcfgUIDefault stdRuleset  -- TODO: init is a hack until Ini accepts empty files
      sUIDefault = unlines commentsUIDefault
      cfgUIDefault = either (assert `failure`) id $ Ini.parse sUIDefault
  dataDir <- appDataDir
  let userPath = dataDir </> cfgUIName <.> "ini"
  cfgUser <- do
    cpExists <- doesFileExist userPath
    if not cpExists
      then return Ini.emptyConfig
      else do
        sUser <- readFile userPath
        return $! either (assert `failure`) id $ Ini.parse sUser
  let cfgUI = M.unionWith M.union cfgUser cfgUIDefault  -- user cfg preferred
      conf = parseConfigUI cfgUI
  -- Catch syntax errors in complex expressions ASAP,
  return $! deepseq conf conf

-- | Get cached BFS data and path or, if not stored, generate,
-- store and return. Due to laziness, they are not calculated until needed.
getCacheBfsAndPath :: forall m. MonadClient m
                   => ActorId -> Point
                   -> m (PointArray.Array BfsDistance, Maybe [Point])
getCacheBfsAndPath aid target = do
  seps <- getsClient seps
  let pathAndStore :: PointArray.Array BfsDistance
                   -> m (PointArray.Array BfsDistance, Maybe [Point])
      pathAndStore bfs = do
        computePath <- computePathBFS aid
        let mpath = computePath target seps bfs
        modifyClient $ \cli ->
          cli {sbfsD = EM.insert aid (bfs, target, seps, mpath) (sbfsD cli)}
        return (bfs, mpath)
  mbfs <- getsClient $ EM.lookup aid . sbfsD
  case mbfs of
    Just (bfs, targetOld, sepsOld, mpath) | targetOld == target
                                            && sepsOld == seps ->
      return (bfs, mpath)
    Just (bfs, _, _, _) -> pathAndStore bfs
    Nothing -> do
      bfs <- computeBFS aid
      pathAndStore bfs

getCacheBfs :: MonadClient m => ActorId -> m (PointArray.Array BfsDistance)
{-# INLINE getCacheBfs #-}
getCacheBfs aid = do
  mbfs <- getsClient $ EM.lookup aid . sbfsD
  case mbfs of
    Just (bfs, _, _, _) -> return bfs
    Nothing -> fmap fst $ getCacheBfsAndPath aid (Point 0 0)

computeBFS :: MonadClient m => ActorId -> m (PointArray.Array BfsDistance)
computeBFS = computeAnythingBFS $ \isEnterable passUnknown aid -> do
  b <- getsState $ getActorBody aid
  Level{lxsize, lysize} <- getLevel $ blid b
  let origin = bpos b
      vInitial = PointArray.replicateA lxsize lysize apartBfs
  -- Here we don't want '$!', because we want the BFS data lazy.
  return ${-keep it!-} fillBfs isEnterable passUnknown origin vInitial

computePathBFS :: MonadClient m
               => ActorId
               -> m (Point -> Int -> PointArray.Array BfsDistance
                     -> Maybe [Point])
computePathBFS = computeAnythingBFS $ \isEnterable passUnknown aid -> do
  b <- getsState $ getActorBody aid
  let origin = bpos b
  -- Here we don't want '$!', because we want the BFS data lazy.
  return ${-keep it!-} findPathBfs isEnterable passUnknown origin

computeAnythingBFS :: MonadClient m
                   => ((Point -> Point -> MoveLegal)
                       -> (Point -> Point -> Bool)
                       -> ActorId
                       -> m a)
                   -> ActorId
                   -> m a
computeAnythingBFS fAnything aid = do
  cops@Kind.COps{cotile=cotile@Kind.Ops{ouniqGroup}} <- getsState scops
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  smarkSuspect <- getsClient smarkSuspect
  sisAI <- getsClient sisAI
  -- We treat doors as an open tile and don't add an extra step for opening
  -- the doors, because other actors open and use them, too,
  -- so it's amortized. We treat unknown tiles specially.
  let -- Suspect tiles treated as a kind of unknown.
      passSuspect = smarkSuspect || sisAI  -- AI checks suspects ASAP
      unknownId = ouniqGroup "unknown space"
      chAccess = checkAccess cops lvl
      chDoorAccess = checkDoorAccess cops lvl
      conditions = catMaybes [chAccess, chDoorAccess]
      -- Legality of move from a known tile, assuming doors freely openable.
      isEnterable :: Point -> Point -> MoveLegal
      isEnterable spos tpos =
        let tt = lvl `at` tpos
            allOK = all (\f -> f spos tpos) conditions
        in if tt == unknownId
           then if allOK
                then MoveToUnknown
                else MoveBlocked
           else if Tile.isSuspect cotile tt
                then if passSuspect && allOK
                     then MoveToUnknown
                     else MoveBlocked
                else if Tile.isPassable cotile tt && allOK
                     then MoveToOpen
                     else MoveBlocked
      -- Legality of move from an unknown tile, assuming unknown are open.
      passUnknown :: Point -> Point -> Bool
      passUnknown = case chAccess of  -- spos is unknown, so not a door
        Nothing -> \_ tpos -> let tt = lvl `at` tpos
                              in tt == unknownId
                                 || passSuspect && Tile.isSuspect cotile tt
        Just ch -> \spos tpos -> let tt = lvl `at` tpos
                                 in (tt == unknownId
                                     || passSuspect
                                        && Tile.isSuspect cotile tt)
                                    && ch spos tpos
  fAnything isEnterable passUnknown aid

accessCacheBfs :: MonadClient m => ActorId -> Point -> m (Maybe Int)
{-# INLINE accessCacheBfs #-}
accessCacheBfs aid target = do
  bfs <- getCacheBfs aid
  return $! accessBfs bfs target

targetDesc :: MonadClientUI m => Maybe Target -> m Text
targetDesc target = do
  lidV <- viewedLevel
  mleader <- getsClient _sleader
  case target of
    Just (TEnemy a _) ->
      getsState $ bname . getActorBody a
    Just (TEnemyPos _ lid p _) ->
      return $! if lid == lidV
                then "hot spot" <+> (T.pack . show) p
                else "a hot spot on level" <+> tshow (abs $ fromEnum lid)
    Just (TPoint lid p) ->
      return $! if lid == lidV
                then "exact spot" <+> (T.pack . show) p
                else "an exact spot on level" <+> tshow (abs $ fromEnum lid)
    Just TVector{} ->
      case mleader of
        Nothing -> return "a relative shift"
        Just aid -> do
          tgtPos <- aidTgtToPos aid lidV target
          let invalidMsg = "an invalid relative shift"
              validMsg p = "shift to" <+> (T.pack . show) p
          return $! maybe invalidMsg validMsg tgtPos
    Nothing -> return "cursor location"

targetDescLeader :: MonadClientUI m => ActorId -> m Text
targetDescLeader leader = do
  tgt <- getsClient $ getTarget leader
  targetDesc tgt

targetDescCursor :: MonadClientUI m => m Text
targetDescCursor = do
  scursor <- getsClient scursor
  targetDesc $ Just scursor

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
      fact <- getsState $ (EM.! side) . sfactionD
      case gquit fact of
        Just Status{stDepth} -> return $! toEnum stDepth
        Nothing -> do
          dungeon <- getsState sdungeon
          let (minD, maxD) =
                case (EM.minViewWithKey dungeon, EM.maxViewWithKey dungeon) of
                  (Just ((s, _), _), Just ((e, _), _)) -> (s, e)
                  _ -> assert `failure` "empty dungeon" `twith` dungeon
          return $! max minD $ min maxD $ playerEntry $ gplayer fact

viewedLevel :: MonadClientUI m => m LevelId
viewedLevel = do
  arena <- getArenaUI
  stgtMode <- getsClient stgtMode
  return $! maybe arena tgtLevelId stgtMode

-- | Calculate the position of an actor's target.
aidTgtToPos :: MonadClient m
            => ActorId -> LevelId -> Maybe Target -> m (Maybe Point)
aidTgtToPos aid lidV tgt =
  case tgt of
    Just (TEnemy a _) -> do
      body <- getsState $ getActorBody a
      return $! if blid body == lidV
                then Just (bpos body)
                else Nothing
    Just (TEnemyPos _ lid p _) ->
      return $! if lid == lidV then Just p else Nothing
    Just (TPoint lid p) ->
      return $! if lid == lidV then Just p else Nothing
    Just (TVector v) -> do
      b <- getsState $ getActorBody aid
      Level{lxsize, lysize} <- getLevel lidV
      let shifted = shiftBounded lxsize lysize (bpos b) v
      return $! if shifted == bpos b && v /= Vector 0 0
                then Nothing
                else Just shifted
    Nothing -> do
      scursor <- getsClient scursor
      aidTgtToPos aid lidV $ Just scursor

-- | Check whether one is permitted to aim at a target
-- (this is only checked for actors; positions let player
-- shoot at obstacles, e.g., to destroy them).
-- This assumes @aidTgtToPos@ does not return @Nothing@.
--
-- Note: Perception is not enough for the check,
-- because the target actor can be obscured by a glass wall
-- or be out of sight range, but in weapon range.
aidTgtAims :: MonadClient m
           => ActorId -> LevelId -> Maybe Target -> m (Maybe Text)
aidTgtAims aid lidV tgt = do
  case tgt of
    Just (TEnemy a _) -> do
      body <- getsState $ getActorBody a
      let pos = bpos body
      b <- getsState $ getActorBody aid
      if blid b == lidV then do
        seps <- getsClient seps
        (steps, _eps) <- makeLine b pos seps
        if steps == chessDist (bpos b) pos
          then return Nothing
          else return $ Just "aiming line to the opponent blocked"
      else return $ Just "target opponent not on this level"
    Just TEnemyPos{} -> return $ Just "target opponent not visible"
    Just TPoint{} -> return Nothing
    Just TVector{} -> return Nothing
    Nothing -> do
      scursor <- getsClient scursor
      aidTgtAims aid lidV $ Just scursor

-- | Counts the number of steps until the projectile would hit
-- an actor or obstacle. Prefers the given eps.
-- TODO: but modifies eps, if needed.
makeLine :: MonadClient m => Actor -> Point -> Int -> m (Int, Int)
makeLine body fpos eps = do
  cops <- getsState scops
  lvl@Level{lxsize, lysize} <- getLevel (blid body)
  bs <- getsState $ actorNotProjList (const True) (blid body)
  let mbl = bla lxsize lysize eps (bpos body) fpos
  case mbl of
    Just bl@(pos1:_) -> do
      let noActor p = any ((== p) . bpos) bs || p == fpos
      case break noActor bl of
        (flies, hits : _) -> do
          let blRest = flies ++ [hits]
              blZip = zip (bpos body : blRest) blRest
              blAccess = takeWhile (uncurry $ accessible cops lvl) blZip
          mab <- getsState $ posToActor pos1 (blid body)
          if maybe True (bproj . snd . fst) mab then
            return $ (length blAccess, eps)
          else return (0, eps)  -- ProjectBlockActor
        _ -> assert `failure` (body, fpos, bl)
    Just [] -> assert `failure` (body, fpos)
    Nothing -> return (0, eps)  -- ProjectAimOnself

leaderTgtToPos :: MonadClientUI m => m (Maybe Point)
leaderTgtToPos = do
  lidV <- viewedLevel
  mleader <- getsClient _sleader
  case mleader of
    Nothing -> return Nothing
    Just aid -> do
      tgt <- getsClient $ getTarget aid
      aidTgtToPos aid lidV tgt

leaderTgtAims :: MonadClientUI m => m (Maybe Text)
leaderTgtAims = do
  lidV <- viewedLevel
  mleader <- getsClient _sleader
  case mleader of
    Nothing -> return $ Just "no leader to target with"
    Just aid -> do
      tgt <- getsClient $ getTarget aid
      aidTgtAims aid lidV tgt

cursorToPos :: MonadClientUI m => m (Maybe Point)
cursorToPos = do
  lidV <- viewedLevel
  mleader <- getsClient _sleader
  scursor <- getsClient scursor
  case mleader of
    Nothing -> return Nothing
    Just aid -> aidTgtToPos aid lidV $ Just scursor

-- | Furthest (wrt paths) known position, except under the actor.
furthestKnown :: MonadClient m => ActorId -> m (Maybe Point)
furthestKnown aid = do
  bfs <- getCacheBfs aid
  getMaxIndex <- rndToAction $ oneOf [ PointArray.maxIndexA
                                     , PointArray.maxLastIndexA ]
  let furthestPos = getMaxIndex bfs
      dist = bfs PointArray.! furthestPos
  return $! if dist <= apartBfs
            then assert `failure` (aid, furthestPos, dist)
            else if dist == succ apartBfs  -- bpos of aid
                 then Nothing
                 else Just furthestPos

-- | Closest reachable unknown tile position, if any.
closestUnknown :: MonadClient m => ActorId -> m (Maybe Point)
closestUnknown aid = do
  bfs <- getCacheBfs aid
  getMinIndex <- rndToAction $ oneOf [ PointArray.minIndexA
                                     , PointArray.minLastIndexA ]
  let closestPos = getMinIndex bfs
      dist = bfs PointArray.! closestPos
  if dist >= apartBfs then do
    body <- getsState $ getActorBody aid
    smarkSuspect <- getsClient smarkSuspect
    sisAI <- getsClient sisAI
    let passSuspect = smarkSuspect || sisAI
    when passSuspect $  -- explored fully, including suspect tiles
      modifyClient $ \cli ->
        cli {sexplored = ES.insert (blid body) (sexplored cli)}
    return Nothing
  else return $ Just closestPos

-- TODO: this is costly, because target has to be changed every
-- turn when walking along trail. But inverting the sort and going
-- to the newest smell, while sometimes faster, may result in many
-- actors following the same trail, unless we wipe the trail as soon
-- as target is assigned (but then we don't know if we should keep the target
-- or not, because somebody already followed it). OTOH, trails are not
-- common and so if wiped they can't incur a large total cost.
-- TODO: remove targets where the smell is likely to get too old by the time
-- the actor gets there.
-- | Finds smells closest to the actor, except under the actor.
closestSmell :: MonadClient m => ActorId -> m [(Int, (Point, Tile.SmellTime))]
closestSmell aid = do
  body <- getsState $ getActorBody aid
  Level{lsmell} <- getLevel $ blid body
  let smells = EM.assocs lsmell
  case smells of
    [] -> return []
    _ -> do
      bfs <- getCacheBfs aid
      let ts = mapMaybe (\x@(p, _) -> fmap (,x) (accessBfs bfs p)) smells
          ds = filter (\(d, _) -> d /= 0) ts  -- bpos of aid
      return $! sortBy (comparing (fst &&& timeNegate . snd . snd)) ds

-- TODO: We assume linear dungeon in @unexploredD@,
-- because otherwise we'd need to calculate shortest paths in a graph, etc.
-- | Closest (wrt paths) triggerable open tiles.
-- The second argument can ever be true only if there's
-- no escape from the dungeon.
closestTriggers :: MonadClient m => Maybe Bool -> Bool -> ActorId -> m [Point]
closestTriggers onlyDir exploredToo aid = do
  Kind.COps{cotile} <- getsState scops
  body <- getsState $ getActorBody aid
  lvl <- getLevel $ blid body
  dungeon <- getsState sdungeon
  explored <- getsClient sexplored
  unexploredD <- unexploredDepth
  let allExplored = ES.size explored == EM.size dungeon
      unexUp = onlyDir /= Just False && unexploredD 1 (blid body)
      unexDown = onlyDir /= Just True && unexploredD (-1) (blid body)
      unexEffect (Effect.Ascend p) = if p > 0 then unexUp else unexDown
      unexEffect _ =
        -- Escape (or guard) only after exploring, for high score, etc.
        allExplored
      isTrigger
        | exploredToo = \t -> Tile.isWalkable cotile t
                              && not (null $ Tile.causeEffects cotile t)
        | otherwise = \t -> Tile.isWalkable cotile t
                            && any unexEffect (Tile.causeEffects cotile t)
      f :: [Point] -> Point -> Kind.Id TileKind -> [Point]
      f acc p t = if isTrigger t then p : acc else acc
  let triggersAll = PointArray.ifoldlA f [] $ ltile lvl
      -- Don't target stairs under the actor. Most of the time they
      -- are blocked and stay so, so we seek other stairs, if any.
      -- If no other stairs in this direction, let's wait here.
      triggers | length triggersAll > 1 = delete (bpos body) triggersAll
               | otherwise = triggersAll
  case triggers of
    [] -> return []
    _ -> do
      bfs <- getCacheBfs aid
      let ds = mapMaybe (\p -> fmap (,p) (accessBfs bfs p)) triggers
      return $! map snd $ sortBy (comparing fst) ds

unexploredDepth :: MonadClient m => m (Int -> LevelId -> Bool)
unexploredDepth = do
  dungeon <- getsState sdungeon
  explored <- getsClient sexplored
  let allExplored = ES.size explored == EM.size dungeon
      unexploredD p =
        let unex lid = allExplored && lescape (dungeon EM.! lid)
                       || ES.notMember lid explored
                       || unexploredD p lid
        in any unex . ascendInBranch dungeon p
  return unexploredD

-- | Closest (wrt paths) items.
closestItems :: MonadClient m => ActorId -> m ([(Int, (Point, ItemBag))])
closestItems aid = do
  body <- getsState $ getActorBody aid
  Level{lfloor} <- getLevel $ blid body
  let items = EM.assocs lfloor
  case items of
    [] -> return []
    _ -> do
      bfs <- getCacheBfs aid
      let ds = mapMaybe (\x@(p, _) -> fmap (,x) (accessBfs bfs p)) items
      return $! sortBy (comparing fst) ds

-- | Closest (wrt paths) enemy actors.
closestFoes :: MonadClient m => ActorId -> m [(Int, (ActorId, Actor))]
closestFoes aid = do
  body <- getsState $ getActorBody aid
  fact <- getsState $ \s -> sfactionD s EM.! bfid body
  foes <- getsState $ actorNotProjAssocs (isAtWar fact) (blid body)
  case foes of
    [] -> return []
    _ -> do
      bfs <- getCacheBfs aid
      let ds = mapMaybe (\x@(_, b) -> fmap (,x) (accessBfs bfs (bpos b))) foes
      return $! sortBy (comparing fst) ds

actorAbilities :: MonadClient m => ActorId -> Maybe ActorId -> m [Ability]
actorAbilities aid mleader = do
  Kind.COps{ coactor=Kind.Ops{okind}
           , cofaction=Kind.Ops{okind=fokind} } <- getsState scops
  body <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid body) . sfactionD
  let factionAbilities
        | Just aid == mleader = fAbilityLeader $ fokind $ gkind fact
        | otherwise = fAbilityOther $ fokind $ gkind fact
  return $! acanDo (okind $ bkind body) `intersect` factionAbilities

tryTakeMVarSescMVar :: MonadClientUI m => m Bool
tryTakeMVarSescMVar = do
  mescMVar <- getsSession sescMVar
  case mescMVar of
    Nothing -> return False
    Just escMVar -> do
      mUnit <- liftIO $ tryTakeMVar escMVar
      return $ isJust mUnit

pongUI :: (MonadClientUI m, MonadClientWriteServer Request m) => m ()
pongUI = do
  -- Ping the frontend, too.
  syncFrames
  escPressed <- tryTakeMVarSescMVar
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  let sendPong ats = writeServer $ ReqTimed $ ReqPongHack ats
      hasAiLeader = playerAiLeader $ gplayer fact
  if escPressed && hasAiLeader then do
    -- Ask server to turn off AI for the faction's leader.
    let atomicCmd = CmdAtomic $ AutoFactionA side False
    sendPong [atomicCmd]
  else
    -- Respond to the server normally.
    sendPong []

type SlideOrCmd a = Either Slideshow a

failWith :: MonadClientUI m => Msg -> m (SlideOrCmd a)
failWith msg = do
  modifyClient $ \cli -> cli {slastKey = Nothing}
  stopPlayBack
  assert (not $ T.null msg) $ fmap Left $ promptToSlideshow msg

failSlides :: MonadClientUI m => Slideshow -> m (SlideOrCmd a)
failSlides slides = do
  modifyClient $ \cli -> cli {slastKey = Nothing}
  stopPlayBack
  return $ Left slides

failSer :: MonadClientUI m => ReqFailure -> m (SlideOrCmd a)
failSer = failWith . showReqFailure
