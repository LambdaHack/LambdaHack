{-# LANGUAGE TupleSections #-}
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
  , getKeyOverlayCommand, getInitConfirms, stopPlayBack, stopRunning
    -- * Display and key input
  , displayFrames, displayMore, displayYesNo, displayChoiceUI
    -- * Generate slideshows
  , promptToSlideshow, overlayToSlideshow, overlayToBlankSlideshow
    -- * Draw frames
  , drawOverlay, animate
    -- * Assorted primitives
  , restoreGame, removeServerSave, displayPush, scoreToSlideshow
  , rndToAction, getArenaUI, getLeaderUI, targetDescLeader
  , viewedLevel, aidTgtToPos, targetToPos, cursorToPos
  , partAidLeader, partActorLeader
  , getCacheBfsAndPath, getCacheBfs, accessCacheBfs
  , actorAimsPos, closestUnknown, closestItems, closestFoes
  , debugPrint
  ) where

import Control.Arrow ((***))
import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.EnumMap.Strict as EM
import Data.List
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
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.Random
import qualified Game.LambdaHack.Common.Save as Save
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Vector
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
  lastPlayOld <- getsClient slastPlay
  km <- case lastPlayOld of
    km : kms | null frontKM || km `elem` frontKM -> do
      displayFrame False $ Just frontFr
      modifyClient $ \cli -> cli {slastPlay = kms}
      return km
    _ -> do
      unless (null lastPlayOld) stopPlayBack  -- something went wrong
      ConnFrontend{..} <- getsSession sfconn
      writeConnFrontend Frontend.FrontKey {..}
      kmRaw <- readConnFrontend
      keyb <- askBinding
      case fromMaybe [kmRaw] $ M.lookup kmRaw $ kmacro keyb of
        [] -> assert `failure` kmRaw
        km : kms -> do
          modifyClient $ \cli -> cli {slastPlay = kms ++ slastPlay cli}
          return km
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
      arena <- getArenaUI
      s <- getState
      when (memActor runLeader arena s) $
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
      return $! km /= K.escKey

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
  getInitConfirms dm [] $ slides Monoid.<> toSlideshow False [[]]

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
  let legalKeys =
        [ K.KM {key=K.Space, modifier=K.NoModifier}
        , K.escKey ]
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
  tgtPos <- fmap (fmap fst) targetToPos
  cursorPos <- cursorToPos
  let pathFromLeader leader =
        maybe (return Nothing) (fmap Just . getCacheBfsAndPath leader) tgtPos
  bfsmpath <- maybe (return Nothing) pathFromLeader mleader
  tgtDesc <- maybe (return "") targetDescLeader mleader
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
  tgtPos <- fmap (fmap fst) targetToPos
  cursorPos <- cursorToPos
  let pathFromLeader leader =
        maybe (return Nothing) (fmap Just . getCacheBfsAndPath leader) tgtPos
  bfsmpath <- maybe (return Nothing) pathFromLeader mleader
  tgtDesc <- maybe (return "") targetDescLeader mleader
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
                  toTr = map mkKM $ words to
              in if [fromTr] == toTr
                 then assert `failure` "degenerate alias" `twith` toTr
                 else (fromTr, toTr)
            section = ConfigIO.getItems cp "macros"
        in map trMacro section
      configMacroDesc =
        map (mkKM *** T.pack) $ ConfigIO.getItems cp "macro descriptions"
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
      vInitial = PointArray.replicateA lxsize lysize maxBound
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
  -- We treat doors as an open tile and don't add an extra step for opening
  -- the doors, because other actors open and use them, too,
  -- so it's amortized. We treat unknown tiles specially.
  -- TODO: Sometimes treat hidden tiles as possibly open?
  let unknownId = ouniqGroup "unknown space"
      chAccess = checkAccess cops lvl
      chDoorAccess = checkDoorAccess cops lvl
      conditions = catMaybes [chAccess, chDoorAccess]
      -- Legality of move from a known tile, assuming doors freely openable.
      isEnterable :: Point -> Point -> MoveLegal
      isEnterable spos tpos =
        let tt = lvl `at` tpos
        in if Tile.isPassable cotile tt
           then if all (\f -> f spos tpos) conditions
                then if tt == unknownId
                     then MoveToUnknown
                     else MoveToOpen
                else MoveBlocked
           else MoveBlocked
      -- Legality of move from an unknown tile, assuming unknown are open.
      passUnknown :: Point -> Point -> Bool
      passUnknown = case chAccess of  -- spos is unknown, so not a door
        Nothing -> \_ tpos -> lvl `at` tpos == unknownId
        Just ch -> \spos tpos -> lvl `at` tpos == unknownId
                                 && ch spos tpos
  fAnything isEnterable passUnknown aid

accessCacheBfs :: MonadClient m => ActorId -> Point -> m (Maybe Int)
{-# INLINE accessCacheBfs #-}
accessCacheBfs aid target = do
  bfs <- getCacheBfs aid
  return $! accessBfs bfs target

actorAimsPos :: MonadClient m => ActorId -> Point -> m Bool
{-# INLINE actorAimsPos #-}
actorAimsPos aid target = do
  bfs <- getCacheBfs aid
  b <- getsState $ getActorBody aid
  return $! posAimsPos bfs (bpos b) target

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
              validMsg (p, _) = "shift to" <+> (T.pack . show) p
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
      factionD <- getsState sfactionD
      let fact = factionD EM.! side
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

-- | Calculate the position of leader's target and whether one is permitted
-- to shoot it (this is only checked for actors; positions let player
-- shoot at obstacles, e.g., to destroy them). Perception is not enough
-- for the check, because the target actor can be obscured by a glass wall
-- or be out of sight range, but in weapon range.
aidTgtToPos :: MonadClient m
            => ActorId -> LevelId -> Maybe Target -> m (Maybe (Point, Bool))
aidTgtToPos aid lidV target = do
  Level{lxsize, lysize} <- getLevel lidV
  b <- getsState $ getActorBody aid
  case target of
    Just (TEnemy a _) -> do
      body <- getsState $ getActorBody a
      if blid body == lidV then do
        let pos = bpos body
        if blid b == lidV then do
          aims <- actorAimsPos aid pos
          return $ Just (pos, aims)
        else return $ Just (pos, False)
      else return Nothing
    Just (TEnemyPos _ lid p _) ->
      return $! if lid == lidV then Just (p, False) else Nothing
    Just (TPoint lid p) ->
      return $! if lid == lidV then Just (p, True) else Nothing
    Just (TVector v) -> do
      let shifted = shiftBounded lxsize lysize (bpos b) v
      return $! if shifted == bpos b && v /= Vector 0 0
                then Nothing
                else Just (shifted, True)
    Nothing -> do
      scursor <- getsClient scursor
      aidTgtToPos aid lidV $ Just scursor

-- TODO: don't point at Enemy you don't see, or you shoot at wall
-- there is a discrepancy between / and * in aquiring unseen Enemy targets
targetToPos :: MonadClientUI m => m (Maybe (Point, Bool))
targetToPos = do
  lidV <- viewedLevel
  mleader <- getsClient _sleader
  case mleader of
    Nothing -> return Nothing
    Just aid -> do
      target <- getsClient $ getTarget aid
      aidTgtToPos aid lidV target

cursorToPos :: MonadClientUI m => m (Maybe Point)
cursorToPos = do
  lidV <- viewedLevel
  mleader <- getsClient _sleader
  scursor <- getsClient scursor
  case mleader of
    Nothing -> return Nothing
    Just aid -> fmap (fmap fst) $ aidTgtToPos aid lidV $ Just scursor

closestUnknown :: MonadClient m => ActorId -> m (Maybe Point)
closestUnknown aid = do
  bfs <- getCacheBfs aid
  let closestPos = PointArray.minIndexA bfs
      dist = bfs PointArray.! closestPos
  return $ if dist >= minKnown
           then Nothing
           else Just closestPos

closestItems :: MonadClient m => ActorId -> m ([(Int, (Point, ItemBag))])
closestItems aid = do
  body <- getsState $ getActorBody aid
  Level{lfloor} <- getLevel $ blid body
  let items = EM.keys lfloor
  case items of
    [] -> return []
    _ -> do
      bfs <- getCacheBfs aid
      let ds = mapMaybe (\x@(p, _) -> fmap (,x) (accessBfs bfs p))
                        $ EM.assocs lfloor
      return $! dropWhile (\(d, _) -> d == 0) $ sort ds

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
      return $! sort ds  -- no foes possible at distance 0
