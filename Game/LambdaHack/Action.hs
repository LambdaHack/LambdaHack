{-# LANGUAGE OverloadedStrings #-}
-- | Game action monads and basic building blocks for player and monster
-- actions. Has no access to the the main action type @Action@.
-- Does not export the @liftIO@ operation nor a few other implementation
-- details.
module Game.LambdaHack.Action
  ( -- * Action monads
    MonadActionRoot
  , MonadServerRO( getGlobal, getsGlobal, getServer, getsServer )
  , MonadClientRO( getClient, getsClient, getLocal, getsLocal )
  , MonadServer( putGlobal, modifyGlobal, putServer, modifyServer )
  , MonadClient( putClient, modifyClient, putLocal, modifyLocal )
  , MonadServerChan
  , MonadClientChan
    -- * Various ways to abort action
  , abort, abortWith, abortIfWith, neverMind
    -- * Abort exception handlers
  , tryWith, tryRepeatedlyWith, tryIgnore, tryWithSlide
    -- * Accessors to the game session Reader and the Perception Reader
  , askBinding, askConfigUI, askPerception, askPerceptionSer
    -- * History and report
  , msgAdd, recordHistory
    -- * Key input
  , getKeyCommand, getKeyOverlayCommand, getManyConfirms
    -- * Display and key input
  , displayFramesPush, displayMore, displayYesNo, displayChoiceUI
    -- * Generate slideshows
  , promptToSlideshow, overlayToSlideshow
    -- * Draw frames
  , drawOverlay
    -- * Turn init operations
  , withPerception, remember, rememberLevel, displayPush
    -- * Assorted primitives
  , saveGameBkp, dumpCfg, endOrLoop, frontendName, startFrontend
  , switchGlobalSelectedSide
  , debug
  , sendToPlayers, sendToClients, sendToClient, askClient, sendToPl
  , respondCli, readChanSer, writeChanSer, readChanCli
  ) where

import Control.Concurrent
import Control.Exception (finally)
import Control.Monad
import Control.Monad.Reader.Class
import qualified Control.Monad.State as St
import Control.Monad.Writer.Strict (WriterT, tell, lift)
import Data.Dynamic
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import System.Time
-- import System.IO (hPutStrLn, stderr) -- just for debugging

import qualified Game.LambdaHack.Action.ConfigIO as ConfigIO
import Game.LambdaHack.Action.Frontend
import Game.LambdaHack.Action.HighScore (register)
import qualified Game.LambdaHack.Action.Save as Save
import Game.LambdaHack.ActionClass
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Animation (SingleFrame, Frames)
import Game.LambdaHack.Binding
import Game.LambdaHack.Command
import Game.LambdaHack.Config
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Draw
import qualified Game.LambdaHack.DungeonState as DungeonState
import Game.LambdaHack.Faction
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Key as K
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Utils.Assert

-- | Reset the state and resume from the last backup point, i.e., invoke
-- the failure continuation.
abort :: MonadActionRoot m => m a
abort = abortWith ""

-- | Abort and print the given msg if the condition is true.
abortIfWith :: MonadActionRoot m => Bool -> Msg -> m a
abortIfWith True msg = abortWith msg
abortIfWith False _  = abortWith ""

-- | Abort and conditionally print the fixed message.
neverMind :: MonadActionRoot m => Bool -> m a
neverMind b = abortIfWith b "never mind"

-- | Take a handler and a computation. If the computation fails, the
-- handler is invoked and then the computation is retried.
tryRepeatedlyWith :: MonadActionRoot m => (Msg -> m ()) -> m () -> m ()
tryRepeatedlyWith exc m =
  tryWith (\msg -> exc msg >> tryRepeatedlyWith exc m) m

-- | Try the given computation and silently catch failure.
tryIgnore :: MonadActionRoot m => m () -> m ()
tryIgnore =
  tryWith (\msg -> if T.null msg
                   then return ()
                   else assert `failure` msg <+> "in tryIgnore")

-- | Set the current exception handler. Apart of executing it,
-- draw and pass along a slide with the abort message (even if message empty).
tryWithSlide :: MonadClient m
             => m a -> WriterT Slideshow m a -> WriterT Slideshow m a
tryWithSlide exc h =
  let excMsg msg = do
        msgReset ""
        slides <- promptToSlideshow msg
        tell slides
        lift exc
  in tryWith excMsg h

-- | Get the frontend session.
askFrontendSession :: MonadClientRO m => m FrontendSession
askFrontendSession = getsSession sfs

-- | Get the key binding.
askBinding :: MonadClientRO m => m Binding
askBinding = getsSession sbinding

-- | Get the config from the config file.
askConfigUI :: MonadClientRO m => m ConfigUI
askConfigUI = getsSession sconfigUI

-- | Add a message to the current report.
msgAdd :: MonadClient m => Msg -> m ()
msgAdd msg = modifyClient $ \d -> d {sreport = addMsg (sreport d) msg}

-- | Wipe out and set a new value for the current report.
msgReset :: MonadClient m => Msg -> m ()
msgReset msg = modifyClient $ \d -> d {sreport = singletonReport msg}

-- | Store current report in the history and reset report.
recordHistory :: MonadClient m => m ()
recordHistory = do
  StateClient{sreport, shistory} <- getClient
  unless (nullReport sreport) $ do
    ConfigUI{configHistoryMax} <- askConfigUI
    msgReset ""
    let nhistory = takeHistory configHistoryMax $! addReport sreport shistory
    modifyClient $ \cli -> cli {shistory = nhistory}

-- | Update the cached perception for the selected level, for all factions,
-- for the given computation. The assumption is the level, and only the level,
-- has changed since the previous perception calculation.
withPerception :: MonadServerRO m => m () -> m ()
withPerception m = do
  cops <- getsGlobal scops
  sconfig <- getsServer sconfig
  sdebugSer <- getsServer sdebugSer
  lvl <- getsGlobal getArena
  arena <- getsGlobal sarena
  let per side = levelPerception cops sconfig (stryFov sdebugSer) side lvl
  local (IM.mapWithKey (\side lp -> M.insert arena (per side) lp)) m

-- | Get the current perception of a client.
askPerception :: MonadClientRO m => m Perception
askPerception = do
  stgtMode <- getsClient stgtMode
  arena <- getsLocal sarena
  let lid = maybe arena tgtLevelId stgtMode
  factionPers <- getsClient sper
  return $! factionPers M.! lid

-- | Get the current perception of the server.
askPerceptionSer :: MonadServerRO m => m Perception
askPerceptionSer = do
  lid <- getsGlobal sarena
  pers <- ask
  side <- getsGlobal sside
  return $! pers IM.! side M.! lid

-- | Wait for a player command.
getKeyCommand :: (MonadActionIO m, MonadClientRO m) => Maybe Bool -> m K.KM
getKeyCommand doPush = do
  fs <- askFrontendSession
  keyb <- askBinding
  (nc, modifier) <- liftIO $ nextEvent fs doPush
  return $! case modifier of
    K.NoModifier -> (fromMaybe nc $ M.lookup nc $ kmacro keyb, modifier)
    _ -> (nc, modifier)

-- | Display an overlay and wait for a player command.
getKeyOverlayCommand :: (MonadActionIO m, MonadClientRO m)
                     => Overlay
                     -> m K.KM
getKeyOverlayCommand overlay = do
  frame <- drawOverlay ColorFull overlay
  fs <- askFrontendSession
  keyb <- askBinding
  (nc, modifier) <- liftIO $ promptGetKey fs [] frame
  return $! case modifier of
    K.NoModifier -> (fromMaybe nc $ M.lookup nc $ kmacro keyb, modifier)
    _ -> (nc, modifier)

-- | Ignore unexpected kestrokes until a SPACE or ESC is pressed.
getConfirm :: (MonadActionIO m, MonadClientRO m)
           => [K.KM] -> SingleFrame
           -> m Bool
getConfirm clearKeys frame = do
  fs <- askFrontendSession
  let keys = [(K.Space, K.NoModifier), (K.Esc, K.NoModifier)] ++ clearKeys
  km <- liftIO $ promptGetKey fs keys frame
  case km of
    (K.Space, K.NoModifier) -> return True
    _ | km `elem` clearKeys -> return True
    _ -> return False

-- | Display a slideshow, awaiting confirmation for each slide.
getManyConfirms :: (MonadActionIO m, MonadClientRO m)
                => [K.KM] -> Slideshow
                -> m Bool
getManyConfirms clearKeys slides =
  case runSlideshow slides of
    [] -> return True
    x : xs -> do
      frame <- drawOverlay ColorFull x
      b <- getConfirm clearKeys frame
      if b
        then getManyConfirms clearKeys (toSlideshow xs)
        else return False

-- | Push frames or frame's worth of delay to the frame queue.
displayFramesPush :: (MonadActionIO m, MonadClientRO m) => Frames -> m ()
displayFramesPush frames = do
  fs <- askFrontendSession
  liftIO $ mapM_ (displayFrame fs False) frames

-- | A yes-no confirmation.
getYesNo :: (MonadActionIO m, MonadClientRO m) => SingleFrame -> m Bool
getYesNo frame = do
  fs <- askFrontendSession
  let keys = [ (K.Char 'y', K.NoModifier)
             , (K.Char 'n', K.NoModifier)
             , (K.Esc, K.NoModifier)
             ]
  (k, _) <- liftIO $ promptGetKey fs keys frame
  case k of
    K.Char 'y' -> return True
    _          -> return False

-- | Display a msg with a @more@ prompt. Return value indicates if the player
-- tried to cancel/escape.
displayMore :: (MonadActionIO m, MonadClientRO m) => ColorMode -> Msg -> m Bool
displayMore dm prompt = do
  sli <- promptToSlideshow $ prompt <+> moreMsg
  frame <- drawOverlay dm $ head $ runSlideshow sli
  getConfirm [] frame

-- | Print a yes/no question and return the player's answer. Use black
-- and white colours to turn player's attention to the choice.
displayYesNo :: (MonadActionIO m, MonadClientRO m) => Msg -> m Bool
displayYesNo prompt = do
  sli <- promptToSlideshow $ prompt <+> yesnoMsg
  frame <- drawOverlay ColorBW $ head $ runSlideshow sli
  getYesNo frame

-- TODO: generalize getManyConfirms and displayChoiceUI to a single op
-- | Print a prompt and an overlay and wait for a player keypress.
-- If many overlays, scroll screenfuls with SPACE. Do not wrap screenfuls
-- (in some menus @?@ cycles views, so the user can restart from the top).
displayChoiceUI :: (MonadActionIO m, MonadClientRO m)
                => Msg -> Overlay -> [K.KM]
                -> m K.KM
displayChoiceUI prompt ov keys = do
  slides <- fmap runSlideshow $ overlayToSlideshow (prompt <> ", ESC]") ov
  fs <- askFrontendSession
  let legalKeys = (K.Space, K.NoModifier) : (K.Esc, K.NoModifier) : keys
      loop [] = neverMind True
      loop (x : xs) = do
        frame <- drawOverlay ColorFull x
        (key, modifier) <- liftIO $ promptGetKey fs legalKeys frame
        case key of
          K.Esc -> neverMind True
          K.Space -> loop xs
          _ -> return (key, modifier)
  loop slides

-- | The prompt is shown after the current message, but not added to history.
-- This is useful, e.g., in targeting mode, not to spam history.
promptToSlideshow :: MonadClientRO m => Msg -> m Slideshow
promptToSlideshow prompt = overlayToSlideshow prompt []

-- | The prompt is shown after the current message at the top of each slide.
-- Together they may take more than one line. The prompt is not added
-- to history. The portions of overlay that fit on the the rest
-- of the screen are displayed below. As many slides as needed are shown.
overlayToSlideshow :: MonadClientRO m => Msg -> Overlay -> m Slideshow
overlayToSlideshow prompt overlay = do
  lysize <- getsLocal (lysize . getArena)  -- TODO: screen length or viewLevel
  StateClient{sreport} <- getClient
  let msg = splitReport (addMsg sreport prompt)
  return $! splitOverlay lysize msg overlay

-- | Draw the current level with the overlay on top.
drawOverlay :: MonadClientRO m => ColorMode -> Overlay -> m SingleFrame
drawOverlay dm over = do
  cops <- getsLocal scops
  per <- askPerception
  cli <- getClient
  loc <- getLocal
  return $! draw dm cops per cli loc over

-- -- | Draw the current level using server data, for debugging.
-- drawOverlayDebug :: MonadServerRO m
--                  => ColorMode -> Overlay -> m SingleFrame
-- drawOverlayDebug dm over = do
--   cops <- getsLocal scops
--   per <- askPerception
--   cli <- getClient
--   glo <- getGlobal
--   return $! draw dm cops per cli glo over

-- | Push the frame depicting the current level to the frame queue.
-- Only one screenful of the report is shown, the rest is ignored.
displayPush :: (MonadActionIO m, MonadClientRO m) => m ()
displayPush = do
  fs <- askFrontendSession
  sls <- promptToSlideshow ""
  let slide = head $ runSlideshow sls
--  DebugModeCli{somniscient} <- getsClient sdebugCli
  frame <- drawOverlay ColorFull slide
--  frame <- if somniscient
--           then drawOverlayDebug ColorFull slide
--           else drawOverlay ColorFull slide
  -- Visually speed up (by remving all empty frames) the show of the sequence
  -- of the move frames if the player is running.
  srunning <- getsClient srunning
  liftIO $ displayFrame fs (isJust srunning) $ Just frame

-- | Update all factions memory of the current level.
--
-- This has to be strict wrt map operation sor we leak one perception
-- per turn. This has to lazy wrt the perception sets or we compute them
-- for factions that do not move, perceive or not even reside on the level.
-- When clients and server communicate via network the communication
-- has to be explicitely lazy and multiple updates have to collapsed
-- when sending is forced by the server asking a client to perceive
-- something or to act.
remember :: MonadServerChan m => m ()
remember = do
  arena <- getsGlobal sarena
  lvl <- getsGlobal getArena
  faction <- getsGlobal sfaction
  pers <- ask
  let f fid =
        let per = pers IM.! fid M.! arena
        in RememberPerCli arena per lvl faction
  sendToClients f

-- | Update faction memory at the given set of positions.
rememberLevel :: Kind.COps -> IS.IntSet -> Level -> Level -> Level
rememberLevel Kind.COps{cotile=cotile@Kind.Ops{ouniqGroup}} visible lvl clvl =
  -- TODO: handle invisible actors, but then change also sendToPlayers, etc.
  let nactor = IM.filter (\m -> bpos m `IS.member` visible) (lactor lvl)
      ninv   = IM.filterWithKey (\p _ -> p `IM.member` nactor) (linv lvl)
      alt Nothing   _ = Nothing
      alt (Just []) _ = assert `failure` lvl
      alt x         _ = x
      rememberItem p m = IM.alter (alt $ IM.lookup p $ litem lvl) p m
      vis = IS.toList visible
      rememberTile = [(pos, lvl `at` pos) | pos <- vis]
      unknownId = ouniqGroup "unknown space"
      eSeen (pos, tk) = clvl `at` pos == unknownId
                        && Tile.isExplorable cotile tk
      extraSeen = length $ filter eSeen rememberTile
  in clvl { lactor = nactor
          , linv = ninv
          , litem = foldr rememberItem (litem clvl) vis
          , ltile = ltile clvl Kind.// rememberTile
  -- TODO: update enemy smell probably only around a sniffing party member
          , lsmell = lsmell lvl
          , lseen = lseen clvl + extraSeen
          , ltime = ltime lvl
  -- TODO: let factions that spawn see hidden features and open all hidden
  -- doors (they built and hid them). Hide the Hidden feature in ltile.
  -- Wait with all that until the semantics of (repeated) searching
  -- is changed.
          , lsecret = IM.empty
          }

-- | Save the history and a backup of the save game file, in case of crashes.
--
-- See 'Save.saveGameBkp'.
saveGameBkp :: MonadServer m => m ()
saveGameBkp = do
  state <- getGlobal
  ser <- getServer
  d <- undefined -- TODO
--  configUI <- askConfigUI
  config <- getsServer sconfig
  liftIO $ Save.saveGameBkp config state ser d

-- | Dumps the current game rules configuration to a file.
dumpCfg :: (MonadActionIO m, MonadServerRO m) => FilePath -> m ()
dumpCfg fn = do
  config <- getsServer sconfig
  liftIO $ ConfigIO.dump config fn

-- | Handle current score and display it with the high scores.
-- Aborts if display of the scores was interrupted by the user.
--
-- Warning: scores are shown during the game,
-- so we should be careful not to leak secret information through them
-- (e.g., the nature of the items through the total worth of inventory).
handleScores :: (MonadActionIO m, MonadServerChan m)
             => Bool -> Status -> Int
             -> m ()
handleScores write status total =
  when (total /= 0) $ do
    config <- getsServer sconfig
    time <- getsGlobal getTime
    curDate <- liftIO getClockTime
    slides <-
      liftIO $ register config write total time curDate status
    side <- getsGlobal sside
    go <- askClient side $ ShowSlidesCli slides
    when (not go) abort

-- | Continue or restart or exit the game.
endOrLoop :: MonadServerChan m => m () -> m ()
endOrLoop handleTurn = do
  squit <- getsServer squit
  side <- getsGlobal sside
  gquit <- getsGlobal $ gquit . (IM.! side) . sfaction
  s <- getGlobal
  ser <- getServer
  d <- undefined -- getDict
  config <- getsServer sconfig
  let (_, total) = calculateTotal s
  -- The first, boolean component of squit determines
  -- if ending screens should be shown, the other argument describes
  -- the cause of the disruption of game flow.
  case (squit, gquit) of
    (Just _, _) -> do
      -- Save and display in parallel.
      mv <- liftIO newEmptyMVar
      liftIO $ void $ forkIO (Save.saveGameFile config s ser d
                              `finally` putMVar mv ())
      tryIgnore $ do
        handleScores False Camping total
        sendToPl [] $ ConfirmMoreFullCli "See you soon, stronger and braver!"
      liftIO $ takeMVar mv  -- wait until saved
      -- Do nothing, that is, quit the game loop.
    (Nothing, Just (showScreens, status@Killed{})) -> do
      nullR <- askClient side NullReportCli
      unless nullR $ do
        -- Sisplay any leftover report. Suggest it could be the cause of death.
        sendToPl [] $ ConfirmMoreBWCli "Who would have thought?"
      tryWith
        (\ finalMsg ->
          let highScoreMsg = "Let's hope another party can save the day!"
              msg = if T.null finalMsg then highScoreMsg else finalMsg
          in sendToPl [] $ ConfirmMoreBWCli msg
          -- Do nothing, that is, quit the game loop.
        )
        (do
           when showScreens $ handleScores True status total
           go <- askClient side
                 $ ConfirmMoreBWCli "Next time will be different."
           when (not go) $ abortWith "You could really win this time."
           restartGame handleTurn
        )
    (Nothing, Just (showScreens, status@Victor)) -> do
      nullR <- askClient side NullReportCli
      unless nullR $ do
        -- Sisplay any leftover report. Suggest it could be the master move.
        sendToPl [] $ ConfirmMoreFullCli "Brilliant, wasn't it?"
      when showScreens $ do
        tryIgnore $ handleScores True status total
        sendToPl [] $ ConfirmMoreFullCli "Can it be done better, though?"
      restartGame handleTurn
    (Nothing, Just (_, Restart)) -> do
      sendToPl [] $ ConfirmMoreBWCli "This time for real."
      restartGame handleTurn
    (Nothing, _) -> handleTurn  -- just continue

restartGame :: MonadServer m => m () -> m ()
restartGame handleTurn = do
  -- Take the original config from config file, to reroll RNG, if needed
  -- (the current config file has the RNG rolled for the previous game).
  cops <- getsGlobal scops
  shistory <- undefined -- getsClient shistory
  (state, ser, dMsg) <- gameResetAction cops
  let d = case filter (isPlayerFaction state) $ IM.keys dMsg of
        [] -> dMsg  -- only robots play
        k : _ -> IM.adjust (\(cli, loc) -> (cli {shistory}, loc)) k dMsg
  putGlobal state
  putServer ser
  -- TODO: send to each client RestartCli; use d in its code; empty channels?
  saveGameBkp
  handleTurn

-- TODO: do this inside Action ()
gameReset :: Kind.COps -> IO (State, StateServer, StateDict)
gameReset cops@Kind.COps{ cofact=Kind.Ops{opick, ofoldrWithKey}
                                  , coitem=coitem@Kind.Ops{okind}
                                  , corule
                                  , costrat=Kind.Ops{opick=sopick}} = do
  -- Rules config reloaded at each new game start.
  (sconfig, dungeonGen, srandom) <- ConfigIO.mkConfigRules corule
  let rnd = do
        sflavour <- dungeonFlavourMap coitem
        (sdiscoS, sdiscoRev) <- serverDiscos coitem
        let f ik = isymbol (okind ik)
                   `notElem` (ritemProject $ Kind.stdRuleset corule)
            disco = M.filter f sdiscoS
        DungeonState.FreshDungeon{..} <-
          DungeonState.generate cops sflavour sdiscoRev sconfig
        let factionName = configFaction sconfig
        playerFactionKindId <- opick factionName (const True)
        let g gkind fk mk = do
              (m, k) <- mk
              let gname = fname fk
                  genemy = fenemy fk
                  gally = fally fk
              gAiSelected <-
                if gkind == playerFactionKindId
                then return Nothing
                else fmap Just $ sopick (fAiSelected fk) (const True)
              gAiIdle <- sopick (fAiIdle fk) (const True)
              let gquit = Nothing
              return (IM.insert k Faction{..} m, k + 1)
        faction <- fmap fst $ ofoldrWithKey g (return (IM.empty, 0))
        let defState =
              defStateGlobal freshDungeon freshDepth sdiscoS faction
                             cops entryLevel
            defSer = defStateServer sdiscoRev sflavour srandom sconfig
            needInitialCrew =
              filter (not . isSpawningFaction defState) $ IM.keys faction
            fo fid (gloF, serF) =
              initialHeroes cops entryLoc fid gloF serF
            (glo, ser) = foldr fo (defState, defSer) needInitialCrew
            defCli = defStateClient entryLoc
            -- This overwrites the "Really save/quit?" messages.
            defLoc = defStateLocal freshDungeon freshDepth
                                   disco faction cops entryLevel
            dHist = IM.mapWithKey (\fid _ -> (defCli, defLoc fid)) faction
            pers = dungeonPerception cops sconfig (sdebugSer ser) glo
            d = IM.mapWithKey (\side (cli, loc) ->
                                  (cli {sper = pers IM.! side}, loc)) dHist
        return (glo, ser, d)
  return $! St.evalState rnd dungeonGen

gameResetAction :: MonadActionIO m
                => Kind.COps
                -> m (State, StateServer, StateDict)
gameResetAction = liftIO . gameReset

-- | Wire together content, the definitions of game commands,
-- config and a high-level startup function
-- to form the starting game session. Evaluate to check for errors,
-- in particular verify content consistency.
-- Then create the starting game config from the default config file
-- and initialize the engine with the starting session.
startFrontend :: (MonadActionRoot m, MonadActionRoot n)
              => (m () -> Pers -> State -> StateServer -> ClientDict -> IO ())
                 -> (n () -> FrontendSession -> Binding -> ConfigUI
                     -> State -> StateClient -> ClientChan -> IO ())
              -> Kind.COps -> m () -> n () -> IO ()
startFrontend executor executorCli
              !copsSlow@Kind.COps{corule, cotile=tile}
              handleTurn handleClient = do
  -- Compute and insert auxiliary optimized components into game content,
  -- to be used in time-critical sections of the code.
  let ospeedup = Tile.speedup tile
      cotile = tile {Kind.ospeedup}
      cops = copsSlow {Kind.cotile}
  -- UI config reloaded at each client start.
  sconfigUI <- ConfigIO.mkConfigUI corule
  -- A throw-away copy of rules config reloaded at client start, too,
  -- until an old version of the config can be read from the savefile.
  (sconfig, _, _) <- ConfigIO.mkConfigRules corule
  let !sbinding = stdBinding sconfigUI
      font = configFont sconfigUI
      -- In addition to handling the turn, if the game ends or exits,
      -- handle the history and backup savefile.
      handleServer = do
        handleTurn
--        d <- undefined -- getDict
        -- Save history often, at each game exit, in case of crashes.
--        liftIO $ Save.rmBkpSaveHistory sconfig sconfigUI d
      loop sfs = start executor executorCli
                       sfs cops sbinding sconfig sconfigUI
                       handleServer handleClient
  startup font loop

-- | Either restore a saved game, or setup a new game.
-- Then call the main game loop.
start :: (MonadActionRoot m, MonadActionRoot n)
      => (m () -> Pers -> State -> StateServer -> ClientDict -> IO ())
      -> (n () -> FrontendSession -> Binding -> ConfigUI
          -> State -> StateClient -> ClientChan -> IO ())
      -> FrontendSession -> Kind.COps -> Binding -> Config -> ConfigUI
      -> m () -> n () -> IO ()
start executor executorCli sfs cops@Kind.COps{corule}
      sbinding sconfig sconfigUI handleServer handleClient = do
  let title = rtitle $ Kind.stdRuleset corule
      pathsDataFile = rpathsDataFile $ Kind.stdRuleset corule
  restored <- Save.restoreGame sconfig sconfigUI pathsDataFile title
  (glo, ser, dBare, shistory, msg) <- case restored of
    Right (shistory, msg) -> do  -- Starting a new game.
      (gloR, serR, dR) <- gameReset cops
      return (gloR, serR, dR, shistory, msg)
    Left (gloL, serL, dL, shistory, msg) -> do  -- Running a restored game.
      let gloCops = updateCOps (const cops) gloL
          dCops = IM.map (\(cli, loc) -> (cli, updateCOps (const cops) loc)) dL
      return (gloCops, serL, dCops, shistory, msg)
  let singMsg (cli, loc) = (cli {sreport = singletonReport msg}, loc)
      dMsg = IM.map singMsg dBare
      dHist = case filter (isPlayerFaction glo) $ IM.keys dMsg of
        [] -> dMsg  -- only robots play
        k : _ -> IM.adjust (\(cli, loc) -> (cli {shistory}, loc)) k dMsg
      pers = dungeonPerception cops sconfig (sdebugSer ser) glo
      dPer = IM.mapWithKey (\side (cli, loc) ->
                               (cli {sper = pers IM.! side}, loc)) dHist
      addChan (k, cliloc) = do
        toClient <- newChan
        toServer <- newChan
        return (k, (cliloc, ClientChan {toClient, toServer}))
  dAssocs <- mapM addChan $ IM.toAscList dPer
  let d = IM.map snd $ IM.fromAscList dAssocs
      forkClient (_, ((cli, loc), chan)) =
        forkIO $ executorCli handleClient sfs sbinding sconfigUI loc cli chan
  mapM_ forkClient dAssocs
  executor handleServer pers glo ser d

switchGlobalSelectedSide :: MonadServer m => FactionId -> m ()
switchGlobalSelectedSide =
  modifyGlobal . switchGlobalSelectedSideOnlyForGlobalState

-- | Debugging.
debug :: MonadActionRoot m => Text -> m ()
debug _x = return () -- liftIO $ hPutStrLn stderr _x

sendToPlayers :: MonadServerChan m => [Point] -> CmdCli -> m ()
sendToPlayers poss cmd = do
  arena <- getsGlobal sarena
  glo <- getGlobal
  let f (cfid, perF) =
        when (isPlayerFaction glo cfid) $ do
          let perceived =
                null poss
                || any (`IS.member` totalVisible (perF M.! arena)) poss
          when perceived $ sendToClient cfid cmd
  pers <- ask
  mapM_ f $ IM.toList pers

sendToPl :: MonadServerChan m => [Point] -> CmdCli -> m ()
sendToPl poss cmd = do
  sendToPlayers poss cmd

sendToClients :: MonadServerChan m => (FactionId -> CmdCli) -> m ()
sendToClients cmd = do
  faction <- getsGlobal sfaction
  let f cfid = sendToClient cfid (cmd cfid)
  mapM_ f $ IM.keys faction

sendToClient :: MonadServerChan m => FactionId -> CmdCli -> m ()
sendToClient fid cmd = do
  ClientChan {toClient} <- getsDict (IM.! fid)
  liftIO $ writeChan toClient cmd

askClient :: (Typeable a, MonadServerChan m) => FactionId -> CmdCli -> m a
askClient fid cmd = do
  ClientChan {toClient, toServer} <- getsDict (IM.! fid)
  liftIO $ writeChan toClient cmd
  ResponseSer a <- liftIO $ readChan toServer
  return $ fromDyn a (assert `failure` (fid, cmd, a))

respondCli :: (Typeable a, MonadClientChan m) => a -> m ()
respondCli a = do
  toServer <- getsChan toServer
  liftIO $ writeChan toServer $ ResponseSer $ toDyn a

readChanSer :: MonadServerChan m => FactionId -> m CmdSer
readChanSer fid = do
  ClientChan {toServer} <- getsDict (IM.! fid)
  liftIO $ readChan toServer

writeChanSer :: MonadClientChan m => CmdSer -> m ()
writeChanSer cmd = do
  toServer <- getsChan toServer
  liftIO $ writeChan toServer cmd

readChanCli :: MonadClientChan m => m CmdCli
readChanCli = do
  toClient <- getsChan toClient
  liftIO $ readChan toClient
