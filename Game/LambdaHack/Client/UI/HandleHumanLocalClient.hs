-- | Semantics of 'HumanCmd' client commands that do not return
-- server commands. None of such commands takes game time.
-- TODO: document
module Game.LambdaHack.Client.UI.HandleHumanLocalClient
  ( -- * Assorted commands
    gameDifficultyCycle
  , pickLeaderHuman, memberCycleHuman, memberBackHuman
  , selectActorHuman, selectNoneHuman, clearHuman
  , stopIfTgtModeHuman, selectWithPointer, repeatHuman, recordHuman
  , historyHuman, markVisionHuman, markSmellHuman, markSuspectHuman
  , helpHuman, mainMenuHuman, macroHuman
    -- * Commands specific to targeting
  , moveCursorHuman, tgtFloorHuman, tgtEnemyHuman
  , tgtAscendHuman, epsIncrHuman, tgtClearHuman
  , cursorUnknownHuman, cursorItemHuman, cursorStairHuman
  , cancelHuman, acceptHuman
  , cursorPointerFloorHuman, cursorPointerEnemyHuman
  , tgtPointerFloorHuman, tgtPointerEnemyHuman
  ) where

-- Cabal
import qualified Paths_LambdaHack as Self (version)

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Text as T
import Data.Version
import Game.LambdaHack.Client.UI.Frontend (frontendName)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.BfsClient
import Game.LambdaHack.Client.CommonClient
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import Game.LambdaHack.Client.UI.InventoryClient
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.MsgClient
import Game.LambdaHack.Client.UI.WidgetClient
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Frequency
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.RuleKind
import qualified Game.LambdaHack.Content.TileKind as TK

-- * GameDifficultyCycle

gameDifficultyCycle :: MonadClientUI m => m ()
gameDifficultyCycle = do
  DebugModeCli{sdifficultyCli} <- getsClient sdebugCli
  let d = if sdifficultyCli >= difficultyBound then 1 else sdifficultyCli + 1
  modifyClient $ \cli -> cli {sdebugCli = (sdebugCli cli) {sdifficultyCli = d}}
  msgAdd $ "Next game difficulty set to" <+> tshow d <> "."

-- * PickLeader

pickLeaderHuman :: MonadClientUI m => Int -> m Slideshow
pickLeaderHuman k = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  arena <- getArenaUI
  mhero <- getsState $ tryFindHeroK side k
  allA <- getsState $ EM.assocs . sactorD
  let mactor = let factionA = filter (\(_, body) ->
                     not (bproj body) && bfid body == side) allA
                   hs = sortBy (comparing keySelected) factionA
               in case drop k hs of
                 [] -> Nothing
                 aidb : _ -> Just aidb
      mchoice = mhero `mplus` mactor
      (autoDun, autoLvl) = autoDungeonLevel fact
  case mchoice of
    Nothing -> failMsg "no such member of the party"
    Just (aid, b)
      | blid b == arena && autoLvl ->
          failMsg $ showReqFailure NoChangeLvlLeader
      | autoDun ->
          failMsg $ showReqFailure NoChangeDunLeader
      | otherwise -> do
          void $ pickLeader True aid
          return mempty

-- * MemberCycle

-- | Switches current member to the next on the level, if any, wrapping.
memberCycleHuman :: MonadClientUI m => m Slideshow
memberCycleHuman = memberCycle True

-- * MemberBack

-- | Switches current member to the previous in the whole dungeon, wrapping.
memberBackHuman :: MonadClientUI m => m Slideshow
memberBackHuman = memberBack True

-- * SelectActor

-- TODO: make the message (and for selectNoneHuman, pickLeader, etc.)
-- optional, since they have a clear representation in the UI elsewhere.
selectActorHuman :: MonadClientUI m => m ()
selectActorHuman = do
  leader <- getLeaderUI
  selectAidHuman leader

selectAidHuman :: MonadClientUI m => ActorId -> m ()
selectAidHuman leader = do
  body <- getsState $ getActorBody leader
  wasMemeber <- getsClient $ ES.member leader . sselected
  let upd = if wasMemeber
            then ES.delete leader  -- already selected, deselect instead
            else ES.insert leader
  modifyClient $ \cli -> cli {sselected = upd $ sselected cli}
  let subject = partActor body
  msgAdd $ makeSentence [subject, if wasMemeber
                                  then "deselected"
                                  else "selected"]

-- * SelectNone

selectNoneHuman :: (MonadClientUI m, MonadClient m) => m ()
selectNoneHuman = do
  side <- getsClient sside
  lidV <- viewedLevel
  oursAssocs <- getsState $ actorRegularAssocs (== side) lidV
  let ours = ES.fromList $ map fst oursAssocs
  oldSel <- getsClient sselected
  let wasNone = ES.null $ ES.intersection ours oldSel
      upd = if wasNone
            then ES.union  -- already all deselected; select all instead
            else ES.difference
  modifyClient $ \cli -> cli {sselected = upd (sselected cli) ours}
  let subject = "all party members on the level"
  msgAdd $ makeSentence [subject, if wasNone
                                  then "selected"
                                  else "deselected"]

-- * Clear

-- | Clear current messages, show the next screen if any.
clearHuman :: Monad m => m ()
clearHuman = return ()

-- * StopIfTgtMode

stopIfTgtModeHuman :: MonadClientUI m => m ()
stopIfTgtModeHuman = do
  tgtMode <- getsClient stgtMode
  when (isJust tgtMode) stopPlayBack

-- * SelectWithPointer

selectWithPointer:: MonadClientUI m => m ()
selectWithPointer = do
  km <- getsClient slastKM
  let Point{..} = K.pointer km
  lidV <- viewedLevel
  Level{lysize} <- getLevel lidV
  side <- getsClient sside
  ours <- getsState $ filter (not . bproj . snd)
                      . actorAssocs (== side) lidV
  -- Select even if no space in status line for the actor's symbol.
  let viewed = sortBy (comparing keySelected) ours
  when (py == lysize + 1 && px <= length viewed && px >= 0) $ do
    if px == 0 then
      selectNoneHuman
    else
      selectAidHuman $ fst $ viewed !! (px - 1)
    stopPlayBack

-- * Repeat

-- Note that walk followed by repeat should not be equivalent to run,
-- because the player can really use a command that does not stop
-- at terrain change or when walking over items.
repeatHuman :: MonadClient m => Int -> m ()
repeatHuman n = do
  (_, seqPrevious, k) <- getsClient slastRecord
  let macro = concat $ replicate n $ reverse seqPrevious
  modifyClient $ \cli -> cli {slastPlay = macro ++ slastPlay cli}
  let slastRecord = ([], [], if k == 0 then 0 else maxK)
  modifyClient $ \cli -> cli {slastRecord}

maxK :: Int
maxK = 100

-- * Record

recordHuman :: MonadClientUI m => m Slideshow
recordHuman = do
  (_seqCurrent, seqPrevious, k) <- getsClient slastRecord
  case k of
    0 -> do
      let slastRecord = ([], [], maxK)
      modifyClient $ \cli -> cli {slastRecord}
      promptToSlideshow $ "Macro will be recorded for up to"
                          <+> tshow maxK <+> "actions."  -- no MU, poweruser
    _ -> do
      let slastRecord = (seqPrevious, [], 0)
      modifyClient $ \cli -> cli {slastRecord}
      promptToSlideshow $ "Macro recording interrupted after"
                          <+> tshow (maxK - k - 1) <+> "actions."

-- * History

historyHuman :: MonadClientUI m => m Slideshow
historyHuman = do
  history <- getsClient shistory
  arena <- getArenaUI
  local <- getsState $ getLocalTime arena
  global <- getsState stime
  let turnsGlobal = global `timeFitUp` timeTurn
      turnsLocal = local `timeFitUp` timeTurn
      msg = makeSentence
        [ "You survived for"
        , MU.CarWs turnsGlobal "half-second turn"
        , "(this level:"
        , MU.Text (tshow turnsLocal) <> ")" ]
        <+> "Past messages:"
  overlayToBlankSlideshow False msg $ renderHistory history

-- * MarkVision, MarkSmell, MarkSuspect

markVisionHuman :: MonadClientUI m => m ()
markVisionHuman = do
  modifyClient toggleMarkVision
  cur <- getsClient smarkVision
  msgAdd $ "Visible area display toggled" <+> if cur then "on." else "off."

markSmellHuman :: MonadClientUI m => m ()
markSmellHuman = do
  modifyClient toggleMarkSmell
  cur <- getsClient smarkSmell
  msgAdd $ "Smell display toggled" <+> if cur then "on." else "off."

markSuspectHuman :: MonadClientUI m => m ()
markSuspectHuman = do
  modifyClient toggleMarkSuspect
  cur <- getsClient smarkSuspect
  msgAdd $ "Suspect terrain display toggled" <+> if cur then "on." else "off."

-- * Help

-- | Display command help.
helpHuman :: MonadClientUI m => m Slideshow
helpHuman = do
  keyb <- askBinding
  return $! keyHelp keyb

-- * MainMenu

-- TODO: merge with the help screens better
-- | Display the main menu.
mainMenuHuman :: MonadClientUI m => m Slideshow
mainMenuHuman = do
  Kind.COps{corule} <- getsState scops
  escAI <- getsClient sescAI
  Binding{brevMap, bcmdList} <- askBinding
  scurDifficulty <- getsClient scurDifficulty
  DebugModeCli{sdifficultyCli} <- getsClient sdebugCli
  let stripFrame t = map (T.tail . T.init) $ tail . init $ T.lines t
      pasteVersion art =
        let pathsVersion = rpathsVersion $ Kind.stdRuleset corule
            version = " Version " ++ showVersion pathsVersion
                      ++ " (frontend: " ++ frontendName
                      ++ ", engine: LambdaHack " ++ showVersion Self.version
                      ++ ") "
            versionLen = length version
        in init art ++ [take (80 - versionLen) (last art) ++ version]
      kds =  -- key-description pairs
        let showKD cmd km = (K.showKM km, HumanCmd.cmdDescription cmd)
            revLookup cmd = maybe ("", "") (showKD cmd) $ M.lookup cmd brevMap
            cmds = [ (K.showKM km, desc)
                   | (km, (desc, [HumanCmd.CmdMenu], cmd)) <- bcmdList,
                     cmd /= HumanCmd.GameDifficultyCycle ]
        in [
             if escAI == EscAIMenu then
               (fst (revLookup HumanCmd.Automate), "back to screensaver")
             else
               (fst (revLookup HumanCmd.Cancel), "back to playing")
           , (fst (revLookup HumanCmd.Accept), "see more help")
           ]
           ++ cmds
           ++ [ (fst ( revLookup HumanCmd.GameDifficultyCycle)
                     , "next game difficulty"
                       <+> tshow sdifficultyCli
                       <+> "(current"
                       <+> tshow scurDifficulty <> ")" ) ]
      bindingLen = 25
      bindings =  -- key bindings to display
        let fmt (k, d) = T.justifyLeft bindingLen ' '
                         $ T.justifyLeft 7 ' ' k <> " " <> d
        in map fmt kds
      overwrite =  -- overwrite the art with key bindings
        let over [] line = ([], T.pack line)
            over bs@(binding : bsRest) line =
              let (prefix, lineRest) = break (=='{') line
                  (braces, suffix)   = span  (=='{') lineRest
              in if length braces == 25
                 then (bsRest, T.pack prefix <> binding
                               <> T.drop (T.length binding - bindingLen)
                                         (T.pack suffix))
                 else (bs, T.pack line)
        in snd . mapAccumL over bindings
      mainMenuArt = rmainMenuArt $ Kind.stdRuleset corule
      menuOverlay =  -- TODO: switch to Text and use T.justifyLeft
        overwrite $ pasteVersion $ map T.unpack $ stripFrame mainMenuArt
  case menuOverlay of
    [] -> assert `failure` "empty Main Menu overlay" `twith` mainMenuArt
    hd : tl -> overlayToBlankSlideshow True hd (toOverlay tl)
               -- TODO: keys don't work if tl/=[]

-- * Macro

macroHuman :: MonadClient m => [String] -> m ()
macroHuman kms =
  modifyClient $ \cli -> cli {slastPlay = map K.mkKM kms ++ slastPlay cli}

-- * MoveCursor

-- in InventoryClient

-- * TgtFloor

-- in InventoryClient

-- * TgtEnemy

-- in InventoryClient

-- * TgtAscend

-- | Change the displayed level in targeting mode to (at most)
-- k levels shallower. Enters targeting mode, if not already in one.
tgtAscendHuman :: MonadClientUI m => Int -> m Slideshow
tgtAscendHuman k = do
  Kind.COps{cotile=cotile@Kind.Ops{okind}} <- getsState scops
  dungeon <- getsState sdungeon
  scursorOld <- getsClient scursor
  cursorPos <- cursorToPos
  lidV <- viewedLevel
  lvl <- getLevel lidV
  let rightStairs = case cursorPos of
        Nothing -> Nothing
        Just cpos ->
          let tile = lvl `at` cpos
          in if Tile.hasFeature cotile (TK.Cause $ IK.Ascend k) tile
             then Just cpos
             else Nothing
  case rightStairs of
    Just cpos -> do  -- stairs, in the right direction
      (nln, npos) <- getsState $ whereTo lidV cpos k . sdungeon
      let !_A = assert (nln /= lidV `blame` "stairs looped" `twith` nln) ()
      nlvl <- getLevel nln
      -- Do not freely reveal the other end of the stairs.
      let ascDesc (TK.Cause (IK.Ascend _)) = True
          ascDesc _ = False
          scursor =
            if any ascDesc $ TK.tfeature $ okind (nlvl `at` npos)
            then TPoint nln npos  -- already known as an exit, focus on it
            else scursorOld  -- unknown, do not reveal
      modifyClient $ \cli -> cli {scursor, stgtMode = Just (TgtMode nln)}
      doLook False
    Nothing ->  -- no stairs in the right direction
      case ascendInBranch dungeon k lidV of
        [] -> failMsg "no more levels in this direction"
        nln : _ -> do
          modifyClient $ \cli -> cli {stgtMode = Just (TgtMode nln)}
          doLook False

-- * EpsIncr

-- in InventoryClient

-- * TgtClear

-- in InventoryClient

-- * CursorUnknown

cursorUnknownHuman :: MonadClientUI m => m Slideshow
cursorUnknownHuman = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  mpos <- closestUnknown leader
  case mpos of
    Nothing -> failMsg "no more unknown spots left"
    Just p -> do
      let tgt = TPoint (blid b) p
      modifyClient $ \cli -> cli {scursor = tgt}
      doLook False

-- * CursorItem

cursorItemHuman :: MonadClientUI m => m Slideshow
cursorItemHuman = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  items <- closestItems leader
  case items of
    [] -> failMsg "no more items remembered or visible"
    (_, (p, _)) : _ -> do
      let tgt = TPoint (blid b) p
      modifyClient $ \cli -> cli {scursor = tgt}
      doLook False

-- * CursorStair

cursorStairHuman :: MonadClientUI m => Bool -> m Slideshow
cursorStairHuman up = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  stairs <- closestTriggers (Just up) leader
  case maxFreq stairs of
    Nothing -> failMsg $ "no stairs" <+> if up then "up" else "down"
    Just p -> do
      let tgt = TPoint (blid b) p
      modifyClient $ \cli -> cli {scursor = tgt}
      doLook False

-- * Cancel

-- | Cancel something, e.g., targeting mode, resetting the cursor
-- to the position of the leader. Chosen target is not invalidated.
cancelHuman :: MonadClientUI m => m Slideshow -> m Slideshow
cancelHuman h = do
  stgtMode <- getsClient stgtMode
  if isJust stgtMode
    then targetReject
    else h  -- nothing to cancel right now, treat this as a command invocation

-- | End targeting mode, rejecting the current position.
targetReject :: MonadClientUI m => m Slideshow
targetReject = do
  modifyClient $ \cli -> cli {stgtMode = Nothing}
  failMsg "target not set"

-- * Accept

-- | Accept something, e.g., targeting mode, keeping cursor where it was.
-- Or perform the default action, if nothing needs accepting.
acceptHuman :: MonadClientUI m => m Slideshow -> m Slideshow
acceptHuman h = do
  stgtMode <- getsClient stgtMode
  if isJust stgtMode
    then do
      targetAccept
      return mempty
    else h  -- nothing to accept right now, treat this as a command invocation

-- | End targeting mode, accepting the current position.
targetAccept :: MonadClientUI m => m ()
targetAccept = do
  endTargeting
  endTargetingMsg
  modifyClient $ \cli -> cli {stgtMode = Nothing}

-- | End targeting mode, accepting the current position.
endTargeting :: MonadClientUI m => m ()
endTargeting = do
  leader <- getLeaderUI
  scursor <- getsClient scursor
  modifyClient $ updateTarget leader $ const $ Just scursor

endTargetingMsg :: MonadClientUI m => m ()
endTargetingMsg = do
  leader <- getLeaderUI
  (targetMsg, _) <- targetDescLeader leader
  subject <- partAidLeader leader
  msgAdd $ makeSentence [MU.SubjectVerbSg subject "target", MU.Text targetMsg]

-- * CursorPointerFloor

cursorPointerFloorHuman :: MonadClientUI m => m ()
cursorPointerFloorHuman = do
  look <- cursorPointerFloor False False
  let !_A = assert (look == mempty `blame` look) ()
  modifyClient $ \cli -> cli {stgtMode = Nothing}

-- * CursorPointerEnemy

cursorPointerEnemyHuman :: MonadClientUI m => m ()
cursorPointerEnemyHuman = do
  look <- cursorPointerEnemy False False
  let !_A = assert (look == mempty `blame` look) ()
  modifyClient $ \cli -> cli {stgtMode = Nothing}

-- * TgtPointerFloor

tgtPointerFloorHuman :: MonadClientUI m => m Slideshow
tgtPointerFloorHuman = cursorPointerFloor True False

-- * TgtPointerEnemy

tgtPointerEnemyHuman :: MonadClientUI m => m Slideshow
tgtPointerEnemyHuman = cursorPointerEnemy True False
