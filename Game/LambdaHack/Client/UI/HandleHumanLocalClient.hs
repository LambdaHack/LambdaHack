-- | Semantics of 'HumanCmd' client commands that do not return
-- server commands. None of such commands takes game time.
-- TODO: document
module Game.LambdaHack.Client.UI.HandleHumanLocalClient
  ( -- * Assorted commands
    gameDifficultyIncr
  , pickLeaderHuman, memberCycleHuman, memberBackHuman
  , selectActorHuman, selectNoneHuman, clearHuman
  , stopIfTgtModeHuman, selectWithPointer, repeatHuman, recordHuman
  , historyHuman, markVisionHuman, markSmellHuman, markSuspectHuman
  , macroHuman
    -- * Commands specific to targeting
  , moveCursorHuman, tgtFloorHuman, tgtEnemyHuman
  , tgtAscendHuman, epsIncrHuman, tgtClearHuman
  , cursorUnknownHuman, cursorItemHuman, cursorStairHuman
  , cursorPointerFloorHuman, cursorPointerEnemyHuman
  , tgtPointerFloorHuman, tgtPointerEnemyHuman
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.BfsClient
import Game.LambdaHack.Client.ItemSlot
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.HandleHelperClient
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.MsgClient
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Client.UI.WidgetClient
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Frequency
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import qualified Game.LambdaHack.Content.ItemKind as IK
import qualified Game.LambdaHack.Content.TileKind as TK

-- * GameDifficultyIncr

gameDifficultyIncr :: MonadClientUI m => m ()
gameDifficultyIncr = do
  let delta = 1
  snxtDiff <- getsClient snxtDiff
  let d | snxtDiff + delta > difficultyBound = 1
        | snxtDiff + delta < 1 = difficultyBound
        | otherwise = snxtDiff + delta
  modifyClient $ \cli -> cli {snxtDiff = d}

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
      | blid b /= arena && autoDun ->
          failMsg $ showReqFailure NoChangeDunLeader
      | autoLvl ->
          failMsg $ showReqFailure NoChangeLvlLeader
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
  wasMemeber <- getsSession $ ES.member leader . sselected
  let upd = if wasMemeber
            then ES.delete leader  -- already selected, deselect instead
            else ES.insert leader
  modifySession $ \sess -> sess {sselected = upd $ sselected sess}
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
  oldSel <- getsSession sselected
  let wasNone = ES.null $ ES.intersection ours oldSel
      upd = if wasNone
            then ES.union  -- already all deselected; select all instead
            else ES.difference
  modifySession $ \sess -> sess {sselected = upd (sselected sess) ours}
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
  tgtMode <- getsSession stgtMode
  when (isJust tgtMode) $
    void $ stopPlayBack

-- * SelectWithPointer

selectWithPointer:: MonadClientUI m => m ()
selectWithPointer = do
  km <- getsSession slastKM
  lidV <- viewedLevel
  Level{lysize} <- getLevel lidV
  side <- getsClient sside
  ours <- getsState $ filter (not . bproj . snd)
                      . actorAssocs (== side) lidV
  -- Select even if no space in status line for the actor's symbol.
  let viewed = sortBy (comparing keySelected) ours
  case K.pointer km of
    Just(Point{..}) | py == lysize + 2 && px <= length viewed && px >= 0 -> do
      if px == 0 then
        selectNoneHuman
      else
        selectAidHuman $ fst $ viewed !! (px - 1)
      void $ stopPlayBack
    _ -> return ()

-- * Repeat

-- Note that walk followed by repeat should not be equivalent to run,
-- because the player can really use a command that does not stop
-- at terrain change or when walking over items.
repeatHuman :: MonadClientUI m => Int -> m ()
repeatHuman n = do
  (_, seqPrevious, k) <- getsSession slastRecord
  let macro = concat $ replicate n $ reverse seqPrevious
  modifySession $ \sess -> sess {slastPlay = macro ++ slastPlay sess}
  let slastRecord = ([], [], if k == 0 then 0 else maxK)
  modifySession $ \sess -> sess {slastRecord}

maxK :: Int
maxK = 100

-- * Record

recordHuman :: MonadClientUI m => m Slideshow
recordHuman = do
  (_seqCurrent, seqPrevious, k) <- getsSession slastRecord
  case k of
    0 -> do
      let slastRecord = ([], [], maxK)
      modifySession $ \sess -> sess {slastRecord}
      promptToSlideshow $ "Macro will be recorded for up to"
                          <+> tshow maxK <+> "actions."  -- no MU, poweruser
    _ -> do
      let slastRecord = (seqPrevious, [], 0)
      modifySession $ \sess -> sess {slastRecord}
      promptToSlideshow $ "Macro recording interrupted after"
                          <+> tshow (maxK - k - 1) <+> "actions."

-- * History

historyHuman :: MonadClientUI m => m ()
historyHuman = do
  history <- getsSession shistory
  arena <- getArenaUI
  Level{lxsize, lysize} <- getLevel arena
  local <- getsState $ getLocalTime arena
  global <- getsState stime
  let histLines = linesHistory history
      turnsGlobal = global `timeFitUp` timeTurn
      turnsLocal = local `timeFitUp` timeTurn
      msg = makeSentence
        [ "You survived for"
        , MU.CarWs turnsGlobal "half-second turn"
        , "(this level:"
        , MU.Text (tshow turnsLocal) <> ")" ]
        <+> "[ESC to cancel]"
      dummySlot = head allZeroSlots
      rh = renderHistory history
      kxs = replicate (length rh)
                      (Right dummySlot, (undefined, 0, lxsize))
  okxs <- splitOKX (lysize + 3) msg (toOverlay rh, kxs)
  let displayAllHistory = do
        menuIxHistory <- getsSession smenuIxHistory
        (ekm, pointer) <-
          displayChoiceScreen True menuIxHistory okxs [K.spaceKM, K.escKM]
        modifySession $ \sess -> sess {smenuIxHistory = pointer}
        case ekm of
          Left km | km `elem` [K.spaceKM, K.escKM] -> return ()
          Right slot | slot == dummySlot -> displayOneReport pointer
          _ -> assert `failure` ekm
      displayOneReport pointer = do
        let timeReport = case drop pointer histLines of
              [] -> assert `failure` pointer
              tR : _ -> tR
            (tturns, rep) =   splitReportForHistory lxsize timeReport
            ov0 = toOverlay rep
            -- TODO: print over history, not over dungeon;
            -- expand this history item, not switch views completely;
            prompt = "The full past message at time"
                     <+> tturns <> ". [ESC to go back]"
        escK <- displayChoiceLine prompt ov0 [K.escKM]
        let !_A = assert (escK == K.escKM) ()
        displayAllHistory
  displayAllHistory

-- * MarkVision, MarkSmell, MarkSuspect

markVisionHuman :: MonadClientUI m => m ()
markVisionHuman = do
  modifySession toggleMarkVision
  cur <- getsSession smarkVision
  msgAdd $ "Visible area display toggled" <+> if cur then "on." else "off."

markSmellHuman :: MonadClientUI m => m ()
markSmellHuman = do
  modifySession toggleMarkSmell
  cur <- getsSession smarkSmell
  msgAdd $ "Smell display toggled" <+> if cur then "on." else "off."

markSuspectHuman :: MonadClientUI m => m ()
markSuspectHuman = do
  -- @condBFS@ depends on the setting we change here.
  modifyClient $ \cli -> cli {sbfsD = EM.empty}
  modifyClient toggleMarkSuspect
  cur <- getsClient smarkSuspect
  msgAdd $ "Suspect terrain display toggled" <+> if cur then "on." else "off."

-- * Macro

macroHuman :: MonadClientUI m => [String] -> m Slideshow
macroHuman kms = do
  modifySession $ \sess -> sess {slastPlay = map K.mkKM kms ++ slastPlay sess}
  promptToSlideshow $ "Macro activated:" <+> T.pack (intercalate " " kms)

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
      modifyClient $ \cli -> cli {scursor}
      modifySession $ \sess -> sess {stgtMode = Just (TgtMode nln)}
      doLook False
    Nothing ->  -- no stairs in the right direction
      case ascendInBranch dungeon k lidV of
        [] -> failMsg "no more levels in this direction"
        nln : _ -> do
          modifySession $ \sess -> sess {stgtMode = Just (TgtMode nln)}
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
  case sortBy (flip compare) $ runFrequency stairs of
    [] -> failMsg $ "no stairs" <+> if up then "up" else "down"
    (_, p) : _ -> do
      let tgt = TPoint (blid b) p
      modifyClient $ \cli -> cli {scursor = tgt}
      doLook False

-- * CursorPointerFloor

cursorPointerFloorHuman :: MonadClientUI m => m ()
cursorPointerFloorHuman = do
  look <- cursorPointerFloor False False
  let !_A = assert (look == mempty `blame` look) ()
  modifySession $ \sess -> sess {stgtMode = Nothing}

-- * CursorPointerEnemy

cursorPointerEnemyHuman :: MonadClientUI m => m ()
cursorPointerEnemyHuman = do
  look <- cursorPointerEnemy False False
  let !_A = assert (look == mempty `blame` look) ()
  modifySession $ \sess -> sess {stgtMode = Nothing}

-- * TgtPointerFloor

tgtPointerFloorHuman :: MonadClientUI m => m Slideshow
tgtPointerFloorHuman = cursorPointerFloor True False

-- * TgtPointerEnemy

tgtPointerEnemyHuman :: MonadClientUI m => m Slideshow
tgtPointerEnemyHuman = cursorPointerEnemy True False
