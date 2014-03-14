-- | Semantics of 'HumanCmd' client commands that do not return
-- server commands. None of such commands takes game time.
-- TODO: document
module Game.LambdaHack.Client.HumanLocal
  ( -- * Assorted commands
    gameDifficultyCycle
  , pickLeaderHuman, memberCycleHuman, memberBackHuman
  , inventoryHuman, equipmentHuman, allOwnedHuman
  , selectActorHuman, selectNoneHuman, clearHuman, repeatHuman, recordHuman
  , historyHuman, markVisionHuman, markSmellHuman, markSuspectHuman
  , helpHuman, mainMenuHuman, macroHuman
    -- * Commands specific to targeting
  , moveCursorHuman, tgtFloorHuman, tgtEnemyHuman
  , tgtUnknownHuman, tgtItemHuman, tgtStairHuman, tgtAscendHuman
  , epsIncrHuman, tgtClearHuman, cancelHuman, acceptHuman
    -- * Helper definitions
  , pickLeader, lookAt
  ) where

-- Cabal
import qualified Paths_LambdaHack as Self (version)

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version
import Game.LambdaHack.Frontend (frontendName)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.Binding
import Game.LambdaHack.Client.Inventory
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.MonadClientUI
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Animation
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import qualified Game.LambdaHack.Common.HumanCmd as HumanCmd
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Key as K
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind

failMsg :: MonadClientUI m => Msg -> m Slideshow
failMsg msg = do
  modifyClient $ \cli -> cli {slastKey = Nothing}
  stopPlayBack
  assert (not $ T.null msg) $ promptToSlideshow msg

-- * GameDifficultyCycle

gameDifficultyCycle :: MonadClientUI m => m ()
gameDifficultyCycle = do
  DebugModeCli{sdifficultyCli} <- getsClient sdebugCli
  let d = if sdifficultyCli > difficultyBound then 1 else sdifficultyCli + 1
  modifyClient $ \cli -> cli {sdebugCli = (sdebugCli cli) {sdifficultyCli = d}}
  msgAdd $ "Next game difficulty set to" <+> tshow d <> "."

-- * PickLeader

pickLeaderHuman :: MonadClientUI m => Int -> m Slideshow
pickLeaderHuman k = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  s <- getState
  case tryFindHeroK s side k of
    _ | isSpawnFact fact -> failMsg "spawners cannot manually change leaders"
    Nothing -> failMsg "No such member of the party."
    Just (aid, _) -> do
      void $ pickLeader aid
      return mempty

-- * MemberCycle

-- | Switches current member to the next on the level, if any, wrapping.
memberCycleHuman :: MonadClientUI m => m Slideshow
memberCycleHuman = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  leader <- getLeaderUI
  body <- getsState $ getActorBody leader
  hs <- partyAfterLeader leader
  case filter (\(_, b) -> blid b == blid body) hs of
    _ | isSpawnFact fact -> failMsg "spawners cannot manually change leaders"
    [] -> failMsg "Cannot pick any other member on this level."
    (np, b) : _ -> do
      success <- pickLeader np
      assert (success `blame` "same leader" `twith` (leader, np, b)) skip
      return mempty

partyAfterLeader :: MonadReadState m => ActorId -> m [(ActorId, Actor)]
partyAfterLeader leader = do
  faction <- getsState $ bfid . getActorBody leader
  allA <- getsState $ EM.assocs . sactorD
  s <- getState
  let hs9 = mapMaybe (tryFindHeroK s faction) [0..9]
      factionA = filter (\(_, body) ->
        not (bproj body) && bfid body == faction) allA
      hs = hs9 ++ deleteFirstsBy ((==) `on` fst) factionA hs9
      i = fromMaybe (-1) $ findIndex ((== leader) . fst) hs
      (lt, gt) = (take i hs, drop (i + 1) hs)
  return $! gt ++ lt

-- | Select a faction leader. False, if nothing to do.
pickLeader :: MonadClientUI m => ActorId -> m Bool
pickLeader actor = do
  leader <- getLeaderUI
  stgtMode <- getsClient stgtMode
  if leader == actor
    then return False -- already picked
    else do
      pbody <- getsState $ getActorBody actor
      assert (not (bproj pbody) `blame` "projectile chosen as the leader"
                                `twith` (actor, pbody)) skip
      -- Even if it's already the leader, give his proper name, not 'you'.
      let subject = partActor pbody
      msgAdd $ makeSentence [subject, "picked as a leader"]
      -- Update client state.
      s <- getState
      modifyClient $ updateLeader actor s
      -- Move the cursor, if active, to the new level.
      case stgtMode of
        Nothing -> return ()
        Just _ ->
          modifyClient $ \cli -> cli {stgtMode = Just $ TgtMode $ blid pbody}
      -- Inform about items, etc.
      lookMsg <- lookAt False "" True (bpos pbody) actor ""
      msgAdd lookMsg
      return True

-- * MemberBack

-- | Switches current member to the previous in the whole dungeon, wrapping.
memberBackHuman :: MonadClientUI m => m Slideshow
memberBackHuman = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  leader <- getLeaderUI
  hs <- partyAfterLeader leader
  case reverse hs of
    _ | isSpawnFact fact -> failMsg "spawners cannot manually change leaders"
    [] -> failMsg "No other member in the party."
    (np, b) : _ -> do
      success <- pickLeader np
      assert (success `blame` "same leader" `twith` (leader, np, b)) skip
      return mempty

-- * Inventory

-- | Display the common inventory of the whole party.
inventoryHuman :: MonadClientUI m => m Slideshow
inventoryHuman = do
  Kind.COps{coactor=Kind.Ops{okind}, corule} <- getsState scops
  let RuleKind{rsharedInventory} = Kind.stdRuleset corule
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  fact <- getsState $ (EM.! bfid b) . sfactionD
  subject <- if rsharedInventory
             then return $ MU.Text $ gname fact
             else partAidLeader leader
  bag <- if rsharedInventory
         then getsState $ sharedInv b
         else return $ binv b
  slots <- getsClient sslots
  if EM.null bag
    then promptToSlideshow $ makeSentence
      [ MU.SubjectVerbSg subject "have"
      , "nothing in inventory" ]
    else do
      let kind = okind $ bkind b
          verbInv = if calmEnough b kind
                    then "see in"
                    else "distractedly peek at"
          nameInv = if rsharedInventory
                    then "the shared inventory:"
                    else "inventory:"
          blurb = makePhrase
            [MU.Capitalize $ MU.SubjectVerbSg subject verbInv, nameInv]
          sl = EM.filter (`EM.member` bag) slots
      io <- itemOverlay bag sl
      overlayToSlideshow blurb io

-- * Equipment

-- TODO: When equipment is displayed, let TAB switch the leader (without
-- announcing that) and show the equipment of the new leader.
-- | Display equipment of the leader.
equipmentHuman :: MonadClientUI m => m Slideshow
equipmentHuman = do
  leader <- getLeaderUI
  subject <- partAidLeader leader
  b <- getsState $ getActorBody leader
  let bag = beqp b
  slots <- getsClient sslots
  if EM.null bag
    then promptToSlideshow $ makeSentence
      [ MU.SubjectVerbSg subject "have"
      , "no equipment" ]
    else do
      let blurb = makePhrase
            [MU.Capitalize
             $ MU.SubjectVerbSg subject "hold as personal equipment:"]
          sl = EM.filter (`EM.member` bag) slots
      io <- itemOverlay bag sl
      overlayToSlideshow blurb io

-- * AllOwned

-- | Display the sum of equipments and inventory of the whole party.
allOwnedHuman :: MonadClientUI m => m Slideshow
allOwnedHuman = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  fact <- getsState $ (EM.! bfid b) . sfactionD
  let subject = MU.Text $ gname fact
  bag <- getsState $ sharedAllOwned b
  slots <- getsClient sslots
  if EM.null bag
    then promptToSlideshow $ makeSentence
      [ MU.SubjectVerbSg subject "have"
      , "nothing in possesion" ]
    else do
      let blurb = makePhrase
            [MU.Capitalize $ MU.SubjectVerbSg subject "own:"]
          sl = EM.filter (`EM.member` bag) slots
      io <- itemOverlay bag sl
      overlayToSlideshow blurb io

-- * SelectActor

-- TODO: make the message (and for selectNoneHuman, pickLeader, etc.)
-- optional, since they have a clear representation in the UI elsewhere.
selectActorHuman :: MonadClientUI m => m Slideshow
selectActorHuman = do
  mleader <- getsClient _sleader
  case mleader of
    Nothing -> failMsg "no leader picked, cannot select"
    Just leader -> do
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
      return mempty

-- * SelectNone

selectNoneHuman :: (MonadClientUI m, MonadClient m) => m ()
selectNoneHuman = do
  side <- getsClient sside
  lidV <- viewedLevel
  oursAssocs <- getsState $ actorNotProjAssocs (== side) lidV
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
  modifyClient $ \cli -> cli {slastKey = Nothing}
  (_seqCurrent, seqPrevious, k) <- getsClient slastRecord
  case k of
    0 -> do
      let slastRecord = ([], [], maxK)
      modifyClient $ \cli -> cli {slastRecord}
      promptToSlideshow $ "Macro will be recorded for up to"
                          <+> tshow maxK <+> "steps."
    _ -> do
      let slastRecord = (seqPrevious, [], 0)
      modifyClient $ \cli -> cli {slastRecord}
      promptToSlideshow $ "Macro recording interrupted after"
                          <+> tshow (maxK - k - 1) <+> "steps."

-- * History

historyHuman :: MonadClientUI m => m Slideshow
historyHuman = do
  history <- getsClient shistory
  arena <- getArenaUI
  local <- getsState $ getLocalTime arena
  global <- getsState stime
  let  msg = makeSentence
        [ "You survived for"
        , MU.CarWs (global `timeFit` timeTurn) "half-second turn"
        , "(this level:"
        , MU.Text (tshow (local `timeFit` timeTurn)) MU.:> ")" ]
        <+> "Past messages:"
  overlayToBlankSlideshow msg $ renderHistory history

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
  -- BFS takes suspect tiles into account depending on @smarkSuspect@,
  -- so we need to invalidate the BFS data caches.
  modifyClient $ \cli -> cli {sbfsD = EM.empty}
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
                   | (km, (desc, HumanCmd.CmdMenu, cmd)) <- bcmdList,
                     cmd /= HumanCmd.GameDifficultyCycle ]
        in [ (fst (revLookup HumanCmd.Cancel), "back to playing")
           , (fst (revLookup HumanCmd.Accept), "see more help") ]
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
    hd : tl -> overlayToBlankSlideshow hd (toOverlay tl)
               -- TODO: keys don't work if tl/=[]

-- * Macro

macroHuman :: MonadClient m => [String] -> m ()
macroHuman kms =
  modifyClient $ \cli -> cli {slastPlay = map K.mkKM kms ++ slastPlay cli}

-- * MoveCursor

moveCursorHuman :: MonadClientUI m => Vector -> Int -> m Slideshow
moveCursorHuman dir n = do
  leader <- getLeaderUI
  stgtMode <- getsClient stgtMode
  let lidV = maybe (assert `failure` leader) tgtLevelId stgtMode
  Level{lxsize, lysize} <- getLevel lidV
  lpos <- getsState $ bpos . getActorBody leader
  scursor <- getsClient scursor
  cursorPos <- cursorToPos
  let cpos = fromMaybe lpos cursorPos
      shiftB pos = shiftBounded lxsize lysize pos dir
      newPos = iterate shiftB cpos !! n
  if newPos == cpos then failMsg "never mind"
  else do
    let tgt = case scursor of
          TVector{} -> TVector $ displacement lpos newPos
          _ -> TPoint lidV newPos
    modifyClient $ \cli -> cli {scursor = tgt}
    doLook

-- TODO: probably move somewhere (Level?)
-- | Produces a textual description of the terrain and items at an already
-- explored position. Mute for unknown positions.
-- The detailed variant is for use in the targeting mode.
lookAt :: MonadClientUI m
       => Bool       -- ^ detailed?
       -> Text       -- ^ how to start tile description
       -> Bool       -- ^ can be seen right now?
       -> Point      -- ^ position to describe
       -> ActorId    -- ^ the actor that looks
       -> Text       -- ^ an extra sentence to print
       -> m Text
lookAt detailed tilePrefix canSee pos aid msg = do
  Kind.COps{coitem, cotile=cotile@Kind.Ops{okind}} <- getsState scops
  lidV <- viewedLevel
  lvl <- getLevel lidV
  subject <- partAidLeader aid
  s <- getState
  let is = lvl `atI` pos
      verb = MU.Text $ if canSee then "notice" else "remember"
  disco <- getsClient sdisco
  let nWs (iid, k) = partItemWs coitem disco k (getItemBody iid s)
      isd = case detailed of
              _ | EM.size is == 0 -> ""
              _ | EM.size is <= 2 ->
                makeSentence [ MU.SubjectVerbSg subject verb
                             , MU.WWandW $ map nWs $ EM.assocs is]
              True -> "Items:"
              _ -> "Items here."
      tile = lvl `at` pos
      obscured | tile /= hideTile cotile lvl pos = "partially obscured"
               | otherwise = ""
      tileText = obscured <+> tname (okind tile)
      tilePart | T.null tilePrefix = MU.Text tileText
               | otherwise = MU.AW $ MU.Text tileText
      tileDesc = [MU.Text tilePrefix, tilePart]
  if not (null (Tile.causeEffects cotile tile)) then
    return $! makeSentence ("activable:" : tileDesc)
              <+> msg <+> isd
  else if detailed then
    return $! makeSentence tileDesc
              <+> msg <+> isd
  else return $! msg <+> isd

-- | Perform look around in the current position of the cursor.
-- Assumes targeting mode and so assumes that a leader is picked.
doLook :: MonadClientUI m => m Slideshow
doLook = do
  leader <- getLeaderUI
  stgtMode <- getsClient stgtMode
  let lidV = maybe (assert `failure` leader) tgtLevelId stgtMode
  lvl <- getLevel lidV
  cursorPos <- cursorToPos
  per <- getPerFid lidV
  b <- getsState $ getActorBody leader
  let p = fromMaybe (bpos b) cursorPos
      canSee = ES.member p (totalVisible per)
  inhabitants <- if canSee
                 then getsState $ posToActors p lidV
                 else return []
  seps <- getsClient seps
  (steps, _eps) <- makeLine b p seps
  let aims = steps == chessDist (bpos b) p
      enemyMsg = case inhabitants of
        [] -> ""
        _ -> -- Even if it's the leader, give his proper name, not 'you'.
             let subjects = map (partActor . snd . fst) inhabitants
                 subject = MU.WWandW subjects
                 verb = "be here"
             in makeSentence [MU.SubjectVerbSg subject verb]
      vis | not canSee = "you cannot see"
          | not aims = "you cannot penetrate"
          | otherwise = "you see"
  -- Show general info about current position.
  lookMsg <- lookAt True vis canSee p leader enemyMsg
  modifyClient $ \cli -> cli {slastKey = Nothing}
  -- Check if there's something lying around at current position.
  let is = lvl `atI` p
  if EM.size is <= 2 then
    promptToSlideshow lookMsg
  else do
    io <- floorItemOverlay is
    overlayToSlideshow lookMsg io

-- * TgtFloor

-- | Cycle targeting mode. Do not change position of the cursor,
-- switch among things at that position.
tgtFloorHuman :: MonadClientUI m => m Slideshow
tgtFloorHuman = do
  lidV <- viewedLevel
  leader <- getLeaderUI
  lpos <- getsState $ bpos . getActorBody leader
  cursorPos <- cursorToPos
  scursor <- getsClient scursor
  stgtMode <- getsClient stgtMode
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  bsAll <- getsState $ actorAssocs (const True) lidV
  let cursor = fromMaybe lpos cursorPos
      tgt = case scursor of
        _ | isNothing stgtMode ->  -- first key press: keep target
          scursor
        TEnemy a False -> TEnemy a True
        TEnemy{} -> TPoint lidV cursor
        TEnemyPos{} -> TPoint lidV cursor
        TPoint{} -> TVector $ displacement lpos cursor
        TVector{} ->
          let isEnemy b = isAtWar fact (bfid b)
                          && not (bproj b)
          -- For projectiles, we pick here the first that would be picked
          -- by '*', so that all other projectiles on the tile come next,
          -- without any intervening actors from other tiles.
          in case find (\(_, m) -> Just (bpos m) == cursorPos) bsAll of
            Just (im, m) | isEnemy m -> TEnemy im False
            Just (im, _) -> TEnemy im True
            Nothing -> TPoint lidV cursor
  modifyClient $ \cli -> cli {scursor = tgt, stgtMode = Just $ TgtMode lidV}
  doLook

-- * TgtEnemy

tgtEnemyHuman :: MonadClientUI m => m Slideshow
tgtEnemyHuman = do
  lidV <- viewedLevel
  leader <- getLeaderUI
  lpos <- getsState $ bpos . getActorBody leader
  cursorPos <- cursorToPos
  scursor <- getsClient scursor
  stgtMode <- getsClient stgtMode
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  bsAll <- getsState $ actorAssocs (const True) lidV
  let ordPos (_, b) = (chessDist lpos $ bpos b, bpos b)
      dbs = sortBy (comparing ordPos) bsAll
      pickUnderCursor =  -- switch to the enemy under cursor, if any
        let i = fromMaybe (-1)
                $ findIndex ((== cursorPos) . Just . bpos . snd) dbs
        in splitAt i dbs
      (permitAnyActor, (lt, gt)) = case scursor of
            TEnemy a permit | isJust stgtMode ->  -- pick next enemy
              let i = fromMaybe (-1) $ findIndex ((== a) . fst) dbs
              in (permit, splitAt (i + 1) dbs)
            TEnemy a permit ->  -- first key press, retarget old enemy
              let i = fromMaybe (-1) $ findIndex ((== a) . fst) dbs
              in (permit, splitAt i dbs)
            TEnemyPos _ _ _ permit -> (permit, pickUnderCursor)
            _ -> (False, pickUnderCursor)  -- the sensible default is only-foes
      gtlt = gt ++ lt
      isEnemy b = isAtWar fact (bfid b)
                  && not (bproj b)
      lf = filter (isEnemy . snd) gtlt
      tgt | permitAnyActor = case gtlt of
        (a, _) : _ -> TEnemy a True
        [] -> scursor  -- no actors in sight, stick to last target
          | otherwise = case lf of
        (a, _) : _ -> TEnemy a False
        [] -> scursor  -- no seen foes in sight, stick to last target
  -- Register the chosen enemy, to pick another on next invocation.
  modifyClient $ \cli -> cli {scursor = tgt, stgtMode = Just $ TgtMode lidV}
  doLook

-- * TgtUnknown

tgtUnknownHuman :: MonadClientUI m => m Slideshow
tgtUnknownHuman = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  mpos <- closestUnknown leader
  case mpos of
    Nothing -> failMsg "no more unknown spots left"
    Just p -> do
      let tgt = TPoint (blid b) p
      modifyClient $ \cli -> cli {scursor = tgt}
      return mempty

-- * TgtItem

tgtItemHuman :: MonadClientUI m => m Slideshow
tgtItemHuman = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  items <- closestItems leader
  case items of
    [] -> failMsg "no more items remembered or visible"
    (_, (p, _)) : _ -> do
      let tgt = TPoint (blid b) p
      modifyClient $ \cli -> cli {scursor = tgt}
      return mempty

-- * TgtStair

tgtStairHuman :: MonadClientUI m => Bool -> m Slideshow
tgtStairHuman up = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  stairs <- closestTriggers (Just up) False leader
  case stairs of
    [] -> failMsg $ "no stairs"
                     <+> if up then "up" else "down"
    p : _ -> do
      let tgt = TPoint (blid b) p
      modifyClient $ \cli -> cli {scursor = tgt}
      return mempty

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
          in if Tile.hasFeature cotile (F.Cause $ Effect.Ascend k) tile
             then Just cpos
             else Nothing
  case rightStairs of
    Just cpos -> do  -- stairs, in the right direction
      (nln, npos) <- getsState $ whereTo lidV cpos k . sdungeon
      assert (nln /= lidV `blame` "stairs looped" `twith` nln) skip
      nlvl <- getLevel nln
      -- Do not freely reveal the other end of the stairs.
      let ascDesc (F.Cause (Effect.Ascend _)) = True
          ascDesc _ = False
          scursor =
            if any ascDesc $ tfeature $ okind (nlvl `at` npos)
            then TPoint nln npos  -- already known as an exit, focus on it
            else scursorOld  -- unknown, do not reveal
      modifyClient $ \cli -> cli {scursor, stgtMode = Just (TgtMode nln)}
      doLook
    Nothing ->  -- no stairs in the right direction
      case ascendInBranch dungeon k lidV of
        [] -> failMsg "no more levels in this direction"
        nln : _ -> do
          modifyClient $ \cli -> cli {stgtMode = Just (TgtMode nln)}
          doLook

-- * EpsIncr

-- | Tweak the @eps@ parameter of the targeting digital line.
epsIncrHuman :: MonadClientUI m => Bool -> m Slideshow
epsIncrHuman b = do
  stgtMode <- getsClient stgtMode
  if isJust stgtMode
    then do
      modifyClient $ \cli -> cli {seps = seps cli + if b then 1 else -1}
      return mempty
    else failMsg "never mind"  -- no visual feedback, so no sense

-- * TgtClear

tgtClearHuman :: MonadClient m => m ()
tgtClearHuman = do
  mleader <- getsClient _sleader
  case mleader of
    Nothing -> return ()
    Just leader -> do
      tgt <- getsClient $ getTarget leader
      case tgt of
        Just _ -> modifyClient $ updateTarget leader (const Nothing)
        Nothing -> do
          scursorOld <- getsClient scursor
          b <- getsState $ getActorBody leader
          let scursor = case scursorOld of
                TEnemy _ permit -> TEnemy leader permit
                TEnemyPos _ _ _ permit -> TEnemy leader permit
                TPoint{} -> TPoint (blid b) (bpos b)
                TVector{} -> TVector (Vector 0 0)
          modifyClient $ \cli -> cli {scursor}

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
  failMsg "targeting canceled"

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
  targetMsg <- targetDescLeader leader
  subject <- partAidLeader leader
  msgAdd $ makeSentence [MU.SubjectVerbSg subject "target", MU.Text targetMsg]
