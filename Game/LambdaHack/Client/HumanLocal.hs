{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Semantics of 'HumanCmd' client commands that do not return
-- server commands. None of such commands takes game time.
-- TODO: document
module Game.LambdaHack.Client.HumanLocal
  ( -- * Semantics of serverl-less human commands
    moveCursor, retargetLeader
  , selectHeroHuman, memberCycleHuman, memberBackHuman
  , inventoryHuman, tgtFloorLeader, tgtEnemyLeader, tgtAscendHuman
  , epsIncrHuman, cancelHuman, displayMainMenu, acceptHuman, clearHuman
  , historyHuman, helpHuman
    -- * Helper functions useful also elsewhere
  , endTargeting, floorItemOverlay, itemOverlay, viewedLevel, selectLeader
  , stopRunning, lookAt
  ) where

-- Cabal
import qualified Paths_LambdaHack as Self (version)

import Control.Monad
import Control.Monad.Writer.Strict (WriterT, tell)
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.Binding
import qualified Game.LambdaHack.Client.HumanCmd as HumanCmd
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Item
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
import Game.LambdaHack.Utils.Assert

default (Text)

-- * Move and Run

moveCursor :: MonadClientUI m => Vector -> Int -> WriterT Slideshow m ()
moveCursor dir n = do
  leader <- getLeaderUI
  lpos <- getsState $ bpos . getActorBody leader
  scursor <- getsClient scursor
  let cpos = fromMaybe lpos scursor
  Level{lxsize, lysize} <- cursorLevel
  let shiftB pos = shiftBounded lxsize (1, 1, lxsize - 2, lysize - 2) pos dir
  modifyClient $ \cli -> cli {scursor = Just $ iterate shiftB cpos !! n}
  doLook

cursorLevel :: MonadClient m => m Level
cursorLevel = do
  dungeon <- getsState sdungeon
  stgtMode <- getsClient stgtMode
  let tgtId =
        maybe (assert `failure` "not targetting right now") tgtLevelId stgtMode
  return $! dungeon EM.! tgtId

viewedLevel :: MonadClientUI m => m (LevelId, Level)
viewedLevel = do
  arena <- getArenaUI
  dungeon <- getsState sdungeon
  stgtMode <- getsClient stgtMode
  let tgtId = maybe arena tgtLevelId stgtMode
  return $! (tgtId, dungeon EM.! tgtId)

-- TODO: probably move somewhere (Level?)
-- | Produces a textual description of the terrain and items at an already
-- explored position. Mute for unknown positions.
-- The detailed variant is for use in the targeting mode.
lookAt :: MonadClientUI m
       => Bool       -- ^ detailed?
       -> Bool       -- ^ can be seen right now?
       -> Point      -- ^ position to describe
       -> Text       -- ^ an extra sentence to print
       -> m Text
lookAt detailed canSee pos msg = do
  Kind.COps{coitem, cotile=Kind.Ops{oname}} <- getsState scops
  (_, lvl) <- viewedLevel
  s <- getState
  let is = lvl `atI` pos
      prefixSee = MU.Text $ if canSee then "you see" else "you remember"
  disco <- getsClient sdisco
  let nWs (iid, k) = partItemNWs coitem disco k (getItemBody iid s)
      isd = case detailed of
              _ | EM.size is == 0 -> ""
              _ | EM.size is <= 2 ->
                makeSentence [prefixSee, MU.WWandW $ map nWs $ EM.assocs is]
              True -> "Objects:"
              _ -> "Objects here."
  if detailed
    then let tile = lvl `at` pos
         in return $! makeSentence [MU.Text $ oname tile] <+> msg <+> isd
    else return $! msg <+> isd

-- | Perform look around in the current position of the cursor.
-- Assumes targeting mode and so assumes that a leader is selected.
doLook :: MonadClientUI m => WriterT Slideshow m ()
doLook = do
  Kind.COps{coactor} <- getsState scops
  scursor <- getsClient scursor
  (lid, lvl) <- viewedLevel
  per <- getPerFid lid
  leader <- getLeaderUI
  lpos <- getsState $ bpos . getActorBody leader
  target <- getsClient $ getTarget leader
  hms <- getsState $ actorList (const True) lid
  let p = fromMaybe lpos scursor
      canSee = ES.member p (totalVisible per)
      ihabitant | canSee = find (\m -> bpos m == p) hms
                | otherwise = Nothing
      enemyMsg =
        maybe "" (\ m -> makeSentence
                         [MU.SubjectVerbSg (partActor coactor m) "be here"])
                 ihabitant
      vis | not $ p `ES.member` totalVisible per = " (not visible)"
          | actorSeesLoc per leader p = ""
          | otherwise = " (not visible by you)"
      mode = case target of
               Just TEnemy{} -> "[targeting foe" <> vis <> "]"
               Just TPos{}   -> "[targeting position" <> vis <> "]"
               Nothing       -> "[targeting current" <> vis <> "]"
      -- Check if there's something lying around at current position.
      is = lvl `atI` p
  -- Show general info about current p.
  lookMsg <- lookAt True canSee p enemyMsg
  modifyClient (\st -> st {slastKey = Nothing})
  if EM.size is <= 2
    then do
      slides <- promptToSlideshow (mode <+> lookMsg)
      tell slides
    else do
     io <- floorItemOverlay is
     slides <- overlayToSlideshow (mode <+> lookMsg) io
     tell slides

-- | Create a list of item names.
floorItemOverlay :: MonadClient m => ItemBag -> m Overlay
floorItemOverlay bag = do
  Kind.COps{coitem} <- getsState scops
  s <- getState
  disco <- getsClient sdisco
  let is = zip (EM.assocs bag) (allLetters ++ repeat (InvChar ' '))
      pr ((iid, k), l) =
         makePhrase [ letterLabel l
                    , partItemNWs coitem disco k (getItemBody iid s) ]
         <> " "
  return $ map pr is

-- | Create a list of item names.
itemOverlay :: MonadClient m => ItemBag -> ItemInv -> m Overlay
itemOverlay bag inv = do
  Kind.COps{coitem} <- getsState scops
  s <- getState
  disco <- getsClient sdisco
  let checkItem (l, iid) = fmap (\k -> (l, iid, k)) $ EM.lookup iid bag
      is = mapMaybe checkItem $ EM.assocs inv
      pr (l, iid, k) =
         makePhrase [ letterLabel l
                    , partItemNWs coitem disco k (getItemBody iid s) ]
         <> " "
  return $ map pr is

-- * Project

retargetLeader :: MonadClientUI m => WriterT Slideshow m ()
retargetLeader = do
  stgtMode <- getsClient stgtMode
  assert (isNothing stgtMode) $ do
    arena <- getArenaUI
    msgAdd "Last target invalid."
    modifyClient $ \cli -> cli {scursor = Nothing, seps = 0}
    tgtEnemyLeader $ TgtAuto arena

-- * SelectHero

selectHeroHuman :: (MonadClientAbort m, MonadClientUI m) => Int -> m ()
selectHeroHuman k = do
  side <- getsClient sside
  loc <- getState
  case tryFindHeroK loc side k of
    Nothing  -> abortWith "No such member of the party."
    Just (aid, _) -> void $ selectLeader aid

-- * MemberCycle

-- | Switches current member to the next on the level, if any, wrapping.
memberCycleHuman :: (MonadClientAbort m, MonadClientUI m) => m ()
memberCycleHuman = do
  leader <- getLeaderUI
  body <- getsState $ getActorBody leader
  hs <- partyAfterLeader leader
  case filter (\(_, b) -> blid b == blid body) hs of
    [] -> abortWith "Cannot select any other member on this level."
    (np, b) : _ -> do
      success <- selectLeader np
      assert (success `blame` (leader, np, b)) skip

partyAfterLeader :: MonadActionRO m
                 => ActorId
                 -> m [(ActorId, Actor)]
partyAfterLeader leader = do
  faction <- getsState $ bfaction . getActorBody leader
  allA <- getsState $ EM.assocs . sactorD
  s <- getState
  let hs9 = catMaybes $ map (tryFindHeroK s faction) [0..9]
      factionA = filter (\(_, body) ->
        not (bproj body) && bfaction body == faction) allA
      hs = hs9 ++ (deleteFirstsBy ((==) `on` fst) factionA hs9)
      i = fromMaybe (-1) $ findIndex ((== leader) . fst) hs
      (lt, gt) = (take i hs, drop (i + 1) hs)
  return $ gt ++ lt

-- | Select a faction leader. False, if nothing to do.
selectLeader :: MonadClientUI m => ActorId -> m Bool
selectLeader actor = do
  Kind.COps{coactor} <- getsState scops
  leader <- getLeaderUI
  stgtMode <- getsClient stgtMode
  if actor == leader
    then return False -- already selected
    else do
      loc <- getState
      modifyClient $ updateLeader actor loc
      pbody <- getsState $ getActorBody actor
      assert (not (bproj pbody) `blame` (actor, pbody)) skip
      -- Move the cursor, if active, to the new level.
      when (isJust stgtMode) $ setTgtId $ blid pbody
      -- Don't continue an old run, if any.
      stopRunning
      -- Announce.
      msgAdd $ makeSentence [partActor coactor pbody, "selected"]
      return True

stopRunning :: MonadClient m => m ()
stopRunning = modifyClient (\ cli -> cli { srunning = Nothing })

-- * MemberBack

-- | Switches current member to the previous in the whole dungeon, wrapping.
memberBackHuman :: (MonadClientAbort m, MonadClientUI m) => m ()
memberBackHuman = do
  leader <- getLeaderUI
  hs <- partyAfterLeader leader
  case reverse hs of
    [] -> abortWith "No other member in the party."
    (np, b) : _ -> do
      success <- selectLeader np
      assert (success `blame` (leader, np, b)) skip

-- * Inventory

-- TODO: When inventory is displayed, let TAB switch the leader (without
-- announcing that) and show the inventory of the new leader.
-- | Display inventory
inventoryHuman :: (MonadClientAbort m, MonadClientUI m)
               => WriterT Slideshow m ()
inventoryHuman = do
  Kind.COps{coactor} <- getsState scops
  leader <- getLeaderUI
  pbody <- getsState $ getActorBody leader
  bag <- getsState $ getActorBag leader
  inv <- getsState $ getActorInv leader
  if EM.null bag
    then abortWith $ makeSentence
      [ MU.SubjectVerbSg (partActor coactor pbody) "be"
      , "not carrying anything" ]
    else do
      let blurb = makePhrase [MU.Capitalize $
            MU.SubjectVerbSg (partActor coactor pbody) "be carrying:"]
      io <- itemOverlay bag inv
      slides <- overlayToSlideshow blurb io
      tell slides

-- * TgtFloor

-- | Start floor targeting mode or reset the cursor position to the leader.
tgtFloorLeader :: MonadClientUI m => TgtMode -> WriterT Slideshow m ()
tgtFloorLeader stgtModeNew = do
  leader <- getLeaderUI
  ppos <- getsState (bpos . getActorBody leader)
  target <- getsClient $ getTarget leader
  stgtMode <- getsClient stgtMode
  let tgt = case target of
        Just (TEnemy _ _) -> Nothing  -- forget enemy target, keep the cursor
        _ | isJust stgtMode ->
          Just (TPos ppos)  -- double key press: reset cursor
        t -> t  -- keep the target from previous targeting session
  -- Register that we want to target only positions.
  modifyClient $ updateTarget leader (const tgt)
  setCursor stgtModeNew

-- | Set, activate and display cursor information.
setCursor :: MonadClientUI m => TgtMode -> WriterT Slideshow m ()
setCursor stgtModeNew = do
  stgtModeOld <- getsClient stgtMode
  scursorOld <- getsClient scursor
  sepsOld <- getsClient seps
  scursor <- targetToPos
  let seps = if scursor == scursorOld then sepsOld else 0
      stgtMode = if isNothing stgtModeOld
                   then Just stgtModeNew
                   else stgtModeOld
  modifyClient $ \cli2 -> cli2 {scursor, seps, stgtMode}
  doLook

-- * TgtEnemy

-- | Start the enemy targeting mode. Cycle between enemy targets.
tgtEnemyLeader :: MonadClientUI m => TgtMode -> WriterT Slideshow m ()
tgtEnemyLeader stgtModeNew = do
  leader <- getLeaderUI
  ppos <- getsState (bpos . getActorBody leader)
  (lid, Level{lxsize}) <- viewedLevel
  per <- getPerFid lid
  target <- getsClient $ getTarget leader
  -- TODO: sort enemies by distance to the leader.
  stgtMode <- getsClient stgtMode
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfaction
  ms <- getsState $ actorNotProjAssocs (isAtWar fact) lid
  let plms = filter ((/= leader) . fst) ms  -- don't target yourself
      ordPos (_, m) = (chessDist lxsize ppos $ bpos m, bpos m)
      dms = sortBy (comparing ordPos) plms
      (lt, gt) = case target of
            Just (TEnemy n _) | isJust stgtMode ->  -- pick next enemy
              let i = fromMaybe (-1) $ findIndex ((== n) . fst) dms
              in splitAt (i + 1) dms
            Just (TEnemy n _) ->  -- try to retarget the old enemy
              let i = fromMaybe (-1) $ findIndex ((== n) . fst) dms
              in splitAt i dms
            _ -> (dms, [])  -- target first enemy (e.g., number 0)
      gtlt = gt ++ lt
      seen (_, m) =
        let mpos = bpos m            -- it is remembered by faction
        in actorSeesLoc per leader mpos  -- is it visible by actor?
      lf = filter seen gtlt
      tgt = case lf of
              [] -> target  -- no enemies in sight, stick to last target
              (na, nm) : _ -> Just (TEnemy na (bpos nm))  -- pick the next
  -- Register the chosen enemy, to pick another on next invocation.
  modifyClient $ updateTarget leader (const tgt)
  setCursor stgtModeNew

-- * TgtAscend

-- | Change the displayed level in targeting mode to (at most)
-- k levels shallower. Enters targeting mode, if not already in one.
tgtAscendHuman :: (MonadClientAbort m, MonadClientUI m)
               => Int -> WriterT Slideshow m ()
tgtAscendHuman k = do
  Kind.COps{cotile} <- getsState scops
  dungeon <- getsState sdungeon
  loc <- getState
  cursor <- getsClient scursor
  (tgtId, lvl) <- viewedLevel
  let rightStairs = case cursor of
        Nothing -> False
        Just cpos ->
          let tile = lvl `at` cpos
          in k ==  1
             && Tile.hasFeature cotile (F.Cause $ Effect.Ascend 1)  tile
             || k == -1
             && Tile.hasFeature cotile (F.Cause $ Effect.Descend 1) tile
  if rightStairs  -- stairs, in the right direction
    then case whereTo loc tgtId k of
      Nothing ->  -- we are at the "end" of the dungeon
        abortWith "no more levels in this direction"
      Just (nln, npos) ->
        assert (nln /= tgtId `blame` (nln, "stairs looped")) $ do
          -- Do not freely reveal the other end of the stairs.
          let scursor =
                if Tile.hasFeature cotile F.Exit (lvl `at` npos)
                then Just npos  -- already know as an exit, focus on it
                else cursor    -- unknown, do not reveal
          modifyClient $ \cli -> cli {scursor}
          setTgtId nln
    else  -- no stairs in the right direction
      case ascendInBranch dungeon tgtId k of
        [] -> abortWith "no more levels in this direction"
        nln : _ -> setTgtId nln
  doLook

setTgtId :: MonadClient m => LevelId -> m ()
setTgtId nln = do
  stgtMode <- getsClient stgtMode
  case stgtMode of
    Just (TgtAuto _) ->
      modifyClient $ \cli -> cli {stgtMode = Just (TgtAuto nln)}
    _ ->
      modifyClient $ \cli -> cli {stgtMode = Just (TgtExplicit nln)}

-- * EpsIncr

-- | Tweak the @eps@ parameter of the targeting digital line.
epsIncrHuman :: MonadClientAbort m => Bool -> m ()
epsIncrHuman b = do
  stgtMode <- getsClient stgtMode
  if isJust stgtMode
    then modifyClient $ \cli -> cli {seps = seps cli + if b then 1 else -1}
    else neverMind True  -- no visual feedback, so no sense

-- * Cancel

-- | Cancel something, e.g., targeting mode, resetting the cursor
-- to the position of the leader. Chosen target is not invalidated.
cancelHuman :: MonadClientUI m
            => WriterT Slideshow m () -> WriterT Slideshow m ()
cancelHuman h = do
  stgtMode <- getsClient stgtMode
  if isJust stgtMode
    then endTargeting False
    else h  -- nothing to cancel right now, treat this as a command invocation

-- | Display the main menu.
displayMainMenu :: MonadClientUI m => WriterT Slideshow m ()
displayMainMenu = do
  Kind.COps{corule} <- getsState scops
  Binding{krevMap} <- askBinding
  let stripFrame t = case T.uncons t of
        Just ('\n', art) -> map (T.tail . T.init) $ tail . init $ T.lines art
        _ -> assert `failure` "displayMainMenu:" <+> t
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
            revLookup cmd = maybe ("", "") (showKD cmd) $ M.lookup cmd krevMap
            cmds = [ HumanCmd.GameSave,
                     HumanCmd.GameExit,
                     HumanCmd.GameRestart,
                     HumanCmd.Help
                   ]
        in map revLookup cmds ++ [(fst (revLookup HumanCmd.Clear), "continue")]
      bindings =  -- key bindings to display
        let bindingLen = 25
            fmt (k, d) =
              let gapLen = (8 - T.length k) `max` 1
                  padLen = bindingLen - T.length k - gapLen - T.length d
              in k <> T.replicate gapLen " " <> d <> T.replicate padLen " "
        in map fmt kds
      overwrite =  -- overwrite the art with key bindings
        let over [] line = ([], T.pack line)
            over bs@(binding : bsRest) line =
              let (prefix, lineRest) = break (=='{') line
                  (braces, suffix)   = span  (=='{') lineRest
              in if length braces == 25
                 then (bsRest, T.pack prefix <> binding <> T.pack suffix)
                 else (bs, T.pack line)
        in snd . mapAccumL over bindings
      mainMenuArt = rmainMenuArt $ Kind.stdRuleset corule
      menuOverlay =  -- TODO: switch to Text and use T.justifyLeft
        overwrite $ pasteVersion $ map T.unpack $ stripFrame $ mainMenuArt
  case menuOverlay of
    [] -> assert `failure` "empty Main Menu overlay"
    hd : tl -> do
      slides <- overlayToSlideshow hd tl
      tell slides

-- * Accept

-- | Accept something, e.g., targeting mode, keeping cursor where it was.
-- Or perform the default action, if nothing needs accepting.
acceptHuman :: MonadClientUI m
            => WriterT Slideshow m () -> WriterT Slideshow m ()
acceptHuman h = do
  stgtMode <- getsClient stgtMode
  if isJust stgtMode
    then endTargeting True
    else h  -- nothing to accept right now, treat this as a command invocation

-- | End targeting mode, accepting the current position or not.
endTargeting :: MonadClientUI m => Bool -> m ()
endTargeting accept = do
  when accept $ do
    leader <- getLeaderUI
    target <- getsClient $ getTarget leader
    scursor <- getsClient scursor
    (lid, _) <- viewedLevel
    side <- getsClient sside
    fact <- getsState $ (EM.! side) . sfaction
    ms <- getsState $ actorNotProjAssocs (isAtWar fact) lid
    case target of
      Just TEnemy{} -> do
        -- If in enemy targeting mode, switch to the enemy under
        -- the current cursor position, if any.
        case find (\ (_im, m) -> Just (bpos m) == scursor) ms of
          Just (im, m)  ->
            let tgt = Just $ TEnemy im (bpos m)
            in modifyClient $ updateTarget leader (const $ tgt)
          Nothing -> return ()
      _ -> case scursor of
        Nothing -> return ()
        Just cpos ->
          modifyClient $ updateTarget leader (const $ Just $ TPos cpos)
  if accept
    then endTargetingMsg
    else msgAdd "targeting canceled"
  modifyClient $ \cli -> cli { stgtMode = Nothing }

endTargetingMsg :: MonadClientUI m => m ()
endTargetingMsg = do
  Kind.COps{coactor} <- getsState scops
  leader <- getLeaderUI
  pbody <- getsState $ getActorBody leader
  target <- getsClient $ getTarget leader
  loc <- getState
  Level{lxsize} <- cursorLevel
  let targetMsg = case target of
                    Just (TEnemy a _ll) ->
                      if memActor a (blid pbody) loc
                      then partActor coactor $ getActorBody a loc
                      else "a fear of the past"
                    Just (TPos pos) ->
                      MU.Text $ "position" <+> showPoint lxsize pos
                    Nothing -> "current cursor position continuously"
  msgAdd $ makeSentence
    [MU.SubjectVerbSg (partActor coactor pbody) "target", targetMsg]

-- * Clear

-- | Clear current messages, show the next screen if any.
clearHuman :: Monad m => m ()
clearHuman = return ()

-- * History

-- TODO: add times from all levels. Also, show time spend on this level alone.
-- "You survived for x turns (y turns on this level)"
historyHuman :: MonadClientUI m => WriterT Slideshow m ()
historyHuman = do
  history <- getsClient shistory
  arena <- getArenaUI
  local <- getsState $ getLocalTime arena
  global <- getsState stime
  let  msg = makeSentence
        [ "You survived for"
        , MU.NWs (global `timeFit` timeTurn) "half-second turn"
        , "(this level:"
        , MU.Text (showT (local `timeFit` timeTurn)) MU.:> ")" ]
        <+> "Past messages:"
  slides <- overlayToSlideshow msg $ renderHistory history
  tell slides

-- * Help

-- | Display command help.
helpHuman :: MonadClientUI m => WriterT Slideshow m ()
helpHuman = do
  keyb <- askBinding
  tell $ keyHelp keyb
