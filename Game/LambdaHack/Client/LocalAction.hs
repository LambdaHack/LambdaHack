{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Semantics of 'CmdHuman.Cmd' client commands that do not return
-- server commands. None of such commands takes game time.
-- TODO: document
module Game.LambdaHack.Client.LocalAction where

-- Cabal
import qualified Paths_LambdaHack as Self (version)

import Control.Monad
import Control.Monad.Writer.Strict (WriterT, lift, tell)
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version
import qualified NLP.Miniutter.English as MU
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.Binding
import qualified Game.LambdaHack.Client.CmdHuman as CmdHuman
import Game.LambdaHack.Client.State
import Game.LambdaHack.Content.RuleKind
import qualified Game.LambdaHack.Effect as Effect
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

default (Text)

-- * Project

retarget :: MonadClient m => WriterT Slideshow m ()
retarget = do
  stgtMode <- getsClient stgtMode
  assert (isNothing stgtMode) $ do
    arena <- getsState sarena
    msgAdd "Last target invalid."
    modifyClient $ \cli -> cli {scursor = Nothing, seps = 0}
    targetEnemy $ TgtAuto arena

-- * Move and Run

moveCursor :: MonadClient m => Vector -> Int -> WriterT Slideshow m ()
moveCursor dir n = do
  Just leader <- getsClient sleader
  lpos <- getsState $ bpos . getActorBody leader
  scursor <- getsClient scursor
  let cpos = fromMaybe lpos scursor
  Level{lxsize, lysize} <- cursorLevel
  let shiftB pos = shiftBounded lxsize (1, 1, lxsize - 2, lysize - 2) pos dir
  modifyClient $ \cli -> cli {scursor = Just $ iterate shiftB cpos !! n}
  doLook

cursorLevel :: MonadClientRO m => m Level
cursorLevel = do
  dungeon <- getsState sdungeon
  stgtMode <- getsClient stgtMode
  let tgtId =
        maybe (assert `failure` "not targetting right now") tgtLevelId stgtMode
  return $! dungeon M.! tgtId

viewedLevel :: MonadClientRO m => m (LevelId, Level)
viewedLevel = do
  arena <- getsState sarena
  dungeon <- getsState sdungeon
  stgtMode <- getsClient stgtMode
  let tgtId = maybe arena tgtLevelId stgtMode
  return $! (tgtId, dungeon M.! tgtId)

-- TODO: probably move somewhere (Level?)
-- | Produces a textual description of the terrain and items at an already
-- explored position. Mute for unknown positions.
-- The detailed variant is for use in the targeting mode.
lookAt :: MonadClientRO m
       => Bool       -- ^ detailed?
       -> Bool       -- ^ can be seen right now?
       -> Point      -- ^ position to describe
       -> Text       -- ^ an extra sentence to print
       -> m Text
lookAt detailed canSee pos msg = do
  Kind.COps{coitem, cotile=Kind.Ops{oname}} <- getsState scops
  (_, lvl) <- viewedLevel
  let is = lvl `atI` pos
      prefixSee = MU.Text $ if canSee then "you see" else "you remember"
  disco <- getsState sdisco
  let nWs = partItemNWs coitem disco
      isd = case is of
              [] -> ""
              _ | length is <= 2 ->
                makeSentence [prefixSee, MU.WWandW $ map nWs is]
              _ | detailed -> "Objects:"
              _ -> "Objects here."
  if detailed
    then let tile = lvl `at` pos
         in return $! makeSentence [MU.Text $ oname tile] <+> msg <+> isd
    else return $! msg <+> isd

-- | Perform look around in the current position of the cursor.
-- Assumes targeting mode and so assumes that a leader is selected.
doLook :: MonadClient m => WriterT Slideshow m ()
doLook = do
  Kind.COps{coactor} <- getsState scops
  scursor <- getsClient scursor
  per <- askPerception
  Just leader <- getsClient sleader
  lpos <- getsState $ bpos . getActorBody leader
  target <- getsClient $ getTarget leader
  lvl <- cursorLevel
  let p = fromMaybe lpos scursor
      hms = lactor lvl
      canSee = ES.member p (totalVisible per)
      ihabitant | canSee = find (\ m -> bpos m == p) (EM.elems hms)
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
      -- Check if there's something lying around at current p.
      is = lvl `atI` p
  -- Show general info about current p.
  lookMsg <- lookAt True canSee p enemyMsg
  modifyClient (\st -> st {slastKey = Nothing})
  if length is <= 2
    then do
      slides <- promptToSlideshow (mode <+> lookMsg)
      tell slides
    else do
     disco <- getsState sdisco
     io <- itemOverlay disco False is
     slides <- overlayToSlideshow (mode <+> lookMsg) io
     tell slides

-- | Create a list of item names.
itemOverlay :: MonadActionRO m
            => Discoveries -> Bool -> [Item] -> m Overlay
itemOverlay disco sorted is = do
  Kind.COps{coitem} <- getsState scops
  let items | sorted = sortBy (cmpLetterMaybe `on` jletter) is
            | otherwise = is
      pr i = makePhrase [ letterLabel (jletter i)
                        , partItemNWs coitem disco i ]
             <> " "
  return $ map pr items

-- GameSave doesn't take time, but needs the server, so it's defined elsewhere.

-- CfgDump doesn't take time, but needs the server, so it's defined elsewhere.

-- * Inventory

-- TODO: When inventory is displayed, let TAB switch the leader (without
-- announcing that) and show the inventory of the new leader.
-- | Display inventory
inventory :: MonadClientRO m => WriterT Slideshow m ()
inventory = do
  Kind.COps{coactor} <- getsState scops
  Just leader <- getsClient sleader
  pbody <- getsState $ getActorBody leader
  items <- getsState $ getActorItem leader
  disco <- getsState sdisco
  if null items
    then abortWith $ makeSentence
      [ MU.SubjectVerbSg (partActor coactor pbody) "be"
      , "not carrying anything" ]
    else do
      let blurb = makePhrase [MU.Capitalize $
            MU.SubjectVerbSg (partActor coactor pbody) "be carrying:"]
      io <- itemOverlay disco True items
      slides <- overlayToSlideshow blurb io
      tell slides

-- * TgtFloor

-- | Start the floor targeting mode or reset the cursor position to the leader.
targetFloor :: MonadClient m => TgtMode -> WriterT Slideshow m ()
targetFloor stgtModeNew = do
  Just leader <- getsClient sleader
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
setCursor :: MonadClient m => TgtMode -> WriterT Slideshow m ()
setCursor stgtModeNew = do
  loc <- getState
  cli <- getClient
  stgtModeOld <- getsClient stgtMode
  scursorOld <- getsClient scursor
  sepsOld <- getsClient seps
  let scursor = targetToPos cli loc
      seps = if scursor == scursorOld then sepsOld else 0
      stgtMode = if isNothing stgtModeOld
                   then Just stgtModeNew
                   else stgtModeOld
  modifyClient $ \cli2 -> cli2 {scursor, seps, stgtMode}
  doLook

-- * TgtEnemy

-- | Start the enemy targeting mode. Cycle between enemy targets.
targetEnemy :: MonadClient m => TgtMode -> WriterT Slideshow m ()
targetEnemy stgtModeNew = do
  Just leader <- getsClient sleader
  ppos <- getsState (bpos . getActorBody leader)
  per <- askPerception
  target <- getsClient $ getTarget leader
  -- TODO: sort enemies by distance to the leader.
  stgtMode <- getsClient stgtMode
  (_, lvl@Level{lxsize}) <- viewedLevel
  genemy <- getsState $ genemy . getSide
  let ms = actorNotProjAssocs (`elem` genemy) lvl
      plms = filter ((/= leader) . fst) ms  -- don't target yourself
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
tgtAscend :: MonadClient m => Int -> WriterT Slideshow m ()
tgtAscend k = do
  Kind.COps{cotile} <- getsState scops
  dungeon <- getsState sdungeon
  loc <- getState
  cursor <- getsClient scursor
  (tgtId, lvl) <- viewedLevel
  let rightStairs = case cursor of
        Nothing -> False
        Just cpos ->
          let tile = lvl `at` cpos
          in k ==  1 && Tile.hasFeature cotile (F.Cause Effect.Ascend)  tile
          || k == -1 && Tile.hasFeature cotile (F.Cause Effect.Descend) tile
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
epsIncr :: MonadClient m => Bool -> m ()
epsIncr b = do
  stgtMode <- getsClient stgtMode
  if isJust stgtMode
    then modifyClient $ \cli -> cli {seps = seps cli + if b then 1 else -1}
    else neverMind True  -- no visual feedback, so no sense

-- * Cancel

-- | Cancel something, e.g., targeting mode, resetting the cursor
-- to the position of the leader. Chosen target is not invalidated.
cancelCurrent :: MonadClient m => WriterT Slideshow m () -> WriterT Slideshow m ()
cancelCurrent h = do
  stgtMode <- getsClient stgtMode
  if isJust stgtMode
    then lift $ endTargeting False
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
        let showKD cmd key = (showT key, CmdHuman.cmdDescription cmd)
            revLookup cmd =
              maybe ("", "") (showKD cmd . fst) $ M.lookup cmd krevMap
            cmds = [ CmdHuman.GameSave,
                     CmdHuman.GameExit,
                     CmdHuman.GameRestart,
                     CmdHuman.Help
                   ]
        in map revLookup cmds ++ [(fst (revLookup CmdHuman.Clear), "continue")]
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
acceptCurrent :: MonadClient m => WriterT Slideshow m () -> WriterT Slideshow m ()
acceptCurrent h = do
  stgtMode <- getsClient stgtMode
  if isJust stgtMode
    then lift $ endTargeting True
    else h  -- nothing to accept right now, treat this as a command invocation

-- | End targeting mode, accepting the current position or not.
endTargeting :: MonadClient m => Bool -> m ()
endTargeting accept = do
  when accept $ do
    Just leader <- getsClient sleader
    target <- getsClient $ getTarget leader
    scursor <- getsClient scursor
    lvl <- cursorLevel
    genemy <- getsState $ genemy . getSide
    let ms = actorNotProjAssocs (`elem` genemy) lvl
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

endTargetingMsg :: MonadClient m => m ()
endTargetingMsg = do
  Kind.COps{coactor} <- getsState scops
  Just leader <- getsClient sleader
  pbody <- getsState $ getActorBody leader
  target <- getsClient $ getTarget leader
  loc <- getState
  Level{lxsize} <- cursorLevel
  let targetMsg = case target of
                    Just (TEnemy a _ll) ->
                      if memActor a loc
                      then partActor coactor $ getActorBody a loc
                      else "a fear of the past"
                    Just (TPos pos) ->
                      MU.Text $ "position" <+> showPoint lxsize pos
                    Nothing -> "current cursor position continuously"
  msgAdd $ makeSentence
    [MU.SubjectVerbSg (partActor coactor pbody) "target", targetMsg]

-- * Clear

-- | Clear current messages, show the next screen if any.
clearCurrent :: MonadActionAbort m => m ()
clearCurrent = return ()

-- * History

-- TODO: add times from all levels. Also, show time spend on this level alone.
-- "You survived for x turns (y turns on this level)"
displayHistory :: MonadClientRO m => WriterT Slideshow m ()
displayHistory = do
  history <- getsClient shistory
  time <- getsState getTime
  let turn = time `timeFit` timeTurn
      msg = makeSentence [ "You spent on this level"
                         , MU.NWs turn "half-second turn" ]
            <+> "Past messages:"
  slides <- overlayToSlideshow msg $ renderHistory history
  tell slides

-- * MemberCycle

-- | Switches current member to the next on the level, if any, wrapping.
cycleMember :: MonadClient m => m ()
cycleMember = do
  Just leader <- getsClient sleader
  arena <- getsState sarena
  hs <- partyAfterLeader leader
  case filter (\(nl, _) -> nl == arena) hs of
    [] -> abortWith "Cannot select any other member on this level."
    (nl, (np, _)) : _ -> selectLeader np nl
                      >>= assert `trueM` (leader, nl, np, "member duplicated")

partyAfterLeader :: MonadActionRO m
                 => ActorId
                 -> m [(LevelId, (ActorId, Actor))]
partyAfterLeader leader = do
  faction <- getsState $ bfaction . getActorBody leader
  allA <- getsState allActorsAnyLevel
  s <- getState
  let hs9 = catMaybes $ map (tryFindHeroK s faction) [0..9]
      factionA = filter (\(_, (_, body)) -> bfaction body == faction) allA
      hs = hs9 ++ (deleteFirstsBy ((==) `on` fst . snd) factionA hs9)
      i = fromMaybe (-1) $ findIndex ((== leader) . fst . snd) hs
      (lt, gt) = (take i hs, drop (i + 1) hs)
  return $ gt ++ lt

-- | Select a faction leader. Switch level, if needed.
-- False, if nothing to do. Should only be invoked as a direct result
-- of a human player action (leader death just sets sleader to -1).
selectLeader :: MonadClient m => ActorId -> LevelId -> m Bool
selectLeader actor arena = do
  Kind.COps{coactor} <- getsState scops
  leader <- getsClient sleader
  stgtMode <- getsClient stgtMode
  if Just actor == leader
    then return False -- already selected
    else do
      arenaOld <- getsState sarena
      when (arenaOld /= arena) $ do
        modifyClient invalidateSelectedLeader
        modifyState $ updateSelectedArena arena
      loc <- getState
      modifyClient $ updateSelectedLeader actor loc
      -- Move the cursor, if active, to the new level.
      when (isJust stgtMode) $ setTgtId arena
      -- Don't continue an old run, if any.
      stopRunning
      -- Announce.
      pbody <- getsState $ getActorBody actor
      msgAdd $ makeSentence [partActor coactor pbody, "selected"]
      return True

stopRunning :: MonadClient m => m ()
stopRunning = modifyClient (\ cli -> cli { srunning = Nothing })

-- * MemberBack

-- | Switches current member to the previous in the whole dungeon, wrapping.
backCycleMember :: MonadClient m => m ()
backCycleMember = do
  Just leader <- getsClient sleader
  hs <- partyAfterLeader leader
  case reverse hs of
    [] -> abortWith "No other member in the party."
    (nl, (np, _)) : _ -> selectLeader np nl
                      >>= assert `trueM` (leader, nl, np, "member duplicated")

-- * Help

-- | Display command help.
displayHelp :: MonadClientUI m => WriterT Slideshow m ()
displayHelp = do
  keyb <- askBinding
  tell $ keyHelp keyb

-- * SelectHero

selectHero :: MonadClient m => Int -> m ()
selectHero k = do
  side <- getsState sside
  loc <- getState
  case tryFindHeroK loc side k of
    Nothing  -> abortWith "No such member of the party."
    Just (lid, (aid, _)) -> void $ selectLeader aid lid
