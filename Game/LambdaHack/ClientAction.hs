{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Semantics of 'Command.Cmd' client commands that do not return
-- server commands. None of such commands takes game time.
-- TODO: document
module Game.LambdaHack.ClientAction where

-- Cabal
import qualified Paths_LambdaHack as Self (version)

import Control.Monad
import Control.Monad.Writer.Strict (WriterT, lift, tell)
import Data.Function
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Action hiding (MonadAction, MonadActionRO, MonadServer,
                               MonadServerRO)
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Animation (blockHit, deathBody, twirlSplash)
import Game.LambdaHack.Binding
import qualified Game.LambdaHack.Command as Command hiding (CmdSer)
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Draw
import Game.LambdaHack.DungeonState
import qualified Game.LambdaHack.Effect as Effect
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

-- + Semantics of client commands that do not return server commands

-- ** Project

retarget :: MonadClient m => WriterT Slideshow m ()
retarget = do
  stgtMode <- getsClient stgtMode
  assert (stgtMode == TgtOff) $ do
    arena <- getsLocal sarena
    ppos <- getsLocal (bpos . getLeaderBody)
    msgAdd "Last target invalid."
    modifyClient $ \cli -> cli {scursor = ppos, seps = 0}
    targetMonster $ TgtAuto arena

-- ** Move and Run

moveCursor :: MonadClient m => Vector -> Int -> WriterT Slideshow m ()
moveCursor dir n = do
  Level{lxsize, lysize} <- cursorLevel
  let shiftB pos = shiftBounded lxsize (1, 1, lxsize - 2, lysize - 2) pos dir
  modifyClient $ \cli -> cli {scursor = iterate shiftB (scursor cli) !! n}
  doLook

cursorLevel :: MonadClientRO m => m Level
cursorLevel = do
  dungeon <- getsLocal sdungeon
  stgtMode <- getsClient stgtMode
  let tgtId = case stgtMode of
        TgtOff -> assert `failure` "not targetting right now"
        _ -> tgtLevelId stgtMode
  return $! dungeon M.! tgtId

viewedLevel :: MonadClientRO m => m (LevelId, Level)
viewedLevel = do
  arena <- getsLocal sarena
  dungeon <- getsLocal sdungeon
  stgtMode <- getsClient stgtMode
  let tgtId = case stgtMode of
        TgtOff -> arena
        _ -> tgtLevelId stgtMode
  return $! (tgtId, dungeon M.! tgtId)

-- TODO: probably move somewhere (Level?)
-- | Produces a textual description of the terrain and items at an already
-- explored position. Mute for unknown positions.
-- The detailed variant is for use in the targeting mode.
lookAt :: MonadClient m
       => Bool       -- ^ detailed?
       -> Bool       -- ^ can be seen right now?
       -> Point      -- ^ position to describe
       -> Text       -- ^ an extra sentence to print
       -> m Text
lookAt detailed canSee pos msg = do
  Kind.COps{coitem, cotile=Kind.Ops{oname}} <- getsLocal scops
  (_, lvl) <- viewedLevel
  let is = lvl `atI` pos
      prefixSee = MU.Text $ if canSee then "you see" else "you remember"
  disco <- getsLocal sdisco
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
-- Assumes targeting mode.
doLook :: MonadClient m => WriterT Slideshow m ()
doLook = do
  Kind.COps{coactor} <- getsLocal scops
  p <- getsClient scursor
  per <- askPerception
  leader <- getsLocal sleader
  target <- getsClient $ getTarget leader
  lvl <- cursorLevel
  let hms = lactor lvl
      canSee = IS.member p (totalVisible per)
      ihabitant | canSee = find (\ m -> bpos m == p) (IM.elems hms)
                | otherwise = Nothing
      monsterMsg =
        maybe "" (\ m -> makeSentence
                         [MU.SubjectVerbSg (partActor coactor m) "be here"])
                 ihabitant
      vis | not $ p `IS.member` totalVisible per = " (not visible)"
          | actorSeesLoc per leader p = ""
          | otherwise = " (not visible by you)"
      mode = case target of
               Just TEnemy{} -> "[targeting monster" <> vis <> "]"
               Just TPos{}   -> "[targeting position" <> vis <> "]"
               Nothing       -> "[targeting current" <> vis <> "]"
      -- Check if there's something lying around at current p.
      is = lvl `atI` p
  -- Show general info about current p.
  lookMsg <- lookAt True canSee p monsterMsg
  modifyClient (\st -> st {slastKey = Nothing})
  if length is <= 2
    then do
      slides <- promptToSlideshow (mode <+> lookMsg)
      tell slides
    else do
     disco <- getsLocal sdisco
     io <- itemOverlay disco False is
     slides <- overlayToSlideshow (mode <+> lookMsg) io
     tell slides

-- GameSave doesn't take time, but needs the server, so it's defined elsewhere.

-- ** Inventory

-- TODO: When inventory is displayed, let TAB switch the leader (without
-- announcing that) and show the inventory of the new leader.
-- | Display inventory
inventory :: MonadClientRO m => WriterT Slideshow m ()
inventory = do
  Kind.COps{coactor} <- getsLocal scops
  pbody <- getsLocal getLeaderBody
  items <- getsLocal getLeaderItem
  disco <- getsLocal sdisco
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

-- | Create a list of item names.
itemOverlay :: MonadClientRO m
            => Discoveries -> Bool -> [Item] -> m Overlay
itemOverlay disco sorted is = do
  Kind.COps{coitem} <- getsLocal scops
  let items | sorted = sortBy (cmpLetterMaybe `on` jletter) is
            | otherwise = is
      pr i = makePhrase [ letterLabel (jletter i)
                        , partItemNWs coitem disco i ]
             <> " "
  return $ map pr items

-- ** TgtFloor

-- | Start the floor targeting mode or reset the cursor position to the leader.
targetFloor :: MonadClient m => TgtMode -> WriterT Slideshow m ()
targetFloor stgtModeNew = do
  ppos <- getsLocal (bpos . getLeaderBody)
  leader <- getsLocal sleader
  target <- getsClient $ getTarget leader
  stgtMode <- getsClient stgtMode
  let tgt = case target of
        Just (TEnemy _ _) -> Nothing  -- forget enemy target, keep the cursor
        _ | stgtMode /= TgtOff ->
          Just (TPos ppos)  -- double key press: reset cursor
        t -> t  -- keep the target from previous targeting session
  -- Register that we want to target only positions.
  modifyClient $ updateTarget leader (const tgt)
  setCursor stgtModeNew

-- | Set, activate and display cursor information.
setCursor :: MonadClient m => TgtMode -> WriterT Slideshow m ()
setCursor stgtModeNew = assert (stgtModeNew /= TgtOff) $ do
  loc <- getLocal
  cli <- getClient
  ppos <- getsLocal (bpos . getLeaderBody)
  stgtModeOld <- getsClient stgtMode
  scursorOld <- getsClient scursor
  sepsOld <- getsClient seps
  let scursor = fromMaybe ppos (targetToPos cli loc)
      seps = if scursor == scursorOld then sepsOld else 0
      stgtMode = if stgtModeOld == TgtOff
                   then stgtModeNew
                   else stgtModeOld
  modifyClient $ \cli2 -> cli2 {scursor, seps, stgtMode}
  doLook

-- ** TgtEnemy

-- | Start the monster targeting mode. Cycle between monster targets.
targetMonster :: MonadClient m => TgtMode -> WriterT Slideshow m ()
targetMonster stgtModeNew = do
  leader <- getsLocal sleader
  ppos <- getsLocal (bpos . getLeaderBody)
  side <- getsLocal sside
  per <- askPerception
  target <- getsClient $ getTarget leader
  -- TODO: sort monsters by distance to the leader.
  stgtMode <- getsClient stgtMode
  (_, lvl@Level{lxsize}) <- viewedLevel
  let ms = hostileAssocs side lvl
      plms = filter ((/= leader) . fst) ms  -- don't target yourself
      ordPos (_, m) = (chessDist lxsize ppos $ bpos m, bpos m)
      dms = sortBy (comparing ordPos) plms
      (lt, gt) = case target of
            Just (TEnemy n _) | stgtMode /= TgtOff ->  -- pick next monster
              let i = fromMaybe (-1) $ findIndex ((== n) . fst) dms
              in splitAt (i + 1) dms
            Just (TEnemy n _) ->  -- try to retarget the old monster
              let i = fromMaybe (-1) $ findIndex ((== n) . fst) dms
              in splitAt i dms
            _ -> (dms, [])  -- target first monster (e.g., number 0)
      gtlt = gt ++ lt
      seen (_, m) =
        let mpos = bpos m            -- it is remembered by faction
        in actorSeesLoc per leader mpos  -- is it visible by actor?
      lf = filter seen gtlt
      tgt = case lf of
              [] -> target  -- no monsters in sight, stick to last target
              (na, nm) : _ -> Just (TEnemy na (bpos nm))  -- pick the next
  -- Register the chosen monster, to pick another on next invocation.
  modifyClient $ updateTarget leader (const tgt)
  setCursor stgtModeNew

-- ** TgtAscend

-- | Change the displayed level in targeting mode to (at most)
-- k levels shallower. Enters targeting mode, if not already in one.
tgtAscend :: MonadClient m => Int -> WriterT Slideshow m ()
tgtAscend k = do
  Kind.COps{cotile} <- getsLocal scops
  loc <- getLocal
  depth <- getsLocal sdepth
  cpos <- getsClient scursor
  (tgtId, lvl) <- viewedLevel
  let tile = lvl `at` cpos
      rightStairs =
        k ==  1 && Tile.hasFeature cotile (F.Cause Effect.Ascend)  tile ||
        k == -1 && Tile.hasFeature cotile (F.Cause Effect.Descend) tile
  if rightStairs  -- stairs, in the right direction
    then case whereTo loc tgtId k of
      Nothing ->  -- we are at the "end" of the dungeon
        abortWith "no more levels in this direction"
      Just (nln, npos) ->
        assert (nln /= tgtId `blame` (nln, "stairs looped")) $ do
          -- Do not freely reveal the other end of the stairs.
          let scursor =
                if Tile.hasFeature cotile F.Exit (lvl `at` npos)
                then npos  -- already know as an exit, focus on it
                else cpos  -- unknown, do not reveal
          modifyClient $ \cli -> cli {scursor}
          setTgtId nln
    else do  -- no stairs in the right direction
      let n = levelNumber tgtId
          nln = levelDefault $ min depth $ max 1 $ n - k
      when (nln == tgtId) $ abortWith "no more levels in this direction"
      setTgtId nln
  doLook

setTgtId :: MonadClient m => LevelId -> m ()
setTgtId nln = do
  stgtMode <- getsClient stgtMode
  case stgtMode of
    TgtAuto _ -> modifyClient $ \cli -> cli {stgtMode = TgtAuto nln}
    _ -> modifyClient $ \cli -> cli {stgtMode = TgtExplicit nln}

-- ** EpsIncr

-- | Tweak the @eps@ parameter of the targeting digital line.
epsIncr :: MonadClient m => Bool -> m ()
epsIncr b = do
  stgtMode <- getsClient stgtMode
  if stgtMode /= TgtOff
    then modifyClient $ \cli -> cli {seps = seps cli + if b then 1 else -1}
    else neverMind True  -- no visual feedback, so no sense

-- ** Cancel

-- | Cancel something, e.g., targeting mode, resetting the cursor
-- to the position of the leader. Chosen target is not invalidated.
cancelCurrent :: MonadClient m => WriterT Slideshow m () -> WriterT Slideshow m ()
cancelCurrent h = do
  stgtMode <- getsClient stgtMode
  if stgtMode /= TgtOff
    then lift $ endTargeting False
    else h  -- nothing to cancel right now, treat this as a command invocation

-- | Display the main menu.
displayMainMenu :: MonadClient m => WriterT Slideshow m ()
displayMainMenu = do
  Kind.COps{corule} <- getsLocal scops
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
        let showKD cmd key = (showT key, Command.cmdDescription cmd)
            revLookup cmd =
              maybe ("", "") (showKD cmd . fst) $ M.lookup cmd krevMap
            cmds = [Command.GameSave, Command.GameExit,
                   Command.GameRestart, Command.Help]
        in map revLookup cmds ++ [(fst (revLookup Command.Clear), "continue")]
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

-- ** Accept

-- | Accept something, e.g., targeting mode, keeping cursor where it was.
-- Or perform the default action, if nothing needs accepting.
acceptCurrent :: MonadClient m => WriterT Slideshow m () -> WriterT Slideshow m ()
acceptCurrent h = do
  stgtMode <- getsClient stgtMode
  if stgtMode /= TgtOff
    then lift $ endTargeting True
    else h  -- nothing to accept right now, treat this as a command invocation

-- | End targeting mode, accepting the current position or not.
endTargeting :: MonadClient m => Bool -> m ()
endTargeting accept = do
  when accept $ do
    leader <- getsLocal sleader
    target <- getsClient $ getTarget leader
    cpos <- getsClient scursor
    side <- getsLocal sside
    lvl <- cursorLevel
    let ms = hostileAssocs side lvl
    case target of
      Just TEnemy{} -> do
        -- If in monster targeting mode, switch to the monster under
        -- the current cursor position, if any.
        case find (\ (_im, m) -> bpos m == cpos) ms of
          Just (im, m)  ->
            let tgt = Just $ TEnemy im (bpos m)
            in modifyClient $ updateTarget leader (const $ tgt)
          Nothing -> return ()
      _ -> modifyClient $ updateTarget leader (const $ Just $ TPos cpos)
  if accept
    then endTargetingMsg
    else msgAdd "targeting canceled"
  modifyClient $ \cli -> cli { stgtMode = TgtOff }

endTargetingMsg :: MonadClient m => m ()
endTargetingMsg = do
  Kind.COps{coactor} <- getsLocal scops
  pbody <- getsLocal getLeaderBody
  leader <- getsLocal sleader
  target <- getsClient $ getTarget leader
  loc <- getLocal
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

-- ** Clear

-- | Clear current messages, show the next screen if any.
clearCurrent :: MonadActionRoot m => m ()
clearCurrent = return ()

-- ** History

-- TODO: add times from all levels. Also, show time spend on this level alone.
-- "You survived for x turns (y turns on this level)"
displayHistory :: MonadClient m => WriterT Slideshow m ()
displayHistory = do
  history <- getsClient shistory
  time <- getsLocal getTime
  let turn = time `timeFit` timeTurn
      msg = makeSentence [ "You spent on this level"
                         , MU.NWs turn "half-second turn" ]
            <+> "Past messages:"
  slides <- overlayToSlideshow msg $ renderHistory history
  tell slides

-- CfgDump doesn't take time, but needs the server, so it's defined elsewhere.

-- ** HeroCycle

-- | Switches current hero to the next hero on the level, if any, wrapping.
-- We cycle through at most 10 heroes (\@, 1--9).
cycleHero :: MonadClient m => m ()
cycleHero = do
  leader <- getsLocal sleader
  s  <- getLocal
  hs <- heroesAfterPl
  case filter (flip memActor s . snd) hs of
    [] -> abortWith "Cannot select any other hero on this level."
    (nl, np) : _ -> selectLeader nl np
                      >>= assert `trueM` (leader, nl, np, "hero duplicated")

-- | Select a faction leader. Switch level, if needed.
-- False, if nothing to do. Should only be invoked as a direct result
-- of a player action (leader death just sets sleader to -1).
selectLeader :: MonadClient m => LevelId -> ActorId -> m Bool
selectLeader nln actor = do
  Kind.COps{coactor} <- getsLocal scops
  leader <- getsLocal sleader
  stgtMode <- getsClient stgtMode
  if actor == leader
    then return False -- already selected
    else do
      modifyLocal $ updateSelected actor nln
      -- Move the cursor, if active, to the new level.
      when (stgtMode /= TgtOff) $ setTgtId nln
      -- Don't continue an old run, if any.
      stopRunning
      -- Announce.
      pbody <- getsLocal getLeaderBody
      msgAdd $ makeSentence [partActor coactor pbody, "selected"]
      return True

stopRunning :: MonadClient m => m ()
stopRunning = modifyClient (\ cli -> cli { srunning = Nothing })

heroesAfterPl :: MonadClientRO m => m [(LevelId, ActorId)]
heroesAfterPl = do
  leader <- getsLocal sleader
  s  <- getLocal
  let hs = map (tryFindHeroK s) [0..9]
      i = fromMaybe (-1) $ findIndex ((== Just leader) . fmap snd) hs
      (lt, gt) = (take i hs, drop (i + 1) hs)
  return $ catMaybes gt ++ catMaybes lt

-- ** HeroBack

-- | Switches current hero to the previous hero in the whole dungeon,
-- if any, wrapping. We cycle through at most 10 heroes (\@, 1--9).
backCycleHero :: MonadClient m => m ()
backCycleHero = do
  leader <- getsLocal sleader
  hs <- heroesAfterPl
  case reverse hs of
    [] -> abortWith "No other hero in the party."
    (nl, np) : _ -> selectLeader nl np
                      >>= assert `trueM` (leader, nl, np, "hero duplicated")

-- ** Help

-- | Display command help.
displayHelp :: MonadClientRO m => WriterT Slideshow m ()
displayHelp = do
  keyb <- askBinding
  tell $ keyHelp keyb

-- ** SelectHero

selectHero :: MonadClient m => Int -> m ()
selectHero k = do
  loc <- getLocal
  case tryFindHeroK loc k of
    Nothing  -> abortWith "No such member of the party."
    Just (lid, aid) -> void $ selectLeader lid aid

-- TODO: move somewhere

-- | Abstract syntax of client commands.
data CmdCli =
    PickupCli ActorId Item Item
  | ShowItemsCli Discoveries Msg [Item]
  | AnimateDeathCli ActorId
  | SelectLeaderCli ActorId LevelId
  | DiscoverCli (Kind.Id ItemKind) Item
  deriving Show

-- | The semantics of client commands.
cmdCli :: MonadClient m => CmdCli -> m Bool
cmdCli cmd = case cmd of
  PickupCli aid i ni -> pickupCli aid i ni >> return True  -- TODO: GADT?
  ShowItemsCli discoS loseMsg items -> do
    io <- itemOverlay discoS True items
    slides <- overlayToSlideshow loseMsg io
    getManyConfirms [] slides
  AnimateDeathCli aid -> animateDeathCli aid
  SelectLeaderCli aid lid -> selectLeader lid aid
  DiscoverCli ik i -> discoverCli ik i

pickupCli :: MonadClient m => ActorId -> Item -> Item -> m ()
pickupCli aid i ni = do
  Kind.COps{coactor, coitem} <- getsLocal scops
  body <- getsLocal (getActorBody aid)
  side <- getsLocal sside
  disco <- getsLocal sdisco
  if bfaction body == side
    then msgAdd $ makePhrase [ letterLabel (jletter ni)
                             , partItemNWs coitem disco ni ]
    else msgAdd $ makeSentence
           [ MU.SubjectVerbSg (partActor coactor body) "pick up"
           , partItemNWs coitem disco i ]  -- single, not 'ni'

animateDeathCli :: MonadClient m => ActorId -> m Bool
animateDeathCli target = do
  Kind.COps{coactor} <- getsLocal scops
  pbody <- getsLocal $ getActorBody target
  side <- getsLocal sside
  msgAdd $ makeSentence [MU.SubjectVerbSg (partActor coactor pbody) "die"]
  go <- if bfaction pbody == side
        then displayMore ColorBW ""
        else return False
  recordHistory  -- Prevent repeating the "die" msgs.
  cli <- getClient
  loc <- getLocal
  per <- askPerception
  let animFrs = animate cli loc per $ deathBody (bpos pbody)
  displayFramesPush animFrs
  when (bfaction pbody == side) $
    msgAdd "The survivors carry on."  -- TODO: reset messages at game over not to display it if there are no survivors.
  return go

-- | Make the item known to the player.
discoverCli :: MonadClient m => Kind.Id ItemKind -> Item -> m Bool
discoverCli ik i = do
  Kind.COps{coitem} <- getsLocal scops
  oldDisco <- getsLocal sdisco
  let ix = jkindIx i
  if (ix `M.member` oldDisco)
    then return False
    else do
      modifyLocal (updateDisco (M.insert ix ik))
      disco <- getsLocal sdisco
      let (object1, object2) = partItem coitem oldDisco i
          msg = makeSentence
            [ "the", MU.SubjectVerbSg (MU.Phrase [object1, object2])
                                      "turn out to be"
            , partItemAW coitem disco i ]
      msgAdd msg
      return True
