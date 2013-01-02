{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Item UI code with the 'Action' type and everything it depends on
-- that is not already in Action.hs and EffectAction.hs.
-- This file should not depend on Actions.hs.
-- TODO: Add an export list and document after it's rewritten according to #17.
module Game.LambdaHack.ItemAction where

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
import Game.LambdaHack.Binding
import qualified Game.LambdaHack.Command as Command
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.DungeonState
import qualified Game.LambdaHack.Effect as Effect
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Key as K
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

-- TODO: When inventory is displayed, let TAB switch the player (without
-- announcing that) and show the inventory of the new player.
-- | Display inventory
inventory :: MonadClientRO m => WriterT Slideshow m ()
inventory = do
  Kind.COps{coactor} <- getsLocal scops
  pbody <- getsLocal getPlayerBody
  items <- getsLocal getPlayerItem
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

-- | Let the player choose any item with a given group name.
-- Note that this does not guarantee the chosen item belongs to the group,
-- as the player can override the choice.
getGroupItem :: MonadClient m
             => [Item]   -- ^ all objects in question
             -> MU.Part  -- ^ name of the group
             -> [Char]   -- ^ accepted item symbols
             -> Text     -- ^ prompt
             -> Text     -- ^ how to refer to the collection of objects
             -> m Item
getGroupItem is object syms prompt packName = do
  let choice i = jsymbol i `elem` syms
      header = makePhrase [MU.Capitalize (MU.Ws object)]
  getItem prompt choice header is packName

retarget :: MonadClient m => WriterT Slideshow m ()
retarget = do
  ppos <- getsLocal (bpos . getPlayerBody)
  msgAdd "Last target invalid."
  let upd cursor = cursor {cposition=ppos, ceps=0}
  modifyClient (updateCursor upd)
  targetMonster TgtAuto

-- | Start the monster targeting mode. Cycle between monster targets.
targetMonster :: MonadClient m => TgtMode -> WriterT Slideshow m ()
targetMonster tgtMode = do
  pl        <- getsLocal splayer
  ppos      <- getsLocal (bpos . getPlayerBody)
  sside  <- getsLocal sside
  ms        <- getsLocal (hostileAssocs sside . getArena)
  per       <- askPerception
  lxsize    <- getsLocal (lxsize . getArena)
  target <- getsClient (IM.lookup pl . starget)
  targeting <- getsClient (ctargeting . scursor)
      -- TODO: sort monsters by distance to the player.
  let plms = filter ((/= pl) . fst) ms  -- don't target yourself
      ordPos (_, m) = (chessDist lxsize ppos $ bpos m, bpos m)
      dms = sortBy (comparing ordPos) plms
      (lt, gt) = case target of
            Just (TEnemy n _) | targeting /= TgtOff ->  -- pick the next monster
              let i = fromMaybe (-1) $ findIndex ((== n) . fst) dms
              in splitAt (i + 1) dms
            Just (TEnemy n _) ->  -- try to retarget the old monster
              let i = fromMaybe (-1) $ findIndex ((== n) . fst) dms
              in splitAt i dms
            _ -> (dms, [])  -- target first monster (e.g., number 0)
      gtlt     = gt ++ lt
      seen (_, m) =
        let mpos = bpos m
        in mpos `IS.member` totalVisible per  -- visible by any
           && actorReachesLoc pl mpos per     -- reachable by player
      lf = filter seen gtlt
      tgt = case lf of
              [] -> target  -- no monsters in sight, stick to last target
              (na, nm) : _ -> Just (TEnemy na (bpos nm))  -- pick the next
  -- Register the chosen monster, to pick another on next invocation.
  modifyClient $ updateTarget pl (const tgt)
  setCursor tgtMode

-- | Start the floor targeting mode or reset the cursor position to the player.
targetFloor :: MonadClient m => TgtMode -> WriterT Slideshow m ()
targetFloor tgtMode = do
  ppos      <- getsLocal (bpos . getPlayerBody)
  pl        <- getsLocal splayer
  target <- getsClient (IM.lookup pl . starget)
  targeting <- getsClient (ctargeting . scursor)
  let tgt = case target of
        Just (TEnemy _ _) -> Nothing  -- forget enemy target, keep the cursor
        _ | targeting /= TgtOff -> Just (TPos ppos)  -- double key press: reset cursor
        t -> t  -- keep the target from previous targeting session
  -- Register that we want to target only positions.
  modifyClient $ updateTarget pl (const tgt)
  setCursor tgtMode

-- | Set, activate and display cursor information.
setCursor :: MonadClient m => TgtMode -> WriterT Slideshow m ()
setCursor tgtMode = assert (tgtMode /= TgtOff) $ do
  pos  <- getLocal
  cli  <- getClient
  ppos   <- getsLocal (bpos . getPlayerBody)
  cposLn <- getsLocal sarena
  let upd cursor@Cursor{ctargeting, cposition=cpositionOld, ceps=cepsOld} =
        let cposition =
              fromMaybe ppos (targetToPos cli pos)
            ceps = if cposition == cpositionOld then cepsOld else 0
            newTgtMode = if ctargeting == TgtOff then tgtMode else ctargeting
        in cursor { ctargeting = newTgtMode, cposition, cposLn, ceps }
  modifyClient (updateCursor upd)
  doLook

-- | Tweak the @eps@ parameter of the targeting digital line.
epsIncr :: MonadClient m => Bool -> m ()
epsIncr b = do
  targeting <- getsClient (ctargeting . scursor)
  if targeting /= TgtOff
    then modifyClient $ updateCursor $
           \ c@Cursor{ceps} -> c {ceps = ceps + if b then 1 else -1}
    else neverMind True  -- no visual feedback, so no sense

-- | End targeting mode, accepting the current position or not.
endTargeting :: MonadClient m => Bool -> m ()
endTargeting accept = do
  returnLn <- getsClient (creturnLn . scursor)
  pl <- getsLocal splayer
  target <- getsClient (IM.lookup pl . starget)
  per      <- askPerception
  cpos     <- getsClient (cposition . scursor)
  sside <- getsLocal sside
  ms       <- getsLocal (hostileAssocs sside . getArena)
  -- Return to the original level of the player. Note that this can be
  -- a different level than the one we started targeting at,
  -- if the player was changed while targeting.
  modifyLocal $ \ s -> s {sarena = returnLn}
  modifyClient (updateCursor (\ c -> c { ctargeting = TgtOff }))
  when accept $ do
    case target of
      Just TEnemy{} -> do
        -- If in monster targeting mode, switch to the monster under
        -- the current cursor position, if any.
        let canSee = IS.member cpos (totalVisible per)
        when (accept && canSee) $
          case find (\ (_im, m) -> bpos m == cpos) ms of
            Just (im, m)  ->
              let tgt = Just $ TEnemy im (bpos m)
              in modifyClient $ updateTarget pl (const $ tgt)
            Nothing -> return ()
      _ -> modifyClient $ updateTarget pl (const $ Just $ TPos cpos)
  if accept
    then endTargetingMsg
    else msgAdd "targeting canceled"

endTargetingMsg :: MonadClient m => m ()
endTargetingMsg = do
  Kind.COps{coactor} <- getsLocal scops
  pbody  <- getsLocal getPlayerBody
  pl <- getsLocal splayer
  target <- getsClient (IM.lookup pl . starget)
  state  <- getLocal
  lxsize <- getsLocal (lxsize . getArena)
  let targetMsg = case target of
                    Just (TEnemy a _ll) ->
                      if memActor a state
                      then partActor coactor $ getActor a state
                      else "a fear of the past"
                    Just (TPos pos) ->
                      MU.Text $ "position" <+> showPoint lxsize pos
                    Nothing -> "current cursor position continuously"
  msgAdd $ makeSentence
      [MU.SubjectVerbSg (partActor coactor pbody) "target", targetMsg]

-- | Cancel something, e.g., targeting mode, resetting the cursor
-- to the position of the player. Chosen target is not invalidated.
cancelCurrent :: MonadClient m => WriterT Slideshow m () -> WriterT Slideshow m ()
cancelCurrent h = do
  targeting <- getsClient (ctargeting . scursor)
  if targeting /= TgtOff
    then lift $ endTargeting False
    else h  -- nothing to cancel right now, treat this as a command invocation

-- | Accept something, e.g., targeting mode, keeping cursor where it was.
-- Or perform the default action, if nothing needs accepting.
acceptCurrent :: MonadClient m => WriterT Slideshow m () -> WriterT Slideshow m ()
acceptCurrent h = do
  targeting <- getsClient (ctargeting . scursor)
  if targeting /= TgtOff
    then lift $ endTargeting True
    else h  -- nothing to accept right now, treat this as a command invocation

-- | Clear current messages, show the next screen if any.
clearCurrent :: MonadActionRoot m => m ()
clearCurrent = return ()

-- TODO: I think that player handlers should be wrappers
-- around more general actor handlers, but
-- the actor handlers should be performing
-- specific actions, i.e., already specify the item to be
-- picked up. It doesn't make sense to invoke dialogues
-- for arbitrary actors, and most likely the
-- decision for a monster is based on perceiving
-- a particular item to be present, so it's already
-- known. In actor handlers we should make sure
-- that messages are printed to the player only if the
-- hero can perceive the action.

-- TODO: you can drop an item already on the floor, which works correctly,
-- but is weird and useless.

allObjectsName :: Text
allObjectsName = "Objects"

-- | Let the player choose any item from a list of items.
getAnyItem :: MonadClient m
           => Text    -- ^ prompt
           -> [Item]  -- ^ all items in question
           -> Text    -- ^ how to refer to the collection of items
           -> m Item
getAnyItem prompt = getItem prompt (const True) allObjectsName

data ItemDialogState = INone | ISuitable | IAll deriving Eq

-- | Let the player choose a single, preferably suitable,
-- item from a list of items.
getItem :: MonadClient m
        => Text            -- ^ prompt message
        -> (Item -> Bool)  -- ^ which items to consider suitable
        -> Text            -- ^ how to describe suitable items
        -> [Item]          -- ^ all items in question
        -> Text            -- ^ how to refer to the collection of items
        -> m Item
getItem prompt p ptext is0 isn = do
  lvl  <- getsLocal getArena
  body <- getsLocal getPlayerBody
  let pos = bpos body
      tis = lvl `atI` pos
      floorFull = not $ null tis
      (floorMsg, floorKey) | floorFull = (", -", [K.Char '-'])
                           | otherwise = ("", [])
      isp = filter p is0
      bestFull = not $ null isp
      (bestMsg, bestKey)
        | bestFull =
          let bestLetter = maybe "" (\ l -> "(" <> T.singleton l <> ")") $
                             jletter $ maximumBy cmpItemLM isp
          in (", RET" <> bestLetter, [K.Return])
        | otherwise = ("", [])
      cmpItemLM i1 i2 = cmpLetterMaybe (jletter i1) (jletter i2)
      keys ims =
        let mls = mapMaybe jletter ims
            ks = bestKey ++ floorKey ++ [K.Char '?'] ++ map K.Char mls
        in zip ks $ repeat K.NoModifier
      choice ims =
        if null ims
        then "[?" <> floorMsg
        else let mls = mapMaybe jletter ims
                 r = letterRange mls
             in "[" <> r <> ", ?" <> floorMsg <> bestMsg
      ask = do
        when (null is0 && null tis) $
          abortWith "Not carrying anything."
        perform INone
      perform itemDialogState = do
        let (ims, imsOver, msg) = case itemDialogState of
              INone     -> (isp, [], prompt)
              ISuitable -> (isp, isp, ptext <+> isn <> ".")
              IAll      -> (is0, is0, allObjectsName <+> isn <> ".")
        disco <- getsLocal sdisco
        io <- itemOverlay disco True imsOver
        (command, modifier) <-
          displayChoiceUI (msg <+> choice ims) io (keys ims)
        assert (modifier == K.NoModifier) $
          case command of
            K.Char '?' -> case itemDialogState of
              INone -> perform ISuitable
              ISuitable | ptext /= allObjectsName -> perform IAll
              _ -> perform INone
            K.Char '-' | floorFull ->
              -- TODO: let player select item
              return $ maximumBy cmpItemLM tis
            K.Char l | l `elem` mapMaybe jletter ims ->
              let mitem = find (maybe False (== l) . jletter) ims
              in return $ fromJust mitem
            K.Return | bestFull ->
              return $ maximumBy cmpItemLM isp
            k -> assert `failure` "perform: unexpected key:" <+> showT k
  ask

moveCursor :: MonadClient m => Vector -> Int -> WriterT Slideshow m ()
moveCursor dir n = do
  lxsize <- getsLocal (lxsize . getArena)
  lysize <- getsLocal (lysize . getArena)
  let upd cursor =
        let shiftB loc =
              shiftBounded lxsize (1, 1, lxsize - 2, lysize - 2) loc dir
            cpos = iterate shiftB (cposition cursor) !! n
        in cursor { cposition = cpos }
  modifyClient (updateCursor upd)
  doLook

ifRunning :: MonadClientRO m => ((Vector, Int) -> m a) -> m a -> m a
ifRunning t e = do
  srunning <- getsClient srunning
  maybe e t srunning

-- | Change the displayed level in targeting mode to (at most)
-- k levels shallower. Enters targeting mode, if not already in one.
tgtAscend :: MonadClient m => Int -> WriterT Slideshow m ()
tgtAscend k = do
  Kind.COps{cotile} <- getsLocal scops
  cursor <- getsClient scursor
  targeting <- getsClient (ctargeting . scursor)
  sarena <- getsLocal sarena
  lvl       <- getsLocal getArena
  st        <- getLocal
  depth     <- getsLocal sdepth
  let loc = cposition cursor
      tile = lvl `at` loc
      rightStairs =
        k ==  1 && Tile.hasFeature cotile (F.Cause Effect.Ascend)  tile ||
        k == -1 && Tile.hasFeature cotile (F.Cause Effect.Descend) tile
  if rightStairs  -- stairs, in the right direction
    then case whereTo st k of
      Nothing ->  -- we are at the "end" of the dungeon
        abortWith "no more levels in this direction"
      Just (nln, npos) ->
        assert (nln /= sarena `blame` (nln, "stairs looped")) $ do
          modifyLocal $ \ s -> s {sarena = nln}
          -- Do not freely reveal the other end of the stairs.
          clvl <- getsLocal getArena
          let upd cur =
                let cposition =
                      if Tile.hasFeature cotile F.Exit (clvl `at` npos)
                      then npos  -- already know as an exit, focus on it
                      else loc   -- unknow, do not reveal the position
                in cur { cposition, cposLn = nln }
          modifyClient (updateCursor upd)
    else do  -- no stairs in the right direction
      let n = levelNumber sarena
          nln = levelDefault $ min depth $ max 1 $ n - k
      when (nln == sarena) $ abortWith "no more levels in this direction"
      modifyLocal $ \ s -> s {sarena = nln}
      let upd cur = cur {cposLn = nln}
      modifyClient (updateCursor upd)
  when (targeting == TgtOff) $ do
    let upd cur = cur {ctargeting = TgtExplicit}
    modifyClient (updateCursor upd)
  doLook

-- | Display command help.
displayHelp :: MonadClient m => WriterT Slideshow m ()
displayHelp = do
  keyb <- askBinding
  tell $ keyHelp keyb

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

-- TODO: add times from all levels. Also, show time spend on this level alone.
-- "You survived for x turns (y turns on this level)"
displayHistory :: MonadClient m => WriterT Slideshow m ()
displayHistory = do
  StateClient{shistory} <- getClient
  time <- getsLocal getTime
  let turn = time `timeFit` timeTurn
      msg = makeSentence [ "You spent on this level"
                         , MU.NWs turn "half-second turn" ]
            <+> "Past messages:"
  slides <- overlayToSlideshow msg $ renderHistory shistory
  tell slides

-- | Make the item known to the player.
discover :: MonadClient m => Discoveries -> Item -> m ()
discover discoS i = do
  Kind.COps{coitem} <- getsLocal scops
  oldDisco <- getsLocal sdisco
  let ix = jkindIx i
      ik = discoS M.! ix
  unless (ix `M.member` oldDisco) $ do
    modifyLocal (updateDiscoveries (M.insert ix ik))
    disco <- getsLocal sdisco
    let (object1, object2) = partItem coitem oldDisco i
        msg = makeSentence
          [ "the", MU.SubjectVerbSg (MU.Phrase [object1, object2])
                                    "turn out to be"
          , partItemAW coitem disco i ]
    msgAdd msg

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

stopRunning :: MonadClient m => m ()
stopRunning = modifyClient (\ cli -> cli { srunning = Nothing })

-- | Perform look around in the current position of the cursor.
doLook :: MonadClient m => WriterT Slideshow m ()
doLook = do
  cops@Kind.COps{coactor} <- getsLocal scops
  p    <- getsClient (cposition . scursor)
  loc  <- getLocal
  clvl   <- getsLocal getArena
  hms    <- getsLocal (lactor . getArena)
  per    <- askPerception
  pl     <- getsLocal splayer
  target <- getsClient (IM.lookup pl . starget)
  targeting <- getsClient (ctargeting . scursor)
  assert (targeting /= TgtOff) $ do
    let canSee = IS.member p (totalVisible per)
        ihabitant | canSee = find (\ m -> bpos m == p) (IM.elems hms)
                  | otherwise = Nothing
        monsterMsg =
          maybe "" (\ m -> makeSentence
                           [MU.SubjectVerbSg (partActor coactor m) "be here"])
                   ihabitant
        vis | not $ p `IS.member` totalVisible per =
                " (not visible)"  -- by party
            | actorReachesLoc pl p per = ""
            | otherwise = " (not reachable)"  -- by hero
        mode = case target of
                 Just TEnemy{} -> "[targeting monster" <> vis <> "]"
                 Just TPos{}   -> "[targeting position" <> vis <> "]"
                 Nothing       -> "[targeting current" <> vis <> "]"
        -- Show general info about current p.
        lookMsg = mode <+> lookAt cops True canSee loc p monsterMsg
        -- Check if there's something lying around at current p.
        is = clvl `atI` p
    modifyClient (\st -> st {slastKey = Nothing})
    if length is <= 2
      then do
        slides <- promptToSlideshow lookMsg
        tell slides
      else do
       disco <- getsLocal sdisco
       io <- itemOverlay disco False is
       slides <- overlayToSlideshow lookMsg io
       tell slides
