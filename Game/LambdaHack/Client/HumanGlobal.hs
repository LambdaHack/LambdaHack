{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Semantics of 'Command.Cmd' client commands that return server commands.
-- A couple of them do not take time, the rest does.
-- TODO: document
module Game.LambdaHack.Client.HumanGlobal
  ( moveRunAid, displaceAid, attackAid, waitHuman, pickupHuman, dropHuman
  , projectAid, applyHuman, alterDirHuman, triggerTileHuman
  , gameRestartHuman, gameExitHuman, gameSaveHuman, cfgDumpHuman
  ) where

import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Function
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.Draw
import Game.LambdaHack.Client.HumanCmd (Trigger (..))
import Game.LambdaHack.Client.HumanLocal
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Key as K
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.ServerCmd
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.TileKind as TileKind
import Game.LambdaHack.Utils.Assert

abortFailure :: MonadClientAbort m => FailureSer -> m a
abortFailure = abortWith . showFailureSer

-- * Move and Run

-- | Actor atttacks an enemy actor or his own projectile.
attackAid :: (MonadClientAbort m, MonadClientUI m)
          => ActorId -> Vector -> ActorId -> m CmdSerTakeTime
attackAid aid dir target = do
  sb <- getsState $ getActorBody aid
  tb <- getsState $ getActorBody target
  sfact <- getsState $ (EM.! bfid sb) . sfactionD
  unless (bproj tb || isAtWar sfact (bfid tb)) $ do
    go <- displayYesNo ColorBW
            "This attack will start a war. Are you sure?"
    unless go $ abortWith "Attack canceled."
  unless (bproj tb || not (isAllied sfact (bfid tb))) $ do
    go <- displayYesNo ColorBW
            "You are bound by an alliance. Really attack?"
    unless go $ abortWith "Attack canceled."
  return $ MoveSer aid dir
  -- Seeing the actor prevents altering a tile under it, but that
  -- does not limit the player, he just doesn't waste a turn
  -- on a failed altering. We don't use AttackSer, because we attack
  -- both visible and invisible actors.

-- | Actor swaps position with another.
displaceAid :: (MonadClientAbort m, MonadClientUI m)
            => ActorId -> Vector -> m CmdSerTakeTime
displaceAid aid dir = do
  cops <- getsState scops
  sb <- getsState $ getActorBody aid
  let lid = blid sb
  lvl <- getsLevel lid id
  let spos = bpos sb           -- source position
      tpos = spos `shift` dir  -- target position
  if accessible cops lvl spos tpos then
    -- Displacing requires full access.
    return $ DisplaceSer aid dir
  else abortFailure RunDisplaceAccess

-- | Actor moves or searches or alters. No visible actor at the position.
moveRunAid :: (MonadClientAbort m, MonadClientUI m)
           => ActorId -> Vector -> m CmdSerTakeTime
moveRunAid aid dir = do
  cops@Kind.COps{cotile} <- getsState scops
  sb <- getsState $ getActorBody aid
  let lid = blid sb
  lvl <- getsLevel lid id
  let spos = bpos sb           -- source position
      tpos = spos `shift` dir  -- target position
      t = lvl `at` tpos
  if accessible cops lvl spos tpos then
    -- Movement requires full access.
    return $ MoveSer aid dir
    -- The potential invisible actor is hit. War is started without asking.
  else if not $ EM.null $ lvl `atI` tpos then
    abortFailure AlterBlockItem
  else if Tile.hasFeature cotile F.Suspect t
          || Tile.hasFeature cotile F.Openable t then  -- TODO: ChangeTo=Alter
    -- No access, so search and/or alter the tile.
    return $ AlterSer aid dir
    -- We don't use MoveSer, because we don't hit invisible actors here.
    -- The potential invisible actor, e.g., in a wall or in
    -- an inaccessible doorway, is made known, taking a turn.
    -- If server performed an attack for free on the invisible actor anyway,
    -- the player (or AI) would be tempted to repeatedly
    -- hit random walls in hopes of killing a monster lurking within.
    -- If the action had a cost, misclicks would incur the cost, too.
    -- Right now the player may repeatedly alter tiles trying to learn
    -- about invisible pass-wall actors, but it costs a turn
    -- and does not harm the invisible actors, so it's not tempting.
  else
    -- Ignore a known boring, not accessible tile.
    neverMind True

-- * Wait

-- | Leader waits a turn (and blocks, etc.).
waitHuman :: MonadClientUI m => m CmdSerTakeTime
waitHuman = do
  leader <- getLeaderUI
  return $ WaitSer leader

-- * Pickup

pickupHuman :: (MonadClientAbort m, MonadClientUI m) => m CmdSerTakeTime
pickupHuman = do
  leader <- getLeaderUI
  body <- getsState $ getActorBody leader
  lvl <- getsLevel (blid body) id
  -- Check if something is here to pick up. Items are never invisible.
  case EM.minViewWithKey $ lvl `atI` bpos body of
    Nothing -> abortWith "nothing here"
    Just ((iid, k), _) ->  do  -- pick up first item; TODO: let pl select item
      item <- getsState $ getItemBody iid
      let l = if jsymbol item == '$' then Just $ InvChar '$' else Nothing
      case assignLetter iid l body of
        Just l2 -> return $ PickupSer leader iid k l2
        Nothing -> abortWith "cannot carry any more"

-- * Drop

-- TODO: you can drop an item already on the floor, which works correctly,
-- but is weird and useless.
-- | Drop a single item.
dropHuman :: (MonadClientAbort m, MonadClientUI m) => m CmdSerTakeTime
dropHuman = do
  -- TODO: allow dropping a given number of identical items.
  Kind.COps{coitem} <- getsState scops
  leader <- getLeaderUI
  bag <- getsState $ getActorBag leader
  inv <- getsState $ getActorInv leader
  ((iid, item), (_, container)) <-
    getAnyItem leader "What to drop?" bag inv "in inventory"
  case container of
    CFloor{} -> neverMind True
    CActor aid _ -> do
      assert (aid == leader) skip
      disco <- getsClient sdisco
      subject <- partAidLeader leader
      msgAdd $ makeSentence
        [ MU.SubjectVerbSg subject "drop"
        , partItemWs coitem disco 1 item ]
      return $ DropSer leader iid

allObjectsName :: Text
allObjectsName = "Objects"

-- | Let the human player choose any item from a list of items.
getAnyItem :: (MonadClientAbort m, MonadClientUI m)
           => ActorId
           -> Text     -- ^ prompt
           -> ItemBag  -- ^ all items in question
           -> ItemInv  -- ^ inventory characters
           -> Text     -- ^ how to refer to the collection of items
           -> m ((ItemId, Item), (Int, Container))
getAnyItem leader prompt = getItem leader prompt (const True) allObjectsName

data ItemDialogState = INone | ISuitable | IAll deriving Eq

-- | Let the human player choose a single, preferably suitable,
-- item from a list of items.
getItem :: (MonadClientAbort m, MonadClientUI m)
        => ActorId
        -> Text            -- ^ prompt message
        -> (Item -> Bool)  -- ^ which items to consider suitable
        -> Text            -- ^ how to describe suitable items
        -> ItemBag         -- ^ all items in question
        -> ItemInv         -- ^ inventory characters
        -> Text            -- ^ how to refer to the collection of items
        -> m ((ItemId, Item), (Int, Container))
getItem aid prompt p ptext bag inv isn = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  lvl <- getsLevel (blid b) id
  s <- getState
  body <- getsState $ getActorBody aid
  let checkItem (l, iid) =
        fmap (\k -> ((iid, getItemBody iid s), (k, l))) $ EM.lookup iid bag
      is0 = mapMaybe checkItem $ EM.assocs inv
      pos = bpos body
      tis = lvl `atI` pos
      floorFull = not $ EM.null tis
      (floorMsg, floorKey) | floorFull = (", -", [K.Char '-'])
                           | otherwise = ("", [])
      isp = filter (p . snd . fst) is0
      bestFull = not $ null isp
      (bestMsg, bestKey)
        | bestFull =
          let bestLetter = invChar $ maximum $ map (snd . snd) isp
          in (", RET(" <> T.singleton bestLetter <> ")", [K.Return])
        | otherwise = ("", [])
      keys ims =
        let mls = map (snd . snd) ims
            ks = bestKey ++ floorKey ++ [K.Char '?']
                 ++ map (K.Char . invChar) mls
        in zipWith K.KM (repeat K.NoModifier) ks
      choice ims =
        if null ims
        then "[?" <> floorMsg
        else let mls = map (snd . snd) ims
                 r = letterRange mls
             in "[" <> r <> ", ?" <> floorMsg <> bestMsg
      ask = do
        when (null is0 && EM.null tis) $
          abortWith "Not carrying anything."
        perform INone
      invP = EM.filter (\iid -> p (getItemBody iid s)) inv
      perform itemDialogState = do
        let (ims, invOver, msg) = case itemDialogState of
              INone     -> (isp, EM.empty, prompt)
              ISuitable -> (isp, invP, ptext <+> isn <> ".")
              IAll      -> (is0, inv, allObjectsName <+> isn <> ".")
        io <- itemOverlay bag invOver
        km@K.KM {..} <-
          displayChoiceUI (msg <+> choice ims) io (keys ims)
        assert (modifier == K.NoModifier) skip
        case key of
          K.Char '?' -> case itemDialogState of
            INone -> perform ISuitable
            ISuitable | ptext /= allObjectsName -> perform IAll
            _ -> perform INone
          K.Char '-' | floorFull ->
            -- TODO: let player select item
            return $ maximumBy (compare `on` fst . fst)
                   $ map (\(iid, k) ->
                           ((iid, getItemBody iid s),
                            (k, CFloor (blid b) pos)))
                   $ EM.assocs tis
          K.Char l | InvChar l `elem` map (snd . snd) ims ->
            case find ((InvChar l ==) . snd . snd) ims of
              Nothing -> assert `failure` (l,  ims)
              Just (iidItem, (k, l2)) ->
                return (iidItem, (k, CActor aid l2))
          K.Return | bestFull ->
            let (iidItem, (k, l2)) = maximumBy (compare `on` snd . snd) isp
            in return (iidItem, (k, CActor aid l2))
          _ -> assert `failure` "perform: unexpected key:" <+> K.showKM km
  ask

-- * Project

projectAid :: (MonadClientAbort m, MonadClientUI m)
           => ActorId -> [Trigger] -> m CmdSerTakeTime
projectAid source ts = do
  Kind.COps{cotile} <- getsState scops
  target <- targetToPos
  let tpos = case target of
        Just p -> p
        Nothing -> assert `failure` (source, "target unexpectedly invalid")
  eps <- getsClient seps
  sb <- getsState $ getActorBody source
  let lid = blid sb
      spos = bpos sb
  fact <- getsState $ (EM.! bfid sb) . sfactionD
  bs <- getsState $ actorNotProjList (isAtWar fact) lid
  lxsize <- getsLevel lid lxsize
  lysize <- getsLevel lid lysize
  if foesAdjacent lxsize lysize spos bs
    then abortFailure ProjectBlockFoes
    else do
      case bla lxsize lysize eps spos tpos of
        Nothing -> abortFailure ProjectAimOnself
        Just [] -> assert `failure`
                     (spos, tpos, "project from the edge of level" :: Text)
        Just (pos : _) -> do
          inhabitants <- getsState (posToActor pos lid)
          lvl <- getsLevel lid id
          let t = lvl `at` pos
          if not $ Tile.hasFeature cotile F.Clear t
            then abortFailure ProjectBlockTerrain
            else if isJust inhabitants
                 then abortFailure ProjectBlockActor
                 else projectBla source tpos eps ts

projectBla :: (MonadClientAbort m, MonadClientUI m)
           => ActorId -> Point -> Int -> [Trigger] -> m CmdSerTakeTime
projectBla source tpos eps ts = do
  let (verb1, object1) = case ts of
        [] -> ("aim", "object")
        tr : _ -> (verb tr, object tr)
      triggerSyms = triggerSymbols ts
  bag <- getsState $ getActorBag source
  inv <- getsState $ getActorInv source
  ((iid, _), (_, container)) <-
    getGroupItem source bag inv object1 triggerSyms
      (makePhrase ["What to", verb1 MU.:> "?"]) "in inventory"
  stgtMode <- getsClient stgtMode
  case stgtMode of
    Just (TgtAuto _) -> endTargeting True
    _ -> return ()
  return $! ProjectSer source tpos eps iid container

triggerSymbols :: [Trigger] -> [Char]
triggerSymbols [] = []
triggerSymbols (ApplyItem{..} : ts) = symbol : triggerSymbols ts
triggerSymbols (_ : ts) = triggerSymbols ts

-- * Apply

applyHuman :: (MonadClientAbort m, MonadClientUI m)
           => [Trigger] -> m CmdSerTakeTime
applyHuman ts = do
  leader <- getLeaderUI
  bag <- getsState $ getActorBag leader
  inv <- getsState $ getActorInv leader
  let (verb1, object1) = case ts of
        [] -> ("activate", "object")
        tr : _ -> (verb tr, object tr)
      triggerSyms = triggerSymbols ts
  ((iid, _), (_, container)) <-
    getGroupItem leader bag inv object1 triggerSyms
                 (makePhrase ["What to", verb1 MU.:> "?"]) "in inventory"
  return $! ApplySer leader iid container

-- | Let a human player choose any item with a given group name.
-- Note that this does not guarantee the chosen item belongs to the group,
-- as the player can override the choice.
getGroupItem :: (MonadClientAbort m, MonadClientUI m)
             => ActorId
             -> ItemBag  -- ^ all objects in question
             -> ItemInv  -- ^ inventory characters
             -> MU.Part  -- ^ name of the group
             -> [Char]   -- ^ accepted item symbols
             -> Text     -- ^ prompt
             -> Text     -- ^ how to refer to the collection of objects
             -> m ((ItemId, Item), (Int, Container))
getGroupItem leader is inv object syms prompt packName = do
  let choice i = jsymbol i `elem` syms
      header = makePhrase [MU.Capitalize (MU.Ws object)]
  getItem leader prompt choice header is inv packName

-- * AlterDir

-- | Ask for a direction and alter a tile, if possible.
alterDirHuman :: (MonadClientAbort m, MonadClientUI m)
                => [Trigger] -> m CmdSerTakeTime
alterDirHuman ts = do
  let verb1 = case ts of
        [] -> "alter"
        tr : _ -> verb tr
      keys = zipWith K.KM (repeat K.NoModifier) K.dirAllMoveKey
      prompt = makePhrase ["What to", verb1 MU.:> "? [movement key"]
  e <- displayChoiceUI prompt [] keys
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  lxsize <- getsLevel (blid b) lxsize
  K.handleDir lxsize e (flip (alterTile leader) ts) (neverMind True)

-- | Player tries to alter a tile using a feature.
alterTile :: (MonadClientAbort m, MonadClientUI m)
         => ActorId -> Vector -> [Trigger] -> m CmdSerTakeTime
alterTile leader dir ts = do
  Kind.COps{cotile} <- getsState scops
  b <- getsState $ getActorBody leader
  lvl <- getsLevel (blid b) id
  let dpos = bpos b `shift` dir
      t = lvl `at` dpos
      alterFeats = alterFeatures ts
  case filter (\feat -> Tile.hasFeature cotile feat t) alterFeats of
    [] -> guessAlter cotile alterFeats t
    _fs -> return $! AlterSer leader dir

alterFeatures :: [Trigger] -> [F.Feature]
alterFeatures [] = []
alterFeatures (AlterFeature{..} : ts) = feature : alterFeatures ts
alterFeatures (_ : ts) = alterFeatures ts

-- | Guess and report why the bump command failed.
guessAlter :: MonadClientAbort m => Kind.Ops TileKind -> [F.Feature] -> Kind.Id TileKind -> m a
guessAlter cotile (F.Openable : _) t | Tile.hasFeature cotile F.Closable t =
  abortWith "already open"
guessAlter _ (F.Openable : _) _ =
  abortWith "not a door"
guessAlter cotile (F.Closable : _) t | Tile.hasFeature cotile F.Openable t =
  abortWith "already closed"
guessAlter _ (F.Closable : _) _ =
  abortWith "not a door"
guessAlter _ _ _ = neverMind True

-- * TriggerTile

-- | Leader tries to trigger the tile he's standing on.
triggerTileHuman :: (MonadClientAbort m, MonadClientUI m)
                 => [Trigger] -> m CmdSerTakeTime
triggerTileHuman ts = do
  leader <- getLeaderUI
  dpos <- getsState (bpos . getActorBody leader)
  triggerTile leader dpos ts

-- | Player tries to trigger a tile using a feature.
triggerTile :: (MonadClientAbort m, MonadClientUI m)
            => ActorId -> Point -> [Trigger] -> m CmdSerTakeTime
triggerTile leader dpos ts = do
  Kind.COps{cotile} <- getsState scops
  b <- getsState $ getActorBody leader
  lvl <- getsLevel (blid b) id
  let t = lvl `at` dpos
      triggerFeats = triggerFeatures ts
  case filter (\feat -> Tile.hasFeature cotile feat t) triggerFeats of
    [] -> guessTrigger cotile triggerFeats t
    fs -> do
      mapM_ (verifyTrigger leader) fs
      return $ TriggerSer leader

triggerFeatures :: [Trigger] -> [F.Feature]
triggerFeatures [] = []
triggerFeatures (TriggerFeature{..} : ts) = feature : triggerFeatures ts
triggerFeatures (_ : ts) = triggerFeatures ts

-- | Verify important feature triggers, such as fleeing the dungeon.
verifyTrigger :: (MonadClientAbort m, MonadClientUI m)
              => ActorId -> F.Feature -> m ()
verifyTrigger leader feat = case feat of
  F.Cause Effect.Escape -> do
    b <- getsState $ getActorBody leader
    side <- getsClient sside
    spawn <- getsState $ isSpawnFaction side
    summon <- getsState $ isSummonFaction side
    when (spawn || summon) $ abortWith
      "This is the way out, but where would you go in this alien world?"
    go <- displayYesNo ColorFull "This is the way out. Really leave now?"
    unless go $ abortWith "Game resumed."
    (_, total) <- getsState $ calculateTotal b
    when (total == 0) $ do
      -- The player can back off at each of these steps.
      go1 <- displayMore ColorBW
               "Afraid of the challenge? Leaving so soon and empty-handed?"
      unless go1 $ abortWith "Brave soul!"
      go2 <- displayMore ColorBW
               "Next time try to grab some loot before escape!"
      unless go2 $ abortWith "Here's your chance!"
  _ -> return ()

-- | Guess and report why the bump command failed.
guessTrigger :: MonadClientAbort m
             => Kind.Ops TileKind -> [F.Feature] -> Kind.Id TileKind -> m a
guessTrigger cotile (F.Cause (Effect.Ascend _) : _) t
  | Tile.hasFeature cotile (F.Cause (Effect.Descend 1)) t =
    abortWith "the way goes down, not up"
guessTrigger _ (F.Cause (Effect.Ascend _) : _) _ =
  abortWith "no stairs up"
guessTrigger cotile (F.Cause (Effect.Descend _) : _) t
  | Tile.hasFeature cotile (F.Cause (Effect.Ascend 1)) t =
    abortWith "the way goes up, not down"
guessTrigger _ (F.Cause (Effect.Descend _) : _) _ =
  abortWith "no stairs down"
guessTrigger _ _ _ = neverMind True

-- * GameRestart; does not take time

gameRestartHuman :: (MonadClientAbort m, MonadClientUI m) => Text -> m CmdSer
gameRestartHuman t = do
  let msg = "You just requested a new" <+> t <+> "game."
  b1 <- displayMore ColorFull msg
  unless b1 $ neverMind True
  b2 <- displayYesNo ColorBW
          "Current progress will be lost! Really restart the game?"
  msg2 <- rndToAction $ oneOf
            [ "Yea, would be a pity to leave them all to die."
            , "Yea, a shame to get your own team stranded." ]
  unless b2 $ abortWith msg2
  leader <- getLeaderUI
  return $ GameRestartSer leader t

-- * GameExit; does not take time

gameExitHuman :: (MonadClientAbort m, MonadClientUI m) => m CmdSer
gameExitHuman = do
  b <- displayYesNo ColorFull "Really save and exit?"
  if b then do
    leader <- getLeaderUI
    return $ GameExitSer leader
  else abortWith "Save and exit canceled."

-- * GameSave; does not take time

gameSaveHuman :: MonadClientUI m => m CmdSer
gameSaveHuman = do
  leader <- getLeaderUI
  -- TODO: do not save to history:
  msgAdd "Saving game backup."
  return $ GameSaveSer leader

-- * CfgDump; does not take time

cfgDumpHuman :: MonadClientUI m => m CmdSer
cfgDumpHuman = do
  leader <- getLeaderUI
  return $ CfgDumpSer leader
