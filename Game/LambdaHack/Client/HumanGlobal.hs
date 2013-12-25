-- | Semantics of 'Command.Cmd' client commands that return server commands.
-- A couple of them do not take time, the rest does.
-- TODO: document
module Game.LambdaHack.Client.HumanGlobal
  ( displaceAid, meleeAid, waitHuman, pickupHuman, dropHuman
  , projectAid, applyHuman, alterDirHuman, triggerTileHuman
  , gameRestartHuman, gameExitHuman, gameSaveHuman
  ) where

import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Function
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Control.Exception.Assert.Sugar
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

abortFailure :: MonadClientAbort m => FailureSer -> m a
abortFailure = abortWith . showFailureSer

-- * Move and Run

-- | Actor atttacks an enemy actor or his own projectile.
meleeAid :: (MonadClientAbort m, MonadClientUI m)
          => ActorId -> ActorId -> m CmdSerTakeTime
meleeAid source target = do
  sb <- getsState $ getActorBody source
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
  return $ MeleeSer source target
  -- Seeing the actor prevents altering a tile under it, but that
  -- does not limit the player, he just doesn't waste a turn
  -- on a failed altering.

-- | Actor swaps position with another.
displaceAid :: MonadClientAbort m
            => ActorId -> ActorId -> m CmdSerTakeTime
displaceAid source target = do
  cops <- getsState scops
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  let lid = blid sb
  lvl <- getLevel lid
  let spos = bpos sb
      tpos = bpos tb
  if accessible cops lvl spos tpos then
    -- Displacing requires full access.
    return $ DisplaceSer source target
  else abortFailure DisplaceAccess

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
  lvl <- getLevel $ blid body
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
  lvl <- getLevel $ blid b
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
          K.Char l ->
            case find ((InvChar l ==) . snd . snd) ims of
              Nothing -> assert `failure` "unexpected inventory letter"
                                `twith` (km, l,  ims)
              Just (iidItem, (k, l2)) ->
                return (iidItem, (k, CActor aid l2))
          K.Return | bestFull ->
            let (iidItem, (k, l2)) = maximumBy (compare `on` snd . snd) isp
            in return (iidItem, (k, CActor aid l2))
          _ -> assert `failure` "unexpected key:" `twith` km
  ask

-- * Project

projectAid :: (MonadClientAbort m, MonadClientUI m)
           => ActorId -> [Trigger] -> m CmdSerTakeTime
projectAid source ts = do
  Kind.COps{cotile} <- getsState scops
  target <- targetToPos
  let tpos = case target of
        Just p -> p
        Nothing -> assert `failure` "target unexpectedly invalid" `twith` source
  eps <- getsClient seps
  sb <- getsState $ getActorBody source
  let lid = blid sb
      spos = bpos sb
  fact <- getsState $ (EM.! bfid sb) . sfactionD
  Level{lxsize, lysize} <- getLevel lid
  foes <- getsState $ actorNotProjList (isAtWar fact) lid
  if foesAdjacent lxsize lysize spos foes
    then abortFailure ProjectBlockFoes
    else do
      case bla lxsize lysize eps spos tpos of
        Nothing -> abortFailure ProjectAimOnself
        Just [] -> assert `failure` "project from the edge of level"
                          `twith` (spos, tpos, sb, ts)
        Just (pos : _) -> do
          as <- getsState $ actorList (const True) lid
          lvl <- getLevel lid
          let t = lvl `at` pos
          if not $ Tile.hasFeature cotile F.Clear t
            then abortFailure ProjectBlockTerrain
            else if unoccupied as pos
                 then projectBla source tpos eps ts
                 else abortFailure ProjectBlockActor

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
triggerSymbols (ApplyItem{symbol} : ts) = symbol : triggerSymbols ts
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
  e <- displayChoiceUI prompt emptyOverlay keys
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  Level{lxsize} <- getLevel $ blid b
  K.handleDir lxsize e (flip (alterTile leader) ts) (neverMind True)

-- | Player tries to alter a tile using a feature.
alterTile :: MonadClientAbort m
          => ActorId -> Vector -> [Trigger] -> m CmdSerTakeTime
alterTile source dir ts = do
  Kind.COps{cotile} <- getsState scops
  b <- getsState $ getActorBody source
  lvl <- getLevel $ blid b
  let tpos = bpos b `shift` dir
      t = lvl `at` tpos
      alterFeats = alterFeatures ts
  case filter (\feat -> Tile.hasFeature cotile feat t) alterFeats of
    [] -> guessAlter cotile alterFeats t
    feat : _ -> return $! AlterSer source tpos $ Just feat

alterFeatures :: [Trigger] -> [F.Feature]
alterFeatures [] = []
alterFeatures (AlterFeature{feature} : ts) = feature : alterFeatures ts
alterFeatures (_ : ts) = alterFeatures ts

-- | Guess and report why the bump command failed.
guessAlter :: MonadClientAbort m
           => Kind.Ops TileKind -> [F.Feature] -> Kind.Id TileKind -> m a
guessAlter cotile (F.OpenTo _ : _) t | Tile.closable cotile t =
  abortWith "already open"
guessAlter _ (F.OpenTo _ : _) _ =
  abortWith "can't be opened"
guessAlter cotile (F.CloseTo _ : _) t | Tile.openable cotile t =
  abortWith "already closed"
guessAlter _ (F.CloseTo _ : _) _ =
  abortWith "can't be closed"
guessAlter _ _ _ = neverMind True

-- * TriggerTile

-- | Leader tries to trigger the tile he's standing on.
triggerTileHuman :: (MonadClientAbort m, MonadClientUI m)
                 => [Trigger] -> m CmdSerTakeTime
triggerTileHuman ts = do
  leader <- getLeaderUI
  triggerTile leader ts

-- | Player tries to trigger a tile using a feature.
triggerTile :: (MonadClientAbort m, MonadClientUI m)
            => ActorId -> [Trigger] -> m CmdSerTakeTime
triggerTile leader ts = do
  Kind.COps{cotile} <- getsState scops
  b <- getsState $ getActorBody leader
  lvl <- getLevel $ blid b
  let t = lvl `at` bpos b
      triggerFeats = triggerFeatures ts
  case filter (\feat -> Tile.hasFeature cotile feat t) triggerFeats of
    [] -> guessTrigger cotile triggerFeats t
    feat : _ -> do
      verifyTrigger leader feat
      return $! TriggerSer leader $ Just feat

triggerFeatures :: [Trigger] -> [F.Feature]
triggerFeatures [] = []
triggerFeatures (TriggerFeature{feature} : ts) = feature : triggerFeatures ts
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
guessTrigger cotile fs@(F.Cause (Effect.Ascend k) : _) t
  | Tile.hasFeature cotile (F.Cause (Effect.Ascend (-k))) t =
    if k > 0 then
      abortWith "the way goes down, not up"
    else if k < 0 then
      abortWith "the way goes up, not down"
    else
      assert `failure` fs
guessTrigger _ fs@(F.Cause (Effect.Ascend k) : _) _ =
    if k > 0 then
      abortWith "can't ascend"
    else if k < 0 then
      abortWith "can't descend"
    else
      assert `failure` fs
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
  go <- displayYesNo ColorFull "Really save and exit?"
  if go then do
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
