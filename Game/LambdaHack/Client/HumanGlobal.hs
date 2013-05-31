{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Semantics of 'Command.Cmd' client commands that return server commands.
-- A couple of them do not take time, the rest does.
-- TODO: document
module Game.LambdaHack.Client.HumanGlobal
  ( moveLeader, exploreLeader, runLeader, waitHuman, pickupHuman, dropHuman
  , projectLeader, applyHuman, triggerDirHuman, triggerTileHuman
  , gameRestartHuman, gameExitHuman, gameSaveHuman, cfgDumpHuman
  ) where

import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Monoid as Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.Draw
import Game.LambdaHack.Client.HumanLocal
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.RunAction
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.ServerCmd
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.TileKind as TileKind
import Game.LambdaHack.Utils.Assert

-- * Move

moveLeader :: MonadClientUI m => Vector -> m CmdSer
moveLeader dir = do
  leader <- getLeaderUI
  return $! MoveSer leader dir

-- * Explore

exploreLeader :: MonadClientUI m => Vector -> m CmdSer
exploreLeader dir = do
  leader <- getLeaderUI
  return $! ExploreSer leader dir

-- * Run

runLeader :: MonadClientUI m => Vector -> m CmdSer
runLeader dir = do
  leader <- getLeaderUI
  (dirR, distNew) <- runDir leader (dir, 0)
  modifyClient $ \cli -> cli {srunning = Just (dirR, distNew)}
  return $! RunSer leader dirR

-- * Wait

-- | Leader waits a turn (and blocks, etc.).
waitHuman :: MonadClientUI m => m CmdSer
waitHuman = do
  leader <- getLeaderUI
  return $ WaitSer leader

-- * Pickup

pickupHuman :: (MonadClientAbort m, MonadClientUI m) => m CmdSer
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
dropHuman :: (MonadClientAbort m, MonadClientUI m) => m CmdSer
dropHuman = do
  -- TODO: allow dropping a given number of identical items.
  Kind.COps{coitem} <- getsState scops
  leader <- getLeaderUI
  bag <- getsState $ getActorBag leader
  inv <- getsState $ getActorInv leader
  ((iid, item), _k) <- getAnyItem leader "What to drop?" bag inv "in inventory"
  disco <- getsClient sdisco
  -- Do not advertise if an enemy drops an item. Probably junk.
  subject <- partActorLeader leader
  msgAdd $ makeSentence
    [ MU.SubjectVerbSg subject "drop"
    , partItemNWs coitem disco 1 item ]
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
        assert (modifier == K.NoModifier) $
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

projectLeader :: (MonadClientAbort m, MonadClientUI m)
              => MU.Part -> MU.Part -> [Char] -> m CmdSer
projectLeader verb object syms = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  let lid = blid b
  ms <- getsState $ actorNotProjList (isAtWar fact) lid
  lxsize <- getsLevel lid lxsize
  lysize <- getsLevel lid lysize
  if foesAdjacent lxsize lysize (bpos b) ms
    then abortWith "You can't aim in melee."
    else actorProjectGI leader verb object syms

actorProjectGI :: (MonadClientAbort m, MonadClientUI m)
               => ActorId -> MU.Part -> MU.Part -> [Char]
               -> m CmdSer
actorProjectGI aid verb object syms = do
  seps <- getsClient seps
  target <- targetToPos
  case target of
    Just p -> do
      bag <- getsState $ getActorBag aid
      inv <- getsState $ getActorInv aid
      ((iid, _), (_, container)) <-
        getGroupItem aid bag inv object syms
          (makePhrase ["What to", verb MU.:> "?"]) "in inventory"
      stgtMode <- getsClient stgtMode
      case stgtMode of
        Just (TgtAuto _) -> endTargeting True
        _ -> return ()
      return $! ProjectSer aid p seps iid container
    Nothing -> assert `failure` (aid, "target unexpectedly invalid")

-- * Apply

applyHuman :: (MonadClientAbort m, MonadClientUI m)
           => MU.Part -> MU.Part -> [Char]
           -> m CmdSer
applyHuman verb object syms = do
  leader <- getLeaderUI
  bag <- getsState $ getActorBag leader
  inv <- getsState $ getActorInv leader
  ((iid, _), (_, container)) <-
    getGroupItem leader bag inv object syms
                 (makePhrase ["What to", verb MU.:> "?"]) "in inventory"
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

-- * TriggerDir

-- | Ask for a direction and trigger a tile, if possible.
triggerDirHuman :: (MonadClientAbort m, MonadClientUI m)
                => F.Feature -> MU.Part -> m CmdSer
triggerDirHuman feat verb = do
  let keys = zipWith K.KM (repeat K.NoModifier) K.dirAllMoveKey
      prompt = makePhrase ["What to", verb MU.:> "? [movement key"]
  e <- displayChoiceUI prompt [] keys
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  lxsize <- getsLevel (blid b) lxsize
  K.handleDir lxsize e (leaderBumpDir feat) (neverMind True)

-- | Leader tries to trigger a tile in a given direction.
leaderBumpDir :: (MonadClientAbort m, MonadClientUI m)
              => F.Feature -> Vector -> m CmdSer
leaderBumpDir feat dir = do
  leader <- getLeaderUI
  body <- getsState $ getActorBody leader
  let dpos = bpos body `shift` dir
  bumpTile leader dpos feat

-- | Player tries to trigger a tile using a feature.
-- To help the player, only visible features can be triggered.
bumpTile :: (MonadClientAbort m, MonadClientUI m)
         => ActorId -> Point -> F.Feature -> m CmdSer
bumpTile leader dpos feat = do
  Kind.COps{cotile} <- getsState scops
  b <- getsState $ getActorBody leader
  lvl <- getsLevel (blid b) id
  let t = lvl `at` dpos
  -- A tile can be triggered even if an invisible monster occupies it.
  -- TODO: let the user choose whether to attack or activate.
  if Tile.hasFeature cotile feat t then do
    verifyTrigger leader feat
    return $ TriggerSer leader dpos
  else guessBump cotile feat t

-- | Verify important feature triggers, such as fleeing the dungeon.
verifyTrigger :: (MonadClientAbort m, MonadClientUI m)
              => ActorId -> F.Feature -> m ()
verifyTrigger leader feat = case feat of
  F.Ascendable -> do
    s <- getState
    b <- getsState $ getActorBody leader
    if isNothing $ whereTo s (blid b) 1 then do -- TODO: consider power > 1
      Kind.COps{coitem=Kind.Ops{oname, ouniqGroup}} <- getsState scops
      go <- displayYesNo "This is the way out. Really leave now?"
      when (not go) $ abortWith "Game resumed."
      side <- getsClient sside
      let (bag, total) = calculateTotal side (blid b) s
      if total == 0 then do
        -- The player can back off at each of these steps.
        go1 <- displayMore ColorBW
                 "Afraid of the challenge? Leaving so soon and empty-handed?"
        when (not go1) $ abortWith "Brave soul!"
        go2 <- displayMore ColorBW
                 "This time try to grab some loot before escape!"
        when (not go2) $ abortWith "Here's your chance!"
      else do
        let currencyName = MU.Text $ oname $ ouniqGroup "currency"
            winMsg = makeSentence
              [ "Congratulations, you won!"
              , "Here's your loot, worth"
              , MU.NWs total currencyName ]
        io <- floorItemOverlay bag
        slides <- overlayToSlideshow winMsg io
        partingSlide <- promptToSlideshow "Can it be done better, though?"
        void $ getInitConfirms [] $ slides Monoid.<> partingSlide
        flushFrames
    else return ()
  _ -> return ()

-- | Guess and report why the bump command failed.
guessBump :: MonadClientAbort m => Kind.Ops TileKind -> F.Feature -> Kind.Id TileKind -> m a
guessBump cotile F.Openable t | Tile.hasFeature cotile F.Closable t =
  abortWith "already open"
guessBump _ F.Openable _ =
  abortWith "not a door"
guessBump cotile F.Closable t | Tile.hasFeature cotile F.Openable t =
  abortWith "already closed"
guessBump _ F.Closable _ =
  abortWith "not a door"
guessBump cotile F.Ascendable t | Tile.hasFeature cotile F.Descendable t =
  abortWith "the way goes down, not up"
guessBump _ F.Ascendable _ =
  abortWith "no stairs up"
guessBump cotile F.Descendable t | Tile.hasFeature cotile F.Ascendable t =
  abortWith "the way goes up, not down"
guessBump _ F.Descendable _ =
  abortWith "no stairs down"
guessBump _ _ _ = neverMind True

-- * TriggerTile

-- | Leader tries to trigger the tile he's standing on.
triggerTileHuman :: (MonadClientAbort m, MonadClientUI m)
                 => F.Feature -> m CmdSer
triggerTileHuman feat = do
  leader <- getLeaderUI
  ppos <- getsState (bpos . getActorBody leader)
  bumpTile leader ppos feat

-- * GameRestart; does not take time

gameRestartHuman :: (MonadClientAbort m, MonadClientUI m) => m CmdSer
gameRestartHuman = do
  b1 <- displayMore ColorFull "You just requested a new game."
  when (not b1) $ neverMind True
  b2 <- displayYesNo "Current progress will be lost! Really restart the game?"
  when (not b2) $ abortWith "Yea, would be a pity to leave them to die."
  msgAdd "Restarting the game now."
  leader <- getLeaderUI
  return $ GameRestartSer leader

-- * GameExit; does not take time

gameExitHuman :: (MonadClientAbort m, MonadClientUI m) => m CmdSer
gameExitHuman = do
  b <- displayYesNo "Really save and exit?"
  if b then do
    slides <- scoreToSlideshow Camping
    partingSlide <- promptToSlideshow "See you soon, stronger and braver!"
    void $ getInitConfirms [] $ slides Monoid.<> partingSlide
    nH <- nHumans
    when (nH > 1) $ do
      flushFrames
      displayPush  -- save one slide to match other factions
      fadeD False True
    leader <- getLeaderUI
    return $ GameExitSer leader
  else abortWith "Save and exit canceled."

-- * GameSave; does not take time

gameSaveHuman :: MonadClientUI m => m CmdSer
gameSaveHuman = do
  leader <- getLeaderUI
  -- TODO: do not save to history:
  msgAdd "Game backup will be saved at the end of the turn."
  -- Let the server save, while the client continues taking commands.
  return $ GameSaveSer leader

-- * CfgDump; does not take time

cfgDumpHuman :: MonadClientUI m => m CmdSer
cfgDumpHuman = do
  leader <- getLeaderUI
  return $ CfgDumpSer leader
