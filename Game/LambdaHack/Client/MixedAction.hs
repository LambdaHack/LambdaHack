{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Semantics of 'Command.Cmd' client commands that return server commands.
-- A couple of them do not take time, the rest does.
-- TODO: document
module Game.LambdaHack.Client.MixedAction where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.Draw
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.LocalAction
import Game.LambdaHack.Client.RunAction
import Game.LambdaHack.Client.State
import Game.LambdaHack.CmdSer
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.TileKind as TileKind
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Point
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

default (Text)

-- + Semantics of client commands that return a server command

-- ** Two commands that do not take time, but still return a server command

-- *** GameSave

gameSave :: MonadClient m => m CmdSer
gameSave = do
  msgAdd "Saving game to a backup file."
  -- Let the server save, while the client continues taking commands.
  return GameSaveSer

-- *** CfgDump

dumpConfig :: MonadActionAbort m => m CmdSer
dumpConfig = return CfgDumpSer

-- ** Apply

leaderApplyGroupItem :: MonadClientUI m
                     => MU.Part -> MU.Part -> [Char]
                     -> m CmdSer
leaderApplyGroupItem verb object syms = do
  Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
  Just leader <- getsClient getLeader
  is <- getsState $ getActorItem leader
  item <- getGroupItem leader is object syms
            (makePhrase ["What to", verb MU.:> "?"]) "in inventory"
  disco <- getsState sdisco
  let verbApply = case jkind disco item of
        Nothing -> verb
        Just ik -> iverbApply $ okind ik
  return $! ApplySer leader verbApply item

-- | Let a human player choose any item with a given group name.
-- Note that this does not guarantee the chosen item belongs to the group,
-- as the player can override the choice.
getGroupItem :: MonadClientUI m
             => ActorId
             -> [Item]   -- ^ all objects in question
             -> MU.Part  -- ^ name of the group
             -> [Char]   -- ^ accepted item symbols
             -> Text     -- ^ prompt
             -> Text     -- ^ how to refer to the collection of objects
             -> m Item
getGroupItem leader is object syms prompt packName = do
  let choice i = jsymbol i `elem` syms
      header = makePhrase [MU.Capitalize (MU.Ws object)]
  getItem leader prompt choice header is packName

-- ** Project

leaderProjectGroupItem :: MonadClientUI m
                       => MU.Part -> MU.Part -> [Char]
                       -> m CmdSer
leaderProjectGroupItem verb object syms = do
  genemy <- getsState $ genemy . getSide
  ms <- getsState $ actorNotProjList (`elem` genemy) . getArena
  lxsize <- getsState (lxsize . getArena)
  lysize <- getsState (lysize . getArena)
  Just leader <- getsClient getLeader
  ppos   <- getsState (bpos . getActorBody leader)
  if foesAdjacent lxsize lysize ppos ms
    then abortWith "You can't aim in melee."
    else actorProjectGI leader verb object syms

actorProjectGI :: MonadClientUI m
               => ActorId -> MU.Part -> MU.Part -> [Char]
               -> m CmdSer
actorProjectGI aid verb object syms = do
  cli <- getClient
  pos <- getState
  seps <- getsClient seps
  case targetToPos cli pos of
    Just p -> do
      Kind.COps{coitem=Kind.Ops{okind}} <- getsState scops
      is <- getsState $ getActorItem aid
      item <- getGroupItem aid is object syms
                (makePhrase ["What to", verb MU.:> "?"]) "in inventory"
      stgtMode <- getsClient stgtMode
      case stgtMode of
        Just (TgtAuto _) -> endTargeting True
        _ -> return ()
      disco <- getsState sdisco
      let verbProject = case jkind disco item of
            Nothing -> verb
            Just ik -> iverbProject $ okind ik
      return $! ProjectSer aid p seps verbProject item
    Nothing -> assert `failure` (pos, aid, "target unexpectedly invalid")

-- ** TriggerDir

-- | Ask for a direction and trigger a tile, if possible.
leaderTriggerDir :: MonadClientUI m => F.Feature -> MU.Part -> m CmdSer
leaderTriggerDir feat verb = do
  let keys = zip K.dirAllMoveKey $ repeat K.NoModifier
      prompt = makePhrase ["What to", verb MU.:> "? [movement key"]
  e <- displayChoiceUI prompt [] keys
  lxsize <- getsState (lxsize . getArena)
  K.handleDir lxsize e (leaderBumpDir feat) (neverMind True)

-- | Leader tries to trigger a tile in a given direction.
leaderBumpDir :: MonadClientRO m => F.Feature -> Vector -> m CmdSer
leaderBumpDir feat dir = do
  Just leader <- getsClient getLeader
  body <- getsState $ getActorBody leader
  let dpos = bpos body `shift` dir
  bumpTile leader dpos feat

-- | Leader tries to trigger a tile using a feature.
bumpTile :: MonadActionRO m => ActorId -> Point -> F.Feature -> m CmdSer
bumpTile leader dpos feat = do
  Kind.COps{cotile} <- getsState scops
  lvl <- getsState getArena
  let t = lvl `at` dpos
  -- Features are never invisible; visible tiles are identified accurately.
  -- A tile can be triggered even if an invisible monster occupies it.
  -- TODO: let the user choose whether to attack or activate.
  if Tile.hasFeature cotile feat t
    then return $ TriggerSer leader dpos
    else guessBump cotile feat t

-- | Guess and report why the bump command failed.
guessBump :: MonadActionAbort m => Kind.Ops TileKind -> F.Feature -> Kind.Id TileKind -> m a
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

-- ** TriggerTile

-- | Leader tries to trigger the tile he's standing on.
leaderTriggerTile :: MonadClientRO m => F.Feature -> m CmdSer
leaderTriggerTile feat = do
  Just leader <- getsClient getLeader
  ppos <- getsState (bpos . getActorBody leader)
  bumpTile leader ppos feat

-- ** Pickup

pickupItem :: MonadClientRO m => m CmdSer
pickupItem = do
  Just leader <- getsClient getLeader
  actorPickupItem leader

actorPickupItem :: MonadActionRO m => ActorId -> m CmdSer
actorPickupItem actor = do
  lvl <- getsState getArena
  body <- getsState (getActorBody actor)
  bitems <- getsState (getActorItem actor)
  -- Check if something is here to pick up. Items are never invisible.
  case lvl `atI` bpos body of
    [] -> abortWith "nothing here"
    i : _ ->  -- pick up first item; TODO: let pl select item; not for monsters
      case assignLetter (jletter i) (bletter body) bitems of
        Just l -> return $ PickupSer actor i l
        Nothing -> abortWith "cannot carry any more"

-- ** Drop

-- TODO: you can drop an item already on the floor, which works correctly,
-- but is weird and useless.
-- | Drop a single item.
dropItem :: MonadClientUI m => m CmdSer
dropItem = do
  -- TODO: allow dropping a given number of identical items.
  Kind.COps{coactor, coitem} <- getsState scops
  Just leader <- getsClient getLeader
  pbody <- getsState $ getActorBody leader
  ims   <- getsState $ getActorItem leader
  stack <- getAnyItem leader "What to drop?" ims "in inventory"
  disco <- getsState sdisco
  let item = stack { jcount = 1 }
  -- Do not advertise if an enemy drops an item. Probably junk.
  msgAdd $ makeSentence
    [ MU.SubjectVerbSg (partActor coactor pbody) "drop"
    , partItemNWs coitem disco item ]
  return $ DropSer leader item

allObjectsName :: Text
allObjectsName = "Objects"

-- | Let the human player choose any item from a list of items.
getAnyItem :: MonadClientUI m
           => ActorId
           -> Text    -- ^ prompt
           -> [Item]  -- ^ all items in question
           -> Text    -- ^ how to refer to the collection of items
           -> m Item
getAnyItem leader prompt = getItem leader prompt (const True) allObjectsName

data ItemDialogState = INone | ISuitable | IAll deriving Eq

-- | Let the human player choose a single, preferably suitable,
-- item from a list of items.
getItem :: MonadClientUI m
        => ActorId
        -> Text            -- ^ prompt message
        -> (Item -> Bool)  -- ^ which items to consider suitable
        -> Text            -- ^ how to describe suitable items
        -> [Item]          -- ^ all items in question
        -> Text            -- ^ how to refer to the collection of items
        -> m Item
getItem leader prompt p ptext is0 isn = do
  lvl  <- getsState getArena
  body <- getsState $ getActorBody leader
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
        disco <- getsState sdisco
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

-- ** Wait

-- | Leader waits a turn (and blocks, etc.).
waitBlock :: MonadClientRO m => m CmdSer
waitBlock = do
  Just leader <- getsClient getLeader
  return $ WaitSer leader

-- ** Move

movePl :: MonadClientRO m => Vector -> m CmdSer
movePl dir = do
  Just leader <- getsClient getLeader
  return $! MoveSer leader dir

-- ** Run

runPl :: MonadClient m => Vector -> m CmdSer
runPl dir = do
  Just leader <- getsClient getLeader
  dirR <- runDir leader (dir, 0)
  return $! RunSer leader dirR

-- ** GameExit

gameExit :: MonadClientUI m => m CmdSer
gameExit = do
  b <- displayYesNo "Really save and exit?"
  if b
    then return GameExitSer
    else abortWith "Game resumed."

-- ** GameRestart

gameRestart :: MonadClientUI m => m CmdSer
gameRestart = do
  b1 <- displayMore ColorFull "You just requested a new game."
  when (not b1) $ neverMind True
  b2 <- displayYesNo "Current progress will be lost! Really restart the game?"
  when (not b2) $ abortWith "Yea, would be a pity to leave them to die."
  return GameRestartSer
