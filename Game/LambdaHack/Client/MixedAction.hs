{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Semantics of 'Command.Cmd' client commands that return server commands.
-- A couple of them do not take time, the rest does.
-- TODO: document
module Game.LambdaHack.Client.MixedAction where

import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Function
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
  Just leader <- getsClient sleader
  bag <- getsState $ getActorBag leader
  inv <- getsState $ getActorInv leader
  ((iid, item), (_, container)) <-
    getGroupItem leader bag inv object syms
                 (makePhrase ["What to", verb MU.:> "?"]) "in inventory"
  disco <- getsState sdisco
  let verbApply = case jkind disco item of
        Nothing -> verb
        Just ik -> iverbApply $ okind ik
  return $! ApplySer leader verbApply iid container

-- | Let a human player choose any item with a given group name.
-- Note that this does not guarantee the chosen item belongs to the group,
-- as the player can override the choice.
getGroupItem :: MonadClientUI m
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

-- ** Project

leaderProjectGroupItem :: MonadClientUI m
                       => MU.Part -> MU.Part -> [Char]
                       -> m CmdSer
leaderProjectGroupItem verb object syms = do
  side <- getsState sside
  genemy <- getsState $ genemy . (EM.! side) . sfaction
  ms <- getsState $ actorNotProjList (`elem` genemy) . getArena
  lxsize <- getsState (lxsize . getArena)
  lysize <- getsState (lysize . getArena)
  Just leader <- getsClient sleader
  ppos <- getsState (bpos . getActorBody leader)
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
      bag <- getsState $ getActorBag aid
      inv <- getsState $ getActorInv aid
      ((iid, item), (_, container)) <-
        getGroupItem aid bag inv object syms
          (makePhrase ["What to", verb MU.:> "?"]) "in inventory"
      stgtMode <- getsClient stgtMode
      case stgtMode of
        Just (TgtAuto _) -> endTargeting True
        _ -> return ()
      disco <- getsState sdisco
      let verbProject = case jkind disco item of
            Nothing -> verb
            Just ik -> iverbProject $ okind ik
      return $! ProjectSer aid p seps verbProject iid container
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
leaderBumpDir :: MonadClient m => F.Feature -> Vector -> m CmdSer
leaderBumpDir feat dir = do
  Just leader <- getsClient sleader
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
leaderTriggerTile :: MonadClient m => F.Feature -> m CmdSer
leaderTriggerTile feat = do
  Just leader <- getsClient sleader
  ppos <- getsState (bpos . getActorBody leader)
  bumpTile leader ppos feat

-- ** Pickup

pickupItem :: MonadClient m => m CmdSer
pickupItem = do
  Just leader <- getsClient sleader
  actorPickupItem leader

actorPickupItem :: MonadActionRO m => ActorId -> m CmdSer
actorPickupItem actor = do
  lvl <- getsState getArena
  body <- getsState $ getActorBody actor
  -- Check if something is here to pick up. Items are never invisible.
  case EM.minViewWithKey $ lvl `atI` bpos body of
    Nothing -> abortWith "nothing here"
    Just ((iid, k), _) ->  do -- pick up first item; TODO: let pl select item; not for monsters
      item <- getsState $ getItemBody iid
      let l = if jsymbol item == '$' then Just $ InvChar '$' else Nothing
      case assignLetter iid l body of
        Just l2 -> return $ PickupSer actor iid k l2
        Nothing -> abortWith "cannot carry any more"

-- ** Drop

-- TODO: you can drop an item already on the floor, which works correctly,
-- but is weird and useless.
-- | Drop a single item.
dropItem :: MonadClientUI m => m CmdSer
dropItem = do
  -- TODO: allow dropping a given number of identical items.
  Kind.COps{coactor, coitem} <- getsState scops
  Just leader <- getsClient sleader
  pbody <- getsState $ getActorBody leader
  bag <- getsState $ getActorBag leader
  inv <- getsState $ getActorInv leader
  ((iid, item), _k) <- getAnyItem leader "What to drop?" bag inv "in inventory"
  disco <- getsState sdisco
  -- Do not advertise if an enemy drops an item. Probably junk.
  msgAdd $ makeSentence  -- TODO: "you" instead of partActor?
    [ MU.SubjectVerbSg (partActor coactor pbody) "drop"
    , partItemNWs coitem disco 1 item ]
  return $ DropSer leader iid

allObjectsName :: Text
allObjectsName = "Objects"

-- | Let the human player choose any item from a list of items.
getAnyItem :: MonadClientUI m
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
getItem :: MonadClientUI m
        => ActorId
        -> Text            -- ^ prompt message
        -> (Item -> Bool)  -- ^ which items to consider suitable
        -> Text            -- ^ how to describe suitable items
        -> ItemBag         -- ^ all items in question
        -> ItemInv         -- ^ inventory characters
        -> Text            -- ^ how to refer to the collection of items
        -> m ((ItemId, Item), (Int, Container))
getItem aid prompt p ptext bag inv isn = do
  lvl <- getsState getArena
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
        in zip ks $ repeat K.NoModifier
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
              return $ maximumBy (compare `on` fst . fst)
                     $ map (\(iid, k) ->
                             ((iid, getItemBody iid s), (k, CFloor pos)))
                     $ EM.assocs tis
            K.Char l | InvChar l `elem` map (snd . snd) ims ->
              case find ((InvChar l ==) . snd . snd) ims of
                Nothing -> assert `failure` (l,  ims)
                Just (iidItem, (k, _)) -> return (iidItem, (k, CActor aid))
            K.Return | bestFull ->
              let (iidItem, (k, _)) = maximumBy (compare `on` snd . snd) isp
              in return (iidItem, (k, CActor aid))
            k -> assert `failure` "perform: unexpected key:" <+> showT k
  ask

-- ** Wait

-- | Leader waits a turn (and blocks, etc.).
waitBlock :: MonadClient m => m CmdSer
waitBlock = do
  Just leader <- getsClient sleader
  return $ WaitSer leader

-- ** Move

movePl :: MonadClient m => Vector -> m CmdSer
movePl dir = do
  Just leader <- getsClient sleader
  return $! MoveSer leader dir

-- ** Run

runPl :: MonadClient m => Vector -> m CmdSer
runPl dir = do
  Just leader <- getsClient sleader
  (dirR, distNew) <- runDir leader (dir, 0)
  modifyClient $ \cli -> cli {srunning = Just (dirR, distNew)}
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
  side <- getsState sside
  return $ GameRestartSer side
