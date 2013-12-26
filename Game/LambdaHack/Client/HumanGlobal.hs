-- | Semantics of 'Command.Cmd' client commands that return server commands.
-- A couple of them do not take time, the rest does.
-- TODO: document
module Game.LambdaHack.Client.HumanGlobal
  ( displaceAid, meleeAid, waitHuman, pickupHuman, dropHuman
  , projectAid, applyHuman, alterDirHuman, triggerTileHuman
  , gameRestartHuman, gameExitHuman, gameSaveHuman
  ) where

import Control.Exception.Assert.Sugar
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

failSer :: MonadClientUI m => FailureSer -> m (Abort a)
failSer = failWith . showFailureSer

-- * Move and Run

-- | Actor atttacks an enemy actor or his own projectile.
meleeAid :: MonadClientUI m
         => ActorId -> ActorId -> m (Abort CmdSerTakeTime)
meleeAid source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  sfact <- getsState $ (EM.! bfid sb) . sfactionD
  let returnCmd = return $ Right $ MeleeSer source target
  if not (bproj tb || isAtWar sfact (bfid tb)) then do
    go1 <- displayYesNo ColorBW
            "This attack will start a war. Are you sure?"
    if not go1 then failWith "Attack canceled."
    else if not (bproj tb || not (isAllied sfact (bfid tb))) then do
      go2 <- displayYesNo ColorBW
              "You are bound by an alliance. Really attack?"
      if not go2 then failWith "Attack canceled."
      else returnCmd
    else returnCmd
  else returnCmd
  -- Seeing the actor prevents altering a tile under it, but that
  -- does not limit the player, he just doesn't waste a turn
  -- on a failed altering.

-- | Actor swaps position with another.
displaceAid :: MonadClientUI m
            => ActorId -> ActorId -> m (Abort CmdSerTakeTime)
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
    return $ Right $ DisplaceSer source target
  else failSer DisplaceAccess

-- * Wait

-- | Leader waits a turn (and blocks, etc.).
waitHuman :: MonadClientUI m => m CmdSerTakeTime
waitHuman = do
  leader <- getLeaderUI
  return $ WaitSer leader

-- * Pickup

pickupHuman :: MonadClientUI m => m (Abort CmdSerTakeTime)
pickupHuman = do
  leader <- getLeaderUI
  body <- getsState $ getActorBody leader
  lvl <- getLevel $ blid body
  -- Check if something is here to pick up. Items are never invisible.
  case EM.minViewWithKey $ lvl `atI` bpos body of
    Nothing -> failWith "nothing here"
    Just ((iid, k), _) ->  do  -- pick up first item; TODO: let pl select item
      item <- getsState $ getItemBody iid
      let l = if jsymbol item == '$' then Just $ InvChar '$' else Nothing
      case assignLetter iid l body of
        Just l2 -> return $ Right $ PickupSer leader iid k l2
        Nothing -> failWith "cannot carry any more"

-- * Drop

-- TODO: you can drop an item already on the floor, which works correctly,
-- but is weird and useless.
-- | Drop a single item.
dropHuman :: MonadClientUI m => m (Abort CmdSerTakeTime)
dropHuman = do
  -- TODO: allow dropping a given number of identical items.
  Kind.COps{coitem} <- getsState scops
  leader <- getLeaderUI
  bag <- getsState $ getActorBag leader
  inv <- getsState $ getActorInv leader
  ggi <- getAnyItem leader "What to drop?" bag inv "in inventory"
  case ggi of
    Right ((iid, item), (_, container)) ->
      case container of
        CFloor{} -> failWith "never mind"
        CActor aid _ -> do
          assert (aid == leader) skip
          disco <- getsClient sdisco
          subject <- partAidLeader leader
          msgAdd $ makeSentence
            [ MU.SubjectVerbSg subject "drop"
            , partItemWs coitem disco 1 item ]
          return $ Right $ DropSer leader iid
    Left msg -> failWith msg

allObjectsName :: Text
allObjectsName = "Objects"

-- | Let the human player choose any item from a list of items.
getAnyItem :: MonadClientUI m
           => ActorId
           -> Text     -- ^ prompt
           -> ItemBag  -- ^ all items in question
           -> ItemInv  -- ^ inventory characters
           -> Text     -- ^ how to refer to the collection of items
           -> m (Abort ((ItemId, Item), (Int, Container)))
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
        -> m (Abort ((ItemId, Item), (Int, Container)))
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
        if null is0 && EM.null tis
        then failWith "Not carrying anything."
        else perform INone
      invP = EM.filter (\iid -> p (getItemBody iid s)) inv
      perform itemDialogState = do
        let (ims, invOver, msg) = case itemDialogState of
              INone     -> (isp, EM.empty, prompt)
              ISuitable -> (isp, invP, ptext <+> isn <> ".")
              IAll      -> (is0, inv, allObjectsName <+> isn <> ".")
        io <- itemOverlay bag invOver
        akm <- displayChoiceUI (msg <+> choice ims) io (keys ims)
        case akm of
          Left amsg -> failWith amsg
          Right (km@K.KM {..}) -> do
            assert (modifier == K.NoModifier) skip
            case key of
              K.Char '?' -> case itemDialogState of
                INone -> perform ISuitable
                ISuitable | ptext /= allObjectsName -> perform IAll
                _ -> perform INone
              K.Char '-' | floorFull ->
                -- TODO: let player select item
                return $ Right
                       $ maximumBy (compare `on` fst . fst)
                       $ map (\(iid, k) ->
                               ((iid, getItemBody iid s),
                                (k, CFloor (blid b) pos)))
                       $ EM.assocs tis
              K.Char l ->
                case find ((InvChar l ==) . snd . snd) ims of
                  Nothing -> assert `failure` "unexpected inventory letter"
                                    `twith` (km, l,  ims)
                  Just (iidItem, (k, l2)) ->
                    return $ Right (iidItem, (k, CActor aid l2))
              K.Return | bestFull ->
                let (iidItem, (k, l2)) = maximumBy (compare `on` snd . snd) isp
                in return $ Right (iidItem, (k, CActor aid l2))
              _ -> assert `failure` "unexpected key:" `twith` km
  ask

-- * Project

projectAid :: MonadClientUI m
           => ActorId -> [Trigger]
           -> m (Abort CmdSerTakeTime)
projectAid source ts = do
  Kind.COps{cotile} <- getsState scops
  target <- targetToPos
  let tpos = case target of
        Just p -> p
        Nothing -> assert `failure` "target unexpectedly invalid"
                          `twith` source
  eps <- getsClient seps
  sb <- getsState $ getActorBody source
  let lid = blid sb
      spos = bpos sb
  fact <- getsState $ (EM.! bfid sb) . sfactionD
  Level{lxsize, lysize} <- getLevel lid
  foes <- getsState $ actorNotProjList (isAtWar fact) lid
  if foesAdjacent lxsize lysize spos foes
    then failSer ProjectBlockFoes
    else do
      case bla lxsize lysize eps spos tpos of
        Nothing -> failSer ProjectAimOnself
        Just [] -> assert `failure` "project from the edge of level"
                          `twith` (spos, tpos, sb, ts)
        Just (pos : _) -> do
          as <- getsState $ actorList (const True) lid
          lvl <- getLevel lid
          let t = lvl `at` pos
          if not $ Tile.hasFeature cotile F.Clear t
            then failSer ProjectBlockTerrain
            else if unoccupied as pos
                 then projectBla source tpos eps ts
                 else failSer ProjectBlockActor

projectBla :: MonadClientUI m
           => ActorId -> Point -> Int -> [Trigger]
           -> m (Abort CmdSerTakeTime)
projectBla source tpos eps ts = do
  let (verb1, object1) = case ts of
        [] -> ("aim", "object")
        tr : _ -> (verb tr, object tr)
      triggerSyms = triggerSymbols ts
  bag <- getsState $ getActorBag source
  inv <- getsState $ getActorInv source
  ggi <- getGroupItem source bag inv object1 triggerSyms
           (makePhrase ["What to", verb1 MU.:> "?"]) "in inventory"
  case ggi of
    Right ((iid, _), (_, container)) -> do
      stgtMode <- getsClient stgtMode
      case stgtMode of
        Just (TgtAuto _) -> endTargeting True
        _ -> return ()
      return $! Right $ ProjectSer source tpos eps iid container
    Left msg -> failWith msg

triggerSymbols :: [Trigger] -> [Char]
triggerSymbols [] = []
triggerSymbols (ApplyItem{symbol} : ts) = symbol : triggerSymbols ts
triggerSymbols (_ : ts) = triggerSymbols ts

-- * Apply

applyHuman :: MonadClientUI m => [Trigger] -> m (Abort CmdSerTakeTime)
applyHuman ts = do
  leader <- getLeaderUI
  bag <- getsState $ getActorBag leader
  inv <- getsState $ getActorInv leader
  let (verb1, object1) = case ts of
        [] -> ("activate", "object")
        tr : _ -> (verb tr, object tr)
      triggerSyms = triggerSymbols ts
  ggi <- getGroupItem leader bag inv object1 triggerSyms
           (makePhrase ["What to", verb1 MU.:> "?"]) "in inventory"
  case ggi of
    Right ((iid, _), (_, container)) ->
      return $! Right $ ApplySer leader iid container
    Left msg -> failWith msg

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
             -> m (Abort ((ItemId, Item), (Int, Container)))
getGroupItem leader is inv object syms prompt packName = do
  let choice i = jsymbol i `elem` syms
      header = makePhrase [MU.Capitalize (MU.Ws object)]
  getItem leader prompt choice header is inv packName

-- * AlterDir

-- | Ask for a direction and alter a tile, if possible.
alterDirHuman :: MonadClientUI m => [Trigger] -> m (Abort CmdSerTakeTime)
alterDirHuman ts = do
  let verb1 = case ts of
        [] -> "alter"
        tr : _ -> verb tr
      keys = zipWith K.KM (repeat K.NoModifier) K.dirAllMoveKey
      prompt = makePhrase ["What to", verb1 MU.:> "? [movement key"]
  me <- displayChoiceUI prompt emptyOverlay keys
  case me of
    Left msg -> failWith msg
    Right e -> do
      leader <- getLeaderUI
      b <- getsState $ getActorBody leader
      Level{lxsize} <- getLevel $ blid b
      K.handleDir lxsize e (flip (alterTile leader) ts) (failWith "never mind")

-- | Player tries to alter a tile using a feature.
alterTile :: MonadClientUI m
          => ActorId -> Vector -> [Trigger]
          -> m (Abort CmdSerTakeTime)
alterTile source dir ts = do
  Kind.COps{cotile} <- getsState scops
  b <- getsState $ getActorBody source
  lvl <- getLevel $ blid b
  let tpos = bpos b `shift` dir
      t = lvl `at` tpos
      alterFeats = alterFeatures ts
  case filter (\feat -> Tile.hasFeature cotile feat t) alterFeats of
    [] -> return $ guessAlter cotile alterFeats t
    feat : _ -> return $! Right $ AlterSer source tpos $ Just feat

alterFeatures :: [Trigger] -> [F.Feature]
alterFeatures [] = []
alterFeatures (AlterFeature{feature} : ts) = feature : alterFeatures ts
alterFeatures (_ : ts) = alterFeatures ts

-- | Guess and report why the bump command failed.
guessAlter :: Kind.Ops TileKind -> [F.Feature] -> Kind.Id TileKind
           -> Abort CmdSerTakeTime
guessAlter cotile (F.OpenTo _ : _) t | Tile.closable cotile t =
  Left "already open"
guessAlter _ (F.OpenTo _ : _) _ =
  Left "can't be opened"
guessAlter cotile (F.CloseTo _ : _) t | Tile.openable cotile t =
  Left "already closed"
guessAlter _ (F.CloseTo _ : _) _ =
  Left "can't be closed"
guessAlter _ _ _ = Left "never mind"

-- * TriggerTile

-- | Leader tries to trigger the tile he's standing on.
triggerTileHuman :: MonadClientUI m
                 => [Trigger] -> m (Abort CmdSerTakeTime)
triggerTileHuman ts = do
  leader <- getLeaderUI
  triggerTile leader ts

-- | Player tries to trigger a tile using a feature.
triggerTile :: MonadClientUI m
            => ActorId -> [Trigger]
            -> m (Abort CmdSerTakeTime)
triggerTile leader ts = do
  Kind.COps{cotile} <- getsState scops
  b <- getsState $ getActorBody leader
  lvl <- getLevel $ blid b
  let t = lvl `at` bpos b
      triggerFeats = triggerFeatures ts
  case filter (\feat -> Tile.hasFeature cotile feat t) triggerFeats of
    [] -> return $ guessTrigger cotile triggerFeats t
    feat : _ -> do
      go <- verifyTrigger leader feat
      case go of
        Right () -> return $ Right $ TriggerSer leader $ Just feat
        Left msg -> failWith msg

triggerFeatures :: [Trigger] -> [F.Feature]
triggerFeatures [] = []
triggerFeatures (TriggerFeature{feature} : ts) = feature : triggerFeatures ts
triggerFeatures (_ : ts) = triggerFeatures ts

-- | Verify important feature triggers, such as fleeing the dungeon.
verifyTrigger :: MonadClientUI m
              => ActorId -> F.Feature -> m (Abort ())
verifyTrigger leader feat = case feat of
  F.Cause Effect.Escape -> do
    b <- getsState $ getActorBody leader
    side <- getsClient sside
    spawn <- getsState $ isSpawnFaction side
    summon <- getsState $ isSummonFaction side
    if spawn || summon then failWith
      "This is the way out, but where would you go in this alien world?"
    else do
      go <- displayYesNo ColorFull "This is the way out. Really leave now?"
      if not go then failWith "Game resumed."
      else do
        (_, total) <- getsState $ calculateTotal b
        if total == 0 then do
          -- The player can back off at each of these steps.
          go1 <- displayMore ColorBW
                   "Afraid of the challenge? Leaving so soon and empty-handed?"
          if not go1 then failWith "Brave soul!"
          else do
             go2 <- displayMore ColorBW
                     "Next time try to grab some loot before escape!"
             if not go2 then failWith "Here's your chance!"
             else return $ Right ()
        else return $ Right ()
  _ -> return $ Right ()

-- | Guess and report why the bump command failed.
guessTrigger :: Kind.Ops TileKind -> [F.Feature] -> Kind.Id TileKind
             -> Abort CmdSerTakeTime
guessTrigger cotile fs@(F.Cause (Effect.Ascend k) : _) t
  | Tile.hasFeature cotile (F.Cause (Effect.Ascend (-k))) t =
    if k > 0 then
      Left "the way goes down, not up"
    else if k < 0 then
      Left "the way goes up, not down"
    else
      assert `failure` fs
guessTrigger _ fs@(F.Cause (Effect.Ascend k) : _) _ =
    if k > 0 then
      Left "can't ascend"
    else if k < 0 then
      Left "can't descend"
    else
      assert `failure` fs
guessTrigger _ _ _ = Left "never mind"

-- * GameRestart; does not take time

gameRestartHuman :: MonadClientUI m => Text -> m (Abort CmdSer)
gameRestartHuman t = do
  let msg = "You just requested a new" <+> t <+> "game."
  b1 <- displayMore ColorFull msg
  if not b1 then failWith "never mind"
  else do
    b2 <- displayYesNo ColorBW
            "Current progress will be lost! Really restart the game?"
    msg2 <- rndToAction $ oneOf
              [ "Yea, would be a pity to leave them all to die."
              , "Yea, a shame to get your own team stranded." ]
    if not b2 then failWith msg2
    else do
      leader <- getLeaderUI
      return $ Right $ GameRestartSer leader t

-- * GameExit; does not take time

gameExitHuman :: MonadClientUI m => m (Abort CmdSer)
gameExitHuman = do
  go <- displayYesNo ColorFull "Really save and exit?"
  if go then do
    leader <- getLeaderUI
    return $ Right $ GameExitSer leader
  else failWith "Save and exit canceled."

-- * GameSave; does not take time

gameSaveHuman :: MonadClientUI m => m CmdSer
gameSaveHuman = do
  leader <- getLeaderUI
  -- TODO: do not save to history:
  msgAdd "Saving game backup."
  return $ GameSaveSer leader
