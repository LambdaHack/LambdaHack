-- | Semantics of 'Command.Cmd' client commands that return server commands.
-- A couple of them do not take time, the rest does.
-- TODO: document
module Game.LambdaHack.Client.HumanGlobal
  ( -- * Commands that usually take time
    moveRunHuman, waitHuman, pickupHuman, dropHuman, wieldHuman, yieldHuman
  , projectHuman, applyHuman, alterDirHuman, triggerTileHuman
  , stepToTargetHuman, resendHuman
    -- * Commands that never take time
  , gameRestartHuman, gameExitHuman, gameSaveHuman, automateHuman
    -- * Helper definitions
  , SlideOrCmd, failWith
  ) where

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.ConfigUI
import Game.LambdaHack.Client.Draw
import Game.LambdaHack.Client.HumanLocal
import Game.LambdaHack.Client.RunAction
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Action
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Animation
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.HumanCmd (Trigger (..))
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
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.TileKind as TileKind

type SlideOrCmd a = Either Slideshow a

failWith :: MonadClientUI m => Msg -> m (SlideOrCmd a)
failWith msg = do
  modifyClient $ \cli -> cli {slastKey = Nothing}
  stopPlayBack
  assert (not $ T.null msg) $ fmap Left $ promptToSlideshow msg

failSlides :: MonadClientUI m => Slideshow -> m (SlideOrCmd a)
failSlides slides = do
  modifyClient $ \cli -> cli {slastKey = Nothing}
  stopPlayBack
  return $ Left slides

failSer :: MonadClientUI m => FailureSer -> m (SlideOrCmd a)
failSer = failWith . showFailureSer

-- * Move and Run

moveRunHuman :: MonadClientUI m
             => Bool -> Vector -> m (SlideOrCmd CmdTakeTimeSer)
moveRunHuman run dir = do
  tgtMode <- getsClient stgtMode
  if isJust tgtMode then
    fmap Left $ moveCursorHuman dir (if run then 10 else 1)
  else do
    arena <- getArenaUI
    leader <- getLeaderUI
    sb <- getsState $ getActorBody leader
    fact <- getsState $ (EM.! bfid sb) . sfactionD
    let tpos = bpos sb `shift` dir
    -- We start by checking actors at the the target position,
    -- which gives a partial information (actors can be invisible),
    -- as opposed to accessibility (and items) which are always accurate
    -- (tiles can't be invisible).
    tgts <- getsState $ posToActors tpos arena
    case tgts of
      [] -> do  -- move or search or alter
        -- Start running in the given direction. The first turn of running
        -- succeeds much more often than subsequent turns, because we ignore
        -- most of the disturbances, since the player is mostly aware of them
        -- and still explicitly requests a run, knowing how it behaves.
        runStopOrCmd <- moveRunAid leader dir
        case runStopOrCmd of
          Left stopMsg -> failWith stopMsg
          Right runCmd -> do
            sel <- getsClient sselected
            let runMembers = if isSpawnFact fact
                             then [leader]  -- TODO: warn?
                             else ES.toList (ES.delete leader sel) ++ [leader]
                runParams = RunParams { runLeader = leader
                                      , runMembers
                                      , runDist = 0
                                      , runStopMsg = Nothing
                                      , runInitDir = Just dir }
            when run $ modifyClient $ \cli -> cli {srunning = Just runParams}
            return $ Right runCmd
        -- When running, the invisible actor is hit (not displaced!),
        -- so that running in the presence of roving invisible
        -- actors is equivalent to moving (with visible actors
        -- this is not a problem, since runnning stops early enough).
        -- TODO: stop running at invisible actor
      [((target, _), _)] | run ->
        -- Displacing requires accessibility, but it's checked later on.
        displaceAid leader target
      _ : _ : _ | run -> do
        assert (all (bproj . snd . fst) tgts) skip
        failSer DisplaceProjectiles
      ((target, tb), _) : _ -> do
        -- No problem if there are many projectiles at the spot. We just
        -- attack the first one.
        -- We always see actors from our own faction.
        if bfid tb == bfid sb && not (bproj tb) then do
          if isSpawnFact fact then
            failWith "spawners cannot manually change leaders"
          else do
            -- Select adjacent actor by bumping into him. Takes no time.
            success <- pickLeader target
            assert (success `blame` "bump self"
                            `twith` (leader, target, tb)) skip
            return $ Left mempty
        else
          -- Attacking does not require full access, adjacency is enough.
          meleeAid leader target

-- | Actor atttacks an enemy actor or his own projectile.
meleeAid :: MonadClientUI m
         => ActorId -> ActorId -> m (SlideOrCmd CmdTakeTimeSer)
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
            => ActorId -> ActorId -> m (SlideOrCmd CmdTakeTimeSer)
displaceAid source target = do
  cops <- getsState scops
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  let adj = checkAdjacent sb tb
  if not adj then failSer DisplaceDistant
  else do
    let lid = blid sb
    lvl <- getLevel lid
    let spos = bpos sb
        tpos = bpos tb
    -- Displacing requires full access.
    if accessible cops lvl spos tpos then do
      tgts <- getsState $ posToActors tpos lid
      case tgts of
        [] -> assert `failure` (source, sb, target, tb)
        [_] -> do
          return $ Right $ DisplaceSer source target
        _ -> failSer DisplaceProjectiles
    else failSer DisplaceAccess

-- * Wait

-- | Leader waits a turn (and blocks, etc.).
waitHuman :: MonadClientUI m => m CmdTakeTimeSer
waitHuman = do
  modifyClient $ \cli -> cli {swaitTimes = abs (swaitTimes cli) + 1}
  leader <- getLeaderUI
  return $! WaitSer leader

-- * Pickup

pickupHuman :: MonadClientUI m => m (SlideOrCmd CmdTakeTimeSer)
pickupHuman = do
  leader <- getLeaderUI
  body <- getsState $ getActorBody leader
  ggi <- getAnyItem False "What to pick up?"
                    [CFloor (blid body) (bpos body)] False True
                    "from the floor"
  case ggi of
    Right ((iid, item), (k, _)) -> do
      slots <- getsClient sslots
      freeSlot <- getsClient sfreeSlot
      let l = if jsymbol item == '$' then Just $ SlotChar '$' else Nothing
      mc <- getsState $ assignSlot iid l body slots freeSlot
      case mc of
        Just l2 -> do
          modifyClient $ \cli ->
            cli { sslots = EM.insert l2 iid (sslots cli)
                , sfreeSlot = max l2 (sfreeSlot cli) }
          return $ Right $ PickupSer leader iid k
        Nothing -> failSer PickupOverfull
    Left slides -> return $ Left slides

-- * Drop

-- TODO: you can drop an item already on the floor (the '-' is there),
-- which is weird and useless.
-- | Drop a single item.
dropHuman :: MonadClientUI m => m (SlideOrCmd CmdTakeTimeSer)
dropHuman = do
  -- TODO: allow dropping a given number of identical items.
  Kind.COps{coitem} <- getsState scops
  leader <- getLeaderUI
  ggi <- getAnyItem True "What to drop?" [CInv leader, CEqp leader] True True
                    "in inventory"
  case ggi of
    Right ((iid, item), (k, container)) ->
      case container of
        CInv aid -> do
          assert (aid == leader) skip
          disco <- getsClient sdisco
          subject <- partAidLeader leader
          msgAdd $ makeSentence
            [ MU.SubjectVerbSg subject "drop"
            , partItemWs coitem disco k item ]
          -- Do not remove from item slots, only from the bag.
          return $ Right $ DropSer leader iid k
        _ -> failWith "never mind"
    Left slides -> return $ Left slides

allObjectsName :: Text
allObjectsName = "Objects"

-- | Let a human player choose any item with a given group name.
-- Note that this does not guarantee the chosen item belongs to the group,
-- as the player can override the choice.
getGroupItem :: MonadClientUI m
             => MU.Part  -- ^ name of the group
             -> [Char]   -- ^ accepted item symbols
             -> Text     -- ^ prompt
             -> [Container]  -- ^ legal containers
             -> Bool     -- ^ whether to ask for the number of items
             -> Bool     -- ^ whether the default is all, instead of one
             -> Text     -- ^ how to refer to the collection of objects
             -> m (SlideOrCmd ((ItemId, Item), (Int, Container)))
getGroupItem object syms prompt = do
  let choice i = jsymbol i `elem` syms
      header = makePhrase [MU.Capitalize (MU.Ws object)]
  getItem True prompt choice header

-- | Let the human player choose any item from a list of items.
getAnyItem :: MonadClientUI m
           => Bool
           -> Text
           -> [Container]
           -> Bool
           -> Bool
           -> Text
           -> m (SlideOrCmd ((ItemId, Item), (Int, Container)))
getAnyItem askWhenLone prompt =
  getItem askWhenLone prompt (const True) allObjectsName

data ItemDialogState = INone | ISuitable | IAll deriving Eq

-- | Let the human player choose a single, preferably suitable,
-- item from a list of items.
-- For now the fourth argument can contain at most the inventory,
-- equipment and floor of the current leader.
getItem :: forall m. MonadClientUI m
        => Bool            -- ^ whether to ask if the item alone
                           --   in the starting container and suitable
        -> Text            -- ^ prompt message
        -> (Item -> Bool)  -- ^ which items to consider suitable
        -> Text            -- ^ how to describe suitable items
        -> [Container]     -- ^ legal containers
        -> Bool            -- ^ whether to ask for the number of items
        -> Bool            -- ^ whether the default is all, instead of one
        -> Text            -- ^ how to refer to the collection of items
        -> m (SlideOrCmd ((ItemId, Item), (Int, Container)))
getItem askWhenLone prompt p ptext cLegal askNumber allNumber isn = do
  leader <- getLeaderUI
  body <- getsState $ getActorBody leader
  s <- getState
  let cStart = case cLegal of
        [] -> assert `failure` prompt
        c : _ -> c
      cFloor = CFloor (blid body) (bpos body)
      cInv = CInv leader
      cEqp = CEqp leader
      mloneItem = case EM.assocs (getCBag cStart s) of
                    [(iid, k)] -> Just ((iid, getItemBody iid s), k)
                    _ -> Nothing
  assert (all (`elem` [cFloor, cInv, cEqp]) cLegal) skip
  slots <- getsClient sslots
  let ask = do
        if all (null . EM.elems) $ map (flip getCBag s) cLegal
        then failWith $ "nothing" <+> isn
        else if not askWhenLone && fmap (p . snd . fst) mloneItem == Just True
        then case mloneItem of
          Nothing -> assert `failure` cStart
          Just (iidItem, k) -> return $ Right (iidItem, (k, cStart))
        else do
          soc <- perform INone cStart cStart
          case soc of
            Left slides -> return $ Left slides
            Right (iidItem, (kAll, c)) -> do
              let kDefault = if allNumber then kAll else 1
                  kRet k = return $ Right (iidItem, (k, c))
              if askNumber && kAll > 1 then do
                let tDefault = tshow kDefault
                    kbound = min 9 kAll
                    kprompt = "Choose number [1-" <> tshow kbound
                              <> ", RET(" <> tDefault <> ")"
                    kkeys = zipWith K.KM (repeat K.NoModifier)
                            $ map (K.Char . Char.intToDigit) [1..kbound]
                              ++ [K.Return]
                kkm <- displayChoiceUI kprompt emptyOverlay kkeys
                case kkm of
                  Left slides -> failSlides slides
                  Right K.KM{key} ->
                    case key of
                      K.Char l -> kRet $! Char.digitToInt l
                      K.Return -> kRet kDefault
                      _ -> assert `failure` "unexpected key:" `twith` kkm
              else kRet kDefault
      isCFull c = length cLegal > 1
                  && c `elem` cLegal
                  && not (EM.null (getCBag c s))
      perform :: ItemDialogState -> Container -> Container
              -> m (SlideOrCmd ((ItemId, Item), (Int, Container)))
      perform itemDialogState cCur cPrev = do
        bag <- getsState $ getCBag cCur
        let inv = EM.filter (`EM.member` bag) slots
            invP = EM.filter (\iid -> p (getItemBody iid s)) inv
            checkItem (l, iid) = ((iid, getItemBody iid s), (bag EM.! iid, l))
            is0 = map checkItem $ EM.assocs inv
            floorFull = isCFull cFloor
            (floorMsg, floorKey) | floorFull = (", -", [K.Char '-'])
                                 | otherwise = ("", [])
            invEqpFull = isCFull cInv && isCFull cEqp
            (invEqpMsg, invEqpKey) | invEqpFull = (", /", [K.Char '/'])
                                   | otherwise = ("", [])
            isp = filter (p . snd . fst) is0
            bestFull = not $ null isp
            (bestMsg, bestKey)
              | bestFull =
                let bestSlot = slotChar $ maximum $ map (snd . snd) isp
                in (", RET(" <> T.singleton bestSlot <> ")", [K.Return])
              | otherwise = ("", [])
            keys ims2 =
              let mls = map (snd . snd) ims2
                  ks = map (K.Char . slotChar) mls
                       ++ [K.Char '?'] ++ floorKey ++ invEqpKey ++ bestKey
              in zipWith K.KM (repeat K.NoModifier) ks
            choice ims2 =
              if null ims2
              then "[?" <> floorMsg <> invEqpMsg
              else let mls = map (snd . snd) ims2
                       r = slotRange mls
                   in "[" <> r <> ", ?" <> floorMsg <> invEqpMsg <> bestMsg
            (ims, invOver, msg) = case itemDialogState of
              INone     -> (isp, EM.empty, prompt)
              ISuitable -> (isp, invP, ptext <+> isn <> ".")
              IAll      -> (is0, inv, allObjectsName <+> isn <> ".")
        io <- itemOverlay bag invOver
        akm <- displayChoiceUI (msg <+> choice ims) io (keys is0)
        case akm of
          Left slides -> failSlides slides
          Right km@K.KM{..} -> do
            assert (modifier == K.NoModifier) skip
            case key of
              K.Char '?' -> case itemDialogState of
                INone -> if EM.null invP
                         then perform IAll cCur cPrev
                         else perform ISuitable cCur cPrev
                ISuitable | ptext /= allObjectsName -> perform IAll cCur cPrev
                _ -> perform INone cCur cPrev
              K.Char '-' | floorFull ->
                let cNext = if cCur == cFloor then cPrev else cFloor
                in perform itemDialogState cNext cCur
              K.Char '/' | invEqpFull ->
                let cNext = if cCur == cInv then cEqp else cInv
                in perform itemDialogState cNext cCur
              K.Char l ->
                case find ((SlotChar l ==) . snd . snd) is0 of
                  Nothing -> assert `failure` "unexpected inventory slot"
                                    `twith` (km, l,  is0)
                  Just (iidItem, (k, _)) ->
                    return $ Right (iidItem, (k, cCur))
              K.Return | bestFull ->
                let (iidItem, (k, _)) = maximumBy (compare `on` snd . snd) isp
                in return $ Right (iidItem, (k, cCur))
              _ -> assert `failure` "unexpected key:" `twith` akm
  ask

-- * Wield

-- Wield/wear a single item.
wieldHuman :: MonadClientUI m => m (SlideOrCmd CmdTakeTimeSer)
wieldHuman = do
  Kind.COps{coitem} <- getsState scops
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  ggi <- getAnyItem True "What to wield/wear?"
                    [CInv leader, CFloor (blid b) (bpos b)] True True
                    "in inventory"
  case ggi of
    Right ((iid, item), (k, container)) ->
      case container of
        CInv aid -> do
          assert (aid == leader) skip
          disco <- getsClient sdisco
          subject <- partAidLeader leader
          msgAdd $ makeSentence
            [ MU.SubjectVerbSg subject "wield"
            , partItemWs coitem disco k item ]
          return $ Right $ WieldSer leader iid k
        _ -> failWith "never mind"
    Left slides -> return $ Left slides

-- * Yield

-- TODO: you can specify an item already on the floor (the '-' is there),
-- which is weird and useless.
-- | Take off a single item.
yieldHuman :: MonadClientUI m => m (SlideOrCmd CmdTakeTimeSer)
yieldHuman = do
  -- TODO: allow dropping a given number of identical items.
  Kind.COps{coitem} <- getsState scops
  leader <- getLeaderUI
  ggi <- getAnyItem True "What to take off?"
                    [CEqp leader] True True "in equipment"
  case ggi of
    Right ((iid, item), (k, container)) ->
      case container of
        CEqp aid -> do
          assert (aid == leader) skip
          disco <- getsClient sdisco
          subject <- partAidLeader leader
          msgAdd $ makeSentence
            [ MU.SubjectVerbSg subject "take off"
            , partItemWs coitem disco k item ]
          return $ Right $ YieldSer leader iid k
        _ -> failWith "never mind"
    Left slides -> return $ Left slides

-- * Project

projectHuman :: MonadClientUI m
             => [Trigger] -> m (SlideOrCmd CmdTakeTimeSer)
projectHuman ts = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  tgtPos <- leaderTgtToPos
  case tgtPos of
    Nothing -> failWith "last target invalid"
    Just pos | pos == bpos b -> failWith "cannot aim at oneself"
    Just pos -> do
      canAim <- leaderTgtAims
      case canAim of
        Nothing -> projectPos ts pos
        Just cause -> failWith cause

projectPos :: MonadClientUI m
           => [Trigger] -> Point -> m (SlideOrCmd CmdTakeTimeSer)
projectPos ts tpos = do
  Kind.COps{coactor=Kind.Ops{okind}, cotile} <- getsState scops
  leader <- getLeaderUI
  eps <- getsClient seps
  sb <- getsState $ getActorBody leader
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
          lvl <- getLevel lid
          let t = lvl `at` pos
          if not $ Tile.isWalkable cotile t
            then failSer ProjectBlockTerrain
            else do
              mab <- getsState $ posToActor pos lid
              if maybe True (bproj . snd . fst) mab
              then if not (asight $ okind $ bkind sb)
                   then failSer ProjectBlind
                   else projectEps ts tpos eps
              else failSer ProjectBlockActor

projectEps :: MonadClientUI m
           => [Trigger] -> Point -> Int
           -> m (SlideOrCmd CmdTakeTimeSer)
projectEps ts tpos eps = do
  let (verb1, object1) = case ts of
        [] -> ("aim", "object")
        tr : _ -> (verb tr, object tr)
      triggerSyms = triggerSymbols ts
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  ggi <- getGroupItem object1 triggerSyms
           (makePhrase ["What to", verb1 MU.:> "?"])
           [CInv leader, CFloor (blid b) (bpos b)] False False "in inventory"
  case ggi of
    Right ((iid, _), (_, container)) ->
      return $ Right $ ProjectSer leader tpos eps iid container
    Left slides -> return $ Left slides

triggerSymbols :: [Trigger] -> [Char]
triggerSymbols [] = []
triggerSymbols (ApplyItem{symbol} : ts) = symbol : triggerSymbols ts
triggerSymbols (_ : ts) = triggerSymbols ts

-- * Apply

applyHuman :: MonadClientUI m => [Trigger] -> m (SlideOrCmd CmdTakeTimeSer)
applyHuman ts = do
  leader <- getLeaderUI
  b <- getsState $ getActorBody leader
  let (verb1, object1) = case ts of
        [] -> ("activate", "object")
        tr : _ -> (verb tr, object tr)
      triggerSyms = triggerSymbols ts
  ggi <- getGroupItem object1 triggerSyms
           (makePhrase ["What to", verb1 MU.:> "?"])
           [CInv leader, CFloor (blid b) (bpos b)] False False "in inventory"
  case ggi of
    Right ((iid, _), (_, container)) ->
      return $ Right $ ApplySer leader iid container
    Left slides -> return $ Left slides

-- * AlterDir

-- | Ask for a direction and alter a tile, if possible.
alterDirHuman :: MonadClientUI m => [Trigger] -> m (SlideOrCmd CmdTakeTimeSer)
alterDirHuman ts = do
  ConfigUI{configVi, configLaptop} <- getsClient sconfigUI
  let verb1 = case ts of
        [] -> "alter"
        tr : _ -> verb tr
      keys = zipWith K.KM (repeat K.NoModifier)
                          (K.dirAllKey configVi configLaptop)
      prompt = makePhrase ["What to", verb1 MU.:> "? [movement key"]
  me <- displayChoiceUI prompt emptyOverlay keys
  case me of
    Left slides -> failSlides slides
    Right e -> do
      leader <- getLeaderUI
      K.handleDir configVi configLaptop e (flip (alterTile leader) ts)
                                          (failWith "never mind")

-- | Player tries to alter a tile using a feature.
alterTile :: MonadClientUI m
          => ActorId -> Vector -> [Trigger]
          -> m (SlideOrCmd CmdTakeTimeSer)
alterTile source dir ts = do
  Kind.COps{cotile} <- getsState scops
  b <- getsState $ getActorBody source
  lvl <- getLevel $ blid b
  let tpos = bpos b `shift` dir
      t = lvl `at` tpos
      alterFeats = alterFeatures ts
  case filter (\feat -> Tile.hasFeature cotile feat t) alterFeats of
    [] -> failWith $ guessAlter cotile alterFeats t
    feat : _ -> return $ Right $ AlterSer source tpos $ Just feat

alterFeatures :: [Trigger] -> [F.Feature]
alterFeatures [] = []
alterFeatures (AlterFeature{feature} : ts) = feature : alterFeatures ts
alterFeatures (_ : ts) = alterFeatures ts

-- | Guess and report why the bump command failed.
guessAlter :: Kind.Ops TileKind -> [F.Feature] -> Kind.Id TileKind -> Msg
guessAlter cotile (F.OpenTo _ : _) t
  | Tile.isClosable cotile t = "already open"
guessAlter _ (F.OpenTo _ : _) _ = "cannot be opened"
guessAlter cotile (F.CloseTo _ : _) t
  | Tile.isOpenable cotile t = "already closed"
guessAlter _ (F.CloseTo _ : _) _ = "cannot be closed"
guessAlter _ _ _ = "never mind"

-- * TriggerTile

-- | Leader tries to trigger the tile he's standing on.
triggerTileHuman :: MonadClientUI m
                 => [Trigger] -> m (SlideOrCmd CmdTakeTimeSer)
triggerTileHuman ts = do
  tgtMode <- getsClient stgtMode
  if isJust tgtMode then do
    let getK tfs = case tfs of
          TriggerFeature {feature = F.Cause (Effect.Ascend k)} : _ -> Just k
          _ : rest -> getK rest
          [] -> Nothing
        mk = getK ts
    case mk of
      Nothing -> failWith  "never mind"
      Just k -> fmap Left $ tgtAscendHuman k
  else do
    leader <- getLeaderUI
    triggerTile leader ts

-- | Player tries to trigger a tile using a feature.
triggerTile :: MonadClientUI m
            => ActorId -> [Trigger]
            -> m (SlideOrCmd CmdTakeTimeSer)
triggerTile leader ts = do
  Kind.COps{cotile} <- getsState scops
  b <- getsState $ getActorBody leader
  lvl <- getLevel $ blid b
  let t = lvl `at` bpos b
      triggerFeats = triggerFeatures ts
  case filter (\feat -> Tile.hasFeature cotile feat t) triggerFeats of
    [] -> failWith $ guessTrigger cotile triggerFeats t
    feat : _ -> do
      go <- verifyTrigger leader feat
      case go of
        Right () -> return $ Right $ TriggerSer leader $ Just feat
        Left slides -> return $ Left slides

triggerFeatures :: [Trigger] -> [F.Feature]
triggerFeatures [] = []
triggerFeatures (TriggerFeature{feature} : ts) = feature : triggerFeatures ts
triggerFeatures (_ : ts) = triggerFeatures ts

-- | Verify important feature triggers, such as fleeing the dungeon.
verifyTrigger :: MonadClientUI m
              => ActorId -> F.Feature -> m (SlideOrCmd ())
verifyTrigger leader feat = case feat of
  F.Cause Effect.Escape{} -> do
    cops <- getsState scops
    b <- getsState $ getActorBody leader
    side <- getsClient sside
    fact <- getsState $ (EM.! side) . sfactionD
    let isHero = isHeroFact cops fact
    if not isHero then failWith
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
guessTrigger :: Kind.Ops TileKind -> [F.Feature] -> Kind.Id TileKind -> Msg
guessTrigger cotile fs@(F.Cause (Effect.Ascend k) : _) t
  | Tile.hasFeature cotile (F.Cause (Effect.Ascend (-k))) t =
    if k > 0 then "the way goes down, not up"
    else if k < 0 then "the way goes up, not down"
    else assert `failure` fs
guessTrigger _ fs@(F.Cause (Effect.Ascend k) : _) _ =
    if k > 0 then "cannot ascend"
    else if k < 0 then "cannot descend"
    else assert `failure` fs
guessTrigger _ _ _ = "never mind"

-- * StepToTarget

stepToTargetHuman :: MonadClientUI m => m (SlideOrCmd CmdTakeTimeSer)
stepToTargetHuman = do
  tgtMode <- getsClient stgtMode
  -- Movement is legal only outside targeting mode.
  -- TODO: use this command for something in targeting mode.
  if isJust tgtMode then failWith "cannot move in targeting mode"
  else do
    leader <- getLeaderUI
    b <- getsState $ getActorBody leader
    tgtPos <- leaderTgtToPos
    case tgtPos of
      Nothing -> failWith "target not set"
      Just c | c == bpos b -> failWith "target reached"
      Just c -> do
        (_, mpath) <- getCacheBfsAndPath leader c
        case mpath of
          Nothing -> failWith "no route to target"
          Just [] -> assert `failure` (leader, b, bpos b, c)
          Just (p1 : _) -> do
            as <- getsState $ posToActors p1 (blid b)
            if not $ null as then
              failWith "actor in the path to target"
            else
              moveRunHuman False $ towards (bpos b) p1

-- * Resend

resendHuman :: MonadClientUI m => m (SlideOrCmd CmdTakeTimeSer)
resendHuman = do
  slastCmd <- getsClient slastCmd
  case slastCmd of
    Just cmd -> return $ Right cmd
    Nothing -> failWith "no time-taking command to repeat"

-- * GameRestart; does not take time

gameRestartHuman :: MonadClientUI m => Text -> m (SlideOrCmd CmdSer)
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
      DebugModeCli{sdifficultyCli} <- getsClient sdebugCli
      ConfigUI{configHeroNames} <- getsClient sconfigUI
      return $ Right $ GameRestartSer leader t sdifficultyCli configHeroNames

-- * GameExit; does not take time

gameExitHuman :: MonadClientUI m => m (SlideOrCmd CmdSer)
gameExitHuman = do
  go <- displayYesNo ColorFull "Really save and exit?"
  if go then do
    leader <- getLeaderUI
    DebugModeCli{sdifficultyCli} <- getsClient sdebugCli
    return $ Right $ GameExitSer leader sdifficultyCli
  else failWith "Save and exit canceled."

-- * GameSave; does not take time

gameSaveHuman :: MonadClientUI m => m CmdSer
gameSaveHuman = do
  leader <- getLeaderUI
  -- TODO: do not save to history:
  msgAdd "Saving game backup."
  return $! GameSaveSer leader

-- * Automate; does not take time

automateHuman :: MonadClientUI m => m (SlideOrCmd CmdSer)
automateHuman = do
  leader <- getLeaderUI
  -- TODO: do not save to history:
  go <- displayMore ColorBW "Ceding control to AI (ESC to regain)."
  if not go
    then failWith "Automation canceled."
    else return $ Right $ AutomateSer leader
