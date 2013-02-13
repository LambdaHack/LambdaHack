{-# LANGUAGE DeriveDataTypeable, GADTs, OverloadedStrings, StandaloneDeriving
             #-}
-- | Semantics of 'CmdCli' client commands.
module Game.LambdaHack.Client.CmdCliSem where

import Control.Monad
import Control.Monad.Writer.Strict (WriterT, runWriterT)
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid (mempty)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Client.Action
import Game.LambdaHack.Client.Animation
import Game.LambdaHack.Client.Binding
import Game.LambdaHack.Client.CmdHuman
import Game.LambdaHack.Client.CmdHumanSem
import Game.LambdaHack.Client.Draw
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.LocalAction
import Game.LambdaHack.Client.RunAction
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.Strategy
import Game.LambdaHack.Client.StrategyAction
import Game.LambdaHack.CmdAtomic
import Game.LambdaHack.CmdAtomicSem
import Game.LambdaHack.CmdSer
import qualified Game.LambdaHack.Color as Color
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.StrategyKind
import Game.LambdaHack.Faction
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.Random
import Game.LambdaHack.Server.EffectSem
import Game.LambdaHack.State
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Vector

-- * cmdUpdateCli

-- | Make the item known to the player.
discoverCli :: (MonadAction m, MonadClient m) => Kind.Id ItemKind -> Item -> m ()
discoverCli ik i = do
  Kind.COps{coitem} <- getsState scops
  oldDisco <- getsState sdisco
  let ix = jkindIx i
  unless (ix `EM.member` oldDisco) $ do
    modifyState (updateDisco (EM.insert ix ik))
    disco <- getsState sdisco
    let (object1, object2) = partItem coitem oldDisco i
        msg = makeSentence
          [ "the", MU.SubjectVerbSg (MU.Phrase [object1, object2])
                                    "turn out to be"
          , partItemAW coitem disco i ]
    msgAdd msg

remCli :: MonadAction m => ES.EnumSet Point -> Level -> LevelId -> m ()
remCli vis lvl arena = do
  cops <- getsState scops
  actorD <- getsState sactorD
  let updArena dng =
        let clvl = fromMaybe (assert `failure` arena) $ EM.lookup arena dng
            nlvl = rememberLevel cops actorD vis lvl clvl
        in EM.insert arena nlvl dng
  modifyState $ updateDungeon updArena

rememberCli :: (MonadAction m, MonadClient m)
            => Level -> LevelId -> ActorDict -> ItemDict -> FactionDict -> m ()
rememberCli lvl lid actorD itemD faction = do
  per <- askPerception
  -- TODO: instead gather info about factions when first encountered
  -- and update when they are killed
  modifyState $ updateFaction (const faction)
  modifyState $ updateActorD (const actorD)
  -- TODO: only add new visible items
  modifyState $ updateItemD (const itemD)
  remCli (totalVisible per) lvl lid

rememberPerCli :: (MonadAction m, MonadClient m)
               => Perception -> Level -> LevelId
               -> ActorDict -> ItemDict -> FactionDict
               -> m ()
rememberPerCli per lvl lid actorD itemD faction = do
  modifyClient $ \cli -> cli {sper = EM.insert lid per (sper cli)}
  rememberCli lvl lid actorD itemD faction

-- switchLevelCli :: MonadClient m
--                => ActorId -> LevelId -> Actor -> ItemBag
--                -> m ()
-- switchLevelCli aid arena pbody items = do
--   arenaOld <- getsState sarena
--   assert (arenaOld /= arena) $ do
--     modifyClient $ invalidateSelectedLeader
--     modifyState $ updateSelectedArena arena
--     modifyState (insertActor aid pbody)
--     modifyState (updateActorDItem aid (const items))
--     loc <- getState
--     modifyClient $ updateSelectedLeader aid loc

showAttackCli :: MonadClient m
              => ActorId -> ActorId -> MU.Part -> Item -> Bool
              -> m ()
showAttackCli source target verb stack say = do
  Kind.COps{ coactor, coitem } <- getsState scops
  per <- askPerception
  disco <- getsState sdisco
  smRaw <- getsState (getActorBody source)
  tmRaw <- getsState (getActorBody target)
  let spos = bpos smRaw
      tpos = bpos tmRaw
      svisible = spos `ES.member` totalVisible per
      tvisible = tpos `ES.member` totalVisible per
      sm | svisible  = smRaw
         | otherwise = smRaw {bname = Just "somebody"}
      tm | tvisible  = tmRaw
         | otherwise = tmRaw {bname = Just "somebody"}
  -- The msg describes the source part of the action.
  -- TODO: right now it also describes the victim and weapon;
  -- perhaps, when a weapon is equipped, just say "you hit"
  -- or "you miss" and then "nose dies" or "nose yells in pain".
  let msg = makeSentence $
        [ MU.SubjectVerbSg (partActor coactor sm) verb
        , partActor coactor tm ]
        ++ if say
           then ["with", partItemAW coitem disco stack]
           else []
  msgAdd msg

-- TODO: here or elsewhere re-read RNG seed from config file
restartCli :: (MonadAction m, MonadClient m) => FactionPers -> State -> m ()
restartCli sper loc = do
  shistory <- getsClient shistory
  sconfigUI <- getsClient sconfigUI
  side <- getsClient sside
  let cli = defStateClient shistory sconfigUI side
  putClient cli {sper}
  putState loc
  -- Save ASAP in case of crashes and disconnects.
  --TODO

atomicSeen :: MonadAction m => Atomic -> m ()
atomicSeen atomic = case atomic of
  Left cmd -> cmdAtomicSem cmd
  Right _ -> return ()

cmdAtomicCli :: MonadClient m => CmdAtomic -> m ()
cmdAtomicCli cmd = case cmd of
  CreateActorA _ _ -> return ()
  DestroyActorA _ _ -> return ()
  CreateItemA _ _ _ _ _ -> return ()
  DestroyItemA _ _ _ _ _ -> return ()
  MoveActorA aid fromP toP -> return ()
  WaitActorA _ _ _ -> return ()
  DisplaceActorA source target -> return ()
  MoveItemA _ iid k c1 c2 -> moveItemA iid k c1 c2
  HealActorA aid n -> do
    Kind.COps{coactor} <- getsState scops
    b <- getsState $ getActorBody aid
    void $ return $ actorVerb coactor b "feel better"
  HasteActorA aid delta -> return ()
  DominateActorA target fromFid toFid -> return ()
  PathActorA aid fromPath toPath -> return ()
  ColorActorA aid fromCol toCol -> return ()
  QuitFactionA fid fromSt toSt -> return ()
  LeadFactionA fid source target ->
    modifyClient $ \cli -> cli {_sleader = target}
  AlterTileA _ _ _ _ -> return ()
  AlterSecretA lid diffL -> return ()
  AlterSmellA lid diffL -> return ()
  SyncA -> return ()

actorVerbMU :: MonadClient m => Actor -> MU.Part -> m ()
actorVerbMU body verb = do
  Kind.COps{coactor} <- getsState scops
  let msg = makeSentence [MU.SubjectVerbSg (partActor coactor body) verb]
  msgAdd msg

aVerbMU :: MonadClient m => ActorId -> MU.Part -> m ()
aVerbMU aid verb = do
  body <- getsState $ getActorBody aid
  actorVerbMU body verb

iVerbMU :: MonadClient m => ItemId -> Int -> MU.Part -> m ()
iVerbMU iid k verb = do
  Kind.COps{coitem} <- getsState scops
  disco <- getsState sdisco
  item <- getsState $ getItemBody iid
  let msg =
        makeSentence [MU.SubjectVerbSg (partItemNWs coitem disco k item) verb]
  msgAdd msg

aiVerbMU :: MonadClient m => ActorId -> MU.Part -> ItemId -> Int -> m ()
aiVerbMU aid verb iid k = do
  Kind.COps{coactor, coitem} <- getsState scops
  disco <- getsState sdisco
  body <- getsState $ getActorBody aid
  item <- getsState $ getItemBody iid
  -- TODO: "you" instead of partActor? but keep partActor for other sides
  -- TODO: perhaps automate you/partActor depending on side
  let msg = makeSentence
        [ MU.SubjectVerbSg (partActor coactor body) verb
        , partItemNWs coitem disco k item ]
  msgAdd msg

moveItemA :: MonadClient m => ItemId -> Int -> Container -> Container -> m ()
moveItemA iid k c1 c2 = do
  Kind.COps{coitem} <- getsState scops
  item <- getsState $ getItemBody iid
  disco <- getsState sdisco
  case (c1, c2) of
    (CFloor _, CActor aid l) -> do
      b <- getsState $ getActorBody aid
      side <- getsClient sside
      if bfaction b == side then
        msgAdd $ makePhrase [ letterLabel l
                            , partItemNWs coitem disco k item
                            , "\n" ]
      else aiVerbMU aid "pick up" iid k
    _ -> return ()

displayHeal :: MonadClientUI m => Msg -> (Point, Point) -> Int -> Bool -> m ()
displayHeal msg poss deltaHP block = do
  msgAdd msg
  cli <- getClient
  loc <- getState
  per <- askPerception
  -- Try to show an animation. Sometimes, e.g., when HP is unchaged,
  -- the animation will not be shown, but a single frame with @msg@ will.
  let anim | deltaHP > 0 =
        twirlSplash poss Color.BrBlue Color.Blue
           | deltaHP < 0 && block =
        blockHit    poss Color.BrRed  Color.Red
           | deltaHP < 0 && not block =
        twirlSplash poss Color.BrRed  Color.Red
           | otherwise = mempty
      animFrs = animate cli loc per anim
  displayFramesPush $ Nothing : animFrs

cmdAtomicVerboseCli :: MonadClient m => CmdAtomic -> m ()
cmdAtomicVerboseCli cmd = case cmd of
  CreateActorA aid body -> actorVerbMU body "appear"
  DestroyActorA aid body -> actorVerbMU body "disappear"
  CreateItemA _ iid item k _ -> iVerbMU iid k "appear"
  DestroyItemA _ iid item k _ -> iVerbMU iid k "disappear"
  MoveActorA aid fromP toP -> return ()
  WaitActorA aid _ _ -> aVerbMU aid "wait"
  DisplaceActorA source target -> return ()
  MoveItemA lid iid k c1 c2 -> return ()
  HealActorA aid n -> do
    Kind.COps{coactor} <- getsState scops
    b <- getsState $ getActorBody aid
    void $ return $ actorVerb coactor b "feel better"
  HasteActorA aid delta -> return ()
  DominateActorA target fromFid toFid -> return ()
  PathActorA aid fromPath toPath -> return ()
  ColorActorA aid fromCol toCol -> return ()
  QuitFactionA fid fromSt toSt -> return ()
  LeadFactionA fid source target ->
    modifyClient $ \cli -> cli {_sleader = target}
  AlterTileA lid p fromTile toTile -> return ()  -- TODO: door opens
  AlterSecretA lid diffL -> return ()
  AlterSmellA lid diffL -> return ()
  SyncA -> return ()

descAtomicCli :: MonadClient m => DescAtomic -> m ()
descAtomicCli desc = case desc of
  StrikeA source target iid b -> return ()
  RecoilA source target iid b -> return ()
  ProjectA aid iid -> aiVerbMU aid "aim" iid 1
  CatchA aid iid -> aiVerbMU aid "catch" iid 1
  ActivateA aid iid -> aiVerbMU aid "activate" {-TODO-} iid 1
  CheckA aid iid -> aiVerbMU aid "check" iid 1
  TriggerA _ _ _ _-> return ()
  ShunA _ _ _ _ -> return ()

descAtomicVerboseCli :: MonadClient m => DescAtomic -> m ()
descAtomicVerboseCli desc = case desc of
  StrikeA source target iid b -> return ()
  RecoilA source target iid b -> return ()
  ProjectA aid iid -> return ()
  CatchA aid iid -> return ()
  ActivateA aid iid -> return ()
  CheckA aid iid -> return ()
  TriggerA aid _p _feat _ -> aVerbMU aid $ "trigger"  -- TODO: opens door
  ShunA aid _p _ _ -> aVerbMU aid $ "shun"  -- TODO: shuns stairs down

-- * cmdUpdateUI

showItemsCli :: MonadClientUI m
             => Msg -> ItemBag -> ItemInv -> m ()
showItemsCli msg bag inv = do
  io <- itemOverlay bag inv
  slides <- overlayToSlideshow msg io
  void $ getManyConfirms [] slides

animateDeathCli :: MonadClientUI m => ActorId -> m ()
animateDeathCli target = do
  Kind.COps{coactor} <- getsState scops
  pbody <- getsState $ getActorBody target
  msgAdd $ makeSentence [MU.SubjectVerbSg (partActor coactor pbody) "die"]
  recordHistory  -- Prevent repeating the "die" msgs.
  cli <- getClient
  loc <- getState
  per <- askPerception
  let animFrs = animate cli loc per $ deathBody (bpos pbody)
  displayFramesPush animFrs

effectCli :: MonadClientUI m => Msg -> (Point, Point) -> Int -> Bool -> m ()
effectCli msg poss deltaHP block = do
  msgAdd msg
  cli <- getClient
  loc <- getState
  per <- askPerception
  -- Try to show an animation. Sometimes, e.g., when HP is unchaged,
  -- the animation will not be shown, but a single frame with @msg@ will.
  let anim | deltaHP > 0 =
        twirlSplash poss Color.BrBlue Color.Blue
           | deltaHP < 0 && block =
        blockHit    poss Color.BrRed  Color.Red
           | deltaHP < 0 && not block =
        twirlSplash poss Color.BrRed  Color.Red
           | otherwise = mempty
      animFrs = animate cli loc per anim
  displayFramesPush $ Nothing : animFrs

animateBlockCli :: MonadClientUI m => ActorId -> ActorId -> MU.Part -> m ()
animateBlockCli source target verb = do
  Kind.COps{coactor} <- getsState scops
  per <- askPerception
  smRaw <- getsState (getActorBody source)
  tmRaw <- getsState (getActorBody target)
  let spos = bpos smRaw
      tpos = bpos tmRaw
      svisible = spos `ES.member` totalVisible per
      tvisible = tpos `ES.member` totalVisible per
      sm | svisible  = smRaw
         | otherwise = smRaw {bname = Just "somebody"}
      tm | tvisible  = tmRaw
         | otherwise = tmRaw {bname = Just "somebody"}
      msgMiss = makeSentence
        [ MU.SubjectVerbSg (partActor coactor sm) "try to"
        , verb MU.:> ", but"
        , MU.SubjectVerbSg (partActor coactor tm) "block"
        ]
  msgAdd msgMiss
  cli <- getClient
  loc <- getState
  let poss = (tpos, spos)
      anim = blockMiss poss
      animFrs = animate cli loc per anim
  displayFramesPush $ Nothing : animFrs

displaceCli :: MonadClientUI m => ActorId -> ActorId -> m ()
displaceCli source target = do
  Kind.COps{coactor} <- getsState scops
  per <- askPerception
  sm <- getsState (getActorBody source)
  tm <- getsState (getActorBody target)
  let spos = bpos sm
      tpos = bpos tm
      msg = makeSentence
        [ MU.SubjectVerbSg (partActor coactor sm) "displace"
        , partActor coactor tm ]
  msgAdd msg
  cli <- getClient
  loc <- getState
  let poss = (tpos, spos)
      animFrs = animate cli loc per $ swapPlaces poss
  displayFramesPush $ Nothing : animFrs

-- * cmdQueryCli

handleAI :: MonadClient m => ActorId -> m CmdSer
handleAI actor = do
  body <- getsState $ getActorBody actor
  side <- getsClient sside
  assert (bfaction body == side `blame` (actor, bfaction body, side)) $ do
    Kind.COps{costrat=Kind.Ops{okind}} <- getsState scops
    leader <- getsClient sleader
    fact <- getsState $ (EM.! bfaction body) . sfaction
    let factionAI | Just actor /= leader = gAiMember fact
                  | otherwise = fromJust $ gAiLeader fact
        factionAbilities = sabilities (okind factionAI)
    stratTarget <- targetStrategy actor factionAbilities
    -- Choose a target from those proposed by AI for the actor.
    btarget <- rndToAction $ frequency $ bestVariant stratTarget
    modifyClient $ updateTarget actor (const btarget)
    stratAction <- actionStrategy actor factionAbilities
    let _debug = T.unpack
          $ "HandleAI abilities:" <+> showT factionAbilities
          <>          ", symbol:" <+> showT (bsymbol body)
          <>          ", loc:"    <+> showT (bpos body)
          <> "\nHandleAI target:" <+> showT stratTarget
          <> "\nHandleAI move:"   <+> showT stratAction
    -- trace _debug $ return ()
    -- Run the AI: chose an action from those given by the AI strategy.
    rndToAction $ frequency $ bestVariant $ stratAction

-- * cmdQueryUI

carryOnCli :: MonadClientUI m => m Bool
carryOnCli = do
  go <- displayMore ColorBW ""
  msgAdd "The survivors carry on."  -- TODO: reset messages at game over not to display it if there are no survivors.
  return go

-- | Handle the move of the hero.
handleHuman :: MonadClientUI m => m [CmdSer]
handleHuman = do
  -- When running, stop if aborted by a disturbance. Otherwise let
  -- the human player issue commands, until any of them takes time.
  -- First time, just after pushing frames, ask for commands in Push mode.
  Just leader <- getsClient sleader
  cmdS <- tryWith (\msg -> stopRunning >> humanCommand msg) $ do
    srunning <- getsClient srunning
    maybe abort (continueRun leader) srunning
--  addSmell leader  -- TODO: instead do for all non-spawning factions
  Just leaderNew <- getsClient sleader
  if leaderNew == leader then
    return [cmdS]
  else do
    fid <- getsClient sside
    return [LeaderSer fid leaderNew, cmdS]

-- | Continue running in the given direction.
continueRun :: MonadClient m => ActorId -> (Vector, Int) -> m CmdSer
continueRun leader dd = do
  (dir, distNew) <- continueRunDir leader dd
  modifyClient $ \cli -> cli {srunning = Just (dir, distNew)}
  -- Attacks and opening doors disallowed when continuing to run.
  return $ RunSer leader dir

-- | Determine and process the next human player command. The argument is
-- the last abort message due to running, if any.
humanCommand :: forall m. MonadClientUI m
             => Msg
             -> m CmdSer
humanCommand msgRunAbort = do
  -- The frame state is now Push.
  kmPush <- case msgRunAbort of
    "" -> getKeyCommand (Just True)
    _  -> do
      slides <- promptToSlideshow msgRunAbort
      getKeyOverlayCommand $ head $ runSlideshow slides
  -- The frame state is now None and remains so between each pair
  -- of lines of @loop@ (but can change within called actions).
  let loop :: K.KM -> m CmdSer
      loop km = do
        -- Messages shown, so update history and reset current report.
        recordHistory
        -- On abort, just reset state and call loop again below.
        -- Each abort that gets this far generates a slide to be shown.
        (mcmdS, slides) <- runWriterT $ tryWithSlide (return Nothing) $ do
          -- Look up the key.
          Binding{kcmd} <- askBinding
          case M.lookup km kcmd of
            Just (_, _, cmd) -> do
              -- Query and clear the last command key.
              lastKey <- getsClient slastKey
              -- TODO: perhaps replace slastKey
              -- with test 'kmNext == km'
              -- or an extra arg to 'loop'.
              -- Depends on whether slastKey
              -- is needed in other parts of code.
              modifyClient (\st -> st {slastKey = Just km})
              if (Just km == lastKey)
                then cmdHumanSem Clear
                else cmdHumanSem cmd
            Nothing -> let msgKey = "unknown command <" <> K.showKM km <> ">"
                       in abortWith msgKey
        -- The command was aborted or successful and if the latter,
        -- possibly took some time.
        case mcmdS of
          Just cmdS -> assert (null (runSlideshow slides) `blame` slides) $ do
            -- Exit the loop and let other actors act. No next key needed
            -- and no slides could have been generated.
            modifyClient (\st -> st {slastKey = Nothing})
            return cmdS
          Nothing ->
            -- If no time taken, rinse and repeat.
            -- Analyse the obtained slides.
            case reverse (runSlideshow slides) of
              [] -> do
                -- Nothing special to be shown; by default draw current state.
                modifyClient (\st -> st {slastKey = Nothing})
                sli <- promptToSlideshow ""
                kmNext <- getKeyOverlayCommand $ head $ runSlideshow sli
                loop kmNext
              sLast : sls -> do
                -- Show, one by one, all but the last slide.
                -- Note: the code that generates the slides is responsible
                -- for inserting the @more@ prompt.
                b <- getManyConfirms [km] $ toSlideshow $ reverse sls
                -- Display the last slide while waiting for the next key,
                -- or display current state if slideshow interrupted.
                kmNext <- if b
                          then getKeyOverlayCommand sLast
                          else do
                            modifyClient (\st -> st {slastKey = Nothing})
                            sli <- promptToSlideshow ""
                            getKeyOverlayCommand $ head $ runSlideshow sli
                -- Look up and perform the next command.
                loop kmNext
  loop kmPush
