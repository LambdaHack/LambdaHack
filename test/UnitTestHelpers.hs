{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
-- | Monadic test harness and other stubs for unit tests.
module UnitTestHelpers
  ( CliState(..)
  , emptyCliState
  , executorCli
  , reportToTexts
  , stubLevel
  , stubState
  , stubCliState
  , stubItem
  , testActor
  , testActorId
  , testActorId2
  , testActorId3
  , testActorWithItem
  , testCliStateWithItem
  , testFactionId
  , testItemId
  , testLevel
  , testLevelId
  , partyCliState
  , partyCliState3
  , partyCliStateBanned
  , partyCliStateScripted
  , runParamsA
  , scriptedFchanFrontend
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , fchanFrontendStub
  , CliMock(..)
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Control.Monad.IO.Class as IO
import           Control.Monad.Trans.State.Strict
  (StateT (StateT, runStateT), gets, state)
import qualified Data.EnumMap.Strict as EM
import           Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as Text

import           Game.LambdaHack.Atomic (MonadStateWrite (..))
import           Game.LambdaHack.Client
import qualified Game.LambdaHack.Client.BfsM as BfsM
import           Game.LambdaHack.Client.HandleResponseM
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
  (StateClient (..), TGoal (..), Target (..), emptyStateClient, updateLeader)
import           Game.LambdaHack.Client.UI
  (MonadClientUI (..), SessionUI (..), emptySessionUI)
import           Game.LambdaHack.Client.UI.ActorUI (ActorUI (..))
import qualified Game.LambdaHack.Client.UI.Content.Input as IC
import           Game.LambdaHack.Client.UI.Content.Screen
  (emptyScreenContent, rheight, rwidth)
import           Game.LambdaHack.Client.UI.ContentClientUI
  (coinput, coscreen, emptyCCUI)
import           Game.LambdaHack.Client.UI.Frontend
  (ChanFrontend (..), FrontReq (..))
import           Game.LambdaHack.Client.UI.Key (KMP (..))
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.Overlay
import           Game.LambdaHack.Client.UI.PointUI (PointUI (..))
import           Game.LambdaHack.Client.UI.SessionUI (RunParams (..))
import           Game.LambdaHack.Client.UI.UIOptions (UIOptions (..))
import           Game.LambdaHack.Common.Actor
  (Actor (..), ResDelta (..), Watchfulness (..))
import           Game.LambdaHack.Common.Area (Area, toArea, trivialArea)
import           Game.LambdaHack.Common.ClientOptions
  (ClientOptions (..), FullscreenMode (..), defClientOptions)
import           Game.LambdaHack.Common.Faction (Faction (..))
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Kind (COps (..), emptyUIFaction)
import           Game.LambdaHack.Common.Level (Level (..))
import           Game.LambdaHack.Common.Misc (FontSet (..))
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Perception (emptyPer)
import           Game.LambdaHack.Common.Point (Point (..))
import           Game.LambdaHack.Common.State
  ( State
  , emptyState
  , unknownTileMap
  , updateActorD
  , updateActorMaxSkills
  , updateCOpsAndCachedData
  , updateDungeon
  , updateFactionD
  )
import           Game.LambdaHack.Common.Time (timeZero)
import           Game.LambdaHack.Common.Types
  (ActorId, FactionId, ItemId, LevelId)
import qualified Game.LambdaHack.Content.FactionKind as FK
import           Game.LambdaHack.Content.RuleKind (RuleContent (..))
import           Game.LambdaHack.Content.TileKind (unknownId)
import qualified Game.LambdaHack.Core.Dice as Dice
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Color (Color (..))
import           Game.LambdaHack.Definition.DefsInternal (toContentId)
import           Game.LambdaHack.Definition.Flavour

import qualified Client.UI.Content.Input as Content.Input

-- * UI frontend stub

-- Read UI requests from the client and send them to the frontend,
fchanFrontendStub :: ChanFrontend
fchanFrontendStub =
  ChanFrontend $ \case
    FrontFrame _ -> putStr "FrontFrame"
    FrontDelay _ -> putStr "FrontDelay"
    FrontKey _ _ -> return KMP {kmpKeyMod = K.escKM, kmpPointer = PointUI 0 0}
    FrontPressed -> return False
    FrontDiscardKey -> putStr "FrontDiscardKey"
    FrontResetKeys -> putStr "FrontResetKeys"
    FrontShutdown -> putStr "FrontShutdown"
    FrontPrintScreen -> putStr "FrontPrintScreen"

-- | A frontend stub that answers each 'FrontKey' request with the next
-- scripted key, and with ESC once the script runs dry (as
-- 'fchanFrontendStub' always does). Other requests are handled as in
-- 'fchanFrontendStub'.
scriptedFchanFrontend :: [K.KM] -> IO ChanFrontend
scriptedFchanFrontend kms = do
  keysRef <- newIORef kms
  return $ ChanFrontend $ \req -> case req of
    FrontKey _ _ -> do
      keys <- readIORef keysRef
      case keys of
        km : rest -> do
          writeIORef keysRef rest
          return KMP {kmpKeyMod = km, kmpPointer = PointUI 0 0}
        [] -> return KMP {kmpKeyMod = K.escKM, kmpPointer = PointUI 0 0}
    _ -> case fchanFrontendStub of ChanFrontend stub -> stub req

-- * Mock client state implementation

data CliState = CliState
  { cliState   :: State            -- ^ current global state
  , cliClient  :: StateClient      -- ^ current client state
  , cliSession :: Maybe SessionUI  -- ^ UI state, empty for AI clients

  -- Not needed for the mock monad (and blank line needed to avoid making this
  -- comment a haddock for @cliSession@ field):
  -- , cliDict    :: ChanServer
  -- , cliToSave  :: Save.ChanSave (StateClient, Maybe SessionUI)
  }

-- * Option stubs

stubUIOptions :: UIOptions
stubUIOptions = UIOptions
  { uCommands = []
  , uHeroNames = []
  , uVi = False
  , uLeftHand = False
  , uChosenFontset = ""
  , uAllFontsScale = 0.0
  , uFullscreenMode = NotFullscreen
  , uhpWarningPercent = 0
  , uMsgWrapColumn = 0
  , uHistoryMax = 0
  , uMaxFps = 0.0
  , uNoAnim = False
  , uOverrideCmdline = []
  , uFonts = []
  , uFontsets = []
  , uMessageColors = []
  }

stubClientOptions :: ClientOptions
stubClientOptions = defClientOptions
  { schosenFontset = Just "snoopy"
  , sfontsets =
      [("snoopy", FontSet { fontMapScalable = "scalable"
                          , fontMapBitmap = "bitmap"
                          , fontPropRegular = "propRegular"
                          , fontPropBold = "propBold"
                          , fontMono = "mono" })]
  -- Unit tests never want the real, platform-specific frontend (Sdl/Dom/
  -- Wasm): they already stand in for it via 'fchanFrontendStub' below.
  -- Reporting the null frontend here keeps Frontend.frontendName and
  -- Frontend.chanFrontendIO from ever calling into it -- good test
  -- isolation regardless of platform, and doubly so for wasm32-wasi:
  -- Frontend.Wasm uses GHC's JSFFI, and even just linking it into the
  -- test binary (let alone calling it) requires the JS glue that
  -- `make test-wasm` sets up via run-wasm-test.mjs.
  , sfrontendNull = True
  }

stubItem :: Item
stubItem = Item { jkind = IdentityObvious (toContentId 0), jfid = Nothing, jflavour = dummyFlavour }

testLevel :: Level
testLevel = Level
  { lkind = toEnum 0
  , ldepth = Dice.AbsDepth 1
  , lfloor = EM.empty
  , lembed = EM.empty
  , lbig = EM.empty
  , lproj = EM.empty
  , ltile = unknownTileMap (fromJust (toArea (0,0,0,0))) unknownId 10 10  --PointArray.empty
  , lentry = EM.empty
  , larea = trivialArea (Point 0 0)
  , lsmell = EM.empty
  , lstair = ([],[])
  , lescape = []
  , lseen = 0
  , lexpl = 0
  , ltime = timeZero
  , lnight = False
  }

-- * Stub identifiers

-- Using different arbitrary numbers for these so that if tests fail
-- due to missing keys we'll have more of a clue.
testLevelId :: LevelId
testLevelId = toEnum 111

testActorId :: ActorId
testActorId = toEnum 112

-- | A second party member, sharing a level with 'testActorId'.
testActorId2 :: ActorId
testActorId2 = toEnum 115

-- | A third party member, for wrong-pick (not just no-op) desync tests.
testActorId3 :: ActorId
testActorId3 = toEnum 116

testItemId :: ItemId
testItemId = toEnum 113

testFactionId :: FactionId
testFactionId = toEnum 114

-- * Game arena element stubs

testArea :: Area
testArea = fromJust(toArea (0, 0, 0, 0))

testLevelDimension :: Int
testLevelDimension = 3

stubLevel :: Level
stubLevel = Level
  { lkind = toEnum 0
  , ldepth = Dice.AbsDepth 1
  , lfloor = EM.empty
  , lembed = EM.empty
  , lbig = EM.empty
  , lproj = EM.empty
  , ltile = unknownTileMap testArea unknownId testLevelDimension testLevelDimension
  , lentry = EM.empty
  , larea = trivialArea (Point 0 0)
  , lsmell = EM.empty
  , lstair = ([],[])
  , lescape = []
  , lseen = 0
  , lexpl = 0
  , ltime = timeZero
  , lnight = False
  }
testFaction :: Faction
testFaction =
  Faction
    { gkind = emptyUIFaction
    , gname = ""
    , gcolor = Black
    , gdoctrine = Ability.TBlock
    , gunderAI = True
    , ginitial = []
    , gdipl = EM.empty
    , gquit = Nothing
    , _gleader = Nothing
    , gstash = Nothing
    , gvictims = EM.empty
    }

testActor :: Actor
testActor = Actor
  { btrunk = testItemId
  , bnumber = Nothing
  , bhp = 0
  , bhpDelta = ResDelta (0,0) (0,0)
  , bcalm = 0
  , bcalmDelta = ResDelta (0,0) (0,0)
  , bpos = Point 0 0
  , boldpos = Nothing
  , blid = testLevelId
  , bfid = testFactionId
  , btrajectory = Nothing
  , borgan = EM.empty
  , beqp = EM.empty
  , bweapon = 0
  , bweapBenign = 0
  , bwatch = WWatch
  , bproj = False
  }

testActorWithItem :: Actor
testActorWithItem =
  testActor { beqp = EM.singleton testItemId (1,[])}

-- Stublike state that should barely function for testing.
stubState :: State
stubState =
  let singletonFactionUpdate _ = EM.singleton testFactionId testFaction
      singletonDungeonUpdate _ = EM.singleton testLevelId stubLevel
      singletonActorDUpdate _ = EM.singleton testActorId testActor
      singletonActorMaxSkillsUpdate _ =
        EM.singleton testActorId Ability.zeroSkills
      copsUpdate oldCOps =
        oldCOps {corule = (corule oldCOps)
                   { rWidthMax = testLevelDimension
                   , rHeightMax = testLevelDimension }}
      stateWithMaxLevelDimension = updateCOpsAndCachedData copsUpdate emptyState
      stateWithFaction =
        updateFactionD singletonFactionUpdate stateWithMaxLevelDimension
      stateWithActorD = updateActorD singletonActorDUpdate stateWithFaction
      stateWithActorMaxSkills =
        updateActorMaxSkills singletonActorMaxSkillsUpdate stateWithActorD
      stateWithDungeon =
        updateDungeon singletonDungeonUpdate stateWithActorMaxSkills
  in stateWithDungeon

testStateWithItem :: State
testStateWithItem =
  let swapToItemActor _ = EM.singleton testActorId testActorWithItem
  in updateActorD swapToItemActor stubState

emptyCliState :: CliState
emptyCliState = CliState
  { cliState = emptyState
  , cliClient = emptyStateClient testFactionId
  , cliSession = Nothing
  }

stubSessionUI :: SessionUI
stubSessionUI =
  let actorUI = ActorUI { bsymbol = 'j'
                        , bname = "Jamie"
                        , bpronoun = "he/him"
                        , bcolor = BrCyan }
  in (emptySessionUI stubUIOptions)
    { sactorUI = EM.singleton testActorId actorUI
      -- The sample game's real key bindings, so tests can resolve real
      -- keys (dispatchCmd, the X2 bridge) with no hand-rolled content.
    , sccui = emptyCCUI
        { coinput = IC.makeData Nothing Content.Input.standardKeysAndMouse
        , coscreen = emptyScreenContent { rwidth = testLevelDimension
                                        , rheight = testLevelDimension + 3 } }
    , schanF = fchanFrontendStub
    }

stubCliState :: CliState
stubCliState = CliState
  { cliState = stubState
  , cliClient = (emptyStateClient testFactionId)
      { soptions = stubClientOptions
      , sfper = EM.singleton testLevelId emptyPer }
  , cliSession = let target = TPoint TUnknown testLevelId (Point 1 0)
                 in Just (stubSessionUI {sxhair = Just target})
  }

testCliStateWithItem :: CliState
testCliStateWithItem = stubCliState { cliState = testStateWithItem }

-- * Two-hero party state (for the leader-desync reproducer)

-- | A "runs as a group" faction, shaped like the sample game's Explorer
-- (hero) faction in the three properties the leader-desync bug depends on,
-- so that 'Game.LambdaHack.Common.Faction.noRunWithMulti' is 'False' and
-- multi-actor runs (which rotate the client pointman through party members
-- and, on interrupt, restore the run leader) are enabled:
--
-- * @fhasPointman = True@ (@emptyUIFaction@ defaults it to 'False', which
--   alone forces 'noRunWithMulti' 'True' via its third disjunct);
-- * @fskillsOther = meleeAdjacent@, whose @SkMove@ is @-10@ (< 0);
-- * @fspawnsFast = False@, so switching the pointman between levels is /not/
--   banned (see
--   'Game.LambdaHack.Common.Faction.bannedPointmanSwitchBetweenLevels').
partyFaction :: Faction
partyFaction = testFaction
  { gkind = emptyUIFaction { FK.fskillsOther = Ability.meleeAdjacent
                           , FK.fhasPointman = True }
  , gunderAI = False }

-- | A variant of 'partyFaction' whose kind spawns fast, so pointman switching
-- between levels is banned ('bannedPointmanSwitchBetweenLevels' is 'True').
-- Used to pin the banned-faction semantics of the cycling commands.
partyFactionBanned :: Faction
partyFactionBanned = testFaction
  { gkind = emptyUIFaction { FK.fskillsOther = Ability.meleeAdjacent
                           , FK.fhasPointman = True
                           , FK.fspawnsFast = True }
  , gunderAI = False }

-- | Live heroes of 'partyFaction' on 'testLevelId'. @A@ ('testActorId')
-- is the run's original leader; @C@ ('testActorId2') is the member a
-- multi-hero run has rotated the client pointman to; @B@ ('testActorId3')
-- sits between them in party order, for wrong-pick desync tests.
heroA, heroB, heroC :: Actor
heroA = testActor {bhp = 100, bpos = Point 1 1}
heroB = testActor {bhp = 100, bpos = Point 0 1}
heroC = testActor {bhp = 100, bpos = Point 2 1}

-- | UI presentations for the heroes. Distinct symbols keep the
-- 'Game.LambdaHack.Client.UI.ActorUI.keySelected' party ordering stable at
-- @[A, B, C]@ (@\'a\' < \'b\' < \'c\'@).
actorUIA, actorUIB, actorUIC :: ActorUI
actorUIA = ActorUI { bsymbol = 'a', bname = "Alpha"
                   , bpronoun = "he/him", bcolor = BrCyan }
actorUIB = ActorUI { bsymbol = 'b', bname = "Bruno"
                   , bpronoun = "he/him", bcolor = BrMagenta }
actorUIC = ActorUI { bsymbol = 'c', bname = "Cadet"
                   , bpronoun = "he/him", bcolor = BrGreen }

partyStateWith :: Faction -> [(ActorId, Actor)] -> State
partyStateWith fact actors =
  let factionUpdate _ = EM.singleton testFactionId fact
      actorDUpdate _ = EM.fromList actors
      actorMaxSkillsUpdate _ =
        EM.fromList $ map (\(aid, _) -> (aid, Ability.zeroSkills)) actors
      dungeonUpdate _ = EM.singleton testLevelId stubLevel
      copsUpdate oldCOps =
        oldCOps {corule = (corule oldCOps)
                   { rWidthMax = testLevelDimension
                   , rHeightMax = testLevelDimension }}
      s0 = updateCOpsAndCachedData copsUpdate emptyState
      s1 = updateFactionD factionUpdate s0
      s2 = updateActorD actorDUpdate s1
      s3 = updateActorMaxSkills actorMaxSkillsUpdate s2
  in updateDungeon dungeonUpdate s3

partyCliStateWith :: Faction -> [(ActorId, Actor, ActorUI)] -> CliState
partyCliStateWith fact actorTriples = CliState
  { cliState =
      partyStateWith fact $ map (\(aid, b, _) -> (aid, b)) actorTriples
  , cliClient = (emptyStateClient testFactionId)
      { soptions = stubClientOptions
      , sfper = EM.singleton testLevelId emptyPer }
  , cliSession = Just $ stubSessionUI
      { sactorUI =
          EM.fromList $ map (\(aid, _, bUI) -> (aid, bUI)) actorTriples }
  }

-- | A client state with a two-hero party (@A@, @C@) on a single level,
-- no leader set yet (tests set it via 'updateClientLeader'), suitable for
-- driving the pointman-cycling code the way keypresses do.
partyCliState :: CliState
partyCliState = partyCliStateWith partyFaction
  [(testActorId, heroA, actorUIA), (testActorId2, heroC, actorUIC)]

-- | A three-hero party (@A@, @B@, @C@), for desyncs that pick the wrong
-- member rather than no member.
partyCliState3 :: CliState
partyCliState3 = partyCliStateWith partyFaction
  [ (testActorId, heroA, actorUIA)
  , (testActorId3, heroB, actorUIB)
  , (testActorId2, heroC, actorUIC) ]

-- | A two-hero party of a faction banned from switching pointman
-- between levels.
partyCliStateBanned :: CliState
partyCliStateBanned = partyCliStateWith partyFactionBanned
  [(testActorId, heroA, actorUIA), (testActorId2, heroC, actorUIC)]

-- | A run led by hero A with the whole two-hero party as members,
-- as a multi-hero run start ('moveRunHuman') would set up. Also reused
-- with the three-hero party: 'restoreLeaderFromRun' reads only 'runLeader',
-- so the member list doesn't matter to the restore.
runParamsA :: RunParams
runParamsA = RunParams { runLeader = testActorId
                       , runMembers = [testActorId2, testActorId]
                       , runInitial = False
                       , runStopMsg = Nothing
                       , runWaiting = 0 }

-- | 'partyCliState' with 'scriptedFchanFrontend' replacing the stub
-- frontend: each 'FrontKey' request pops the next scripted key.
partyCliStateScripted :: [K.KM] -> IO CliState
partyCliStateScripted kms = do
  chanF <- scriptedFchanFrontend kms
  return $ partyCliState
    {cliSession = (\sess -> sess {schanF = chanF}) <$> cliSession partyCliState}


-- * Monad harness mock

-- | Client state transformation monad mock.
newtype CliMock a = CliMock
  { runCliMock :: StateT CliState IO a }
    -- we build off io so we can compile but we don't want to use it;
    -- TODO: let's try to get rid of the IO. I can't see any problem right now.
    -- We'd need to to define dummy liftIO in some monads, etc.
  deriving (Monad, Functor, Applicative)

instance MonadStateRead CliMock where
  {-# INLINE getsState #-}
  getsState f = CliMock $ gets $ f . cliState

instance MonadStateWrite CliMock where
  {-# INLINE modifyState #-}
  modifyState f = CliMock $ state $ \cliS ->
    let !newCliS = cliS {cliState = f $ cliState cliS}
    in ((), newCliS)
  {-# INLINE putState #-}
  putState newCliState = CliMock $ state $ \cliS ->
    let !newCliS = cliS {cliState = newCliState}
    in ((), newCliS)

instance MonadClientRead CliMock where
  {-# INLINE getsClient #-}
  getsClient f = CliMock $ gets $ f . cliClient
  liftIO = CliMock . IO.liftIO

instance MonadClient CliMock where
  {-# INLINE modifyClient #-}
  modifyClient f = CliMock $ state $ \cliS ->
    let !newCliS = cliS {cliClient = f $ cliClient cliS}
    in ((), newCliS)

instance MonadClientUI CliMock where
  {-# INLINE getsSession #-}
  getsSession f = CliMock $ gets $ f . fromJust . cliSession
  {-# INLINE modifySession #-}
  modifySession f = CliMock $ state $ \cliS ->
    let !newCliSession = f $ fromJust $ cliSession cliS
        !newCliS = cliS {cliSession = Just newCliSession}
    in ((), newCliS)
  updateClientLeader aid = do
    s <- getState
    modifyClient $ updateLeader aid s
  getCacheBfs = BfsM.getCacheBfs
  getCachePath = BfsM.getCachePath

instance MonadClientAtomic CliMock where
  {-# INLINE execUpdAtomic #-}
  execUpdAtomic _ = return ()  -- handleUpdAtomic, until needed, save resources
    -- Don't catch anything; assume exceptions impossible.
  {-# INLINE execPutState #-}
  execPutState = putState

executorCli :: CliMock a -> CliState -> IO (a, CliState)
executorCli = runStateT . runCliMock


-- | Transform 'Report' type to a list of 'Text'.
reportToTexts :: Report -> [Text.Text]
reportToTexts report = Text.pack . attrStringToString <$> renderReport False report
