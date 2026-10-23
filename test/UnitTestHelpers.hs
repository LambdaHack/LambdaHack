{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
-- | Monadic test harness and other stubs for unit tests.
module UnitTestHelpers
  ( CliState(..)
  , emptyCliState
  , executorCli
  , reportToTexts
  , stubLevel
  , walkableLevel
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
  , testItemId2
  , testLevel
  , testLevelId
  , heroA
  , heroB
  , heroC
  , freeFloorPos
  , partyCliState
  , partyCliState3
  , partyCliStateBanned
  , partyCliStateWalkable
  , partyCliStateScripted
  , scriptedCliState
  , enlargeScreen
  , withEqpItems
  , withSkills
  , withPositions
  , projectSkills
  , runParamsA
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
import qualified Data.EnumSet as ES
import           Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as Text

import           Game.LambdaHack.Atomic (MonadStateWrite (..))
import           Game.LambdaHack.Client
import qualified Game.LambdaHack.Client.BfsM as BfsM
import           Game.LambdaHack.Client.CommonM (createSalter)
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
import           Game.LambdaHack.Client.UI.SessionUI
  (ItemRoles (..), RunParams (..))
import           Game.LambdaHack.Client.UI.UIOptions (UIOptions (..))
import           Game.LambdaHack.Common.Actor
  (Actor (..), ResDelta (..), Watchfulness (..))
import           Game.LambdaHack.Common.ActorState (getActorBody)
import           Game.LambdaHack.Common.Area
  (Area, punindex, toArea, trivialArea)
import           Game.LambdaHack.Common.ClientOptions
  (ClientOptions (..), FullscreenMode (..), defClientOptions)
import           Game.LambdaHack.Common.Faction (Faction (..))
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Kind
  (COps (..), emptyUIFaction, emptyUnknownTile)
import           Game.LambdaHack.Common.Level (Level (..), updateBigMap)
import           Game.LambdaHack.Common.Misc (FontSet (..))
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Perception (emptyPer)
import           Game.LambdaHack.Common.Point (Point (..), setSpeedupHackXSize)
import qualified Game.LambdaHack.Common.PointArray as PointArray
import           Game.LambdaHack.Common.State
  ( State
  , emptyState
  , scops
  , unknownTileMap
  , updateActorD
  , updateActorMaxSkills
  , updateCOpsAndCachedData
  , updateDungeon
  , updateFactionD
  , updateItemD
  )
import qualified Game.LambdaHack.Common.Tile as Tile
import           Game.LambdaHack.Common.Time (timeZero)
import           Game.LambdaHack.Common.Types
  (ActorId, FactionId, ItemId, LevelId)
import qualified Game.LambdaHack.Content.FactionKind as FK
import           Game.LambdaHack.Content.RuleKind (RuleContent (..))
import qualified Game.LambdaHack.Content.TileKind as TK
import qualified Game.LambdaHack.Core.Dice as Dice
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Color (Color (..))
import           Game.LambdaHack.Definition.Defs (ContentId, SLore (..), X, Y)
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
        [] -> stub req
    _ -> stub req
 where
  stub :: FrontReq a -> IO a
  stub req = case fchanFrontendStub of ChanFrontend f -> f req

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
  -- Unit tests stand in for the frontend with 'fchanFrontendStub' above,
  -- so they report the null one ("null test" to Frontend.frontendName).
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
  , ltile = unknownTileMap (fromJust (toArea (0,0,0,0))) TK.unknownId 10 10  --PointArray.empty
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

-- | A second item, for tests where two actors must hold *different*
-- items, so that a bag looked up for the wrong actor misses.
testItemId2 :: ItemId
testItemId2 = toEnum 117

testFactionId :: FactionId
testFactionId = toEnum 114

-- * Game arena element stubs

testArea :: Area
testArea = fromJust(toArea (0, 0, 0, 0))

testLevelDimension :: Int
testLevelDimension = 4

stubLevel :: Level
stubLevel = Level
  { lkind = toEnum 0
  , ldepth = Dice.AbsDepth 1
  , lfloor = EM.empty
  , lembed = EM.empty
  , lbig = EM.empty
  , lproj = EM.empty
  , ltile = unknownTileMap testArea TK.unknownId testLevelDimension testLevelDimension
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

-- | A walkable, transparent floor tile, modeled on the sample game's
-- @floorCorridor@. It's the second tile kind of the unit tests' content
-- ('floorTileId'); its @talter@ must differ from @1@, which validation
-- reserves for the unknown tile.
floorTile :: TK.TileKind
floorTile = TK.TileKind
  { tsymbol  = '.'
  , tname    = "floor"
  , tfreq    = []  -- in no group: the mandatory groups are singletons
                   -- and their single member is the unknown tile
  , tcolor   = BrWhite
  , tcolor2  = BrBlack
  , talter   = 0
  , tfeature = [TK.Walkable, TK.Clear]
  }

-- | An impenetrable wall, to fence 'walkableLevel' with. Unknown tiles
-- would not do: their @talter@ of @1@ is exactly the skill the BFS raises
-- itself to, in order to path through unexplored terrain.
wallTile :: TK.TileKind
wallTile = TK.TileKind
  { tsymbol  = '#'
  , tname    = "wall"
  , tfreq    = []
  , tcolor   = BrWhite
  , tcolor2  = BrBlack
  , talter   = maxBound
  , tfeature = []
  }

-- | The content identifiers of 'floorTile' and 'wallTile', in the order
-- 'stubCOpsUpdate' lists them.
floorTileId, wallTileId :: ContentId TK.TileKind
floorTileId = toContentId 1
wallTileId = toContentId 2

-- | The content of every stub state: the dimensions the stub boards use
-- and tile content holding all three stub tiles. The unknown tile has to
-- come first (validation) and the board that uses only it is unaffected
-- by the other two being defined, so one content set serves both boards
-- and keeps them comparable.
stubCOpsUpdate :: COps -> COps
stubCOpsUpdate oldCOps =
  let cotile = TK.makeData [emptyUnknownTile, floorTile, wallTile] [] []
  in oldCOps { corule = (corule oldCOps) { rWidthMax = testLevelDimension
                                         , rHeightMax = testLevelDimension }
             , cotile
             , coTileSpeedup = Tile.speedupTile False cotile }
                 -- rebuilt in the same update, because
                 -- @updateCOpsAndCachedData@ recomputes only the actor
                 -- skills cache

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

-- | An empty state with the stub content, built once for every stub state.
stubCOpsState :: State
stubCOpsState = updateCOpsAndCachedData stubCOpsUpdate emptyState

-- | A stub state of one level and one faction, holding the given actors,
-- none of them skilled, each non-projectile one standing in the level's
-- 'lbig' at its position, where aiming and moving look for it.
stateWith :: Level -> Faction -> [(ActorId, Actor)] -> State
stateWith lvl fact actors =
  let factionUpdate _ = EM.singleton testFactionId fact
      actorDUpdate _ = EM.fromList actors
      actorMaxSkillsUpdate _ =
        EM.fromList $ map (\(aid, _) -> (aid, Ability.zeroSkills)) actors
      bigs = [(bpos b, aid) | (aid, b) <- actors, not (bproj b)]
      big = EM.fromList bigs
      dungeonUpdate _ = assert (EM.size big == length bigs)
                        $ EM.singleton testLevelId lvl {lbig = big}
      s1 = updateFactionD factionUpdate stubCOpsState
      s2 = updateActorD actorDUpdate s1
      s3 = updateActorMaxSkills actorMaxSkillsUpdate s2
  in updateDungeon dungeonUpdate s3

-- Stublike state that should barely function for testing.
stubState :: State
stubState = stateWith stubLevel testFaction [(testActorId, testActor)]

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

-- | The stub faction's client state over the given state.
stubClient :: State -> StateClient
stubClient s = (emptyStateClient testFactionId)
  { soptions = stubClientOptions
  , sfper = EM.singleton testLevelId emptyPer
  , salter = createSalter s }
    -- the client's cached per-level alter-skill map; the BFS indexes it by
    -- level id, so without it any frame drawn while an xhair is set (the
    -- path to the xhair is a BFS query) dies on a missing key

stubCliState :: CliState
stubCliState = CliState
  { cliState = stubState
  , cliClient = stubClient stubState
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
partyFactionBanned =
  partyFaction {gkind = (gkind partyFaction) {FK.fspawnsFast = True}}

-- | Live heroes of 'partyFaction' on 'testLevelId'. @A@ ('testActorId')
-- is the run's original leader; @C@ ('testActorId2') is the member a
-- multi-hero run has rotated the client pointman to; @B@ ('testActorId3')
-- sits between them in party order, for wrong-pick desync tests.
heroA, heroB, heroC :: Actor
heroA = testActor {bhp = 100, bpos = Point 1 1}
heroB = testActor {bhp = 100, bpos = Point 0 1}
heroC = testActor {bhp = 100, bpos = Point 2 1}

-- | The square below @A@, floor on 'walkableLevel' and stood on by
-- no actor: what a test aims at there, an actor's own square blocking
-- the aim.
freeFloorPos :: Point
freeFloorPos = Point 1 2

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

-- | A walkable stub board, for tests whose aiming or altering pipeline has
-- to get past the terrain: the heroes' two positions and 'freeFloorPos'
-- are floor, the rest of the board, around them and between them and its
-- edge, is impenetrable wall.
--
-- The fence is not decoration. The BFS -- which any frame drawn with an
-- xhair set queries, for the path to the xhair -- walks tile
-- neighbourhoods without bounds checks, as every real level is ringed by
-- tiles it cannot path through; a board that lets it reach the array's
-- edge crashes on an out-of-bounds index instead. Unknown space would not
-- fence it either (see 'wallTile'). That is why
-- 'stubLevel' -- unfenced unknown space -- is no use for such tests.
--
-- The array is unfolded in index order, each index read by 'punindex' at
-- the board's width, rather than updated by 'PointArray.//', whose
-- 'fromEnum' reads the width 'executorCli' sets: a test may build its
-- fixture before 'executorCli' runs.
walkableLevel :: Level
walkableLevel = stubLevel
  { ltile = PointArray.unfoldrNA testLevelDimension testLevelDimension
                                 (\i -> (tileAt i, i + 1)) 0 }
 where
  tileAt i = if punindex testLevelDimension i
                `elem` [bpos heroA, bpos heroC, freeFloorPos]
             then floorTileId
             else wallTileId

partyCliStateWith :: Level -> Faction -> [(ActorId, Actor, ActorUI)] -> CliState
partyCliStateWith lvl fact actorTriples =
  let s = stateWith lvl fact $ map (\(aid, b, _) -> (aid, b)) actorTriples
  in CliState
    { cliState = s
    , cliClient = stubClient s
    , cliSession = Just $ stubSessionUI
        { sactorUI =
            EM.fromList $ map (\(aid, _, bUI) -> (aid, bUI)) actorTriples }
    }

-- | Heroes @A@ and @C@, the party of every two-hero fixture.
twoHeroes :: [(ActorId, Actor, ActorUI)]
twoHeroes = [(testActorId, heroA, actorUIA), (testActorId2, heroC, actorUIC)]

-- | A client state with a two-hero party (@A@, @C@) on a single level,
-- no leader set yet (tests set it via 'updateClientLeader'), suitable for
-- driving the pointman-cycling code the way keypresses do.
partyCliState :: CliState
partyCliState = partyCliStateWith stubLevel partyFaction twoHeroes

-- | A three-hero party (@A@, @B@, @C@), for desyncs that pick the wrong
-- member rather than no member.
partyCliState3 :: CliState
partyCliState3 = partyCliStateWith stubLevel partyFaction
  [ (testActorId, heroA, actorUIA)
  , (testActorId3, heroB, actorUIB)
  , (testActorId2, heroC, actorUIC) ]

-- | A two-hero party of a faction banned from switching pointman
-- between levels.
partyCliStateBanned :: CliState
partyCliStateBanned = partyCliStateWith stubLevel partyFactionBanned twoHeroes

-- | 'partyCliState' on the 'walkableLevel': the same two heroes in the
-- same places, with the terrain under them and at 'freeFloorPos' walkable,
-- so that aiming from @A@ at that square succeeds.
partyCliStateWalkable :: CliState
partyCliStateWalkable = partyCliStateWith walkableLevel partyFaction twoHeroes

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

-- | A fixture with 'scriptedFchanFrontend' replacing the stub frontend:
-- each 'FrontKey' request pops the next scripted key.
scriptedCliState :: CliState -> [K.KM] -> IO CliState
scriptedCliState cliS kms = do
  chanF <- scriptedFchanFrontend kms
  return $ cliS
    {cliSession = (\sess -> sess {schanF = chanF}) <$> cliSession cliS}

-- | 'partyCliState' with the scripted frontend.
partyCliStateScripted :: [K.KM] -> IO CliState
partyCliStateScripted = scriptedCliState partyCliState

-- | Dialog prompts wrap via @indentSplitAttrString@, which asserts a
-- screen wider than 4, so tests that open real dialogs enlarge the stub
-- screen first. The level can stay 4x4: the map is drawn over
-- @rWidthMax@/@rHeightMax@, which the stub content keeps at 4.
enlargeScreen :: SessionUI -> SessionUI
enlargeScreen = resizeScreen 24 12

-- | A menu that displays an item splits the screen into a label pane and
-- a description pane, and each pane's width has to clear that same
-- assertion, so a dialog with items in it needs more room still.
enlargeScreenForItems :: SessionUI -> SessionUI
enlargeScreenForItems = resizeScreen 60 20

resizeScreen :: X -> Y -> SessionUI -> SessionUI
resizeScreen rwidth rheight sess = sess {sccui = (sccui sess)
  {coscreen = (coscreen (sccui sess)) {rwidth, rheight}}}

-- | Each listed actor's equipment holds just the listed item, registered
-- as 'stubItem', with what a dialog displaying it needs besides: the role
-- the store dialog lists an item by, without which the dialog reads the
-- store as empty however full the bag, and the screen of
-- 'enlargeScreenForItems'.
withEqpItems :: [(ActorId, ItemId)] -> CliState -> CliState
withEqpItems held cliS = cliS
  { cliState = updateItemD (EM.union $ EM.fromList $ map (, stubItem) iids)
               $ updateActorD (\actorD -> foldr give actorD held)
               $ cliState cliS
  , cliSession = enlargeScreenForItems . seeItems <$> cliSession cliS }
 where
  iids = map snd held
  give (aid, iid) = EM.adjust (\b -> b {beqp = EM.singleton iid (1, [])}) aid
  seeItems sess =
    let ItemRoles roles = sroles sess
    in sess {sroles = ItemRoles
                      $ EM.adjust (ES.union $ ES.fromList iids) SItem roles}

-- | The listed actors' skills, in place of the none every fixture gives.
withSkills :: [(ActorId, Ability.Skills)] -> CliState -> CliState
withSkills skills cliS = cliS {cliState =
  updateActorMaxSkills (EM.union $ EM.fromList skills) (cliState cliS)}

-- | The listed actors moved to the listed positions, in the level's
-- 'lbig' too.
withPositions :: [(ActorId, Point)] -> CliState -> CliState
withPositions moves cliS = cliS {cliState = foldr move (cliState cliS) moves}
 where
  move (aid, pos) s =
    let b = getActorBody aid s
        -- Off its old square, unless another actor has moved there.
        unplace a = if a == aid then Nothing else Just a
    in updateDungeon (EM.adjust (updateBigMap $ EM.insert pos aid
                                              . EM.update unplace (bpos b))
                                (blid b))
       $ updateActorD (EM.adjust (\b2 -> b2 {bpos = pos}) aid) s

-- | The skill to fling, and no other.
projectSkills :: Ability.Skills
projectSkills = Ability.addSk Ability.SkProject 1 Ability.zeroSkills


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

-- | Run a test under the mock as the game starts a client. The width the
-- 'Point' 'Enum' instance reads is set to the stub content's, every fixture's,
-- as @TieKnot@ sets it to the game's, so that a level's rows lie that far
-- apart whatever ran before. And the BFS scratch arrays are filled in, as the
-- client fills them, any frame drawn while an xhair is set needing them, the
-- path to the xhair being a BFS query.
executorCli :: CliMock a -> CliState -> IO (a, CliState)
executorCli m cliS = do
  setSpeedupHackXSize testLevelDimension
  flip runStateT cliS $ runCliMock $ do
    width <- getsState $ rWidthMax . corule . scops
    let !_A = assert (width == testLevelDimension) ()
    initTabsBFS
    m


-- | Transform 'Report' type to a list of 'Text'.
reportToTexts :: Report -> [Text.Text]
reportToTexts report = Text.pack . attrStringToString <$> renderReport False report
