{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The implementation of our custom game client monads. Just as any other
-- component of the library, this implementation can be substituted.
module UnitTestHelpers
  ( emptyCliState
  , executorCli
  , stubLevel
  , stubState
  , stubCliState
  , testActor
  , testActorId
  , testItemId
  , testCliStateWithItem
  , testItemKind
-- #ifdef EXPOSE_INTERNAL
--     -- * Internal operations
  , CliMock(..)
  , CliState(..)
-- #endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Concurrent
import qualified Control.Monad.IO.Class as IO
--import qualified Control.Monad.IO.Class as IO


import Control.Monad.Trans.State.Strict
    ( StateT(StateT, runStateT), gets, state, evalStateT )

import qualified Data.EnumMap.Strict as EM
import qualified Data.Vector.Unboxed as U


import           Game.LambdaHack.Atomic (MonadStateWrite (..))
import           Game.LambdaHack.Client
import qualified Game.LambdaHack.Client.BfsM as BfsM
import           Game.LambdaHack.Client.HandleAtomicM
import           Game.LambdaHack.Client.HandleResponseM
import           Game.LambdaHack.Client.LoopM
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.UIOptions

import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.ActorState
import           Game.LambdaHack.Common.Area
import           Game.LambdaHack.Common.ClientOptions
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.PointArray as PointArray
import qualified Game.LambdaHack.Common.Save as Save
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Common.Vector

import           Game.LambdaHack.Content.ItemKind
import           Game.LambdaHack.Content.ModeKind
import           Game.LambdaHack.Content.RuleKind
import           Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Core.Dice as Dice

import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Color
import           Game.LambdaHack.Definition.DefsInternal
import           Game.LambdaHack.Definition.Flavour

import           Game.LambdaHack.Server (ChanServer (..))

import Content.ModeKindPlayer


-- just for test code
-- import           Game.LambdaHack.Client.UI.HandleHelperM
-- import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
-- import           Game.LambdaHack.Client.UI.HandleHumanLocalM
-- import Game.LambdaHack.Definition.DefsInternal ( toContentSymbol )



data CliState = CliState
  { cliState   :: State            -- ^ current global state
  , cliClient  :: StateClient      -- ^ current client state
  , cliSession :: Maybe SessionUI  -- ^ UI state, empty for AI clients
  -- , cliDict    :: ChanServer       -- ^ this client connection information
  -- , cliToSave  :: Save.ChanSave (StateClient, Maybe SessionUI)
  }


-- minimalPlayer :: Player
-- minimalPlayer = Player
--     { fname = ""
--     , fgroups = []
--     , fskillsOther = [] -- :: Ability.Skills
--     , fcanEscape = False
--     , fneverEmpty = False
--     , fhiCondPoly = []
--     , fhasGender = False
--     , fdoctrine = TExplore
--     , fleaderMode = Nothing
--     , fhasUI = False
--     , funderAI = False
--     }

--testRuleContent = emptyRuleContent { corule = emptyRuleContent { rXmax = 2, rYmax = 2 }}

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


testLevelDimension = 3

-- using different arbitrary numbers for these so if tests fail to missing keys we'll have more of a clue
testLevelId = toEnum 111
testActorId = toEnum 112
testItemId = toEnum 113

testFactionId = toEnum 114

testTileKind :: TileKind
testTileKind = TileKind
  { tsymbol  = '0'
  , tname    = "testtile"
  , tfreq    = [(GroupName "testgroup", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 0
  , tfeature = [Walkable, Clear]
  }


(Just testArea) = toArea (0, 0, 0, 0)

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
    { gname = ""
    , gcolor = Black
    , gplayer = playerAnimal
    , gteamCont = Nothing
    , ginitial = []
    , gdipl = EM.empty
    , gquit = Nothing
    , _gleader = Nothing
    , gstash = Nothing
    , gvictims = EM.empty
    , gvictimsD = EM.empty
    }

testActor = 
  Actor
  { btrunk = toEnum 0
  , bnumber = Nothing
  , bhp = 0
  , bhpDelta = ResDelta (0,0) (0,0)
  , bcalm = 0
  , bcalmDelta = ResDelta (0,0) (0,0)
  , bpos = Point 0 0
  , boldpos = Nothing
  , blid = toEnum 0
  , bfid = toEnum 0
  , btrajectory = Nothing
  , borgan = EM.empty
  , beqp = EM.empty
  , bweapon = 0
  , bweapBenign = 0
  , bwatch = WWatch
  , bproj = False
  }

testItemKind = ItemKind
  { isymbol  = 'x'
  , iname    = "12345678901234567890123"
  , ifreq    = [ (UNREPORTED_INVENTORY, 1) ]
  , iflavour = zipPlain [Green]
  , icount   = 1 + 1 `Dice.d` 2
  , irarity  = [(1, 50), (10, 1)]
  , iverbHit = "hit"
  , iweight  = 300
  , idamage  = 1 `Dice.d` 1
  , iaspects = [ AddSkill Ability.SkHurtMelee $ -16 * 5
                , SetFlag Ability.Fragile
                , toVelocity 70 ]
  , ieffects = []
  , idesc    = "A really cool test item."
  , ikit     = []
  }

testActorWithItem = 
  testActor { beqp = EM.singleton testItemId (1,[])}

-- stublike state instance that should barely function for testing
stubState :: State
stubState = let singletonFactionUpdate _ = EM.singleton testFactionId testFaction
                singletonDungeonUpdate _ = EM.singleton testLevelId stubLevel
                singletonActorDUpdate _ = EM.singleton testActorId testActor
                singletonActorMaxSkillsUpdate _ = EM.singleton testActorId Ability.zeroSkills
                copsUpdate oldCOps = oldCOps{corule=((corule oldCOps){rXmax=testLevelDimension, rYmax=testLevelDimension})}
                stateWithMaxLevelDimension = updateCOpsAndCachedData copsUpdate emptyState
                stateWithFaction = updateFactionD singletonFactionUpdate stateWithMaxLevelDimension
                stateWithActorD = updateActorD singletonActorDUpdate stateWithFaction
                stateWithActorMaxSkills = updateActorMaxSkills singletonActorMaxSkillsUpdate stateWithActorD
                stateWithDungeon = updateDungeon singletonDungeonUpdate stateWithActorMaxSkills
            in stateWithDungeon

testStateWithItem = let swapToItemActor _ = EM.singleton testActorId testActorWithItem
                     in updateActorD swapToItemActor stubState

emptyCliState :: CliState
emptyCliState = CliState
  { cliState = emptyState 
  , cliClient = emptyStateClient $ toEnum 0
  , cliSession = Nothing
  -- , cliDict = undefined 
  -- , cliToSave = undefined 
  }  

stubCliState = CliState
  { cliState = stubState
  , cliClient = emptyStateClient $ toEnum 0
  , cliSession = Just ((emptySessionUI stubUIOptions) {sxhair = Just (TPoint TUnknown (toEnum 0) (Point 1 0))}) --(TVector Vector {vx=1, vy=0})}) -- (TNonEnemy (toEnum 1))})-- 
  }

testCliStateWithItem = stubCliState { cliState = testStateWithItem }

-- | Client state mock transformation monad.
newtype CliMock a = CliMock
  { runCliMock :: StateT CliState IO a }  -- we build off io so we can compile but we don't want to use it
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
  liftIO = return undefined --CliMock . IO.liftIO

instance MonadClient CliMock where
  {-# INLINE modifyClient #-}
  modifyClient f = CliMock $ state $ \cliS ->
    let !newCliS = cliS {cliClient = f $ cliClient cliS}
    in ((), newCliS)

-- instance MonadClientSetup CliMock where
--   saveClient = CliMock $ do
--     --toSave <- gets cliToSave
--     cli <- gets cliClient
--     msess <- gets cliSession
--     IO.liftIO $ Save.saveToChan toSave (cli, msess)

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

-- instance MonadClientReadResponse CliMock where
--   receiveResponse = CliMock $ do
--     ChanServer{responseS} <- gets cliDict
--     IO.liftIO $ takeMVar responseS

-- instance MonadClientWriteRequest CliMock where
--   sendRequestAI scmd = CliMock $ do
--     ChanServer{requestAIS} <- gets cliDict
--     IO.liftIO $ putMVar requestAIS scmd
--   sendRequestUI scmd = CliMock $ do
--     ChanServer{requestUIS} <- gets cliDict
--     IO.liftIO $ putMVar (fromJust requestUIS) scmd
--   clientHasUI = CliMock $ do
--     mSession <- gets cliSession
--     return $! isJust mSession

instance MonadClientAtomic CliMock where
  {-# INLINE execUpdAtomic #-}
  execUpdAtomic _ = return ()  -- handleUpdAtomic, until needed, save resources
    -- Don't catch anything; assume exceptions impossible.
  {-# INLINE execPutState #-}
  execPutState = putState


-- the compiler is recommending I ditch the testCliState redundancy ... it feels less readable to me that way though ... but maybe that's because I suck at haskell
executorCli :: CliMock a -> CliState -> IO (a, CliState)
executorCli testFn testCliState = 
  runStateT (runCliMock testFn) testCliState 
  
  
