{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The implementation of our custom game client monads. Just as any other
-- component of the library, this implementation can be substituted.
module MonadClientMock
  ( emptyCliState
  , executorCli
-- #ifdef EXPOSE_INTERNAL
--     -- * Internal operations
  , CliMock(..)
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


import           Game.LambdaHack.Atomic (MonadStateWrite (..))
import           Game.LambdaHack.Client
import qualified Game.LambdaHack.Client.BfsM as BfsM
import           Game.LambdaHack.Client.HandleAtomicM
import           Game.LambdaHack.Client.HandleResponseM
import           Game.LambdaHack.Client.LoopM
import           Game.LambdaHack.Client.MonadClient


import Game.LambdaHack.Client.State

import           Game.LambdaHack.Client.UI
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.UIOptions

import           Game.LambdaHack.Common.Actor
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


import           Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Core.Dice as Dice

import           Game.LambdaHack.Definition.Color
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

testLevel :: Level
testLevel = Level
  { lkind = toEnum 0
  , ldepth = Dice.AbsDepth 1
  , lfloor = EM.empty
  , lembed = EM.empty
  , lbig = EM.empty
  , lproj = EM.empty
  , ltile = PointArray.empty
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


-- stublike state instance that should barely function for testing
testState :: State
testState = let singletonFactionUpdate _ = EM.singleton (toEnum 0) testFaction
                singletonDungeonUpdate _ = EM.singleton (toEnum 0) testLevel
                singletonActorDUpdate _ = EM.singleton (toEnum 1) testActor
                stateWithFaction = updateFactionD singletonFactionUpdate emptyState
                stateWithActorD = updateActorD singletonActorDUpdate stateWithFaction
                stateWithDungeon = updateDungeon singletonDungeonUpdate stateWithActorD
             in stateWithDungeon

emptyCliState :: CliState
emptyCliState = CliState
  { cliState = emptyState 
  , cliClient = emptyStateClient $ toEnum 0
  , cliSession = Nothing
  -- , cliDict = undefined 
  -- , cliToSave = undefined 
  }  

testCliState = CliState
  { cliState = testState
  , cliClient = emptyStateClient $ toEnum 0
  , cliSession = Just ((emptySessionUI stubUIOptions) {sxhair = Just (TNonEnemy (toEnum 1))})-- Vector {vx=1, vy=0})})
  }

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


executorCli :: CliMock a -> IO (a, CliState)
executorCli testFn = 
  runStateT (runCliMock testFn) testCliState 
  
  
