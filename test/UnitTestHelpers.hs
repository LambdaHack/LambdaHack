{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
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
  , testActorWithItem
  , testCliStateWithItem
  , testFactionId
  , testItemId
  , testItemKind
  , testLevelId
-- #ifdef EXPOSE_INTERNAL
--     -- * Internal operations
  , CliMock(..)
  , CliState(..)
-- #endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Control.Monad.IO.Class as IO

import Control.Monad.Trans.State.Strict
    ( StateT(StateT, runStateT), gets, state, evalStateT )

import qualified Data.EnumMap.Strict as EM

import           Game.LambdaHack.Atomic (MonadStateWrite (..))
import           Game.LambdaHack.Client
import qualified Game.LambdaHack.Client.BfsM as BfsM
import           Game.LambdaHack.Client.HandleResponseM
import           Game.LambdaHack.Client.MonadClient
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI
import           Game.LambdaHack.Client.UI.ActorUI
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Client.UI.Content.Screen
import           Game.LambdaHack.Client.UI.Frontend
import           Game.LambdaHack.Client.UI.Key (KMP (..))
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.PointUI
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.UIOptions

import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.Area
import           Game.LambdaHack.Common.ClientOptions
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.MonadStateRead
import           Game.LambdaHack.Common.Perception
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types

import           Game.LambdaHack.Content.ItemKind
import           Game.LambdaHack.Content.RuleKind
import           Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Core.Dice as Dice

import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Color
import           Game.LambdaHack.Definition.Flavour

import Content.ModeKindPlayer
import Game.LambdaHack.Common.Misc


-- Read UI requests from the client and send them to the frontend,
fchanFrontendStub :: ChanFrontend
fchanFrontendStub =
  ChanFrontend $ \case
    FrontFrame _ -> print "FrontFrame"
    FrontDelay _ -> print "FrontDelay"
    FrontKey _ _ -> do return KMP { kmpKeyMod=K.escKM, kmpPointer=PointUI 0 0 }
    FrontPressed -> do return False
    FrontDiscardKey -> print "FrontDiscardKey"
    FrontResetKeys -> print "FrontResetKeys"
    FrontShutdown -> print "FrontShutdown"
    FrontPrintScreen -> print "FrontPrintScreen"

data CliState = CliState
  { cliState   :: State            -- ^ current global state
  , cliClient  :: StateClient      -- ^ current client state
  , cliSession :: Maybe SessionUI  -- ^ UI state, empty for AI clients
  -- , cliDict    :: ChanServer       -- ^ this client connection information
  -- , cliToSave  :: Save.ChanSave (StateClient, Maybe SessionUI)
  }


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
  , sfontsets = [("snoopy",FontSet {fontMapScalable="scalable",fontMapBitmap="bitmap",fontPropRegular="propRegular",fontPropBold="propBold",fontMono="mono"})]
  }

testLevelDimension :: Int
testLevelDimension = 3

-- using different arbitrary numbers for these so if tests fail to missing keys we'll have more of a clue
testLevelId :: LevelId 
testLevelId = toEnum 111

testActorId :: ActorId
testActorId = toEnum 112

testItemId :: ItemId 
testItemId = toEnum 113

testFactionId :: FactionId 
testFactionId = toEnum 114



testArea :: Area
testArea = fromJust(toArea (0, 0, 0, 0))

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

testActor :: Actor
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

testItemKind :: ItemKind 
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

testActorWithItem :: Actor
testActorWithItem = 
  testActor { beqp = EM.singleton testItemId (1,[])}

-- stublike state instance that should barely function for testing
stubState :: State
stubState = let singletonFactionUpdate _ = EM.singleton testFactionId testFaction
                singletonDungeonUpdate _ = EM.singleton testLevelId stubLevel
                singletonActorDUpdate _ = EM.singleton testActorId testActor
                singletonActorMaxSkillsUpdate _ = EM.singleton testActorId Ability.zeroSkills
                copsUpdate oldCOps = oldCOps{corule=((corule oldCOps){rWidthMax=testLevelDimension, rHeightMax=testLevelDimension})}
                stateWithMaxLevelDimension = updateCOpsAndCachedData copsUpdate emptyState
                stateWithFaction = updateFactionD singletonFactionUpdate stateWithMaxLevelDimension
                stateWithActorD = updateActorD singletonActorDUpdate stateWithFaction
                stateWithActorMaxSkills = updateActorMaxSkills singletonActorMaxSkillsUpdate stateWithActorD
                stateWithDungeon = updateDungeon singletonDungeonUpdate stateWithActorMaxSkills
            in stateWithDungeon

testStateWithItem :: State
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

stubSessionUI :: SessionUI
stubSessionUI = (emptySessionUI stubUIOptions) 
  { sactorUI = EM.singleton testActorId ActorUI { bsymbol='j', bname="Jamie", bpronoun="he/him", bcolor=BrCyan }
  , sccui = emptyCCUI { coscreen = ScreenContent { rwidth = 10 -- unit test expects rheight > 1
                             , rheight = 5  -- unit test expects rheight - 2 > 2
                             , rwebAddress = ""
                             , rintroScreen = ([], [])
                             , rapplyVerbMap = EM.empty
                             , rFontFiles = []
                             }}
  , schanF = fchanFrontendStub
  } 

stubCliState :: CliState
stubCliState = CliState
  { cliState = stubState
  , cliClient = (emptyStateClient testFactionId) { soptions = stubClientOptions, sfper = EM.singleton testLevelId emptyPer }
  , cliSession = Just (stubSessionUI {sxhair = Just (TPoint TUnknown testLevelId (Point 1 0))}) --(TVector Vector {vx=1, vy=0})}) -- (TNonEnemy (toEnum 1))})-- 
  }

testCliStateWithItem :: CliState
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
  liftIO = CliMock . IO.liftIO

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
  
  
