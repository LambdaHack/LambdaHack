{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The client UI session state.
module Game.LambdaHack.Client.UI.SessionUI
  ( SessionUI(..), ItemDictUI, AimMode(..), RunParams(..)
  , HintMode(..), Macro(..)
  , emptySessionUI, toggleMarkVision, toggleMarkSmell, getActorUI
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M
import           Data.Time.Clock.POSIX

import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.ActorUI
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Client.UI.Frontend
import           Game.LambdaHack.Client.UI.ItemSlot
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Definition.Defs
import qualified System.Random.SplitMix32 as SM

-- | The information that is used across a client playing session,
-- including many consecutive games in a single session.
-- Some of it is saved, some is reset when a new playing session starts.
-- An important component is the frontend session.
data SessionUI = SessionUI
  { sxhair         :: Maybe Target       -- ^ the common xhair
  , sactorUI       :: ActorDictUI        -- ^ assigned actor UI presentations
  , sitemUI        :: ItemDictUI         -- ^ assigned item first seen level
  , sslots         :: ItemSlots          -- ^ map from slots to items
  , slastItemMove  :: Maybe (CStore, CStore)
                                         -- ^ last item move stores
  , schanF         :: ChanFrontend       -- ^ connection with the frontend
  , sccui          :: CCUI               -- ^ UI client content
  , sUIOptions     :: UIOptions          -- ^ UI options as set by the player
  , saimMode       :: Maybe AimMode      -- ^ aiming mode
  , sxhairMoused   :: Bool               -- ^ last mouse aiming not vacuus
  , sitemSel       :: Maybe (ItemId, CStore, Bool)
                                    -- ^ selected item, if any, it's store and
                                    --   whether to override suitability check
  , sselected      :: ES.EnumSet ActorId
                                    -- ^ the set of currently selected actors
  , srunning       :: Maybe RunParams
                                    -- ^ parameters of the current run, if any
  , shistory       :: History       -- ^ history of messages
  , spointer       :: K.PointUI     -- ^ mouse pointer position
  , slastAction    :: Maybe K.KM    -- ^ last pressed key
  , smacroBuffer   :: Either [K.KM] Macro
                                    -- ^ state of recording a macro; if Left,
                                    --   record keystrokes
  , slastPlay      :: Macro         -- ^ state of key sequence playback
  , slastLost      :: ES.EnumSet ActorId
                                    -- ^ actors that just got out of sight
  , swaitTimes     :: Int           -- ^ player just waited this many times
  , swasAutomated  :: Bool          -- ^ the player just exited AI automation
  , smarkVision    :: Bool          -- ^ mark leader and party FOV
  , smarkSmell     :: Bool          -- ^ mark smell, if the leader can smell
  , smenuIxMap     :: M.Map String Int
                                    -- ^ indices of last used menu items
  , sdisplayNeeded :: Bool          -- ^ current level needs displaying
  , shintMode      :: HintMode      -- ^ how to show keys hints when no messages
  , sreportNull    :: Bool          -- ^ whether no report created last UI turn
                                    --   or the report wiped out from screen
  , sstart         :: POSIXTime     -- ^ this session start time
  , sgstart        :: POSIXTime     -- ^ this game start time
  , sallTime       :: Time          -- ^ clips from start of session
                                    --   to current game start
  , snframes       :: Int           -- ^ this game current frame count
  , sallNframes    :: Int           -- ^ frame count from start of session
                                    --   to current game start
  , srandomUI      :: SM.SMGen      -- ^ current random generator for UI
  }

type ItemDictUI = EM.EnumMap ItemId LevelId

-- | Current aiming mode of a client.
newtype AimMode = AimMode { aimLevelId :: LevelId }
  deriving (Show, Eq, Binary)

-- | In-game macros. We record only the keystrokes that are bound to commands,
-- with one exception -- we exclude keys that invoke Record command.
-- Keys are kept in the same order in which they're meant to be replayed,
-- i.e. the first element of the list is replayed also as the first one.
newtype Macro = KeyMacro { unMacro :: [K.KM] }
  deriving (Eq, Binary, Semigroup, Monoid)

-- | Parameters of the current run.
data RunParams = RunParams
  { runLeader  :: ActorId         -- ^ the original leader from run start
  , runMembers :: [ActorId]       -- ^ the list of actors that take part
  , runInitial :: Bool            -- ^ initial run continuation by any
                                  --   run participant, including run leader
  , runStopMsg :: Maybe Text      -- ^ message with the next stop reason
  , runWaiting :: Int             -- ^ waiting for others to move out of the way
  }
  deriving (Show)

data HintMode =
    HintAbsent
  | HintShown
  | HintWiped
  deriving (Eq, Enum, Bounded)

emptySessionUI :: UIOptions -> SessionUI
emptySessionUI sUIOptions =
  SessionUI
    { sxhair = Nothing
    , sactorUI = EM.empty
    , sitemUI = EM.empty
    , sslots = ItemSlots $ EM.fromDistinctAscList
               $ zip [minBound..maxBound] (repeat EM.empty)
    , slastItemMove = Nothing
    , schanF = ChanFrontend $ const $
        error $ "emptySessionUI: ChanFrontend" `showFailure` ()
    , sccui = emptyCCUI
    , sUIOptions
    , saimMode = Nothing
    , sxhairMoused = True
    , sitemSel = Nothing
    , sselected = ES.empty
    , srunning = Nothing
    , shistory = emptyHistory 0
    , spointer = K.PointUI 0 0
    , slastAction = Nothing
    , smacroBuffer = Right mempty
    , slastPlay = mempty
    , slastLost = ES.empty
    , swaitTimes = 0
    , swasAutomated = False
    , smarkVision = False
    , smarkSmell = True
    , smenuIxMap = M.singleton "main" (2 - 9)  -- subtracting @initIx@
    , sdisplayNeeded = False
    , sreportNull = True
    , shintMode = HintAbsent
    , sstart = 0
    , sgstart = 0
    , sallTime = timeZero
    , snframes = 0
    , sallNframes = 0
    , srandomUI = SM.mkSMGen 0
    }

toggleMarkVision :: SessionUI -> SessionUI
toggleMarkVision s@SessionUI{smarkVision} = s {smarkVision = not smarkVision}

toggleMarkSmell :: SessionUI -> SessionUI
toggleMarkSmell s@SessionUI{smarkSmell} = s {smarkSmell = not smarkSmell}

getActorUI :: ActorId -> SessionUI -> ActorUI
getActorUI aid sess =
  EM.findWithDefault (error $ "" `showFailure` (aid, sactorUI sess)) aid
  $ sactorUI sess

instance Binary SessionUI where
  put SessionUI{..} = do
    put sxhair
    put sactorUI
    put sitemUI
    put sslots
    put sUIOptions
    put saimMode
    put sitemSel
    put sselected
    put srunning
    put shistory
    put smarkVision
    put smarkSmell
    put sdisplayNeeded
  get = do
    sxhair <- get
    sactorUI <- get
    sitemUI <- get
    sslots <- get
    sUIOptions <- get  -- is overwritten ASAP, but useful for, e.g., crash debug
    saimMode <- get
    sitemSel <- get
    sselected <- get
    srunning <- get
    shistory <- get
    smarkVision <- get
    smarkSmell <- get
    sdisplayNeeded <- get
    let slastItemMove = Nothing
        schanF = ChanFrontend $ const $
          error $ "Binary: ChanFrontend" `showFailure` ()
        sccui = emptyCCUI
        sxhairMoused = True
        spointer = K.PointUI 0 0
        slastAction = Nothing
        smacroBuffer = Right mempty
        slastPlay = mempty
        slastLost = ES.empty
        swaitTimes = 0
        swasAutomated = False
        smenuIxMap = M.singleton "main" (2 - 9)  -- subtracting @initIx@
        sreportNull = True
        shintMode = HintAbsent
        sstart = 0
        sgstart = 0
        sallTime = timeZero
        snframes = 0
        sallNframes = 0
        srandomUI = SM.mkSMGen 0
    return $! SessionUI{..}

instance Binary RunParams where
  put RunParams{..} = do
    put runLeader
    put runMembers
    put runInitial
    put runStopMsg
    put runWaiting
  get = do
    runLeader <- get
    runMembers <- get
    runInitial <- get
    runStopMsg <- get
    runWaiting <- get
    return $! RunParams{..}
