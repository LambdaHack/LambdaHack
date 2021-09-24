{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- | The client UI session state.
module Game.LambdaHack.Client.UI.SessionUI
  ( SessionUI(..), ReqDelayed(..), ItemDictUI, AimMode(..), KeyMacro(..)
  , KeyMacroFrame(..), RunParams(..), ChosenLore(..)
  , emptySessionUI, emptyMacroFrame
  , toggleMarkVision, toggleMarkSmell, cycleOverrideTut, getActorUI
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Time.Clock.POSIX
import           GHC.Generics (Generic)
import qualified System.Random.SplitMix32 as SM

import           Game.LambdaHack.Client.Request
import           Game.LambdaHack.Client.State
import           Game.LambdaHack.Client.UI.ActorUI
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Client.UI.EffectDescription (DetailLevel (..))
import           Game.LambdaHack.Client.UI.Frontend
import           Game.LambdaHack.Client.UI.ItemSlot
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.PointUI
import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Types
import           Game.LambdaHack.Definition.Defs

-- | The information that is used across a client playing session,
-- including many consecutive games in a single session.
-- Some of it is saved, some is reset when a new playing session starts.
-- An important component is the frontend session.
data SessionUI = SessionUI
  { sreqPending    :: Maybe RequestUI
                                    -- ^ request created by a UI query
                                    --   but not yet sent to the server
  , sreqDelayed    :: ReqDelayed    -- ^ server delayed sending query to client
                                    --   or receiving request from client
  , sreqQueried    :: Bool          -- ^ player is now queried for a command
  , sxhair         :: Maybe Target  -- ^ the common xhair
  , sxhairGoTo     :: Maybe Target  -- ^ xhair set for last GoTo
  , sactorUI       :: ActorDictUI   -- ^ assigned actor UI presentations
  , sitemUI        :: ItemDictUI    -- ^ assigned item first seen level
  , sslots         :: ItemSlots     -- ^ map from slots to items
  , slastItemMove  :: Maybe (CStore, CStore)
                                    -- ^ last item move stores
  , schanF         :: ChanFrontend  -- ^ connection with the frontend
  , sccui          :: CCUI          -- ^ UI client content
  , sUIOptions     :: UIOptions     -- ^ UI options as set by the player
  , saimMode       :: Maybe AimMode -- ^ aiming mode
  , sxhairMoused   :: Bool          -- ^ last mouse aiming not vacuus
  , sitemSel       :: Maybe (ItemId, CStore, Bool)
                                    -- ^ selected item, if any, it's store and
                                    --   whether to override suitability check
  , sselected      :: ES.EnumSet ActorId
                                    -- ^ the set of currently selected actors
  , srunning       :: Maybe RunParams
                                    -- ^ parameters of the current run, if any
  , shistory       :: History       -- ^ history of messages
  , spointer       :: PointUI       -- ^ mouse pointer position
  , smacroFrame    :: KeyMacroFrame -- ^ the head of the key macro stack
  , smacroStack    :: [KeyMacroFrame]
                                    -- ^ the tail of the key macro stack
  , slastLost      :: ES.EnumSet ActorId
                                    -- ^ actors that just got out of sight
  , swaitTimes     :: Int           -- ^ player just waited this many times
  , swasAutomated  :: Bool          -- ^ the player just exited AI automation
  , smarkVision    :: Bool          -- ^ mark leader and party FOV
  , smarkSmell     :: Bool          -- ^ mark smell, if the leader can smell
  , snxtScenario   :: Int           -- ^ next game scenario number
  , scurTutorial   :: Bool          -- ^ whether current game is a tutorial
  , snxtTutorial   :: Bool          -- ^ whether next game is to be tutorial
  , soverrideTut   :: Maybe Bool    -- ^ override display of tutorial hints
  , susedHints     :: S.Set Msg     -- ^ tutorial hints already shown this game
  , smenuIxMap     :: M.Map String Int
                                    -- ^ indices of last used menu items
  , schosenLore    :: ChosenLore    -- ^ last lore chosen to display
  , sdisplayNeeded :: Bool          -- ^ current level needs displaying
  , sturnDisplayed :: Bool          -- ^ a frame was already displayed this turn
  , sreportNull    :: Bool          -- ^ whether no visible report created
                                    --   last UI faction turn or the report
                                    --   wiped out from screen since
  , sstart         :: POSIXTime     -- ^ this session start time
  , sgstart        :: POSIXTime     -- ^ this game start time
  , sallTime       :: Time          -- ^ clips from start of session
                                    --   to current game start
  , snframes       :: Int           -- ^ this game current frame count
  , sallNframes    :: Int           -- ^ frame count from start of session
                                    --   to current game start
  , srandomUI      :: SM.SMGen      -- ^ current random generator for UI
  }

data ReqDelayed = ReqDelayedNot | ReqDelayedHandled | ReqDelayedAlarm
  deriving Eq

-- | Local macro buffer frame. Predefined macros have their own in-game macro
-- buffer, allowing them to record in-game macro, queue actions and repeat
-- the last macro's action.
-- Running predefined macro pushes new @KeyMacroFrame@ onto the stack. We pop
-- buffers from the stack if locally there are no actions pending to be handled.
data KeyMacroFrame = KeyMacroFrame
  { keyMacroBuffer :: Either [K.KM] KeyMacro -- ^ record keystrokes in Left;
                                             --   repeat from Right
  , keyPending     :: KeyMacro               -- ^ actions pending to be handled
  , keyLast        :: Maybe K.KM             -- ^ last pressed key
  } deriving Show

-- This can stay a map forever, not a vector, because it's added to often,
-- but never read from, except when the user requests item details.
type ItemDictUI = EM.EnumMap ItemId LevelId

-- | Current aiming mode of a client.
data AimMode = AimMode
  { aimLevelId  :: LevelId
  , detailLevel :: DetailLevel
  }
  deriving (Show, Eq, Generic)

instance Binary AimMode

-- | In-game macros. We record menu navigation keystrokes and keystrokes
-- bound to commands with one exception --- we exclude keys that invoke
-- the @Record@ command, to avoid surprises.
-- Keys are kept in the same order in which they're meant to be replayed,
-- i.e. the first element of the list is replayed also as the first one.
newtype KeyMacro = KeyMacro {unKeyMacro :: [K.KM]}
  deriving (Show, Eq, Binary, Semigroup, Monoid)

-- | Parameters of the current run.
data RunParams = RunParams
  { runLeader  :: ActorId         -- ^ the original leader from run start
  , runMembers :: [ActorId]       -- ^ the list of actors that take part
  , runInitial :: Bool            -- ^ initial run continuation by any
                                  --   run participant, including run leader
  , runStopMsg :: Maybe Text      -- ^ message with the next stop reason
  , runWaiting :: Int             -- ^ waiting for others to move out of the way
  }
  deriving Show

-- | Last lore being aimed at.
data ChosenLore =
    ChosenLore [(ActorId, Actor)] [(ItemId, ItemQuant)]
  | ChosenNothing

emptySessionUI :: UIOptions -> SessionUI
emptySessionUI sUIOptions =
  SessionUI
    { sreqPending = Nothing
    , sreqDelayed = ReqDelayedNot
    , sreqQueried = False
    , sxhair = Nothing
    , sxhairGoTo = Nothing
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
    , spointer = PointUI 0 0
    , smacroFrame = emptyMacroFrame
    , smacroStack = []
    , slastLost = ES.empty
    , swaitTimes = 0
    , swasAutomated = False
    , smarkVision = False
    , smarkSmell = True
    , snxtScenario = 0
    , scurTutorial = False
    , snxtTutorial = True  -- matches @snxtScenario = 0@
    , soverrideTut = Nothing
    , susedHints = S.empty
    , smenuIxMap = M.empty
    , schosenLore = ChosenNothing
    , sdisplayNeeded = False
    , sturnDisplayed = False
    , sreportNull = True
    , sstart = 0
    , sgstart = 0
    , sallTime = timeZero
    , snframes = 0
    , sallNframes = 0
    , srandomUI = SM.mkSMGen 0
    }

emptyMacroFrame :: KeyMacroFrame
emptyMacroFrame = KeyMacroFrame (Right mempty) mempty Nothing

toggleMarkVision :: SessionUI -> SessionUI
toggleMarkVision sess = sess {smarkVision = not (smarkVision sess)}

toggleMarkSmell :: SessionUI -> SessionUI
toggleMarkSmell sess = sess {smarkSmell = not (smarkSmell sess)}

cycleOverrideTut :: SessionUI -> SessionUI
cycleOverrideTut sess = sess {soverrideTut = case soverrideTut sess of
                                Nothing -> Just False
                                Just False -> Just True
                                Just True -> Nothing}

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
    put $ archiveReport True shistory
      -- avoid displaying ending messages again at game start
    put smarkVision
    put smarkSmell
    put snxtScenario
    put scurTutorial
    put snxtTutorial
    put soverrideTut
    put susedHints
    put (show srandomUI)
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
    snxtScenario <- get
    scurTutorial <- get
    snxtTutorial <- get
    soverrideTut <- get
    susedHints <- get
    g <- get
    let sreqPending = Nothing
        sreqDelayed = ReqDelayedNot
        sreqQueried = False
        sxhairGoTo = Nothing
        slastItemMove = Nothing
        schanF = ChanFrontend $ const $
          error $ "Binary: ChanFrontend" `showFailure` ()
        sccui = emptyCCUI
        sxhairMoused = True
        spointer = PointUI 0 0
        smacroFrame = emptyMacroFrame
        smacroStack = []
        slastLost = ES.empty
        swaitTimes = 0
        swasAutomated = False
        smenuIxMap = M.empty
        schosenLore = ChosenNothing
        sdisplayNeeded = False  -- displayed regardless
        sturnDisplayed = False
        sreportNull = True
        sstart = 0
        sgstart = 0
        sallTime = timeZero
        snframes = 0
        sallNframes = 0
        srandomUI = read g
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
