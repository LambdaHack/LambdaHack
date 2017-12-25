{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The client UI session state.
module Game.LambdaHack.Client.UI.SessionUI
  ( SessionUI(..), AimMode(..), RunParams(..), LastRecord(..), KeysHintMode(..)
  , sreport, emptySessionUI, toggleMarkVision, toggleMarkSmell, getActorUI
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M
import           Data.Time.Clock.POSIX

import           Game.LambdaHack.Client.UI.ActorUI
import           Game.LambdaHack.Client.UI.Frontend
import           Game.LambdaHack.Client.UI.ItemSlot
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.KeyBindings
import           Game.LambdaHack.Client.UI.Msg
import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Common.Actor
import           Game.LambdaHack.Common.Faction
import           Game.LambdaHack.Common.Item
import           Game.LambdaHack.Common.Level
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.Point
import           Game.LambdaHack.Common.Time
import           Game.LambdaHack.Common.Vector

-- | The information that is used across a client playing session,
-- including many consecutive games in a single session.
-- Some of it is saved, some is reset when a new playing session starts.
-- An important component is the frontend session.
data SessionUI = SessionUI
  { sxhair         :: Target             -- ^ the common xhair
  , sactorUI       :: ActorDictUI        -- ^ assigned actor UI presentations
  , sslots         :: ItemSlots          -- ^ map from slots to items
  , slastSlot      :: SlotChar           -- ^ last used slot
  , slastItemMove  :: Maybe (CStore, CStore)  -- ^ last item move stores
  , schanF         :: ChanFrontend       -- ^ connection with the frontend
  , sbinding       :: Binding            -- ^ binding of keys to commands
  , sUIOptions     :: UIOptions          -- ^ UI options as set by the player
  , saimMode       :: Maybe AimMode      -- ^ aiming mode
  , sxhairMoused   :: Bool               -- ^ last mouse aiming not vacuus
  , sitemSel       :: Maybe (CStore, ItemId)  -- ^ selected item, if any
  , sselected      :: ES.EnumSet ActorId
                                    -- ^ the set of currently selected actors
  , srunning       :: Maybe RunParams
                                    -- ^ parameters of the current run, if any
  , _sreport       :: Report        -- ^ current messages
  , shistory       :: History       -- ^ history of messages
  , spointer       :: Point         -- ^ mouse pointer position
  , slastRecord    :: LastRecord    -- ^ state of key sequence recording
  , slastPlay      :: [K.KM]        -- ^ state of key sequence playback
  , slastLost      :: ES.EnumSet ActorId
                                    -- ^ actors that just got out of sight
  , swaitTimes     :: Int           -- ^ player just waited this many times
  , smarkVision    :: Bool          -- ^ mark leader and party FOV
  , smarkSmell     :: Bool          -- ^ mark smell, if the leader can smell
  , smenuIxMap     :: M.Map String Int
                                    -- ^ indices of last used menu items
  , sdisplayNeeded :: Bool          -- ^ current level needs displaying
  , skeysHintMode  :: KeysHintMode  -- ^ how to show keys hints when no messages
  , sstart         :: POSIXTime     -- ^ this session start time
  , sgstart        :: POSIXTime     -- ^ this game start time
  , sallTime       :: Time          -- ^ clips from start of session to current game start
  , snframes       :: Int           -- ^ this game current frame count
  , sallNframes    :: Int           -- ^ frame count from start of session to current game start
  }

-- | Current aiming mode of a client.
newtype AimMode = AimMode { aimLevelId :: LevelId }
  deriving (Show, Eq, Binary)

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

-- | State of last recorded and currently being recorded key sequences.
data LastRecord = LastRecord
  { currentKeys  :: [K.KM]  -- ^ accumulated keys of the current command
  , previousKeys :: [K.KM]  -- ^ keys of the rest of the recorded command batch
  , freeSpace    :: Int     -- ^ space left for commands to record in this batch
  }

data KeysHintMode =
    KeysHintBlocked
  | KeysHintAbsent
  | KeysHintPresent
  deriving (Eq, Enum, Bounded)

sreport :: SessionUI -> Report
sreport = _sreport

emptySessionUI :: UIOptions -> SessionUI
emptySessionUI sUIOptions =
  SessionUI
    { sxhair = TVector $ Vector 0 0
    , sactorUI = EM.empty
    , sslots = ItemSlots $ EM.fromAscList
               $ zip [minBound..maxBound] (repeat EM.empty)
    , slastSlot = SlotChar 0 'Z'
    , slastItemMove = Nothing
    , schanF = ChanFrontend $ const $
        error $ "emptySessionUI: ChanFrontend" `showFailure` ()
    , sbinding = Binding M.empty [] M.empty
    , sUIOptions
    , saimMode = Nothing
    , sxhairMoused = True
    , sitemSel = Nothing
    , sselected = ES.empty
    , srunning = Nothing
    , _sreport = emptyReport
    , shistory = emptyHistory 0
    , spointer = originPoint
    , slastRecord = LastRecord [] [] 0
    , slastPlay = []
    , slastLost = ES.empty
    , swaitTimes = 0
    , smarkVision = False
    , smarkSmell = True
    , smenuIxMap = M.singleton "main" 2
    , sdisplayNeeded = False
    , skeysHintMode = KeysHintPresent
    , sstart = 0
    , sgstart = 0
    , sallTime = timeZero
    , snframes = 0
    , sallNframes = 0
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
    put sslots
    put slastSlot
    put sUIOptions
    put saimMode
    put sitemSel
    put sselected
    put srunning
    put _sreport
    put shistory
    put smarkVision
    put smarkSmell
    put sdisplayNeeded
  get = do
    sxhair <- get
    sactorUI <- get
    sslots <- get
    slastSlot <- get
    sUIOptions <- get  -- is overwritten ASAP, but useful for, e.g., crash debug
    saimMode <- get
    sitemSel <- get
    sselected <- get
    srunning <- get
    _sreport <- get
    shistory <- get
    smarkVision <- get
    smarkSmell <- get
    sdisplayNeeded <- get
    let slastItemMove = Nothing
        schanF = ChanFrontend $ const $
          error $ "Binary: ChanFrontend" `showFailure` ()
        sbinding = Binding M.empty [] M.empty
        sxhairMoused = True
        spointer = originPoint
        slastRecord = LastRecord [] [] 0
        slastPlay = []
        slastLost = ES.empty
        swaitTimes = 0
        smenuIxMap = M.singleton "main" 7
        skeysHintMode = KeysHintAbsent
        sstart = 0
        sgstart = 0
        sallTime = timeZero
        snframes = 0
        sallNframes = 0
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
