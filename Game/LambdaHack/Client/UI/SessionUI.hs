{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The client UI session state.
module Game.LambdaHack.Client.UI.SessionUI
  ( SessionUI(..), emptySessionUI
  , AimMode(..), RunParams(..), LastRecord, KeysHintMode(..)
  , toggleMarkVision, toggleMarkSmell
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Binary
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M
import Data.Time.Clock.POSIX

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.Frontend
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector

-- | The information that is used across a client playing session,
-- including many consecutive games in a single session.
-- Some of it is save, some is reset when a new playing session starts.
-- An important component is a frontend session.
data SessionUI = SessionUI
  { sxhair          :: !Target             -- ^ the common xhair
  , schanF          :: !ChanFrontend       -- ^ connection with the frontend
  , sbinding        :: !Binding            -- ^ binding of keys to commands
  , sconfig         :: !Config
  , saimMode        :: !(Maybe AimMode)    -- ^ aiming mode
  , sxhairMoused    :: !Bool               -- ^ last mouse aiming not vacuus
  , sitemSel        :: !(Maybe (CStore, ItemId))  -- ^ selected item, if any
  , sselected       :: !(ES.EnumSet ActorId)
                                      -- ^ the set of currently selected actors
  , srunning        :: !(Maybe RunParams)
                                      -- ^ parameters of the current run, if any
  , _sreport        :: !Report        -- ^ current messages
  , shistory        :: !History       -- ^ history of messages
  , spointer        :: !Point         -- ^ mouse pointer position
  , slastRecord     :: !LastRecord    -- ^ state of key sequence recording
  , slastPlay       :: ![K.KM]        -- ^ state of key sequence playback
  , slastLost       :: !(ES.EnumSet ActorId)
                                      -- ^ actors that just got out of sight
  , swaitTimes      :: !Int           -- ^ player just waited this many times
  , smarkVision     :: !Bool          -- ^ mark leader and party FOV
  , smarkSmell      :: !Bool          -- ^ mark smell, if the leader can smell
  , smenuIxMain     :: !Int           -- ^ index of last used Main Menu item
  , smenuIxSettings :: !Int           -- ^ index of last used Settings Menu item
  , smenuIxHelp     :: !Int           -- ^ index of last used Help Menu item
  , smenuIxHistory  :: !Int           -- ^ index of last used History Menu item
  , sdisplayNeeded  :: !Bool          -- ^ something to display on current level
  , skeysHintMode   :: !KeysHintMode  -- ^ how to show keys hints when no messages
  , sstart          :: !POSIXTime     -- ^ this session start time
  , sgstart         :: !POSIXTime     -- ^ this game start time
  , sallTime        :: !Time          -- ^ clips from start of session to current game start
  , snframes        :: !Int           -- ^ this game current frame count
  , sallNframes     :: !Int           -- ^ frame count from start of session to current game start
  }

-- | Current aiming mode of a client.
newtype AimMode = AimMode { aimLevelId :: LevelId }
  deriving (Show, Eq, Binary)

-- | Parameters of the current run.
data RunParams = RunParams
  { runLeader  :: !ActorId         -- ^ the original leader from run start
  , runMembers :: ![ActorId]       -- ^ the list of actors that take part
  , runInitial :: !Bool            -- ^ initial run continuation by any
                                   --   run participant, including run leader
  , runStopMsg :: !(Maybe Text)    -- ^ message with the next stop reason
  , runWaiting :: !Int             -- ^ waiting for others to move out of the way
  }
  deriving (Show)

type LastRecord = ( [K.KM]  -- accumulated keys of the current command
                  , [K.KM]  -- keys of the rest of the recorded command batch
                  , Int     -- commands left to record for this batch
                  )

data KeysHintMode =
    KeysHintBlocked
  | KeysHintAbsent
  | KeysHintPresent
  deriving (Eq, Enum, Bounded)

-- | Initial empty game client state.
emptySessionUI :: Config -> SessionUI
emptySessionUI sconfig =
  SessionUI
    { sxhair = TVector $ Vector 1 1
    , schanF = ChanFrontend $ const $ error "emptySessionUI: ChanFrontend "
    , sbinding = Binding M.empty [] M.empty
    , sconfig
    , saimMode = Nothing
    , sxhairMoused = True
    , sitemSel = Nothing
    , sselected = ES.empty
    , srunning = Nothing
    , _sreport = emptyReport
    , shistory = emptyHistory 0
    , spointer = originPoint
    , slastRecord = ([], [], 0)
    , slastPlay = []
    , slastLost = ES.empty
    , swaitTimes = 0
    , smarkVision = False
    , smarkSmell = True
    , smenuIxMain = 2
    , smenuIxSettings = 0
    , smenuIxHelp = 0
    , smenuIxHistory = 0
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

instance Binary SessionUI where
  put SessionUI{..} = do
    put sxhair
    put sconfig
    put saimMode
    put sitemSel
    put sselected
    put srunning
    put _sreport
    put shistory
    put smarkVision
    put smarkSmell
    put smenuIxSettings
    put smenuIxHelp
    put smenuIxHistory
    put sdisplayNeeded
  get = do
    sxhair <- get
    sconfig <- get
    saimMode <- get
    sitemSel <- get
    sselected <- get
    srunning <- get
    _sreport <- get
    shistory <- get
    smarkVision <- get
    smarkSmell <- get
    smenuIxSettings <- get
    smenuIxHelp <- get
    smenuIxHistory <- get
    sdisplayNeeded <- get
    let schanF = ChanFrontend $ const $ error "Binary: ChanFrontend"
        sbinding = Binding M.empty [] M.empty
        sxhairMoused = True
        spointer = originPoint
        slastRecord = ([], [], 0)
        slastPlay = []
        slastLost = ES.empty
        swaitTimes = 0
        smenuIxMain = 7
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
