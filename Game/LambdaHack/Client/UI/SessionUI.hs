{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The client UI session state.
module Game.LambdaHack.Client.UI.SessionUI
  ( SessionUI(..), emptySessionUI
  , TgtMode(..), RunParams(..), LastRecord
  , toggleMarkVision, toggleMarkSmell
  ) where

import Prelude ()
import Prelude.Compat

import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Map.Strict as M
import Data.Text (Text)

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.Frontend
import Game.LambdaHack.Client.UI.KeyBindings
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Time

-- | The information that is used across a client playing session,
-- including many consecutive games in a single session.
-- Some of it is save, some is reset when a new playing session starts.
-- An important component is a frontend session.
data SessionUI = SessionUI
  { schanF          :: !ChanFrontend       -- ^ connection with the frontend
  , sbinding        :: !Binding            -- ^ binding of keys to commands
  , sconfig         :: !Config
  , stgtMode        :: !(Maybe TgtMode)    -- ^ targeting mode
  , sselected       :: !(ES.EnumSet ActorId)
                                      -- ^ the set of currently selected actors
  , srunning        :: !(Maybe RunParams)
                                      -- ^ parameters of the current run, if any
  , sreport         :: !Report        -- ^ current messages
  , shistory        :: !History       -- ^ history of messages
  , sdisplayed      :: !(EM.EnumMap LevelId Time)
                                      -- ^ moves already displayed up to then
  , slastKM         :: !K.KM          -- ^ last issued key command
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
  }

-- | Current targeting mode of a client.
newtype TgtMode = TgtMode { tgtLevelId :: LevelId }
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

-- | Initial empty game client state.
emptySessionUI :: Config -> SessionUI
emptySessionUI sconfig =
  SessionUI
    { schanF = ChanFrontend $ const $ error "emptySessionUI: ChanFrontend "
    , sbinding = Binding M.empty [] M.empty
    , sconfig
    , stgtMode = Nothing
    , sselected = ES.empty
    , srunning = Nothing
    , sreport = emptyReport
    , shistory = emptyHistory 0
    , sdisplayed = EM.empty
    , slastKM = K.escKM
    , slastRecord = ([], [], 0)
    , slastPlay = []
    , slastLost = ES.empty
    , swaitTimes = 0
    , smarkVision = False
    , smarkSmell = True
    , smenuIxMain = 0
    , smenuIxSettings = 0
    , smenuIxHelp = 0
    , smenuIxHistory = 0
    }

toggleMarkVision :: SessionUI -> SessionUI
toggleMarkVision s@SessionUI{smarkVision} = s {smarkVision = not smarkVision}

toggleMarkSmell :: SessionUI -> SessionUI
toggleMarkSmell s@SessionUI{smarkSmell} = s {smarkSmell = not smarkSmell}

instance Binary SessionUI where
  put SessionUI{..} = do
    put sconfig
    put stgtMode
    put sselected
    put srunning
    put sreport
    put shistory
    put sdisplayed
    put smarkVision
    put smarkSmell
    put smenuIxMain
    put smenuIxSettings
    put smenuIxHelp
    put smenuIxHistory
  get = do
    sconfig <- get
    stgtMode <- get
    sselected <- get
    srunning <- get
    sreport <- get
    shistory <- get
    sdisplayed <- get
    smarkVision <- get
    smarkSmell <- get
    smenuIxMain <- get
    smenuIxSettings <- get
    smenuIxHelp <- get
    smenuIxHistory <- get
    let schanF = ChanFrontend $ const $ error "Binary: ChanFrontend"
        sbinding = Binding M.empty [] M.empty
        slastKM = K.escKM
        slastRecord = ([], [], 0)
        slastPlay = []
        slastLost = ES.empty
        swaitTimes = 0
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
