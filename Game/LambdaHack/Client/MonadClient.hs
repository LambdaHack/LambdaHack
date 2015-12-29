-- | Basic client monad and related operations.
module Game.LambdaHack.Client.MonadClient
  ( -- * Basic client monad
    MonadClient( getClient, getsClient, modifyClient, putClient
               , saveClient  -- exposed only to be implemented, not used
               , restartClient
               , liftIO  -- exposed only to be implemented, not used
               )
    -- * Assorted primitives
  , debugPrint, saveName, removeServerSave
  , defaultHistory, rndToAction
  ) where

import Control.Monad
import qualified Control.Monad.State as St
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.LocalTime
import qualified NLP.Miniutter.English as MU
import System.Directory
import System.FilePath

import Game.LambdaHack.Client.FileClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Random
import qualified Game.LambdaHack.Common.Save as Save
import Game.LambdaHack.Common.Time

class MonadStateRead m => MonadClient m where
  getClient     :: m StateClient
  getsClient    :: (StateClient -> a) -> m a
  modifyClient  :: (StateClient -> StateClient) -> m ()
  putClient     :: StateClient -> m ()
  saveClient    :: m ()
  restartClient :: m ()
  -- We do not provide a MonadIO instance, so that outside of Action/
  -- nobody can subvert the action monads by invoking arbitrary IO.
  liftIO        :: IO a -> m a

debugPrint :: MonadClient m => Text -> m ()
debugPrint t = do
  sdbgMsgCli <- getsClient $ sdbgMsgCli . sdebugCli
  when sdbgMsgCli $ liftIO $ Save.delayPrint t

saveName :: FactionId -> Bool -> String
saveName side isAI =
  let n = fromEnum side  -- we depend on the numbering hack to number saves
  in (if n > 0
      then "human_" ++ show n
      else "computer_" ++ show (-n))
     ++ if isAI then ".ai.sav" else ".ui.sav"

-- | Assuming the client runs on the same machine and for the same
-- user as the server, move the server savegame out of the way.
removeServerSave :: MonadClient m => m ()
removeServerSave = do
  -- Hack: assume the same prefix for client as for the server.
  prefix <- getsClient $ ssavePrefixCli . sdebugCli
  dataDir <- liftIO appDataDir
  let serverSaveFile = dataDir
                       </> "saves"
                       </> prefix
                       <.> serverSaveName
  bSer <- liftIO $ doesFileExist serverSaveFile
  when bSer $ liftIO $ renameFile serverSaveFile (serverSaveFile <.> "bkp")

defaultHistory :: MonadClient m => Int -> m History
defaultHistory configHistoryMax = liftIO $ do
  utcTime <- getCurrentTime
  timezone <- getTimeZone utcTime
  let curDate = MU.Text $ tshow $ utcToLocalTime timezone utcTime
  let emptyHist = emptyHistory configHistoryMax
  return $! addReport emptyHist timeZero
         $! singletonReport
         $! makeSentence ["Human history log started on", curDate]

-- | Invoke pseudo-random computation with the generator kept in the state.
rndToAction :: MonadClient m => Rnd a -> m a
rndToAction r = do
  g <- getsClient srandom
  let (a, ng) = St.runState r g
  modifyClient $ \cli -> cli {srandom = ng}
  return a
