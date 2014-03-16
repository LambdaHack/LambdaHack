-- | Basic client monad and related operations.
module Game.LambdaHack.Client.MonadClient
  ( -- * Basic client monad
    MonadClient( getClient, getsClient, modifyClient, putClient, saveClient
               , liftIO  -- exposed only to be implemented, not used
               )
    -- * Assorted primitives
  , restoreGame, removeServerSave, rndToAction, saveName, debugPrint
  ) where

import Control.Monad
import qualified Control.Monad.State as St
import Data.Maybe
import Data.Text (Text)
import System.Directory
import System.FilePath

import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.ClientOptions
import Game.LambdaHack.Common.Faction
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Random
import qualified Game.LambdaHack.Common.Save as Save
import Game.LambdaHack.Common.State
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Utils.File

class MonadStateRead m => MonadClient m where
  getClient    :: m StateClient
  getsClient   :: (StateClient -> a) -> m a
  modifyClient :: (StateClient -> StateClient) -> m ()
  putClient    :: StateClient -> m ()
  -- We do not provide a MonadIO instance, so that outside of Action/
  -- nobody can subvert the action monads by invoking arbitrary IO.
  liftIO       :: IO a -> m a
  saveClient   :: m ()

saveName :: FactionId -> Bool -> String
saveName side isAI =
  let n = fromEnum side  -- we depend on the numbering hack to number saves
  in (if n > 0
      then "human_" ++ show n
      else "computer_" ++ show (-n))
     ++ if isAI then ".ai.sav" else ".ui.sav"

debugPrint :: MonadClient m => Text -> m ()
debugPrint t = do
  sdbgMsgCli <- getsClient $ sdbgMsgCli . sdebugCli
  when sdbgMsgCli $ liftIO $ Save.delayPrint t

restoreGame :: MonadClient m => m (Maybe (State, StateClient))
restoreGame = do
  Kind.COps{corule} <- getsState scops
  let stdRuleset = Kind.stdRuleset corule
      pathsDataFile = rpathsDataFile stdRuleset
      cfgUIName = rcfgUIName stdRuleset
  side <- getsClient sside
  isAI <- getsClient sisAI
  prefix <- getsClient $ ssavePrefixCli . sdebugCli
  let copies = [( "GameDefinition" </> cfgUIName <.> "default"
                , cfgUIName <.> "ini" )]
      name = fromMaybe "save" prefix <.> saveName side isAI
  liftIO $ Save.restoreGame name copies pathsDataFile

-- | Assuming the client runs on the same machine and for the same
-- user as the server, move the server savegame out of the way.
removeServerSave :: MonadClient m => m ()
removeServerSave = do
  prefix <- getsClient $ ssavePrefixCli . sdebugCli  -- hack: assume the same
  dataDir <- liftIO appDataDir
  let serverSaveFile = dataDir
                       </> fromMaybe "save" prefix
                       <.> serverSaveName
  bSer <- liftIO $ doesFileExist serverSaveFile
  when bSer $ liftIO $ renameFile serverSaveFile (serverSaveFile <.> "bkp")

-- | Invoke pseudo-random computation with the generator kept in the state.
rndToAction :: MonadClient m => Rnd a -> m a
rndToAction r = do
  g <- getsClient srandom
  let (a, ng) = St.runState r g
  modifyClient $ \cli -> cli {srandom = ng}
  return a
