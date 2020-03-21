module SessionUIMock
  ( unwindMacros, unwindMacrosAcc
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Lazy
import           Control.Monad.Trans.Writer.Lazy
import           Data.Bifunctor (bimap)
import qualified Data.Map.Strict as M

import qualified Game.LambdaHack.Client.UI.Content.Input as IC
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Client.UI.FrameM
import           Game.LambdaHack.Client.UI.HandleHumanLocalM
import           Game.LambdaHack.Client.UI.HandleHumanM
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.SessionUI
  (KeyMacro (..), KeyMacroFrame (..), emptyMacroFrame)

data SessionUIMock = SessionUIMock
  { smacroFrame :: KeyMacroFrame
  , smacroStack :: [KeyMacroFrame]
  , sccui       :: CCUI
  , counter     :: Int
  }

type MacroBufferMock = Either String String
type LastPlayMock = String
type LastActionMock = String

type BufferTrace = [(MacroBufferMock, LastPlayMock, LastActionMock)]
type ActionLog = String

data Op = Quit | Looped | HeadEmpty

humanCommandMock :: WriterT [(BufferTrace, ActionLog)] (State SessionUIMock) ()
humanCommandMock = do
  abuffs <- lift $ do
    tmp <- get
    return . fst $ storeTrace (smacroFrame tmp : smacroStack tmp) []
      -- log session
  abortOrCmd <- lift iterationMock -- do stuff
  -- GC macro stack if there are no actions left to handle,
  -- removing all unnecessary macro frames at once,
  -- but leaving the last one for user's in-game macros.
  lift $ modify $ \sess ->
          let (smacroFrameNew, smacroStackMew) =
                dropEmptyMacroFrames (smacroFrame sess) (smacroStack sess)
          in sess { smacroFrame = smacroFrameNew
                  , smacroStack = smacroStackMew }
  case abortOrCmd of
    Right Looped -> tell [(abuffs, "Macro looped")] >> pure ()
    Right _ -> tell [(abuffs, "")] >> pure () -- exit loop
    Left Nothing -> tell [(abuffs, "")] >> humanCommandMock
    Left (Just out) -> tell [(abuffs, show out)] >> humanCommandMock

iterationMock :: State SessionUIMock (Either (Maybe K.KM) Op)
iterationMock = do
  SessionUIMock _ _ CCUI{coinput=IC.InputContent{bcmdMap}} n <- get
  if n <= 1000
  then do
    modify $ \sess -> sess {counter = 1 + counter sess} -- increment counter
    mkm <- promptGetKeyMock []
    case mkm of
      Nothing -> return $ Right HeadEmpty
      Just km -> do
        abortOrCmd <- case km `M.lookup` bcmdMap of
          Just (_, _, cmd) -> restrictedCmdSemInCxtOfKMMock km cmd
          _ -> error "uknown command"
        case abortOrCmd of
          -- exit loop
          Right _ -> return $ Right Quit
          -- loop without appending
          Left Nothing -> return (Left Nothing)
          -- recursive call
          Left (Just out) -> return (Left $ Just out)
  else return $ Right Looped

restrictedCmdSemInCxtOfKMMock :: K.KM -> HumanCmd.HumanCmd
                              -> State SessionUIMock (Either (Maybe K.KM) ())
restrictedCmdSemInCxtOfKMMock = cmdSemInCxtOfKMMock

cmdSemInCxtOfKMMock :: K.KM -> HumanCmd.HumanCmd
                    -> State SessionUIMock (Either (Maybe K.KM) ())
cmdSemInCxtOfKMMock km cmd = do
  modify $ \sess ->
    sess {smacroFrame = updateKeyLast km cmd $ smacroFrame sess}
  cmdSemanticsMock km cmd

cmdSemanticsMock :: K.KM -> HumanCmd.HumanCmd
                 -> State SessionUIMock (Either (Maybe K.KM) ())
cmdSemanticsMock km = \case
  HumanCmd.Record -> do
    modify $ \sess ->
      sess {smacroFrame = fst $ recordHumanTransition (smacroFrame sess) }
    return $ Left Nothing
  HumanCmd.Macro ys -> do
    modify $ \sess ->
      let (smacroFrameNew, smacroStackMew) =
             macroHumanTransition ys (smacroFrame sess) (smacroStack sess)
      in sess { smacroFrame = smacroFrameNew
              , smacroStack = smacroStackMew }
    return $ Left Nothing
  HumanCmd.Repeat n -> do
    modify $ \sess ->
      sess {smacroFrame = repeatHumanTransition n (smacroFrame sess) }
    return $ Left Nothing
  HumanCmd.RepeatLast n -> do
    modify $ \sess ->
      sess {smacroFrame = repeatLastHumanTransition n (smacroFrame sess) }
    return $ Left Nothing
  _ -> return $ Left (Just km)

promptGetKeyMock :: [K.KM] -> State SessionUIMock (Maybe K.KM)
promptGetKeyMock frontKeyKeys = do
  SessionUIMock macroFrame _ CCUI{coinput=IC.InputContent{bcmdMap}} _ <- get
  case keyPending macroFrame of
    KeyMacro (km : kms)
      | null frontKeyKeys || km `elem` frontKeyKeys -> do
        modify $ \sess ->
          sess {smacroFrame = (smacroFrame sess) {keyPending = KeyMacro kms}}
        modify $ \sess ->
          sess {smacroFrame = addToMacro bcmdMap km $ smacroFrame sess}
        return (Just km)
    KeyMacro (_ : _) -> do
      resetPlayBackMock
      return Nothing
    KeyMacro [] -> return Nothing

resetPlayBackMock :: State SessionUIMock ()
resetPlayBackMock = modify $ \sess ->
  let lastFrame = lastMacroFrame (smacroFrame sess) (smacroStack sess)
  in sess { smacroFrame = lastFrame {keyPending = mempty}
          , smacroStack = [] }

-- The mock for macro testing.
unwindMacros :: IC.InputContent -> KeyMacro -> [(BufferTrace, ActionLog)]
unwindMacros ic initMacro =
  let initSession = SessionUIMock
        { smacroFrame = emptyMacroFrame { keyPending = initMacro }
        , smacroStack = []
        , sccui = emptyCCUI { coinput = ic }
        , counter = 0 }
  in evalState (execWriterT humanCommandMock) initSession

accumulateActions :: [(BufferTrace, ActionLog)] -> [(BufferTrace, ActionLog)]
accumulateActions ba =
  let (buffers, actions) = unzip ba
      actionlog = concat <$> inits actions
  in zip buffers actionlog

unwindMacrosAcc :: IC.InputContent -> KeyMacro -> [(BufferTrace, ActionLog)]
unwindMacrosAcc  ic initMacro = accumulateActions $ unwindMacros ic initMacro

storeTrace :: [KeyMacroFrame] -> [K.KM] -> (BufferTrace, ActionLog)
storeTrace abuffs out =
  let tmacros = bimap (concatMap K.showKM)
                      (concatMap K.showKM . unKeyMacro)
                . keyMacroBuffer <$> abuffs
      tlastPlay = concatMap K.showKM . unKeyMacro . keyPending <$> abuffs
      tlastAction = maybe "" K.showKM . keyLast <$> abuffs
      toutput = concatMap K.showKM out
  in (zip3 tmacros tlastPlay tlastAction, toutput)
