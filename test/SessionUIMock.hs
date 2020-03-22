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
  , unwindTicks :: Int
  }

type KeyMacroBufferMock = Either String String
type KeyPendingMock = String
type KeyLastMock = String

type BufferTrace = [(KeyMacroBufferMock, KeyPendingMock, KeyLastMock)]
type ActionLog = String

data Op = Looped | HeadEmpty

humanCommandMock :: WriterT [(BufferTrace, ActionLog)] (State SessionUIMock) ()
humanCommandMock = do
  abuffs <- lift $ do
    sess <- get
    return $ renderTrace (smacroFrame sess : smacroStack sess)  -- log session
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
    Left Looped -> tell [(abuffs, "Macro looped")] >> pure ()
    Left HeadEmpty -> tell [(abuffs, "")] >> pure ()  -- exit loop
    Right Nothing -> tell [(abuffs, "")] >> humanCommandMock
    Right (Just out) -> tell [(abuffs, show out)] >> humanCommandMock

iterationMock :: State SessionUIMock (Either Op (Maybe K.KM))
iterationMock = do
  SessionUIMock _ _ CCUI{coinput=IC.InputContent{bcmdMap}} ticks <- get
  if ticks <= 1000
  then do
    modify $ \sess -> sess {unwindTicks = ticks + 1}
    mkm <- promptGetKeyMock
    case mkm of
      Nothing -> return $ Left HeadEmpty  -- macro finished
      Just km -> case km `M.lookup` bcmdMap of
        Just (_, _, cmd) -> Right <$> cmdSemInCxtOfKMMock km cmd
        _ -> return $ Right $ Just km  -- unknown command; fine for tests
  else return $ Left Looped

cmdSemInCxtOfKMMock :: K.KM -> HumanCmd.HumanCmd
                    -> State SessionUIMock (Maybe K.KM)
cmdSemInCxtOfKMMock km cmd = do
  modify $ \sess ->
    sess {smacroFrame = updateKeyLast km cmd $ smacroFrame sess}
  cmdSemanticsMock km cmd

cmdSemanticsMock :: K.KM -> HumanCmd.HumanCmd
                 -> State SessionUIMock (Maybe K.KM)
cmdSemanticsMock km = \case
  HumanCmd.Macro s -> do
    modify $ \sess ->
      let kms = K.mkKM <$> s
          (smacroFrameNew, smacroStackMew) =
             macroHumanTransition kms (smacroFrame sess) (smacroStack sess)
      in sess { smacroFrame = smacroFrameNew
              , smacroStack = smacroStackMew }
    return Nothing
  HumanCmd.Repeat n -> do
    modify $ \sess ->
      let (smacroFrameNew, smacroStackMew) =
             repeatHumanTransition n (smacroFrame sess) (smacroStack sess)
      in sess { smacroFrame = smacroFrameNew
              , smacroStack = smacroStackMew }
    return Nothing
  HumanCmd.RepeatLast n -> do
    modify $ \sess ->
      sess {smacroFrame = repeatLastHumanTransition n (smacroFrame sess) }
    return Nothing
  HumanCmd.Record -> do
    modify $ \sess ->
      sess {smacroFrame = fst $ recordHumanTransition (smacroFrame sess) }
    return Nothing
  _ -> return $ Just km

promptGetKeyMock :: State SessionUIMock (Maybe K.KM)
promptGetKeyMock = do
  SessionUIMock macroFrame _ CCUI{coinput=IC.InputContent{bcmdMap}} _ <- get
  case keyPending macroFrame of
    KeyMacro (km : kms) -> do
        modify $ \sess ->
          sess {smacroFrame = (smacroFrame sess) {keyPending = KeyMacro kms}}
        modify $ \sess ->
          sess {smacroFrame = addToMacro bcmdMap km $ smacroFrame sess}
        return (Just km)
    KeyMacro [] -> return Nothing

unwindMacros :: IC.InputContent -> KeyMacro -> [(BufferTrace, ActionLog)]
unwindMacros coinput keyPending =
  let initSession = SessionUIMock
        { smacroFrame = emptyMacroFrame {keyPending}
        , smacroStack = []
        , sccui = emptyCCUI {coinput}
        , unwindTicks = 0 }
  in evalState (execWriterT humanCommandMock) initSession

accumulateActions :: [(BufferTrace, ActionLog)] -> [(BufferTrace, ActionLog)]
accumulateActions ba =
  let (buffers, actions) = unzip ba
      actionlog = concat <$> inits actions
  in zip buffers actionlog

unwindMacrosAcc :: IC.InputContent -> KeyMacro -> [(BufferTrace, ActionLog)]
unwindMacrosAcc coinput keyPending =
  accumulateActions $ unwindMacros coinput keyPending

renderTrace :: [KeyMacroFrame] -> BufferTrace
renderTrace macroFrames =
  let buffers = bimap (concatMap K.showKM)
                      (concatMap K.showKM . unKeyMacro)
                . keyMacroBuffer <$> macroFrames
      pendingKeys = concatMap K.showKM . unKeyMacro . keyPending <$> macroFrames
      lastKeys = maybe "" K.showKM . keyLast <$> macroFrames
  in zip3 buffers pendingKeys lastKeys
