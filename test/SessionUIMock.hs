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

data SessionUIMock = SessionUIMock
  { smacroStackMock :: [KeyMacroFrame]
  , sccuiMock       :: CCUI
  , counter         :: Int
  }

type MacroBufferM = Either String String
type LastPlayM = String
type LastActionM = String

type BufferTrace = [(MacroBufferM, LastPlayM, LastActionM)]
type ActionLog = String

data Op = Quit | Looped | HeadEmpty

humanCommandM :: WriterT [(BufferTrace, ActionLog)] (State SessionUIMock) ()
humanCommandM = do
  abuffs <- lift $ do
    tmp <- get
    return . fst $ storeTrace (smacroStackMock tmp) [] -- log session
  abortOrCmd <- lift iterationM -- do stuff
  -- GC action buffer if there's no actions left to handle,
  -- removing all unnecessary buffers at once,
  -- but leaving the last one for user's in-game macros.
  lift $ modify $ \sess ->
    sess { smacroStackMock = dropEmptyBuffers $ smacroStackMock sess }
  case abortOrCmd of
    Right Looped -> tell [(abuffs, "Macro looped")] >> pure ()
    Right _ -> tell [(abuffs, "")] >> pure () -- exit loop
    Left Nothing -> tell [(abuffs, "")] >> humanCommandM
    Left (Just out) -> tell [(abuffs, show out)] >> humanCommandM

iterationM :: State SessionUIMock (Either (Maybe K.KM) Op)
iterationM = do
  SessionUIMock _ CCUI{coinput=IC.InputContent{bcmdMap}} n <- get
  if n <= 1000
  then do
    modify $ \sess -> sess {counter = 1 + counter sess} -- increment counter
    mkm <- promptGetKeyM []
    case mkm of
      Nothing -> return $ Right HeadEmpty
      Just km -> do
        abortOrCmd <- case km `M.lookup` bcmdMap of
          Just (_, _, cmd) -> restrictedCmdSemInCxtOfKMM km cmd
          _ -> error "uknown command"
        case abortOrCmd of
          -- exit loop
          Right _ -> return $ Right Quit
          -- loop without appending
          Left Nothing -> return (Left Nothing)
          -- recursive call
          Left (Just out) -> return (Left $ Just out)
  else return $ Right Looped

restrictedCmdSemInCxtOfKMM :: K.KM -> HumanCmd.HumanCmd
                           -> State SessionUIMock (Either (Maybe K.KM) ())
restrictedCmdSemInCxtOfKMM = cmdSemInCxtOfKMM

cmdSemInCxtOfKMM :: K.KM -> HumanCmd.HumanCmd
                 -> State SessionUIMock (Either (Maybe K.KM) ())
cmdSemInCxtOfKMM km cmd = do
  modify $ \sess ->
    sess {smacroStackMock = updateLastAction km cmd $ smacroStackMock sess}
  cmdSemanticsM km cmd

cmdSemanticsM :: K.KM -> HumanCmd.HumanCmd
              -> State SessionUIMock (Either (Maybe K.KM) ())
cmdSemanticsM km = \case
  HumanCmd.Record -> do
    modify $ \sess ->
      sess { smacroStackMock =
               fst $ recordHumanTransition (smacroStackMock sess) }
    return $ Left Nothing
  HumanCmd.Macro ys -> do
    modify $ \sess ->
      sess { smacroStackMock = macroHumanTransition ys (smacroStackMock sess) }
    return $ Left Nothing
  HumanCmd.Repeat n -> do
    modify $ \sess ->
      sess { smacroStackMock = repeatHumanTransition n (smacroStackMock sess) }
    return $ Left Nothing
  HumanCmd.RepeatLast n -> do
    modify $ \sess ->
      sess { smacroStackMock =
        repeatLastHumanTransition n (smacroStackMock sess) }
    return $ Left Nothing
  _ -> return $ Left (Just km)

promptGetKeyM :: [K.KM] -> State SessionUIMock (Maybe K.KM)
promptGetKeyM frontKeyKeys = do
  SessionUIMock abuffs CCUI{coinput=IC.InputContent{bcmdMap}} _ <- get
  let abuff = head abuffs
  case keyPending abuff of
    KeyMacro (km : kms)
      | null frontKeyKeys || km `elem` frontKeyKeys -> do
        modify $ \sess ->
          sess { smacroStackMock =
            let newHead = abuff { keyPending = KeyMacro kms }
            in newHead : tail abuffs }
        modify $ \sess ->
          sess {smacroStackMock = addToMacro bcmdMap km $ smacroStackMock sess}
        return (Just km)
    KeyMacro (_ : _) -> do
      resetPlayBackM
      return Nothing
    KeyMacro [] -> return Nothing

resetPlayBackM :: State SessionUIMock ()
resetPlayBackM = modify $ \sess ->
  sess { smacroStackMock = let abuff = last (smacroStackMock sess)
                           in [ abuff {keyPending = mempty} ] }

-- The mock for macro testing.
unwindMacros :: IC.InputContent -> KeyMacro -> [(BufferTrace, ActionLog)]
unwindMacros ic initMacro =
  let emptyBuffer = KeyMacroFrame { keyMacroBuffer = Right mempty
                                  , keyPending = mempty
                                  , keyLast = Nothing }
      initSession = SessionUIMock
          { smacroStackMock = [emptyBuffer { keyPending = initMacro }]
          , sccuiMock = emptyCCUI { coinput = ic }
          , counter = 0 }
  in evalState (execWriterT humanCommandM) initSession

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
