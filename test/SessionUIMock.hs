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
  { sactionPendingM :: [ActionBuffer]
  , sccuiM          :: CCUI
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
    return . fst $ storeTrace (sactionPendingM tmp) [] -- log session
  abortOrCmd <- lift iterationM -- do stuff
  lift $ modify $ \sess ->
    sess { sactionPendingM = dropEmptyBuffers $ sactionPendingM sess }
    -- remove all unnecessary buffers at once
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
    sess {sactionPendingM = updateLastAction km cmd $ sactionPendingM sess}
  cmdSemanticsM km cmd

cmdSemanticsM :: K.KM -> HumanCmd.HumanCmd
              -> State SessionUIMock (Either (Maybe K.KM) ())
cmdSemanticsM km = \case
  HumanCmd.Record -> do
    modify $ \sess ->
      sess { sactionPendingM =
               fst $ recordHumanTransition (sactionPendingM sess) }
    return $ Left Nothing
  HumanCmd.Macro ys -> do
    modify $ \sess ->
      sess { sactionPendingM = macroHumanTransition ys (sactionPendingM sess) }
    return $ Left Nothing
  HumanCmd.Repeat n -> do
    modify $ \sess ->
      sess { sactionPendingM = repeatHumanTransition n (sactionPendingM sess) }
    return $ Left Nothing
  HumanCmd.RepeatLast n -> do
    modify $ \sess ->
      sess { sactionPendingM =
        repeatLastHumanTransition n (sactionPendingM sess) }
    return $ Left Nothing
  _ -> return $ Left (Just km)

promptGetKeyM :: [K.KM] -> State SessionUIMock (Maybe K.KM)
promptGetKeyM frontKeyKeys = do
  SessionUIMock abuffs CCUI{coinput=IC.InputContent{bcmdMap}} _ <- get
  let abuff = head abuffs
  case slastPlay abuff of
    KeyMacro (km : kms)
      | null frontKeyKeys || km `elem` frontKeyKeys -> do
        modify $ \sess ->
          sess { sactionPendingM =
            let newHead = abuff { slastPlay = KeyMacro kms }
            in newHead : tail abuffs }
        modify $ \sess ->
          sess {sactionPendingM = addToMacro bcmdMap km $ sactionPendingM sess}
        return (Just km)
    KeyMacro (_ : _) -> do
      resetPlayBackM
      return Nothing
    KeyMacro [] -> return Nothing

resetPlayBackM :: State SessionUIMock ()
resetPlayBackM = modify $ \sess ->
  sess { sactionPendingM = let abuff = last (sactionPendingM sess)
                           in [ abuff {slastPlay = mempty} ] }

-- The mock for macro testing.
unwindMacros :: IC.InputContent -> KeyMacro -> [(BufferTrace, ActionLog)]
unwindMacros ic initMacro =
  let emptyBuffer = ActionBuffer { smacroBuffer = Right mempty
                                 , slastPlay = mempty
                                 , slastAction = Nothing }
      initSession = SessionUIMock
          { sactionPendingM = [emptyBuffer { slastPlay = initMacro }]
          , sccuiM = emptyCCUI { coinput = ic }
          , counter = 0 }
  in evalState (execWriterT humanCommandM) initSession

accumulateActions :: [(BufferTrace, ActionLog)] -> [(BufferTrace, ActionLog)]
accumulateActions ba =
  let (buffers, actions) = unzip ba
      actionlog = concat <$> inits actions
  in zip buffers actionlog

unwindMacrosAcc :: IC.InputContent -> KeyMacro -> [(BufferTrace, ActionLog)]
unwindMacrosAcc  ic initMacro = accumulateActions $ unwindMacros ic initMacro

storeTrace :: [ActionBuffer] -> [K.KM] -> (BufferTrace, ActionLog)
storeTrace abuffs out =
  let tmacros = bimap (concatMap K.showKM)
                      (concatMap K.showKM . unKeyMacro)
                . smacroBuffer <$> abuffs
      tlastPlay = concatMap K.showKM . unKeyMacro . slastPlay <$> abuffs
      tlastAction = maybe "" K.showKM . slastAction <$> abuffs
      toutput = concatMap K.showKM out
  in (zip3 tmacros tlastPlay tlastAction, toutput)
