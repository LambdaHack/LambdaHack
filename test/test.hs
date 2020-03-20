import Prelude ()

import           Data.Bifunctor (bimap)
import qualified Data.Map.Strict as M
import           Options.Applicative
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Game.LambdaHack.Client.UI.Content.Input as IC
import           Game.LambdaHack.Client.UI.FrameM
import           Game.LambdaHack.Client.UI.Frontend.Chosen
import           Game.LambdaHack.Client.UI.HandleHumanLocalM
import           Game.LambdaHack.Client.UI.HandleHumanM
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.ContentClientUI
import           Game.LambdaHack.Core.Prelude
import           Game.LambdaHack.Server

import qualified Client.UI.Content.Input as Content.Input

import TieKnot
import           Control.Monad.Trans.Writer.Lazy
import           Control.Monad.Trans.State.Lazy
import           Control.Monad.Trans.Class

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [macroTests, integrationTests]

macroTests :: TestTree
macroTests = testGroup "macroTests" $
  let coinput = IC.makeData Nothing Content.Input.standardKeysAndMouse
      stringToKeyMacro = KeyMacro . map (K.mkKM . (: []))
      listToKeyMacro = KeyMacro . map K.mkKM
      bindInput l input =
        let ltriple = M.fromList $ map (\(k, ks) ->
              (K.mkKM k, ([], "", HumanCmd.Macro $ map (: []) ks))) l
        in input {IC.bcmdMap = M.union ltriple $ IC.bcmdMap input}
  in [ testCase "Macro 1 from PR#192 description" $
         fst <$> unwindMacros coinput (stringToKeyMacro "'j''j'")
         @?= [ [ (Right "",     "'j''j'",  "")  ]
             , [ (Left  "",     "j''j'",   "") ]
             , [ (Left  "j",    "''j'",    "j") ]
             , [ (Right "j",    "'j'",     "j") ]
             , [ (Left  "",     "j'",      "j") ]
             , [ (Left  "j",    "'",       "j") ]
             , [ (Right "j",    "",        "j") ]
             ]
     , testCase "Macro 1 from Issue#189 description" $
         snd (last (unwindMacros (bindInput [ ("a", "'bc'V")
                                       , ("c", "'aaa'V") ] coinput)
                 (stringToKeyMacro "a")))
         @?= "Macro looped"
     , testCase "Macro 2 from Issue#189 description" $
         snd (last (accumulateActions (unwindMacros
                                (bindInput [("a", "'x'")] coinput)
                                  (stringToKeyMacro "'a'"))))
         @?= "x"
     , testCase "Macro 3 from Issue#189 description" $
         snd (last (accumulateActions (unwindMacros coinput
                                (stringToKeyMacro "'x''x'"))))
         @?= "xx"
     , testCase "Macro 4 from Issue#189 description" $
         snd (last (accumulateActions (unwindMacros coinput
                                (stringToKeyMacro "'x''x'V"))))
         @?= "xxx"
     , testCase "Macro 5 from Issue#189 description" $
         snd (last (accumulateActions (unwindMacros coinput
                                (stringToKeyMacro "x'x'V"))))
         @?= "xxx"
     , testCase "Macro test 10" $
         snd (last (accumulateActions (unwindMacros coinput
                                (stringToKeyMacro "x'y'V"))))
         @?= "xyy"
     , testCase "Macro test 11" $
         snd (last (accumulateActions (unwindMacros coinput
                                (stringToKeyMacro "'x''y'V"))))
         @?= "xyy"
     , testCase "Macro test 12" $
         snd (last (accumulateActions (unwindMacros coinput
                                (listToKeyMacro ["x", "C-V"]))))
         @?= "x"
     , testCase "Macro test 13" $
         snd (last (accumulateActions (unwindMacros coinput
                                (listToKeyMacro ["'", "x", "'", "C-V"]))))
         @?= "xxxxxxxxxxxxxxxxxxxxxxxxxx"
     , testCase "Macro test 14" $
         snd (last (accumulateActions (unwindMacros coinput
                                (listToKeyMacro ["'", "x", "'", "y", "C-V"]))))
         @?= "xyxxxxxxxxxxxxxxxxxxxxxxxxx"
     , testCase "Macro test 15" $
         snd (last (accumulateActions (unwindMacros
                                (bindInput [("a", "x")] coinput)
                                  (stringToKeyMacro "'a'V"))))
         @?= "xx"
     , testCase "Macro test 16" $
         snd (last (accumulateActions (unwindMacros
                                (bindInput [("a", "'x'")] coinput)
                                  (stringToKeyMacro "'a'V"))))
         @?= "xx"
     , testCase "Macro test 17" $
         snd (last (accumulateActions (unwindMacros
                                (bindInput [("a", "'x'V")] coinput)
                                  (stringToKeyMacro "a"))))
         @?= "xx"
     , testCase "Macro test 18" $
         snd (last (accumulateActions (unwindMacros
                                (bindInput [("a", "'x'V")] coinput)
                                  (stringToKeyMacro "'a'"))))
         @?= "xx"
     , testCase "Macro test 19" $
         snd (last (accumulateActions (unwindMacros
                                (bindInput [("a", "'x'V")] coinput)
                                  (stringToKeyMacro "'a'V"))))
         @?= "xxxx"
     , testCase "Macro test 20" $
         snd (last (accumulateActions (unwindMacros
                                (bindInput [ ("a", "'bz'V")
                                           , ("c", "'aaa'V") ] coinput)
                                  (stringToKeyMacro "c"))))
         @?= "bzbzbzbzbzbzbzbzbzbzbzbz"
     , testCase "RepeatLast test 10" $
         snd (last (accumulateActions (unwindMacros coinput
                                 (stringToKeyMacro "x'y'v"))))
         @?= "xyy"
     , testCase "RepeatLast test 11" $
         snd (last (accumulateActions (unwindMacros coinput
                                 (stringToKeyMacro "'x'yv"))))
         @?= "xyy"
     , testCase "RepeatLast test 12" $
         snd (last (accumulateActions (unwindMacros coinput
                                 (listToKeyMacro ["v", "C-v"]))))
         @?= ""
     , testCase "RepeatLast test 13" $
         snd (last (accumulateActions (unwindMacros coinput
                                 (listToKeyMacro ["'", "x", "'", "C-v"]))))
         @?= "xxxxxxxxxxxxxxxxxxxxxxxxxx"
     , testCase "RepeatLast test 14" $
         snd (last (accumulateActions (unwindMacros coinput
                                 (listToKeyMacro ["'", "x", "'", "V", "C-v"]))))
         @?= "xxxxxxxxxxxxxxxxxxxxxxxxxxx"
     , testCase "RepeatLast test 15" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [("a", "x")] coinput)
                                 (stringToKeyMacro "av"))))
         @?= "xx"
     , testCase "RepeatLast test 16" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [("a", "'x'")] coinput)
                                 (stringToKeyMacro "'a'v"))))
         @?= "xx"
     , testCase "RepeatLast test 17" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [("a", "'x'v")] coinput)
                                 (stringToKeyMacro "a"))))
         @?= "xx"
     , testCase "RepeatLast test 18" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [("a", "'x'v")] coinput)
                                 (stringToKeyMacro "'a'"))))
         @?= "xx"
     , testCase "RepeatLast test 19" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [("a", "'x'v")] coinput)
                                 (stringToKeyMacro "'a'v"))))
         @?= "xxxx"
     , testCase "RepeatLast test 20" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [ ("a", "'bz'v")
                                         , ("c", "'aaa'v") ] coinput)
                                 (stringToKeyMacro "c"))))
         @?= "bzzbzzbzzbzz"
     , testCase "RepeatLast test 21" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [("a", "'x'V")] coinput)
                                 (stringToKeyMacro "'a'v"))))
         @?= "xxxx"
     , testCase "RepeatLast test 22" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [("a", "'xy'V")] coinput)
                                 (stringToKeyMacro "'aa'v"))))
         @?= "xyxyxyxyxyxy"
     , testCase "RepeatLast test 23" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [("a", "'xy'v")] coinput)
                                 (stringToKeyMacro "'aa'V"))))
         @?= "xyyxyyxyyxyy"
     , testCase "RepeatLast test 24" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [("a", "'xy'vv")] coinput)
                                 (stringToKeyMacro "'aa'vv"))))
         @?= "xyyyxyyyxyyyxyyy"
     , testCase "RepeatLast test 25" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [("a", "'xyv'v")] coinput)
                                 (stringToKeyMacro "'a'a'vv'"))))
         @?= "xyyyxyyyxyyyxyyy"
     , testCase "RepeatLast test 26" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [ ("a", "'xy'v")
                                         , ("b", "'za'v")
                                         , ("c", "'ab'v") ] coinput)
                                 (stringToKeyMacro "'c'v"))))
         @?= "xyyzxyyxyyzxyyxyyxyyzxyyxyyzxyyxyy"
     , testCase "RepeatLast test 27" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [ ("a", "'xy'V")
                                         , ("b", "'za'v")
                                         , ("c", "'ab'v") ] coinput)
                                 (stringToKeyMacro "'c'v"))))
         @?= "xyxyzxyxyxyxyzxyxyxyxyxyxyzxyxyxyxyzxyxyxyxy"
     , testCase "RepeatLast test 28" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [ ("a", "'xy'v")
                                         , ("b", "'za'V")
                                         , ("c", "'ab'v") ] coinput)
                                 (stringToKeyMacro "'c'v"))))
         @?= "xyyzxyyzxyyzxyyzxyyxyyzxyyzxyyzxyyzxyy"
     , testCase "RepeatLast test 29" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [ ("a", "'xy'v")
                                         , ("b", "'za'V")
                                         , ("c", "'ab'V") ] coinput)
                                 (stringToKeyMacro "'c'v"))))
         @?= "xyyzxyyzxyyxyyzxyyzxyyxyyzxyyzxyyxyyzxyyzxyy"
     , testCase "RepeatLast test 30" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [ ("a", "'xy'v")
                                         , ("b", "'za'V")
                                         , ("c", "'ab'V") ] coinput)
                                 (stringToKeyMacro "'c'V"))))
         @?= "xyyzxyyzxyyxyyzxyyzxyyxyyzxyyzxyyxyyzxyyzxyy"
     , testCase "RepeatLast test 31" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [ ("a", "'xy'v")
                                         , ("b", "'za'v")
                                         , ("c", "'ab'V") ] coinput)
                                 (stringToKeyMacro "'c'V"))))
         @?= "xyyzxyyxyyxyyzxyyxyyxyyzxyyxyyxyyzxyyxyy"
     , testCase "RepeatLast test 32" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [ ("a", "'xy'v")
                                         , ("b", "'za'v") ] coinput)
                                 (stringToKeyMacro "'ab'vv"))))
         @?= "xyyzxyyxyyzxyyxyyzxyyxyy"
     , testCase "RepeatLast test 33" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [ ("a", "'xy'V") ] coinput)
                                 (stringToKeyMacro "a'za'vvv"))))
         @?= "xyxyzxyxyxyxyxyxyxyxy"
     , testCase "RepeatLast test 34" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [ ("a", "'xy'v")
                                         , ("c", "a'za'Vv") ] coinput)
                                 (stringToKeyMacro "'c'v"))))
         @?= "xyyzxyyzxyyxyyxyyzxyyzxyyxyy"
     , testCase "RepeatLast test 35" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [ ("a", "'xy'v")
                                         , ("b", "'za'V") ] coinput)
                                 (stringToKeyMacro "'ab'Vv"))))
         @?= "xyyzxyyzxyyxyyzxyyzxyyzxyyzxyy"
     , testCase "RepeatLast test 36" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [ ("a", "'xy'v")
                                         , ("b", "za'za'") ] coinput)
                                 (stringToKeyMacro "'ab'V'ab'V"))))
         @?= "xyyzxyyzxyyxyyzxyyzxyyxyyzxyyzxyyxyyzxyyzxyy"
     , testCase "RepeatLast test 37" $
         snd (last (accumulateActions (unwindMacros
                              (bindInput [ ("b", "z'xy'vv")
                                         , ("c", "'xyvb'V") ] coinput)
                                 (stringToKeyMacro "'c'V"))))
         @?= "xyyzxyyyxyyzxyyyxyyzxyyyxyyzxyyy"
     , testCase "RepeatLast test 38" $
         snd (last (accumulateActions (unwindMacros coinput
                                        (stringToKeyMacro "'xv'V"))))
         @?= "xxxx"
     , testCase "RepeatLast test 39" $
         fst <$> unwindMacros coinput (stringToKeyMacro "'xv'V")
         @?= [[(Right   "", "'xv'V",  "")],
              [(Left    "",  "xv'V",  "")],
              [(Left   "x",   "v'V", "x")],
              [(Left   "x",   "x'V", "x")],
              [(Left  "xx",    "'V", "x")],
              [(Right "xx",     "V", "x")],
              [(Right "xx",    "xx", "V")],
              [(Right "xx",     "x", "x")],
              [(Right "xx",      "", "x")]]
     ]

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
          sess { sactionPendingM = addToMacro bcmdMap km $ sactionPendingM sess }
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

storeTrace :: [ActionBuffer] -> [K.KM] -> (BufferTrace, ActionLog)
storeTrace abuffs out =
  let tmacros = bimap (concatMap K.showKM)
                      (concatMap K.showKM . unKeyMacro)
              . smacroBuffer <$> abuffs
      tlastPlay = concatMap K.showKM . unKeyMacro . slastPlay <$> abuffs
      tlastAction = maybe "" K.showKM . slastAction <$> abuffs
      toutput = concatMap K.showKM out
  in (zip3 tmacros tlastPlay tlastAction, toutput)

integrationTests :: TestTree
integrationTests = testGroup "integrationTests"
  [ testCase "Null frontend; 50 frames" $ do
      let args = words "--dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 50 --automateAll --keepAutomated --gameMode crawl" ++ ["--setDungeonRng", "SMGen 123 123", "--setMainRng", "SMGen 123 123"]
      serverOptions <- handleParseResult $ execParserPure defaultPrefs serverOptionsPI args
      tieKnot serverOptions
  , testCase "SDL fronted; init only" $
      when (frontendName == "sdl") $ do
        -- The hacky log priority 0 tells SDL frontend to init and quit at once,
        -- for testing on CIs without graphics access.
        let args2 = words "--dbgMsgSer --logPriority 0 --newGame 3 --maxFps 100000 --benchmark --stopAfterFrames 50 --automateAll --keepAutomated --gameMode battle" ++ ["--setDungeonRng", "SMGen 125 125", "--setMainRng", "SMGen 125 125"]
        serverOptions2 <- handleParseResult $ execParserPure defaultPrefs serverOptionsPI args2
        tieKnot serverOptions2
  ]
