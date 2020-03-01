import Prelude ()

import qualified Data.Map.Strict as M
import           Options.Applicative
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Game.LambdaHack.Client.UI.Content.Input as IC
import           Game.LambdaHack.Client.UI.FrameM
import           Game.LambdaHack.Client.UI.Frontend.Chosen
import           Game.LambdaHack.Client.UI.HandleHumanLocalM
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Core.Prelude
import           Game.LambdaHack.Server

import qualified Client.UI.Content.Input as Content.Input

import TieKnot

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
  in [ testCase "Macro 1 from PR#192 description" $ do
         map fst (unwindMacros coinput (stringToKeyMacro "'j''j'"))
         @?= [ ([Right []],       "'j''j'")
             , ([Left []],        "j''j'")
             , ([Left ["j"]],     "''j'")
             , ([Right ["j"]],    "'j'")
             , ([Left []],        "j'")
             , ([Left ["j"]],     "'")
             , ([Right ["j"]],    "") ]
     , testCase "Macro 1 from Issue#189 description" $ do
         snd (last (unwindMacros (bindInput [ ("a", "'bc'v")
                                            , ("c", "'aaa'v") ] coinput)
                                 (stringToKeyMacro "a")))
         @?= "macro looped"
     , testCase "Macro 2 from Issue#189 description" $ do
         snd (last (unwindMacros (bindInput [("a", "'x'")] coinput)
                                 (stringToKeyMacro "'a'")))
         @?= "x"
     , testCase "Macro 3 from Issue#189 description" $ do
         snd (last (unwindMacros coinput (stringToKeyMacro "'x''x'")))
         @?= "xx"
     , testCase "Macro 4 from Issue#189 description" $ do
         snd (last (unwindMacros coinput (stringToKeyMacro "'x''x'v")))
         @?= "xxx"
     , testCase "Macro 5 from Issue#189 description" $ do
         snd (last (unwindMacros coinput (stringToKeyMacro "x'x'v")))
         @?= "xxx"
     , testCase "Macro test 10" $ do
         snd (last (unwindMacros coinput (stringToKeyMacro "x'y'v")))
         @?= "xyy"
     , testCase "Macro test 11" $ do
         snd (last (unwindMacros coinput (stringToKeyMacro "'x''y'v")))
         @?= "xyy"
     , testCase "Macro test 12" $ do
         snd (last (unwindMacros coinput
                                 (listToKeyMacro ["x", "C-V"])))
         @?= "x"
     , testCase "Macro test 13" $ do
         snd (last (unwindMacros coinput
                                 (listToKeyMacro ["'", "x", "'", "C-V"])))
         @?= "xxxxxxxxxxxxxxxxxxxxxxxxxx"
     , testCase "Macro test 14" $ do
         snd (last (unwindMacros coinput
                                 (listToKeyMacro ["'", "x", "'", "y", "C-V"])))
         @?= "xyxxxxxxxxxxxxxxxxxxxxxxxxx"
     -- , testCase "Macro test 15" $ do
     --     snd (last (unwindMacros (bindInput [("a", "x")] coinput)
     --                             (stringToKeyMacro "'a'v")))
     --     @?= "xx"
     , testCase "Macro test 16" $ do
         snd (last (unwindMacros (bindInput [("a", "'x'")] coinput)
                                 (stringToKeyMacro "'a'v")))
         @?= "xx"
     , testCase "Macro test 17" $ do
         snd (last (unwindMacros (bindInput [("a", "'x'v")] coinput)
                                 (stringToKeyMacro "a")))
         @?= "xx"
     , testCase "Macro test 18" $ do
         snd (last (unwindMacros (bindInput [("a", "'x'v")] coinput)
                                 (stringToKeyMacro "'a'")))
         @?= "xx"
     -- , testCase "Macro test 19" $ do
     --     snd (last (unwindMacros (bindInput [("a", "'x'v")] coinput)
     --                             (stringToKeyMacro "'a'v")))
     --     @?= "xxxx"
     -- , testCase "Macro test 20" $ do
     --     snd (last (unwindMacros (bindInput [ ("a", "'bz'v")
     --                                        , ("c", "'aaa'v") ] coinput)
     --                             (stringToKeyMacro "c")))
     --     @?= "bzbzbzbzbzbzbzbzbzbzbzbz"
     ]

-- The mock for macro testing.
unwindMacros :: IC.InputContent -> KeyMacro
             -> [(([Either [String] [String]], String), String)]
unwindMacros IC.InputContent{bcmdMap, brevMap} lastPlay0 =
  let transitionMacros (0 :: Int) _ _ _ =
        [(([], "macro looped"), "macro looped")]  -- probably
      transitionMacros k macros lastPlay cmds =
        storeTrace macros lastPlay cmds
        : case lastPlay of
          KeyMacro [] -> []
          KeyMacro (km : rest) ->
            let ((macros1, lastPlay1), cmds1) = case M.lookup km bcmdMap of
                  Just (_, _, HumanCmd.Record) ->
                    ((fst $ recordHumanTransition macros, KeyMacro rest), cmds)
                  Just (_, _, HumanCmd.Macro ys) ->
                    ( macroHumanTransition ys brevMap macros (KeyMacro rest)
                    , cmds )
                  Just (_, _, HumanCmd.Repeat n) ->
                    ( (macros, repeatHumanTransition n macros (KeyMacro rest))
                    , cmds )
                  _ -> ((macros, KeyMacro rest), cmds ++ [km])
                macros2 = addToMacro brevMap km macros1
            in transitionMacros (k - 1) macros2 lastPlay1 cmds1
      storeTrace macros lastPlay cmds =
        let tmacros = map (either (Left . map K.showKM)
                                  (Right . map K.showKM . unKeyMacro))
                          macros
            tlastPlay = concatMap K.showKM $ unKeyMacro lastPlay
            tcmds = concatMap K.showKM cmds
        in ((tmacros, tlastPlay), tcmds)
  in transitionMacros 1000 [Right (KeyMacro [])] lastPlay0 []

{-

2. Let `j` be an atomic action and the starting content of `slastPlay` be `[',A-b,']` and `A-b` := `[',j,']`.

   | # | smacroStack                           | slastPlay  |
   | - | ------------------------------------- | ------------ |
   | 1 | `[Right []]                  `     | `'A-b'`
   | 2 | `[Left []]                   `     | `A-b'`
   | 3 | `[Right [], Left ["A-b"]]    `     | `'j''`
   | 4 | `[Left [], Left ["A-b"]]     `     | `j''`
   | 5 | `[Left ["j"], Left ["A-b"]]  `     | `''`
   | 6 | `[Right ["j"], Left ["A-b"]] `     | `'`
   | 7 | `[Right ["A-b"]]             `     |

-}

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
