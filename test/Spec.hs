module Main (main) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Options.Applicative
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import qualified Game.LambdaHack.Client.UI.Content.Input as IC
import           Game.LambdaHack.Client.UI.Frontend.Chosen
import qualified Game.LambdaHack.Client.UI.HumanCmd as HumanCmd
import qualified Game.LambdaHack.Client.UI.Key as K
import           Game.LambdaHack.Client.UI.SessionUI
import           Game.LambdaHack.Client.UI.UIOptions
import           Game.LambdaHack.Client.UI.UIOptionsParse
import           Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Content.RuleKind as RK
import           Game.LambdaHack.Server

import qualified Client.UI.Content.Input as Content.Input
import qualified Content.RuleKind
import           TieKnot

import SessionUIMock

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
         snd (last (unwindMacrosAcc (bindInput [("a", "'x'")] coinput)
                                    (stringToKeyMacro "'a'")))
         @?= "x"
     , testCase "Macro 3 from Issue#189 description" $
         snd (last (unwindMacrosAcc coinput
                                    (stringToKeyMacro "'x''x'")))
         @?= "xx"
     , testCase "Macro 4 from Issue#189 description" $
         snd (last (unwindMacrosAcc coinput
                                    (stringToKeyMacro "'x''x'V")))
         @?= "xxx"
     , testCase "Macro 5 from Issue#189 description" $
         snd (last (unwindMacrosAcc coinput
                                    (stringToKeyMacro "x'x'V")))
         @?= "xxx"
     , testCase "Macro test 10" $
         snd (last (unwindMacrosAcc coinput
                                    (stringToKeyMacro "x'y'V")))
         @?= "xyy"
     , testCase "Macro test 11" $
         snd (last (unwindMacrosAcc coinput
                                    (stringToKeyMacro "'x''y'V")))
         @?= "xyy"
     , testCase "Macro test 12" $
         snd (last (unwindMacrosAcc coinput
                                    (listToKeyMacro ["x", "C-V"])))
         @?= "x"
     , testCase "Macro test 13" $
         snd (last (unwindMacrosAcc coinput
                                    (listToKeyMacro ["'", "x", "'", "C-V"])))
         @?= "xxxxxxxxxxxxxxxxxxxxxxxxxx"
     , testCase "Macro test 14" $
         snd (last (unwindMacrosAcc coinput
                                    (listToKeyMacro
                                       ["'", "x", "'", "y", "C-V"])))
         @?= "xyxxxxxxxxxxxxxxxxxxxxxxxxx"
     , testCase "Macro test 15" $
         snd (last (unwindMacrosAcc (bindInput [("a", "x")] coinput)
                                    (stringToKeyMacro "'a'V")))
         @?= "xx"
     , testCase "Macro test 16" $
         snd (last (unwindMacrosAcc (bindInput [("a", "'x'")] coinput)
                                    (stringToKeyMacro "'a'V")))
         @?= "xx"
     , testCase "Macro test 17" $
         snd (last (unwindMacrosAcc (bindInput [("a", "'x'V")] coinput)
                                    (stringToKeyMacro "a")))
         @?= "xx"
     , testCase "Macro test 18" $
         snd (last (unwindMacrosAcc (bindInput [("a", "'x'V")] coinput)
                                    (stringToKeyMacro "'a'")))
         @?= "xx"
     , testCase "Macro test 19" $
         snd (last (unwindMacrosAcc (bindInput [("a", "'x'V")] coinput)
                                    (stringToKeyMacro "'a'V")))
         @?= "xxxx"
     , testCase "Macro test 20" $
         snd (last (unwindMacrosAcc (bindInput [ ("a", "'bz'V")
                                               , ("c", "'aaa'V") ] coinput)
                                    (stringToKeyMacro "c")))
         @?= "bzbzbzbzbzbzbzbzbzbzbzbz"
     , testCase "RepeatLast test 10" $
         snd (last (unwindMacrosAcc coinput
                                    (stringToKeyMacro "x'y'v")))
         @?= "xyy"
     , testCase "RepeatLast test 11" $
         snd (last (unwindMacrosAcc coinput
                                    (stringToKeyMacro "'x'yv")))
         @?= "xyy"
     , testCase "RepeatLast test 12" $
         snd (last (unwindMacrosAcc coinput
                                    (listToKeyMacro ["v", "C-v"])))
         @?= ""
     , testCase "RepeatLast test 13" $
         snd (last (unwindMacrosAcc coinput
                                    (listToKeyMacro ["'", "x", "'", "C-v"])))
         @?= "xxxxxxxxxxxxxxxxxxxxxxxxxx"
     , testCase "RepeatLast test 14" $
         snd (last (unwindMacrosAcc coinput
                                    (listToKeyMacro
                                       ["'", "x", "'", "V", "C-v"])))
         @?= "xxxxxxxxxxxxxxxxxxxxxxxxxxx"
     , testCase "RepeatLast test 15" $
         snd (last (unwindMacrosAcc (bindInput [("a", "x")] coinput)
                                    (stringToKeyMacro "av")))
         @?= "xx"
     , testCase "RepeatLast test 16" $
         snd (last (unwindMacrosAcc (bindInput [("a", "'x'")] coinput)
                                    (stringToKeyMacro "'a'v")))
         @?= "xx"
     , testCase "RepeatLast test 17" $
         snd (last (unwindMacrosAcc (bindInput [("a", "'x'v")] coinput)
                                    (stringToKeyMacro "a")))
         @?= "xx"
     , testCase "RepeatLast test 18" $
         snd (last (unwindMacrosAcc (bindInput [("a", "'x'v")] coinput)
                                    (stringToKeyMacro "'a'")))
         @?= "xx"
     , testCase "RepeatLast test 19" $
         snd (last (unwindMacrosAcc (bindInput [("a", "'x'v")] coinput)
                                    (stringToKeyMacro "'a'v")))
         @?= "xxxx"
     , testCase "RepeatLast test 20" $
         snd (last (unwindMacrosAcc (bindInput [ ("a", "'bz'v")
                                         , ("c", "'aaa'v") ] coinput)
                                    (stringToKeyMacro "c")))
         @?= "bzzbzzbzzbzz"
     , testCase "RepeatLast test 21" $
         snd (last (unwindMacrosAcc (bindInput [("a", "'x'V")] coinput)
                                    (stringToKeyMacro "'a'v")))
         @?= "xxxx"
     , testCase "RepeatLast test 22" $
         snd (last (unwindMacrosAcc (bindInput [("a", "'xy'V")] coinput)
                                    (stringToKeyMacro "'aa'v")))
         @?= "xyxyxyxyxyxy"
     , testCase "RepeatLast test 23" $
         snd (last (unwindMacrosAcc (bindInput [("a", "'xy'v")] coinput)
                                    (stringToKeyMacro "'aa'V")))
         @?= "xyyxyyxyyxyy"
     , testCase "RepeatLast test 24" $
         snd (last (unwindMacrosAcc (bindInput [("a", "'xy'vv")] coinput)
                                    (stringToKeyMacro "'aa'vv")))
         @?= "xyyyxyyyxyyyxyyy"
     , testCase "RepeatLast test 25" $
         snd (last (unwindMacrosAcc (bindInput [("a", "'xyv'v")] coinput)
                                    (stringToKeyMacro "'a'a'vv'")))
         @?= "xyyyxyyyxyyyxyyy"
     , testCase "RepeatLast test 26" $
         snd (last (unwindMacrosAcc (bindInput [ ("a", "'xy'v")
                                               , ("b", "'za'v")
                                               , ("c", "'ab'v") ] coinput)
                                    (stringToKeyMacro "'c'v")))
         @?= "xyyzxyyxyyzxyyxyyxyyzxyyxyyzxyyxyy"
     , testCase "RepeatLast test 27" $
         snd (last (unwindMacrosAcc (bindInput [ ("a", "'xy'V")
                                               , ("b", "'za'v")
                                               , ("c", "'ab'v") ] coinput)
                                    (stringToKeyMacro "'c'v")))
         @?= "xyxyzxyxyxyxyzxyxyxyxyxyxyzxyxyxyxyzxyxyxyxy"
     , testCase "RepeatLast test 28" $
         snd (last (unwindMacrosAcc (bindInput [ ("a", "'xy'v")
                                               , ("b", "'za'V")
                                               , ("c", "'ab'v") ] coinput)
                                    (stringToKeyMacro "'c'v")))
         @?= "xyyzxyyzxyyzxyyzxyyxyyzxyyzxyyzxyyzxyy"
     , testCase "RepeatLast test 29" $
         snd (last (unwindMacrosAcc (bindInput [ ("a", "'xy'v")
                                               , ("b", "'za'V")
                                               , ("c", "'ab'V") ] coinput)
                                    (stringToKeyMacro "'c'v")))
         @?= "xyyzxyyzxyyxyyzxyyzxyyxyyzxyyzxyyxyyzxyyzxyy"
     , testCase "RepeatLast test 30" $
         snd (last (unwindMacrosAcc (bindInput [ ("a", "'xy'v")
                                               , ("b", "'za'V")
                                               , ("c", "'ab'V") ] coinput)
                                    (stringToKeyMacro "'c'V")))
         @?= "xyyzxyyzxyyxyyzxyyzxyyxyyzxyyzxyyxyyzxyyzxyy"
     , testCase "RepeatLast test 31" $
         snd (last (unwindMacrosAcc (bindInput [ ("a", "'xy'v")
                                               , ("b", "'za'v")
                                               , ("c", "'ab'V") ] coinput)
                                    (stringToKeyMacro "'c'V")))
         @?= "xyyzxyyxyyxyyzxyyxyyxyyzxyyxyyxyyzxyyxyy"
     , testCase "RepeatLast test 32" $
         snd (last (unwindMacrosAcc (bindInput [ ("a", "'xy'v")
                                               , ("b", "'za'v") ] coinput)
                                    (stringToKeyMacro "'ab'vv")))
         @?= "xyyzxyyxyyzxyyxyyzxyyxyy"
     , testCase "RepeatLast test 33" $
         snd (last (unwindMacrosAcc (bindInput [ ("a", "'xy'V") ] coinput)
                                    (stringToKeyMacro "a'za'vvv")))
         @?= "xyxyzxyxyxyxyxyxyxyxy"
     , testCase "RepeatLast test 34" $
         snd (last (unwindMacrosAcc (bindInput [ ("a", "'xy'v")
                                               , ("c", "a'za'Vv") ] coinput)
                                    (stringToKeyMacro "'c'v")))
         @?= "xyyzxyyzxyyzxyyxyyzxyyzxyyzxyy"
     , testCase "RepeatLast test 35" $
         snd (last (unwindMacrosAcc (bindInput [ ("a", "'xy'v")
                                               , ("b", "'za'V") ] coinput)
                                    (stringToKeyMacro "'ab'Vv")))
         @?= "xyyzxyyzxyyxyyzxyyzxyyxyyzxyyzxyy"
     , testCase "RepeatLast test 36" $
         snd (last (unwindMacrosAcc (bindInput [ ("a", "'xy'v")
                                               , ("b", "za'za'") ] coinput)
                                    (stringToKeyMacro "'ab'V'ab'V")))
         @?= "xyyzxyyzxyyxyyzxyyzxyyxyyzxyyzxyyxyyzxyyzxyy"
     , testCase "RepeatLast test 37" $
         snd (last (unwindMacrosAcc (bindInput [ ("b", "z'xy'vv")
                                               , ("c", "'xyvb'V") ] coinput)
                                    (stringToKeyMacro "'c'V")))
         @?= "xyyzxyyyxyyzxyyyxyyzxyyyxyyzxyyy"
     , testCase "RepeatLast test 38" $
         snd (last (unwindMacrosAcc coinput (stringToKeyMacro "'xv'V")))
         @?= "xxxx"
     , testCase "RepeatLast test 39" $
         fst <$> unwindMacros coinput (stringToKeyMacro "'xv'V")
         @?= [[(Right   "", "'xv'V",  "")],
              [(Left    "",  "xv'V",  "")],
              [(Left   "x",   "v'V", "x")],
              [(Left   "x",   "x'V", "x")],
              [(Left  "xx",    "'V", "x")],
              [(Right "xx",     "V", "x")],
              [(Right   "",    "xx",  ""), (Right "xx",    "", "V")],
              [(Right   "",     "x", "x"), (Right "xx",    "", "V")],
              [(Right "xx",      "", "V")]]
     , testCase "RepeatLast test 40" $
         snd (last (unwindMacrosAcc coinput (stringToKeyMacro "'xy'Vv")))
         @?= "xyxyxy"
     , testCase
         "RepeatLast test 41; named macros not referentially transparent" $
         snd (last (unwindMacrosAcc (bindInput [("a", "'xy'V")] coinput)
                                    (stringToKeyMacro "av")))
         @?= "xyxyxyxy"  -- because @a@ repeated; good!
     , testCase "RepeatLast test 42" $
         snd (last (unwindMacrosAcc (bindInput [("a", "xy")] coinput)
                                    (stringToKeyMacro "'a'Vv")))
         @?= "xyxyxy"  -- because @V@ repeated; good!
     , testCase "RepeatLast test 43" $
         snd (last (unwindMacrosAcc coinput (stringToKeyMacro "'xyV'V")))
         @?= "xyxy"
     , testCase "RepeatLast test 44" $
         snd (last (unwindMacrosAcc coinput (stringToKeyMacro "'xyV'v")))
         @?= "xyxy"
     , testCase "RepeatLast test 45" $
         snd (last (unwindMacrosAcc (bindInput [("a", "xyV")] coinput)
                                    (stringToKeyMacro "'a'V")))
         @?= "xyxy"
     , testCase "RepeatLast test 46" $
         snd (last (unwindMacrosAcc (bindInput [("a", "xyV")] coinput)
                                    (stringToKeyMacro "'a'v")))
         @?= "xyxy"
     ]

integrationTests :: TestTree
integrationTests = testGroup "integrationTests" $
  [ testCase "Null frontend; 5 frames" $ do
      let seed = "SMGen 131 141"
          args = words "--dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 5 --automateAll --keepAutomated --gameMode crawl"
                 ++ [ "--setDungeonRng", seed, "--setMainRng", seed]
      serverOptions <- handleParseResult $ execParserPure defaultPrefs serverOptionsPI args
      tieKnot serverOptions
  ]
  ++
  let corule = RK.makeData Content.RuleKind.standardRules
      uiOptions = unsafePerformIO $ mkUIOptions corule defClientOptions
      testFontset :: (Int, String) -> TestTree
      testFontset (n, fontsetName) =
        testCase ("SDL fronted; init only; " ++ fontsetName ++ " fontset") $
          when (frontendName == "sdl") $ do
            -- This test only works when run from the same directory that
            -- the .cabal files is in. And this is what Debian needs, so OK.
            -- The hacky log priority 0 tells SDL frontend to init
            -- and quit at once, for testing on CIs without graphics access.
            let seed = "SMGen " ++ show (13 + 2 * n) ++ " " ++ show (15 + 4 * n)
                args2 = words "--dbgMsgSer --logPriority 0 --newGame 3 --maxFps 100000 --benchmark --stopAfterFrames 5 --automateAll --keepAutomated --gameMode battle"
                        ++ [ "--setDungeonRng", seed, "--setMainRng", seed
                           , "--fontset", fontsetName ]
            serverOptions2 <- handleParseResult $ execParserPure defaultPrefs serverOptionsPI args2
            tieKnot serverOptions2
      isUbuntu = ("ubuntu" `T.isPrefixOf`)
  in map testFontset $ zip [0..] $ map T.unpack $ filter (not . isUbuntu)
     $ map fst $ uFontsets uiOptions
