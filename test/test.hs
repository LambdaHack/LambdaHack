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
         last (unwindMacros (bindInput [ ("a", "'bc'V")
                                       , ("c", "'aaa'V") ] coinput)
                 (stringToKeyMacro "a"))
         @?= macroLooped
     , testCase "Macro 2 from Issue#189 description" $
         snd (last (unwindMacros (bindInput [("a", "'x'")] coinput)
                                 (stringToKeyMacro "'a'")))
         @?= "x"
     , testCase "Macro 3 from Issue#189 description" $
         snd (last (unwindMacros coinput (stringToKeyMacro "'x''x'")))
         @?= "xx"
     , testCase "Macro 4 from Issue#189 description" $
         snd (last (unwindMacros coinput (stringToKeyMacro "'x''x'V")))
         @?= "xxx"
     , testCase "Macro 5 from Issue#189 description" $
         snd (last (unwindMacros coinput (stringToKeyMacro "x'x'V")))
         @?= "xxx"
     , testCase "Macro test 10" $
         snd (last (unwindMacros coinput (stringToKeyMacro "x'y'V")))
         @?= "xyy"
     , testCase "Macro test 11" $
         snd (last (unwindMacros coinput (stringToKeyMacro "'x''y'V")))
         @?= "xyy"
     , testCase "Macro test 12" $
         snd (last (unwindMacros coinput
                                 (listToKeyMacro ["x", "C-V"])))
         @?= "x"
     , testCase "Macro test 13" $
         snd (last (unwindMacros coinput
                                 (listToKeyMacro ["'", "x", "'", "C-V"])))
         @?= "xxxxxxxxxxxxxxxxxxxxxxxxxx"
     , testCase "Macro test 14" $
         snd (last (unwindMacros coinput
                                 (listToKeyMacro ["'", "x", "'", "y", "C-V"])))
         @?= "xyxxxxxxxxxxxxxxxxxxxxxxxxx"
     , testCase "Macro test 15" $
         snd (last (unwindMacros (bindInput [("a", "x")] coinput)
                                 (stringToKeyMacro "'a'V")))
         @?= "xx"
     , testCase "Macro test 16" $
         snd (last (unwindMacros (bindInput [("a", "'x'")] coinput)
                                 (stringToKeyMacro "'a'V")))
         @?= "xx"
     , testCase "Macro test 17" $
         snd (last (unwindMacros (bindInput [("a", "'x'V")] coinput)
                                 (stringToKeyMacro "a")))
         @?= "xx"
     , testCase "Macro test 18" $
         snd (last (unwindMacros (bindInput [("a", "'x'V")] coinput)
                                 (stringToKeyMacro "'a'")))
         @?= "xx"
     , testCase "Macro test 19" $
         snd (last (unwindMacros (bindInput [("a", "'x'V")] coinput)
                                 (stringToKeyMacro "'a'V")))
         @?= "xxxx"
     , testCase "Macro test 20" $
         snd (last (unwindMacros (bindInput [ ("a", "'bz'V")
                                            , ("c", "'aaa'V") ] coinput)
                                 (stringToKeyMacro "c")))
         @?= "bzbzbzbzbzbzbzbzbzbzbzbz"
     , testCase "RepeatLast test 10" $
         snd (last (unwindMacros coinput (stringToKeyMacro "x'y'v")))
         @?= "xyy"
     , testCase "RepeatLast test 11" $
         snd (last (unwindMacros coinput (stringToKeyMacro "'x'yv")))
         @?= "xyy"
     , testCase "RepeatLast test 12" $
         snd (last (unwindMacros coinput
                                 (listToKeyMacro ["v", "C-v"])))
         @?= ""
     , testCase "RepeatLast test 13" $
         snd (last (unwindMacros coinput
                                 (listToKeyMacro ["'", "x", "'", "C-v"])))
         @?= "xxxxxxxxxxxxxxxxxxxxxxxxxx"
     , testCase "RepeatLast test 14" $
         snd (last (unwindMacros coinput
                                 (listToKeyMacro ["'", "x", "'", "V", "C-v"])))
         @?= "xxxxxxxxxxxxxxxxxxxxxxxxxxx"
     , testCase "RepeatLast test 15" $
         snd (last (unwindMacros (bindInput [("a", "x")] coinput)
                                 (stringToKeyMacro "av")))
         @?= "xx"
     , testCase "RepeatLast test 16" $
         snd (last (unwindMacros (bindInput [("a", "'x'")] coinput)
                                 (stringToKeyMacro "'a'v")))
         @?= "xx"
     , testCase "RepeatLast test 17" $
         snd (last (unwindMacros (bindInput [("a", "'x'v")] coinput)
                                 (stringToKeyMacro "a")))
         @?= "xx"
     , testCase "RepeatLast test 18" $
         snd (last (unwindMacros (bindInput [("a", "'x'v")] coinput)
                                 (stringToKeyMacro "'a'")))
         @?= "xx"
     , testCase "RepeatLast test 19" $
         snd (last (unwindMacros (bindInput [("a", "'x'v")] coinput)
                                 (stringToKeyMacro "'a'v")))
         @?= "xxxx"
     , testCase "RepeatLast test 20" $
         snd (last (unwindMacros (bindInput [ ("a", "'bz'v")
                                            , ("c", "'aaa'v") ] coinput)
                                 (stringToKeyMacro "c")))
         @?= "bzzbzzbzzbzz"
     , testCase "RepeatLast test 21" $
         snd (last (unwindMacros (bindInput [("a", "'x'V")] coinput)
                                 (stringToKeyMacro "'a'v")))
         @?= "xxxx"
     , testCase "RepeatLast test 22" $
         snd (last (unwindMacros (bindInput [("a", "'xy'V")] coinput)
                                 (stringToKeyMacro "'aa'v")))
         @?= "xyxyxyxyxyxy"
     , testCase "RepeatLast test 23" $
         snd (last (unwindMacros (bindInput [("a", "'xy'v")] coinput)
                                 (stringToKeyMacro "'aa'V")))
         @?= "xyyxyyxyyxyy"
     , testCase "RepeatLast test 24" $
         snd (last (unwindMacros (bindInput [("a", "'xy'vv")] coinput)
                                 (stringToKeyMacro "'aa'vv")))
         @?= "xyyyxyyyxyyyxyyy"
     , testCase "RepeatLast test 25" $
         snd (last (unwindMacros (bindInput [("a", "'xyv'v")] coinput)
                                 (stringToKeyMacro "'a'a'vv'")))
         @?= "xyyyxyyyxyyyxyyy"
     , testCase "RepeatLast test 26" $
         snd (last (unwindMacros (bindInput [ ("a", "'xy'v")
                                            , ("b", "'za'v")
                                            , ("c", "'ab'v") ] coinput)
                                 (stringToKeyMacro "'c'v")))
         @?= "xyyzxyyxyyzxyyxyyxyyzxyyxyyzxyyxyy"
     , testCase "RepeatLast test 27" $
         snd (last (unwindMacros (bindInput [ ("a", "'xy'V")
                                            , ("b", "'za'v")
                                            , ("c", "'ab'v") ] coinput)
                                 (stringToKeyMacro "'c'v")))
         @?= "xyxyzxyxyxyxyzxyxyxyxyxyxyzxyxyxyxyzxyxyxyxy"
     , testCase "RepeatLast test 28" $
         snd (last (unwindMacros (bindInput [ ("a", "'xy'v")
                                            , ("b", "'za'V")
                                            , ("c", "'ab'v") ] coinput)
                                 (stringToKeyMacro "'c'v")))
         @?= "xyyzxyyzxyyzxyyzxyyxyyzxyyzxyyzxyyzxyy"
     , testCase "RepeatLast test 29" $
         snd (last (unwindMacros (bindInput [ ("a", "'xy'v")
                                            , ("b", "'za'V")
                                            , ("c", "'ab'V") ] coinput)
                                 (stringToKeyMacro "'c'v")))
         @?= "xyyzxyyzxyyxyyzxyyzxyyxyyzxyyzxyyxyyzxyyzxyy"
     , testCase "RepeatLast test 30" $
         snd (last (unwindMacros (bindInput [ ("a", "'xy'v")
                                            , ("b", "'za'V")
                                            , ("c", "'ab'V") ] coinput)
                                 (stringToKeyMacro "'c'V")))
         @?= "xyyzxyyzxyyxyyzxyyzxyyxyyzxyyzxyyxyyzxyyzxyy"
     -- , testCase "RepeatLast test 31" $
     --     snd (last (unwindMacros (bindInput [ ("a", "'xy'v")
     --                                        , ("b", "'za'v")
     --                                        , ("c", "'ab'V") ] coinput)
     --                             (stringToKeyMacro "'c'V")))
     --     @?= "xyyzxyyxyyzxyyxyyzxyyxyyzxyy"
     -- , testCase "RepeatLast test 32" $
     --     snd (last (unwindMacros (bindInput [ ("a", "'xy'v")
     --                                        , ("b", "'za'v") ] coinput)
     --                             (stringToKeyMacro "'ab'vv")))
     --     @?= "xyyzxyyxyyzxyyxyyxyyzxyyxyyzxyyxyy"
     -- , testCase "RepeatLast test 33" $
     --     snd (last (unwindMacros (bindInput [ ("a", "'xy'V") ] coinput)
     --                             (stringToKeyMacro "a'za'vvv")))
     --     @?= "xyxyzxyxyxyxyzxyxyxyxyxyxyzxyxyxyxyzxyxyxyxy"
     -- , testCase "RepeatLast test 34" $
     --     snd (last (unwindMacros (bindInput [ ("a", "'xy'v")
     --                                        , ("c", "a'za'Vv") ] coinput)
     --                             (stringToKeyMacro "'c'v")))
     --     @?= "xyyzxyyzxyyzxyyzxyyxyyzxyyzxyyzxyyzxyy"
     -- , testCase "RepeatLast test 35" $
     --     snd (last (unwindMacros (bindInput [ ("a", "'xy'v")
     --                                        , ("b", "'za'V") ] coinput)
     --                             (stringToKeyMacro "'ab'Vv")))
     --     @?= "xyyzxyyzxyyxyyzxyyzxyyxyyzxyyzxyyxyyzxyyzxyy"
     , testCase "RepeatLast test 36" $
         snd (last (unwindMacros (bindInput [ ("a", "'xy'v")
                                            , ("b", "za'za'") ] coinput)
                                 (stringToKeyMacro "'ab'V'ab'V")))
         @?= "xyyzxyyzxyyxyyzxyyzxyyxyyzxyyzxyyxyyzxyyzxyy"
     -- , testCase "RepeatLast test 37" $
     --     snd (last (unwindMacros (bindInput [ ("b", "z'xy'vv")
     --                                        , ("c", "'xyvb'V") ] coinput)
     --                             (stringToKeyMacro "'c'V")))
     --     @?= "xyyzxyyxyyzxyyxyyzxyyxyyzxyy"
     ]

type BufferTrace = [(Either String String, String, String)]
type ActionLog = String

macroLooped :: (BufferTrace, String)
macroLooped = ([(Left mempty, [], [])], "macro looped")

-- The mock for macro testing.
unwindMacros :: IC.InputContent -> KeyMacro -> [(BufferTrace, ActionLog)]
unwindMacros IC.InputContent{bcmdMap, brevMap} startMacro =
  let transitionMacros :: Int -> [K.KM] -> [ActionBuffer]
                       -> [(BufferTrace, ActionLog)]
      transitionMacros (0 :: Int) _ _ = [macroLooped]  -- probably
      transitionMacros _ _ [] = error "bad initial conditions"
      transitionMacros k out abuffs@(abuff : abuffRest) =
        storeTrace abuffs out : case slastPlay abuff of
        KeyMacro [] -> if null abuffRest
                       then []
                       else transitionMacros (k - 1) out abuffRest
        KeyMacro (km : kms) -> case km `M.lookup` bcmdMap of
          Nothing -> error "unwindMacros: not a command"
          Just (_, _, cmd) ->
            let abuffs0 = addToMacro brevMap km abuffs

                abuffs1 = updateLastAction km cmd abuffs0

                abuffs2 =
                  let abuff1 = head abuffs1
                  in abuff1 { slastPlay = KeyMacro kms } : tail abuffs1

                (abuffs3, out') = case cmd of
                  HumanCmd.Record ->
                    (fst $ recordHumanTransition abuffs2, out)
                  HumanCmd.Macro ys ->
                    (macroHumanTransition ys abuffs2, out)
                  HumanCmd.Repeat n ->
                    (repeatHumanTransition n abuffs2, out)
                  HumanCmd.RepeatLast n ->
                    (repeatLastHumanTransition n abuffs2, out)
                  _ -> (abuffs2, out ++ [km])

                abuffs4 = case abuffs3 of
                  ActionBuffer _ (KeyMacro []) _ : as | not (null as) -> as
                  _ -> abuffs3

            in transitionMacros (k - 1) out' abuffs4

      storeTrace :: [ActionBuffer] -> [K.KM] -> (BufferTrace, ActionLog)
      storeTrace abuffs out =
        let tmacros = bimap (concatMap K.showKM)
                            (concatMap K.showKM . unKeyMacro)
                    . smacroBuffer <$> abuffs
            tlastPlay = concatMap K.showKM . unKeyMacro . slastPlay <$> abuffs
            tlastAction = maybe "" K.showKM . slastAction <$> abuffs
            toutput = concatMap K.showKM out
        in (zip3 tmacros tlastPlay tlastAction, toutput)

      emptyBuffer = ActionBuffer { smacroBuffer = Right mempty
                                 , slastPlay = mempty
                                 , slastAction = Nothing }

  in transitionMacros 1000 [] [emptyBuffer { slastPlay = startMacro }]

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
