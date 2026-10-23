-- | Tests for the decisions of InputDecision, and for
-- the web key-translation table, which feeds the browser
-- frontend's decisions and which the DeadKey bug class lived in
-- (@docs/wasm-frontend-unified-plan.md@, 0.1).
module InputDecisionUnitTests (inputDecisionUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import           Game.LambdaHack.Client.UI.Frontend
  (KeyDecision (..), decideKey, decideMouseButton, decideWheel)
import qualified Game.LambdaHack.Client.UI.Key as K

inputDecisionUnitTests :: TestTree
inputDecisionUnitTests = testGroup "inputDecisionUnitTests"
  [ testGroup "decideKey squashes Shift out of the modifier"
      [ testCase (show modifier) $
          kdModifier (decideKey modifier (K.Char 'a')) @?= squashed
      | (modifier, squashed) <-
          [ (K.NoModifier, K.NoModifier)
          , (K.Shift, K.NoModifier)
          , (K.Control, K.Control)
          , (K.ControlShift, K.Control)
          , (K.Alt, K.Alt)
          , (K.AltShift, K.Alt)
          ]
      ]
  , testGroup "decideKey leaves the browser a bare Control chord of"
      [ testCase [c] $ do
          kdPreventBrowser (decideKey K.Control (K.Char c)) @?= False
          kdPreventBrowser (decideKey K.NoModifier (K.Char c)) @?= True
          kdPreventBrowser (decideKey K.ControlShift (K.Char c)) @?= True
      | c <- "+-0"
      ]
  , testCase "decideKey keeps other Control chords from the browser" $ do
      kdPreventBrowser (decideKey K.Control (K.Char 'a')) @?= True
      kdPreventBrowser (decideKey K.Control (K.Char 't')) @?= True
      kdPreventBrowser (decideKey K.Control K.Tab) @?= True
  , testCase "decideKey keeps Tab from the browser" $
      kdPreventBrowser (decideKey K.NoModifier K.Tab) @?= True
  , testCase "decideKey leaves the browser Alt chords, not Alt-Shift ones" $ do
      kdPreventBrowser (decideKey K.Alt (K.Char 'a')) @?= False
      kdPreventBrowser (decideKey K.AltShift (K.Char 'a')) @?= True
  , testCase "decideKey leaves the browser a dead key, under any modifier" $
      forM_ [minBound .. maxBound] $ \modifier ->
        kdPreventBrowser (decideKey modifier K.DeadKey) @?= False
  , testCase "decideKey empties the key queue on Esc and no other key" $ do
      forM_ [minBound .. maxBound] $ \modifier ->
        kdResetChan (decideKey modifier K.Esc) @?= True
      kdResetChan (decideKey K.NoModifier K.Return) @?= False
      kdResetChan (decideKey K.Control (K.Char 'c')) @?= False
  , testCase "decideWheel turns a delta beyond 0.01 into the wheel's key" $
      forM_ [ (-100, Just K.WheelNorth), (-0.02, Just K.WheelNorth)
            , (-0.01, Nothing), (0, Nothing), (0.01, Nothing)
            , (0.02, Just K.WheelSouth), (100, Just K.WheelSouth) ] $
        \(deltaY, expected) ->
          assertEqual ("deltaY " ++ show deltaY) expected (decideWheel deltaY)
  , testCase "decideMouseButton reads DOM button codes, any other as left" $
      forM_ [ (0, K.LeftButtonRelease), (1, K.MiddleButtonRelease)
            , (2, K.RightButtonRelease), (3, K.LeftButtonRelease)
            , (4, K.LeftButtonRelease) ] $
        \(button, expected) ->
          assertEqual ("button " ++ show button)
                      expected (decideMouseButton button)
  , testGroup "keyTranslateWeb"
      [ testCase (show s ++ (if shift then " with Shift" else "")) $
          K.keyTranslateWeb s shift @?= expected
      | (s, shift, expected) <- keyTranslateWebTable
      ]
  ]

-- | @KeyboardEvent.key@ values, the flag 'K.keyTranslateWeb' takes for
-- Shift held alone, and the key each translates to.
keyTranslateWebTable :: [(String, Bool, K.Key)]
keyTranslateWebTable =
  [ ("Tab", False, K.Tab)
  , ("Tab", True, K.BackTab)
  , ("Escape", False, K.Esc)
  , ("Esc", False, K.Esc)
  , ("Enter", False, K.Return)
  , (" ", False, K.Space)
  , ("Backspace", False, K.BackSpace)
  , ("ArrowUp", False, K.Up)
  , ("ArrowUp", True, K.KP '8')
  , ("Home", False, K.Home)
  , ("Home", True, K.KP '7')
  , ("Clear", False, K.Begin)
  , ("Clear", True, K.KP '5')
  , ("1", False, K.Char '1')
  , ("1", True, K.KP '1')
  , ("F1", False, K.Fun 1)
  , ("F12", False, K.Fun 12)
  , ("/", False, K.Char '/')
  , ("/", True, K.Char '?')
  , ("+", False, K.Char '+')
  , ("Add", False, K.Char '+')
  , ("a", False, K.Char 'a')
  , ("A", True, K.Char 'A')
  , ("\x00A7", False, K.Char '`')
  , ("\x00A7", True, K.Char '\x00A7')
  , ("Unidentified", False, K.Begin)
  , ("NoSuchKey", False, K.Unknown "NoSuchKey")
  ]
  ++ [ (s, shift, K.DeadKey)
     | s <- [ "Dead", "Shift", "Control", "Meta", "Menu", "ContextMenu"
            , "Alt", "AltGraph", "Num_Lock", "NumLock", "Caps_Lock"
            , "CapsLock", "Win" ]
     , shift <- [False, True] ]
