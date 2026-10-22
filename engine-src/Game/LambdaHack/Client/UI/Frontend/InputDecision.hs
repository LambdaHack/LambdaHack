-- | The input decisions made once for the SDL and browser frontends: given
-- a key press already translated, the modifier it is sent with, whether it
-- empties the key queue and whether the browser keeps its default action;
-- and, for the browser alone, the key a DOM wheel delta or button code
-- stands for.
module Game.LambdaHack.Client.UI.Frontend.InputDecision
  ( KeyDecision(..), decideKey, decideWheel, decideMouseButton
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Game.LambdaHack.Client.UI.Key as K

-- | What a frontend does with a key press.
data KeyDecision = KeyDecision
  { kdModifier       :: K.Modifier  -- ^ squashed modifier to actually send
  , kdResetChan      :: Bool        -- ^ True for Esc
  , kdPreventBrowser :: Bool        -- ^ True unless an allowed passthrough
  }
  deriving (Show, Eq)

-- No key field: nothing here remaps a key, and both consumers translate it
-- before they ask (Sdl.hs's @keyTranslate@ of @SDL.keysymKeycode@,
-- and @keyTranslateWeb@ in Wasm.hs's @lhKey@), so each already holds
-- *a* translated key. That the two are not the same key under a compound
-- modifier is not closed here: Sdl.hs translates under the raw left/right
-- shift bits, independent of Ctrl and Alt, while @lhKey@ passes
-- @modifier == K.Shift@, False whenever Ctrl or Alt is also down,
-- so @C-S-<key>@ translates shifted natively and unshifted in the browser.
-- The flag is consumed upstream of 'decideKey', so closing that divergence
-- means deciding the shift flag here too, which this signature does not;
-- the audit note of 2026-09-07 in docs/wasm-frontend-unified-plan.md,
-- Appendix C, records the behaviour. Dead keys reach only
-- 'kdPreventBrowser'; if a rule ever does remap, the field returns and this
-- comment goes.
--
-- Nor a pointer field: the position a key press carries is each frontend's
-- own, SDL sending the mouse position with every key and the browser
-- @PointUI 0 0@, and every key overwrites the session's @spointer@ with
-- it, so a key binding for a @*WithPointer@ or @ByArea@ command would
-- behave differently in the browser (docs/wasm-frontend-unified-plan.md,
-- Appendix C).

-- | Decide a key press, given the modifier held, unsquashed, and the key
-- as the frontend translated it.
decideKey :: K.Modifier -> K.Key -> KeyDecision
decideKey modifier key = KeyDecision
  { kdModifier = case modifier of  -- to prevent S-!, etc.
      K.Shift -> K.NoModifier
      K.ControlShift -> K.Control
      K.AltShift -> K.Alt
      _ -> modifier
  , kdResetChan = key == K.Esc
    -- Pass through C-+ and others, but disable special behaviour on Tab, etc.
  , kdPreventBrowser =
      not $ modifier == K.Alt
            || modifier == K.Control && key `elem` map K.Char browserKeys
            || key == K.DeadKey  -- NumLock in particular
  }

-- | The keys whose bare Control chords the browser keeps: those of zoom.
browserKeys :: String
browserKeys = "+-0"

-- | Decide a wheel turn, given the DOM @WheelEvent.deltaY@. Only a delta
-- beyond a small epsilon counts, to filter out zero-delta glitches.
decideWheel :: Double -> Maybe K.Key
decideWheel deltaY
  | deltaY < -0.01 = Just K.WheelNorth
  | deltaY > 0.01 = Just K.WheelSouth
  | otherwise = Nothing  -- probably a glitch

-- | Decide a mouse button release, given the DOM @MouseEvent.button@ code.
decideMouseButton :: Int -> K.Key
decideMouseButton button = case button of
  0 -> K.LeftButtonRelease
  1 -> K.MiddleButtonRelease
  2 -> K.RightButtonRelease  -- not handled in contextmenu
  _ -> K.LeftButtonRelease  -- any other is alternate left
