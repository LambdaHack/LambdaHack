-- | Text frontend based on SDL2.
module Game.LambdaHack.Client.UI.Frontend.Sdl
  ( startup, frontendName
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , startupFun, shutdown, display
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude hiding (Alt)

import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.Char as Char
import qualified Data.Text as T

import qualified SDL as SDL
import SDL.Input.Keyboard.Codes
import qualified SDL.Raw as Raw
import qualified SDL.TTF as TTF
import qualified SDL.TTF.FFI as TTF (TTFFont)
import qualified SDL.Vect as Vect

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Frame
import Game.LambdaHack.Client.UI.Frontend.Common
import Game.LambdaHack.Common.ClientOptions
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { swindow   :: !SDL.Window
  , srenderer :: !SDL.Renderer
  , sfont     :: !TTF.TTFFont
  }

-- | The name of the frontend.
frontendName :: String
frontendName = "sdl"

-- | Set up and start the main loop providing input and output.
--
-- It seems, even on Windows, SDL2 doesn't require a bound thread.
-- so we can avoid the communication overhead of bound threads.
-- However, events can only be pumped in the thread that initialized
-- the video subsystem, so we need to enter the event-gathering loop
-- after the initialization and stay there.
startup :: DebugModeCli -> IO RawFrontend
startup sdebugCli = do
  rfMVar <- newEmptyMVar
  a <- async $ startupFun sdebugCli rfMVar
  link a
  takeMVar rfMVar

startupFun :: DebugModeCli -> MVar RawFrontend -> IO ()
startupFun sdebugCli@DebugModeCli{..} rfMVar = do
  SDL.initialize [SDL.InitVideo, SDL.InitEvents]
  let title = fromJust stitle
      fontFile = maybe "16x16x.fon" T.unpack sfontFamily
      fontSize = fromMaybe 16 sfontSize
      boxSize = fontSize
      xsize = fst normalLevelBound + 1
      ysize = snd normalLevelBound + 4
      windowConfig = SDL.defaultWindow
        {SDL.windowInitialSize =
           SDL.V2 (toEnum $ xsize * boxSize) (toEnum $ ysize * boxSize)}
  swindow <- SDL.createWindow title windowConfig
  srenderer <- SDL.createRenderer swindow (-1) SDL.defaultRenderer
  code <- TTF.init
  when (code /= 0) $ error $ "init of sdl2-ttf failed with: " ++ show code
  sfont <- TTF.openFont fontFile fontSize
  let sess = FrontendSession{..}
  rf <- createRawFrontend (display sdebugCli sess) (shutdown sess)
  putMVar rfMVar rf
  let pointTranslate (SDL.P (SDL.V2 x y)) = Point (fromEnum x) (fromEnum y)
      storeKeys :: IO ()
      storeKeys = do
        e <- SDL.waitEvent  -- blocks here, so no polling
        case SDL.eventPayload e of
          SDL.KeyboardEvent keyboardEvent
            | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed -> do
              let sym = SDL.keyboardEventKeysym keyboardEvent
                  md = modTranslate $ SDL.keysymModifier sym
                  key = keyTranslate md $ SDL.keysymKeycode sym
                  modifier = if md == K.Shift then K.NoModifier else md
              p <- SDL.getAbsoluteMouseLocation
              when (key == K.Esc) $ resetChanKey (fchanKey rf)
              saveKMP rf modifier key (pointTranslate p)
          SDL.WindowClosedEvent{} -> shutdown sess
          SDL.QuitEvent -> shutdown sess
          _ -> return ()
        storeKeys
  storeKeys

shutdown :: FrontendSession -> IO ()
shutdown FrontendSession{..} = do
  SDL.destroyRenderer srenderer
  SDL.destroyWindow swindow
  TTF.closeFont sfont
  TTF.quit
  SDL.quit

-- | Add a frame to be drawn.
display :: DebugModeCli
        -> FrontendSession  -- ^ frontend session data
        -> SingleFrame      -- ^ the screen frame to draw
        -> IO ()
display DebugModeCli{..} FrontendSession{..} SingleFrame{singleFrame} = do
  SDL.clear srenderer
  let level = chunk $ map Color.attrCharFromW32
                    $ PointArray.toListA singleFrame
      nm = zip [0..] $ map (zip [0..]) level
      lxsize = fst normalLevelBound + 1
      fontSize = fromMaybe 16 sfontSize
      boxSize = fontSize
      chunk [] = []
      chunk l = let (ch, r) = splitAt lxsize l
                in ch : chunk r
      render x y Color.AttrChar{acAttr=Color.Attr{..}, ..} = do
        textSurface <-
          TTF.renderUTF8Shaded sfont [acChar] (colorToRGBA fg) (colorToRGBA bg)
        Vect.V2 vx vy <- SDL.surfaceDimensions textSurface
        textTexture <- SDL.createTextureFromSurface srenderer textSurface
        SDL.freeSurface textSurface
        let loc = SDL.Rectangle (Vect.P $ Vect.V2 (toEnum $ x * boxSize)
                                                  (toEnum $ y * boxSize))
                                (Vect.V2 vx vy)
        SDL.copy srenderer textTexture Nothing (Just loc)
        SDL.destroyTexture textTexture
  sequence_ [render x y ac | (y, line) <- nm, (x, ac) <- line]
  SDL.present srenderer

-- | Translates modifiers to our own encoding.
modTranslate :: SDL.KeyModifier -> K.Modifier
modTranslate m =
  modifierTranslate
    (SDL.keyModifierLeftCtrl m || SDL.keyModifierRightCtrl m)
    (SDL.keyModifierLeftShift m || SDL.keyModifierRightShift m)
    (SDL.keyModifierLeftAlt m
     || SDL.keyModifierRightAlt m
     || SDL.keyModifierAltGr m)
    False

keyTranslate :: K.Modifier -> SDL.Keycode -> K.Key
keyTranslate md n =
  case n of
    KeycodeEscape     -> K.Esc
    KeycodeReturn     -> K.Return
    KeycodeBackspace  -> K.BackSpace
    KeycodeTab        -> if md == K.Shift then K.BackTab else K.Tab
    KeycodeSpace      -> K.Space
    KeycodeExclaim -> K.Char '!'
    KeycodeQuoteDbl -> K.Char '"'
    KeycodeHash -> K.Char '#'
    KeycodePercent -> K.Char '%'
    KeycodeDollar -> K.Char '$'
    KeycodeAmpersand -> K.Char '&'
    KeycodeQuote -> if md == K.Shift then K.Char '"' else K.Char '\''
    KeycodeLeftParen -> K.Char '('
    KeycodeRightParen -> K.Char ')'
    KeycodeAsterisk -> K.Char '*'
    KeycodePlus -> K.Char '+'
    KeycodeComma -> if md == K.Shift then K.Char '<' else K.Char ','
    KeycodeMinus -> if md == K.Shift then K.Char '_' else K.Char '-'
    KeycodePeriod -> if md == K.Shift then K.Char '>' else K.Char '.'
    KeycodeSlash -> if md == K.Shift then K.Char '?' else K.Char '/'
    Keycode1 -> if md == K.Shift then K.Char '!' else K.Char '1'
    Keycode2 -> if md == K.Shift then K.Char '@' else K.Char '2'
    Keycode3 -> if md == K.Shift then K.Char '#' else K.Char '3'
    Keycode4 -> if md == K.Shift then K.Char '$' else K.Char '4'
    Keycode5 -> if md == K.Shift then K.Char '%' else K.Char '5'
    Keycode6 -> if md == K.Shift then K.Char '^' else K.Char '6'
    Keycode7 -> if md == K.Shift then K.Char '&' else K.Char '7'
    Keycode8 -> if md == K.Shift then K.Char '*' else K.Char '8'
    Keycode9 -> if md == K.Shift then K.Char '(' else K.Char '9'
    Keycode0 -> if md == K.Shift then K.Char ')' else K.Char '0'
    KeycodeColon -> K.Char ':'
    KeycodeSemicolon -> if md == K.Shift then K.Char ':' else K.Char ';'
    KeycodeLess -> K.Char '<'
    KeycodeEquals -> if md == K.Shift then K.Char '+' else K.Char '='
    KeycodeGreater -> K.Char '>'
    KeycodeQuestion -> K.Char '?'
    KeycodeAt -> K.Char '@'
    KeycodeLeftBracket -> if md == K.Shift then K.Char '{' else K.Char '['
    KeycodeBackslash -> if md == K.Shift then K.Char '|' else K.Char '\\'
    KeycodeRightBracket -> if md == K.Shift then K.Char '}' else K.Char ']'
    KeycodeCaret -> K.Char '^'
    KeycodeUnderscore -> K.Char '_'
    KeycodeBackquote -> if md == K.Shift then K.Char '~' else K.Char '`'
    KeycodeUp         -> K.Up
    KeycodeDown       -> K.Down
    KeycodeLeft       -> K.Left
    KeycodeRight      -> K.Right
    KeycodeHome       -> K.Home
    KeycodeEnd        -> K.End
    KeycodePageUp     -> K.PgUp
    KeycodePageDown   -> K.PgDn
    KeycodeInsert     -> K.Insert
    KeycodeDelete     -> K.Delete
    KeycodeKPDivide   -> K.KP '/'
    KeycodeKPMultiply -> K.KP '*'
    KeycodeKPMinus    -> K.Char '-'  -- KP and normal are merged here
    KeycodeKPPlus     -> K.Char '+'  -- KP and normal are merged here
    KeycodeKPEnter    -> K.Return
    KeycodeKPEquals   -> K.Return  -- in case of some funny layouts
    KeycodeKP1 -> if md == K.Shift then K.KP '1' else K.End
    KeycodeKP2 -> if md == K.Shift then K.KP '2' else K.Down
    KeycodeKP3 -> if md == K.Shift then K.KP '3' else K.PgDn
    KeycodeKP4 -> if md == K.Shift then K.KP '4' else K.Left
    KeycodeKP5 -> if md == K.Shift then K.KP '5' else K.Begin
    KeycodeKP6 -> if md == K.Shift then K.KP '6' else K.Right
    KeycodeKP7 -> if md == K.Shift then K.KP '7' else K.Home
    KeycodeKP8 -> if md == K.Shift then K.KP '8' else K.Up
    KeycodeKP9 -> if md == K.Shift then K.KP '9' else K.PgUp
    KeycodeKP0 -> if md == K.Shift then K.KP '0' else K.Insert
    KeycodeKPPeriod -> K.Char '.'  -- KP and normal are merged here
    KeycodeKPComma  -> K.Char '.'  -- in case of some funny layouts
    KeycodeF1       -> K.Fun 1
    KeycodeF2       -> K.Fun 2
    KeycodeF3       -> K.Fun 3
    KeycodeF4       -> K.Fun 4
    KeycodeF5       -> K.Fun 5
    KeycodeF6       -> K.Fun 6
    KeycodeF7       -> K.Fun 7
    KeycodeF8       -> K.Fun 8
    KeycodeF9       -> K.Fun 9
    KeycodeF10      -> K.Fun 10
    KeycodeF11      -> K.Fun 11
    KeycodeF12      -> K.Fun 12
    KeycodeLCtrl    -> K.DeadKey
    KeycodeLShift   -> K.DeadKey
    KeycodeLAlt     -> K.DeadKey
    KeycodeLGUI     -> K.DeadKey
    KeycodeRCtrl    -> K.DeadKey
    KeycodeRShift   -> K.DeadKey
    KeycodeRAlt     -> K.DeadKey
    KeycodeRGUI     -> K.DeadKey
    KeycodeMode     -> K.DeadKey
    KeycodeUnknown  -> K.Unknown "KeycodeUnknown"
    _ -> let i = fromEnum $ unwrapKeycode n
         in if | 97 <= i && i <= 122
                 && md == K.Shift -> K.Char $ Char.chr $ i - 32
               | 32 <= i && i <= 126 -> K.Char $ Char.chr i
               | otherwise -> K.Unknown $ show n

colorToRGBA :: Color.Color -> Raw.Color
colorToRGBA Color.Black     = Raw.Color 0 0 0 0
colorToRGBA Color.Red       = Raw.Color 0xD5 0x00 0x00 0
colorToRGBA Color.Green     = Raw.Color 0x00 0xAA 0x00 0
colorToRGBA Color.Brown     = Raw.Color 0xAA 0x55 0x00 0
colorToRGBA Color.Blue      = Raw.Color 0x20 0x3A 0xF0 0
colorToRGBA Color.Magenta   = Raw.Color 0xAA 0x00 0xAA 0
colorToRGBA Color.Cyan      = Raw.Color 0x00 0xAA 0xAA 0
colorToRGBA Color.White     = Raw.Color 0xC5 0xBC 0xB8 0
colorToRGBA Color.BrBlack   = Raw.Color 0x6F 0x5F 0x5F 0
colorToRGBA Color.BrRed     = Raw.Color 0xFF 0x55 0x55 0
colorToRGBA Color.BrGreen   = Raw.Color 0x75 0xFF 0x45 0
colorToRGBA Color.BrYellow  = Raw.Color 0xFF 0xE8 0x55 0
colorToRGBA Color.BrBlue    = Raw.Color 0x40 0x90 0xFF 0
colorToRGBA Color.BrMagenta = Raw.Color 0xFF 0x77 0xFF 0
colorToRGBA Color.BrCyan    = Raw.Color 0x60 0xFF 0xF0 0
colorToRGBA Color.BrWhite   = Raw.Color 0xFF 0xFF 0xFF 0
