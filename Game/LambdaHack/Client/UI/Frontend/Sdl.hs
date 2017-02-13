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
import qualified Data.EnumMap.Strict as EM
import Data.IORef
import qualified Data.Text as T
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)

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

type FontAtlas = EM.EnumMap Color.AttrCharW32 SDL.Texture

-- | Session data maintained by the frontend.
data FrontendSession = FrontendSession
  { swindow   :: !SDL.Window
  , srenderer :: !SDL.Renderer
  , sfont     :: !TTF.TTFFont
  , satlas    :: !(IORef FontAtlas)
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
           SDL.V2 (toEnum $ xsize * boxSize + 1) (toEnum $ ysize * boxSize + 1)}
  swindow <- SDL.createWindow title windowConfig
  srenderer <- SDL.createRenderer swindow (-1) SDL.defaultRenderer
  code <- TTF.init
  when (code /= 0) $ error $ "init of sdl2-ttf failed with: " ++ show code
  sfont <- TTF.openFont fontFile fontSize
  satlas <- newIORef EM.empty
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
  SDL.rendererDrawColor srenderer SDL.$= SDL.V4 0 0 0 0
  SDL.clear srenderer
  let lxsize = fst normalLevelBound + 1
      fontSize = fromMaybe 16 sfontSize
      boxSize = fontSize
      vp x y = Vect.P $ Vect.V2 (toEnum x) (toEnum y)
      drawHighlight x y color = do
        let v4 = let Raw.Color r g b a = colorToRGBA color
                 in SDL.V4 r g b a
        SDL.rendererDrawColor srenderer SDL.$= v4
        let bottomRight = vp ((x + 1) * boxSize - 1) ((y + 1) * boxSize)
            topRight = vp ((x + 1) * boxSize - 1) (y * boxSize + 2)
            bottomLeft = vp (x * boxSize + 1) ((y + 1) * boxSize)
            bottomRight1 = vp ((x + 1) * boxSize) ((y + 1) * boxSize + 1)
            topRight1 = vp ((x + 1) * boxSize) (y * boxSize + 2)
            bottomLeft1 = vp (x * boxSize + 1) ((y + 1) * boxSize + 1)
            v = G.fromList [ bottomRight, topRight, topRight1
                           , bottomRight1, bottomLeft1, bottomLeft
                           , bottomRight ]
        SDL.drawLines srenderer v
  let setChar :: Int -> Word32 -> IO ()
      setChar i w = do
        atlas <- readIORef satlas
        let (y, x) = i `divMod` lxsize
            acRaw = Color.AttrCharW32 w
            Color.AttrChar{acAttr=Color.Attr{bg=bgRaw, fg}, acChar} =
              Color.attrCharFromW32 acRaw
            normalizeBg color =
              (Color.Black, Color.attrChar2ToW32 fg acChar, Just color)
            (bg, ac, mlineColor) = case bgRaw of
              Color.BrRed ->  -- highlighted tile
                normalizeBg Color.Red
              Color.BrBlue ->  -- blue highlighted tile
                normalizeBg Color.Blue
              Color.BrYellow ->  -- yellow highlighted tile
                normalizeBg Color.BrYellow
              _ -> (bgRaw, acRaw, Nothing)
        textTexture <- case EM.lookup ac atlas of
          Nothing -> do
            -- https://github.com/rongcuid/sdl2-ttf/blob/master/src/SDL/TTF.hsc
            -- http://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf_frame.html
            textSurface <-
              TTF.renderUTF8Shaded sfont [acChar] (colorToRGBA fg)
                                                  (colorToRGBA bg)
            textTexture <- SDL.createTextureFromSurface srenderer textSurface
            SDL.freeSurface textSurface
            writeIORef satlas $ EM.insert ac textTexture atlas  -- not @acRaw@
            return textTexture
          Just textTexture -> do
            writeIORef satlas atlas
            return textTexture
        ti <- SDL.queryTexture textTexture
        let loc = SDL.Rectangle (vp (x * boxSize) (y * boxSize))
                                (Vect.V2 (SDL.textureWidth ti)
                                         (SDL.textureHeight ti))
        SDL.copy srenderer textTexture Nothing (Just loc)
        maybe (return ()) (drawHighlight x y) mlineColor
  U.imapM_ setChar (PointArray.avector singleFrame)
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
