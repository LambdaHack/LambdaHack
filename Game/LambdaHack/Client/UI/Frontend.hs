{-# LANGUAGE GADTs, KindSignatures, RankNTypes #-}
-- | Display game data on the screen and receive user input
-- using one of the available raw frontends and derived operations.
module Game.LambdaHack.Client.UI.Frontend
  ( -- * Connection types
    FrontReq(..), ChanFrontend
    -- * Re-exported part of the raw frontend
  , startupF, frontendName
    -- * Derived operations
  , chanFrontend
  ) where

import qualified Data.Char as Char
import Data.IORef
import Data.Text (Text)

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Animation
import Game.LambdaHack.Client.UI.Frontend.Chosen

-- | The instructions sent by clients to the raw frontend.
data FrontReq :: * -> * where
  FrontFrame :: {frontFrame :: !SingleFrame} -> FrontReq ()
    -- ^ show a frame
  FrontDelay :: FrontReq ()
    -- ^ perform a single explicit delay
  FrontKey :: { frontKeyKeys  :: ![K.KM]
              , frontKeyFrame :: !SingleFrame } -> FrontReq K.KM
    -- ^ flush frames, display a frame and ask for a keypress
  FrontInt :: { frontIntKeys  :: ![K.KM]
              , frontIntFrame :: !SingleFrame
              , frontIntMax   :: Int } -> FrontReq (Either K.KM Int)
    -- ^ flush frames, display a frame and ask for a number
  FrontText :: { frontTextKeys  :: ![K.KM]
               , frontTextFrame :: !SingleFrame } -> FrontReq (Either K.KM Text)
    -- ^ flush frames, display a frame and ask for a number
  FrontSync :: FrontReq ()
    -- ^ flush frames
  FrontAutoYes :: !Bool -> FrontReq ()
    -- ^ set in the frontend that it should auto-answer prompts

-- | Connection channel between a frontend and a client. Frontend acts
-- as a server, serving keys, etc., when given frames to display.
type ChanFrontend = forall a. FrontReq a -> IO a

-- | Display a prompt, wait for any of the specified keys (for any key,
-- if the list is empty). Repeat if an unexpected key received.
promptGetKey :: RawFrontend -> [K.KM] -> SingleFrame -> IO K.KM
promptGetKey fs [] frame = fpromptGetKey fs frame
promptGetKey fs keys frame = do
  autoYes <- readIORef (fautoYesRef fs)
  if autoYes && K.spaceKM `elem` keys then do
    fdisplay fs (Just frame)
    return K.spaceKM
  else do
    km <- fpromptGetKey fs frame
    if km{K.pointer=Nothing} `elem` keys
      then return km
      else promptGetKey fs keys frame

-- Read UI requests from the client and send them to the frontend,
chanFrontend :: RawFrontend -> ChanFrontend
chanFrontend fs req = case req of
  FrontFrame{..} -> do
    fdisplay fs (Just frontFrame)
    return ()
  FrontDelay -> do
    fdisplay fs Nothing
    return ()
  FrontKey{..} -> do
    promptGetKey fs frontKeyKeys frontKeyFrame
  FrontInt{..} -> do
    let keys = frontIntKeys ++ K.escKM : K.returnKM
               : map (K.toKM K.NoModifier)
                   (map (K.Char . Char.intToDigit) [0..9])
    km <- promptGetKey fs keys frontIntFrame
    -- TODO: permit any number, perhaps auto-cap at frontIntMax
    case K.key km of
      K.Char l | Char.digitToInt l <= frontIntMax ->
        return $ Right $ Char.digitToInt l
      _ -> return $ Left km
  FrontText{..} -> undefined
  FrontSync -> fsyncFrames fs
  FrontAutoYes b ->
    writeIORef (fautoYesRef fs) b
