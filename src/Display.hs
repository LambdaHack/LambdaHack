{-# LANGUAGE CPP #-}

module Display where

-- wrapper for selected Display frontend

#ifdef CURSES
import qualified Display.Curses as D
#elif VTY
import qualified Display.Vty as D
#elif STD
import qualified Display.Std as D
#else
import qualified Display.Gtk as D
#endif

-- Display routines that are independent of the selected display frontend.

import qualified Data.Char as Char
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M
import Control.Monad.IO.Class
import Data.Maybe

import Message
import qualified Color
import State
import Geometry
import Level
import LevelState
import Dungeon
import Perception
import Actor
import ActorState
import Content.ActorKind
import Item
import qualified Keys as K
import WorldLoc
import Random
import qualified Kind

-- Re-exported from the display frontend, with an extra slot for function
-- for translating keys to a canonical form.
type InternalSession = D.Session
type Session = (InternalSession, M.Map K.Key K.Key)
display :: Area -> Session -> (Loc -> (Color.Attr, Char)) -> String -> String
           -> IO ()
display area = D.display area . fst
startup :: (InternalSession -> IO ()) -> IO ()
startup = D.startup
shutdown :: Session -> IO ()
shutdown = D.shutdown . fst
displayId :: String
displayId = D.displayId

-- | Next event translated to a canonical form.
nextCommand :: MonadIO m => Session -> m K.Key
nextCommand session =
  do
    e <- liftIO $ D.nextEvent (fst session)
    return $ fromMaybe (K.canonMoveKey e) (M.lookup e (snd session))

-- | Displays a message on a blank screen. Waits for confirmation.
displayBlankConfirm :: Session -> String -> IO Bool
displayBlankConfirm session txt =
  let x = txt ++ more
      doBlank = const (Color.defaultAttr, ' ')
  in do
       display ((0, 0), normalLevelSize) session doBlank x ""
       getConfirm session

-- | Waits for a space or return or '?' or '*'. The last two act this way,
-- to let keys that request information toggle display the information off.
getConfirm :: MonadIO m => Session -> m Bool
getConfirm session =
  getOptionalConfirm return (const $ getConfirm session) session

getOptionalConfirm :: MonadIO m =>
                      (Bool -> m a) -> (K.Key -> m a) -> Session -> m a
getOptionalConfirm h k session =
  do
    e <- liftIO $ nextCommand session
    case e of
      K.Char ' ' -> h True
      K.Char '?' -> h True
      K.Char '*' -> h True
      K.Return   -> h True
      K.Esc      -> h False
      _          -> k e

-- | A yes-no confirmation.
getYesNo :: MonadIO m => Session -> m Bool
getYesNo session =
  do
    e <- liftIO $ nextCommand session
    case e of
      K.Char 'y' -> return True
      K.Char 'n' -> return False
      K.Esc      -> return False
      _          -> getYesNo session

splitOverlay :: Int -> String -> [[String]]
splitOverlay s xs = splitOverlay' (lines xs)
  where
    splitOverlay' ls
      | length ls <= s = [ls]  -- everything fits on one screen
      | otherwise      = let (pre,post) = splitAt (s - 1) ls
                         in  (pre ++ [more]) : splitOverlay' post

-- | Returns a function that looks up the characters in the
-- string by location. Takes the height of the display plus
-- the string. Returns also the number of screens required
-- to display all of the string.
stringByLocation :: Y -> String -> (Int, Loc -> Maybe Char)
stringByLocation sy xs =
  let
    ls   = splitOverlay sy xs
    m    = M.fromList (zip [0..] (L.map (M.fromList . zip [0..]) (concat ls)))
    k    = length ls
  in
    (k, \ (y,x) -> M.lookup y m >>= \ n -> M.lookup x n)

data ColorMode = ColorFull | ColorBW

displayLevel ::
  ColorMode -> Session -> Perceptions -> State -> Message -> Maybe String
  -> IO Bool
displayLevel
  dm session per
  (state@(State { scursor = cursor,
                  stime   = time,
                  sassocs = asso,
                  slevel  = Level ln _ (sy, sx) _ smap lm _ _ }))
  msg moverlay =
  let Actor{akind, ahp, aloc, aitems} = getPlayerBody state
      ActorKind{bhp} = Kind.getKind akind
      reachable = ptreachable per
      visible   = ptvisible per
      overlay   = fromMaybe "" moverlay
      (ns, over) = stringByLocation (sy+1) overlay -- n overlay screens needed
      sSml   = ssensory state == Smell
      sVis   = case ssensory state of Vision _ -> True; _ -> False
      sOmn   = sdisplay state == Omniscient
      lAt    = if sOmn then at else rememberAt
      sVisBG = if sVis
               then \ vis rea -> if vis
                                 then Color.Blue
                                 else if rea
                                      then Color.Magenta
                                      else Color.defBG
                else \ _vis _rea -> Color.defBG
      wealth  = L.sum $ L.map itemPrice aitems
      damage  = case strongestItem aitems "sword" of
                  Just sw -> 3 + ipower sw
                  Nothing -> 3
      hs      = levelHeroList state
      ms      = levelMonsterList state
      dis n loc0 =
        let tile = lm `lAt` loc0
            sm = smelltime $ M.findWithDefault (SmellTime 0) loc0 smap
            sml = (sm - time) `div` 100
            viewActor loc Actor{akind = akind2, asymbol}
              | loc == aloc && ln == creturnLn cursor =
                  (symbol, Color.defBG)  -- highlight player
              | otherwise = (symbol, bcolor)
              where
                ActorKind{bsymbol, bcolor} = Kind.getKind akind2
                symbol = fromMaybe bsymbol asymbol
            viewSmell :: Int -> Char
            viewSmell k
              | k > 9     = '*'
              | k < 0     = '-'
              | otherwise = Char.intToDigit k
            rainbow loc = toEnum $ uncurry (+) loc `mod` 14 + 1
            (char, fg0) =
              case L.find (\ m -> loc0 == Actor.aloc m) (hs ++ ms) of
                Just m | sOmn || vis -> viewActor loc0 m
                _ | sSml && sml >= 0 -> (viewSmell sml, rainbow loc0)
                  | otherwise        -> viewTile vis tile asso
            vis = S.member loc0 visible
            rea = S.member loc0 reachable
            bg0 = if ctargeting cursor && loc0 == clocation cursor
                  then Color.defFG      -- highlight targeting cursor
                  else sVisBG vis rea  -- FOV debug
            reverseVideo = (snd Color.defaultAttr, fst Color.defaultAttr)
            optVisually (fg, bg) =
              if (fg == Color.defBG) || (bg == Color.defFG && fg == Color.defFG)
              then reverseVideo
              else (fg, bg)
            a = case dm of
                  ColorBW   -> Color.defaultAttr
                  ColorFull -> optVisually (fg0, bg0)
        in case over (loc0 `shift` ((sy+1) * n, 0)) of
             Just c -> (Color.defaultAttr, c)
             _      -> (a, char)
      status =
        take 30 (levelName ln ++ repeat ' ') ++
        take 10 ("T: " ++ show (time `div` 10) ++ repeat ' ') ++
        take 10 ("$: " ++ show wealth ++ repeat ' ') ++
        take 10 ("Dmg: " ++ show damage ++ repeat ' ') ++
        take 20 ("HP: " ++ show ahp ++
                 " (" ++ show (maxDice bhp) ++ ")" ++ repeat ' ')
      disp n mesg = display ((0, 0), (sy, sx)) session (dis n) mesg status
      msgs = splitMsg sx msg
      perf k []     = perfo k ""
      perf k [xs]   = perfo k xs
      perf k (x:xs) = disp ns (x ++ more) >> getConfirm session >>= \ b ->
                      if b then perf k xs else return False
      perfo k xs
        | k < ns - 1 = disp k xs >> getConfirm session >>= \ b ->
                       if b then perfo (k+1) xs else return False
        | otherwise = disp k xs >> return True
  in  perf 0 msgs
