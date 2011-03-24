module Display2 (module Display, module Display2) where

-- Display routines that are independent of the selected display frontend.

import qualified Data.Char as Char
import Data.Set as S
import Data.List as L
import Data.Map as M
import qualified Data.IntMap as IM
import Control.Monad.State hiding (State) -- for MonadIO, seems to be portable between mtl-1 and 2
import Data.Maybe

import Message
import Display
import qualified Attr
import State
import Geometry
import Level
import LevelState
import Dungeon
import Perception
import Movable
import MovableState
import Monster
import Item
import Keys as K

-- | Next event translated to a canonical form
nextCommand :: MonadIO m => Session -> m Key
nextCommand session =
  do
    e <- liftIO $ nextEvent session
    return (canonicalKey e)

-- | maps a key to the canonical key for the command it denotes
canonicalKey :: Key -> Key
canonicalKey e =
  case e of
    K.KP '8' -> K.Char 'K'
    K.KP '2' -> K.Char 'J'
    K.KP '4' -> K.Char 'H'
    K.KP '6' -> K.Char 'L'
    K.KP '7' -> K.Char 'Y'
    K.KP '9' -> K.Char 'U'
    K.KP '1' -> K.Char 'B'
    K.KP '3' -> K.Char 'N'
    K.KP '5' -> K.Char '.'
    K.Up     -> K.Char 'k'
    K.Down   -> K.Char 'j'
    K.Left   -> K.Char 'h'
    K.Right  -> K.Char 'l'
    K.Home   -> K.Char 'y'
    K.PgUp   -> K.Char 'u'
    K.End    -> K.Char 'b'
    K.PgDn   -> K.Char 'n'
    K.Begin  -> K.Char '.'
    k        -> k

-- | Displays a message on a blank screen. Waits for confirmation.
displayBlankConfirm :: Session -> String -> IO Bool
displayBlankConfirm session txt =
  let x = txt ++ more
  in  do
        display ((0, 0), normalLevelSize) session (const (attr, ' ')) x ""
        getConfirm session

-- | Waits for a space or return or '?' or '*'. The last two to let keys that
-- request (more) information toggle display of the obtained information off.
getConfirm :: MonadIO m => Session -> m Bool
getConfirm session =
  getOptionalConfirm return (const $ getConfirm session) session

getOptionalConfirm :: MonadIO m =>
                      (Bool -> m a) -> (Key -> m a) -> Session -> m a
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

-- | Configurable event handler for the direction keys. Is used to
--   handle player moves, but can also be used for directed commands
--   such as open/close.
handleDirection :: Key -> (Dir -> a) -> a -> a
handleDirection e h k =
  case e of
    K.Char 'k' -> h up
    K.Char 'j' -> h down
    K.Char 'h' -> h left
    K.Char 'l' -> h right
    K.Char 'y' -> h upleft
    K.Char 'u' -> h upright
    K.Char 'b' -> h downleft
    K.Char 'n' -> h downright
    _          -> k

-- | Configurable event handler for the upper direction keys. Is used to
--   handle player moves, but can also be used for directed commands
--   such as open/close.
handleUDirection :: Key -> (Dir -> a) -> a -> a
handleUDirection e h k =
  case e of
    K.Char 'K' -> h up
    K.Char 'J' -> h down
    K.Char 'H' -> h left
    K.Char 'L' -> h right
    K.Char 'Y' -> h upleft
    K.Char 'U' -> h upright
    K.Char 'B' -> h downleft
    K.Char 'N' -> h downright
    _          -> k

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

displayLevel ::
  Session -> Perceptions -> State -> Message -> Maybe String -> IO Bool
displayLevel
  session per
  (state@(State { scursor = cursor,
                  stime   = time,
                  sassocs = assocs,
                  slevel  = Level ln _ (sy, sx) _ smap lmap _ }))
  msg moverlay =
  let Movable { mtype = MovableType { nhpMax = xhp },
                mhp = php, mloc = ploc, mitems = pitems } = getPlayerBody state
      reachable = ptreachable per
      visible   = ptvisible per
      overlay   = fromMaybe "" moverlay
      (n, over) = stringByLocation (sy+1) overlay -- n overlay screens needed
      sSml   = ssensory state == Smell
      sVis   = case ssensory state of Vision _ -> True; _ -> False
      sOmn   = sdisplay state == Omniscient
      sTer   = case sdisplay state of Terrain n -> n; _ -> 0
      lAt    = if sOmn || sTer > 0 then at else rememberAt
      sVisBG = if sVis
               then \ vis rea -> if vis
                                 then Attr.Blue
                                 else if rea
                                      then Attr.Magenta
                                      else Attr.defBG
                else \ vis rea -> Attr.defBG
      gItem   = findItem (\ i -> iletter i == Just '$') pitems
      gold    = maybe 0 (icount . fst) gItem
      hs      = levelHeroList state
      ms      = levelMonsterList state
      dis n loc =
        let tile = lmap `lAt` loc
            sml  = ((smap ! loc) - time) `div` 100
            viewMovable loc (Movable { mtype = mt })
              | loc == ploc && ln == creturnLn cursor =
                  (nsymbol mt, Attr.defBG)  -- highlight player
              | otherwise = (nsymbol mt, ncolor mt)
            viewSmell :: Int -> Char
            viewSmell n
              | n > 9     = '*'
              | n < 0     = '-'
              | otherwise = Char.intToDigit n
            (char, fg) =
              case L.find (\ m -> loc == mloc m) (hs ++ ms) of
                _ | sTer > 0         -> viewTerrain sTer False (tterrain tile)
                Just m | sOmn || vis -> viewMovable loc m
                _ | sSml && sml >= 0 -> (viewSmell sml, Attr.Green)
                  | otherwise        -> viewTile vis tile assocs
            vis = S.member loc visible
            rea = S.member loc reachable
            bg = if ctargeting cursor && loc == clocation cursor
                 then Attr.defFG      -- highlight targeting cursor
                 else sVisBG vis rea  -- FOV debug
            reverseVideo = (Attr.defBG, Attr.defFG)
            optVisually (fg, bg) =
              if fg == Attr.defBG
              then reverseVideo
              else if bg == Attr.defFG && fg == Attr.defFG
                   then reverseVideo
                   else (fg, bg)
            optComputationally (fg, bg) =
              let fgSet = if fg == Attr.defFG then id else setFG fg
                  bgSet = if bg == Attr.defBG then id else setBG bg
              in  fgSet . bgSet
            set = optComputationally . optVisually $ (fg, bg)
        in case over (loc `shift` ((sy+1) * n, 0)) of
             Just c -> (attr, c)
             _      -> (set attr, char)
      bottomLine =
        take 40 (levelName ln ++ repeat ' ') ++
        take 10 ("$: " ++ show gold ++ repeat ' ') ++
        take 15 ("HP: " ++ show php ++ " (" ++ show xhp ++ ")" ++ repeat ' ') ++
        take 15 ("T: " ++ show (time `div` 10) ++ repeat ' ')
      disp n msg = display ((0, 0), (sy, sx)) session (dis n) msg bottomLine
      msgs = splitMsg sx msg
      perf k []     = perfo k ""
      perf k [xs]   = perfo k xs
      perf k (x:xs) = disp n (x ++ more) >> getConfirm session >>= \ b ->
                      if b then perf k xs else return False
      perfo k xs
        | k < n - 1 = disp k xs >> getConfirm session >>= \ b ->
                      if b then perfo (k+1) xs else return False
        | otherwise = disp k xs >> return True
  in  perf 0 msgs
