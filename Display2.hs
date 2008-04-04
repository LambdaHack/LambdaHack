module Display2 (module Display, module Display2) where

import Data.Set as S
import Data.List as L
import Data.Map as M

import Message
import Display
import State
import Geometry
import Level
import Perception
import Monster

-- | Displays a message on a blank screen. Waits for confirmation.
displayBlankConfirm :: Session -> String -> IO ()
displayBlankConfirm session txt =
  let x = txt ++ more
  in  do
        display ((0,0),(0,length x - 1)) session (const (attr, ' ')) x ""
        getConfirm session

-- | Waits for a space or return.
getConfirm :: Session -> IO ()
getConfirm session =
  do
    e <- nextEvent session
    handleModifier e (getConfirm session) $
      case e of
        "space"  -> return ()
        "Return" -> return ()
        _        -> getConfirm session 

-- | Handler that ignores modifier events as they are
--   currently produced by the Gtk frontend.
handleModifier :: String -> IO () -> IO () -> IO ()
handleModifier e h k =
  case e of
    "Shift_R"   -> h
    "Shift_L"   -> h
    "Control_L" -> h
    "Control_R" -> h
    "Super_L"   -> h
    "Super_R"   -> h
    "Menu"      -> h
    "Alt_L"     -> h
    "Alt_R"     -> h
    _           -> k

-- | Configurable event handler for the direction keys. Is used to
--   handle player moves, but can also be used for directed commands
--   such as open/close.
handleDirection :: String -> ((Y,X) -> IO ()) -> IO () -> IO ()
handleDirection e h k =
  case e of
    "k" -> h (-1,0)
    "j" -> h (1,0)
    "h" -> h (0,-1)
    "l" -> h (0,1)
    "y" -> h (-1,-1)
    "u" -> h (-1,1)
    "b" -> h (1,-1)
    "n" -> h (1,1)
    _   -> k


displayLevel :: Session -> Level -> Perception -> State -> Message -> IO ()
displayLevel session (lvl@(Level nm sz ms smap nlmap lmeta))
                     per
                     (state@(State { splayer = player@(Monster { mhp = php, mdir = pdir, mloc = ploc }), stime = time }))
                     msg =
    let
      reachable = preachable per
      visible   = pvisible per
      sSml    = ssensory state == Smell
      sVis    = ssensory state == Vision
      sOmn    = sdisplay state == Omniscient
      sTer    = case sdisplay state of Terrain n -> n; _ -> 0
      lAt     = if sOmn || sTer > 0 then at else rememberAt
      lVision = if sVis
                  then \ vis rea ->
                       if      vis then setBG blue
                       else if rea then setBG magenta
                                   else id
                  else \ vis rea -> id
      disp msg = 
        display ((0,0),sz) session 
                 (\ loc -> let tile = nlmap `lAt` loc
                               sml  = ((smap ! loc) - time) `div` 100
                               vis  = S.member loc visible
                               rea  = S.member loc reachable
                               (rv,ra) = case L.find (\ m -> loc == mloc m) (player:ms) of
                                           _ | sTer > 0          -> viewTerrain sTer (tterrain tile)
                                           Just m | sOmn || vis  -> viewMonster (mtype m) 
                                           _ | sSml && sml >= 0  -> viewSmell sml
                                             | otherwise         -> viewTile tile
                               vision = lVision vis rea
                           in
                             (ra . vision $
                              attr, rv))
                msg
                (take 40 (levelName nm ++ repeat ' ') ++ take 10 ("HP: " ++ show php ++ repeat ' ') ++
                 take 10 ("T: " ++ show (time `div` 10) ++ repeat ' '))
      msgs = splitMsg (snd sz) msg
      perf []     = disp ""
      perf [xs]   = disp xs
      perf (x:xs) = disp (x ++ more) >> getConfirm session >> perf xs
    in perf msgs


