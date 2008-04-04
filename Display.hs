module Display (module D, module Display) where

-- wrapper for selected Display frontend

#ifdef GTK
import Display.Gtk as D
#else
import Display.Vty as D
#endif

-- a few common high-level functions

more :: String
more = " --more--"

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

