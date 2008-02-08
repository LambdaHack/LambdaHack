module Display (module D) where

-- wrapper for selected Display frontend

#ifdef GTK
import Display.Gtk as D
#else
import Display.Vty as D
#endif

