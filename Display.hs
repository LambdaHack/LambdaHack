{-# LANGUAGE CPP #-}

module Display (module D) where

-- wrapper for selected Display frontend

#ifdef CURSES
import Display.Curses as D
#elif GTK
import Display.Gtk as D
#else
import Display.Vty as D
#endif

