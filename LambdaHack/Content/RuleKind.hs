{-# LANGUAGE TemplateHaskell #-}
-- | Game rules and assorted game setup data for LambdaHack.
module Content.RuleKind ( cdefs ) where

import Language.Haskell.TH.Syntax

-- Cabal
import qualified Paths_LambdaHack as Self (getDataFileName, version)

import Game.LambdaHack.Common.ContentDef
import qualified Game.LambdaHack.Common.Feature as F
import Game.LambdaHack.Common.Vector
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind

cdefs :: ContentDef RuleKind
cdefs = ContentDef
  { getSymbol = rsymbol
  , getName = rname
  , getFreq = rfreq
  , validate = validateRuleKind
  , content =
      [standard]
  }

standard :: RuleKind
standard = RuleKind
  { rsymbol        = 's'
  , rname          = "standard LambdaHack ruleset"
  , rfreq          = [("standard", 100)]
  -- Check whether one position is accessible from another.
  -- Precondition: the two positions are next to each other.
  -- Apart of checking the target tile, we forbid diagonal movement
  -- to and from doors.
  , raccessible    = \spos src tpos tgt ->
      let getTo F.CloseTo{} = True
          getTo _ = False
      in F.Walkable `elem` tfeature tgt
         && not ((any getTo (tfeature  src) ||
                  any getTo (tfeature tgt))
                 && diagonal (displacement spos tpos))
  , rtitle         = "LambdaHack"
  , rpathsDataFile = Self.getDataFileName
  , rpathsVersion  = Self.version
  , ritemMelee     = ")"
  , ritemRanged    = "|"
  -- Wasting weapons and armour would be too cruel to the player.
  , ritemProject   = "!?|/"
  -- The strings containing the default configuration files,
  -- included from files config.game.default and config.ui.default.
  -- Warning: cabal does not detect that the config files are changed,
  -- so touching them is needed to reinclude configs and recompile.
  -- Note: consider code.haskell.org/~dons/code/compiled-constants
  -- as soon as the config file grows very big.
  , rcfgRulesDefault = $(do
      qAddDependentFile "config.rules.default"
      x <- qRunIO (readFile "config.rules.default")
      lift x)
  , rcfgUIDefault = $(do
      qAddDependentFile "config.ui.default"
      x <- qRunIO (readFile "config.ui.default")
      lift x)
  -- ASCII art for the Main Menu. Only pure 7-bit ASCII characters are
  -- allowed. The picture should be exactly 24 rows by 80 columns,
  -- plus an extra frame (of any characters) that is ignored.
  -- For a different screen size, the picture is centered and the outermost
  -- rows and columns cloned. When displayed in the Main Menu screen,
  -- it's overwritten with the game version string and keybinding strings.
  -- The game version string begins and ends with a space and is placed
  -- in the very bottom right corner. The keybindings overwrite places
  -- marked with 25 left curly brace signs '{' in a row. The sign is forbidden
  -- everywhere else. A specific number of such places with 25 left braces
  -- are required, at most one per row, and all are overwritten
  -- with text that is flushed left and padded with spaces.
  -- The Main Menu is displayed dull white on black.
  -- TODO: Show highlighted keybinding in inverse video or bright white on grey
  -- background. The spaces that pad keybindings are not highlighted.
  , rmainMenuArt = $(do
      qAddDependentFile "MainMenu.ascii"
      x <- qRunIO (readFile "MainMenu.ascii")
      lift x)
  }
