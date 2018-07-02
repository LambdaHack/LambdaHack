-- | General content types and operations.
module Game.LambdaHack.Client.UI.ContentClientUI
  ( CCUI(..), emptyCCUI
  ) where

import Prelude ()

import Game.LambdaHack.Client.UI.Content.Input
import Game.LambdaHack.Client.UI.Content.Screen

-- | Operations for all content types, gathered together.
data CCUI = CCUI
  { coinput  :: InputContent
  , coscreen :: ScreenContent
  }

emptyCCUI :: CCUI
emptyCCUI = CCUI
  { coinput = InputContent []
  , coscreen = ScreenContent { rwidth = 0
                             , rheight = 0
                             , rmainMenuArt = ""
                             , rintroScreen = []
                             }
  }
