-- | General content types and operations.
module Game.LambdaHack.Client.UI.ContentClientUI
  ( CCUI(..), emptyCCUI
  ) where

import Prelude ()

import Game.LambdaHack.Client.UI.Content.Input
import Game.LambdaHack.Client.UI.Content.Screen

-- | Operations for all content types, gathered together.
data CCUI = CCUI
  { coinput  :: InputContentData
  , coscreen :: ScreenContentData
  }

emptyCCUI :: CCUI
emptyCCUI = CCUI
  { coinput = InputContentData []
  , coscreen = ScreenContentData { rmainMenuArt = ""
                                 , rintroScreen = []
                                 }
  }
