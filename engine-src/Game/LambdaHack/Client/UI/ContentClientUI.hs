-- | General content types and operations.
module Game.LambdaHack.Client.UI.ContentClientUI
  ( CCUI(..), emptyCCUI
  ) where

import Prelude ()

import qualified Data.Map.Strict as M

import Game.LambdaHack.Client.UI.Content.Input
import Game.LambdaHack.Client.UI.Content.Screen

-- | Operations for all UI content types, gathered together.
data CCUI = CCUI
  { coinput  :: InputContent
  , coscreen :: ScreenContent
  }

emptyCCUI :: CCUI
emptyCCUI = CCUI
  { coinput = InputContent M.empty [] M.empty
  , coscreen = emptyScreenContent
  }
