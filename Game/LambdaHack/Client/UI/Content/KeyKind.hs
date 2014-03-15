-- | The type of game key-command mappings to be used for UI.
module Game.LambdaHack.Client.UI.Content.KeyKind
  ( KeyKind(..)
  ) where

import Game.LambdaHack.Common.HumanCmd
import qualified Game.LambdaHack.Common.Key as K

data KeyKind = KeyKind
  { rhumanCommands :: ![(K.KM, (CmdCategory, HumanCmd))]
                                   -- ^ default client UI commands
  }
