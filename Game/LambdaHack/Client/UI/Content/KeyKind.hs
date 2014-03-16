-- | The type of game key-command mappings to be used for UI.
module Game.LambdaHack.Client.UI.Content.KeyKind
  ( KeyKind(..)
  ) where

import Game.LambdaHack.Client.UI.HumanCmd
import qualified Game.LambdaHack.Client.Key as K

data KeyKind = KeyKind
  { rhumanCommands :: ![(K.KM, (CmdCategory, HumanCmd))]
                                   -- ^ default client UI commands
  }
