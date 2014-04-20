-- | The type of game key-command mappings to be used for UI.
module Game.LambdaHack.Client.UI.Content.KeyKind
  ( KeyKind(..)
  ) where

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.HumanCmd

data KeyKind = KeyKind
  { rhumanCommands :: ![(K.KM, ([CmdCategory], HumanCmd))]
                                   -- ^ default client UI commands
  }
