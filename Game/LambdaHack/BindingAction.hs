{-# LANGUAGE OverloadedStrings #-}
-- | Binding of keys to commands implemented with the 'Action' monad.
module Game.LambdaHack.BindingAction
  ( stdBinding
  ) where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Text (Text)
import Data.Tuple (swap)

import Game.LambdaHack.Binding
import Game.LambdaHack.Command
import Game.LambdaHack.Config
import qualified Game.LambdaHack.Key as K

-- TODO: clean up: probably add Command.Cmd for each operation

semanticsCmds' :: [(K.Key, Cmd)]
               -> [((K.Key, K.Modifier), (Text, Bool, Cmd))]
semanticsCmds' cmdList =
  let mkDescribed cmd = (cmdDescription cmd, timedCmd cmd, cmd)
      mkCommand (key, def) = ((key, K.NoModifier), mkDescribed def)
  in L.map mkCommand cmdList

-- | Binding of keys to movement and other standard commands,
-- as well as commands defined in the config file.
stdBinding :: ConfigUI  -- ^ game config
           -> Binding   -- ^ concrete binding
stdBinding !config@ConfigUI{configMacros} =
  let kmacro = M.fromList $ configMacros
      cmdList = configCmds config
      semList = semanticsCmds' cmdList
      -- TODO
      -- Targeting cursor movement and others are wrongly marked as timed;
      -- fixed in their definitions by rewinding time.
      cmdDir = K.moveBinding (const True) (const True)
  in Binding
  { kcmd   = M.fromList semList
  , kmacro
  , kmajor = L.map fst $ L.filter (majorCmd . snd) cmdList
  , kdir   = L.map fst cmdDir
  , krevMap = M.fromList $ map swap cmdList
  }
