{-# LANGUAGE OverloadedStrings #-}
-- | Abstract syntax of player commands.
module Game.LambdaHack.Command
  ( Cmd(..), majorCmd, timedCmd, cmdDescription, configCmds
  ) where

import Data.Text (Text)
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Config
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Msg
import Game.LambdaHack.Utils.Assert
import qualified Game.LambdaHack.Key as K

-- | Abstract syntax of player commands. The type is abstract, but the values
-- are created outside this module via the Read class (from config file) .
data Cmd =
    -- These take time.
    Apply       { verb :: MU.Part, object :: MU.Part, syms :: [Char] }
  | Project     { verb :: MU.Part, object :: MU.Part, syms :: [Char] }
  | TriggerDir  { verb :: MU.Part, object :: MU.Part, feature :: F.Feature }
  | TriggerTile { verb :: MU.Part, object :: MU.Part, feature :: F.Feature }
  | Pickup
  | Drop
  | Wait
  | GameExit
  | GameRestart
    -- These do not take time, or not quite.
  | GameSave
  | Inventory
  | TgtFloor
  | TgtEnemy
  | TgtAscend Int
  | EpsIncr Bool
  | Cancel
  | Accept
  | Clear
  | History
  | CfgDump
  | HeroCycle
  | HeroBack
  | Help
  deriving (Show, Read, Eq, Ord)

-- | Major commands land on the first page of command help.
majorCmd :: Cmd -> Bool
majorCmd cmd = case cmd of
  Apply{}       -> True
  Project{}     -> True
  TriggerDir{}  -> True
  TriggerTile{} -> True
  Pickup        -> True
  Drop          -> True
  GameExit      -> True
  GameRestart   -> True
  GameSave      -> True
  Inventory     -> True
  Help          -> True
  _             -> False

-- | Time cosuming commands are marked as such in help and cannot be
-- invoked in targeting mode on a remote level (level different than
-- the level of the selected hero).
timedCmd :: Cmd -> Bool
timedCmd cmd = case cmd of
  Apply{}       -> True
  Project{}     -> True
  TriggerDir{}  -> True
  TriggerTile{} -> True
  Pickup        -> True
  Drop          -> True
  Wait          -> True
  GameExit      -> True  -- takes time, then rewinds time
  GameRestart   -> True  -- takes time, then resets state
  _             -> False

-- | Description of player commands.
cmdDescription :: Cmd -> Text
cmdDescription cmd = case cmd of
  Apply{..}       -> makePhrase [verb, MU.AW object]
  Project{..}     -> makePhrase [verb, MU.AW object]
  TriggerDir{..}  -> makePhrase [verb, MU.AW object]
  TriggerTile{..} -> makePhrase [verb, MU.AW object]
  Pickup    -> "get an object"
  Drop      -> "drop an object"
  Wait      -> ""
  GameExit  -> "save and exit"
  GameRestart -> "restart game"
  GameSave  -> "save game"

  Inventory -> "display inventory"
  TgtFloor  -> "target location"
  TgtEnemy  -> "target monster"
  TgtAscend k | k == 1  -> "target next shallower level"
  TgtAscend k | k >= 2  -> "target" <+> showT k    <+> "levels shallower"
  TgtAscend k | k == -1 -> "target next deeper level"
  TgtAscend k | k <= -2 -> "target" <+> showT (-k) <+> "levels deeper"
  TgtAscend _ ->
    assert `failure` ("void level change in targeting in config file" :: Text)
  EpsIncr True  -> "swerve targeting line"
  EpsIncr False -> "unswerve targeting line"
  Cancel    -> "cancel action"
  Accept    -> "accept choice"
  Clear     -> "clear messages"
  History   -> "display previous messages"
  CfgDump   -> "dump current configuration"
  HeroCycle -> "cycle among heroes on level"
  HeroBack  -> "cycle among heroes in the dungeon"
  Help      -> "display help"

-- | The associaction of commands to keys defined in config.
configCmds :: ConfigUI -> [(K.Key, Cmd)]
configCmds ConfigUI{configCommands} =
  let mkCommand (key, def) = (key, read def :: Cmd)
  in map mkCommand configCommands
