module Command where

import Action
import Actions
import Geometry
import qualified Keys as K
import Level

data Described a = Described { chelp :: String, caction :: a }
                 | Undescribed { caction :: a }

type Command    = Described (Action ())
type DirCommand = Described (Dir -> Action ())

closeCommand     = Described "close a door"      (openclose False)
openCommand      = Described "open a door"       (openclose True)
pickupCommand    = Described "pick up an object" pickupItem
dropCommand      = Described "drop an object"    dropItem
inventoryCommand = Described "display inventory" inventory
searchCommand    = Described "search for secret doors" search
ascendCommand    = Described "ascend a level"    (lvlchange Up)
descendCommand   = Described "descend a level"   (lvlchange Down)
lookCommand      = Described "toggle look mode"  lookAround
drinkCommand     = Described "quaff a potion"    drinkPotion
waitCommand      = Described "wait"              (return () :: Action ())
saveCommand      = Described "save and quit the game" saveGame
quitCommand      = Described "quit without saving" quitGame
cancelCommand    = Described "cancel current action" cancelCurrent
historyCommand   = Described "display previous messages" displayHistory
heroCommand      = Described "cycle among heroes on level" cycleHero  -- TODO: should take 0 turns!

moveDirCommand   = Described "move in direction" move
runDirCommand    = Described "run in direction"  run
