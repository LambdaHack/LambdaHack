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

closeCommand     = Described "close a door"      (checkLook (openclose False))
openCommand      = Described "open a door"       (checkLook (openclose True))
pickupCommand    = Described "pick up an object" (checkLook pickupItem)
dropCommand      = Described "drop an object"    (checkLook dropItem)
inventoryCommand = Described "display inventory" inventory
searchCommand    = Described "search for secret doors" (checkLook search)
ascendCommand    = Described "ascend a level"    (lvlchange Up)
descendCommand   = Described "descend a level"   (lvlchange Down)
lookCommand      = Described "toggle look mode"  lookAround  -- TODO: should not take time
drinkCommand     = Described "quaff a potion"    drinkPotion
waitCommand      = Described "wait"              (return () :: Action ())
saveCommand      = Described "save and quit the game" saveGame
quitCommand      = Described "quit without saving" quitGame
cancelCommand    = Described "cancel current action" cancelCurrent
historyCommand   = Described "display previous messages" displayHistory
dumpCommand      = Described "dump current configuration" dumpConfig
heroCommand      = Described "cycle among heroes on level" cycleHero  -- TODO: should take 0 turns! When it does, use in Turn.hs.

moveDirCommand   = Described "move in direction" move
runDirCommand    = Described "run in direction"  run
