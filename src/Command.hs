module Command where

import Action
import Actions
import ItemAction
import Geometry
import Level

data Described a = Described { chelp :: String, caction :: a }
                 | Undescribed { caction :: a }

type Command    = Described (Action ())
type DirCommand = Described (Dir -> Action ())

closeCommand     = Described "close a door"      (checkCursor (openclose False))
openCommand      = Described "open a door"       (checkCursor (openclose True))
pickupCommand    = Described "get an object"     (checkCursor pickupItem)
dropCommand      = Described "drop an object"    (checkCursor dropItem)
inventoryCommand = Described "display inventory" inventory
searchCommand    = Described "search for secret doors" (checkCursor search)
ascendCommand    = Described "ascend a level"    (lvlChange Up)
descendCommand   = Described "descend a level"   (lvlChange Down)
floorCommand     = Described "target location"   targetFloor
monsterCommand   = Described "target monster"    (checkCursor targetMonster)
quaffCommand     = Described "quaff a potion"    quaffPotion
readCommand      = Described "read a scroll"     readScroll
throwCommand     = Described "throw a weapon"    (checkCursor throwItem)
aimCommand       = Described "aim a wand"        (checkCursor aimItem)
waitCommand      = Described "wait"              playerAdvanceTime
saveCommand      = Described "save and quit the game" saveGame
quitCommand      = Described "quit without saving" quitGame
cancelCommand    = Described "cancel action"     cancelCurrent
acceptCommand h  = Described "accept choice"     (acceptCurrent h)
historyCommand   = Described "display previous messages" displayHistory
dumpCommand      = Described "dump current configuration" dumpConfig
heroCommand      = Described "cycle among heroes on level" cycleHero

moveDirCommand   = Described "move in direction" move
runDirCommand    = Described "run in direction"  run
