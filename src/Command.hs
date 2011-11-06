module Command where

import Action
import Actions
import Dir
import ItemAction
import Version

data Described a = Described { chelp :: String, caction :: a }
                 | Undescribed { caction :: a }

type Command    = Described (Action ())
type DirCommand = Described (Dir -> Action ())

closeCommand, pickupCommand, dropCommand, inventoryCommand, ascendCommand, descendCommand, floorCommand, monsterCommand, quaffCommand, readCommand, throwCommand, aimCommand, saveCommand, quitCommand, cancelCommand, historyCommand, dumpCommand, heroCommand, versionCommand :: Described (Action ())
closeCommand     = Described "close a door"      (checkCursor closeDoor)
pickupCommand    = Described "get an object"     (checkCursor pickupItem)
dropCommand      = Described "drop an object"    (checkCursor dropItem)
inventoryCommand = Described "display inventory" inventory
ascendCommand    = Described "ascend a level"    (lvlGoUp True)
descendCommand   = Described "descend a level"   (lvlGoUp False)
floorCommand     = Described "target location"   targetFloor
monsterCommand   = Described "target monster"    (checkCursor targetMonster)
quaffCommand     = Described "quaff a potion"    (checkCursor quaffPotion)
readCommand      = Described "read a scroll"     (checkCursor readScroll)
throwCommand     = Described "throw a weapon"    (checkCursor throwItem)
aimCommand       = Described "aim a wand"        (checkCursor aimItem)
saveCommand      = Described "save and exit the game" saveGame
quitCommand      = Described "quit without saving" quitGame
cancelCommand    = Described "cancel action"     cancelCurrent
historyCommand   = Described "display previous messages" displayHistory
dumpCommand      = Described "dump current configuration" dumpConfig
heroCommand      = Described "cycle among heroes on level" cycleHero
versionCommand   = Described "display game version" (abortWith version)

acceptCommand :: Action () -> Described (Action ())
acceptCommand h  = Described "accept choice"     (acceptCurrent h)

moveDirCommand, runDirCommand :: Described (Dir -> Action ())
moveDirCommand   = Described "move in direction" move
runDirCommand    = Described "run in direction"  run
