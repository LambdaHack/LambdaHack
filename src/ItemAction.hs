module ItemAction where

import Control.Monad
import Control.Monad.State hiding (State)
import Data.Function
import Data.List as L
import Data.Map as M
import qualified Data.IntMap as IM
import Data.Maybe
import Data.Set as S
import System.Time

import Action
import Display hiding (display)
import Dungeon
import Geometry
import Grammar
import qualified HighScores as H
import Item
import qualified ItemKind
import qualified Keys as K
import Level
import LevelState
import Message
import Movable
import MovableState
import MovableKind
import MovableAdd
import Perception
import Random
import State
import qualified Config
import qualified Save
import Terrain
import qualified Effect
import EffectAction

-- Item UI code with the Action type and everything it depends on
-- that is not already in Action.hs and EffectAction.hs.
-- This file should not depend on Action.hs.

-- | Display inventory
inventory :: Action a
inventory = do
  items <- gets (mitems . getPlayerBody)
  if L.null items
    then abortWith "Not carrying anything."
    else do
           displayItems "Carrying:" True items
           session getConfirm
           abortWith ""

-- | Let the player choose any item with a given group name.
-- Note that this does not guarantee an item from the group to be chosen,
-- as the player can override the choice.
getGroupItem :: [Item] ->  -- all objects in question
                String ->  -- name of the group
                String ->  -- prompt
                String ->  -- how to refer to the collection of objects
                Action (Maybe Item)
getGroupItem is groupName prompt packName =
  let choice i = groupName == ItemKind.jname (ItemKind.getIK (ikind i))
      header = capitalize $ suffixS groupName
  in  getItem prompt choice header is packName

applyGroupItem :: Actor ->   -- actor applying the item; on current level
                  String ->  -- how the "applying" is called
                  Item ->    -- the item to be applied
                  Action ()
applyGroupItem actor verb item = do
  state <- get
  body  <- gets (getActor actor)
  per   <- currentPerception
  -- only one item consumed, even if several in inventory
  let consumed = item { icount = 1 }
      msg = subjectVerbIObject state body verb consumed ""
      loc = mloc body
  removeFromInventory actor consumed loc
  when (loc `S.member` ptvisible per) $ messageAdd msg
  itemEffectAction actor actor consumed
  advanceTime actor

playerApplyGroupItem :: String -> Action ()
playerApplyGroupItem groupName = do
  is   <- gets (mitems . getPlayerBody)
  iOpt <- getGroupItem is groupName
            ("What to " ++ applyToVerb groupName ++ "?") "in inventory"
  pl   <- gets splayer
  case iOpt of
    Just i  ->
      let verb = applyToVerb (ItemKind.jname (ItemKind.getIK (ikind i)))
      in  applyGroupItem pl verb i
    Nothing -> neverMind True

applyToVerb :: String -> String
applyToVerb "potion" = "quaff"
applyToVerb "scroll" = "read"
applyToVerb _ = "destructively apply"

quaffPotion :: Action ()
quaffPotion = playerApplyGroupItem "potion"

readScroll :: Action ()
readScroll = playerApplyGroupItem "scroll"

zapGroupItem :: Actor ->   -- actor zapping the item; on current level
                Loc ->     -- target location for the zapping
                String ->  -- how the "zapping" is called
                Item ->    -- the item to be zapped
                Action ()
zapGroupItem source loc verb item = do
  state <- get
  sm    <- gets (getActor source)
  per   <- currentPerception
  let consumed = item { icount = 1 }
      msg = subjectVerbIObject state sm verb consumed ""
      sloc = mloc sm
  removeFromInventory source consumed sloc
  -- The message describes the source part of the action.
  when (sloc `S.member` ptvisible per) $ messageAdd msg
  case locToActor loc state of
    Just ta -> do
      -- Messages inside itemEffectAction describe the target part.
      b <- itemEffectAction source ta consumed
      when (not b) $
        modify (updateLevel (dropItemsAt [consumed] loc))
    Nothing ->
      modify (updateLevel (dropItemsAt [consumed] loc))
  advanceTime source

playerZapGroupItem :: String -> Action ()
playerZapGroupItem groupName = do
  state <- get
  is    <- gets (mitems . getPlayerBody)
  iOpt  <- getGroupItem is groupName
             ("What to " ++ zapToVerb groupName ++ "?") "in inventory"
  pl    <- gets splayer
  per   <- currentPerception
  case iOpt of
    Just i  ->
      case targetToLoc (ptvisible per) state of
        Nothing  -> abortWith "target invalid"
        Just loc ->
          -- TODO: draw digital line and see if obstacles prevent firing
          if actorReachesLoc pl loc per (Just pl)
          then let verb = zapToVerb (ItemKind.jname (ItemKind.getIK (ikind i)))
               in  zapGroupItem pl loc verb i
          else abortWith "target not reachable"
    Nothing -> neverMind True

zapToVerb :: String -> String
zapToVerb "wand" = "aim"
zapToVerb "dart" = "throw"
zapToVerb _ = "furiously zap"

aimItem :: Action ()
aimItem = playerZapGroupItem "wand"

throwItem :: Action ()
throwItem = playerZapGroupItem "dart"

-- | Drop a single item.
-- TODO: allow dropping a given number of identical items.
dropItem :: Action ()
dropItem = do
  pl    <- gets splayer
  state <- get
  pbody <- gets getPlayerBody
  ploc  <- gets (mloc . getPlayerBody)
  items <- gets (mitems . getPlayerBody)
  iOpt  <- getAnyItem "What to drop?" items "inventory"
  case iOpt of
    Just stack -> do
      let i = stack { icount = 1 }
      removeOnlyFromInventory pl i (mloc pbody)
      messageAdd (subjectVerbIObject state pbody "drop" i "")
      modify (updateLevel (dropItemsAt [i] ploc))
    Nothing -> neverMind True
  playerAdvanceTime

-- TODO: this is a hack for dropItem, because removeFromInventory
-- makes it impossible to drop items if the floor not empty.
removeOnlyFromInventory :: Actor -> Item -> Loc -> Action ()
removeOnlyFromInventory actor i loc = do
  updateAnyActor actor (\ m -> m { mitems = removeItemByLetter i (mitems m) })

-- | Remove given item from an actor's inventory or floor.
-- TODO: this is subtly wrong: if identical items are on the floor and in
-- inventory, the floor one will be chosen, regardless of player intention.
-- TODO: right now it ugly hacks (with the ploc) around removing items
-- of dead heros/monsters. The subtle incorrectness helps here a lot,
-- because items of dead heroes land on the floor, so we use them up
-- in inventory, but remove them after use from the floor.
removeFromInventory :: Actor -> Item -> Loc -> Action ()
removeFromInventory actor i loc = do
  b <- removeFromLoc i loc
  when (not b) $
    updateAnyActor actor (\ m -> m { mitems = removeItemByLetter i (mitems m) })

-- | Remove given item from the given location. Tell if successful.
removeFromLoc :: Item -> Loc -> Action Bool
removeFromLoc i loc = do
  lmap  <- gets (lmap . slevel)
  if not $ L.any (equalItemIdentity i) (titems (lmap `at` loc))
    then return False
    else
      modify (updateLevel (updateLMap adj)) >>
      return True
        where
          adj = M.adjust (\ (t, rt) -> (remove t, rt)) loc
          remove t = t { titems = removeItemByIdentity i (titems t) }

actorPickupItem :: Actor -> Action ()
actorPickupItem actor = do
  state <- get
  pl    <- gets splayer
  per   <- currentPerception
  lmap  <- gets (lmap . slevel)
  body  <- gets (getActor actor)
  let loc       = mloc body
      t         = lmap `at` loc -- the map tile in question
      perceived = loc `S.member` ptvisible per
      isPlayer  = actor == pl
  -- check if something is here to pick up
  case titems t of
    []   -> abortIfWith isPlayer "nothing here"
    i:rs -> -- pick up first item; TODO: let player select item;not for monsters
      case assignLetter (iletter i) (mletter body) (mitems body) of
        Just l -> do
          let (ni, nitems) = joinItem (i { iletter = Just l }) (mitems body)
          -- message depends on who picks up and if a hero can perceive it
          if isPlayer
            then messageAdd (letterLabel (iletter ni) ++ objectItem state ni)
            else when perceived $
                   messageAdd $ subjCompoundVerbIObj state body "pick" "up" i ""
          assertTrue $ removeFromLoc i loc
          -- add item to actor's inventory:
          updateAnyActor actor $ \ m ->
            m { mitems = nitems, mletter = maxLetter l (mletter body) }
        Nothing -> abortIfWith isPlayer "cannot carry any more"
  advanceTime actor

pickupItem :: Action ()
pickupItem = do
  pl <- gets splayer
  actorPickupItem pl

-- TODO: I think that player handlers should be wrappers
-- around more general actor handlers, but
-- the actor handlers should be performing
-- specific actions, i.e., already specify the item to be
-- picked up. It doesn't make sense to invoke dialogues
-- for arbitrary actors, and most likely the
-- decision for a monster is based on perceiving
-- a particular item to be present, so it's already
-- known. In actor handlers we should make sure
-- that messages are printed to the player only if the
-- hero can perceive the action.
-- Perhaps this means half of this code should be split and moved
-- to ItemState, to be independent of any IO code from Action/Display. Actually, not, since the message display depends on Display. Unless we return a string to be displayed.

-- | Let the player choose any item from a list of items.
-- TODO: you can drop an item on the floor, which works correctly,
-- but is weird and useless.
getAnyItem :: String ->  -- prompt
              [Item] ->  -- all objects in question
              String ->  -- how to refer to the collection of objects
              Action (Maybe Item)
getAnyItem prompt is isn = getItem prompt (const True) "Objects" is isn

-- | Let the player choose a single item from a list of items.
getItem :: String ->              -- prompt message
           (Item -> Bool) ->      -- which items to consider suitable
           String ->              -- how to describe suitable objects
           [Item] ->              -- all objects in question
           String ->              -- how to refer to the collection of objects
           Action (Maybe Item)
getItem prompt p ptext is0 isn = do
  lmap  <- gets (lmap . slevel)
  body  <- gets getPlayerBody
  let loc       = mloc body
      t         = lmap `at` loc -- the map tile in question
      tis       = titems t
      floorMsg  = if L.null tis then "" else " -,"
      is = L.filter p is0
      choice = if L.null is
               then "[*," ++ floorMsg ++ " ESC]"
               else let r = letterRange (concatMap (maybeToList . iletter) is)
                    in  "[" ++ r ++ ", ?, *," ++ floorMsg ++ " RET, ESC]"
      interact = do
        when (L.null is0 && L.null tis) $
          abortWith "Not carrying anything."
        messageWipeAndSet (prompt ++ " " ++ choice)
        display
        session nextCommand >>= perform
      perform command = do
        resetMessage
        case command of
          K.Char '?' -> do
            -- filter for supposedly suitable objects
            b <- displayItems (ptext ++ " " ++ isn) True is
            if b then session (getOptionalConfirm (const interact) perform)
                 else interact
          K.Char '*' -> do
            -- show all objects
            b <- displayItems ("Objects " ++ isn) True is0
            if b then session (getOptionalConfirm (const interact) perform)
                 else interact
          K.Char '-' ->
            case tis of
              []   -> return Nothing
              i:rs -> -- use first item; TODO: let player select item
                      return $ Just i
          K.Char l   ->
            return (find (\ i -> maybe False (== l) (iletter i)) is0)
          K.Return   ->  -- TODO: i should be the first displayed (except $)
            return (case is of [] -> Nothing ; i : _ -> Just i)
          _          -> return Nothing
  interact
