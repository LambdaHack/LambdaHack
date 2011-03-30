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
inventory =
  do
    items <- gets (mitems . getPlayerBody)
    if L.null items
      then abortWith "You are not carrying anything"
      else do
             displayItems "This is what you are carrying:" True items
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

applyGroupItem :: String ->  -- name of the group
                  String ->  -- how the "applying" is called
                  Action ()
applyGroupItem groupName verb = do
  state <- get
  pbody <- gets getPlayerBody
  is    <- gets (mitems . getPlayerBody)
  if L.null is
    then abortWith "You are not carrying anything."
    else do
      iOpt <- getGroupItem is groupName
              ("What to " ++ verb ++ "?") "in your inventory"
      case iOpt of
        Just item@(Item { ikind = ik }) -> do
          -- only one item consumed, even if several in inventory
          let v = if ItemKind.jname (ItemKind.getIK ik) == groupName
                  then verb
                  else "creatively apply"
              consumed = item { icount = 1 }
          message (subjectVerbIObject state pbody v consumed "")
          pl <- gets splayer
          b <- itemEffectAction consumed pl pl
          when b $ removeFromInventory consumed
        Nothing -> neverMind True
  playerAdvanceTime

zapGroupItem :: String ->  -- name of the group
                String ->  -- how the "applying" is called
                Action ()
zapGroupItem groupName verb = do
  state  <- get
  per    <- currentPerception
  pbody  <- gets getPlayerBody
  is     <- gets (mitems . getPlayerBody)
  target <- gets (mtarget . getPlayerBody)
  pl     <- gets splayer
  if L.null is
    then abortWith "You are not carrying anything."
    else do
      iOpt <- getGroupItem is groupName
              ("What to " ++ verb ++ "?") "in your inventory"
      case iOpt of
        Just item@(Item { ikind = ik }) -> do
          -- only one item consumed, even if several in inventory
          let v = if ItemKind.jname (ItemKind.getIK ik) == groupName
                  then verb
                  else "furiously zap"
              consumed = item { icount = 1 }
          removeFromInventory consumed
          case targetToLoc (ptvisible per) state of
            Nothing  -> abortWith "target invalid"
            Just loc ->
              -- TODO: draw digital line and see if obstacles prevent firing
              if actorReachesLoc pl loc per pl
              then case locToActor loc state of
                     Just ta -> do
                       b <- itemEffectAction consumed pl ta
                       when (not b) $
                         modify (updateLevel (scatterItems [consumed] loc))
                     Nothing -> do
                       message (subjectVerbIObject state pbody verb consumed "")
                       modify (updateLevel (scatterItems [consumed] loc))
              else abortWith "target not reachable"
        Nothing -> neverMind True
  playerAdvanceTime

quaffPotion :: Action ()
quaffPotion = applyGroupItem "potion" "quaff"

readScroll :: Action ()
readScroll  = applyGroupItem "scroll" "read"

aimItem :: Action ()
aimItem = zapGroupItem "wand" "aim"

throwItem :: Action ()
throwItem = zapGroupItem "dart" "throw"

dropItem :: Action ()
dropItem =
  do
    state <- get
    pbody <- gets getPlayerBody
    ploc <- gets (mloc . getPlayerBody)
    items <- gets (mitems . getPlayerBody)
    if L.null items
      then abortWith "You are not carrying anything."
      else do
             i <- getAnyItem "What to drop?" items "inventory"
             case i of
               Just i' ->
                 do
                   removeFromInventory i'
                   message (subjectVerbIObject state pbody "drop" i' "")
                   dropItemsAt [i'] ploc
               Nothing -> neverMind True
    playerAdvanceTime

-- | Remove given item from the hero's inventory.
removeFromInventory :: Item -> Action ()
removeFromInventory i =
  updatePlayerBody (\ p -> p { mitems = removeItemByLetter i (mitems p) })

-- | Remove given item from the given location.
removeFromLoc :: Item -> Loc -> Action ()
removeFromLoc i loc =
  modify (updateLevel (updateLMap adj))
  where
    adj = M.adjust (\ (t, rt) -> (remove t, rt)) loc
    remove t = t { titems = removeItemByIdentity i (titems t) }

actorPickupItem :: Actor -> Action ()
actorPickupItem actor =
  do
    state <- get
    pl    <- gets splayer
    per   <- currentPerception
    lmap  <- gets (lmap . slevel)
    movable <- gets (getActor actor)
    let loc       = mloc movable
    let t         = lmap `at` loc -- the map tile in question
    let perceived = loc `S.member` ptvisible per
    let isPlayer  = actor == pl
    -- check if something is here to pick up
    case titems t of
      []     -> abortIfWith isPlayer "nothing here"
      (i:rs) -> -- pick up first item; TODO: let player select item; not for monsters
        case assignLetter (iletter i) (mletter movable) (mitems movable) of
          Just l ->
            do
              let (ni, nitems) = joinItem (i { iletter = Just l }) (mitems movable)
              -- message is dependent on who picks up and if the hero can perceive it
              if isPlayer
                then message (letterLabel (iletter ni) ++ objectItem state ni)
                else when perceived $
                       message $ subjectCompoundVerbIObject state movable "pick" "up" i ""
              removeFromLoc i loc
              -- add item to actor's inventory:
              updateAnyActor actor $ \ m ->
                m { mitems = nitems, mletter = maxLetter l (mletter movable) }
          Nothing -> abortIfWith isPlayer "you cannot carry any more"
    advanceTime actor

pickupItem :: Action ()
pickupItem = do
  pl <- gets splayer
  actorPickupItem pl

-- TODO: I think that player handlers should be wrappers around more general actor handlers, but
-- the actor handlers should be performing specific actions, i.e., already specify the item to be
-- picked up. It doesn't make sense to invoke dialogues for arbitrary actors, and most likely the
-- decision for a monster is based on perceiving a particular item to be present, so it's already
-- known. In actor handlers we should make sure that messages are printed to the player only if the
-- hero can perceive the action.

-- | Let the player choose any item from a list of items.
getAnyItem :: String ->  -- prompt
              [Item] ->  -- all objects in question
              String ->  -- how to refer to the collection of objects, e.g. "in your inventory"
              Action (Maybe Item)
getAnyItem prompt is isn = getItem prompt (const True) "Objects" is isn

-- | Let the player choose a single item from a list of items.
getItem :: String ->              -- prompt message
           (Item -> Bool) ->      -- which items to consider suitable
           String ->              -- how to describe suitable objects
           [Item] ->              -- all objects in question
           String ->              -- how to refer to the collection of objects, e.g. "in your inventory"
           Action (Maybe Item)
getItem prompt p ptext is0 isn =
  let is = L.filter p is0
      choice | L.null is = "[*, ESC]"
             | otherwise = "[" ++ letterRange (concatMap (maybeToList . iletter) is) ++ ", ?, *, RET, ESC]"
      r = do
            message (prompt ++ " " ++ choice)
            display
            let h = session nextCommand >>= h'
                h' e = case e of
                         K.Char '?' -> do
                                         -- filter for supposedly suitable objects
                                         b <- displayItems (ptext ++ " " ++ isn) True is
                                         if b then session (getOptionalConfirm (const r) h')
                                              else r
                         K.Char '*' -> do
                                         -- show all objects
                                         b <- displayItems ("Objects " ++ isn) True is0
                                         if b then session (getOptionalConfirm (const r) h')
                                              else r
                         K.Char l   -> return (find (\ i -> maybe False (== l) (iletter i)) is0)
                         K.Return   -> return (case is of [] -> Nothing ; i : _ -> Just i)
                         _          -> return Nothing
            h
  in r
