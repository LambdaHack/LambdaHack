-- | Item UI code with the 'Action' type and everything it depends on
-- that is not already in Action.hs and EffectAction.hs.
-- This file should not depend on Actions.hs.
-- TODO: Add an export list and document after it's rewritten according to #17.
module Game.LambdaHack.ItemAction where

import Control.Monad
import Control.Monad.State hiding (State, state)
import qualified Data.List as L
import qualified Data.IntMap as IM
import Data.Maybe
import Data.Ord
import qualified Data.IntSet as IS

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Action
import Game.LambdaHack.Point
import Game.LambdaHack.Grammar
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Key as K
import Game.LambdaHack.Level
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Perception
import Game.LambdaHack.State
import Game.LambdaHack.EffectAction
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Time

-- | Display inventory
inventory :: ActionFrame ()
inventory = do
  items <- gets getPlayerItem
  if L.null items
    then abortWith "Not carrying anything."
    else do
      io <- itemOverlay True items
      displayOverlays "Carrying:" io

-- | Let the player choose any item with a given group name.
-- Note that this does not guarantee the chosen item belongs to the group,
-- as the player can override the choice.
getGroupItem :: [Item]  -- ^ all objects in question
             -> Object  -- ^ name of the group
             -> [Char]  -- ^ accepted item symbols
             -> String  -- ^ prompt
             -> String  -- ^ how to refer to the collection of objects
             -> Action Item
getGroupItem is object syms prompt packName = do
  Kind.COps{coitem=Kind.Ops{osymbol}} <- getCOps
  let choice i = osymbol (jkind i) `elem` syms
      header = capitalize $ suffixS object
  getItem prompt choice header is packName

applyGroupItem :: ActorId  -- ^ actor applying the item (is on current level)
               -> Verb     -- ^ how the applying is called
               -> Item     -- ^ the item to be applied
               -> Action ()
applyGroupItem actor verb item = do
  cops  <- getCOps
  state <- get
  body  <- gets (getActor actor)
  per   <- getPerception
  -- only one item consumed, even if several in inventory
  let consumed = item { jcount = 1 }
      msg = actorVerbItemExtra cops state body verb consumed ""
      loc = bloc body
  removeFromInventory actor consumed loc
  when (loc `IS.member` totalVisible per) $ msgAdd msg
  itemEffectAction 5 actor actor consumed

playerApplyGroupItem :: Verb -> Object -> [Char] -> Action ()
playerApplyGroupItem verb object syms = do
  Kind.COps{coitem=Kind.Ops{okind}} <- getCOps
  is   <- gets getPlayerItem
  item <- getGroupItem is object syms
            ("What to " ++ verb ++ "?") "in inventory"
  pl   <- gets splayer
  applyGroupItem pl (iverbApply $ okind $ jkind item) item

projectGroupItem :: ActorId  -- ^ actor projecting the item (is on current lvl)
                 -> Point    -- ^ target location of the projectile
                 -> Verb     -- ^ how the projecting is called
                 -> Item     -- ^ the item to be projected
                 -> Action ()
projectGroupItem source tloc _verb item = do
  cops@Kind.COps{coactor} <- getCOps
  state <- get
  sm    <- gets (getActor source)
  per   <- getPerception
  pl    <- gets splayer
  body  <- gets getPlayerBody
  lvl   <- gets slevel
  ceps  <- gets (ceps . scursor)
  lxsize <- gets (lxsize . slevel)
  lysize <- gets (lysize . slevel)
  let consumed = item { jcount = 1 }
      sloc = bloc sm
      sourceVis = sloc `IS.member` totalVisible per
      subject =
        if sourceVis
        then sm
        else template (heroKindId coactor)
               Nothing (Just "somebody") 99 sloc timeZero neutralParty
      -- When projecting, the first turn is spent aiming.
      -- The projectile is seen one tile from the actor, giving a hint
      -- about the aim and letting the target evade.
      msg = actorVerbItemExtra cops state subject "aim" consumed ""
      -- TODO: AI should choose the best eps.
      eps = if source == pl then ceps else 0
      party = if bparty sm == heroParty
              then neutralParty
              else monsterParty
      bl = bla lxsize lysize eps sloc tloc
  case bl of
    Nothing -> abortWith "cannot zap oneself"
    Just [] -> assert `failure` (sloc, tloc, "project from the edge of level")
    Just path@(loc:_) -> do
      let projVis = loc `IS.member` totalVisible per
      removeFromInventory source consumed sloc
      inhabitants <- gets (locToActor loc)
      if accessible cops lvl sloc loc && isNothing inhabitants
        -- Setting the projectile time to player time is brutal, but ensures
        -- the projectile covers the whole normal distance already the first
        -- step that the player observes it moving. This removes
        -- the possibility of micromanagement by, e.g.,  waiting until
        -- the first distance is short.
        -- If and when monster all move at once, the projectile should be set
        -- to the tile of the opposite party. Then it would be symmetric.
        -- Both parties would see their projectiles move part of the way
        -- and the opposite projectile waiting one step.
        then
          modify $ addProjectile cops consumed loc party path (btime body)
        else
          abortWith "blocked"
      when (sourceVis || projVis) $ msgAdd msg

playerProjectGroupItem :: Verb -> Object -> [Char] -> ActionFrame ()
playerProjectGroupItem verb object syms = do
  ms     <- gets levelMonsterList   -- TODO: exclude projectiles already here
  lxsize <- gets (lxsize . slevel)
  ploc   <- gets (bloc . getPlayerBody)
  if L.any (adjacent lxsize ploc) $ L.map bloc $ L.filter ((> 0) . bhp) ms
    then abortWith "You can't aim in melee."
    else playerProjectGI verb object syms

playerProjectGI :: Verb -> Object -> [Char] -> ActionFrame ()
playerProjectGI verb object syms = do
  state <- get
  pl    <- gets splayer
  ploc  <- gets (bloc . getPlayerBody)
  per   <- getPerception
  let retarget msg = do
        msgAdd msg
        let upd cursor = cursor {clocation=ploc, ceps=0}
        modify (updateCursor upd)
        frs <- targetMonster TgtAuto
        -- Mark that unexpectedly it does not take time.
        modify (\ s -> s {snoTime = True})
        return frs
  case targetToLoc (totalVisible per) state ploc of
    Just loc -> do
      Kind.COps{coitem=Kind.Ops{okind}} <- getCOps
      is   <- gets getPlayerItem
      item <- getGroupItem is object syms
                ("What to " ++ verb ++ "?") "in inventory"
      targeting <- gets (ctargeting . scursor)
      when (targeting == TgtAuto) $ endTargeting True
      projectGroupItem pl loc (iverbProject $ okind $ jkind item) item
      returnNoFrame ()
    Nothing -> retarget "Last target invalid."

-- | Start the monster targeting mode. Cycle between monster targets.
targetMonster :: TgtMode -> ActionFrame ()
targetMonster tgtMode = do
  pl        <- gets splayer
  ploc      <- gets (bloc . getPlayerBody)
  ms        <- gets (monsterAssocs . slevel)
  per       <- getPerception
  lxsize    <- gets (lxsize . slevel)
  target    <- gets (btarget . getPlayerBody)
  targeting <- gets (ctargeting . scursor)
      -- TODO: sort monsters by distance to the player.
  let plms = L.filter ((/= pl) . fst) ms  -- don't target yourself
      ordLoc (_, m) = (chessDist lxsize ploc $ bloc m, bloc m)
      dms = L.sortBy (comparing ordLoc) plms
      (lt, gt) = case target of
            TEnemy n _ | targeting /= TgtOff ->  -- pick the next monster
              let i = fromMaybe (-1) $ L.findIndex ((== n) . fst) dms
              in L.splitAt (i + 1) dms
            TEnemy n _ ->  -- try to retarget the old monster
              let i = fromMaybe (-1) $ L.findIndex ((== n) . fst) dms
              in L.splitAt i dms
            _ -> (dms, [])  -- target first monster (e.g., number 0)
      gtlt     = gt ++ lt
      seen (_, m) =
        let mloc = bloc m
        in mloc `IS.member` totalVisible per         -- visible by any
           && actorReachesLoc pl mloc per (Just pl)  -- reachable by player
      lf = L.filter seen gtlt
      tgt = case lf of
              [] -> target  -- no monsters in sight, stick to last target
              (na, nm) : _ -> TEnemy na (bloc nm)  -- pick the next
  -- Register the chosen monster, to pick another on next invocation.
  updatePlayerBody (\ p -> p { btarget = tgt })
  setCursor tgtMode

-- | Start the floor targeting mode or reset the cursor location to the player.
targetFloor :: TgtMode -> ActionFrame ()
targetFloor tgtMode = do
  ploc      <- gets (bloc . getPlayerBody)
  target    <- gets (btarget . getPlayerBody)
  targeting <- gets (ctargeting . scursor)
  let tgt = case target of
        TEnemy _ _ -> TCursor  -- forget enemy target, keep the cursor
        _ | targeting /= TgtOff -> TLoc ploc  -- double key press: reset cursor
        TPath _ -> TCursor
        t -> t  -- keep the target from previous targeting session
  -- Register that we want to target only locations.
  updatePlayerBody (\ p -> p { btarget = tgt })
  setCursor tgtMode

-- | Set, activate and display cursor information.
setCursor :: TgtMode -> ActionFrame ()
setCursor tgtMode = assert (tgtMode /= TgtOff) $ do
  state  <- get
  per    <- getPerception
  ploc   <- gets (bloc . getPlayerBody)
  clocLn <- gets slid
  let upd cursor@Cursor{ctargeting, clocation=clocationOld, ceps=cepsOld} =
        let clocation =
              fromMaybe ploc (targetToLoc (totalVisible per) state ploc)
            ceps = if clocation == clocationOld then cepsOld else 0
            newTgtMode = if ctargeting == TgtOff then tgtMode else ctargeting
        in cursor { ctargeting = newTgtMode, clocation, clocLn, ceps }
  modify (updateCursor upd)
  doLook

-- | Tweak the @eps@ parameter of the targeting digital line.
epsIncr :: Bool -> Action ()
epsIncr b =
  modify $ updateCursor $
    \ c@Cursor{ceps} -> c {ceps = ceps + if b then 1 else -1}

-- | End targeting mode, accepting the current location or not.
endTargeting :: Bool -> Action ()
endTargeting accept = do
  returnLn <- gets (creturnLn . scursor)
  target   <- gets (btarget . getPlayerBody)
  per      <- getPerception
  cloc     <- gets (clocation . scursor)
  ms       <- gets (monsterAssocs . slevel)
  -- Return to the original level of the player. Note that this can be
  -- a different level than the one we started targeting at,
  -- if the player was changed while targeting.
  switchLevel returnLn
  modify (updateCursor (\ c -> c { ctargeting = TgtOff }))
  when accept $ do
    case target of
      TEnemy _ _ -> do
        -- If in monster targeting mode, switch to the monster under
        -- the current cursor location, if any.
        let canSee = IS.member cloc (totalVisible per)
        when (accept && canSee) $
          case L.find (\ (_im, m) -> bloc m == cloc) ms of
            Just (im, m)  ->
              let tgt = TEnemy im (bloc m)
              in updatePlayerBody (\ p -> p { btarget = tgt })
            Nothing -> return ()
      _ -> updatePlayerBody (\ p -> p { btarget = TLoc cloc })
  if accept
    then endTargetingMsg
    else msgAdd "targeting canceled"

endTargetingMsg :: Action ()
endTargetingMsg = do
  Kind.COps{coactor} <- getCOps
  pbody  <- gets getPlayerBody
  state  <- get
  lxsize <- gets (lxsize . slevel)
  let verb = "target"
      targetMsg = case btarget pbody of
                    TEnemy a _ll ->
                      if memActor a state
                      then objectActor coactor $ getActor a state
                      else "a fear of the past"
                    TLoc loc -> "location " ++ showPoint lxsize loc
                    TPath _ -> "a path"
                    TCursor  -> "current cursor position continuously"
  msgAdd $ actorVerbExtra coactor pbody verb targetMsg

-- | Cancel something, e.g., targeting mode, resetting the cursor
-- to the position of the player. Chosen target is not invalidated.
cancelCurrent :: Action ()
cancelCurrent = do
  targeting <- gets (ctargeting . scursor)
  if targeting /= TgtOff
    then endTargeting False
    else abortWith "Press Q to quit."

-- | Accept something, e.g., targeting mode, keeping cursor where it was.
-- Or perform the default action, if nothing needs accepting.
acceptCurrent :: ActionFrame () -> ActionFrame ()
acceptCurrent h = do
  targeting <- gets (ctargeting . scursor)
  if targeting /= TgtOff
    then inFrame $ endTargeting True
    else h  -- nothing to accept right now

-- | Drop a single item.
dropItem :: Action ()
dropItem = do
  -- TODO: allow dropping a given number of identical items.
  cops  <- getCOps
  pl    <- gets splayer
  state <- get
  pbody <- gets getPlayerBody
  ploc  <- gets (bloc . getPlayerBody)
  ims   <- gets getPlayerItem
  stack <- getAnyItem "What to drop?" ims "in inventory"
  let item = stack { jcount = 1 }
  removeOnlyFromInventory pl item (bloc pbody)
  msgAdd (actorVerbItemExtra cops state pbody "drop" item "")
  modify (updateLevel (dropItemsAt [item] ploc))

-- TODO: this is a hack for dropItem, because removeFromInventory
-- makes it impossible to drop items if the floor not empty.
removeOnlyFromInventory :: ActorId -> Item -> Point -> Action ()
removeOnlyFromInventory actor i _loc =
  modify (updateAnyActorItem actor (removeItemByLetter i))

-- | Remove given item from an actor's inventory or floor.
-- TODO: this is subtly wrong: if identical items are on the floor and in
-- inventory, the floor one will be chosen, regardless of player intention.
-- TODO: right now it ugly hacks (with the ploc) around removing items
-- of dead heros/monsters. The subtle incorrectness helps here a lot,
-- because items of dead heroes land on the floor, so we use them up
-- in inventory, but remove them after use from the floor.
removeFromInventory :: ActorId -> Item -> Point -> Action ()
removeFromInventory actor i loc = do
  b <- removeFromLoc i loc
  unless b $
    modify (updateAnyActorItem actor (removeItemByLetter i))

-- | Remove given item from the given location. Tell if successful.
removeFromLoc :: Item -> Point -> Action Bool
removeFromLoc i loc = do
  lvl <- gets slevel
  if not $ L.any (equalItemIdentity i) (lvl `atI` loc)
    then return False
    else
      modify (updateLevel (updateIMap adj)) >>
      return True
     where
      rib Nothing = assert `failure` (i, loc)
      rib (Just (is, irs)) =
        case (removeItemByIdentity i is, irs) of
          ([], []) -> Nothing
          iss -> Just iss
      adj = IM.alter rib loc

actorPickupItem :: ActorId -> Action ()
actorPickupItem actor = do
  cops@Kind.COps{coitem} <- getCOps
  state <- get
  pl    <- gets splayer
  per   <- getPerception
  lvl   <- gets slevel
  body  <- gets (getActor actor)
  bitems <- gets (getActorItem actor)
  let loc       = bloc body
      perceived = loc `IS.member` totalVisible per
      isPlayer  = actor == pl
  -- check if something is here to pick up
  case lvl `atI` loc of
    []   -> abortWith "nothing here"
    i:is -> -- pick up first item; TODO: let pl select item; not for monsters
      case assignLetter (jletter i) (bletter body) bitems of
        Just l -> do
          let (ni, nitems) = joinItem (i { jletter = Just l }) bitems
          -- msg depends on who picks up and if a hero can perceive it
          if isPlayer
            then msgAdd (letterLabel (jletter ni)
                         ++ objectItem coitem state ni)
            else when perceived $
                   msgAdd $
                   actorVerbExtraItemExtra cops state body "pick" "up" i ""
          removeFromLoc i loc
            >>= assert `trueM` (i, is, loc, "item is stuck")
          -- add item to actor's inventory:
          updateAnyActor actor $ \ m ->
            m { bletter = maxLetter l (bletter body) }
          modify (updateAnyActorItem actor (const nitems))
        Nothing -> abortWith "cannot carry any more"

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

-- TODO: you can drop an item already the floor, which works correctly,
-- but is weird and useless.

allObjectsName :: String
allObjectsName = "Objects"

-- | Let the player choose any item from a list of items.
getAnyItem :: String  -- ^ prompt
           -> [Item]  -- ^ all items in question
           -> String  -- ^ how to refer to the collection of items
           -> Action Item
getAnyItem prompt = getItem prompt (const True) allObjectsName

data ItemDialogState = INone | ISuitable | IAll deriving Eq

-- | Let the player choose a single, preferably suitable,
-- item from a list of items.
getItem :: String               -- ^ prompt message
        -> (Item -> Bool)       -- ^ which items to consider suitable
        -> String               -- ^ how to describe suitable items
        -> [Item]               -- ^ all items in question
        -> String               -- ^ how to refer to the collection of items
        -> Action Item
getItem prompt p ptext is0 isn = do
  lvl  <- gets slevel
  body <- gets getPlayerBody
  let loc = bloc body
      tis = lvl `atI` loc
      floorFull = not $ null tis
      (floorMsg, floorKey) | floorFull = (", -", [K.Char '-'])
                           | otherwise = ("", [])
      isp = L.filter p is0
      bestFull = not $ null isp
      (bestMsg, bestKey)
        | bestFull =
          let bestLetter = maybe "" (\ l -> ['(', l, ')']) $
                             jletter $ L.maximumBy cmpItemLM isp
          in (", RET" ++ bestLetter, [K.Return])
        | otherwise = ("", [])
      cmpItemLM i1 i2 = cmpLetterMaybe (jletter i1) (jletter i2)
      keys ims =
        let mls = mapMaybe jletter ims
            ks = bestKey ++ floorKey ++ [K.Char '?'] ++ map K.Char mls
        in zip ks $ repeat K.NoModifier
      choice ims =
        if null ims
        then "[?" ++ floorMsg
        else let mls = mapMaybe jletter ims
                 r = letterRange mls
             in "[" ++ r ++ ", ?" ++ floorMsg ++ bestMsg
      ask = do
        when (L.null is0 && L.null tis) $
          abortWith "Not carrying anything."
        perform INone
      perform itemDialogState = do
        let (ims, imsOver, msg) = case itemDialogState of
              INone     -> (isp, [], prompt ++ " ")
              ISuitable -> (isp, isp, ptext ++ " " ++ isn ++ ". ")
              IAll      -> (is0, is0, allObjectsName ++ " " ++ isn ++ ". ")
        io <- itemOverlay True imsOver
        (command, modifier) <-
          displayChoiceUI (msg ++ choice ims) io (keys ims)
        assert (modifier == K.NoModifier) $
          case command of
            K.Char '?' -> case itemDialogState of
              INone -> perform ISuitable
              ISuitable | ptext /= allObjectsName -> perform IAll
              _ -> perform INone
            K.Char '-' | floorFull ->
              -- TODO: let player select item
              return $ L.maximumBy cmpItemLM tis
            K.Char l | l `elem` mapMaybe jletter ims ->
              let mitem = L.find (maybe False (== l) . jletter) ims
              in return $ fromJust mitem
            K.Return | bestFull ->
              return $ L.maximumBy cmpItemLM isp
            k -> assert `failure` "perform: unexpected key: " ++ show k
  ask
