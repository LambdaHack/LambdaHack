{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Item UI code with the 'Action' type and everything it depends on
-- that is not already in Action.hs and EffectAction.hs.
-- This file should not depend on Actions.hs.
-- TODO: Add an export list and document after it's rewritten according to #17.
module Game.LambdaHack.ItemAction where

import Control.Monad
import Control.Monad.Writer.Strict (WriterT, lift, tell)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.List as L
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.EffectAction
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Key as K
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.State
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert

default (Text)

-- TODO: When inventory is displayed, let TAB switch the player (without
-- announcing that) and show the inventory of the new player.
-- | Display inventory
inventory :: MonadClientRO m => WriterT Slideshow m ()
inventory = do
  Kind.COps{coactor} <- getsLocal scops
  pbody <- getsLocal getPlayerBody
  items <- getsLocal getPlayerItem
  disco <- getsLocal sdisco
  if L.null items
    then abortWith $ makeSentence
      [ MU.SubjectVerbSg (partActor coactor pbody) "be"
      , "not carrying anything" ]
    else do
      let blurb = makePhrase [MU.Capitalize $
            MU.SubjectVerbSg (partActor coactor pbody) "be carrying:"]
      io <- itemOverlay disco True items
      slides <- overlayToSlideshow blurb io
      tell slides

-- | Let the player choose any item with a given group name.
-- Note that this does not guarantee the chosen item belongs to the group,
-- as the player can override the choice.
getGroupItem :: MonadClient m
             => [Item]   -- ^ all objects in question
             -> MU.Part  -- ^ name of the group
             -> [Char]   -- ^ accepted item symbols
             -> Text     -- ^ prompt
             -> Text     -- ^ how to refer to the collection of objects
             -> m Item
getGroupItem is object syms prompt packName = do
  let choice i = jsymbol i `elem` syms
      header = makePhrase [MU.Capitalize (MU.Ws object)]
  getItem prompt choice header is packName

applyGroupItem :: MonadAction m
               => ActorId  -- ^ actor applying the item (is on current level)
               -> MU.Part  -- ^ how the applying is called
               -> Item     -- ^ the item to be applied
               -> m ()
applyGroupItem actor verb item = do
  Kind.COps{coactor, coitem} <- getsLocal scops
  body  <- getsLocal (getActor actor)
  per   <- askPerception
  disco <- getsLocal sdisco
  -- only one item consumed, even if several in inventory
  let consumed = item { jcount = 1 }
      msg = makeSentence
        [ MU.SubjectVerbSg (partActor coactor body) verb
        , partItemNWs coitem disco consumed ]
      loc = bpos body
  removeFromInventory actor consumed loc
  when (loc `IS.member` totalVisible per) $ msgAdd msg
  itemEffectAction 5 actor actor consumed False

playerApplyGroupItem :: MonadAction m => MU.Part -> MU.Part -> [Char] -> m ()
playerApplyGroupItem verb object syms = do
  Kind.COps{coitem=Kind.Ops{okind}} <- getsLocal scops
  is   <- getsLocal getPlayerItem
  item <- getGroupItem is object syms
            (makePhrase ["What to", verb MU.:> "?"]) "in inventory"
  pl   <- getsLocal splayer
  disco <- getsLocal sdisco
  let verbApply = case jkind disco item of
        Nothing -> verb
        Just ik -> iverbApply $ okind ik
  applyGroupItem pl verbApply item

projectGroupItem :: MonadAction m => ActorId  -- ^ actor projecting the item (is on current lvl)
                 -> Point    -- ^ target position of the projectile
                 -> MU.Part  -- ^ how the projecting is called
                 -> Item     -- ^ the item to be projected
                 -> m ()
projectGroupItem source tpos _verb item = do
  cops@Kind.COps{coactor, coitem} <- getsLocal scops
  sm    <- getsLocal (getActor source)
  per   <- askPerceptionSer
  pl    <- getsLocal splayer
  Actor{btime}  <- getsLocal getPlayerBody
  lvl   <- getsLocal getArena
  ceps  <- getsClient (ceps . scursor)
  lxsize <- getsLocal (lxsize . getArena)
  lysize <- getsLocal (lysize . getArena)
  sside <- getsLocal sside
  disco <- getsLocal sdisco
  let consumed = item { jcount = 1 }
      spos = bpos sm
      svisible = spos `IS.member` totalVisible per
      subject =
        if svisible
        then sm
        else sm {bname = Just "somebody"}
      -- When projecting, the first turn is spent aiming.
      -- The projectile is seen one tile from the actor, giving a hint
      -- about the aim and letting the target evade.
      msg = makeSentence
        [ MU.SubjectVerbSg (partActor coactor subject) "aim"
        , partItemNWs coitem disco consumed ]
      -- TODO: AI should choose the best eps.
      eps = if source == pl then ceps else 0
      -- Setting monster's projectiles time to player time ensures
      -- the projectile covers the whole normal distance already the first
      -- turn that the player observes it moving. This removes
      -- the possibility of micromanagement by, e.g.,  waiting until
      -- the first distance is short.
      -- When the monster faction has its selected player, hero player's
      -- projectiles should be set to the time of the opposite party as well.
      -- Both parties would see their own projectiles move part of the way
      -- and the opposite party's projectiles waiting one turn.
      btimeDelta = timeAddFromSpeed coactor sm btime
      time =
        if bfaction sm == sside || source == pl
        then btimeDelta `timeAdd` timeNegate timeClip
        else btime
      bl = bla lxsize lysize eps spos tpos
  case bl of
    Nothing -> abortWith "cannot zap oneself"
    Just [] -> assert `failure` (spos, tpos, "project from the edge of level")
    Just path@(pos:_) -> do
      let projVis = pos `IS.member` totalVisible per
      removeFromInventory source consumed spos
      inhabitants <- getsGlobal (posToActor pos)
      if accessible cops lvl spos pos && isNothing inhabitants
        then do
          glo <- getGlobal
          ser <- getServer
          let (nglo, nser) =
                addProjectile cops consumed pos (bfaction sm) path time glo ser
          putGlobal nglo
          putServer nser
        else
          abortWith "blocked"
      when (svisible || projVis) $ msgAdd msg

playerProjectGroupItem :: MonadAction m => MU.Part -> MU.Part -> [Char] -> m ()
playerProjectGroupItem verb object syms = do
  ms     <- getsLocal hostileList
  lxsize <- getsLocal (lxsize . getArena)
  lysize <- getsLocal (lysize . getArena)
  ppos   <- getsLocal (bpos . getPlayerBody)
  if foesAdjacent lxsize lysize ppos ms
    then abortWith "You can't aim in melee."
    else playerProjectGI verb object syms

playerProjectGI :: MonadAction m => MU.Part -> MU.Part -> [Char] -> m ()
playerProjectGI verb object syms = do
  cli <- getClient
  pos <- getLocal
  pl    <- getsLocal splayer
  case targetToPos cli pos of
    Just p -> do
      Kind.COps{coitem=Kind.Ops{okind}} <- getsLocal scops
      is   <- getsLocal getPlayerItem
      item <- getGroupItem is object syms
                (makePhrase ["What to", verb MU.:> "?"]) "in inventory"
      targeting <- getsClient (ctargeting . scursor)
      when (targeting == TgtAuto) $ endTargeting True
      disco <- getsLocal sdisco
      let verbProject = case jkind disco item of
            Nothing -> verb
            Just ik -> iverbProject $ okind ik
      projectGroupItem pl p verbProject item
    Nothing -> assert `failure` (pos, pl, "target unexpectedly invalid")

retarget :: MonadAction m => WriterT Slideshow m ()
retarget = do
  ppos <- getsLocal (bpos . getPlayerBody)
  msgAdd "Last target invalid."
  let upd cursor = cursor {cposition=ppos, ceps=0}
  modifyClient (updateCursor upd)
  targetMonster TgtAuto

-- | Start the monster targeting mode. Cycle between monster targets.
targetMonster :: MonadAction m => TgtMode -> WriterT Slideshow m ()
targetMonster tgtMode = do
  pl        <- getsLocal splayer
  ppos      <- getsLocal (bpos . getPlayerBody)
  sside  <- getsLocal sside
  ms        <- getsLocal (hostileAssocs sside . getArena)
  per       <- askPerception
  lxsize    <- getsLocal (lxsize . getArena)
  target <- getsClient (IM.lookup pl . starget)
  targeting <- getsClient (ctargeting . scursor)
      -- TODO: sort monsters by distance to the player.
  let plms = L.filter ((/= pl) . fst) ms  -- don't target yourself
      ordPos (_, m) = (chessDist lxsize ppos $ bpos m, bpos m)
      dms = L.sortBy (comparing ordPos) plms
      (lt, gt) = case target of
            Just (TEnemy n _) | targeting /= TgtOff ->  -- pick the next monster
              let i = fromMaybe (-1) $ L.findIndex ((== n) . fst) dms
              in L.splitAt (i + 1) dms
            Just (TEnemy n _) ->  -- try to retarget the old monster
              let i = fromMaybe (-1) $ L.findIndex ((== n) . fst) dms
              in L.splitAt i dms
            _ -> (dms, [])  -- target first monster (e.g., number 0)
      gtlt     = gt ++ lt
      seen (_, m) =
        let mpos = bpos m
        in mpos `IS.member` totalVisible per  -- visible by any
           && actorReachesLoc pl mpos per     -- reachable by player
      lf = L.filter seen gtlt
      tgt = case lf of
              [] -> target  -- no monsters in sight, stick to last target
              (na, nm) : _ -> Just (TEnemy na (bpos nm))  -- pick the next
  -- Register the chosen monster, to pick another on next invocation.
  modifyClient $ updateTarget pl (const tgt)
  setCursor tgtMode

-- | Start the floor targeting mode or reset the cursor position to the player.
targetFloor :: MonadAction m => TgtMode -> WriterT Slideshow m ()
targetFloor tgtMode = do
  ppos      <- getsLocal (bpos . getPlayerBody)
  pl        <- getsLocal splayer
  target <- getsClient (IM.lookup pl . starget)
  targeting <- getsClient (ctargeting . scursor)
  let tgt = case target of
        Just (TEnemy _ _) -> Nothing  -- forget enemy target, keep the cursor
        _ | targeting /= TgtOff -> Just (TPos ppos)  -- double key press: reset cursor
        t -> t  -- keep the target from previous targeting session
  -- Register that we want to target only positions.
  modifyClient $ updateTarget pl (const tgt)
  setCursor tgtMode

-- | Set, activate and display cursor information.
setCursor :: MonadClient m => TgtMode -> WriterT Slideshow m ()
setCursor tgtMode = assert (tgtMode /= TgtOff) $ do
  pos  <- getLocal
  cli  <- getClient
  ppos   <- getsLocal (bpos . getPlayerBody)
  cposLn <- getsLocal sarena
  let upd cursor@Cursor{ctargeting, cposition=cpositionOld, ceps=cepsOld} =
        let cposition =
              fromMaybe ppos (targetToPos cli pos)
            ceps = if cposition == cpositionOld then cepsOld else 0
            newTgtMode = if ctargeting == TgtOff then tgtMode else ctargeting
        in cursor { ctargeting = newTgtMode, cposition, cposLn, ceps }
  modifyClient (updateCursor upd)
  doLook

-- | Tweak the @eps@ parameter of the targeting digital line.
epsIncr :: MonadClient m => Bool -> m ()
epsIncr b = do
  targeting <- getsClient (ctargeting . scursor)
  if targeting /= TgtOff
    then modifyClient $ updateCursor $
           \ c@Cursor{ceps} -> c {ceps = ceps + if b then 1 else -1}
    else neverMind True  -- no visual feedback, so no sense

-- | End targeting mode, accepting the current position or not.
endTargeting :: MonadAction m => Bool -> m ()
endTargeting accept = do
  returnLn <- getsClient (creturnLn . scursor)
  pl <- getsLocal splayer
  target <- getsClient (IM.lookup pl . starget)
  per      <- askPerception
  cpos     <- getsClient (cposition . scursor)
  sside <- getsLocal sside
  ms       <- getsLocal (hostileAssocs sside . getArena)
  -- Return to the original level of the player. Note that this can be
  -- a different level than the one we started targeting at,
  -- if the player was changed while targeting.
  switchLevel returnLn
  modifyClient (updateCursor (\ c -> c { ctargeting = TgtOff }))
  when accept $ do
    case target of
      Just TEnemy{} -> do
        -- If in monster targeting mode, switch to the monster under
        -- the current cursor position, if any.
        let canSee = IS.member cpos (totalVisible per)
        when (accept && canSee) $
          case L.find (\ (_im, m) -> bpos m == cpos) ms of
            Just (im, m)  ->
              let tgt = Just $ TEnemy im (bpos m)
              in modifyClient $ updateTarget pl (const $ tgt)
            Nothing -> return ()
      _ -> modifyClient $ updateTarget pl (const $ Just $ TPos cpos)
  if accept
    then endTargetingMsg
    else msgAdd "targeting canceled"

endTargetingMsg :: MonadClient m => m ()
endTargetingMsg = do
  Kind.COps{coactor} <- getsLocal scops
  pbody  <- getsLocal getPlayerBody
  pl <- getsLocal splayer
  target <- getsClient (IM.lookup pl . starget)
  state  <- getLocal
  lxsize <- getsLocal (lxsize . getArena)
  let targetMsg = case target of
                    Just (TEnemy a _ll) ->
                      if memActor a state
                      then partActor coactor $ getActor a state
                      else "a fear of the past"
                    Just (TPos pos) ->
                      MU.Text $ "position" <+> showPoint lxsize pos
                    Nothing -> "current cursor position continuously"
  msgAdd $ makeSentence
      [MU.SubjectVerbSg (partActor coactor pbody) "target", targetMsg]

-- | Cancel something, e.g., targeting mode, resetting the cursor
-- to the position of the player. Chosen target is not invalidated.
cancelCurrent :: MonadAction m => WriterT Slideshow m () -> WriterT Slideshow m ()
cancelCurrent h = do
  targeting <- getsClient (ctargeting . scursor)
  if targeting /= TgtOff
    then lift $ endTargeting False
    else h  -- nothing to cancel right now, treat this as a command invocation

-- | Accept something, e.g., targeting mode, keeping cursor where it was.
-- Or perform the default action, if nothing needs accepting.
acceptCurrent :: MonadAction m => WriterT Slideshow m () -> WriterT Slideshow m ()
acceptCurrent h = do
  targeting <- getsClient (ctargeting . scursor)
  if targeting /= TgtOff
    then lift $ endTargeting True
    else h  -- nothing to accept right now, treat this as a command invocation

-- | Clear current messages, show the next screen if any.
clearCurrent :: MonadActionRoot m => m ()
clearCurrent = return ()

-- | Drop a single item.
dropItem :: MonadAction m => m ()
dropItem = do
  -- TODO: allow dropping a given number of identical items.
  Kind.COps{coactor, coitem} <- getsLocal scops
  pl    <- getsLocal splayer
  pbody <- getsLocal getPlayerBody
  ppos  <- getsLocal (bpos . getPlayerBody)
  ims   <- getsLocal getPlayerItem
  stack <- getAnyItem "What to drop?" ims "in inventory"
  disco <- getsLocal sdisco
  let item = stack { jcount = 1 }
  removeOnlyFromInventory pl item (bpos pbody)
  msgAdd $ makeSentence
    [ MU.SubjectVerbSg (partActor coactor pbody) "drop"
    , partItemNWs coitem disco item ]
  modifyLocal (updateArena (dropItemsAt [item] ppos))
  modifyGlobal (updateArena (dropItemsAt [item] ppos))  -- a hack

-- TODO: this is a hack for dropItem, because removeFromInventory
-- makes it impossible to drop items if the floor not empty.
removeOnlyFromInventory :: MonadAction m => ActorId -> Item -> Point -> m ()
removeOnlyFromInventory actor i _pos = do
  modifyLocal (updateAnyActorItem actor (removeItemByLetter i))
  modifyGlobal (updateAnyActorItem actor (removeItemByLetter i))  -- a hack

-- | Remove given item from an actor's inventory or floor.
-- TODO: this is subtly wrong: if identical items are on the floor and in
-- inventory, the floor one will be chosen, regardless of player intention.
-- TODO: right now it ugly hacks (with the ppos) around removing items
-- of dead heros/monsters. The subtle incorrectness helps here a lot,
-- because items of dead heroes land on the floor, so we use them up
-- in inventory, but remove them after use from the floor.
removeFromInventory :: MonadAction m => ActorId -> Item -> Point -> m ()
removeFromInventory actor i pos = do
  b <- removeFromPos i pos
  unless b $ do
    modifyLocal (updateAnyActorItem actor (removeItemByLetter i))
    modifyGlobal (updateAnyActorItem actor (removeItemByLetter i))  -- a hack

-- | Remove given item from the given position. Tell if successful.
removeFromPos :: MonadServer m => Item -> Point -> m Bool
removeFromPos i pos = do
  lvl <- getsGlobal getArena
  if not $ L.any (equalItemIdentity i) (lvl `atI` pos)
    then return False
    else do
      modifyGlobal (updateArena (updateIMap adj))
      return True
     where
      rib Nothing = assert `failure` (i, pos)
      rib (Just is) =
        case removeItemByIdentity i is of
          [] -> Nothing
          x  -> Just x
      adj = IM.alter rib pos

actorPickupItem :: MonadAction m => ActorId -> m ()
actorPickupItem actor = do
  Kind.COps{coactor, coitem} <- getsGlobal scops
  pl <- getsGlobal splayer
  per <- askPerception
  lvl <- getsGlobal getArena
  body <- getsGlobal (getActor actor)
  bitems <- getsGlobal (getActorItem actor)
  disco <- getsLocal sdisco
  let p       = bpos body
      perceived = p `IS.member` totalVisible per
      isPlayer  = actor == pl
  -- check if something is here to pick up
  case lvl `atI` p of
    []   -> abortWith "nothing here"
    i:is -> -- pick up first item; TODO: let pl select item; not for monsters
      case assignLetter (jletter i) (bletter body) bitems of
        Just l -> do
          let (ni, nitems) = joinItem (i { jletter = Just l }) bitems
          -- msg depends on who picks up and if a hero can perceive it
          if isPlayer
            then msgAdd $ makePhrase [ letterLabel (jletter ni)
                                     , partItemNWs coitem disco ni ]
            else when perceived $
                   msgAdd $ makeSentence
                     [ MU.SubjectVerbSg (partActor coactor body) "pick up"
                     , partItemNWs coitem disco i ]
          removeFromPos i p
            >>= assert `trueM` (i, is, p, "item is stuck")
          -- add item to actor's inventory:
          updateAnyActor actor $ \ m ->
            m { bletter = maxLetter l (bletter body) }
          modifyGlobal (updateAnyActorItem actor (const nitems))
        Nothing -> abortWith "cannot carry any more"

pickupItem :: MonadAction m => m ()
pickupItem = do
  pl <- getsLocal splayer
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

-- TODO: you can drop an item already on the floor, which works correctly,
-- but is weird and useless.

allObjectsName :: Text
allObjectsName = "Objects"

-- | Let the player choose any item from a list of items.
getAnyItem :: MonadClient m
           => Text    -- ^ prompt
           -> [Item]  -- ^ all items in question
           -> Text    -- ^ how to refer to the collection of items
           -> m Item
getAnyItem prompt = getItem prompt (const True) allObjectsName

data ItemDialogState = INone | ISuitable | IAll deriving Eq

-- | Let the player choose a single, preferably suitable,
-- item from a list of items.
getItem :: MonadClient m
        => Text            -- ^ prompt message
        -> (Item -> Bool)  -- ^ which items to consider suitable
        -> Text            -- ^ how to describe suitable items
        -> [Item]          -- ^ all items in question
        -> Text            -- ^ how to refer to the collection of items
        -> m Item
getItem prompt p ptext is0 isn = do
  lvl  <- getsLocal getArena
  body <- getsLocal getPlayerBody
  let pos = bpos body
      tis = lvl `atI` pos
      floorFull = not $ null tis
      (floorMsg, floorKey) | floorFull = (", -", [K.Char '-'])
                           | otherwise = ("", [])
      isp = L.filter p is0
      bestFull = not $ null isp
      (bestMsg, bestKey)
        | bestFull =
          let bestLetter = maybe "" (\ l -> "(" <> T.singleton l <> ")") $
                             jletter $ L.maximumBy cmpItemLM isp
          in (", RET" <> bestLetter, [K.Return])
        | otherwise = ("", [])
      cmpItemLM i1 i2 = cmpLetterMaybe (jletter i1) (jletter i2)
      keys ims =
        let mls = mapMaybe jletter ims
            ks = bestKey ++ floorKey ++ [K.Char '?'] ++ map K.Char mls
        in zip ks $ repeat K.NoModifier
      choice ims =
        if null ims
        then "[?" <> floorMsg
        else let mls = mapMaybe jletter ims
                 r = letterRange mls
             in "[" <> r <> ", ?" <> floorMsg <> bestMsg
      ask = do
        when (L.null is0 && L.null tis) $
          abortWith "Not carrying anything."
        perform INone
      perform itemDialogState = do
        let (ims, imsOver, msg) = case itemDialogState of
              INone     -> (isp, [], prompt)
              ISuitable -> (isp, isp, ptext <+> isn <> ".")
              IAll      -> (is0, is0, allObjectsName <+> isn <> ".")
        disco <- getsLocal sdisco
        io <- itemOverlay disco True imsOver
        (command, modifier) <-
          displayChoiceUI (msg <+> choice ims) io (keys ims)
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
            k -> assert `failure` "perform: unexpected key:" <+> showT k
  ask
