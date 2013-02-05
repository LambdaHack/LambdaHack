-- | Semantics of 'CmdAction' server commands.
-- TODO: document
module Game.LambdaHack.Server.CmdAtomicSem where

import Control.Monad
import Control.Monad.Reader.Class
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.List
import Data.Maybe
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Action
import Game.LambdaHack.Actor
import Game.LambdaHack.ActorState
import Game.LambdaHack.CmdCli
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Content.FactionKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.TileKind as TileKind
import Game.LambdaHack.Faction
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.Item
import qualified Game.LambdaHack.Kind as Kind
import Game.LambdaHack.Level
import Game.LambdaHack.Msg
import Game.LambdaHack.Perception
import Game.LambdaHack.Point
import Game.LambdaHack.Random
import Game.LambdaHack.Server.Action
import Game.LambdaHack.Server.CmdAtomic
import Game.LambdaHack.Server.Config
import Game.LambdaHack.Server.State
import Game.LambdaHack.State
import qualified Game.LambdaHack.Tile as Tile
import Game.LambdaHack.Time
import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Utils.Frequency
import Game.LambdaHack.Vector

cmdAtomicSem :: MonadAction m => CmdAtomic -> m ()
cmdAtomicSem cmd = case cmd of
  HealAtomic n aid -> healAtomic n aid

healAtomic :: MonadAction m => Int -> ActorId -> m ()
healAtomic n aid = assert (n /= 0) $
  modifyState $ updateActorBody aid $ \b -> b {bhp = n + bhp b}

hasteAtomic :: MonadAction m => ActorId -> Speed -> m ()
hasteAtomic aid delta = assert (delta /= speedZero) $ do
  Kind.COps{coactor=Kind.Ops{okind}} <- getsState scops
  modifyState $ updateActorBody aid $ \ b ->
    let innateSpeed = aspeed $ okind $ bkind b
        curSpeed = fromMaybe innateSpeed (bspeed b)
        newSpeed = speedAdd curSpeed delta
    in assert (newSpeed >= speedZero `blame` (aid, curSpeed, delta)) $
       if curSpeed == innateSpeed
       then b {bspeed = Nothing}
       else b {bspeed = Just newSpeed}

dominateAtomic :: MonadAction m => FactionId -> FactionId -> ActorId -> m ()
dominateAtomic fromFaction toFaction target = do
  tm <- getsState (getActorBody target)
  assert (fromFaction == bfaction tm `blame` (fromFaction, tm, toFaction)) $
    modifyState $ updateActorBody target $ \b -> b {bfaction = toFaction}

-- TODO: perhaps assert that the inventory of the actor is empty
-- or at least that the items belong to litem.
spawnAtomic :: MonadAction m => ActorId -> Actor -> m ()
spawnAtomic aid body = modifyState $ insertActor aid body

-- TODO: perhaps assert that the inventory of the actor is empty.
killAtomic :: MonadAction m => ActorId -> Actor -> m ()
killAtomic aid _body = modifyState $ deleteActor aid

setSmellAtomic :: MonadAction m => SmellMap -> SmellMap -> m ()
setSmellAtomic _fromSmell toSmell = do
  modifyState $ updateArena $ updateSmell $ const toSmell

alterSmellAtomic :: MonadAction m => Point -> Maybe Time -> Maybe Time -> m ()
alterSmellAtomic pos oldS newS = do
  let f old = assert (oldS == old `blame` (pos, old, oldS)) newS
  modifyState $ updateArena $ updateSmell $ EM.alter f pos

-- | Create a few copies of an item. The item may already be present
-- on the level, in which case only some more copies are added.
createItemAtomic :: MonadAction m
                 => ItemId -> Item -> Int -> Container -> m ()
createItemAtomic iid item k container = assert (k > 0) $ do
  let f item1 item2 = assert (item1 == item2) item1
  modifyState $ updateArena $ updateItem $ EM.insertWith f iid item
  let l = if jsymbol item == '$' then Just $ InvChar '$' else Nothing
      bag = EM.singleton iid k
  case container of
    CFloor pos -> do
      let mergeBag = EM.insertWith (EM.unionWith (+)) pos bag
      modifyState $ updateArena $ updateFloor mergeBag
    CActor aid -> do
      body <- getsState $ getActorBody aid
      case assignLetter iid l body of
        Nothing -> createItemAtomic iid item k (CFloor $ bpos body)
        Just l2 -> do
          let upd = EM.unionWith (+) bag
          modifyState $ updateArena $ updateActor
            $ EM.adjust (\b -> b {bbag = upd (bbag b)}) aid
          modifyState $ updateArena $ updateActor
            $ EM.adjust (\b -> b {binv = EM.insert l2 iid (binv b)}) aid
          modifyState $ updateActorBody aid $ \b ->
            b {bletter = max l2 (bletter b)}

-- | Destroy some copies (possibly not all) of an item.
destroyItemAtomic :: MonadAction m
                  => ItemId -> Item -> Int -> Container -> m ()
destroyItemAtomic iid item k container = assert (k > 0) $ do
  -- TODO: check if the item appears anywhere else and GC it
  let rmFromBag (Just bag) = Just $ removeFromBag k iid bag
      rmFromBag Nothing = assert `failure` (iid, item, k, container)
  case container of
    CFloor pos ->
      modifyState $ updateArena $ updateFloor $ EM.alter rmFromBag pos
    CActor aid ->
      modifyState $ updateArena $ updateActor
      $ EM.adjust (\b -> b {bbag = removeFromBag k iid (bbag b)}) aid
      -- Actor's @bletter@ for UI not reset. This is OK up to isomorphism.
