{-# LANGUAGE DeriveGeneric #-}
-- | Possible causes of failure of request.
module Game.LambdaHack.Common.ReqFailure
  ( ReqFailure(..)
  , impossibleReqFailure, showReqFailure
  , permittedPrecious, permittedProject, permittedProjectAI, permittedApply
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Data.Binary
import GHC.Generics (Generic)

import           Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.ItemAspect as IA
import           Game.LambdaHack.Common.Time
import qualified Game.LambdaHack.Content.ItemKind as IK
import qualified Game.LambdaHack.Definition.Ability as Ability
import           Game.LambdaHack.Definition.Defs

-- | Possible causes of failure of request.
data ReqFailure =
    MoveUnskilled
  | MoveUnskilledAsleep
  | MoveNothing
  | MeleeUnskilled
  | MeleeSelf
  | MeleeDistant
  | DisplaceUnskilled
  | DisplaceDistant
  | DisplaceAccess
  | DisplaceMultiple
  | DisplaceDying
  | DisplaceBraced
  | DisplaceImmobile
  | DisplaceSupported
  | AlterUnskilled
  | AlterUnwalked
  | AlterDistant
  | AlterBlockActor
  | AlterBlockItem
  | AlterNothing
  | CloseDistant
  | CloseClosed
  | CloseNothing
  | CloseNonClosable
  | WaitUnskilled
  | YellUnskilled
  | MoveItemUnskilled
  | EqpOverfull
  | EqpStackFull
  | ApplyUnskilled
  | ApplyFood
  | ApplyRead
  | ApplyPeriodic
  | ApplyOutOfReach
  | ApplyCharging
  | ApplyNoEffects
  | ItemNothing
  | ItemNotCalm
  | ItemOverStash
  | NotCalmPrecious
  | ProjectUnskilled
  | ProjectAimOnself
  | ProjectBlockTerrain
  | ProjectBlockActor
  | ProjectLobable
  | ProjectOutOfReach
  | NoChangeDunLeader
  deriving (Show, Eq, Generic)

instance Binary ReqFailure

impossibleReqFailure :: ReqFailure -> Bool
impossibleReqFailure reqFailure = case reqFailure of
  MoveUnskilled -> False  -- unidentified skill items
  MoveUnskilledAsleep -> False  -- unidentified skill items
  MoveNothing -> True
  MeleeUnskilled -> False  -- unidentified skill items
  MeleeSelf -> True
  MeleeDistant -> True
  DisplaceUnskilled -> False  -- unidentified skill items
  DisplaceDistant -> True
  DisplaceAccess -> True
  DisplaceMultiple -> True
  DisplaceDying -> True
  DisplaceBraced -> True
  DisplaceImmobile -> False  -- unidentified skill items
  DisplaceSupported -> False
  AlterUnskilled -> False  -- unidentified skill items
  AlterUnwalked -> False
  AlterDistant -> True
  AlterBlockActor -> True  -- adjacent actor always visible
  AlterBlockItem -> True  -- adjacent item always visible
  AlterNothing -> True  -- if tile known, its properties known
  CloseDistant -> True
  CloseClosed -> True
  CloseNothing -> True
  CloseNonClosable -> True
  WaitUnskilled -> False  -- unidentified skill items
  YellUnskilled -> False  -- unidentified skill items
  MoveItemUnskilled -> False  -- unidentified skill items
  EqpOverfull -> True
  EqpStackFull -> True
  ApplyUnskilled -> False  -- unidentified skill items
  ApplyFood -> False  -- unidentified skill items
  ApplyRead -> False  -- unidentified skill items
  ApplyPeriodic -> False  -- unidentified skill items
  ApplyOutOfReach -> True
  ApplyCharging -> False  -- if aspect record unknown, charging unknown
  ApplyNoEffects -> False  -- if effects unknown, can't prevent it
  ItemNothing -> True
  ItemNotCalm -> False  -- unidentified skill items
  ItemOverStash -> True
  NotCalmPrecious -> False  -- unidentified skill items
  ProjectUnskilled -> False  -- unidentified skill items
  ProjectAimOnself -> True
  ProjectBlockTerrain -> True  -- adjacent terrain always visible
  ProjectBlockActor -> True  -- adjacent actor always visible
  ProjectLobable -> False  -- unidentified skill items
  ProjectOutOfReach -> True
  NoChangeDunLeader -> True

showReqFailure :: ReqFailure -> Text
showReqFailure reqFailure = case reqFailure of
  MoveUnskilled -> "too low movement stat; use equipment menu to take off stat draining gear or switch to another teammate or wait until a stat draining condition passes as seen in organ menu"
  MoveUnskilledAsleep -> "actor asleep; yawn to wake up"
  MoveNothing -> "wasting time on moving into obstacle"
  MeleeUnskilled -> "too low melee combat stat"
  MeleeSelf -> "trying to melee oneself"
  MeleeDistant -> "trying to melee a distant foe"
  DisplaceUnskilled -> "too low actor displacing stat"
  DisplaceDistant -> "trying to displace a distant actor"
  DisplaceAccess -> "trying to switch places without access"
  DisplaceMultiple -> "trying to displace multiple actors"
  DisplaceDying -> "trying to displace a dying foe"
  DisplaceBraced -> "trying to displace a braced foe"
  DisplaceImmobile -> "trying to displace an immobile foe"
  DisplaceSupported -> "trying to displace a foe supported by teammates or supply stash"
  AlterUnskilled -> "modify stat is needed to search or activate or transform terrain"
  AlterUnwalked -> "too low modify stat to enter or activate or transform terrain; find and equip gear that improves the stat or try with a teammate whose skills menu shows a higher stat"
  AlterDistant -> "trying to modify distant terrain"
  AlterBlockActor -> "blocked by an actor"
  AlterBlockItem -> "jammed by an item"
  AlterNothing -> "wasting time on modifying nothing"
  CloseDistant -> "trying to close a distant terrain"
  CloseClosed -> "already closed"
  CloseNothing -> "no adjacent terrain can be closed"
  CloseNonClosable -> "cannot be closed"
  WaitUnskilled -> "too low wait stat"
  YellUnskilled -> "actors unskilled in waiting cannot yell/yawn"
  MoveItemUnskilled -> "too low item moving stat"
  EqpOverfull -> "cannot equip any more items"
  EqpStackFull -> "cannot equip the whole item stack"
  ApplyUnskilled -> "too low item triggering stat"
  ApplyFood -> "trigger stat 1 is enough only to eat food from the ground"
  ApplyRead -> "activating cultural artifacts requires trigger stat 3"
  ApplyPeriodic -> "manually activating periodic items requires trigger stat 4"
  ApplyOutOfReach -> "cannot trigger an item out of reach"
  ApplyCharging -> "cannot trigger an item that is still charging"
  ApplyNoEffects -> "cannot trigger an item that produces no effect"
  ItemNothing -> "wasting time on void item manipulation"
  ItemNotCalm -> "you try to organize equipment but your calm fails you"
  ItemOverStash -> "you roll in your hoard a little"
  NotCalmPrecious -> "you are too distracted to handle such an exquisite item"
  ProjectUnskilled -> "too low item flinging stat"
  ProjectAimOnself -> "cannot aim at oneself"
  ProjectBlockTerrain -> "aiming obstructed by terrain"
  ProjectBlockActor -> "aiming blocked by an actor"
  ProjectLobable -> "flinging a lobable item that stops at target position requires fling stat 3"
  ProjectOutOfReach -> "cannot aim an item out of reach"
  NoChangeDunLeader -> "no manual level change for your team"

-- The item should not be applied nor thrown because it's too delicate
-- to operate when not calm or because it's too precious to identify by use.
permittedPrecious :: Bool -> Bool -> ItemFull -> Either ReqFailure Bool
permittedPrecious forced calmE itemFull@ItemFull{itemDisco} =
  let arItem = aspectRecordFull itemFull
      isPrecious = IA.checkFlag Ability.Precious arItem
  in if not forced && not calmE && isPrecious
     then Left NotCalmPrecious
     else Right $ IA.checkFlag Ability.Durable arItem
                  || case itemDisco of
                       ItemDiscoFull{} -> True
                       _ -> not isPrecious

-- Simplified, faster version, for inner AI loop.
permittedPreciousAI :: Bool -> ItemFull -> Bool
permittedPreciousAI calmE itemFull@ItemFull{itemDisco} =
  let arItem = aspectRecordFull itemFull
      isPrecious = IA.checkFlag Ability.Precious arItem
  in (calmE || not isPrecious)
     && IA.checkFlag Ability.Durable arItem
        || case itemDisco of
             ItemDiscoFull{} -> True
             _ -> not isPrecious

permittedProject :: Bool -> Int -> Bool -> ItemFull -> Either ReqFailure Bool
permittedProject forced skill calmE itemFull =
 let arItem = aspectRecordFull itemFull
 in if | not forced && skill < 1 -> Left ProjectUnskilled
       | not forced
         && IA.checkFlag Ability.Lobable arItem
         && skill < 3 -> Left ProjectLobable
       | otherwise -> permittedPrecious forced calmE itemFull

-- Simplified, faster and more permissive version, for inner AI loop.
permittedProjectAI :: Int -> Bool -> ItemFull -> Bool
permittedProjectAI skill calmE itemFull =
 let arItem = aspectRecordFull itemFull
 in if | skill < 1 -> False
       | IA.checkFlag Ability.Lobable arItem
         && skill < 3 -> False
       | otherwise -> permittedPreciousAI calmE itemFull

permittedApply :: Time -> Int -> Bool -> Maybe CStore -> ItemFull -> ItemQuant
               -> Either ReqFailure Bool
permittedApply localTime skill calmE mstore
               itemFull@ItemFull{itemKind, itemSuspect} kit =
  if | skill < 1 -> Left ApplyUnskilled
     | skill < 2
       && (mstore /= Just CGround
           || IK.isymbol itemKind `notElem` [',', '"']) -> Left ApplyFood
     | skill < 3 && IK.isymbol itemKind == '?' -> Left ApplyRead
     | skill < 4
       && let arItem = aspectRecordFull itemFull
          in IA.checkFlag Ability.Periodic arItem -> Left ApplyPeriodic
     -- If the item is discharged, neither the kinetic hit nor
     -- any effects activate, so there's no point triggering.
     -- Note that if client doesn't know the timeout, here we may leak the fact
     -- that the item is still charging, but the client risks destruction
     -- if the item is, in fact, recharged and is not durable
     -- (likely in case of jewellery). So it's OK (the message may be
     -- somewhat alarming though).
     | not $ hasCharge localTime kit -> Left ApplyCharging
     | otherwise ->
       if null (IK.ieffects itemKind) && not itemSuspect
       then Left ApplyNoEffects
       else permittedPrecious False calmE itemFull
