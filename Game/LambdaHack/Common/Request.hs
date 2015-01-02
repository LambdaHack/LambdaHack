{-# LANGUAGE DataKinds, ExistentialQuantification, GADTs, KindSignatures,
             StandaloneDeriving #-}
-- | Abstract syntax of server commands.
-- See
-- <https://github.com/LambdaHack/LambdaHack/wiki/Client-server-architecture>.
module Game.LambdaHack.Common.Request
  ( RequestAI(..), RequestUI(..), RequestTimed(..), RequestAnyAbility(..)
  , ReqFailure(..), impossibleReqFailure, showReqFailure, anyToUI
  , permittedPrecious, permittedProject, permittedApply
  ) where

import Data.Text (Text)

import Game.LambdaHack.Atomic
import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Content.TileKind as TK

-- TODO: make remove second arg from ReqLeader; this requires a separate
-- channel for Ping, probably, and then client sends as many commands
-- as it wants at once
-- | Cclient-server requests sent by AI clients.
data RequestAI =
    forall a. ReqAITimed !(RequestTimed a)
  | ReqAILeader !ActorId !(Maybe Target) !RequestAI
  | ReqAIPong

deriving instance Show RequestAI

-- | Client-server requests sent by UI clients.
data RequestUI =
    forall a. ReqUITimed !(RequestTimed a)
  | ReqUILeader !ActorId !(Maybe Target) !RequestUI
  | ReqUIGameRestart !ActorId !(GroupName ModeKind) !Int ![(Int, (Text, Text))]
  | ReqUIGameExit !ActorId !Int
  | ReqUIGameSave
  | ReqUITactic !Tactic
  | ReqUIAutomate
  | ReqUIPong [CmdAtomic]

deriving instance Show RequestUI

data RequestAnyAbility = forall a. RequestAnyAbility !(RequestTimed a)

deriving instance Show RequestAnyAbility

anyToUI :: RequestAnyAbility -> RequestUI
anyToUI (RequestAnyAbility cmd) = ReqUITimed cmd

-- | Client-server requests that take game time. Sent by both AI and UI clients.
data RequestTimed :: Ability -> * where
  ReqMove :: !Vector -> RequestTimed AbMove
  ReqMelee :: !ActorId -> !ItemId -> !CStore -> RequestTimed AbMelee
  ReqDisplace :: !ActorId -> RequestTimed AbDisplace
  ReqAlter :: !Point -> !(Maybe TK.Feature) -> RequestTimed AbAlter
  ReqWait :: RequestTimed AbWait
  ReqMoveItems :: ![(ItemId, Int, CStore, CStore)] -> RequestTimed AbMoveItem
  ReqProject :: !Point -> !Int -> !ItemId -> !CStore -> RequestTimed AbProject
  ReqApply :: !ItemId -> !CStore -> RequestTimed AbApply
  ReqTrigger :: !(Maybe TK.Feature) -> RequestTimed AbTrigger

deriving instance Show (RequestTimed a)

data ReqFailure =
    MoveNothing
  | MeleeSelf
  | MeleeDistant
  | DisplaceDistant
  | DisplaceAccess
  | DisplaceProjectiles
  | DisplaceDying
  | DisplaceBraced
  | DisplaceImmobile
  | DisplaceSupported
  | AlterDistant
  | AlterBlockActor
  | AlterBlockItem
  | AlterNothing
  | EqpOverfull
  | ApplyUnskilled
  | ApplyRead
  | ApplyOutOfReach
  | ApplyCharging
  | ItemNothing
  | ItemNotCalm
  | NotCalmPrecious
  | ProjectAimOnself
  | ProjectBlockTerrain
  | ProjectBlockActor
  | ProjectUnskilled
  | ProjectFragile
  | ProjectOutOfReach
  | TriggerNothing
  | NoChangeDunLeader
  | NoChangeLvlLeader

impossibleReqFailure :: ReqFailure -> Bool
impossibleReqFailure reqFailure = case reqFailure of
  MoveNothing -> True
  MeleeSelf -> True
  MeleeDistant -> True
  DisplaceDistant -> True
  DisplaceAccess -> True
  DisplaceProjectiles -> True
  DisplaceDying -> True
  DisplaceBraced -> True
  DisplaceImmobile -> False  -- unidentified skill items
  DisplaceSupported -> True
  AlterDistant -> True
  AlterBlockActor -> True  -- adjacent actor always visible
  AlterBlockItem -> True  -- adjacent item always visible
  AlterNothing -> True
  EqpOverfull -> True
  ApplyUnskilled -> False  -- unidentified skill items
  ApplyRead -> False  -- unidentified skill items
  ApplyOutOfReach -> True
  ApplyCharging -> False  -- if aspects unknown, charging unknown
  ItemNothing -> True
  ItemNotCalm -> False  -- unidentified skill items
  NotCalmPrecious -> False  -- unidentified skill items
  ProjectAimOnself -> True
  ProjectBlockTerrain -> True  -- adjacent terrain always visible
  ProjectBlockActor -> True  -- adjacent actor always visible
  ProjectUnskilled -> False  -- unidentified skill items
  ProjectFragile -> False  -- unidentified skill items
  ProjectOutOfReach -> True
  TriggerNothing -> True  -- terrain underneath always visibl
  NoChangeDunLeader -> True
  NoChangeLvlLeader -> True

showReqFailure :: ReqFailure -> Msg
showReqFailure reqFailure = case reqFailure of
  MoveNothing -> "wasting time on moving into obstacle"
  MeleeSelf -> "trying to melee oneself"
  MeleeDistant -> "trying to melee a distant foe"
  DisplaceDistant -> "trying to displace a distant actor"
  DisplaceAccess -> "switching places without access"
  DisplaceProjectiles -> "trying to displace multiple projectiles"
  DisplaceDying -> "trying to displace a dying foe"
  DisplaceBraced -> "trying to displace a braced foe"
  DisplaceImmobile -> "trying to displace an immoblie foe"
  DisplaceSupported -> "trying to displace a supported foe"
  AlterDistant -> "trying to alter a distant tile"
  AlterBlockActor -> "blocked by an actor"
  AlterBlockItem -> "jammed by an item"
  AlterNothing -> "wasting time on altering nothing"
  EqpOverfull -> "cannot equip any more items"
  ApplyUnskilled -> "unskilled actors cannot activate items"
  ApplyRead -> "activating this kind of items requires skill level 2"
  ApplyOutOfReach -> "cannot activate an item out of reach"
  ApplyCharging -> "cannot activate an item that is still charging"
  ItemNothing -> "wasting time on void item manipulation"
  ItemNotCalm -> "you are too alarmed to sort through the shared stash"
  NotCalmPrecious -> "you are too alarmed to handle such an exquisite item"
  ProjectAimOnself -> "cannot aim at oneself"
  ProjectBlockTerrain -> "aiming obstructed by terrain"
  ProjectBlockActor -> "aiming blocked by an actor"
  ProjectUnskilled -> "unskilled actors cannot aim"
  ProjectFragile -> "to lob a fragile item requires fling skill 2"
  ProjectOutOfReach -> "cannot aim an item out of reach"
  TriggerNothing -> "wasting time on triggering nothing"
  NoChangeDunLeader -> "no manual level change for your team"
  NoChangeLvlLeader -> "no manual leader change for your team"

-- The item should not be activated nor thrown because it's too delicate
-- to operate when not calm or becuse it's too precious to identify by use.
permittedPrecious :: Bool -> Bool -> ItemFull -> Either ReqFailure Bool
permittedPrecious calm10 forced itemFull =
  let isPrecious = IK.Precious `elem` jfeature (itemBase itemFull)
  in if not calm10 && not forced && isPrecious then Left NotCalmPrecious
     else Right $ IK.Durable `elem` jfeature (itemBase itemFull)
                  || case itemDisco itemFull of
                    Just ItemDisco{itemAE=Just _} -> True
                    _ -> not isPrecious

permittedProject :: [Char] -> Bool -> Int -> ItemFull -> Actor -> [ItemFull]
                 -> Either ReqFailure Bool
permittedProject triggerSyms forced skill itemFull@ItemFull{itemBase}
                 b activeItems =
  let calm10 = calmEnough10 b activeItems
  in if not forced
        && skill < 1 then Left ProjectUnskilled
  else if not forced
          && IK.Fragile `elem` jfeature itemBase
          && skill < 2 then Left ProjectFragile
  else
    let legal = permittedPrecious calm10 forced itemFull
    in case legal of
      Left{} -> legal
      Right False -> legal
      Right True -> Right $
        let hasEffects = case itemDisco itemFull of
              Just ItemDisco{itemAE=Just ItemAspectEffect{jeffects=[]}} -> False
              Just ItemDisco{ itemAE=Nothing
                            , itemKind=IK.ItemKind{IK.ieffects=[]} } -> False
              _ -> True
            permittedSlot =
              if ' ' `elem` triggerSyms
              then case strengthEqpSlot itemBase of
                Just (IK.EqpSlotAddLight, _) -> True
                Just _ -> False
                Nothing -> True
              else jsymbol itemBase `elem` triggerSyms
        in hasEffects && permittedSlot

permittedApply :: [Char] -> Time -> Int -> ItemFull -> Actor -> [ItemFull]
               -> Either ReqFailure Bool
permittedApply triggerSyms localTime skill itemFull@ItemFull{itemBase}
               b activeItems =
  let calm10 = calmEnough10 b activeItems
  in if skill < 1 then Left ApplyUnskilled
  else if jsymbol itemBase == '?' && skill < 2 then Left ApplyRead
  else if not $ hasCharge localTime itemFull
       then Left ApplyCharging
       else let legal = permittedPrecious calm10 False itemFull
            in case legal of
              Left{} -> legal
              Right False -> legal
              Right True -> Right $
                if ' ' `elem` triggerSyms
                then IK.Applicable `elem` jfeature itemBase
                else jsymbol itemBase `elem` triggerSyms
