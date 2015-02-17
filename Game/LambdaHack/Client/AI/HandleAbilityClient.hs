{-# LANGUAGE DataKinds #-}
-- | Semantics of abilities in terms of actions and the AI procedure
-- for picking the best action for an actor.
module Game.LambdaHack.Client.AI.HandleAbilityClient
  ( actionStrategy
  ) where

import Control.Applicative
import Control.Arrow (second)
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import Data.Ratio
import Data.Text (Text)

import Game.LambdaHack.Client.AI.ConditionClient
import Game.LambdaHack.Client.AI.Preferences
import Game.LambdaHack.Client.AI.Strategy
import Game.LambdaHack.Client.BfsClient
import Game.LambdaHack.Client.CommonClient
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Frequency
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemStrongest
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Perception
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Content.TileKind as TK

type ToAny a = Strategy (RequestTimed a) -> Strategy RequestAnyAbility

toAny :: ToAny a
toAny strat = RequestAnyAbility <$> strat

-- | AI strategy based on actor's sight, smell, etc.
-- Never empty.
actionStrategy :: forall m. MonadClient m
               => ActorId -> m (Strategy RequestAnyAbility)
actionStrategy aid = do
  body <- getsState $ getActorBody aid
  activeItems <- activeItemsClient aid
  fact <- getsState $ (EM.! bfid body) . sfactionD
  condTgtEnemyPresent <- condTgtEnemyPresentM aid
  condTgtEnemyRemembered <- condTgtEnemyRememberedM aid
  condTgtEnemyAdjFriend <- condTgtEnemyAdjFriendM aid
  condAnyFoeAdj <- condAnyFoeAdjM aid
  threatDistL <- threatDistList aid
  condHpTooLow <- condHpTooLowM aid
  condOnTriggerable <- condOnTriggerableM aid
  condBlocksFriends <- condBlocksFriendsM aid
  condNoEqpWeapon <- condNoEqpWeaponM aid
  let condNoUsableWeapon = all (not . isMelee) activeItems
  condFloorWeapon <- condFloorWeaponM aid
  condCanProject <- condCanProjectM aid
  condNotCalmEnough <- condNotCalmEnoughM aid
  condDesirableFloorItem <- condDesirableFloorItemM aid
  condMeleeBad <- condMeleeBadM aid
  aInAmbient <- getsState $ actorInAmbient body
  fleeL <- fleeList 1 aid
  panic2FleeL <- fleeList 2 aid
  panic3FleeL <- fleeList 3 aid
  let actorShines = sumSlotNoFilter IK.EqpSlotAddLight activeItems > 0
      condThreatAdj = not $ null $ takeWhile ((== 1) . fst) threatDistL
      condThreatAtHand = not $ null $ takeWhile ((<= 2) . fst) threatDistL
      condThreatNearby = not $ null $ takeWhile ((<= 7) . fst) threatDistL
      speed1_5 = speedScale (3%2) (bspeed body activeItems)
      condFastThreatAdj = any (\(_, (_, b)) -> bspeed b activeItems > speed1_5)
                          $ takeWhile ((== 1) . fst) threatDistL
      heavilyDistressed =  -- actor hit by a proj or similarly distressed
        deltaSerious (bcalmDelta body)
  actorSk <- actorSkillsClient aid
  let stratToFreq :: MonadStateRead m
                  => Int -> m (Strategy RequestAnyAbility)
                  -> m (Frequency RequestAnyAbility)
      stratToFreq scale mstrat = do
        st <- mstrat
        return $! scaleFreq scale $ bestVariant st  -- TODO: flatten instead?
      -- Order matters within the list, because it's summed with .| after
      -- filtering. Also, the results of prefix, distant and suffix
      -- are summed with .| at the end.
      prefix, suffix :: [([Ability], m (Strategy RequestAnyAbility), Bool)]
      prefix =
        [ ( [AbApply], (toAny :: ToAny AbApply)
            <$> applyItem aid ApplyFirstAid
          , condHpTooLow && not condAnyFoeAdj
            && not condOnTriggerable )  -- don't block stairs, perhaps ascend
        , ( [AbTrigger], (toAny :: ToAny AbTrigger)
            <$> trigger aid True
              -- flee via stairs, even if to wrong level
              -- may return via different stairs
          , condOnTriggerable
            && ((condNotCalmEnough || condHpTooLow)
                && condThreatNearby && not condTgtEnemyPresent
                || condMeleeBad && condThreatAdj) )
        , ( [AbMove]
          , flee aid fleeL
          , condMeleeBad && condThreatAtHand
            && not condFastThreatAdj && not (null fleeL) )
        , ( [AbDisplace]
          , displaceFoe aid  -- only swap with an enemy to expose him
          , condBlocksFriends && condAnyFoeAdj
            && not condOnTriggerable && not condDesirableFloorItem )
        , ( [AbMoveItem], (toAny :: ToAny AbMoveItem)
            <$> pickup aid True
          , condNoEqpWeapon && condFloorWeapon && not condHpTooLow
            && EM.findWithDefault 0 AbMelee actorSk > 0 )
        , ( [AbMelee], (toAny :: ToAny AbMelee)
            <$> meleeBlocker aid  -- only melee target or blocker
          , condAnyFoeAdj
            || EM.findWithDefault 0 AbDisplace actorSk <= 0
                 -- melee friends, not displace
               && fleaderMode (gplayer fact) == LeaderNull  -- not restrained
               && (condTgtEnemyPresent || condTgtEnemyRemembered) )  -- excited
        , ( [AbTrigger], (toAny :: ToAny AbTrigger)
            <$> trigger aid False
          , condOnTriggerable && not condDesirableFloorItem )
        , ( [AbDisplace]  -- prevents some looping movement
          , displaceBlocker aid  -- fires up only when path blocked
          , not condDesirableFloorItem )
        , ( [AbMoveItem], (toAny :: ToAny AbMoveItem)
            <$> equipItems aid  -- doesn't take long, very useful if safe
                                -- only if calm enough, so high priority
          , not condAnyFoeAdj && not condDesirableFloorItem )
        ]
      -- Order doesn't matter, scaling does.
      distant :: [([Ability], m (Frequency RequestAnyAbility), Bool)]
      distant =
        [ ( [AbProject]  -- for high-value target, shoot even in melee
          , stratToFreq 2 $ (toAny :: ToAny AbProject)
            <$> projectItem aid
          , condTgtEnemyPresent && condCanProject && not condOnTriggerable )
        , ( [AbApply]
          , stratToFreq 2 $ (toAny :: ToAny AbApply)
            <$> applyItem aid ApplyAll  -- use any potion or scroll
          , (condTgtEnemyPresent || condThreatNearby)  -- can affect enemies
            && not condOnTriggerable )
        , ( [AbMove]
          , stratToFreq (if not condTgtEnemyPresent
                         then 3  -- if enemy only remembered, investigate anyway
                         else if condTgtEnemyAdjFriend
                         then 1000  -- friends probably pummeled, go to help
                         else 100)
            $ chase aid True (condMeleeBad && condThreatNearby
                              && not aInAmbient && not actorShines)
          , (condTgtEnemyPresent || condTgtEnemyRemembered)
            && not condDesirableFloorItem
            && EM.findWithDefault 0 AbMelee actorSk > 0
            && not condNoUsableWeapon )
        ]
      -- Order matters again.
      suffix =
        [ ( [AbMoveItem], (toAny :: ToAny AbMoveItem)
            <$> pickup aid False
          , not condThreatAtHand )  -- e.g., to give to other party members
        , ( [AbMove]
          , flee aid fleeL
          , condMeleeBad && condThreatNearby && not condFastThreatAdj
            && (condNotCalmEnough
                || condThreatAtHand) )
        , ( [AbMelee], (toAny :: ToAny AbMelee)
            <$> meleeAny aid  -- avoid getting damaged for naught
          , condAnyFoeAdj )
        , ( [AbMove]
            -- TODO: forget old target (e.g., tile), to start shooting,
            -- unless can't shoot, etc.
          , flee aid panic2FleeL  -- panic mode; chasing would be pointless
          , condMeleeBad && condThreatNearby && not condFastThreatAdj
            && not heavilyDistressed  -- don't run in circles if shot at
            && (condNotCalmEnough
                || condThreatAtHand
                || condNoUsableWeapon) )
        , ( [AbMove]
          , flee aid panic3FleeL  -- ultimate panic mode, runs into foes
          , condMeleeBad && condThreatNearby && not condFastThreatAdj
            && not heavilyDistressed  -- don't run in circles if shot at
            && (condNotCalmEnough
                || condThreatAtHand
                || condNoUsableWeapon) )
        , ( [AbMoveItem], (toAny :: ToAny AbMoveItem)
            <$> unEquipItems aid  -- late, because better to throw than unequip
          , True )
        , ( [AbMove]
          , chase aid False (condTgtEnemyPresent
                             && condMeleeBad && condThreatNearby
                             && not aInAmbient && not actorShines)
          , True )
        ]
      fallback =
        [ ( [AbWait], (toAny :: ToAny AbWait)
            <$> waitBlockNow
            -- Wait until friends sidestep; ensures strategy is never empty.
            -- TODO: try to switch leader away before that (we already
            -- switch him afterwards)
          , True )
        ]
      -- TODO: don't msum not to evaluate until needed
      abInSkill ab = EM.findWithDefault 0 ab actorSk > 0
      checkAction :: ([Ability], m a, Bool) -> Bool
      checkAction (abts, _, cond) = all abInSkill abts && cond
      sumS abAction = do
        let as = filter checkAction abAction
        strats <- mapM (\(_, m, _) -> m) as
        return $! msum strats
      sumF abFreq = do
        let as = filter checkAction abFreq
        strats <- mapM (\(_, m, _) -> m) as
        return $! msum strats
      combineDistant as = liftFrequency <$> sumF as
  sumPrefix <- sumS prefix
  comDistant <- combineDistant distant
  sumSuffix <- sumS suffix
  sumFallback <- sumS fallback
  return $! sumPrefix .| comDistant .| sumSuffix .| sumFallback

-- | A strategy to always just wait.
waitBlockNow :: MonadClient m => m (Strategy (RequestTimed AbWait))
waitBlockNow = return $! returN "wait" ReqWait

pickup :: MonadClient m
       => ActorId -> Bool -> m (Strategy (RequestTimed AbMoveItem))
pickup aid onlyWeapon = do
  benItemL <- benGroundItems aid
  b <- getsState $ getActorBody aid
  activeItems <- activeItemsClient aid
  -- This calmE is outdated when one of the items increases max Calm
  -- (e.g., in pickup, which handles many items at once), but this is OK,
  -- the server accepts item movement based on calm at the start, not end
  -- or in the middle.
  -- The calmE is inaccurate also if an item not IDed, but that's intended
  -- and the server will ignore and warn (and content may avoid that,
  -- e.g., making all rings identified)
  let calmE = calmEnough b activeItems
      isWeapon (_, (_, itemFull)) = isMeleeEqp itemFull
      filterWeapon | onlyWeapon = filter isWeapon
                   | otherwise = id
      prepareOne (oldN, l4) ((_, (k, _)), (iid, itemFull)) =
        -- TODO: instead of pickup to eqp and then move to inv, pickup to inv
        let n = oldN + k
            (newN, toCStore)
              | calmE && goesIntoSha itemFull = (oldN, CSha)
              | goesIntoInv itemFull || eqpOverfull b n = (oldN, CInv)
              | otherwise = (n, CEqp)
        in (newN, (iid, k, CGround, toCStore) : l4)
      (_, prepared) = foldl' prepareOne (0, []) $ filterWeapon benItemL
  return $! if null prepared
            then reject
            else returN "pickup" $ ReqMoveItems prepared

equipItems :: MonadClient m => ActorId -> m (Strategy (RequestTimed AbMoveItem))
equipItems aid = do
  cops <- getsState scops
  body <- getsState $ getActorBody aid
  activeItems <- activeItemsClient aid
  let calmE = calmEnough body activeItems
  fact <- getsState $ (EM.! bfid body) . sfactionD
  eqpAssocs <- fullAssocsClient aid [CEqp]
  invAssocs <- fullAssocsClient aid [CInv]
  shaAssocs <- fullAssocsClient aid [CSha]
  condAnyFoeAdj <- condAnyFoeAdjM aid
  condLightBetrays <- condLightBetraysM aid
  let improve :: CStore
              -> (Int, [(ItemId, Int, CStore, CStore)])
              -> ( IK.EqpSlot
                 , ( [(Int, (ItemId, ItemFull))]
                   , [(Int, (ItemId, ItemFull))] ) )
              -> (Int, [(ItemId, Int, CStore, CStore)])
      improve fromCStore (oldN, l4) (slot, (bestInv, bestEqp)) =
        let n = 1 + oldN
        in case (bestInv, bestEqp) of
          ((_, (iidInv, _)) : _, []) | not (eqpOverfull body n) ->
            (n, (iidInv, 1, fromCStore, CEqp) : l4)
          ((vInv, (iidInv, _)) : _, (vEqp, _) : _)
            | not (eqpOverfull body n)
              && (vInv > vEqp || not (toShare slot)) ->
                (n, (iidInv, 1, fromCStore, CEqp) : l4)
          _ -> (n, l4)
      -- We filter out unneeded items. In particular, we ignore them in eqp
      -- when comparing to items we may want to equip. Anyway, the unneeded
      -- items should be removed in yieldUnneeded earlier or soon after.
      filterNeeded (_, itemFull) =
        not $ unneeded cops condAnyFoeAdj condLightBetrays
                       body activeItems fact itemFull
      bestThree = bestByEqpSlot (filter filterNeeded eqpAssocs)
                                (filter filterNeeded invAssocs)
                                (filter filterNeeded shaAssocs)
      bEqpInv = foldl' (improve CInv) (0, [])
                $ map (\((slot, _), (eqp, inv, _)) ->
                        (slot, (inv, eqp))) bestThree
      bEqpBoth | calmE =
                   foldl' (improve CSha) bEqpInv
                   $ map (\((slot, _), (eqp, _, sha)) ->
                           (slot, (sha, eqp))) bestThree
               | otherwise = bEqpInv
      (_, prepared) = bEqpBoth
  return $! if null prepared
            then reject
            else returN "equipItems" $ ReqMoveItems prepared

toShare :: IK.EqpSlot -> Bool
toShare IK.EqpSlotPeriodic = False
toShare _ = True

unEquipItems :: MonadClient m
             => ActorId -> m (Strategy (RequestTimed AbMoveItem))
unEquipItems aid = do
  cops <- getsState scops
  body <- getsState $ getActorBody aid
  activeItems <- activeItemsClient aid
  let calmE = calmEnough body activeItems
  fact <- getsState $ (EM.! bfid body) . sfactionD
  eqpAssocs <- fullAssocsClient aid [CEqp]
  invAssocs <- fullAssocsClient aid [CInv]
  shaAssocs <- fullAssocsClient aid [CSha]
  condAnyFoeAdj <- condAnyFoeAdjM aid
  condLightBetrays <- condLightBetraysM aid
      -- Here AI hides from the human player the Ring of Speed And Bleeding,
      -- which is a bit harsh, but fair. However any subsequent such
      -- rings will not be picked up at all, so the human player
      -- doesn't lose much fun. Additionally, if AI learns alchemy later on,
      -- they can repair the ring, wield it, drop at death and it's
      -- in play again.
  let yieldSingleUnneeded (iidEqp, itemEqp) =
        let csha = if calmE then CSha else CInv
        in if harmful cops body activeItems fact itemEqp
           then [(iidEqp, itemK itemEqp, CEqp, CInv)]
           else if hinders condAnyFoeAdj condLightBetrays
                           body activeItems itemEqp
           then [(iidEqp, itemK itemEqp, CEqp, csha)]
           else []
      yieldUnneeded = concatMap yieldSingleUnneeded eqpAssocs
      -- Don't share around items that are not critically needed and that
      -- cumulate their effects well.
      improve :: CStore -> ( IK.EqpSlot
                           , ( [(Int, (ItemId, ItemFull))]
                             , [(Int, (ItemId, ItemFull))] ) )
              -> [(ItemId, Int, CStore, CStore)]
      improve fromCStore (slot, (bestSha, bestEOrI)) =
        case (bestSha, bestEOrI) of
          _ | not (toShare slot)
              && fromCStore == CEqp
              && not (eqpOverfull body 1) ->  -- keep one eqp slot empty
            []
          (_, (vEOrI, (iidEOrI, _)) : _) | (toShare slot || fromCStore == CInv)
                                           && getK bestEOrI > 1
                                           && betterThanSha vEOrI bestSha ->
            -- To share the best items with others, if they care.
            [(iidEOrI, getK bestEOrI - 1, fromCStore, CSha)]
          (_, _ : (vEOrI, (iidEOrI, _)) : _) | (toShare slot
                                                || fromCStore == CInv)
                                               && betterThanSha vEOrI bestSha ->
            -- To share the second best items with others, if they care.
            [(iidEOrI, getK bestEOrI, fromCStore, CSha)]
          (_, (vEOrI, (_, _)) : _) | eqpOverfull body 1
                                     && if worseThanSha vEOrI bestSha
                                        then fromCStore == CEqp
                                        else fromCStore == CInv ->
            -- To make place in eqp for an item better than any ours
            -- or to remove from inv bad items that won't be equipped.
            [(fst $ snd $ last bestEOrI, 1, fromCStore, CSha)]
          _ -> []
      getK [] = 0
      getK ((_, (_, itemFull)) : _) = itemK itemFull
      betterThanSha _ [] = True
      betterThanSha vEOrI ((vSha, _) : _) = vEOrI > vSha
      worseThanSha _ [] = False
      worseThanSha vEOrI ((vSha, _) : _) = vEOrI < vSha
      bestThree = bestByEqpSlot eqpAssocs invAssocs shaAssocs
      bInvSha = concatMap (improve CInv)
                $ map (\((slot, _), (_, inv, sha)) ->
                        (slot, (sha, inv))) bestThree
      bEqpSha = concatMap (improve CEqp)
                $ map (\((slot, _), (eqp, _, sha)) ->
                        (slot, (sha, eqp))) bestThree
      prepared = if null yieldUnneeded
                 then if calmE then bInvSha ++ bEqpSha else []
                 else yieldUnneeded
  return $! if null prepared
            then reject
            else returN "unEquipItems" $ ReqMoveItems prepared

groupByEqpSlot :: [(ItemId, ItemFull)]
               -> M.Map (IK.EqpSlot, Text) [(ItemId, ItemFull)]
groupByEqpSlot is =
  let f (iid, itemFull) = case strengthEqpSlot $ itemBase itemFull of
        Nothing -> Nothing
        Just es -> Just (es, [(iid, itemFull)])
      withES = mapMaybe f is
  in M.fromListWith (++) withES

bestByEqpSlot :: [(ItemId, ItemFull)]
              -> [(ItemId, ItemFull)]
              -> [(ItemId, ItemFull)]
              -> [((IK.EqpSlot, Text)
                  , ( [(Int, (ItemId, ItemFull))]
                    , [(Int, (ItemId, ItemFull))]
                    , [(Int, (ItemId, ItemFull))] ) )]
bestByEqpSlot eqpAssocs invAssocs shaAssocs =
  let eqpMap = M.map (\g -> (g, [], [])) $ groupByEqpSlot eqpAssocs
      invMap = M.map (\g -> ([], g, [])) $ groupByEqpSlot invAssocs
      shaMap = M.map (\g -> ([], [], g)) $ groupByEqpSlot shaAssocs
      appendThree (g1, g2, g3) (h1, h2, h3) = (g1 ++ h1, g2 ++ h2, g3 ++ h3)
      eqpInvShaMap = M.unionsWith appendThree [eqpMap, invMap, shaMap]
      bestSingle = strongestSlot
      bestThree (eqpSlot, _) (g1, g2, g3) = (bestSingle eqpSlot g1,
                                             bestSingle eqpSlot g2,
                                             bestSingle eqpSlot g3)
  in M.assocs $ M.mapWithKey bestThree eqpInvShaMap

-- TODO: also take into account dynamic lights *not* wielded by the actor
hinders :: Bool -> Bool -> Actor -> [ItemFull] -> ItemFull -> Bool
hinders condAnyFoeAdj condLightBetrays body activeItems itemFull =
  let itemLit = isJust $ strengthFromEqpSlot IK.EqpSlotAddLight itemFull
  in -- Fast actors want to hide in darkness to ambush opponents and want
     -- to hit hard for the short span they get to survive melee.
     bspeed body activeItems > speedNormal
     && (itemLit
         || 0 > fromMaybe 0 (strengthFromEqpSlot IK.EqpSlotAddHurtMelee
                                                 itemFull)
         || 0 > fromMaybe 0 (strengthFromEqpSlot IK.EqpSlotAddHurtRanged
                                                 itemFull))
     -- Distressed actors want to hide in the dark.
     || let heavilyDistressed =  -- actor hit by a proj or similarly distressed
              deltaSerious (bcalmDelta body)
        in heavilyDistressed && condLightBetrays && not condAnyFoeAdj
           && itemLit
  -- TODO:
  -- teach AI to turn shields OFF (or stash) when ganging up on an enemy
  -- (friends close, only one enemy close)
  -- and turning on afterwards (AI plays for time, especially spawners
  -- so shields are preferable by default;
  -- also, turning on when no friends and enemies close is too late,
  -- AI should flee or fire at such times, not muck around with eqp)

harmful :: Kind.COps -> Actor -> [ItemFull] -> Faction -> ItemFull -> Bool
harmful cops body activeItems fact itemFull =
  -- Items that are known and their effects are not stricly beneficial
  -- should not be equipped (either they are harmful or they waste eqp space).
  maybe False (\(u, _) -> u <= 0)
    (totalUsefulness cops body activeItems fact itemFull)

unneeded :: Kind.COps -> Bool -> Bool -> Actor
         -> [ItemFull] -> Faction -> ItemFull
         -> Bool
unneeded cops condAnyFoeAdj condLightBetrays body activeItems fact itemFull =
  harmful cops body activeItems fact itemFull
  || hinders condAnyFoeAdj condLightBetrays body activeItems itemFull
  || let calm10 = calmEnough10 body activeItems  -- unneeded risk
         itemLit = isJust $ strengthFromEqpSlot IK.EqpSlotAddLight itemFull
     in itemLit && not calm10

-- Everybody melees in a pinch, even though some prefer ranged attacks.
meleeBlocker :: MonadClient m => ActorId -> m (Strategy (RequestTimed AbMelee))
meleeBlocker aid = do
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  actorSk <- actorSkillsClient aid
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  case mtgtMPath of
    Just (_, Just (_ : q : _, (goal, _))) -> do
      -- We prefer the goal (e.g., when no accessible, but adjacent),
      -- but accept @q@ even if it's only a blocking enemy position.
      let maim | adjacent (bpos b) goal = Just goal
               | adjacent (bpos b) q = Just q
               | otherwise = Nothing  -- MeleeDistant
      mBlocker <- case maim of
        Nothing -> return Nothing
        Just aim -> getsState $ posToActor aim (blid b)
      case mBlocker of
        Just ((aid2, _), _) -> do
          -- No problem if there are many projectiles at the spot. We just
          -- attack the first one.
          body2 <- getsState $ getActorBody aid2
          if not (actorDying body2)  -- already dying
             && (not (bproj body2)  -- displacing saves a move
                 && isAtWar fact (bfid body2)  -- they at war with us
                 || EM.findWithDefault 0 AbDisplace actorSk <= 0  -- not disp.
                    && fleaderMode (gplayer fact) == LeaderNull  -- no restrain
                    && EM.findWithDefault 0 AbMove actorSk > 0  -- blocked move
                    && bhp body2 < bhp b)  -- respect power
            then do
              mel <- maybeToList <$> pickWeaponClient aid aid2
              return $! liftFrequency $ uniformFreq "melee in the way" mel
            else return reject
        Nothing -> return reject
    _ -> return reject  -- probably no path to the enemy, if any

-- Everybody melees in a pinch, skills and weapons allowing,
-- even though some prefer ranged attacks.
meleeAny :: MonadClient m => ActorId -> m (Strategy (RequestTimed AbMelee))
meleeAny aid = do
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  allFoes <- getsState $ actorRegularAssocs (isAtWar fact) (blid b)
  let adjFoes = filter (adjacent (bpos b) . bpos . snd) allFoes
  mels <- mapM (pickWeaponClient aid . fst) adjFoes
      -- TODO: prioritize somehow
  let freq = uniformFreq "melee adjacent" $ catMaybes mels
  return $! liftFrequency freq

-- TODO: take charging status into account
-- Fast monsters don't pay enough attention to features.
trigger :: MonadClient m
        => ActorId -> Bool -> m (Strategy (RequestTimed AbTrigger))
trigger aid fleeViaStairs = do
  cops@Kind.COps{cotile=Kind.Ops{okind}} <- getsState scops
  dungeon <- getsState sdungeon
  explored <- getsClient sexplored
  b <- getsState $ getActorBody aid
  activeItems <- activeItemsClient aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  let lid = blid b
  lvl <- getLevel lid
  unexploredD <- unexploredDepth
  s <- getState
  let unexploredCurrent = ES.notMember lid explored
      allExplored = ES.size explored == EM.size dungeon
      t = lvl `at` bpos b
      feats = TK.tfeature $ okind t
      ben feat = case feat of
        TK.Cause (IK.Ascend k) -> do -- change levels sensibly, in teams
          (lid2, pos2) <- getsState $ whereTo lid (bpos b) k . sdungeon
          per <- getPerFid lid2
          let canSee = ES.member (bpos b) (totalVisible per)
              aimless = ftactic (gplayer fact) `elem` [TRoam, TPatrol]
              easeDelta = abs (fromEnum lid) - abs (fromEnum lid2)
              unexpForth = unexploredD (signum k) lid
              unexpBack = unexploredD (- signum k) lid
              expBenefit
                | aimless = 100  -- faction is not exploring, so switch at will
                | unexploredCurrent = 0  -- don't leave level until explored
                | unexpForth =
                    if easeDelta > 0  -- alway try as easy level as possible
                       || not unexpBack  -- no other choice for exploration
                    then 1000 * abs easeDelta
                    else 0
                | unexpBack = 0  -- wait for stairs in the opposite direciton
                | lescape lvl = 0  -- all explored, stay on the escape level
                | otherwise = 2  -- no escape, switch levels occasionally
              actorsThere = posToActors pos2 lid2 s
          return $!
             if boldpos b == bpos b   -- probably used stairs last turn
                && boldlid b == lid2  -- in the opposite direction
             then 0  -- avoid trivial loops (pushing, being pushed, etc.)
             else let leaderless = fleaderMode (gplayer fact) == LeaderNull
                      eben = case actorsThere of
                        [] | canSee -> expBenefit
                        _ | leaderless -> 0  -- leaderless clog stairs easily
                        _ -> min 1 expBenefit  -- risk pushing
                  in if fleeViaStairs
                     then 1000 * eben + 1  -- strongly prefer correct direction
                     else eben
        TK.Cause ef@IK.Escape{} -> return $  -- flee via this way, too
          -- Only some factions try to escape but they first explore all
          -- for high score.
          if not (fcanEscape $ gplayer fact) || not allExplored
          then 0
          else effectToBenefit cops b activeItems fact ef
        TK.Cause ef | not fleeViaStairs ->
          return $! effectToBenefit cops b activeItems fact ef
        _ -> return 0
  benFeats <- mapM ben feats
  let benFeat = zip benFeats feats
  return $! liftFrequency $ toFreq "trigger"
    [ (benefit, ReqTrigger (Just feat))
    | (benefit, feat) <- benFeat
    , benefit > 0 ]

projectItem :: MonadClient m => ActorId -> m (Strategy (RequestTimed AbProject))
projectItem aid = do
  btarget <- getsClient $ getTarget aid
  b <- getsState $ getActorBody aid
  mfpos <- aidTgtToPos aid (blid b) btarget
  seps <- getsClient seps
  case (btarget, mfpos) of
    (_, Just fpos) | chessDist (bpos b) fpos == 1 -> return reject
    (Just TEnemy{}, Just fpos) -> do
      mnewEps <- makeLine False b fpos seps
      case mnewEps of
        Just newEps -> do
          actorSk <- actorSkillsClient aid
          let skill = EM.findWithDefault 0 AbProject actorSk
          -- ProjectAimOnself, ProjectBlockActor, ProjectBlockTerrain
          -- and no actors or obstracles along the path.
          let q _ itemFull b2 activeItems =
                either (const False) id
                $ permittedProject " " False skill itemFull b2 activeItems
          benList <- benAvailableItems aid q [CEqp, CInv, CGround]
          localTime <- getsState $ getLocalTime (blid b)
          let coeff CGround = 2
              coeff COrgan = 3  -- can't give to others
              coeff CEqp = 1
              coeff CInv = 1
              coeff CSha = undefined  -- banned
              fRanged ( (mben, (_, cstore))
                      , (iid, itemFull@ItemFull{itemBase}) ) =
                -- We assume if the item has a timeout, most effects are under
                -- Recharging, so no point projecting if not recharged.
                -- This is not an obvious assumption, so recharging is not
                -- included in permittedProject and can be tweaked here easily.
                let recharged = hasCharge localTime itemFull
                    trange = totalRange itemBase
                    bestRange =
                      chessDist (bpos b) fpos + 2  -- margin for fleeing
                    rangeMult =  -- penalize wasted or unsafely low range
                      10 + max 0 (10 - abs (trange - bestRange))
                    durable = IK.Durable `elem` jfeature itemBase
                    durableBonus = if durable
                                   then 2  -- we or foes keep it after the throw
                                   else 1
                    benR = durableBonus
                           * coeff cstore
                           * case mben of
                               Nothing -> -1  -- experiment if no options
                               Just (_, ben) -> ben
                           * (if recharged then 1 else 0)
                in if -- Durable weapon is usually too useful for melee.
                      not (isMeleeEqp itemFull)
                      && benR < 0
                      && trange >= chessDist (bpos b) fpos
                   then Just ( -benR * rangeMult `div` 10
                             , ReqProject fpos newEps iid cstore )
                   else Nothing
              benRanged = mapMaybe fRanged benList
          return $! liftFrequency $ toFreq "projectItem" benRanged
        _ -> return reject
    _ -> return reject

data ApplyItemGroup = ApplyAll | ApplyFirstAid
  deriving Eq

applyItem :: MonadClient m
          => ActorId -> ApplyItemGroup -> m (Strategy (RequestTimed AbApply))
applyItem aid applyGroup = do
  actorSk <- actorSkillsClient aid
  b <- getsState $ getActorBody aid
  localTime <- getsState $ getLocalTime (blid b)
  let skill = EM.findWithDefault 0 AbProject actorSk
      q _ itemFull _ activeItems =
        either (const False) id
        $ permittedApply " " localTime skill itemFull b activeItems
  benList <- benAvailableItems aid q [CEqp, CInv, CGround]
  organs <- mapM (getsState . getItemBody) $ EM.keys $ borgan b
  let itemLegal itemFull = case applyGroup of
        ApplyFirstAid ->
          let getP (IK.RefillHP p) _ | p > 0 = True
              getP (IK.OverfillHP p) _ | p > 0 = True
              getP _ acc = acc
          in case itemDisco itemFull of
            Just ItemDisco{itemAE=Just ItemAspectEffect{jeffects}} ->
              foldr getP False jeffects
            _ -> False
        ApplyAll -> True
      coeff CGround = 2
      coeff COrgan = 3  -- can't give to others
      coeff CEqp = 1
      coeff CInv = 1
      coeff CSha = undefined  -- banned
      fTool ((mben, (_, cstore)), (iid, itemFull@ItemFull{itemBase})) =
        let durableBonus = if IK.Durable `elem` jfeature itemBase
                           then 5  -- we keep it after use
                           else 1
            oldGrps = map (toGroupName . jname) organs
            createOrganAgain =
              -- This assumes the organ creation is beneficial. If it's
              -- a drawback of an otherwise good item, we should reverse
              -- the condition.
              let newGrps = strengthCreateOrgan itemFull
              in not $ null $ intersect newGrps oldGrps
            dropOrganVoid =
              -- This assumes the organ dropping is beneficial. If it's
              -- a drawback of an otherwise good item, or a marginal
              -- advantage only, we should reverse or ignore the condition.
              -- We ignore a very general @grp@ being used for a very
              -- common and easy to drop organ, etc.
              let newGrps = strengthDropOrgan itemFull
                  hasDropOrgan = not $ null newGrps
              in hasDropOrgan && null (newGrps `intersect` oldGrps)
            benR = case mben of
                     Nothing -> 0
                       -- experimenting is fun, but it's better to risk
                       -- foes' skin than ours -- TODO: when {applied}
                       -- is implemented, enable this for items too heavy,
                       -- etc. for throwing
                     Just (_, ben) -> ben
                   * (if not createOrganAgain then 1 else 0)
                   * (if not dropOrganVoid then 1 else 0)
                   * durableBonus
                   * coeff cstore
        in if itemLegal itemFull && benR > 0
           then Just (benR, ReqApply iid cstore)
           else Nothing
      benTool = mapMaybe fTool benList
  return $! liftFrequency $ toFreq "applyItem" benTool

-- If low on health or alone, flee in panic, close to the path to target
-- and as far from the attackers, as possible. Usually fleeing from
-- foes will lead towards friends, but we don't insist on that.
-- We use chess distances, not pathfinding, because melee can happen
-- at path distance 2.
flee :: MonadClient m
     => ActorId -> [(Int, Point)] -> m (Strategy RequestAnyAbility)
flee aid fleeL = do
  b <- getsState $ getActorBody aid
  let vVic = map (second (`vectorToFrom` bpos b)) fleeL
      str = liftFrequency $ toFreq "flee" vVic
  mapStrategyM (moveOrRunAid True aid) str

displaceFoe :: MonadClient m => ActorId -> m (Strategy RequestAnyAbility)
displaceFoe aid = do
  cops <- getsState scops
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  fact <- getsState $ (EM.! bfid b) . sfactionD
  let friendlyFid fid = fid == bfid b || isAllied fact fid
  friends <- getsState $ actorRegularList friendlyFid (blid b)
  allFoes <- getsState $ actorRegularAssocs (isAtWar fact) (blid b)
  let accessibleHere = accessible cops lvl $ bpos b
      displaceable body =  -- DisplaceAccess
        adjacent (bpos body) (bpos b) && accessibleHere (bpos body)
      nFriends body = length $ filter (adjacent (bpos body) . bpos) friends
      nFrHere = nFriends b + 1
      qualifyActor (aid2, body2) = do
        activeItems <- activeItemsClient aid2
        dEnemy <- getsState $ dispEnemy aid aid2 activeItems
          -- DisplaceDying, DisplaceBraced, DisplaceImmobile, DisplaceSupported
        let nFr = nFriends body2
        return $! if displaceable body2 && dEnemy && nFr < nFrHere
          then Just (nFr * nFr, bpos body2 `vectorToFrom` bpos b)
          else Nothing
  vFoes <- mapM qualifyActor allFoes
  let str = liftFrequency $ toFreq "displaceFoe" $ catMaybes vFoes
  mapStrategyM (moveOrRunAid True aid) str

displaceBlocker :: MonadClient m => ActorId -> m (Strategy RequestAnyAbility)
displaceBlocker aid = do
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  str <- case mtgtMPath of
    Just (_, Just (p : q : _, _)) -> displaceTowards aid p q
    _ -> return reject  -- goal reached
  mapStrategyM (moveOrRunAid True aid) str

-- TODO: perhaps modify target when actually moving, not when
-- producing the strategy, even if it's a unique choice in this case.
displaceTowards :: MonadClient m
                => ActorId -> Point -> Point -> m (Strategy Vector)
displaceTowards aid source target = do
  cops <- getsState scops
  b <- getsState $ getActorBody aid
  let !_A = assert (source == bpos b && adjacent source target) ()
  lvl <- getLevel $ blid b
  if boldpos b /= target -- avoid trivial loops
     && accessible cops lvl source target then do  -- DisplaceAccess
    mleader <- getsClient _sleader
    mBlocker <- getsState $ posToActors target (blid b)
    case mBlocker of
      [] -> return reject
      [((aid2, b2), _)] | Just aid2 /= mleader -> do
        mtgtMPath <- getsClient $ EM.lookup aid2 . stargetD
        case mtgtMPath of
          Just (tgt, Just (p : q : rest, (goal, len)))
            | q == source && p == target
              || waitedLastTurn b2 -> do
              let newTgt = if q == source && p == target
                           then Just (tgt, Just (q : rest, (goal, len - 1)))
                           else Nothing
              modifyClient $ \cli ->
                cli {stargetD = EM.alter (const newTgt) aid (stargetD cli)}
              return $! returN "displace friend" $ target `vectorToFrom` source
          Just _ -> return reject
          Nothing -> do
            tfact <- getsState $ (EM.! bfid b2) . sfactionD
            activeItems <- activeItemsClient aid2
            dEnemy <- getsState $ dispEnemy aid aid2 activeItems
            if not (isAtWar tfact (bfid b)) || dEnemy then
              return $! returN "displace other" $ target `vectorToFrom` source
            else return reject  -- DisplaceDying, etc.
      _ -> return reject  -- DisplaceProjectiles or trying to displace leader
  else return reject

chase :: MonadClient m
      => ActorId -> Bool -> Bool -> m (Strategy RequestAnyAbility)
chase aid doDisplace avoidAmbient = do
  Kind.COps{cotile} <- getsState scops
  body <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid body) . sfactionD
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  lvl <- getLevel $ blid body
  let isAmbient pos = Tile.isLit cotile (lvl `at` pos)
  str <- case mtgtMPath of
    Just (_, Just (p : q : _, (goal, _))) | not $ avoidAmbient && isAmbient q ->
      -- With no leader, the goal is vague, so permit arbitrary detours.
      moveTowards aid p q goal (fleaderMode (gplayer fact) == LeaderNull)
    _ -> return reject  -- goal reached
  -- If @doDisplace@: don't pick fights, assuming the target is more important.
  -- We'd normally melee the target earlier on via @AbMelee@, but for
  -- actors that don't have this ability (and so melee only when forced to),
  -- this is meaningul.
  mapStrategyM (moveOrRunAid doDisplace aid) str

moveTowards :: MonadClient m
            => ActorId -> Point -> Point -> Point -> Bool -> m (Strategy Vector)
moveTowards aid source target goal relaxed = do
  cops@Kind.COps{cotile} <- getsState scops
  b <- getsState $ getActorBody aid
  let !_A = assert (source == bpos b && adjacent source target) ()
  lvl <- getLevel $ blid b
  fact <- getsState $ (EM.! bfid b) . sfactionD
  friends <- getsState $ actorList (not . isAtWar fact) $ blid b
  let noFriends = unoccupied friends
      accessibleHere = accessible cops lvl source
      bumpableHere p =
        let t = lvl `at` p
        in Tile.isOpenable cotile t
           || Tile.isSuspect cotile t
           || Tile.isChangeable cotile t
      enterableHere p = accessibleHere p || bumpableHere p
  if noFriends target && enterableHere target then
    return $! returN "moveTowards adjacent" $ target `vectorToFrom` source
  else do
    let goesBack v = v == boldpos b `vectorToFrom` source
        nonincreasing p = chessDist source goal >= chessDist p goal
        isSensible p = (relaxed || nonincreasing p)
                       && noFriends p
                       && enterableHere p
        sensible = [ ((goesBack v, chessDist p goal), v)
                   | v <- moves, let p = source `shift` v, isSensible p ]
        sorted = sortBy (comparing fst) sensible
        groups = map (map snd) $ groupBy ((==) `on` fst) sorted
        freqs = map (liftFrequency . uniformFreq "moveTowards") groups
    return $! foldr (.|) reject freqs

-- | Actor moves or searches or alters or attacks. Displaces if @run@.
-- This function is very general, even though it's often used in contexts
-- when only one or two of the many cases can possibly occur.
moveOrRunAid :: MonadClient m
             => Bool -> ActorId -> Vector -> m (Maybe RequestAnyAbility)
moveOrRunAid run source dir = do
  cops@Kind.COps{cotile} <- getsState scops
  sb <- getsState $ getActorBody source
  let lid = blid sb
  lvl <- getLevel lid
  let spos = bpos sb           -- source position
      tpos = spos `shift` dir  -- target position
      t = lvl `at` tpos
  -- We start by checking actors at the the target position,
  -- which gives a partial information (actors can be invisible),
  -- as opposed to accessibility (and items) which are always accurate
  -- (tiles can't be invisible).
  tgts <- getsState $ posToActors tpos lid
  case tgts of
    [((target, b2), _)] | run -> do
      -- @target@ can be a foe, as well as a friend.
      tfact <- getsState $ (EM.! bfid b2) . sfactionD
      activeItems <- activeItemsClient target
      dEnemy <- getsState $ dispEnemy source target activeItems
      if boldpos sb /= tpos -- avoid trivial Displace loops
         && accessible cops lvl spos tpos -- DisplaceAccess
         && (not (isAtWar tfact (bfid sb))
             || dEnemy)  -- DisplaceDying, etc.
      then
        return $! Just $ RequestAnyAbility $ ReqDisplace target
      else do
        wps <- pickWeaponClient source target
        case wps of
          Nothing -> return Nothing
          Just wp -> return $! Just $ RequestAnyAbility wp
    ((target, _), _) : _ -> do  -- can be a foe, as well as friend (e.g., proj.)
      -- No problem if there are many projectiles at the spot. We just
      -- attack the first one.
      -- Attacking does not require full access, adjacency is enough.
      wps <- pickWeaponClient source target
      case wps of
        Nothing -> return Nothing
        Just wp -> return $! Just $ RequestAnyAbility wp
    [] ->  -- move or search or alter
      if accessible cops lvl spos tpos then
        -- Movement requires full access.
        return $! Just $ RequestAnyAbility $ ReqMove dir
        -- The potential invisible actor is hit.
      else if EM.member tpos $ lfloor lvl then
        -- This is, e.g., inaccessible open door with an item in it.
        assert `failure` "AI causes AlterBlockItem" `twith` (run, source, dir)
      else if not (Tile.isWalkable cotile t)  -- not implied
              && (Tile.isSuspect cotile t
                  || Tile.isOpenable cotile t
                  || Tile.isClosable cotile t
                  || Tile.isChangeable cotile t) then
        -- No access, so search and/or alter the tile.
        return $! Just $ RequestAnyAbility $ ReqAlter tpos Nothing
      else
        -- Boring tile, no point bumping into it, do WaitSer if really idle.
        assert `failure` "AI causes MoveNothing or AlterNothing"
               `twith` (run, source, dir)
