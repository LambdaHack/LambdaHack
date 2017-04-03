{-# LANGUAGE DataKinds, TupleSections #-}
-- | Semantics of abilities in terms of actions and the AI procedure
-- for picking the best action for an actor.
module Game.LambdaHack.Client.AI.HandleAbilityM
  ( actionStrategy
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , waitBlockNow, pickup, equipItems, toShare, yieldUnneeded, unEquipItems
  , groupByEqpSlot, bestByEqpSlot, harmful, unneeded, meleeBlocker, meleeAny
  , trigger, projectItem, applyItem, flee
  , displaceFoe, displaceBlocker, displaceTowards
  , chase, moveTowards, moveOrRunAid
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Function
import Data.Ord
import Data.Ratio

import Game.LambdaHack.Client.AI.ConditionM
import Game.LambdaHack.Client.AI.Preferences
import Game.LambdaHack.Client.AI.Strategy
import Game.LambdaHack.Client.Bfs
import Game.LambdaHack.Client.BfsM
import Game.LambdaHack.Client.CommonM
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
import Game.LambdaHack.Common.Point
import qualified Game.LambdaHack.Common.PointArray as PointArray
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind

type ToAny a = Strategy (RequestTimed a) -> Strategy RequestAnyAbility

toAny :: ToAny a
toAny strat = RequestAnyAbility <$> strat

-- | AI strategy based on actor's sight, smell, etc.
-- Never empty.
actionStrategy :: forall m. MonadClient m
               => ActorId -> m (Strategy RequestAnyAbility)
{-# INLINE actionStrategy #-}
actionStrategy aid = do
  body <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid body) . sfactionD
  condInMelee <- getsClient scondInMelee
  let (newCondInMelee, _oldCondInMelee) = case condInMelee EM.! blid body of
        Right conds -> conds
        Left{} -> assert `failure` condInMelee
  condAimEnemyPresent <- condAimEnemyPresentM aid
  condAimEnemyRemembered <- condAimEnemyRememberedM aid
  condAnyFoeAdj <- condAnyFoeAdjM aid
  threatDistL <- threatDistList aid
  condHpTooLow <- condHpTooLowM aid
  condAdjTriggerable <- condAdjTriggerableM aid
  condBlocksFriends <- condBlocksFriendsM aid
  condNoEqpWeapon <- condNoEqpWeaponM aid
  condEnoughGear <- condEnoughGearM aid
  condFloorWeapon <- condFloorWeaponM aid
  condCanProject <- condCanProjectM False aid
  condNotCalmEnough <- condNotCalmEnoughM aid
  condDesirableFloorItem <- condDesirableFloorItemM aid
  condMeleeBad <- condMeleeBadM aid
  condMeleeBest <- condMeleeBestM aid
  condStrongFriendAdj <- condStrongFriendAdjM aid
  condTgtNonmoving <- condTgtNonmovingM aid
  aInAmbient <- getsState $ actorInAmbient body
  explored <- getsClient sexplored
  (fleeL, badVic) <- fleeList aid
  actorAspect <- getsClient sactorAspect
  let condCanMelee = actorCanMelee actorAspect aid body
      ar = case EM.lookup aid actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` aid
      lidExplored = ES.member (blid body) explored
      panicFleeL = fleeL ++ badVic
      actorShines = aShine ar > 0
      condThreat n = not $ null $ takeWhile ((<= n) . fst) threatDistL
      speed1_5 = speedScale (3%2) (bspeed body ar)
      threatAdj = takeWhile ((== 1) . fst) threatDistL
      condManyThreatAdj = length threatAdj >= 2
      condFastThreatAdj = any (\(_, (aid2, b2)) ->
                                let ar2 = actorAspect EM.! aid2
                                in bspeed b2 ar2 > speed1_5)
                              threatAdj
      heavilyDistressed =  -- actor hit by a proj or similarly distressed
        deltaSerious (bcalmDelta body)
      actorMaxSk = aSkills ar
      abInMaxSkill ab = EM.findWithDefault 0 ab actorMaxSk > 0
      stratToFreq :: Int -> m (Strategy RequestAnyAbility)
                  -> m (Frequency RequestAnyAbility)
      stratToFreq scale mstrat = do
        st <- mstrat
        return $! if scale == 0
                  then mzero
                  else scaleFreq scale $ bestVariant st
      -- Order matters within the list, because it's summed with .| after
      -- filtering. Also, the results of prefix, distant and suffix
      -- are summed with .| at the end.
      prefix, suffix :: [([Ability], m (Strategy RequestAnyAbility), Bool)]
      prefix =
        [ ( [AbApply], (toAny :: ToAny 'AbApply)
            <$> applyItem aid ApplyFirstAid
          , condHpTooLow && not condAnyFoeAdj
            && not condAdjTriggerable )  -- don't block stairs, perhaps ascend
        , ( [AbAlter], (toAny :: ToAny 'AbAlter)
            <$> trigger aid ViaStairs
              -- explore next or flee via stairs, even if to wrong level;
              -- in the latter case, may return via different stairs later on
          , condAdjTriggerable && not condAimEnemyPresent
            && ((condNotCalmEnough || condHpTooLow)  -- flee
                && condMeleeBad && condThreat 1
                || (lidExplored || condEnoughGear)  -- explore
                   && not condDesirableFloorItem) )
        , ( [AbDisplace]
          , displaceFoe aid  -- only swap with an enemy to expose him
          , condBlocksFriends && condAnyFoeAdj  -- checks later if foe eligible
            && not condDesirableFloorItem )
        , ( [AbMoveItem], (toAny :: ToAny 'AbMoveItem)
            <$> pickup aid True
          , condNoEqpWeapon  -- we assume organ weapons usually inferior
            && condFloorWeapon && not condHpTooLow
            && abInMaxSkill AbMelee )
        , ( [AbMelee], (toAny :: ToAny 'AbMelee)
            <$> meleeBlocker aid  -- only melee target or blocker
          , condAnyFoeAdj  -- if foes, don't displace, otherwise friends:
            || not (abInMaxSkill AbDisplace)  -- displace friends, if possible
               && fleaderMode (gplayer fact) == LeaderNull  -- not restrained
               && condAimEnemyPresent  -- excited
               && not newCondInMelee )  -- don't incur overhead
        , ( [AbAlter], (toAny :: ToAny 'AbAlter)
            <$> trigger aid ViaEscape
          , condAdjTriggerable
            && not condDesirableFloorItem )  -- collect the last loot
        , ( [AbAlter], (toAny :: ToAny 'AbAlter)
            <$> trigger aid ViaNothing
          , condAdjTriggerable
            && not condAimEnemyPresent  -- focus if enemies nearby
            && not newCondInMelee )  -- don't incur overhead
        , ( [AbMove]
          , flee aid fleeL
          , -- Flee either from melee, if our melee is bad and enemy close
            -- or from missiles, if hit and enemies are only far away,
            -- can fling at us and we can't well fling at them nor well melee.
            not condFastThreatAdj
            && if | condManyThreatAdj -> not condStrongFriendAdj
                  | condThreat 2
                    || condThreat 5 && aInAmbient && not actorShines ->
                    condMeleeBad
                    -- Don't keep fleeing if just hit, because too close
                    -- to enemy to get out of his range, most likely,
                    -- and so melee him instead, unless can't melee at all.
                    && not (heavilyDistressed && condCanMelee)
                  | condThreat 5 ->
                    -- Too far to flee from melee, too close from ranged,
                    -- not in ambient, so no point fleeing into dark; stay.
                    False
                  | condThreat 12 ->
                    -- Too far to close in for melee; can't shoot; flee from
                    -- ranged attack and prepare ambush for later on.
                    heavilyDistressed
                    && not condCanProject
                  | otherwise -> False ) -- not threats in range; don't flee
        , ( [AbDisplace]  -- prevents some looping movement
          , displaceBlocker aid  -- fires up only when path blocked
          , not condDesirableFloorItem )
        ]
      -- Order doesn't matter, scaling does.
      -- These are flattened (taking only the best variant) and then summed,
      -- so if any of these can fire, it will fire. If none, @suffix@ is tried.
      -- Only the best variant of @chase@ is taken, but it's almost always
      -- good, and if not, the @chase@ in @suffix@ may fix that.
      distant :: [([Ability], m (Frequency RequestAnyAbility), Bool)]
      distant =
        [ ( [AbMoveItem]
          , stratToFreq (if newCondInMelee then 2 else 20000)
            $ (toAny :: ToAny 'AbMoveItem)
            <$> yieldUnneeded aid  -- 20000 to unequip ASAP, unless is thrown
          , True )
        , ( [AbMoveItem]
          , stratToFreq 1 $ (toAny :: ToAny 'AbMoveItem)
            <$> equipItems aid  -- doesn't take long, very useful if safe
          , not (condAnyFoeAdj
                 || condDesirableFloorItem
                 || condNotCalmEnough
                 || heavilyDistressed
                 || newCondInMelee) )
        , ( [AbProject]
          , stratToFreq 2 $ (toAny :: ToAny 'AbProject)
            <$> projectItem aid
          , condAimEnemyPresent && condCanProject && not newCondInMelee )
        , ( [AbApply]
          , stratToFreq 2 $ (toAny :: ToAny 'AbApply)
            <$> applyItem aid ApplyAll  -- use any potion or scroll
          , (condAimEnemyPresent || condThreat 9) )  -- can affect enemies
        , ( [AbMove]
          , stratToFreq (if | not condAimEnemyPresent ->
                              2  -- if enemy only remembered, investigate anyway
                            | condTgtNonmoving && condMeleeBad ->
                              0
                            | newCondInMelee ->
                              1000  -- friends pummeled by target, go to help
                            | otherwise ->
                              50)
            $ chase aid True (condThreat 5
                              && not aInAmbient && not actorShines
                              && condMeleeBad)
          , (condAimEnemyPresent
             || condAimEnemyRemembered && not newCondInMelee)
            && (not (condThreat 2) || condMeleeBest)
              -- this results in animals in corridor never attacking,
              -- because they can't swarm the opponent, which is logical,
              -- and in rooms they do attack, so not too boring;
              -- 2 monsters attack always, because they are more aggressive
            && not (condDesirableFloorItem && not newCondInMelee)
            && condCanMelee )
        ]
      -- Order matters again.
      suffix =
        [ ( [AbMelee], (toAny :: ToAny 'AbMelee)
            <$> meleeAny aid  -- avoid getting damaged for naught
          , condAnyFoeAdj )
        , ( [AbMove]
          , flee aid panicFleeL  -- ultimate panic mode, displaces foes
          , condAnyFoeAdj )
        , ( [AbMoveItem], (toAny :: ToAny 'AbMoveItem)
            <$> pickup aid False  -- e.g., to give to other party members
          , not newCondInMelee )
        , ( [AbMoveItem], (toAny :: ToAny 'AbMoveItem)
            <$> unEquipItems aid  -- late, because these items not bad
          , not newCondInMelee )
        , ( [AbMove]
          , chase aid True (-- Don't keep hiding in darkness if hit right now,
                            -- unless can't melee at all.
                            condThreat 5
                            && not aInAmbient && not actorShines
                            && condMeleeBad
                            && not (heavilyDistressed && condCanMelee))
          , not (condTgtNonmoving && condThreat 2)
            && (not (condThreat 2) || condMeleeBest)
            && (not newCondInMelee || condAimEnemyPresent) )
        ]
      fallback =
        [ ( [AbWait], (toAny :: ToAny 'AbWait)
            <$> waitBlockNow
            -- Wait until friends sidestep; ensures strategy is never empty.
          , True )
        ]
  -- Check current, not maximal skills, since this can be a non-leader action.
  actorSk <- actorSkillsClient aid
  let abInSkill ab = EM.findWithDefault 0 ab actorSk > 0
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
waitBlockNow :: MonadClient m => m (Strategy (RequestTimed 'AbWait))
waitBlockNow = return $! returN "wait" ReqWait

pickup :: MonadClient m
       => ActorId -> Bool -> m (Strategy (RequestTimed 'AbMoveItem))
pickup aid onlyWeapon = do
  benItemL <- benGroundItems aid
  b <- getsState $ getActorBody aid
  -- This calmE is outdated when one of the items increases max Calm
  -- (e.g., in pickup, which handles many items at once), but this is OK,
  -- the server accepts item movement based on calm at the start, not end
  -- or in the middle.
  -- The calmE is inaccurate also if an item not IDed, but that's intended
  -- and the server will ignore and warn (and content may avoid that,
  -- e.g., making all rings identified)
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup aid actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` aid
      calmE = calmEnough b ar
      isWeapon (_, (_, item)) = isMelee item
      filterWeapon | onlyWeapon = filter isWeapon
                   | otherwise = id
      prepareOne (oldN, l4) ((_, (k, _)), (iid, item)) =
        let n = oldN + k
            (newN, toCStore)
              | calmE && goesIntoSha item = (oldN, CSha)
              | goesIntoEqp item && eqpOverfull b n =
                (oldN, if calmE then CSha else CInv)
              | goesIntoEqp item = (n, CEqp)
              | otherwise = (oldN, CInv)
        in (newN, (iid, k, CGround, toCStore) : l4)
      (_, prepared) = foldl' prepareOne (0, [])
                      $ filterWeapon $ map (second (second itemBase)) benItemL
  return $! if null prepared
            then reject
            else returN "pickup" $ ReqMoveItems prepared

equipItems :: MonadClient m
           => ActorId -> m (Strategy (RequestTimed 'AbMoveItem))
equipItems aid = do
  cops <- getsState scops
  body <- getsState $ getActorBody aid
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup aid actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` aid
      calmE = calmEnough body ar
  fact <- getsState $ (EM.! bfid body) . sfactionD
  eqpAssocs <- fullAssocsClient aid [CEqp]
  invAssocs <- fullAssocsClient aid [CInv]
  shaAssocs <- fullAssocsClient aid [CSha]
  condAnyFoeAdj <- condAnyFoeAdjM aid
  condShineWouldBetray <- condShineWouldBetrayM aid
  condAimEnemyPresent <- condAimEnemyPresentM aid
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
          _ -> (oldN, l4)
      heavilyDistressed =  -- Actor hit by a projectile or similarly distressed.
        deltaSerious (bcalmDelta body)
      -- We filter out unneeded items. In particular, we ignore them in eqp
      -- when comparing to items we may want to equip. Anyway, the unneeded
      -- items should be removed in yieldUnneeded earlier or soon after.
      filterNeeded (_, itemFull) =
        not $ unneeded cops condAnyFoeAdj condShineWouldBetray
                       condAimEnemyPresent heavilyDistressed (not calmE)
                       body ar fact itemFull
      bestThree = bestByEqpSlot (filter filterNeeded eqpAssocs)
                                (filter filterNeeded invAssocs)
                                (filter filterNeeded shaAssocs)
      bEqpInv = foldl' (improve CInv) (0, [])
                $ map (\(slot, (eqp, inv, _)) ->
                        (slot, (inv, eqp))) bestThree
      bEqpBoth | calmE =
                   foldl' (improve CSha) bEqpInv
                   $ map (\(slot, (eqp, _, sha)) ->
                           (slot, (sha, eqp))) bestThree
               | otherwise = bEqpInv
      (_, prepared) = bEqpBoth
  return $! if null prepared
            then reject
            else returN "equipItems" $ ReqMoveItems prepared

toShare :: IK.EqpSlot -> Bool
toShare IK.EqpSlotMiscBonus = False
toShare IK.EqpSlotMiscAbility = False
toShare _ = True

yieldUnneeded :: MonadClient m
              => ActorId -> m (Strategy (RequestTimed 'AbMoveItem))
yieldUnneeded aid = do
  cops <- getsState scops
  body <- getsState $ getActorBody aid
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup aid actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` aid
      calmE = calmEnough body ar
  fact <- getsState $ (EM.! bfid body) . sfactionD
  eqpAssocs <- fullAssocsClient aid [CEqp]
  condAnyFoeAdj <- condAnyFoeAdjM aid
  condShineWouldBetray <- condShineWouldBetrayM aid
  condAimEnemyPresent <- condAimEnemyPresentM aid
      -- Here AI hides from the human player the Ring of Speed And Bleeding,
      -- which is a bit harsh, but fair. However any subsequent such
      -- rings will not be picked up at all, so the human player
      -- doesn't lose much fun. Additionally, if AI learns alchemy later on,
      -- they can repair the ring, wield it, drop at death and it's
      -- in play again.
  let heavilyDistressed =  -- Actor hit by a projectile or similarly distressed.
        deltaSerious (bcalmDelta body)
      yieldSingleUnneeded (iidEqp, itemEqp) =
        let csha = if calmE then CSha else CInv
        in if | harmful cops body ar fact itemEqp ->
                [(iidEqp, itemK itemEqp, CEqp, CInv)]
              | hinders condAnyFoeAdj condShineWouldBetray
                        condAimEnemyPresent heavilyDistressed (not calmE)
                        body ar itemEqp ->
                [(iidEqp, itemK itemEqp, CEqp, csha)]
              | otherwise -> []
      yieldAllUnneeded = concatMap yieldSingleUnneeded eqpAssocs
  return $! if null yieldAllUnneeded
            then reject
            else returN "yieldUnneeded" $ ReqMoveItems yieldAllUnneeded

unEquipItems :: MonadClient m
             => ActorId -> m (Strategy (RequestTimed 'AbMoveItem))
unEquipItems aid = do
  cops <- getsState scops
  body <- getsState $ getActorBody aid
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup aid actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` aid
      calmE = calmEnough body ar
  fact <- getsState $ (EM.! bfid body) . sfactionD
  eqpAssocs <- fullAssocsClient aid [CEqp]
  invAssocs <- fullAssocsClient aid [CInv]
  shaAssocs <- fullAssocsClient aid [CSha]
  condAnyFoeAdj <- condAnyFoeAdjM aid
  condShineWouldBetray <- condShineWouldBetrayM aid
  condAimEnemyPresent <- condAimEnemyPresentM aid
      -- Here AI hides from the human player the Ring of Speed And Bleeding,
      -- which is a bit harsh, but fair. However any subsequent such
      -- rings will not be picked up at all, so the human player
      -- doesn't lose much fun. Additionally, if AI learns alchemy later on,
      -- they can repair the ring, wield it, drop at death and it's
      -- in play again.
  let improve :: CStore -> ( IK.EqpSlot
                           , ( [(Int, (ItemId, ItemFull))]
                             , [(Int, (ItemId, ItemFull))] ) )
              -> [(ItemId, Int, CStore, CStore)]
      improve fromCStore (slot, (bestSha, bestEOrI)) =
        case (bestSha, bestEOrI) of
          _ | not (toShare slot)
              && fromCStore == CEqp
              && not (eqpOverfull body 1) ->  -- keep minor boosts up to M-1
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
          (_, (vEOrI, (_, _)) : _) | fromCStore == CEqp
                                     && eqpOverfull body 1
                                     && worseThanSha vEOrI bestSha ->
            -- To make place in eqp for an item better than any ours.
            [(fst $ snd $ last bestEOrI, 1, fromCStore, CSha)]
          _ -> []
      getK [] = 0
      getK ((_, (_, itemFull)) : _) = itemK itemFull
      betterThanSha _ [] = True
      betterThanSha vEOrI ((vSha, _) : _) = vEOrI > vSha
      worseThanSha _ [] = False
      worseThanSha vEOrI ((vSha, _) : _) = vEOrI < vSha
      heavilyDistressed =  -- Actor hit by a projectile or similarly distressed.
        deltaSerious (bcalmDelta body)
      filterNeeded (_, itemFull) =
        not $ unneeded cops condAnyFoeAdj condShineWouldBetray
                       condAimEnemyPresent heavilyDistressed (not calmE)
                       body ar fact itemFull
      bestThree =
        bestByEqpSlot eqpAssocs invAssocs (filter filterNeeded shaAssocs)
      bInvSha = concatMap
                  (improve CInv . (\(slot, (_, inv, sha)) ->
                                    (slot, (sha, inv)))) bestThree
      bEqpSha = concatMap
                  (improve CEqp . (\(slot, (eqp, _, sha)) ->
                                    (slot, (sha, eqp)))) bestThree
      prepared = if calmE then bInvSha ++ bEqpSha else []
  return $! if null prepared
            then reject
            else returN "unEquipItems" $ ReqMoveItems prepared

groupByEqpSlot :: [(ItemId, ItemFull)]
               -> EM.EnumMap IK.EqpSlot [(ItemId, ItemFull)]
groupByEqpSlot is =
  let f (iid, itemFull) = case strengthEqpSlot itemFull of
        Nothing -> Nothing
        Just es -> Just (es, [(iid, itemFull)])
      withES = mapMaybe f is
  in EM.fromListWith (++) withES

bestByEqpSlot :: [(ItemId, ItemFull)]
              -> [(ItemId, ItemFull)]
              -> [(ItemId, ItemFull)]
              -> [(IK.EqpSlot
                  , ( [(Int, (ItemId, ItemFull))]
                    , [(Int, (ItemId, ItemFull))]
                    , [(Int, (ItemId, ItemFull))] ) )]
bestByEqpSlot eqpAssocs invAssocs shaAssocs =
  let eqpMap = EM.map (\g -> (g, [], [])) $ groupByEqpSlot eqpAssocs
      invMap = EM.map (\g -> ([], g, [])) $ groupByEqpSlot invAssocs
      shaMap = EM.map (\g -> ([], [], g)) $ groupByEqpSlot shaAssocs
      appendThree (g1, g2, g3) (h1, h2, h3) = (g1 ++ h1, g2 ++ h2, g3 ++ h3)
      eqpInvShaMap = EM.unionsWith appendThree [eqpMap, invMap, shaMap]
      bestSingle = strongestSlot
      bestThree eqpSlot (g1, g2, g3) = (bestSingle eqpSlot g1,
                                        bestSingle eqpSlot g2,
                                        bestSingle eqpSlot g3)
  in EM.assocs $ EM.mapWithKey bestThree eqpInvShaMap

harmful :: Kind.COps -> Actor -> AspectRecord -> Faction -> ItemFull -> Bool
harmful cops body ar fact itemFull =
  -- Items that are known and their effects are not stricly beneficial
  -- should not be equipped (either they are harmful or they waste eqp space).
  maybe False (\(u, _) -> u <= 0)
    (totalUsefulness cops body ar fact itemFull)

unneeded :: Kind.COps -> Bool -> Bool -> Bool -> Bool -> Bool
         -> Actor -> AspectRecord -> Faction -> ItemFull
         -> Bool
unneeded cops condAnyFoeAdj condShineWouldBetray
         condAimEnemyPresent heavilyDistressed condNotCalmEnough
         body ar fact itemFull =
  harmful cops body ar fact itemFull
  || hinders condAnyFoeAdj condShineWouldBetray
             condAimEnemyPresent heavilyDistressed condNotCalmEnough
             body ar itemFull

-- Everybody melees in a pinch, even though some prefer ranged attacks.
meleeBlocker :: MonadClient m => ActorId -> m (Strategy (RequestTimed 'AbMelee))
meleeBlocker aid = do
  b <- getsState $ getActorBody aid
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup aid actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  actorSk <- actorSkillsClient aid
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  case mtgtMPath of
    Just TgtAndPath{tapPath=AndPath{pathList=q : _, pathGoal}} -> do
      -- We prefer the goal (e.g., when no accessible, but adjacent),
      -- but accept @q@ even if it's only a blocking enemy position.
      let maim | adjacent (bpos b) pathGoal = Just pathGoal
               | adjacent (bpos b) q = Just q
               | otherwise = Nothing  -- MeleeDistant
      lBlocker <- case maim of
        Nothing -> return []
        Just aim -> getsState $ posToAssocs aim (blid b)
      case lBlocker of
        (aid2, body2) : _ -> do
          let ar2 = case EM.lookup aid2 actorAspect of
                Just aspectRecord -> aspectRecord
                Nothing -> assert `failure` aid
          -- No problem if there are many projectiles at the spot. We just
          -- attack the first one.
          if | actorDying body2
               || bproj body2  -- displacing saves a move
                  && EM.findWithDefault 0 AbDisplace actorSk <= 0 ->
               return reject
             | isAtWar fact (bfid body2)  -- at war with us, hit, not disp
               || bfid body2 == bfid b  -- don't start a war
                  && EM.findWithDefault 0 AbDisplace actorSk <= 0  -- can't disp
                  && fleaderMode (gplayer fact) == LeaderNull  -- no restrain
                  && EM.findWithDefault 0 AbMove actorSk > 0  -- blocked move
                  && 3 * bhp body2 < bhp b  -- only get rid of weak friends
                  && bspeed body2 ar2 <= bspeed b ar -> do
               mel <- maybeToList <$> pickWeaponClient aid aid2
               return $! liftFrequency $ uniformFreq "melee in the way" mel
             | otherwise -> return reject
        [] -> return reject
    _ -> return reject  -- probably no path to the enemy, if any

-- Everybody melees in a pinch, skills and weapons allowing,
-- even though some prefer ranged attacks.
meleeAny :: MonadClient m => ActorId -> m (Strategy (RequestTimed 'AbMelee))
meleeAny aid = do
  b <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  allFoes <- getsState $ actorRegularAssocs (isAtWar fact) (blid b)
  let adjFoes = filter (adjacent (bpos b) . bpos . snd) allFoes
  mels <- mapM (pickWeaponClient aid . fst) adjFoes
  let freq = uniformFreq "melee adjacent" $ catMaybes mels
  return $! liftFrequency freq

data FleeViaStairsOrEscape = ViaStairs | ViaEscape | ViaNothing
  deriving (Show, Eq)

-- | The level the actor is on is either explored or the actor already
-- has a weapon equipped, so no need to explore further, he tries to find
-- enemies on other levels.
-- We don't verify the stairs are targeted by the actor, but at least
-- the actor doesn't target a visible enemy at this point.
trigger :: MonadClient m
        => ActorId -> FleeViaStairsOrEscape
        -> m (Strategy (RequestTimed 'AbAlter))
trigger aid fleeVia = do
  cops@Kind.COps{coTileSpeedup} <- getsState scops
  dungeon <- getsState sdungeon
  explored <- getsClient sexplored
  b <- getsState $ getActorBody aid
  actorSk <- actorSkillsClient aid
  let alterSkill = EM.findWithDefault 0 AbAlter actorSk
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup aid actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` aid
  fact <- getsState $ (EM.! bfid b) . sfactionD
  lvl <- getLevel (blid b)
  unexploredD <- unexploredDepth
  condEnoughGear <- condEnoughGearM aid
  itemToF <- itemToFullClient
  let aiAlterMinSkill p = Tile.aiAlterMinSkill coTileSpeedup $ lvl `at` p
      lidExplored = ES.member (blid b) explored
      allExplored = ES.size explored == EM.size dungeon
      -- Only actors with high enough AbAlter can use stairs.
      enterableHere p = alterSkill >= fromEnum (aiAlterMinSkill p)
      iidToEffs (iid, kit) = case itemDisco $ itemToF iid kit of
        Nothing -> []
        Just ItemDisco{itemKind} -> IK.ieffects itemKind
      -- Ignoring the number of items, because only one of each @iid@
      -- is triggered at the same time, others are left to be used later on.
      f pos = case EM.lookup pos $ lembed lvl of
        Nothing -> []
        Just bag -> [(pos, concatMap iidToEffs $ EM.assocs bag)]
      feats = concatMap f $ filter enterableHere $ vicinityUnsafe (bpos b)
      bens (_, fs) = do
        bs <- mapM ben fs
        return $! if any (< -10) bs
                  then 0  -- mixed blessing
                  else sum bs
      ben feat = case feat of
        IK.Ascend up -> do  -- change levels sensibly, in teams
          let easier = up /= (fromEnum (blid b) > 0)
              unexpForth = unexploredD up (blid b)
              unexpBack = unexploredD (not up) (blid b)
              -- Forbid loops via peeking at unexplored and getting back.
              aiCond = if unexpForth
                       then easier && condEnoughGear
                            || (not unexpBack || easier) && lidExplored
                       else easier && allExplored && null (lescape lvl)
              -- Prefer one direction of stairs, to team up.
              eben = if aiCond then if easier then 2 else 1 else 0
          return $!
             if fleeVia == ViaStairs -- don't flee prematurely
             then 10000 * eben
             else 0
        ef@IK.Escape{} -> return $  -- flee via this way, too
          -- Only some factions try to escape but they first explore all
          -- for high score.
          if fleeVia /= ViaEscape
             || not (fcanEscape $ gplayer fact)
             || not allExplored
          then 0
          else effectToBenefit cops b ar fact ef
        ef -> return $! if fleeVia == ViaNothing
                        then effectToBenefit cops b ar fact ef
                        else 0
  benFeats <- mapM bens feats
  let benFeat = zip benFeats feats
  return $! liftFrequency $ toFreq "trigger"
    [ (benefit, ReqAlter pos)
    | (benefit, (pos, _)) <- benFeat
    , benefit > 0 ]

projectItem :: MonadClient m
            => ActorId -> m (Strategy (RequestTimed 'AbProject))
projectItem aid = do
  btarget <- getsClient $ getTarget aid
  b <- getsState $ getActorBody aid
  mfpos <- case btarget of
    Nothing -> return Nothing
    Just target -> aidTgtToPos aid (blid b) target
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
          -- and no actors or obstacles along the path.
          let q _ itemFull b2 ar =
                either (const False) id
                $ permittedProject False skill b2 ar "" itemFull
          actorAspect <- getsClient sactorAspect
          let ar = case EM.lookup aid actorAspect of
                Just aspectRecord -> aspectRecord
                Nothing -> assert `failure` aid
              calmE = calmEnough b ar
              stores = [CEqp, CInv, CGround] ++ [CSha | calmE]
          benList <- benAvailableItems aid q stores
          localTime <- getsState $ getLocalTime (blid b)
          let coeff CGround = 2
              coeff COrgan = 3  -- can't give to others
              coeff CEqp = 100000  -- must hinder currently
              coeff CInv = 1
              coeff CSha = 1
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
                               Nothing -> -10  -- experiment if no options
                               Just (_, ben) -> ben
                           * (if recharged then 1 else 0)
                in if -- Melee weapon is usually needed in hand.
                      not (isMelee itemBase)
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
          => ActorId -> ApplyItemGroup -> m (Strategy (RequestTimed 'AbApply))
applyItem aid applyGroup = do
  actorSk <- actorSkillsClient aid
  b <- getsState $ getActorBody aid
  localTime <- getsState $ getLocalTime (blid b)
  let skill = EM.findWithDefault 0 AbApply actorSk
      q _ itemFull _ ar =
        let freq = case itemDisco itemFull of
              Nothing -> []
              Just ItemDisco{itemKind} -> IK.ifreq itemKind
        in maybe True (<= 0) (lookup "gem" freq)
           && either (const False) id
                (permittedApply localTime skill b ar " " itemFull)
  actorAspect <- getsClient sactorAspect
  let ar = case EM.lookup aid actorAspect of
        Just aspectRecord -> aspectRecord
        Nothing -> assert `failure` aid
      calmE = calmEnough b ar
      stores = [CEqp, CInv, CGround] ++ [CSha | calmE]
  benList <- benAvailableItems aid q stores
  organs <- mapM (getsState . getItemBody) $ EM.keys $ borgan b
  let itemLegal itemFull = case applyGroup of
        ApplyFirstAid ->
          let getP (IK.RefillHP p) _ | p > 0 = True
              getP (IK.OverfillHP p) _ | p > 0 = True
              getP _ acc = acc
          in case itemDisco itemFull of
            Just ItemDisco{itemKind=IK.ItemKind{IK.ieffects}} ->
              foldr getP False ieffects
            _ -> False
        ApplyAll -> True
      coeff CGround = 2
      coeff COrgan = 3  -- can't give to others
      coeff CEqp = 100000  -- must hinder currently
      coeff CInv = 1
      coeff CSha = 1
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
                       -- foes' skin than ours
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

-- We assume @aid@ is a foe and so @dispEnemy@ is binding.
-- The result of all these conditions is that AI displaces rarely,
-- but it can't be helped as long as the enemy is smart enough to form fronts.
displaceFoe :: MonadClient m => ActorId -> m (Strategy RequestAnyAbility)
displaceFoe aid = do
  Kind.COps{coTileSpeedup} <- getsState scops
  b <- getsState $ getActorBody aid
  lvl <- getLevel $ blid b
  fact <- getsState $ (EM.! bfid b) . sfactionD
  let friendlyFid fid = fid == bfid b || isAllied fact fid
  friends <- getsState $ actorRegularList friendlyFid (blid b)
  allFoes <- getsState $ actorRegularAssocs (isAtWar fact) (blid b)
  let displaceable body =  -- DisplaceAccess
        adjacent (bpos body) (bpos b)
        && Tile.isWalkable coTileSpeedup (lvl `at` (bpos body))
      nFriends body = length $ filter (adjacent (bpos body) . bpos) friends
      nFrHere = nFriends b + 1
      qualifyActor (aid2, body2) = do
        actorMaxSk <- enemyMaxAb aid2
        dEnemy <- getsState $ dispEnemy aid aid2 actorMaxSk
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
  b <- getsState $ getActorBody aid
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  str <- case mtgtMPath of
    Just TgtAndPath{tapPath=AndPath{pathList=q : _}} ->
      displaceTowards aid (bpos b) q
    _ -> return reject  -- goal reached
  mapStrategyM (moveOrRunAid True aid) str

displaceTowards :: MonadClient m
                => ActorId -> Point -> Point -> m (Strategy Vector)
displaceTowards aid source target = do
  Kind.COps{coTileSpeedup} <- getsState scops
  b <- getsState $ getActorBody aid
  let !_A = assert (source == bpos b && adjacent source target) ()
  lvl <- getLevel $ blid b
  if boldpos b /= Just target -- avoid trivial loops
     && Tile.isWalkable coTileSpeedup (lvl `at` target) then do
       -- DisplaceAccess
    mleader <- getsClient _sleader
    mBlocker <- getsState $ posToAssocs target (blid b)
    case mBlocker of
      [] -> return reject
      [(aid2, b2)] | Just aid2 /= mleader -> do
        mtgtMPath <- getsClient $ EM.lookup aid2 . stargetD
        actorAspect <- getsClient sactorAspect
        case mtgtMPath of
          Just TgtAndPath{tapPath=AndPath{pathList=q : _}}
            | q == source  -- friend wants to swap
              || waitedLastTurn b2  -- he had no progress anyway
              || actorCanMelee actorAspect aid b
                 && (not $ actorCanMelee actorAspect aid2 b2) -> do
                   -- he can't meelee and I can, so push him aside
              return $! returN "displace friend" $ target `vectorToFrom` source
          Just _ -> return reject
          Nothing -> do  -- an enemy or disoriented friend --- swap
            tfact <- getsState $ (EM.! bfid b2) . sfactionD
            actorMaxSk <- enemyMaxAb aid2
            dEnemy <- getsState $ dispEnemy aid aid2 actorMaxSk
            if not (isAtWar tfact (bfid b)) || dEnemy then
              return $! returN "displace other" $ target `vectorToFrom` source
            else return reject  -- DisplaceDying, etc.
      _ -> return reject  -- DisplaceProjectiles or trying to displace leader
  else return reject

chase :: MonadClient m
      => ActorId -> Bool -> Bool -> m (Strategy RequestAnyAbility)
chase aid doDisplace avoidAmbient = do
  Kind.COps{coTileSpeedup} <- getsState scops
  body <- getsState $ getActorBody aid
  fact <- getsState $ (EM.! bfid body) . sfactionD
  mtgtMPath <- getsClient $ EM.lookup aid . stargetD
  lvl <- getLevel $ blid body
  let isAmbient pos = Tile.isLit coTileSpeedup (lvl `at` pos)
  str <- case mtgtMPath of
    Just TgtAndPath{tapPath=AndPath{pathList=q : _, ..}}
      | pathGoal == bpos body -> return reject  -- shortcut and just to be sure
      | not $ avoidAmbient && isAmbient q ->
      -- With no leader, the goal is vague, so permit arbitrary detours.
      moveTowards aid q pathGoal (fleaderMode (gplayer fact) == LeaderNull)
    _ -> return reject  -- goal reached or banned ambient lit tile
  -- If @doDisplace@: don't pick fights, assuming the target is more important.
  -- We'd normally melee the target earlier on via @AbMelee@, but for
  -- actors that don't have this ability (and so melee only when forced to),
  -- this is meaningul.
  mapStrategyM (moveOrRunAid doDisplace aid) str

moveTowards :: MonadClient m
            => ActorId -> Point -> Point -> Bool -> m (Strategy Vector)
moveTowards aid target goal relaxed = do
  b <- getsState $ getActorBody aid
  actorSk <- actorSkillsClient aid
  let source = bpos b
      alterSkill = EM.findWithDefault 0 AbAlter actorSk
      !_A = assert (source == bpos b
                    `blame` (source, bpos b, aid, b, goal)) ()
      !_B = assert (adjacent source target
                    `blame` (source, target, aid, b, goal)) ()
  fact <- getsState $ (EM.! bfid b) . sfactionD
  salter <- getsClient salter
  let noF = isAtWar fact . bfid
  noFriends <- getsState $ \s p -> all (noF . snd) $ posToAssocs p (blid b) s
  let lalter = salter EM.! blid b
      -- Only actors with AbAlter can search for hidden doors, etc.
      enterableHere p = alterSkill >= fromEnum (lalter PointArray.! p)
  if noFriends target && enterableHere target then
    return $! returN "moveTowards adjacent" $ target `vectorToFrom` source
  else do
    let goesBack v = maybe False (\oldpos -> v == oldpos `vectorToFrom` source)
                           (boldpos b)
        nonincreasing p = chessDist source goal >= chessDist p goal
        isSensible | relaxed = \p -> noFriends p
                                     && enterableHere p
                   | otherwise = \p -> nonincreasing p
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
  Kind.COps{coTileSpeedup} <- getsState scops
  sb <- getsState $ getActorBody source
  actorSk <- actorSkillsClient source
  let lid = blid sb
  lvl <- getLevel lid
  let alterSkill = EM.findWithDefault 0 AbAlter actorSk
      spos = bpos sb           -- source position
      tpos = spos `shift` dir  -- target position
      t = lvl `at` tpos
  -- We start by checking actors at the target position,
  -- which gives a partial information (actors can be invisible),
  -- as opposed to accessibility (and items) which are always accurate
  -- (tiles can't be invisible).
  tgts <- getsState $ posToAssocs tpos lid
  case tgts of
    [(target, b2)] | run -> do
      -- @target@ can be a foe, as well as a friend.
      tfact <- getsState $ (EM.! bfid b2) . sfactionD
      actorMaxSk <- enemyMaxAb target
      dEnemy <- getsState $ dispEnemy source target actorMaxSk
      if | boldpos sb == Just tpos && not (waitedLastTurn sb)
             -- avoid Displace loops
           || not (Tile.isWalkable coTileSpeedup $ lvl `at` tpos) ->
             -- DisplaceAccess
           return Nothing
         | isAtWar tfact (bfid sb) && not dEnemy -> do  -- DisplaceDying, etc.
           wps <- pickWeaponClient source target
           case wps of
             Nothing -> return Nothing
             Just wp -> return $! Just $ RequestAnyAbility wp
         | otherwise ->
           return $! Just $ RequestAnyAbility $ ReqDisplace target
    (target, _) : _ -> do  -- can be a foe, as well as friend (e.g., proj.)
      -- No problem if there are many projectiles at the spot. We just
      -- attack the first one.
      -- Attacking does not require full access, adjacency is enough.
      wps <- pickWeaponClient source target
      case wps of
        Nothing -> return Nothing
        Just wp -> return $! Just $ RequestAnyAbility wp
    [] -- move or search or alter
       | Tile.isWalkable coTileSpeedup $ lvl `at` tpos ->
         -- Movement requires full access.
         return $! Just $ RequestAnyAbility $ ReqMove dir
         -- The potential invisible actor is hit.
       | alterSkill < Tile.alterMinWalk coTileSpeedup t ->
         assert `failure` "AI causes AlterUnwalked" `twith` (run, source, dir)
       | EM.member tpos $ lfloor lvl ->
         -- Only possible if items allowed inside unwalkable tiles.
         assert `failure` "AI causes AlterBlockItem" `twith` (run, source, dir)
       | otherwise -> do
         -- Not walkable, but alter skill suffices, so search or alter the tile.
         return $ Just $ RequestAnyAbility $ ReqAlter tpos
