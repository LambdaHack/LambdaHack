-- | Helper functions for both inventory management and human commands.
module Game.LambdaHack.Client.UI.HandleHelperM
  ( MError, FailOrCmd
  , showFailError, failWith, failSer, failMsg, weaveJust
  , memberCycle, memberBack, partyAfterLeader, pickLeader
  , itemOverlay, statsOverlay
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import Data.Ord
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.CommonM
import Game.LambdaHack.Client.ItemSlot
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.MsgM
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.OverlayM
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Client.UI.Slideshow
import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemDescription
import Game.LambdaHack.Common.ItemStrongest
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Content.ItemKind as IK

newtype FailError = FailError Text
  deriving Show

showFailError :: FailError -> Text
showFailError (FailError err) = "*" <> err <> "*"

type MError = Maybe FailError

type FailOrCmd a = Either FailError a

failWith :: MonadClientUI m => Text -> m (FailOrCmd a)
failWith err = assert (not $ T.null err) $ return $ Left $ FailError err

failSer :: MonadClientUI m => ReqFailure -> m (FailOrCmd a)
failSer = failWith . showReqFailure

failMsg :: MonadClientUI m => Text -> m MError
failMsg err = assert (not $ T.null err) $ return $ Just $ FailError err

weaveJust :: FailOrCmd RequestUI -> Either MError RequestUI
weaveJust (Left ferr) = Left $ Just ferr
weaveJust (Right a) = Right a

-- | Switches current member to the next on the level, if any, wrapping.
memberCycle :: MonadClientUI m => Bool -> m MError
memberCycle verbose = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  leader <- getLeaderUI
  body <- getsState $ getActorBody leader
  hs <- partyAfterLeader leader
  let autoLvl = snd $ autoDungeonLevel fact
  case filter (\(_, b) -> blid b == blid body) hs of
    _ | autoLvl -> failMsg $ showReqFailure NoChangeLvlLeader
    [] -> failMsg "cannot pick any other member on this level"
    (np, b) : _ -> do
      success <- pickLeader verbose np
      let !_A = assert (success `blame` "same leader"
                                `twith` (leader, np, b)) ()
      return Nothing

-- | Switches current member to the previous in the whole dungeon, wrapping.
memberBack :: MonadClientUI m => Bool -> m MError
memberBack verbose = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  leader <- getLeaderUI
  hs <- partyAfterLeader leader
  let (autoDun, autoLvl) = autoDungeonLevel fact
  case reverse hs of
    _ | autoDun -> failMsg $ showReqFailure NoChangeDunLeader
    _ | autoLvl -> failMsg $ showReqFailure NoChangeLvlLeader
    [] -> failMsg "no other member in the party"
    (np, b) : _ -> do
      success <- pickLeader verbose np
      let !_A = assert (success `blame` "same leader"
                                `twith` (leader, np, b)) ()
      return Nothing

partyAfterLeader :: MonadStateRead m => ActorId -> m [(ActorId, Actor)]
partyAfterLeader leader = do
  faction <- getsState $ bfid . getActorBody leader
  allA <- getsState $ EM.assocs . sactorD
  let factionA = filter (\(_, body) ->
        not (bproj body) && bfid body == faction) allA
      hs = sortBy (comparing keySelected) factionA
      i = fromMaybe (-1) $ findIndex ((== leader) . fst) hs
      (lt, gt) = (take i hs, drop (i + 1) hs)
  return $! gt ++ lt

-- | Select a faction leader. False, if nothing to do.
pickLeader :: MonadClientUI m => Bool -> ActorId -> m Bool
pickLeader verbose aid = do
  leader <- getLeaderUI
  saimMode <- getsSession saimMode
  if leader == aid
    then return False -- already picked
    else do
      pbody <- getsState $ getActorBody aid
      let !_A = assert (not (bproj pbody)
                        `blame` "projectile chosen as the leader"
                        `twith` (aid, pbody)) ()
      -- Even if it's already the leader, give his proper name, not 'you'.
      let subject = partActor pbody
      when verbose $ msgAdd $ makeSentence [subject, "picked as a leader"]
      -- Update client state.
      s <- getState
      modifyClient $ updateLeader aid s
      -- Move the xhair, if active, to the new level.
      case saimMode of
        Nothing -> return ()
        Just _ ->
          modifySession $ \sess -> sess {saimMode = Just $ AimMode $ blid pbody}
      -- Inform about items, etc.
      lookMsg <- lookAt False "" True (bpos pbody) aid ""
      when verbose $ msgAdd lookMsg
      return True

-- | Create a list of item names.
itemOverlay :: MonadClient m => CStore -> LevelId -> ItemBag -> m OKX
itemOverlay c lid bag = do
  localTime <- getsState $ getLocalTime lid
  itemToF <- itemToFullClient
  (itemSlots, organSlots) <- getsClient sslots
  let isOrgan = c == COrgan
      lSlots = if isOrgan then organSlots else itemSlots
      !_A = assert (all (`elem` EM.elems lSlots) (EM.keys bag)
                    `blame` (c, lid, bag, lSlots)) ()
      pr (l, iid) =
        case EM.lookup iid bag of
          Nothing -> Nothing
          Just kit@(k, _) ->
            let itemFull = itemToF iid kit
                label = slotLabel l
                phrase = makePhrase
                           [ MU.Text label
                           , "D"  -- dummy
                           , partItemWs k c localTime itemFull ]
                insertSymbol line =
                  let colorSymbol = uncurry (flip Color.AttrChar)
                                            (viewItem $ itemBase itemFull)
                  in take (T.length label + 1) line
                     ++ [colorSymbol]
                     ++ drop (T.length label + 2) line
                ov = updateOverlayLine 0 insertSymbol [toAttrLine phrase]
                ekm = Right l
                kx = (ekm, (undefined, 0, T.length phrase))
            in Just (ov, kx)
      (ts, kxs) = unzip $ mapMaybe pr $ EM.assocs lSlots
  return (concat ts, kxs)

statsOverlay :: MonadClient m => ActorId -> m OKX
statsOverlay aid = do
  b <- getsState $ getActorBody aid
  activeItems <- activeItemsClient aid
  let block n = n + if braced b then 50 else 0
      prSlot :: SlotChar -> (IK.EqpSlot, Int -> Text) -> (Text, KYX)
      prSlot c (eqpSlot, f) =
        let fullText t =
              makePhrase [ MU.Text $ slotLabel c
                         , MU.Text $ T.justifyLeft 22 ' '
                                   $ IK.slotName eqpSlot
                         , MU.Text t ]
            valueText = f $ sumSlotNoFilter eqpSlot activeItems
            ft = fullText valueText
        in (ft, (Right c, (undefined, 0, T.length ft)))
      -- Some values can be negative, for others 0 is equivalent but shorter.
      slotList =  -- TODO:  [IK.EqpSlotAddHurtMelee..IK.EqpSlotAddLight]
        [ (IK.EqpSlotAddHurtMelee, \t -> tshow t <> "%")
        -- TODO: not applicable right now, IK.EqpSlotAddHurtRanged
        , (IK.EqpSlotAddArmorMelee, \t -> "[" <> tshow (block t) <> "%]")
        , (IK.EqpSlotAddArmorRanged, \t -> "{" <> tshow (block t) <> "%}")
        , (IK.EqpSlotAddMaxHP, \t -> tshow $ max 0 t)
        , (IK.EqpSlotAddMaxCalm, \t -> tshow $ max 0 t)
        , (IK.EqpSlotAddSpeed, \t -> tshow (max 0 t) <> "m/10s")
        , (IK.EqpSlotAddSight, \t ->
            tshow (max 0 $ min (fromIntegral $ bcalm b `div` (5 * oneM)) t)
            <> "m")
        , (IK.EqpSlotAddSmell, \t -> tshow (max 0 t) <> "m")
        , (IK.EqpSlotAddLight, \t -> tshow (max 0 t) <> "m")
        ]
      skills = sumSkills activeItems
      -- TODO: are negative total skills meaningful?
      -- TODO: unduplicate with prSlot
      prAbility :: SlotChar -> Ability.Ability -> (Text, KYX)
      prAbility c ability =
        let fullText t =
              makePhrase [ MU.Text $ slotLabel c
                         , MU.Text $ T.justifyLeft 22 ' '
                           $ "ability" <+> tshow ability
                         , MU.Text t ]
            valueText = tshow $ EM.findWithDefault 0 ability skills
            ft = fullText valueText
        in (ft, (Right c, (undefined, 0, T.length ft)))
      abilityList = [minBound..maxBound]
      reslot c = either (prSlot c) (prAbility c)
      zipReslot = zipWith reslot allZeroSlots
      (ts, kxs) = unzip $ zipReslot $ map Left slotList ++ map Right abilityList
  return (map toAttrLine ts, kxs)
