-- | Client monad for interacting with a human through UI.
module Game.LambdaHack.Client.UI.MsgClient
  ( msgAdd, msgReset, recordHistory
  , SlideOrCmd, failWith, failSlides, failSer
  , lookAt, itemOverlay, floorItemOverlay
  ) where

import Control.Arrow (first)
import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import qualified Data.IntMap.Strict as IM
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Game.LambdaHack.Common.Kind as Kind
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.CommonClient
import Game.LambdaHack.Client.ItemSlot
import Game.LambdaHack.Client.MonadClient hiding (liftIO)
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.WidgetClient
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Content.TileKind

-- | Add a message to the current report.
msgAdd :: MonadClientUI m => Msg -> m ()
msgAdd msg = modifyClient $ \d -> d {sreport = addMsg (sreport d) msg}

-- | Wipe out and set a new value for the current report.
msgReset :: MonadClientUI m => Msg -> m ()
msgReset msg = modifyClient $ \d -> d {sreport = singletonReport msg}

-- | Store current report in the history and reset report.
recordHistory :: MonadClientUI m => m ()
recordHistory = do
  StateClient{sreport, shistory} <- getClient
  unless (nullReport sreport) $ do
    Config{configHistoryMax} <- askConfig
    msgReset ""
    let nhistory = takeHistory configHistoryMax $! addReport sreport shistory
    modifyClient $ \cli -> cli {shistory = nhistory}

type SlideOrCmd a = Either Slideshow a

failWith :: MonadClientUI m => Msg -> m (SlideOrCmd a)
failWith msg = do
  modifyClient $ \cli -> cli {slastKey = Nothing}
  stopPlayBack
  assert (not $ T.null msg) $ fmap Left $ promptToSlideshow msg

failSlides :: MonadClientUI m => Slideshow -> m (SlideOrCmd a)
failSlides slides = do
  modifyClient $ \cli -> cli {slastKey = Nothing}
  stopPlayBack
  return $ Left slides

failSer :: MonadClientUI m => ReqFailure -> m (SlideOrCmd a)
failSer = failWith . showReqFailure

-- TODO: probably move somewhere (Level?)
-- | Produces a textual description of the terrain and items at an already
-- explored position. Mute for unknown positions.
-- The detailed variant is for use in the targeting mode.
lookAt :: MonadClientUI m
       => Bool       -- ^ detailed?
       -> Text       -- ^ how to start tile description
       -> Bool       -- ^ can be seen right now?
       -> Point      -- ^ position to describe
       -> ActorId    -- ^ the actor that looks
       -> Text       -- ^ an extra sentence to print
       -> m Text
lookAt detailed tilePrefix canSee pos aid msg = do
  Kind.COps{cotile=cotile@Kind.Ops{okind}} <- getsState scops
  itemToF <- itemToFullClient
  lidV <- viewedLevel
  lvl <- getLevel lidV
  b <- getsState $ getActorBody aid
  subject <- partAidLeader aid
  let is = lvl `atI` pos
      verb = MU.Text $ if pos == bpos b
                       then "stand on"
                       else if canSee then "notice" else "remember"
  let nWs (iid, k) = partItemWs k (itemToF iid)
      isd = case detailed of
              _ | EM.size is == 0 -> ""
              _ | EM.size is <= 2 ->
                makeSentence [ MU.SubjectVerbSg subject verb
                             , MU.WWandW $ map nWs $ EM.assocs is]
              True -> "Items:"
              _ -> "Items here."
      tile = lvl `at` pos
      obscured | tile /= hideTile cotile lvl pos = "partially obscured"
               | otherwise = ""
      tileText = obscured <+> tname (okind tile)
      tilePart | T.null tilePrefix = MU.Text tileText
               | otherwise = MU.AW $ MU.Text tileText
      tileDesc = [MU.Text tilePrefix, tilePart]
  if not (null (Tile.causeEffects cotile tile)) then
    return $! makeSentence ("activable:" : tileDesc)
              <+> msg <+> isd
  else if detailed then
    return $! makeSentence tileDesc
              <+> msg <+> isd
  else return $! msg <+> isd

-- | Create a list of item names.
itemOverlay :: MonadClient m => ItemBag -> ItemSlots -> m Overlay
itemOverlay bag (letterSlots, numberSlots) = do
  itemToF <- itemToFullClient
  let pr (l, iid) = makePhrase [ slotLabel l
                               , partItemWs (bag EM.! iid) (itemToF iid) ]
                    <> " "
  return $! toOverlay $ map pr $ map (first Left) (EM.assocs letterSlots)
                                 ++ (map (first Right) (IM.assocs numberSlots))

-- | Create a list of item names.
floorItemOverlay :: MonadClient m => ItemBag -> m Overlay
floorItemOverlay bag = do
  let len = length allSlots
      iids = EM.keys bag
      letterSlots = EM.fromAscList $ zip allSlots iids
      numberSlots = IM.fromAscList $ zip [0..] (drop len iids)
  itemOverlay bag (letterSlots, numberSlots)
