-- | Client monad for interacting with a human through UI.
module Game.LambdaHack.Client.UI.MsgClient
  ( msgAdd, promptAdd, promptAddAttr, recordHistory
  , MError, SlideOrCmd, failWith, failSer, failMsg, weaveJust
  , lookAt, itemOverlay, overlayToSlideshow, reportToSlideshow
  ) where

import Prelude ()
import Prelude.Compat

import Control.Exception.Assert.Sugar
import Control.Monad
import qualified Data.EnumMap.Strict as EM
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Game.LambdaHack.Common.Kind as Kind
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.CommonClient
import Game.LambdaHack.Client.ItemSlot
import Game.LambdaHack.Client.MonadClient hiding (liftIO)
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Item
import Game.LambdaHack.Common.ItemDescription
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import qualified Game.LambdaHack.Content.TileKind as TK

-- | Add a message to the current report.
msgAdd :: MonadClientUI m => Text -> m ()
msgAdd msg = modifySession $ \sess ->
  sess {_sreport = snocReport (_sreport sess) (toMsg msg)}

-- | Add a prompt to the current report.
promptAdd :: MonadClientUI m => Text -> m ()
promptAdd msg = modifySession $ \sess ->
  sess {_sreport = snocReport (_sreport sess) (toPrompt $ toAttrLine msg)}

-- | Add a prompt to the current report.
promptAddAttr :: MonadClientUI m => AttrLine -> m ()
promptAddAttr msg = modifySession $ \sess ->
  sess {_sreport = snocReport (_sreport sess) (toPrompt msg)}

-- | Store current report in the history and reset report.
recordHistory :: MonadClientUI m => m ()
recordHistory = do
  time <- getsState stime
  SessionUI{_sreport, shistory} <- getSession
  unless (nullReport _sreport) $ do
    let nhistory = addReport shistory time _sreport
    modifySession $ \sess -> sess { _sreport = emptyReport
                                  , shistory = nhistory }

type MError = Maybe Text

newtype FailError = FailError Text
  deriving Show

type SlideOrCmd a = Either FailError a

failWith :: MonadClientUI m => Text -> m (SlideOrCmd a)
failWith err = assert (not $ T.null err) $ return $ Left $ FailError err

failSer :: MonadClientUI m => ReqFailure -> m (SlideOrCmd a)
failSer = failWith . showReqFailure

failMsg :: MonadClientUI m => Text -> m MError
failMsg err = assert (not $ T.null err) $ return $ Just err

weaveJust :: SlideOrCmd RequestUI -> Either MError RequestUI
weaveJust (Left (FailError err)) = Left $ Just err
weaveJust (Right a) = Right a

-- | Produces a textual description of the terrain and items at an already
-- explored position. Mute for unknown positions.
-- The detailed variant is for use in the aiming mode.
lookAt :: MonadClientUI m
       => Bool       -- ^ detailed?
       -> Text       -- ^ how to start tile description
       -> Bool       -- ^ can be seen right now?
       -> Point      -- ^ position to describe
       -> ActorId    -- ^ the actor that looks
       -> Text       -- ^ an extra sentence to print
       -> m Text
lookAt detailed tilePrefix canSee pos aid msg = do
  cops@Kind.COps{cotile=cotile@Kind.Ops{okind}} <- getsState scops
  itemToF <- itemToFullClient
  b <- getsState $ getActorBody aid
  saimMode <- getsSession saimMode
  let lidV = maybe (blid b) aimLevelId saimMode
  lvl <- getLevel lidV
  localTime <- getsState $ getLocalTime lidV
  subject <- partAidLeader aid
  is <- getsState $ getCBag $ CFloor lidV pos
  let verb = MU.Text $ if | pos == bpos b -> "stand on"
                          | canSee -> "notice"
                          | otherwise -> "remember"
  let nWs (iid, kit@(k, _)) = partItemWs k CGround localTime (itemToF iid kit)
      isd = if | EM.size is == 0 -> ""
               | EM.size is <= 2 ->
                 makeSentence [ MU.SubjectVerbSg subject verb
                              , MU.WWandW $ map nWs $ EM.assocs is]
               | otherwise ->
                 makeSentence [MU.Cardinal (EM.size is), "items here"]
      tile = lvl `at` pos
      obscured | knownLsecret lvl
                 && tile /= hideTile cops lvl pos = "partially obscured"
               | otherwise = ""
      tileText = obscured <+> TK.tname (okind tile)
      tilePart | T.null tilePrefix = MU.Text tileText
               | otherwise = MU.AW $ MU.Text tileText
      tileDesc = [MU.Text tilePrefix, tilePart]
  if | not (null (Tile.causeEffects cotile tile)) ->
       return $! makeSentence ("activable:" : tileDesc)
                 <+> msg <+> isd
     | detailed ->
       return $! makeSentence tileDesc
                 <+> msg <+> isd
     | otherwise ->
       return $! msg <+> isd

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
                ov = updateOverlayLine 0 insertSymbol $ toOverlay [phrase]
                ekm = Right l
                kx = (ekm, (undefined, 0, T.length phrase))
            in Just (ov, kx)
      (ts, kxs) = unzip $ mapMaybe pr $ EM.assocs lSlots
  return (mconcat ts, kxs)

-- | The prompt is shown after the current message at the top of each slide.
-- Together they may take more than one line. The prompt is not added
-- to history. The portions of overlay that fit on the the rest
-- of the screen are displayed below. As many slides as needed are shown.
overlayToSlideshow :: MonadClientUI m => Overlay -> m Slideshow
overlayToSlideshow overlay = do
  lid <- getArenaUI
  Level{lxsize, lysize} <- getLevel lid  -- TODO: screen length or viewLevel
  report <- getReport
  let msg = splitReport lxsize report
  return $! splitOverlay (lysize + 1) msg overlay

reportToSlideshow :: MonadClientUI m => m Slideshow
reportToSlideshow = overlayToSlideshow mempty
