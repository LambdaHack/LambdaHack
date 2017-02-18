-- | A set of Overlay monad operations.
module Game.LambdaHack.Client.UI.OverlayM
  ( describeMainKeys, lookAt
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Client.CommonM
import Game.LambdaHack.Client.MonadClient
import Game.LambdaHack.Client.State
import Game.LambdaHack.Client.UI.Config
import Game.LambdaHack.Client.UI.MonadClientUI
import Game.LambdaHack.Client.UI.SessionUI
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.ItemDescription
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Content.TileKind as TK

describeMainKeys :: MonadClientUI m => m Text
describeMainKeys = do
  saimMode <- getsSession saimMode
  Config{configVi, configLaptop} <- getsSession sconfig
  xhair <- getsClient sxhair
  let moveKeys | configVi = "keypad or hjklyubn"
               | configLaptop = "keypad or uk8o79jl"
               | otherwise = "keypad"
      keys | isNothing saimMode =
        "Explore with" <+> moveKeys <+> "keys or mouse."
           | otherwise =
        "Aim" <+> tgtKindDescription xhair
        <+> "with" <+> moveKeys <+> "keys or mouse."
  return $! keys

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
  Kind.COps{cotile=Kind.Ops{okind}} <- getsState scops
  itemToF <- itemToFullClient
  b <- getsState $ getActorBody aid
  lidV <- viewedLevelUI
  lvl <- getLevel lidV
  localTime <- getsState $ getLocalTime lidV
  subject <- partAidLeader aid
  is <- getsState $ getFloorBag lidV pos
  side <- getsClient sside
  factionD <- getsState sfactionD
  let verb = MU.Text $ if | pos == bpos b -> "stand on"
                          | canSee -> "notice"
                          | otherwise -> "remember"
  let nWs (iid, kit@(k, _)) =
        partItemWs side factionD k CGround localTime (itemToF iid kit)
      isd = if EM.size is == 0 then ""
            else makeSentence [ MU.SubjectVerbSg subject verb
                              , MU.WWandW $ map nWs $ EM.assocs is]
      tile = lvl `at` pos
      tileText = TK.tname (okind tile)
      tilePart | T.null tilePrefix = MU.Text tileText
               | otherwise = MU.AW $ MU.Text tileText
      tileDesc = [MU.Text tilePrefix, tilePart]
  if | detailed ->
       return $! makeSentence tileDesc <+> msg <+> isd
     | otherwise ->
       return $! msg <+> isd
