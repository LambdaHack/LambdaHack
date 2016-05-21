{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Slideshows.
module Game.LambdaHack.Client.UI.Slideshow
  ( KYX, OKX, keyOfEKM
  , Slideshow(slideshow), toSlideshow, menuToSlideshow, textsToSlideshow
  , splitOverlay
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Client.ItemSlot
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Client.UI.Overlay
import Game.LambdaHack.Common.Point

type KYX = (Either K.KM SlotChar, (Y, X, X))

-- Neither list may be empty.
type OKX = (Overlay, [KYX])

keyOfEKM :: Int -> Either K.KM SlotChar -> Maybe K.KM
keyOfEKM _ (Left km) = Just km
keyOfEKM numPrefix (Right SlotChar{..}) | slotPrefix == numPrefix =
  Just $ K.KM K.NoModifier $ K.Char slotChar
keyOfEKM _ _ = Nothing

-- May be empty, but nothing inside may be empty.
newtype Slideshow = Slideshow {slideshow :: [OKX]}
  deriving (Show, Eq, Monoid)

toSlideshow :: [OKX] -> Slideshow
toSlideshow okxs = Slideshow $ addFooters okxs
 where
  addFooters [] = assert `failure` okxs
  addFooters [(als, kxs)] =
    [( als ++ [toAttrLine tendMsg]
     , kxs ++ [(Left K.escKM, (length als, 0, 8))] )]
  addFooters ((als, kxs) : rest) =
    ( als ++ [toAttrLine tmoreMsg]
    , kxs ++ [(Left K.pgdnKM, (length als, 0, 8))] )
    : addFooters rest

menuToSlideshow :: OKX -> Slideshow
menuToSlideshow (als, kxs) =
  assert (not (null als || null kxs)) $ Slideshow [(als, kxs)]

textsToSlideshow :: [[Text]] -> Slideshow
textsToSlideshow = toSlideshow . map (\t -> (map toAttrLine t, []))

-- TODO: assert that ov0 nonempty and perhaps that kxs0 not too short
-- (or should we just keep the rest of the overlay unclickable?)
splitOverlay :: X -> Y -> Report -> OKX -> Slideshow
splitOverlay lxsize yspace report (ls0, kxs0) =
  let rrep = renderReport report
      msg = splitAttrLine lxsize rrep
      msg0 = if yspace - length msg - 1 <= 0  -- all space taken by @msg@
             then [rrep]  -- will display "$" (unless has EOLs)
             else msg
      len = length msg0
      renumber y (km, (_, x1, x2)) = (km, (y, x1, x2))
      zipRenumber = zipWith renumber [len..]
      splitO ls kxs =
        let (pre, post) = splitAt (yspace - 1) $ msg0 ++ ls
        in if null post
           then [(pre, zipRenumber kxs)]  -- all fits on one screen
           else let (preX, postX) = splitAt (yspace - len - 1) kxs
                in (pre, zipRenumber preX) : splitO post postX
      okxs = splitO ls0 kxs0
  in assert (not $ null okxs) $ toSlideshow okxs
