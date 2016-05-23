{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Slideshows.
module Game.LambdaHack.Client.UI.Slideshow
  ( KYX, OKX, Slideshow(slideshow)
  , unsnoc, toSlideshow, menuToSlideshow, textsToSlideshow
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

type OKX = (Overlay, [KYX])

-- May be empty, but both of each @OKX@ list have to be nonempty.
-- Guaranteed by construction.
newtype Slideshow = Slideshow {slideshow :: [OKX]}
  deriving (Show, Eq, Monoid)

unsnoc :: Slideshow -> Maybe (Slideshow, OKX)
unsnoc Slideshow{slideshow} =
  case reverse slideshow of
    [] -> Nothing
    okx : rest -> Just (Slideshow $ reverse rest, okx)

toSlideshow :: [OKX] -> Slideshow
toSlideshow okxs = Slideshow $ addFooters okxs
 where
  addFooters [] = assert `failure` okxs
  addFooters [(als, kxs)] =
    [( als ++ [toAttrLine tendMsg]
     , kxs ++ [(Left K.spaceKM, (length als, 0, 8))] )]
  addFooters ((als, kxs) : rest) =
    ( als ++ [toAttrLine tmoreMsg]
    , kxs ++ [(Left K.spaceKM, (length als, 0, 8))] )
    : addFooters rest

menuToSlideshow :: OKX -> Slideshow
menuToSlideshow (als, kxs) =
  assert (not (null als || null kxs)) $ Slideshow [(als, kxs)]

textsToSlideshow :: [[Text]] -> Slideshow
textsToSlideshow = toSlideshow . map (\t -> (map toAttrLine t, []))

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
  in toSlideshow okxs
