{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Slideshows.
module Game.LambdaHack.Client.UI.Slideshow
  ( KYX, OKX, Slideshow(slideshow)
  , emptySlideshow, unsnoc, toSlideshow, menuToSlideshow, textsToSlideshow
  , splitOverlay
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Text as T

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
  deriving (Show, Eq)

emptySlideshow :: Slideshow
emptySlideshow = Slideshow []

unsnoc :: Slideshow -> Maybe (Slideshow, OKX)
unsnoc Slideshow{slideshow} =
  case reverse slideshow of
    [] -> Nothing
    okx : rest -> Just (Slideshow $ reverse rest, okx)

toSlideshow :: [OKX] -> Slideshow
toSlideshow okxs = Slideshow $ addFooters okxs
 where
  addFooters [] = assert `failure` okxs
  addFooters [(als, [])] =
    [( als ++ [toAttrLine tendMsg]
     , [(Left K.spaceKM, (length als, 0, 8))] )]
  addFooters [(als, kxs)] = [(als, kxs)]
  addFooters ((als, kxs) : rest) =
    ( als ++ [toAttrLine tmoreMsg]
    , kxs ++ [(Left K.spaceKM, (length als, 0, 8))] )
    : addFooters rest

menuToSlideshow :: OKX -> Slideshow
menuToSlideshow (als, kxs) =
  assert (not (null als || null kxs)) $ Slideshow [(als, kxs)]

textsToSlideshow :: [[Text]] -> Slideshow
textsToSlideshow = toSlideshow . map (\t -> (map toAttrLine t, []))

keysOKX :: Y -> X -> X -> [K.KM] -> OKX
keysOKX ystart xstart xBound keys =
  let wrapB s = "[" <> s <> "]"
      f ((y, x), (kL, kV, kX)) key =
        let ks = wrapB $ K.showKM key
        in if x + T.length ks > xBound
           then f ((y + 1, 0), ([], kL : kV, kX)) key
           else ( (y, x + T.length ks + 1)
                , (ks : kL, kV, (Left key, (y, x, x + T.length ks)) : kX) )
      (kL1, kV1, kX1) = snd $ foldl' f ((ystart, xstart), ([], [], [])) keys
      catL = toAttrLine . T.intercalate " " . reverse
  in (reverse $ map catL $ kL1 : kV1, reverse kX1)

splitOverlay :: X -> Y -> Report -> [K.KM] -> OKX -> Slideshow
splitOverlay lxsize yspace report keys (ls0, kxs0) =
  assert (length ls0 == length kxs0 && yspace > 2) $
  let rrep = renderReport report
      msgRaw = splitAttrLine lxsize rrep
      (lX0, keysX0) = keysOKX 0 0 maxBound keys
      (lX, keysX) = keysOKX (length msgRaw - 1) (length (last msgRaw) + 1)
                            lxsize keys
      msgOkx = (glueOverlay msgRaw lX, keysX)
      (okxInit, (header, rkxs)) =
        -- Check whether most space taken by report and keys.
        if (length msgRaw + length lX0) * 2 > yspace
        then (msgOkx, ([intercalate (toAttrLine " ") lX0 <+:> rrep], keysX0))
               -- will display "$" (unless has EOLs)
        else (([], []), msgOkx)
      renumber y (km, (_, x1, x2)) = (km, (y, x1, x2))
      splitO (hdr, rk) (ls, kxs) =
        let zipRenumber = zipWith renumber [length hdr..]
            (pre, post) = splitAt (yspace - 1) $ hdr ++ ls
        in if null post
           then [(pre, rk ++ zipRenumber kxs)]  -- all fits on one screen
           else let (preX, postX) = splitAt (yspace - length hdr - 1) kxs
                in (pre, rk ++ zipRenumber preX)
                   : splitO (hdr, rk) (post, postX)
  in toSlideshow $ splitO ([], []) okxInit ++ splitO (header, rkxs) (ls0, kxs0)
