{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Slideshows.
module Game.LambdaHack.Client.UI.Slideshow
  ( KYX, OKX, Slideshow(slideshow)
  , emptySlideshow, unsnoc, toSlideshow, menuToSlideshow
  , splitOverlay, splitOKX
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Client.ItemSlot
import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Msg
import Game.LambdaHack.Client.UI.Overlay
import qualified Game.LambdaHack.Common.Color as Color
import Game.LambdaHack.Common.Point

type KYX = (Either [K.KM] SlotChar, (Y, X, X))

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
    [( als ++ [stringToAL endMsg]
     , [(Left [K.safeSpaceKM], (length als, 0, 8))] )]
  addFooters [(als, kxs)] = [(als, kxs)]
  addFooters ((als, kxs) : rest) =
    ( als ++ [stringToAL moreMsg]
    , kxs ++ [(Left [K.safeSpaceKM], (length als, 0, 8))] )
    : addFooters rest

moreMsg :: String
moreMsg = "--more--  "

endMsg :: String
endMsg = "--end--  "

menuToSlideshow :: OKX -> Slideshow
menuToSlideshow (als, kxs) =
  assert (not (null als || null kxs)) $ Slideshow [(als, kxs)]

keysOKX :: Y -> X -> X -> [K.KM] -> OKX
keysOKX ystart xstart xBound keys =
  let wrapB s = "[" ++ s ++ "]"
      f ((y, x), (kL, kV, kX)) key =
        let ks = wrapB $ K.showKM key
            len = length ks
        in if x + len > xBound
           then f ((y + 1, 0), ([], kL : kV, kX)) key
           else ( (y, x + len + 1)
                , (ks : kL, kV, (Left [key], (y, x, x + len)) : kX) )
      (kL1, kV1, kX1) = snd $ foldl' f ((ystart, xstart), ([], [], [])) keys
      catL = stringToAL . intercalate " " . reverse
  in (reverse $ map catL $ kL1 : kV1, reverse kX1)

splitOverlay :: X -> Y -> Report -> [K.KM] -> OKX -> Slideshow
splitOverlay lxsize yspace report keys (ls0, kxs0) =
  toSlideshow $ splitOKX lxsize yspace (renderReport report) keys (ls0, kxs0)

splitOKX :: X -> Y -> AttrLine -> [K.KM] -> OKX -> [OKX]
splitOKX lxsize yspace rrep keys (ls0, kxs0) =
  assert (yspace > 2) $  -- and kxs0 is sorted
  let msgRaw = splitAttrLine lxsize rrep
      (lX0, keysX0) = keysOKX 0 0 maxBound keys
      (lX, keysX) | null msgRaw = (lX0, keysX0)
                  | otherwise = keysOKX (length msgRaw - 1)
                                        (length (last msgRaw) + 1)
                                        lxsize keys
      msgOkx = (glueOverlay msgRaw lX, keysX)
      ((lsInit, kxsInit), (header, rkxs)) =
        -- Check whether most space taken by report and keys.
        if (length $ glueOverlay msgRaw lX0) * 2 > yspace
        then (msgOkx, ([intercalate  [Color.spaceAttr] lX0 <+:> rrep], keysX0))
               -- will display "$" (unless has EOLs)
        else (([], []), msgOkx)
      renumber y (km, (y0, x1, x2)) = (km, (y0 + y, x1, x2))
      splitO yoffset (hdr, rk) (ls, kxs) =
        let zipRenumber = map $ renumber $ length hdr - yoffset
            (pre, post) = splitAt (yspace - 1) $ hdr ++ ls
            yoffsetNew = yoffset + yspace - length hdr - 1
        in if null post
           then [(pre, rk ++ zipRenumber kxs)]  -- all fits on one screen
           else let (preX, postX) =
                      break (\(_, (y1, _, _)) -> y1 >= yoffsetNew) kxs
                in (pre, rk ++ zipRenumber preX)
                   : splitO yoffsetNew (hdr, rk) (post, postX)
      initSlides = if null lsInit
                   then assert (null kxsInit) []
                   else splitO 0 ([], []) (lsInit, kxsInit)
      mainSlides = if null ls0 && not (null lsInit)
                   then assert (null kxs0) []
                   else splitO 0 (header, rkxs) (ls0, kxs0)
  in initSlides ++ mainSlides
