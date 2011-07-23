module Event where

import Control.Monad
import Data.Binary

import Geometry
import Monster

data Event = ActionEvent
              { etime  :: Time,
                eactor :: Actor }
  deriving (Show, Eq, Ord)

instance Binary Event where
  put (ActionEvent t a) =
    do
      put t
      put a
  get =
    liftM2 ActionEvent get get
