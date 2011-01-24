module Event where

import Geometry
import Monster

data Event = ActionEvent
              { etime  :: Time,
                eactor :: Actor }
  deriving (Show, Eq, Ord)

