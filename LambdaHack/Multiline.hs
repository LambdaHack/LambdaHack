module Multiline (multiline) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TQ

-- | Handle multiline verbatim string expressions.
multiline :: TQ.QuasiQuoter
multiline  = TQ.QuasiQuoter (\x -> (TH.litE . TH.stringL) x)
               undefined undefined undefined
