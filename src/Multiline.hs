module Multiline (multiline) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TQ

-- | Used to handle multiline verbatim string expressions, see ConfigDefault.hs.
multiline :: TQ.QuasiQuoter
multiline  = TQ.QuasiQuoter (TH.litE . TH.stringL) undefined undefined undefined
