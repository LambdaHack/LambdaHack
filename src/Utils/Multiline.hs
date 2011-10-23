module Utils.Multiline (multiline) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TQ

-- | Handle multiline verbatim string expressions.
multiline :: TQ.QuasiQuoter
multiline  = TQ.QuasiQuoter (TH.litE . TH.stringL) undefined undefined undefined
