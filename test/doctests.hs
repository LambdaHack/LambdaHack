import Prelude
import Test.DocTest

main :: IO ()
main = doctest ["-idefinition-src", "-iengine-src", "engine-src/Game/LambdaHack/Common/Point.hs", "-XMonoLocalBinds", "-XScopedTypeVariables", "-XOverloadedStrings", "-XBangPatterns", "-XRecordWildCards", "-XNamedFieldPuns", "-XMultiWayIf", "-XLambdaCase", "-XDefaultSignatures", "-XInstanceSigs", "-XPatternSynonyms", "-XStrictData", "-XCPP", "-XTypeApplications" ]
  -- This needs to match @default-extensions@ of @common options@
  -- of LambdaHack.cabal. So far, there's no way to automate that.
