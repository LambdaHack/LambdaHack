{-# OPTIONS_GHC -F -pgmF doctest-driver-gen -optF --verbose -optF definition-src -optF engine-src -optF -XMonoLocalBinds -optF -XScopedTypeVariables -optF -XOverloadedStrings -optF -XBangPatterns -optF -XRecordWildCards -optF -XNamedFieldPuns -optF -XMultiWayIf -optF -XLambdaCase -optF -XDefaultSignatures -optF -XInstanceSigs -optF -XPatternSynonyms -optF -XStrictData -optF -XCPP -optF -XTypeApplications #-}
  -- This needs to match @default-extensions@ of @common options@
  -- of LambdaHack.cabal. So far, there's no way to automate that.
  -- This is slow, but @--fast@ doesn't help at all, so not enabled.
  -- No slowdown from @--verbose@.
