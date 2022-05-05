# HaskellFMU
The Haskell FMU is a library that enables FMU development in Haskell.
This is the raw version and forwards most requests directly.

# Development Environment
This requires GHC 8.10.7 due to the following error:
```
Undefined symbols for architecture x86_64:
  "___darwin_check_fd_set_overflow", referenced from:
      _awaitEvent in libHSrts.a(Select.o)
ld: symbol(s) not found for architecture x86_64
clang: error: linker command failed with exit code 1 (use -v to see invocation)
`clang' failed in phase `Linker'. (Exit code: 1)
cabal: Failed to build exe:hsc2hs from hsc2hs-0.68.7 (which is required by
HaskellFMU-0.1.0.0). See the build log above for details.
```

This is considered in this issue: https://gitlab.haskell.org/ghc/ghc/-/issues/19950
