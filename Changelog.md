### 1.6.1

- Use custom *Setup.hs* to configure Haddock options on build of documentation.

### 1.6

- Refactor module *NgxExport.Log.Gen*.
- Add CPP macro *NGX_CSTUB* in module *NgxExport.Log.CLog* to work around the
  lack of macro *\_\_HADDOCK_VERSION\_\_* in recent Haddock versions. Use this
  macro in *simple/cabal.project*: this must fix building the simple module with
  documentation for dependencies.
- **Note**: due to a [bug](https://gitlab.haskell.org/ghc/ghc/-/issues/26136)
  introduced in GHC major versions *9.12*, *9.10*, and *9.8*, the code won't
  compile with GHC *9.12.2*, *9.12.1* (fixed in *9.12.4*), *9.10.2* (fixed in
  *9.10.3*), *9.8.4* (not fixed, compile with *9.8.2*).

### 1.5.2

- Foreign calls were extracted in a separate module.

### 1.5.1

- Fixed dynamic linker errors when building haddocks.

### 1.5

- Auto-generate haddocks for generated functions.

