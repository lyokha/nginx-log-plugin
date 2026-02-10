### 1.6

- Refactor module *NgxExport.Log.Gen*.
- Add CPP macro *NGX_CSTUB* in module *NgxExport.Log.CLog* to work around the
  lack of macro *\_\_HADDOCK_VERSION\_\_* in recent Haddock versions. Use this
  macro in *simple/Makefile*: this must fix building the simple module with
  documentation for dependencies.

### 1.5.2

- Foreign calls were extracted in a separate module.

### 1.5.1

- Fixed dynamic linker errors when building haddocks.

### 1.5

- Auto-generate haddocks for generated functions.

