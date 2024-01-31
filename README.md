# [WIP] A new Agda backend for compiling to Rust

- The backend is defined in `src/Main.hs`.
- A custom pipeline for translating internal syntax to the treeless representation is in `src/AgdaInternals.hs`.
- The `test/` directory contains a golden-testing suite (c.f. `test/AllTests.agda`).

## TODO

- [x] Switch to treeless syntax
- [ ] identifiers
  + [ ] transcribe to valid Rust identifiers (e.g. remove Unicode somehow)
  + [ ] ensure qualification/scopes are OK
- [ ] primitives
  + [x] Nat->i32
  + [ ] ...
  + [ ] buitin modules
- [x] functions
  + [x] function type signatures
  + [x] termrs (e.g. function bodys)
  + [x] Let bindings
  + [x] Case expresssions
  + [x] higher-order functions
- [ ] datatypes
  + [x] polymorphic types / type variables
  + [x] constructors/variants
  + [x] PhantomData for unused type variables
    * [ ] add Phantom constructor *only if needed*
  + [x] Infinite types (e.g. List)
  + [ ] more dependent/indexed types
- [ ] records
- [ ] imports
  + [ ] private definitions (i.e. not `pub` for everything)
- [x] FOREIGN pragmas
- [x] test suite
  + [x] golden files
  + [x] CI
- [ ] interface
  + [ ] emacs mode?
  + [ ] TUI?
