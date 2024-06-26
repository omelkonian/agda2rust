# [WIP] A new Agda backend for compiling to Rust

- The backend is defined in `src/Main.hs`.
- A custom pipeline for translating internal syntax to the treeless representation is in `src/AgdaInternals.hs`.
- The `test/` directory contains a golden-testing suite (c.f. `test/AllTests.agda`).

## How to run

Assuming a working Haskell installation with `ghc` and `cabal` (e.g. using `ghcup`):
- `$ make build` builds the project
- `$ make install` installs the `agda2rust` executable
- `$ make test` runs the test suite
- `$ make repl` starts an interactive session with the agda2rust library loaded
- `$ make html` renders the Github website where you can navigate through the Agda test cases
alongside the corresponding generated Rust code

## TODO

- [x] Switch to treeless syntax
- [ ] identifiers
  + [x] transcribe to valid Rust identifiers (e.g. remove Unicode somehow)
  + [ ] ensure qualification/scopes are OK
- [ ] primitive/builtin types
  + [x] Nat
    * [ ] avoid overflow
  + [x] Char
  + [x] String
  + [x] Bool
  + [x] Int
  + [ ] primitive functions
- [x] functions
  + [x] function type signatures
  + [x] termrs (e.g. function bodys)
  + [x] Let bindings
  + [x] Case expresssions
  + [x] higher-order functions
- [ ] type aliases
  + [x] simple aliases without arguments
  + [x] parameterized aliases
- [ ] datatypes
  + [x] polymorphic types / type variables
  + [x] constructors/variants
  + [x] PhantomData for unused type variables
    * [x] add Phantom constructor *only if needed*
  + [x] Infinite types (e.g. List)
  + [ ] more dependent/indexed types
- [ ] records
  + [x] simple records
  + [ ] complex records
- [ ] imports (module-related)
  + [ ] private definitions (i.e. not `pub` for everything)
- [x] Postulates
- [x] FOREIGN pragmas
  + [x] FFI with postulates
- [x] Erasure (a.k.a. run-time irrelevance)
- [ ] Compile-time irrelevance
  + [ ] Prop universe
- [x] test suite
  + [x] golden files
  + [x] CI
- [ ] Compiler instructions
  + [x] COMPILE ignore
  + [x] COMPILE const/static
  + [ ] COMPILE rename
  + [ ] COMPILE derive
  + [ ] Inlining
- [ ] interface
  + [ ] emacs mode?
  + [ ] TUI?
- [ ] Optional features
  + [ ] IO
  + [ ] Procedural code
