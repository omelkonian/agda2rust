# [WIP] A new Agda backend for compiling to Rust

- The backend is defined in `src/Main.hs`.
- The `test/` directory contains an example compilation of `Test.agda` to `Test.rs`.

## TODO

- [x] Switch to treeless syntax
- [ ] identifiers
  + [ ] transcribe to valid Rust identifiers (e.g. remove Unicode somehow)
  + [ ] ensure qualification/scopes are OK
- [ ] primitives
  + [x] Nat->i32
- [x] functions
  + [x] function type signatures
  + [x] termrs (e.g. function bodys)
  + [x] Let bindings
  + [x] Case expresssions
- [ ] datatypes
  + [x] polymorphic types / type variables
  + [x] constructors/variants
  + [x] PhantomData for unused type variables
  + [] Infinite types (e.g. List)
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
