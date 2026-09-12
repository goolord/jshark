# Contributing to JShark

Thanks for helping improve JShark. This guide covers the local workflow; the
[README](README.md) explains what the project is.

## Prerequisites

- GHC 9.14+ and Cabal 3.12+
- [Bun](https://bun.sh) on `PATH` (JS execution tests and `jshark-bindgen`)
- LLVM 20 (`opt-20`, `llc-20`) on non-Windows platforms
- [Fourmolu](https://github.com/fourmolu/fourmolu) for formatting

The easiest way to get all of these is `nix develop`, which pins the whole
toolchain. See the README for a manual setup.

## Build, test, and benchmark

```bash
cabal build all
cabal test all --test-show-details=direct
cabal bench jshark-compiler -- --quick
```

See [docs/benchmarking-and-testing.md](docs/benchmarking-and-testing.md) for test
filters, RTS settings, and profiling.

## Formatting

Format before committing:

```bash
fourmolu --mode inplace $(git ls-files '*.hs')
fourmolu --mode check $(git ls-files '*.hs')
```

## Warnings

Each package declares a `werror` flag. `cabal.project` enables it for local
builds, so warnings are errors in development; released packages default it
off so future GHC warning sets do not break downstream builds. Keep the tree
warning-clean.

## Commits

Use [Conventional Commits](https://www.conventionalcommits.org/) prefixes that
match the existing history (`feat:`, `fix:`, `refactor:`, `docs:`, `test:`,
`chore:`), with an optional scope (`compiler`, `bindgen`, `lucid`, `hotreload`).
Keep commits focused. Codegen refactors must preserve emitted JavaScript
byte-for-byte; the golden tests under `packages/jshark/test/` enforce this.

## Architecture

Start with the module header of `JShark` (`packages/jshark/src/JShark.hs`) for
the compile pipeline, then `JShark.Api` for the surface syntax. Satellite
packages are described in the [README](README.md#monorepo-packages).

## Pull requests

- Add or update tests for behavioral changes.
- Update [CHANGELOG.md](CHANGELOG.md) under `## Unreleased`.
- Make sure `cabal test all`, `fourmolu --mode check`, and `cabal check` (in each
  released package directory) pass.
