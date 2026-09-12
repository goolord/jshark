# Contributing to JShark

Thanks for contributing. Start here to build, test, and prepare a change.
For a project overview, see the [README](README.md).

## Prerequisites

- GHC 9.14+ and Cabal 3.12+
- [Bun](https://bun.sh) on `PATH` (JS execution tests and `jshark-bindgen`)
- LLVM 20 (`opt-20`, `llc-20`) on non-Windows platforms
- [Fourmolu](https://github.com/fourmolu/fourmolu) for formatting

Run `nix develop` for the pinned toolchain, or follow the
[manual setup](README.md#building-and-development).

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

Local builds treat warnings as errors through each package's `werror` flag,
enabled in `cabal.project`. Keep changes warning-free. Released packages
leave the flag off to accommodate future GHC warnings.

## Commits

Use [Conventional Commits](https://www.conventionalcommits.org/) prefixes that
match the existing history (`feat:`, `fix:`, `refactor:`, `docs:`, `test:`,
`chore:`), with an optional scope (`compiler`, `bindgen`, `lucid`, `hotreload`).
Keep commits focused. Codegen refactors must preserve emitted JavaScript
byte-for-byte; the golden tests under `packages/jshark/test/` enforce this.

## Architecture

Read the module header of [`JShark`](packages/jshark/src/JShark.hs) for the
compiler pipeline, then [`JShark.Api`](packages/jshark/src/JShark/Api.hs) for
the public syntax. See the [package overview](README.md#monorepo-packages)
for the rest of the repository.

## Pull requests

- Add or update tests for behavioral changes.
- Update [CHANGELOG.md](CHANGELOG.md) under `## Unreleased`.
- Make sure `cabal test all`, `fourmolu --mode check`, and `cabal check` (in each
  released package directory) pass.
