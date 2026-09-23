# Benchmarking, profiling, and testing

Run these commands from the repository root unless noted. The Cabal project
contains five packages:

- `packages/jshark` — core AST, EDSL surface, and compiler
- `packages/jshark-base` — platform bindings and tooling; hosts the integration suite `jshark-test` and bench `jshark-compiler`
- `packages/jshark-lucid` — Lucid DOM integration (suite `jshark-lucid-test`, bench `jshark-lucid-bench`)
- `packages/jshark-bindgen` — TypeScript/JS FFI generator (suite `jshark-bindgen-test`)
- `packages/jshark-hotreload` — hot-reload hub/WAI/watcher (suite `jshark-hotreload-test`)
- `examples` — the four showcase apps, dev server, and compiler (suite `jshark-examples-test`, bench `jshark-examples-bench`)

Shared test/bench support is the `jshark-base:testing` sublibrary
(`packages/jshark-base/test-support/`: `Test.Support`, `CaptureStderr`,
`Bench.Stages`); it is used by the core suite and by the examples package.

## Prerequisites

| Tool | Required for |
|------|----------------|
| GHC 9.14+ / Cabal 3.12+ | build, test, bench |
| [Bun](https://bun.sh) on `PATH` | `jshark-examples-test` JS engine probes (`BunTests`, `LifeTests`, `ExampleTests`) |

## Testing

Run every suite:

```bash
cabal test all --test-show-details=direct
```

Run one suite:

```bash
cabal build jshark-test
cabal test jshark-test --test-show-details=direct
```

The core and example suites use a threaded runtime with these defaults:

- `-N1` — single capability (avoids parallel compile/metadata contention on large examples)
- `-M10G` heap cap by default (`jshark-test` / `jshark-examples-test`)

Override the runtime settings when debugging:

```bash
cabal test jshark-test --test-options='+RTS -N1 -M4G -RTS' --test-show-details=direct
```

### Filter tests (Tasty)

```bash
# one group in the core suite
cabal test jshark-test --test-options='-p codegen' --test-show-details=direct

# Life group in the examples suite (needs bun)
cabal test jshark-examples-test --test-options='-p life -t 120s' --test-show-details=direct

# per-test wall-clock cap (recommended for slow life paths)
cabal test all --test-options='-t 120s' --test-show-details=direct
```

`-t DURATION` applies **per test case**, not to the whole run.

### What the suites cover

| Package | Suite / tree | Notes |
|---------|--------------|-------|
| `jshark` | `test/Main.hs` | interpreter, codegen goldens, optimizer, compiler |
| `jshark` | `jshark-test` | `-p codegen`, `-p optimize`, `-p compiler`, `-p "codegen folds"` |
| `jshark-lucid` | `jshark-lucid-test` | Lucid → DOM codegen (happy-dom via bun) |
| `jshark-bindgen` | `jshark-bindgen-test` | `.d.ts`/JSDoc parse + emit, CLI, golden `BindgenToy` |
| `jshark-hotreload` | `jshark-hotreload-test` | SSE hub, WAI middleware, watcher mapping |
| `examples` | `jshark-examples-test` | `LifeTests`, `CatalogTests`, `LifeWorkerTests`, `StaticCssTests`, `ExampleTests` (parse-every-example via bun), `BunTests`, `PerfTests` |

`ExampleTests` runs inside `jshark-examples-test` (gated on bun). Life's full
emit is slow; always use `-t 120s` when iterating over `-p life` / `-p examples`.

### Run the test executable directly

Useful for profiling (see below):

```bash
EXE=$(cabal list-bin jshark-test)
"$EXE" -p 'compiler' -t 120s +RTS -N1 -M4G -RTS
```

On Windows, kill a stuck `jshark-test.exe` / `jshark-examples-test.exe` before relinking if the linker reports `Permission denied`.

---

## Benchmarks

Benchmarks use tasty-bench and are for manual investigation only.

| Cabal target | Package | Purpose |
|--------------|---------|---------|
| `jshark-compiler` | `jshark-base` | Synthetic programs at growing sizes (`packages/jshark-base/bench/Main.hs`) |
| `jshark-examples-bench` | `examples` | Each example's whole program (`examples/bench/Main.hs`) |
| `jshark-lucid-bench` | `jshark-lucid` | Lucid → DOM compile path |

Both compiler benches split each program into the stages defined by
`Bench.Stages` in the `jshark-base:testing` sublibrary:

| Bench | Meaning |
|-------|---------|
| `optimize` | lower and optimize; forces the node count |
| `emit` | `renderJS . effectfulAST` (readable codegen, no formatter) |
| `program` | `renderJS . effectfulProgram` (the IIFE) |
| `compile/default` | `compileEffect defaultCompilerConfig` |
| `compile/readable` | `compileEffect readableConfig`, including Biome |

Pure programs use the same names over `pureAST` / `compilePure`.

```bash
cabal bench jshark-compiler -- -p 'bindChain' -t 120s
cabal bench jshark-examples-bench -- -p 'life' -t 120s
```

If `optimize` is fast but `emit` is slow, the bottleneck is codegen;
if both are slow, look at the optimizer.

---

## Profiling

Keep reports and logs in the gitignored `profile/` directory.

```bash
mkdir -p profile
cabal build jshark-examples-test --enable-profiling --ghc-options="-fprof-auto-top -fprof-late"
EXE=$(cabal list-bin jshark-examples-test)
(cd profile && "$EXE" -p 'life' -t 120s +RTS -p -N1 -M4G -RTS 2>&1 | tee life.log)
# Report: profile/jshark-examples-test.prof
```

The same works for `jshark-compiler` and `jshark-examples-bench`; put tasty
options before RTS flags. Prefer one `-p` case per run so the report
matches the hypothesis, and prefer `-t` over killing the process, which can
omit the `.prof` output.

---

## Recorded baselines

Performance budgets live in tests beside the workloads they measure:

| Budget | Where | What it gates |
|--------|-------|---------------|
| Life raw/opt IR nodes, optimize alloc | `examples/test/PerfTests.hs` | optimizer/storage |
| Life output UTF-8 bytes and `$`-helper count | `examples/test/PerfTests.hs` | emission/hoisting |
| Probe 16/32 JS size and alloc | `examples/test/PerfTests.hs` | IR→JS scale |
| Life golden node/byte counts | `examples/test/LifeTests.hs` | inline/optimizer regressions |

Record a new baseline by measuring on GHC 9.14.1 / `-O2`, keeping enough
slack for RTS/GC noise, and lowering the ceiling when an intentional win
lands.

## Release verification

Run the same checks locally as the release workflow before publishing:

```bash
./scripts/check-tag-version.sh v0.1.0.0   # tag matches every package version
./scripts/verify-sdists.sh                # unpack, build, test, install bindgen
```

`verify-sdists.sh` unpacks the four published source distributions into a
fresh directory, writes a minimal `cabal.project` that points only at the
unpacked sources (no root overrides), builds and tests them, installs
`jshark-bindgen`, and runs it on a shipped fixture. It leaves the verified
tarballs in `dist-newstyle/sdist/`; publish only those paths. The clean
project uses GHC's default backend, so it also confirms the packages do not
require the monorepo's LLVM options.

---

## Quick reference

```bash
# Fast sanity (core suite)
cabal test jshark-test --test-options='-p codegen' --test-show-details=direct

# Everything
cabal test all --test-show-details=direct --test-options='+RTS -N1 -M4G -RTS -t120'

# Examples (needs bun)
cabal test jshark-examples-test --test-options='-p examples -t 120s' --test-show-details=direct

# Compiler stage attribution
cabal bench jshark-compiler -- -t 120s -p 'bindChain'
cabal bench jshark-examples-bench -- -t 120s -p 'life'
```

## Related files

| Path | Role |
|------|------|
| `packages/jshark-base/test/Main.hs` | core test tree |
| `packages/jshark-base/bench/Main.hs`, `Bench.Stages` (in `jshark-base:testing`) | synthetic `jshark-compiler` bench |
| `examples/test/Main.hs` | example/Life test tree |
| `examples/test/ExampleTests.hs` | Bun parse tests for every example |
| `examples/test/LifeTests.hs`, `BunTests.hs` | runtime JS checks |
| `examples/bench/` | full-example bench |
| `cabal.project` | project config: tests/benchmarks on, `werror` flag enabled |
| `profile/` | gitignored `.prof` / bench logs from manual runs |
