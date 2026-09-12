# Benchmarking, profiling, and testing

Run these commands from the repository root unless noted. The Cabal project
contains five packages:

- `packages/jshark` — core EDSL + compiler (suite `jshark-test`, bench `jshark-compiler`)
- `packages/jshark-lucid` — Lucid DOM integration (suite `jshark-lucid-test`, bench `jshark-lucid-bench`)
- `packages/jshark-bindgen` — TypeScript/JS FFI generator (suite `jshark-bindgen-test`)
- `packages/jshark-hotreload` — hot-reload hub/WAI/watcher (suite `jshark-hotreload-test`)
- `examples` — the four showcase apps, dev server, and compiler (suite `jshark-examples-test`, bench `jshark-examples-bench`)

Shared test/bench support is the `jshark:testing` sublibrary
(`packages/jshark/test-support/`: `Test.Support`, `CaptureStderr`,
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
| `jshark` | `jshark-test` | `-p codegen`, `-p optimize`, `-p compiler`, `-p flat soa` |
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

Benchmarks use tasty-bench for compiler investigation. CI checks selected
performance budgets; run these benchmarks locally for detailed timings.

### Targets

| Cabal target | Package | Purpose |
|--------------|---------|---------|
| `jshark-compiler` | `jshark` | Synthetic compiler microbenchmarks (`packages/jshark/bench/Main.hs`, `Bench.Stages` in `jshark:testing`). Best for attributing **which compiler stage** is slow on synthetic trees. |
| `jshark-examples-bench` | `examples` | Full example ASTs (`examples/bench/Main.hs`: Breakout, TodoMvc, Synth, Life). Life `emit` is very slow. |
| `jshark-forced` | `examples` | `NFData` / forcing costs on Life |
| `jshark-life-*` | `examples` | `jshark-life-phases`, `jshark-life-emit`, `jshark-life-full-emit`, `jshark-probe` — Life-only stage profiling executables |
| `jshark-lucid-bench` | `jshark-lucid` | Lucid → DOM compile path |

Default bench RTS: `-N` (multicore), `-M10G`; optimization comes from
`optimization: 2` in `cabal.project`.

### Run benchmarks

```bash
cabal bench jshark-compiler
cabal bench jshark-compiler -- jshark-compiler --list-tests
```

List tests, then filter (Tasty patterns use regex; slashes must be escaped):

```bash
cabal list-bin jshark-compiler | xargs -I{} {} -l | grep codepath
cabal bench jshark-compiler -- jshark-compiler -p 'deepUseChain' -t 120s
cabal bench jshark-examples-bench -- jshark-examples-bench -p 'life'
```

### Per-benchmark timeout

Large paths (`life emit`, `effectfulAST`) can run for minutes under tasty-bench calibration. **Cap wall time** when diagnosing hangs:

```bash
cabal bench jshark-examples-bench -- jshark-examples-bench -t 120s -p 'life.emit'
```

A timeout identifies a path that exceeds the budget. Profile it to locate
the expensive work.

### Stage names

Each effectful microprogram gets a `stages/<name>/` group, defined by
`Bench.Stages` in the `jshark:testing` sublibrary:

| Bench | Meaning |
|-------|---------|
| `optimizeEffect` | `optimizeEffect` only |
| `optNodes+emit/bytes` | node count + full emit length |
| `effectfulAST` | `renderJS . effectfulAST` byte length |
| `renderJS` / `emit` | compact render |
| `emit/bytes` | `BS.length . renderJS . effectfulAST` (full compile path used by `compileEffect` before pretty) |
| `effectfulProgram` | unoptimized `effectfulProgram` (slow; avoid for routine runs) |
| `prettyJS/e2e` | emit + pretty printer |
| `compileEffect/readable/e2e` | full `compileEffect readableConfig` in IO |

Pure microprograms use the same names with `optimize`, `pureAST`, `compilePure`, etc.

Typical attribution on Life-shaped trees:

- **`optimizeEffect` fast, `effectfulAST` / `emit/bytes` timeout** → bottleneck is **codegen** (and metadata/bind walks), not the optimizer.
- **Both slow** → optimizer or shared metadata work; profile both stages separately.

Life-only stage attribution is easiest with the profiling executables:

```bash
cabal run jshark-life-phases     # wall clock per compiler phase
cabal run jshark-life-full-emit  # end-to-end emit timing
```

---

## Profiling

Use GHC time and allocation profiling to find expensive functions. Select
one case with `-p` and set a timeout with `-t`.

Keep reports and logs in the gitignored `profile/` directory. From the
repository root:

```bash
mkdir -p profile
```

### 1. Build with profiling

```bash
cabal build jshark-examples-test --enable-profiling --ghc-options="-fprof-auto-top -fprof-late"
# or
cabal build jshark-compiler --enable-profiling --ghc-options="-fprof-auto-top -fprof-late"
```

On Windows, kill a stuck `jshark-compiler.exe` / `jshark-test.exe` before relinking if the linker reports `Permission denied`.

### 2. Run one slow case with `+RTS -p`

**Tests:**

```bash
EXE=$(cabal list-bin jshark-examples-test)
(cd profile && "$EXE" -p 'life' -t 120s +RTS -p -N1 -M4G -RTS 2>&1 | tee life.log)
# Report: profile/jshark-examples-test.prof
```

**Benchmarks:**

```bash
EXE=$(cabal list-bin jshark-compiler)
(cd profile && "$EXE" -t 120s -p 'deepUseChain' +RTS -p -N1 -M4G -RTS 2>&1 | tee deepUseChain.log)
# Report: profile/jshark-compiler.prof
```

Put **tasty options before RTS flags** when using the cabal wrapper:

```bash
cabal bench jshark-compiler -- jshark-compiler -t 120s -p 'deepUseChain' +RTS -p -N1 -M4G -RTS 2>&1 | tee profile/bench-emit-bytes.log
```

### 3. Read the report

Open `profile/*.prof`. Check:

- **total time** vs wall time (low CPU + long wall → allocation/GC or blocking)
- **total alloc** (hundreds of GB on a small kernel → repeated tree walks / `IntMap` churn in metadata or bind codegen)
- Top cost centres: `unionWithKey`, `optBind`, `renameEff`, `effectfulAST'`, `bindEffectCode`, `countEffect`, `mergeMetadata`

### 4. List benchmark names

```bash
cabal list-bin jshark-compiler | xargs -I{} {} -l
```

Names look like `All.codepaths.effect.deepUseChain.emit/bytes`; filter with `-p 'deepUseChain'` or `-p '/emit\/bytes/'`.

### Profiling tips

- Prefer **one** `-p` case per run so the `.prof` file matches the hypothesis.
- Force-killing the process on Windows may **omit** `.prof` output; prefer `-t` timeout so the process exits normally.
- Match RTS caps to the test suite (`-N1`) when comparing to `cabal test` behavior.
- Rebuild after compiler source changes before trusting an old `.prof`.

---

## Investigating a slow compile

When a test or benchmark exceeds its budget, compare optimization,
emission, and full compilation. Use bounded runs and keep a profile for
each stage.

1. **Kill locked exes** (Windows): `Get-Process jshark-compiler,jshark-test -EA SilentlyContinue | Stop-Process -Force`
2. **Build profiled bench**: `cabal build jshark-compiler --enable-profiling --ghc-options="-fprof-auto-top -fprof-late"`
3. **Stage sweep with timeout** (120s per case; adjust as needed):

```bash
mkdir -p profile
EXE=$(cabal list-bin jshark-compiler)
for PAT in 'deepUseChain.optimizeEffect' 'deepUseChain.effectfulAST' 'deepUseChain.emit' '/emit\/bytes/'; do
  LOG=profile/bench-${PAT//\//-}.log
  echo "=== $PAT ===" | tee "$LOG"
  "$EXE" -t 120s -p "$PAT" +RTS -p -N1 -M4G -RTS 2>&1 | tee -a "$LOG" || true
  mv -f jshark-compiler.prof "profile/jshark-compiler-${PAT//\//-}.prof" 2>/dev/null || true
done
```

For Life-shaped paths, swap the executable for `jshark-examples-bench`
(`cabal list-bin jshark-examples-bench`) and filter `-p 'life.…'`.

4. If optimization is fast but emission times out, inspect codegen and
   metadata traversal. If both are slow, inspect the optimizer and shared work.
5. Fix, rebuild, and rerun the failing case. Compare total allocation and
   top cost centres in the new report.

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
lands. Compiler stage attribution (`optimizeEffect` vs `effectfulAST` vs
`emit`) comes from `jshark-compiler`; full Life stages from
`jshark-examples-bench` / `jshark-life-phases`. Keep isolated stage timings
separate from end-to-end numbers.

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

# Compiler stage attribution (synthetic)
cabal bench jshark-compiler -- jshark-compiler -t 120s -p 'deepUseChain'

# Life stage attribution (full example)
cabal bench jshark-examples-bench -- jshark-examples-bench -t 120s -p 'life.optimize'
```

## Related files

| Path | Role |
|------|------|
| `packages/jshark/test/Main.hs` | core test tree |
| `packages/jshark/bench/Main.hs`, `Bench.Stages` (in `jshark:testing`) | synthetic `jshark-compiler` bench |
| `examples/test/Main.hs` | example/Life test tree |
| `examples/test/ExampleTests.hs` | Bun parse tests for every example |
| `examples/test/LifeTests.hs`, `BunTests.hs` | runtime JS checks |
| `examples/bench/` | full-example bench + `jshark-life-*` profiling executables |
| `cabal.project` | project config: tests/benchmarks on, `werror` flag enabled |
| `profile/` | gitignored `.prof` / bench logs from manual runs |
| `.cursor/rules/` | `cabal test`, Fourmolu, architecture notes |
