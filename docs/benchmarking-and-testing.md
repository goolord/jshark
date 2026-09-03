# Benchmarking, profiling, and testing

Commands assume the repo root and Cabal v2 (`cabal build`, `cabal test`,
`cabal bench`). The repo is a cabal project of five packages:

- `packages/jshark` — core EDSL + compiler (suite `jshark-test`, bench `jshark-compiler`)
- `packages/jshark-lucid` — Lucid DOM integration (suite `jshark-lucid-test`, bench `jshark-lucid-bench`)
- `packages/jshark-bindgen` — TypeScript/JS FFI generator (suite `jshark-bindgen-test`)
- `packages/jshark-hotreload` — hot-reload hub/WAI/watcher (suite `jshark-hotreload-test`)
- `examples` — the five showcase apps, dev server, and compiler (suite `jshark-examples-test`, bench `jshark-examples-bench`, plus `jshark-life-*` profiling executables)

## Prerequisites

| Tool | Required for |
|------|----------------|
| GHC 9.14+ / Cabal 3.x | build, test, bench |
| [Bun](https://bun.sh) on `PATH` | `jshark-examples-test` JS engine probes (`BunTests`, `LifeTests`, `ExampleTests`) |
| `esbuild` / `terser` (optional) | compiler minifier tests (skipped if missing) |

## Testing

Run every suite:

```bash
cabal test all --test-show-details=direct
```

or one suite:

```bash
cabal build jshark-test
cabal test jshark-test --test-show-details=direct
```

The suites are **threaded** with a conservative RTS:

- `-N1` — single capability (avoids parallel compile/metadata contention on large examples)
- `-M10G` heap cap by default (`jshark-test` / `jshark-examples-test`)

Override RTS when debugging:

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
| `examples` | `jshark-examples-test` | `LifeTests`, `CatalogTests`, `LifeWorkerTests`, `StaticCssTests`, `ExampleTests` (parse-every-example via bun), `BunTests`, `PerfTests`, `Hvm2Tests` |

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

Benchmarks use **tasty-bench**. They are for **manual** compiler investigation; not all are CI-gated.

### Targets

| Cabal target | Package | Purpose |
|--------------|---------|---------|
| `jshark-compiler` | `jshark` | Synthetic compiler microbenchmarks (`packages/jshark/bench/Main.hs`, `Stages.hs`). Best for attributing **which compiler stage** is slow on synthetic trees. |
| `jshark-examples-bench` | `examples` | Full example ASTs (`examples/bench/Main.hs`: Breakout, TodoMvc, Synth, Life). Life `emit` is very slow. |
| `jshark-forced` | `examples` | `NFData` / forcing costs on Life |
| `jshark-life-*` | `examples` | `jshark-life-phases`, `jshark-life-metrics`, `jshark-life-flatopt`, `jshark-life-iropt`, `jshark-life-lower`, `jshark-life-emit`, `jshark-life-full-emit` — Life-only stage profiling executables |
| `jshark-lucid-bench` | `jshark-lucid` | Lucid → DOM compile path |

Default bench RTS: `-N` (multicore), `-M10G`, `-O2`.

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

A `TIMEOUT` after 120s still means “this path is too slow for the budget”; use profiling to see where CPU went before the cap.

### Stage names

Each effectful microprogram gets a `stages/<name>/` group (see `Stages.hs` in
each bench dir):

| Bench | Meaning |
|-------|---------|
| `optimizeEffect` | `optimizeEffect` only |
| `optNodes+emit/bytes` | node count + full emit length |
| `effectfulAST` | `renderJS . effectfulAST` byte length |
| `renderJSCompact` / `emit` | compact render |
| `emit/bytes` | `T.length . renderJSCompact . effectfulAST` (full compile path used by `compileEffect` before pretty/minify) |
| `effectfulProgram` | unoptimized `effectfulProgram` (slow; avoid for routine runs) |
| `prettyJS/e2e` | emit + pretty printer |
| `compileEffect/readable/e2e` | full `compileEffect readableConfig` in IO |

Pure microprograms use the same names with `optimize`, `pureAST`, `compilePure`, etc.

Typical attribution on Life-shaped trees:

- **`optimizeEffect` fast, `effectfulAST` / `emit/bytes` timeout** → bottleneck is **codegen** (and metadata/bind walks), not the optimizer.
- **Both slow** → optimizer or shared metadata work; profile both stages separately.

Life-only stage attribution is easiest with the profiling executables:

```bash
cabal run jshark-life-metrics    # raw/opt node counts + allocations
cabal run jshark-life-phases     # wall clock per compiler phase
cabal run jshark-life-full-emit  # end-to-end emit timing
```

---

## Profiling

Use GHC time/allocation profiling to find hot functions. **Do not** let hung benches run unbounded; use `-t` and/or a single `-p` filter.

Write `.prof`, bench stdout, and stderr under **`profile/`** (gitignored). From repo root:

```bash
mkdir -p profile
cd profile
```

### 1. Build with profiling

```bash
cabal build jshark-test --enable-profiling --ghc-options="-fprof-auto-top -fprof-late"
# or
cabal build jshark-compiler --enable-profiling --ghc-options="-fprof-auto-top -fprof-late"
```

On Windows, kill a stuck `jshark-compiler.exe` / `jshark-test.exe` before relinking if the linker reports `Permission denied`.

### 2. Run one slow case with `+RTS -p`

**Tests:**

```bash
EXE=$(cabal list-bin jshark-examples-test)
"$EXE" -p 'life' -t 120s +RTS -p -N1 -M4G -RTS 2>&1 | tee profile/life.log
# → profile/jshark-examples-test.prof when cwd is profile/
```

**Benchmarks:**

```bash
EXE=$(cabal list-bin jshark-compiler)
cd profile
"$EXE" -t 120s -p 'deepUseChain' +RTS -p -N1 -M4G -RTS 2>&1 | tee lifeStep-emit-bytes.log
# → profile/jshark-compiler.prof
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
- Rebuild after `packages/jshark/src/JShark.hs` changes before trusting an old `.prof`.

---

## For Cursor agents: timeout loop on slow compile

Use this when a bench or test **hangs or exceeds budget**. Goal: attribute **optimize vs emit vs full compile**, capture a `.prof` before the cap, iterate without unbounded runs.

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

4. **Interpret**: `optimizeEffect` OK + `emit/bytes` TIMEOUT → codegen/metadata/bind path; both slow → optimizer + shared walks.
5. **Fix, rebuild, rerun only the failing `-p`**; compare `total alloc` and top cost centres in the new `profile/*.prof`.
6. **Do not commit** `profile/` contents; commit code/docs only.

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
| `packages/jshark/bench/Main.hs`, `Stages.hs` | synthetic `jshark-compiler` bench |
| `examples/test/Main.hs` | example/Life test tree |
| `examples/test/ExampleTests.hs` | Bun parse tests for every example |
| `examples/test/LifeTests.hs`, `BunTests.hs` | runtime JS checks |
| `examples/bench/` | full-example bench + `jshark-life-*` profiling executables |
| `cabal.project` | project-wide warning flags, tests/benchmarks on |
| `profile/` | gitignored `.prof` / bench logs from manual runs |
| `.cursor/rules/` | `cabal test`, Fourmolu, architecture notes |
