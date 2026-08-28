# Benchmarking, profiling, and testing

Commands assume repo root and Cabal v2 (`cabal build`, `cabal test`,
`cabal bench`).

## Prerequisites

| Tool | Required for |
|------|----------------|
| GHC 9.14+ / Cabal 3.x | build, test, bench |
| [Bun](https://bun.sh) on `PATH` | `BunTests`, `LifeTests` JS engine probes |
| `esbuild` / `terser` (optional) | compiler minifier tests in `test/Main.hs` (skipped if missing) |

Build the library and test executable first:

```bash
cabal build jshark-test
```

## Testing (`jshark-test`)

### Run the full suite

```bash
cabal test jshark-test --test-show-details=direct
```

The test suite is **threaded** with default RTS:

- `-N1` — single capability (avoids parallel compile/metadata contention on large examples)
- `-M10G` — 10 GB heap cap

Override RTS when debugging:

```bash
cabal test jshark-test --test-options='+RTS -N1 -M4G -RTS' --test-show-details=direct
```

### Filter tests (Tasty)

```bash
# one group
cabal test jshark-test --test-options='-p codegen' --test-show-details=direct

# one example parse case
cabal test jshark-test --test-options='-p todo-mvc' --test-show-details=direct

# per-test wall-clock cap (recommended for slow example / life paths)
cabal test jshark-test --test-options='-t 120s' --test-show-details=direct
```

`-t DURATION` applies **per test case**, not to the whole run.

### What the suite covers

| Area | Module / group | Notes |
|------|----------------|-------|
| Interpreter | `evaluate` in `test/Main.hs` | pure `evaluate` / `evaluateCached` |
| Codegen | `codegen` | golden JS strings from `effectfulAST` / `pureAST` |
| Optimizer | `optimize` | constant folding, bind elimination |
| Compiler | `compiler` | minify, cache, `readableConfig`, `prettyJS` |
| Bun eval | `BunTests` | emitted JS vs interpreter |
| Lucid | `LucidTests` | `jshark-lucid` DOM codegen |
| Life | `LifeTests` | Conway rules, grid steps, WASM/JS engine helpers |

`test/ExampleTests.hs` typechecks with the suite (`other-modules`) but is
not in the default Tasty tree — Life's full emit is too slow for every
`cabal test`. Wire `exampleTests` into `test/Main.hs` to run it.

### Slow tests

`todo-mvc`, `life`, and some `LifeTests` grid cases compile large ASTs. Always use a timeout when iterating:

```bash
cabal test jshark-test --test-options='-p life -t 120s' --test-show-details=direct
```

### Run the test executable directly

Useful for profiling (see below):

```bash
EXE=$(cabal list-bin jshark-test)
"$EXE" -p 'todo-mvc' -t 120s +RTS -N1 -M4G -RTS
```

On Windows, kill a stuck `jshark-test.exe` before relinking if the linker reports `Permission denied`.

---

## Benchmarks

Benchmarks use **tasty-bench**. They are for **manual** compiler investigation; not all are CI-gated.

### Targets

| Cabal target | Purpose |
|--------------|---------|
| `jshark-compiler` | Microprograms + Life-shaped kernels (`bench/Compiler.hs`). Best for attributing **which compiler stage** is slow. |
| `jshark-compiler-examples` | Full example ASTs (`bench/Examples.hs`: Breakout, TodoMvc, Synth, Life). Life `emit` is very slow. |
| `jshark-synthetic` | Synthetic AST matrix |
| `jshark-forced` | `NFData` / forcing costs |
| `jshark-lucid-bench` | Lucid → DOM compile path |

Default bench RTS: `-N` (multicore), `-M10G`, `-O2`.

### Run benchmarks

```bash
cabal bench jshark-compiler
cabal bench jshark-compiler -- jshark-compiler --list-tests
```

List tests, then filter (Tasty patterns use regex; slashes must be escaped):

```bash
cabal list-bin jshark-compiler | xargs -I{} {} -l | grep lifeStep
cabal bench jshark-compiler -- jshark-compiler -p 'lifeStep.emit' -t 120s
cabal bench jshark-compiler -- jshark-compiler -p '/lifeStep.emit\/bytes/' -t 120s
cabal bench jshark-compiler-examples -- jshark-compiler-examples -p 'life'
```

### Per-benchmark timeout

Large paths (`lifeStep`, `lifeMedium`, `emit/bytes`, `effectfulAST`) can run for minutes under tasty-bench calibration. **Cap wall time** when diagnosing hangs:

```bash
cabal bench jshark-compiler -- jshark-compiler -t 120s -p 'stages.lifeStep.emit'
```

A `TIMEOUT` after 120s still means “this path is too slow for the budget”; use profiling to see where CPU went before the cap.

### Stage names (`bench/Stages.hs`)

Each effectful microprogram gets a `stages/<name>/` group:

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
EXE=$(cabal list-bin jshark-test)
"$EXE" -p 'todo-mvc' -t 120s +RTS -p -N1 -M4G -RTS 2>&1 | tee profile/todo-mvc.log
# → profile/jshark-test.prof when cwd is profile/
```

**Benchmarks:**

```bash
EXE=$(cabal list-bin jshark-compiler)
cd profile
"$EXE" -t 120s -p '/lifeStep.emit\/bytes/' +RTS -p -N1 -M4G -RTS 2>&1 | tee lifeStep-emit-bytes.log
# → profile/jshark-compiler.prof
```

Put **tasty options before RTS flags** when using the cabal wrapper:

```bash
cabal bench jshark-compiler -- jshark-compiler -t 120s -p '/lifeStep.emit\/bytes/' +RTS -p -N1 -M4G -RTS 2>&1 | tee profile/bench-emit-bytes.log
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

Names look like `All.stages.lifeStep.emit/bytes`; filter with `-p 'lifeStep.emit'` or `-p '/lifeStep.emit\/bytes/'`.

### Profiling tips

- Prefer **one** `-p` case per run so the `.prof` file matches the hypothesis.
- Force-killing the process on Windows may **omit** `.prof` output; prefer `-t` timeout so the process exits normally.
- Match RTS caps to the test suite (`-N1`) when comparing to `cabal test` behavior.
- Rebuild after `src/JShark.hs` changes before trusting an old `.prof`.

---

## For Cursor agents: timeout loop on slow compile

Use this when a bench or test **hangs or exceeds budget**. Goal: attribute **optimize vs emit vs full compile**, capture a `.prof` before the cap, iterate without unbounded runs.

1. **Kill locked exes** (Windows): `Get-Process jshark-compiler,jshark-test -EA SilentlyContinue | Stop-Process -Force`
2. **Build profiled bench**: `cabal build jshark-compiler --enable-profiling --ghc-options="-fprof-auto-top -fprof-late"`
3. **Stage sweep with timeout** (120s per case; adjust as needed):

```bash
mkdir -p profile
EXE=$(cabal list-bin jshark-compiler)
for PAT in 'lifeStep.optimizeEffect' 'lifeStep.effectfulAST' 'lifeStep.emit' '/lifeStep.emit\/bytes/'; do
  LOG=profile/bench-${PAT//\//-}.log
  echo "=== $PAT ===" | tee "$LOG"
  "$EXE" -t 120s -p "$PAT" +RTS -p -N1 -M4G -RTS 2>&1 | tee -a "$LOG" || true
  mv -f jshark-compiler.prof "profile/jshark-compiler-${PAT//\//-}.prof" 2>/dev/null || true
done
```

4. **Interpret**: `optimizeEffect` OK + `emit/bytes` TIMEOUT → codegen/metadata/bind path; both slow → optimizer + shared walks.
5. **Fix, rebuild, rerun only the failing `-p`**; compare `total alloc` and top cost centres in the new `profile/*.prof`.
6. **Do not commit** `profile/` contents; commit code/docs only.

---

## Quick reference

```bash
# Fast sanity
cabal test jshark-test --test-options='-p codegen' --test-show-details=direct

# Examples (needs bun)
cabal test jshark-test --test-options='-p examples -t 120s' --test-show-details=direct

# Compiler stage attribution
cabal bench jshark-compiler -- jshark-compiler -t 120s -p 'stages.lifeStep'

# Profile a hung emit path (outputs in profile/)
mkdir -p profile && cd profile
cabal build jshark-compiler --enable-profiling --ghc-options="-fprof-auto-top -fprof-late"
EXE=$(cabal list-bin jshark-compiler)
"$EXE" -t 120s -p '/lifeStep.emit\/bytes/' +RTS -p -N1 -M4G -RTS 2>&1 | tee lifeStep-emit-bytes.log
```

## Related files

| Path | Role |
|------|------|
| `test/Main.hs` | main test tree |
| `test/ExampleTests.hs` | optional Bun parse tests; not in default Tasty tree |
| `test/BunTests.hs`, `test/LifeTests.hs` | runtime JS checks |
| `bench/Stages.hs` | shared stage benchmarks |
| `bench/Compiler.hs` | `jshark-compiler` |
| `bench/Examples.hs` | `jshark-compiler-examples` |
| `jshark.cabal` | RTS defaults for `jshark-test` and benches |
| `profile/` | gitignored `.prof` / bench logs from manual runs |
| `.cursorrules` | `cabal test`, Fourmolu, architecture notes |
