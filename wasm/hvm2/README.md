# HVM2 kernel WASM build

Compiles Bend/HVM2-generated C to WebAssembly with Zig (same toolchain pattern
as `examples/Life/wasm/`).

Pipeline:

1. JShark `Hvm2Kernel` nodes → Bend (`.bend`) via `JShark.Hvm2.bendModule`
2. `bend gen-c` → `kernel.c`
3. JShark generates `kernel_exports.c` with per-kernel WASM export shims
4. `zig build` (this directory) → `jshark-hvm2.wasm`

## Build

After generating sources (Haskell `compileHvm2Wasm` writes them, or by hand):

```bash
cd wasm/hvm2
mkdir -p generated
# copy or emit kernel.bend, then:
bend gen-c generated/kernel.bend > generated/kernel.c
# emit kernel_exports.c from Haskell (see JShark.EmitBend.emitKernelExportsC)
zig build \
  -Doptimize=ReleaseFast \
  -Dtpc-l2=2 \
  -Dkernel-c=/absolute/path/to/kernel.c \
  -Dexports-c=/absolute/path/to/kernel_exports.c
cp zig-out/bin/jshark-hvm2.wasm ../../examples/static/jshark-hvm2.wasm
```

Requires [Bend](https://github.com/HigherOrderCO/Bend) and [Zig](https://ziglang.org/) 0.16+.

## JS load

Page loads the module into `globalThis.__jsharkHvm2.exports`. Use
`JShark.Api.loadHvm2Wasm` in your effect program to fetch and instantiate the
module before calling `hvm2Kernel` sites. JShark codegen for `hvm2Kernel "name" …`
emits a callable wrapper around `__jsharkHvm2.exports["name"]` (throws if the
kernel is missing).

## Headless dev tools

Node scripts (no browser needed) for the HVM2 threading work. They run
against the built demo wasm (`scripts/check-wasm.sh --build-only` or a
prior site export):

- `check-threads.mjs` — measures the parallel steal path on real
  `node:worker_threads` (eval workers first over a shared memory,
  coordinator last). Reports ms per grid job; tune with `LIVE`, `CELLS`,
  `REPS`, `BX`, `BY` env vars.
- `check-tm-fix.mjs` — regression check for the eval-worker TM binding
  fix: instantiates the wasm twice over one shared memory and asserts
  both instances agree on the same non-null `tm[]` slots.
- `wasm-mem.mjs` — shared helper that reads imported-memory limits out
  of the wasm binary so harnesses do not hardcode page counts.
- `thread-eval-worker.mjs` — the eval worker driven by
  `check-threads.mjs` (mirrors `examples/static/hvm2-worker.js`).

## Lint

Pass `--warn-hvm2-candidates` when compiling (via `JShark.Compiler.applyCompilerArgs`)
to print closed pure subtrees that compile to Bend and exceed the default size
threshold (8 IR nodes). Example:

```bash
cabal run examples -- --warn-hvm2-candidates
```

Warnings go to stderr; each line suggests a `hvm2Kernel "candidate_N" (...)` site.
