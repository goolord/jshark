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
bend gen-c generated/kernel.bend -o generated/kernel.c
# emit kernel_exports.c from Haskell (see JShark.EmitBend.emitKernelExportsC)
zig build \
  -Doptimize=ReleaseFast \
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

## Lint

Pass `--warn-hvm2-candidates` when compiling (via `JShark.Compiler.applyCompilerArgs`)
to print closed pure subtrees that compile to Bend and exceed the default size
threshold (8 IR nodes). Example:

```bash
cabal run examples -- --warn-hvm2-candidates
```

Warnings go to stderr; each line suggests a `hvm2Kernel "candidate_N" (...)` site.
