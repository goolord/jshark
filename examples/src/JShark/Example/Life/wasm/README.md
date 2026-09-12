# Experimental Life WebAssembly kernels

Zig kernels for SIMD row operations and lookup-table (LUT) stepping. These
kernels are not connected to the Life app, which uses the Haskell/JShark
scalar LUT implementation.

## Build

```bash
cd examples/src/JShark/Example/Life/wasm
zig build -Doptimize=ReleaseFast
# artifact: zig-out/bin/life-simd.wasm (not vendored under js/)
```

Requires [Zig](https://ziglang.org/) 0.16+. The build enables wasm `simd128`
so `@Vector(16, u8)` compiles to SIMD instructions.

## Memory layout

After `growTo(w*h*2 + 65536)`:

| Region | Offset | Size |
|--------|--------|------|
| gridA | 0 | w×h |
| gridB | w×h | w×h |
| LUT | 2×w×h | 65536 |

## Exports

| Export | Purpose |
|--------|---------|
| `memory` | Wasm linear memory |
| `growTo(need)` | Grow memory to at least `need` bytes |
| `initLUT(offset)` | Build 65536-entry Conway LUT at `offset` |
| `stepRegionLUT(lut, a, b, w, h, y0, y1)` | Step rows with LUT chunking |
| `clearRow(offset, len)` | SIMD zero-fill |
| `copyRow(src, dst, len)` | SIMD memcpy |

To integrate the kernels, allocate the engine grids in WebAssembly memory
and call these exports from JShark in place of `Lut.stepRegionLUT`.
