# Life WASM SIMD kernels (optional / not wired)

Zig module with SIMD `clearRow` / `copyRow` and LUT `stepRegionLUT`. The Life
app uses the Haskell/JShark scalar LUT path today; this tree is kept for a
possible future wasm grid migration.

## Build

```bash
cd examples/Life/wasm
zig build -Doptimize=ReleaseFast
# artifact: zig-out/bin/life-simd.wasm (not vendored under js/)
```

Requires [Zig](https://ziglang.org/) 0.16+. The build enables wasm `simd128`
so `@Vector(16, u8)` lowers to real SIMD instructions.

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

To use this again, allocate engine grids in wasm memory and call the exports
from JShark instead of the scalar `Lut.stepRegionLUT` loop.
