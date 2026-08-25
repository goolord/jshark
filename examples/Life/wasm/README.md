# Life WASM SIMD kernels

Zig module that accelerates the JavaScript Life engine. Wasm provides:

- SIMD `clearRow` / `copyRow` on linear memory
- Full `stepRegionLUT` (same algorithm as `LUTGenerator.js`, compiled)

## Build

```bash
cd examples/Life/wasm
zig build -Doptimize=ReleaseFast
cp zig-out/bin/life-simd.wasm ../js/life-simd.wasm
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
| `memory` | Wasm linear memory (grids rebound here by `LifeSimd.bindGrids`) |
| `growTo(need)` | Grow memory to at least `need` bytes |
| `initLUT(offset)` | Build 65536-entry Conway LUT at `offset` |
| `stepRegionLUT(lut, a, b, w, h, y0, y1)` | Step rows with LUT chunking |
| `clearRow(offset, len)` | SIMD zero-fill |
| `copyRow(src, dst, len)` | SIMD memcpy |

## Integration

Load order in the page:

1. `LUTGenerator.js`: LUT + `stepRegionLUT` (delegates to wasm when ready)
2. `LifeSimd.js`: loader + JS fallbacks
3. `Main.js`: `LifeEngine.loadWasm('js/life-simd.wasm')` migrates grids into wasm memory

Workers keep the pure-JS path (no wasm in `EngineWorker.js`). If wasm is
unavailable, `LifeSimd` methods fall back to `Uint8Array.fill` / `.set` and
`LUTGenerator.js` runs the scalar JS step loop.
