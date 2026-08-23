# Life WASM SIMD kernels

Small Zig module that accelerates hot memory ops for the JavaScript Life
engine. **All Conway stepping logic lives in `LUTGenerator.js`**; wasm only
provides SIMD `clearRow` and `copyRow` on linear memory.

## Build

```bash
cd examples/Life/wasm
zig build -Doptimize=ReleaseFast
cp zig-out/bin/life-simd.wasm ../js/life-simd.wasm
```

Requires [Zig](https://ziglang.org/) 0.16+. The build enables wasm `simd128`
so `@Vector(16, u8)` lowers to real SIMD instructions.

## Exports

| Export | Purpose |
|--------|---------|
| `memory` | Wasm linear memory (grids are rebound here by `LifeSimd.bindGrids`) |
| `growTo(need)` | Grow memory to at least `need` bytes |
| `clearRow(offset, len)` | SIMD zero-fill |
| `copyRow(src, dst, len)` | SIMD memcpy |

## Integration

Load order in the page:

1. `LUTGenerator.js` — LUT + `stepRegionLUT` (unchanged logic, calls `LifeSimd` when ready)
2. `LifeSimd.js` — loader + JS fallbacks
3. `Main.js` — `LifeEngine.loadWasm('js/life-simd.wasm')` migrates grids into wasm memory

If wasm is unavailable, `LifeSimd.clearRow` / `copyRow` fall back to `Uint8Array.fill` / `.set`.
