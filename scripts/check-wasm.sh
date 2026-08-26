#!/usr/bin/env bash
# Rebuild vendored WASM artifacts and refresh checksums.
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"

cabal run build-hvm2-demo-wasm -v0
(
  cd examples/Life/wasm
  zig build -Doptimize=ReleaseFast
)
cp examples/Life/wasm/zig-out/bin/life-simd.wasm examples/Life/js/life-simd.wasm

sha256sum \
  examples/static/hvm2-demo.wasm \
  examples/Life/js/life-simd.wasm \
  > wasm/checksums.sha256
echo "WASM checksums written to wasm/checksums.sha256"
