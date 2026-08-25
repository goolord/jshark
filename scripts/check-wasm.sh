#!/usr/bin/env bash
# Rebuild vendored WASM artifacts. With no flags, rebuild then verify checksums.
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"

build_only=false
if [[ "${1:-}" == "--build-only" ]]; then
  build_only=true
fi

cabal run build-hvm2-demo-wasm -v0
(
  cd examples/Life/wasm
  zig build -Doptimize=ReleaseFast
)
cp examples/Life/wasm/zig-out/bin/life-simd.wasm examples/Life/js/life-simd.wasm

if [[ "$build_only" == false ]]; then
  sha256sum -c wasm/checksums.sha256
  echo "WASM checksums OK"
fi
