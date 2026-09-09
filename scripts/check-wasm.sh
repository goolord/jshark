#!/usr/bin/env bash
# Rebuild vendored WASM artifacts and compile-check optional Life zig kernels.
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"

cabal run build-hvm2-demo-wasm -v0

if command -v zig >/dev/null 2>&1; then
  (
    cd examples/src/JShark/Example/Life/wasm
    zig build -Doptimize=ReleaseFast
  )
else
  echo "warning: zig not on PATH; skipping Life wasm compile-check" >&2
fi
