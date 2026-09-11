#!/usr/bin/env bash
# Compile-check optional Life zig kernels.
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"

if command -v zig >/dev/null 2>&1; then
  (
    cd examples/src/JShark/Example/Life/wasm
    zig build -Doptimize=ReleaseFast
  )
else
  echo "warning: zig not on PATH; skipping Life wasm compile-check" >&2
fi
