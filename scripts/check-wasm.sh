#!/usr/bin/env bash
# Rebuild vendored WASM artifacts and compile-check optional Life zig kernels.
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"

build_only=false
if [[ "${1:-}" == "--build-only" ]]; then
  build_only=true
fi

cabal run build-hvm2-demo-wasm -v0

if command -v zig >/dev/null 2>&1; then
  (
    cd examples/src/JShark/Example/Life/wasm
    zig build -Doptimize=ReleaseFast
  )
else
  echo "warning: zig not on PATH; skipping Life wasm compile-check" >&2
fi

# not deterministic
# if [[ "$build_only" == false ]]; then
#   sha256sum examples/static/hvm2/hvm2-demo.wasm > wasm/checksums.sha256
#   echo "WASM checksums written to wasm/checksums.sha256"
# fi
