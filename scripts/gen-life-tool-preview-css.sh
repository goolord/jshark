#!/usr/bin/env bash
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
out="$root/examples/static/css/life-tool-preview.css"
ghc -i"$root/examples/src" "$root/scripts/gen-life-tool-preview-css.hs" -o /tmp/gen-life-tool-preview-css
/tmp/gen-life-tool-preview-css > "$out"
