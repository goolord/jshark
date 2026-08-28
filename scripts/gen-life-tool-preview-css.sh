#!/usr/bin/env bash
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
out="$root/examples/static/life-tool-preview.css"
ghc -i"$root/examples/Life" "$root/scripts/gen-life-tool-preview-css.hs" -o /tmp/gen-life-tool-preview-css
/tmp/gen-life-tool-preview-css > "$out"
