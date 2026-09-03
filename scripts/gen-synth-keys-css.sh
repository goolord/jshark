#!/usr/bin/env bash
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
out="$root/examples/static/css/synth-keys.css"
ghc -i"$root/examples/src" "$root/scripts/gen-synth-keys-css.hs" -o /tmp/gen-synth-keys-css
/tmp/gen-synth-keys-css > "$out"
