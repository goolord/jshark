#!/usr/bin/env bash
# Bump helper — Pico is a committed static asset. CI does not run this.
# Usage: ./scripts/vendor-pico.sh [version]
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
dest="$root/examples/static/pico"
pin="$root/scripts/pico-version"
ver="${1:-$(tr -d '[:space:]' < "$pin")}"
tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT
mkdir -p "$dest"
curl -fsSL "https://cdn.jsdelivr.net/npm/@picocss/pico@${ver}/css/pico.min.css" \
  -o "$tmpdir/pico.min.css"
printf '%s\n' "$ver" > "$pin"
printf '%s\n' "$ver" > "$dest/VERSION"
mv "$tmpdir/pico.min.css" "$dest/pico.min.css"
echo "wrote @picocss/pico@${ver} -> examples/static/pico/pico.min.css"
