#!/usr/bin/env bash
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
dest="$root/examples/static/speed-highlight"
ver="${1:-2.1.0}"
tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT
if ! command -v bun >/dev/null; then
  echo "bun required — install from https://bun.sh" >&2
  exit 1
fi
cd "$tmpdir"
bun init -y >/dev/null
bun add "@speed-highlight/core@${ver}" >/dev/null
rm -rf "$dest"
mkdir -p "$dest"
cp -a node_modules/@speed-highlight/core/dist/. "$dest/"
echo "vendored @speed-highlight/core@${ver} -> examples/static/speed-highlight/"
echo "note: jshark.cabal data-files lists speed-highlight/**/*.js|css|ts|map|json — extend if upstream adds new extensions"
