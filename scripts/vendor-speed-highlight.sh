#!/usr/bin/env bash
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
dest="$root/examples/static/speed-highlight"
ver="${1:-2.1.0}"
tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT
cd "$tmpdir"
npm pack "@speed-highlight/core@${ver}" >/dev/null
tar -xzf speed-highlight-core-*.tgz
rm -rf "$dest"
mkdir -p "$dest"
cp -a package/dist/. "$dest/"
echo "vendored @speed-highlight/core@${ver} -> examples/static/speed-highlight/"
echo "note: jshark.cabal data-files lists speed-highlight/**/*.js|css|ts|map|json — extend if npm adds new extensions"
