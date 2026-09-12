#!/usr/bin/env bash
# Verify that the release source distributions build and test outside the
# monorepo, with no root-project overrides. Unpacks the four published
# packages into a fresh directory, writes a minimal cabal.project that
# points only at the unpacked sources, and runs the test suites.
#
# Usage: scripts/verify-sdists.sh
set -euo pipefail

root=$(pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

echo "== sdist =="
rm -f dist-newstyle/sdist/*.tar.gz
cabal sdist jshark jshark-lucid jshark-bindgen jshark-hotreload

version=$(sed -n 's/^version:[[:space:]]*//p' packages/jshark/jshark.cabal | head -1 | tr -d '[:space:]')

echo "== unpack into $tmp =="
for p in jshark jshark-lucid jshark-bindgen jshark-hotreload; do
  tarball="dist-newstyle/sdist/$p-$version.tar.gz"
  if [ ! -f "$tarball" ]; then
    echo "missing $tarball" >&2
    exit 1
  fi
  tar -xzf "$tarball" -C "$tmp"
  echo "  $(basename "$tarball")"
done

packages=$(cd "$tmp" && ls -d jshark*/ | sed 's:/$::' | tr '\n' ' ')
cat >"$tmp/cabal.project" <<EOF
packages: $packages
tests: True
benchmarks: False
optimization: 2
EOF

echo "== build (clean project) =="
cd "$tmp"
cabal build all

# The bindgen test suite drives the shipped extract.mjs, which needs the
# locked typescript. Install it from the sdist's package.json/bun.lock.
bindgen_dir=$(ls -d "$tmp"/jshark-bindgen-*/ | head -1)
echo "== bun install ($bindgen_dir) =="
(cd "$bindgen_dir" && bun install)

echo "== test (clean project) =="
cabal test all --test-show-details=direct \
  --test-options='+RTS -N1 -M4G -RTS -t120'

echo "== install jshark-bindgen and run it on a shipped fixture =="
cabal install exe:jshark-bindgen --installdir="$tmp/bin" --overwrite-policy=always
bindgen="$tmp/bin/jshark-bindgen"
fixture=$(ls "$tmp"/jshark-bindgen-*/test/fixtures/jshark-bindgen/toy.d.ts)
typescript_dir="$bindgen_dir/node_modules/typescript"
# An installed extractor has no node_modules beside it; point it at the
# locked TypeScript the way a consumer would.
out=$(JSHARK_BINDGEN_TYPESCRIPT="$typescript_dir" \
  "$bindgen" -m VerifyToy -p toy "$fixture")
echo "$out" | grep -q 'ffi "toy.greet"'
echo "$out" | grep -q "data Widget"
echo "jshark-bindgen verified"

echo "verify-sdists: ok"
