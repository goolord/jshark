#!/usr/bin/env sh
# Check that a release tag (vX.Y.Z.W) matches every published package's
# cabal `version:`. Usage: scripts/check-tag-version.sh TAG
set -eu

tag="${1:-}"
if [ -z "$tag" ]; then
  echo "usage: $0 TAG" >&2
  exit 2
fi

case "$tag" in
  v*) ver="${tag#v}" ;;
  *)
    echo "tag '$tag' must start with 'v'" >&2
    exit 1
    ;;
esac

rc=0
for p in jshark jshark-lucid jshark-bindgen jshark-hotreload; do
  cabal_file="packages/$p/$p.cabal"
  if [ ! -f "$cabal_file" ]; then
    echo "missing $cabal_file" >&2
    rc=1
    continue
  fi
  pv=$(sed -n 's/^version:[[:space:]]*//p' "$cabal_file" | head -1 | tr -d '[:space:]')
  if [ "$pv" != "$ver" ]; then
    echo "$p: cabal version '$pv' != tag '$tag'" >&2
    rc=1
  else
    echo "$p: $pv ok"
  fi
done

exit "$rc"
