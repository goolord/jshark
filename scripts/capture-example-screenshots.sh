#!/usr/bin/env bash
# Refresh examples/static/img/<name>.png from a running examples server.
# Usage: cabal run exe:jshark-examples
#        ./scripts/capture-example-screenshots.sh
set -euo pipefail

root="$(cd "$(dirname "$0")/.." && pwd)"
script="$root/scripts/capture-example-screenshots.mjs"
base="${SHOT_BASE:-http://127.0.0.1:3000}"

if [[ -z "${CHROME:-}" ]]; then
  for candidate in chromium chromium-browser google-chrome google-chrome-stable google-chrome-unstable; do
    if command -v "$candidate" >/dev/null 2>&1; then
      CHROME="$(command -v "$candidate")"
      break
    fi
  done
fi
if [[ -z "${CHROME:-}" ]]; then
  echo "capture: chromium/chrome not on PATH (set CHROME)" >&2
  exit 1
fi
export CHROME

if ! command -v curl >/dev/null 2>&1; then
  echo "capture: curl required to probe $base" >&2
  exit 1
fi
if ! curl -fsS -o /dev/null "$base/"; then
  echo "capture: examples server not reachable at $base" >&2
  echo "capture: start it with: cabal run exe:jshark-examples" >&2
  exit 1
fi

cache="${XDG_CACHE_HOME:-$HOME/.cache}/jshark-screenshots"
mkdir -p "$cache"
if [[ ! -d "$cache/node_modules/puppeteer-core" ]]; then
  if command -v npm >/dev/null 2>&1; then
    npm install --prefix "$cache" --silent puppeteer-core
  else
    echo "capture: npm required to install puppeteer-core" >&2
    exit 1
  fi
fi

export NODE_PATH="$cache/node_modules${NODE_PATH:+:$NODE_PATH}"
export SHOT_BASE="$base"
export SHOT_OUT="${SHOT_OUT:-$root/examples/static/img}"

if command -v bun >/dev/null 2>&1; then
  bun "$script"
else
  node "$script"
fi
