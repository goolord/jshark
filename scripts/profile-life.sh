#!/usr/bin/env bash
# Sample Life FPS / GL from a running examples server.
# Usage: cabal run exe:jshark-examples
#        ./scripts/profile-life.sh
# Env: LIFE_BASE   (default http://127.0.0.1:3000)
#      LIFE_MS     (default 8000)  how long to collect
#      LIFE_URL    (default $LIFE_BASE/life/frame/)
#      LIFE_HEADED=1  show the window (real GPU; headless is often SwiftShader)
set -euo pipefail

root="$(cd "$(dirname "$0")/.." && pwd)"
script="$root/scripts/profile-life.mjs"
base="${LIFE_BASE:-http://127.0.0.1:3000}"

if [[ -z "${CHROME:-}" ]]; then
  for candidate in chromium chromium-browser google-chrome google-chrome-stable google-chrome-unstable; do
    if command -v "$candidate" >/dev/null 2>&1; then
      CHROME="$(command -v "$candidate")"
      break
    fi
  done
fi
if [[ -z "${CHROME:-}" ]]; then
  echo "profile-life: chromium/chrome not on PATH (set CHROME)" >&2
  exit 1
fi
export CHROME

if ! command -v curl >/dev/null 2>&1; then
  echo "profile-life: curl required to probe $base" >&2
  exit 1
fi
if ! curl -fsS -o /dev/null "$base/"; then
  echo "profile-life: examples server not reachable at $base" >&2
  echo "profile-life: start it with: cabal run exe:jshark-examples" >&2
  exit 1
fi

cache="${XDG_CACHE_HOME:-$HOME/.cache}/jshark-screenshots"
mkdir -p "$cache"
if [[ ! -d "$cache/node_modules/puppeteer-core" ]]; then
  if command -v npm >/dev/null 2>&1; then
    npm install --prefix "$cache" --silent puppeteer-core
  else
    echo "profile-life: npm required to install puppeteer-core" >&2
    exit 1
  fi
fi

export NODE_PATH="$cache/node_modules${NODE_PATH:+:$NODE_PATH}"
export LIFE_BASE="$base"
export LIFE_URL="${LIFE_URL:-$base/life/frame/}"
export LIFE_MS="${LIFE_MS:-8000}"

if command -v bun >/dev/null 2>&1; then
  bun "$script"
else
  node "$script"
fi
