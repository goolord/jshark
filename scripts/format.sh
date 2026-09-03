#!/usr/bin/env bash
# Format all Haskell sources across the monorepo packages.
set -euo pipefail
fourmolu -i ./packages ./examples
