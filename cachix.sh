#!/bin/bash
# Push a Nix build to Cachix (optional; CI uses Cabal on GitHub Actions).

set -e

nix build -L
nix-store -qR result | cachix push layer-3-cachix
