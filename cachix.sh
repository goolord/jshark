#!/bin/bash

set -e

nix build -L
nix-store -qR result | cachix push layer-3-cachix
