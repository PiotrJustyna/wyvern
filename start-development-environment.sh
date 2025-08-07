#!/bin/sh

# 2024-12-04 PJ:
# --------------
# At the time of writing this, these directories are covered here:
# https://cabal.readthedocs.io/en/latest/config.html#directories
# Mounting them as volumes allows us to reuse cabal files
# between containers saving a significant amount of build time.
docker volume create wyvern-cabal-config
docker volume create wyvern-cabal-cache
docker volume create wyvern-cabal-state

docker buildx build \
  -t "wyvern:latest" \
  -f "dockerfile" \
  . \
&& \
docker run \
  -it \
  -v "$HOME/.ssh/:/root/.ssh:ro" \
  -v "$(pwd):/root/code/wyvern" \
  -v wyvern-cabal-config:/root/.config/cabal \
  -v wyvern-cabal-cache:/root/.cache/cabal \
  -v wyvern-cabal-state:/root/.local/state/cabal \
  --rm "wyvern:latest"
