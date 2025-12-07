#!/bin/sh

docker volume create wyvern-cabal-config
docker volume create wyvern-cabal-cache
docker volume create wyvern-cabal-state

docker buildx build \
  -t "wyvern:latest" \
  -f "dockerfile-sdk" \
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
