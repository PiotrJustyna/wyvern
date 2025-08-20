#!/bin/sh

docker buildx build \
  -t "wyvern-runtime:latest" \
  -f "dockerfile-runtime" \
  . \
&& \
docker run \
  -v ./diagrams:/tmp/ \
  --rm wyvern-runtime:latest \
  -i /tmp/development-environment.txt \
  -o /tmp/development-environment.svg
