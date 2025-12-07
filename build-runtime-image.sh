#!/bin/sh

docker buildx build \
  -t "wyvern-runtime:latest" \
  -f "dockerfile-runtime" \
  .
