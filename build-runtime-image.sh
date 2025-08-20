#!/bin/sh

docker buildx build \
  -t "wyvern-runtime:latest" \
  -f "dockerfile-runtime" \
  . \
&& \
docker run \
  -v ./diagrams:/app/diagrams \
  --rm wyvern-runtime:latest \
  -i /app/diagrams/development-environment.txt \
  -o /app/diagrams/development-environment.svg
