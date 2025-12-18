#!/bin/bash

docker run \
  -v ./diagrams:/app/diagrams \
  --rm wyvern-runtime:latest \
  -i /app/diagrams/bubble-sort.txt \
  -o /app/diagrams/bubble-sort.svg
