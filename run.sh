#!/bin/bash

cabal run Wyvern -- \
  -i "./diagrams/bubble-sort.txt" \
  -o "./diagrams/bubble-sort.svg"

# cabal run Wyvern -- \
#   -i "./diagrams/temp.txt" \
#   -o "./diagrams/bubble-sort.svg"
