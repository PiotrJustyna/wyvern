#!/bin/bash

cabal run Wyvern -- \
  -i "./diagrams/bubble-sort.txt" \
  -o "./diagrams/bubble-sort.svg"
