#!/bin/bash
cabal run Wyvern -- \
  -i "./diagrams/development-environment.txt" \
  -o "./diagrams/development-environment.svg"
