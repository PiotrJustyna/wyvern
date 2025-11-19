#!/bin/bash

cabal run Wyvern -- \
  -i "./diagrams/simple-diagram-1.txt" \
  -o "./diagrams/simple-diagram-1.svg"

cabal run Wyvern -- \
  -i "./diagrams/simple-diagram-2.txt" \
  -o "./diagrams/simple-diagram-2.svg"

cabal run Wyvern -- \
  -i "./diagrams/simple-diagram-3.txt" \
  -o "./diagrams/simple-diagram-3.svg"

cabal run Wyvern -- \
   -i "./diagrams/simple-diagram-4.txt" \
   -o "./diagrams/simple-diagram-4.svg"

cabal run Wyvern -- \
   -i "./diagrams/simple-diagram-5.txt" \
   -o "./diagrams/simple-diagram-5.svg"

cabal run Wyvern -- \
   -i "./diagrams/simple-diagram-6.txt" \
   -o "./diagrams/simple-diagram-6.svg"