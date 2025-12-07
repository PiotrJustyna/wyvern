#!/bin/bash

cabal run wyvern -- \
  -i "./diagrams/simple-diagram-1.txt" \
  -o "./diagrams/simple-diagram-1.svg"

cabal run wyvern -- \
  -i "./diagrams/simple-diagram-2.txt" \
  -o "./diagrams/simple-diagram-2.svg"

cabal run wyvern -- \
  -i "./diagrams/simple-diagram-3.txt" \
  -o "./diagrams/simple-diagram-3.svg"

cabal run wyvern -- \
   -i "./diagrams/simple-diagram-4.txt" \
   -o "./diagrams/simple-diagram-4.svg"

cabal run wyvern -- \
   -i "./diagrams/simple-diagram-5.txt" \
   -o "./diagrams/simple-diagram-5.svg"

cabal run wyvern -- \
   -i "./diagrams/simple-diagram-6.txt" \
   -o "./diagrams/simple-diagram-6.svg"

cabal run wyvern -- \
   -i "./diagrams/simple-diagram-7.txt" \
   -o "./diagrams/simple-diagram-7.svg"

cabal run wyvern -- \
   -i "./diagrams/simple-diagram-8.txt" \
   -o "./diagrams/simple-diagram-8.svg"
