#!/bin/bash

hlint . \
  --ignore-glob=src/lib/Parser.hs \
  --ignore-glob=src/lib/Lexer.hs

find . -name '*.hs' ! -name 'Lexer.hs' ! -name 'Parser.hs' -exec ormolu --mode inplace {} +

cabal test
