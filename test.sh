#!/bin/bash

hlint . \
  --ignore-glob=lib/Parser.hs \
  --ignore-glob=lib/Lexer.hs

find . -name '*.hs' ! -name 'Lexer.hs' ! -name 'Parser.hs' -exec ormolu --mode inplace {} +

cabal test
