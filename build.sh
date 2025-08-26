#!/bin/bash

alex  --ghc ./lib/Lexer.x \
      --outfile="./lib/Lexer.hs" \
      --info="./lib/Lexer.info" && \

happy --ghc ./lib/Parser.y \
      --outfile="./lib/Parser.hs" \
      --info="./lib/Parser.info" && \

hlint . \
  --ignore-glob=lib/Parser.hs \
  --ignore-glob=lib/Lexer.hs && \

find . -name '*.hs' ! -name 'Lexer.hs' ! -name 'Parser.hs' -exec ormolu --mode inplace {} + && \

cabal build --enable-executable-stripping
