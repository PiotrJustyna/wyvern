#!/bin/bash

alex  --ghc ./app/Lexer.x \
      --outfile="./app/Lexer.hs" \
      --info="./app/Lexer.info" && \

happy --ghc ./app/Parser.y \
      --outfile="./app/Parser.hs" \
      --info="./app/Parser.info" && \

cabal build && \

hlint . \
  --ignore-glob=app/Parser.hs \
  --ignore-glob=app/Lexer.hs && \

find . -name '*.hs' ! -name 'Lexer.hs' ! -name 'Parser.hs' -exec ormolu --mode inplace {} +
