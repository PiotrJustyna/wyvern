#!/bin/bash

alex  --ghc ./src/lib/Lexer.x \
      --outfile="./src/lib/Lexer.hs" \
      --info="./src/lib/Lexer.info" && \

alex  --ghc ./src/lib/LexerV2.x \
      --outfile="./src/lib/LexerV2.hs" \
      --info="./src/lib/LexerV2.info" && \

happy --ghc ./src/lib/Parser.y \
      --outfile="./src/lib/Parser.hs" \
      --info="./src/lib/Parser.info" && \

hlint . \
  --ignore-glob=src/lib/Parser.hs \
  --ignore-glob=src/lib/Lexer.hs

find . -name '*.hs' ! -name 'Lexer.hs' ! -name 'Parser.hs' -exec ormolu --mode inplace {} +

cabal build --enable-executable-stripping
