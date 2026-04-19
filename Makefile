.PHONY: help build test lint format format-check clean run repl all

help:
	@echo "Available targets:"
	@echo "  make build          - Build the project"
	@echo "  make test           - Run tests"
	@echo "  make lint           - Run hlint"
	@echo "  make format         - Format code with ormolu"
	@echo "  make format-check   - Check formatting without changes"
	@echo "  make clean          - Clean build artifacts"
	@echo "  make run            - Run the application"
	@echo "  make repl           - Start GHCi REPL"
	@echo "  make all            - Build and test"

build: lint format
	cabal build --enable-executable-stripping --ghc-options="-Wall -Wunused-packages"

test: lint format
	cabal test

lint:
	hlint . \
	  --ignore-glob=src/lib/Parser.hs \
	  --ignore-glob=src/lib/Lexer.hs

format:
	find . -name '*.hs' ! -name 'Lexer.hs' ! -name 'Parser.hs' -exec ormolu --mode inplace {} +

format-check:
	find . -name '*.hs' ! -name 'Lexer.hs' ! -name 'Parser.hs' -exec ormolu --mode check {} +

clean:
	cabal clean

run: lint format
	cabal run wyvern-diagrams -- \
	    -i "./diagrams/general/temp.txt" \
	    -o "./diagrams/general/temp.svg"

repl:
	cabal repl wyvern-diagrams --repl-options="-fbreak-on-error -fbreak-on-exception"

all: build test
