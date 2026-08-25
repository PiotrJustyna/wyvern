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
	@echo "  make demo           - Run the application in demo mode"
	@echo "  make repl           - Start GHCi REPL"
	@echo "  make all            - Build and test"

build: lint format
	cabal build --enable-executable-stripping --ghc-options="-Wall -Wunused-packages"

test: lint format
	cabal test

lint:
	hlint .

format:
	find . -name '*.hs' ! -name 'Lexer.hs' ! -name 'Parser.hs' -exec ormolu --mode inplace {} +

format-check:
	find . -name '*.hs' ! -name 'Lexer.hs' ! -name 'Parser.hs' -exec ormolu --mode check {} +

clean:
	cabal clean

run: lint format
	cabal run wyvern-diagrams -- \
	    -i "./diagrams/general/simple-diagram-4.txt" \
	    -o "./diagrams/general/simple-diagram-4.svg"

demo: lint format
	cabal run wyvern-diagrams -- \
	    -i "./diagrams/general/simple-diagram-1.txt" \
	    -o "./diagrams/general/simple-diagram-1.svg" && \
	cabal run wyvern-diagrams -- \
	    -i "./diagrams/general/simple-diagram-2.txt" \
	    -o "./diagrams/general/simple-diagram-2.svg" && \
	cabal run wyvern-diagrams -- \
	    -i "./diagrams/general/simple-diagram-3.txt" \
	    -o "./diagrams/general/simple-diagram-3.svg" && \
	cabal run wyvern-diagrams -- \
	    -i "./diagrams/general/simple-diagram-4.txt" \
	    -o "./diagrams/general/simple-diagram-4.svg" && \
	cabal run wyvern-diagrams -- \
	    -i "./diagrams/general/simple-diagram-5.txt" \
	    -o "./diagrams/general/simple-diagram-5.svg"

repl:
	cabal repl wyvern-diagrams --repl-options="-fbreak-on-error -fbreak-on-exception"

all: build test
