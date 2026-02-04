## [0.4.0.0](https://github.com/PiotrJustyna/wyvern/tree/0.4.0.0) (2026-02-04)

Continuing my prior work on extracting more error information from the lexical analyzer, I am now extracting more information from parser errors, all pretty printed:

* the token initiating the error
* line number
* column number

### before

```
input path: "./diagrams/temp.txt"
output path: "./diagrams/temp.svg"
[TokenAction (AlexPn 0 1 1) "# \"action\"",TokenAction (AlexPn 9 2 1) "# \"question\"",TokenOCB (AlexPn 20 3 1),TokenSoloIdentifier (AlexPn 26 4 5) "123",TokenSoloIdentifier (AlexPn 30 4 9) "456",TokenAction (AlexPn 33 4 12) "# \"action\"",TokenCCB (AlexPn 42 5 1),TokenOCB (AlexPn 44 6 1),TokenAction (AlexPn 50 7 5) "# \"action\"",TokenCCB (AlexPn 59 8 1),TokenAction (AlexPn 61 9 1) "# \"action\""]
wyvern-diagrams: Parse error in line 1.

CallStack (from HasCallStack):
  error, called at src/lib/Parser.hs:279:24 in wyvern-diagrams-0.3.2.0-inplace:Parser
```

### after

```
input path: "./diagrams/temp.txt"
output path: "./diagrams/temp.svg"
wyvern-diagrams: Parse error - provided input did not match any grammar production rules: token "123" - line: 4, column: 5.
CallStack (from HasCallStack):
  error, called at src/lib/Parser.hs:281:20 in wyvern-diagrams-0.3.2.0-inplace:Parser
```

- resolved issue 55 - more details extracted from the lexer: https://github.com/PiotrJustyna/wyvern/issues/55

## [0.3.2.0](https://github.com/PiotrJustyna/wyvern/tree/0.3.2.0) (2026-01-28)

It is imperative to the end user to see where in the input files they are making mistakes. Previous versions just informed the user that there is a lexing error in their input while the new version points out exactly where it was found. The change makes learning wyvern and working with it that much easier. I hope!

- resolved issue 55 - more details extracted from the lexer: https://github.com/PiotrJustyna/wyvern/issues/55

## [0.3.1.3](https://github.com/PiotrJustyna/wyvern/tree/0.3.1.2) (2026-01-22)

- resolved issue 52 - support for text 2.1.3: https://github.com/PiotrJustyna/wyvern/issues/52

## [0.3.1.1](https://github.com/PiotrJustyna/wyvern/tree/0.3.1.1) (2026-01-21)

- resolved issue 50 - support for GHC 9.14.1 and base 4.22.0.0: https://github.com/PiotrJustyna/wyvern/issues/50

## [0.3.1.0](https://github.com/PiotrJustyna/wyvern/tree/0.3.1.0) (2026-01-19)

- resolved issue 47 - a problem with connections in tested forks: https://github.com/PiotrJustyna/wyvern/issues/47

## [0.3.0.7](https://github.com/PiotrJustyna/wyvern/tree/0.3.0.7) (2026-01-15)

- support for ghc 9.8.4 and base 4.19.2.0 for hackage
- various changes for hackage: new example, readme update, etc.

## [0.3.0.6](https://github.com/PiotrJustyna/wyvern/tree/0.3.0.6) (2026-01-14)

- Producing wyvern release candidate for hackage.
