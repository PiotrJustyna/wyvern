# wyvern

![](./img/wyvern-small.png)

Simple flowchart diagrams. Inspired by DRAKON.

## goals

* no ambiguity in produced flowcharts
* simple flowchart syntax
* code easy to build and run

## constraints

* connections never cross
* connections are comprised only of horizontal and vertical lines
* default direction: top-to-bottom
* deterministic: input A will always produce output B

## how to compile

* prerequisites:
  * ghc
  * cabal
    * alex
    * happy
    * hlint
    * ormolu

For local installation, [ghcup](https://www.haskell.org/ghcup/) is the most reasonable choice on amd64 machines.

Simply execute:

```bash
./build.sh
```

## how to run

Having compiled the project, simply execute:

```bash
./run.sh
```

## how to test

Having compiled the project, simply execute:

```bash
./test.sh
```

### debugging

* `cabal repl wyvern --repl-options="-fbreak-on-error -fbreak-on-exception"`
* `:load app/Main`
* `:break Blocks 365`
* `:main -i "./diagrams/simple-diagram-1.txt" -o "./diagrams/simple-diagram-1.svg"`

## resources

### DRAKON

DRAKON is wyvern's big brother - more complex, steeper learning curve, more terms, more rules.

* [drakon whitepaper](https://drakon.su/_media/video_i_prezentacii/graphical_syntax_.pdf)

* [drakon wiki](https://en.m.wikipedia.org/wiki/DRAKON)

* [drakon.su](https://drakon.su/start)

* [drakon](https://drakonhub.com/read/docs)

### haskell

* [ghcup](https://www.haskell.org/ghcup)

* [diagrams](https://archives.haskell.org/projects.haskell.org/diagrams/doc/quickstart.html#introduction)

* [diagrams - user manual](https://archives.haskell.org/projects.haskell.org/diagrams/doc/manual.html)

* [alex](https://haskell-alex.readthedocs.io/en/latest/index.html)

* [lexing with alex](https://serokell.io/blog/lexing-with-alex#our-first-lexer)

### colours

* [colours](https://www.colourlovers.com)

* [colorkit](https://colorkit.co/)

  * [default palette](https://colorkit.co/palette/642915-963e20-c7522a-e5c185-fbf2c4-74a892-008585-006464-004343/)
