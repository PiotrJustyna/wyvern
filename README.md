# wyvern

**wyvern**: a simplified version of DRAKON diagramming language.

![](./img/wyvern-small.png)

## goals

* no ambiguity block diagrams
* accessible - easy to:
  * build
  * run
  * troubleshoot

## constraints

* only horizontal and vertical lines
* default direction: top-to-bottom
* connections never cross
* connections overlap only when it doesn't cause ambiguity
* deterministic: input A will always produce output B

## how to compile

* prerequisites:
  * ghc
  * cabal
    * alex
    * happy
    * hlint
    * ormolu

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

## how to build runtime image

Having compiled the project, simply execute:

```bash
./ops/build-runtime-image.sh
```

TODO:

* implement lexing and parsing in runtime image building process

### debugging

* `cabal repl wyvern --repl-options="-fbreak-on-error -fbreak-on-exception"`
* `:load app/Main`
* `:break Blocks 365`
* `:main -i "./diagrams/simple-diagram-1.txt" -o "./diagrams/simple-diagram-1.svg"`

## runtime environment

TODO

For those looking to only run a compiled version of wyvern in a container, there is a runtime image I prepared (only `arm64` version for now):

```
docker pull TODO
```

| command | description |
| --- | --- |
| `./ops/run-in-container.sh` | runs code |

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
