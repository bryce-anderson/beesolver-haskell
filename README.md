# Spelling Bee Solver

Solves the [New York Times Spelling Bee](https://www.nytimes.com/puzzles/spelling-bee).

This is a modification of Kevin Olivers's [projects](https://github.com/kevinoliver/beesolver-java),
just to see what it's like in Haskell.

Their dictionary is not the same as the one included so you may find missing words as well as extras.

## Usage

```
$ stack build
$ stack exec beesolver-haskell-exe -- --help
The beesolver program

beesolver [OPTIONS] REQUIRED OTHERS
  Solve a Puzzle

Common flags:
  -d --dict=ITEM    Path to custom dictionary
  -w --wordsoutput  Output matching words
  -? --help         Display help message
  -V --version      Print version information
```

## Requirements

* [Haskell stack](https://docs.haskellstack.org/en/stable/). Version 2.9.1 is what I used. The
  [text-icu](https://hackage.haskell.org/package/text-icu) package uses an external library and
  may be a pain and in addition to install the native library you'll probably need to to do
  this as mentioned in the docs:

```
$ export PKG_CONFIG_PATH="/usr/local/opt/icu4c/lib/pkgconfig"

```
