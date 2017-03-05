# monkey-hs

An interpreter for the Monkey programming language written in Haskell

![The Monkey Programming Language](https://cloud.githubusercontent.com/assets/1013641/22617482/9c60c27c-eb09-11e6-9dfa-b04c7fe498ea.png)

*The official Monkey logo*

## What's Monkey?

Monkey is a programming language designed for learning about interpreter
implementation, used in a book, [Writing An Interpreter In Go](https://interpreterbook.com/#the-monkey-programming-language).

## Why Haskell?

The original implementation in the book is written in Go. Go is a great
language, but I personally find Haskell more attractive than Go. Haskell is
well-known for its efficiency in writing compiler too.

Most importantly, writing Haskell makes me productive and happy. :sparkles:

## Instruction

### Build, test and install

```bash
$ stack build
$ stack test
$ stack install # will install binaries in $PATH
```

### REPL

```bash
$ mkrepl
$ stack exec mkrepl # without install
```

### Interpreter

``` bash
$ mki examples/map-reduce.mk
$ stack exec mki examples/hash.mk # without install
```

## License

[BSD3](LICENSE)
