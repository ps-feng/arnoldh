# ArnoldH

This is a compiler for the ArnoldC language, which is based on the one-liners of Arnold
Schwarzenegger. It was developed during some innovation weeks at XING for the purpose of learning Haskell. The original implementation is in Scala and can be found [here]( http://lhartikk.github.io/ArnoldC/).

The original version compiles to JVM bytecode while this one converts to JavaScript.

Improvements over the original implementation:
- Scoped blocks. `if-else` and `while` blocks in the original implementation were not scoped. You could declare a variable within a block and it'd become usable in the whole method after that.
- Semantic validation happens before code generation. The original implementation relied on the bytecode library it was using to check for errors and the error reporting is not very nice.
- Nice errors. When an error occurs, the line where the error was found is printed
and the actual error is underlined.

TODO:
- `Read` code generation. Implementation varies depending on the target platform (Node, browser...) so it was left out for the time being.
- Replace usage of `String` for `Text` or `ByteString`.
- Better validation for non-void functions. Currently the last statement of a non-void function has to be a 'return' but an `if-else` with return statements in both blocks should be valid too.
- Less unit tests, more integration tests. The unit tests have been time-consuming and they hinder future refactoring efforts. Can check how Idris is developed for reference.

## HelloWorld.arnoldc

```
IT'S SHOWTIME
TALK TO THE HAND "hello world"
YOU HAVE BEEN TERMINATED
```

## Quick Start

1. Install the Haskell platform: https://www.haskell.org/platform/mac.html
2. Clone this repo and run

```
$ stack build
```

3. This will build a binary `arnoldH` file in the default `.stack-work` folder. If you want
to install it globally you can run `stack install` instead.
4. In the binary folder (you'll need Node.js)

```
$ ./arnoldH HelloWorld.arnoldc
$ node HelloWorld.arnoldc.js
hello world
```