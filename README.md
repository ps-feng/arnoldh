# ArnoldH

This is a compiler for the ArnoldC language, which is based on the one-liners of Arnold
Schwarzenegger. The original implementation is in Scala and can be found in [here]( http://lhartikk.github.io/ArnoldC/).

The original version compiles to JVM bytecode while this one converts to JavaScript.

Improvements over the original implementation:
- Scoped blocks. `if-else` and `while` blocks in the original implementation were not scoped. You could declare a variable within a block and it'd become usable in the whole method.
- Semantic validation happens before code generation. The original implementation relied on the bytecode library it was using to check for errors and the error reporting is not very nice.
- Nice errors. When an error occurs, the line where the error was found is printed
and the actual error is underlined.

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