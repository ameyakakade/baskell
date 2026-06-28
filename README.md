### Note: This compiler is a work in progress and certain features are not implemented. 
###       Only MacOS gas AArch64 target is supported.

# Baskell
## Compiler for B programming language

There are no third-party dependencies except Base (Haskell standard library).

### Parser
This compiler uses parser combinators. There is no tokenizer, which has increased complexity and decreased usefulness 
of error messages. Dealing with "right recursive grammars" is espically painful. The parsers are heavily inspired by this 
video: https://www.youtube.com/watch?v=N9RUqGYuGfw. There is usable reporting of syntax errors.

### Generator
The generator converts AST into intermediate representation. I used a "precedence parser" so the binary operators are nested 
properly in the AST itself. The IR is a simplified version inspired by IR of this project https://github.com/bext-lang/b.
There is decent error reporting, the compiler can keep on compiling and accumulate errors.

### "Targets"
This part of the compiler is to be rewritten to make it easy to add new targets. Right now the only target is MacOS gas AArch64.

#### How to use
Do "runhaskell Build.hs" to compile the compiler and test utility. The binaries will be in the build folder.

Orignal reference manual for B: https://www.nokia.com/bell-labs/about/dennis-m-ritchie/kbman.html

No AI was used to write this project.
