### Note: This compiler is a work in progress and many features are not implemented. 
###       All it can do right now is call functions(enough for a hello world!)
###       Only MacOS gas AArch64 target is supported.

# Baskell
## Compiler for B programming language

There are no third-party dependencies except Base (Haskell standard library).

### Parser
This compiler uses parser combinators. They are great for the most part unless you are dealing 
with "right recursive grammars". The parsers are heavily inspired by this video: https://www.youtube.com/watch?v=N9RUqGYuGfw.
There is acceptable reporting of syntax errors.

### Generator
The generator converts AST into intermediate representation. I used a "precedence parser" so the binary operators are nested 
properly in the AST itself. The IR is a simplified version inspired by IR of this project https://github.com/bext-lang/b.
There is decent error reporting, the compiler can keep on compiling and accumulate errors.

### "Targets"
This part of the compiler is to be rewritten to make it easy to add new targets. Right now the only target is MacOS gas AArch64.

#### How to use
You have been warned, this compiler is a work in progress.
Install GHC (Glasglow haskell compiler), start up GHCi (repl of haskell) with "Main.hs" file. Call the function "compileFile"
with a valid(and simple) B program and it will spit out a "as.s" file. Assemble this file and link appropriately (i wrote 
write and exit functions in c for the hello world example. should work with non variadic libc functions.)

Orignal reference manual for B: https://www.nokia.com/bell-labs/about/dennis-m-ritchie/kbman.html
