# LJS Runtime

Compiler and runtime libraries allowing code written in ECMAScript 2016 syntax to be executed on the LuaJIT VM.

The compiler is implemented using a flex lexer and a bison parser.

## JSR CLI

    LJS runtime CLI, version 0.2.0
    Usage:  jsi [-c <out file>|-l] [-s] <filename>
    -c <out file> Dump bytecode into specified file
    -l            List compiled bytecode
    -s            Do not generate debug information
    (local name, upvalue name, line info and etc.)
    -v            Print LJS runtime version and exit
    -h            Show this help and exit
    If no filename is specified, an interactive repl will be available.

##  Useful links

- [Standard Compatibility](https://github.com/JSLua/ljs/wiki/Standard_Compatibility)
- [Building Instructions](https://github.com/JSLua/ljs/wiki/Building_Instructions)
- [License](LICENSE)
