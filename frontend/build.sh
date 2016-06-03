#!/bin/bash -e
if [ ! -e "last_build" ]; then
    rm -f last_build
    stack build 2> log || (cat log && false)
    ./get_args.sh
    stack exec ghc -- src/Ast.hs src/Parsing.hs && ar rcs libParsing.a src/Parsing.o src/Ast.o
    ./compile_closure_func.sh
    ./bind.sh
    touch last_build
fi
